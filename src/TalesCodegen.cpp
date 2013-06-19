/*
 * Copyright 2013 Elie Morisse
 *
 * This file is part of Tales.
 *
 * Tales is free software: you can redistribute it and/or modify it under the
 * terms of the GNU Lesser General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 *
 * Tales is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along
 * with Tales.  If not, see <http://www.gnu.org/licenses/>.
 */

#define TalesCodegen
#include "TalesAST.hpp"
#include "TalesCodegen.hpp"

#include <iostream>

namespace Tales {
	using namespace std;
	using namespace llvm;
	
	bool TalesModule::_Load() {
		// Retrieve LLVM types and functions from the TalesRuntime.c IR
		module = ParseIRFile("TalesRuntime.s", smDiag, getGlobalContext());
		
		if (!module)
			return false;
		
		DynamicValueT = module->getTypeByName("struct.__TalesDynamicValue");
		ChainPairT = module->getTypeByName("struct.__TalesChainPair");
		StructFieldT = module->getTypeByName("struct.__TalesStructField");
		TableHeaderT = module->getTypeByName("struct.__TalesTableHeader");
		FunctionValueT = module->getTypeByName("struct.__TalesFunctionValue");
		
		FPUT = module->getTypeByName("union.__TalesFPU"); // Clang's union are LLVM structs
		TypeIndexT = IntegerType::getInt32Ty(getGlobalContext()); // NOTE: Clang doesn't define an enum type in the IR, it merely uses i32
		NumberT = Type::getFloatTy(getGlobalContext());
		
		NtoSF = module->getFunction("__TalesNtoS");
		DVtoNF = module->getFunction("__TalesDVtoN");
		DVtoSF = module->getFunction("__TalesDVtoS");
		DVtoPF = module->getFunction("__TalesDVtoP");
		
		CheckAndConvertP = module->getFunction("__TalesCheckAndConvertP");
		CheckAndConvertN = module->getFunction("__TalesCheckAndConvertN");
		
		AssignRuntimeTF = module->getFunction("__TalesAssignRuntimeT");
		GetTF = module->getFunction("__TalesGetT");
		FunctionTypecheckF = module->getFunction("__TalesFunctionTypecheck");
		
		MallocF = module->getFunction("malloc");
		AtofF = module->getFunction("__Tales_atof");
		AtoiF = module->getFunction("__Tales_atoi");
		AtolF = module->getFunction("__Tales_atol");
		StrdupF = module->getFunction("strdup");
		
		pm.add(createAlwaysInlinerPass());
		
		return true;
	}
	
	CodegenContext::CodegenContext(TalesModule& talesModule, ExecutionEngine* execEngine) :
				talesModule(talesModule), execEngine(execEngine),
				dataLayout(*execEngine->getDataLayout()),
				builder(getGlobalContext()), fpm(talesModule.module) {
		// TODO: Only allow whitelisted external functions (and classes through Clang demangler)
// 			execEngine->DisableSymbolSearching();
// 			execEngine->InstallLazyFunctionCreator();
		
		// Set up the optimizer pipeline.  Start with registering info about how the target lays out data structures.
		fpm.add(new DataLayout(*execEngine->getDataLayout()));
		
		// Provide basic AliasAnalysis support for GVN.
		fpm.add(createBasicAliasAnalysisPass());
		// Do simple "peephole" optimizations and bit-twiddling optzns.
		fpm.add(createInstructionCombiningPass());
		// Reassociate expressions.
		fpm.add(createReassociatePass());
		// Eliminate Common SubExpressions.
		fpm.add(createGVNPass());

		// Simplify the control flow graph (deleting unreachable blocks, etc).
		fpm.add(createCFGSimplificationPass());
		
		// Promote allocas to registers.
		fpm.add(createPromoteMemoryToRegisterPass());
		// Do simple "peephole" optimizations and bit-twiddling optzns.
		fpm.add(createInstructionCombiningPass());
		// Reassociate expressions.
		fpm.add(createReassociatePass());

		fpm.doInitialization();
	}
	
	
	inline Value *ErrorV(const char *str) { cerr << str << endl; return nullptr; }
	inline Constant* ConstantFieldIndex(uint64_t Idx) { return ConstantInt::get(IntegerType::getInt32Ty(getGlobalContext()), Idx); }
	inline Constant* ConstantTypeIdx(CodegenContext& context, __TalesTypeIndex ty) { return ConstantInt::get(context.talesModule.TypeIndexT, ty); }
	inline Constant* ConstantString(CodegenContext& context, const string& s) { // returns a pointer to a constant array of char
		Constant* StrCV = ConstantDataArray::getString(getGlobalContext(), s, true);
		Value* StrGV = new GlobalVariable(*context.talesModule.module, StrCV->getType(), true, GlobalValue::PrivateLinkage, StrCV);
		Value* Idxs[2] = { context.builder.getInt32(0), context.builder.getInt32(0) };
		return cast<Constant>(context.builder.CreateGEP(StrGV, Idxs));
	}
	
	inline Value* ToFPUE(CodegenContext& context, Value* V) { // Clang avoids struct types in function parameters and return values NOTE: Actually I've found the piece of code that expand structs in CGCall.cpp and Clang has ABIArgInfo::Direct that is said to stick to LLVM types but there's not a single difference between Direct and Expand in the code (Clang BUG?) This is something that deserves to be delved into since it's hard to breath with all these expands (and even seems to make the inlining pass segfault)
		if (!V->getType()->isPointerTy()) {
			V = context.builder.CreateBitCast(V, context.builder.getInt32Ty());
			V = context.builder.CreateZExtOrBitCast(V, context.dataLayout.getIntPtrType(getGlobalContext()));
			V = context.builder.CreateIntToPtr(V, context.talesModule.FPUT->getElementType(0));
		} else
			V = context.builder.CreatePointerCast(V, context.talesModule.FPUT->getElementType(0));
		
		return V;
	}
	inline Value* ToFPU(CodegenContext& context, Value* V) {
		const unsigned int Idxs[1] = { 0 };
		return context.builder.CreateInsertValue(UndefValue::get(context.talesModule.FPUT), ToFPUE(context, V), Idxs);
	}
	
	// "Expand" a RHS DV, ie when an expression is TYPEIDX_NIL to the compiler, that means it returns a dynamic value and the real value and type index have to be extracted
	inline bool ReachForDV(CodegenContext& context, Value*& V, __TalesTypeIndex vType, Value*& VTI) {
		if (vType == TYPEIDX_NIL) {
			const unsigned int VTIIdxs[1] = {0}; VTI = context.builder.CreateExtractValue(V, VTIIdxs);
			const unsigned int VIdxs[2] = {1,0}; V = context.builder.CreateExtractValue(V, VIdxs);
			return true;
		} else {
			VTI = ConstantTypeIdx(context, vType);
			return false;
		}
	}
	
	// Performs type conversions if possible
	inline Value* CheckAndConvert(CodegenContext& context, __TalesTypeIndex expectedTy, Value* V, __TalesTypeIndex VTy) {
		if (expectedTy == VTy) // NOTE: not sure if we should use strdup for strings
			return V;
			
		if (expectedTy == TYPEIDX_NIL) {
			// V is statically typed and a dvalue is expected
			unsigned int Idxs[1] = { 0 };
			Value* DV = context.builder.CreateInsertValue(UndefValue::get(context.talesModule.DynamicValueT), ConstantInt::get(context.talesModule.TypeIndexT, VTy), Idxs);
			Idxs[0] = 1;
			DV = context.builder.CreateInsertValue(DV, ToFPU(context, V), Idxs);
			
			return DV;
		}
		
		// At this point we're expecting a statically-typed value
		
		if (VTy == TYPEIDX_NIL) { // V is a dynamic value, we don't know its type at compilation time
			if (expectedTy == TYPEIDX_NUMBER) {
				Value* DVtoNA[1] = { V };
				return context.builder.CreateCall(context.talesModule.DVtoNF, DVtoNA, "dvton");
			} else {
				Value* DVtoPA[2] = { ConstantTypeIdx(context, expectedTy), V };
				return context.builder.CreateCall(context.talesModule.DVtoPF, DVtoPA, "dvtop");
			}
		}
		
		if (expectedTy == TYPEIDX_NUMBER && VTy == TYPEIDX_STRING) {
			Value* AtofA[1] = { V };
			return context.builder.CreateFPTrunc(context.builder.CreateCall(context.talesModule.AtofF, AtofA, "atof"), NType(TYPEIDX_NUMBER).Typegen(context));
		} else if (expectedTy == TYPEIDX_STRING && VTy == TYPEIDX_NUMBER) {
			Value* NtoSA[1] = { V };
			return context.builder.CreateCall(context.talesModule.NtoSF, NtoSA, "ntos"); // calls the inline __TalesNtoS function
		} else {
			return ErrorV("Type mismatch");
		}
	}
	
	// Returns a nil or 0 value of the right type, often used as an invalid value
	inline Value* NilValue(CodegenContext& context, __TalesTypeIndex ty) {
		if (ty == TYPEIDX_NIL) {
			unsigned int Idxs[1] = { 0 }; Value* DV = context.builder.CreateInsertValue(UndefValue::get(context.talesModule.DynamicValueT), ConstantTypeIdx(context, TYPEIDX_NIL), Idxs);
			Idxs[0] = 1; DV = context.builder.CreateInsertValue(DV, ToFPU(context, ConstantPointerNull::get(context.builder.getInt8PtrTy())), Idxs);
			return DV;
		} else if (ty == TYPEIDX_NUMBER)
			return ConstantFP::get(context.talesModule.NumberT, 0.0);
		else
			return ConstantPointerNull::get(cast<PointerType>(NType(ty).Typegen(context)));
	}
	
	Type* NType::Typegen(CodegenContext& context) const {
		switch(kind) {
			case TYPEIDX_NUMBER:
				return context.talesModule.NumberT;
			case TYPEIDX_STRING:
				return context.builder.getInt8PtrTy();
			case TYPEIDX_TABLE:
				if (structDecl)
					return static_cast<const NTable*>(structDecl)->Typegen(context);
				else
					return NTable().Typegen(context);
			case TYPEIDX_CLASSINST:
// 				{ const NClassDeclaration* c = static_cast<const NClassDeclaration*>(structDecl);
// 				   return c->Typegen(context); }
			case TYPEIDX_FUNCTION:
				return funcType->Typegen(context);
			default: // Undefined, a.k.a dynamic value
				return context.talesModule.DynamicValueT;
			}
	}
	
	StructType* FieldList::Typegen(CodegenContext& context) const {
		vector<Type*> Elements(size());
		for (vector<Type*>::size_type Idx = 0, LIdx = Elements.size(); Idx != LIdx; ++Idx)
			Elements[Idx] = at(Idx).type.Typegen(context);
		
		return StructType::get(getGlobalContext(), Elements, true);
	}
	
	Type* NTable::Typegen(CodegenContext& context) const {
		Type* Elements[] = { context.talesModule.TableHeaderT, fields.Typegen(context) }; // { { @firstchainpair, ipaircount, @ipairs, spairinfocount, @spairsinfo }, {spairs} }
		return StructType::get(getGlobalContext(), Elements, true)->getPointerTo();
	}
	
// 	StructType* NClassDeclaration::Typegen(CodegenContext& context) const {
// 	}
	
	__TalesTypeIndex NIdentifier::RuntimeType(CodegenEnv& env, const NType** ty) const {
		vector<string>::size_type i;
		const FieldList* fields;
		FieldList::const_iterator field, lastField;
		
		pair<const NMutable*, Value*> mu(env.semanticStack.FindMutable(levels[0]));
		if (mu.first != nullptr) {
			// A local/upvalue/argument/field was found
			
			if (levels.size() == 1) {
				if (ty) *ty = &mu.first->type;
				return mu.first->type;
			}
			
			if (mu.first->type.structDecl == nullptr) {
				if (mu.first->type != TYPEIDX_NIL)  // If neither a dynamic value, a class instance or a table, warns that the identifier is wrong
					cerr << "Invalid identifier, expected children but stuck at number/string/.." << endl;
				
				if (ty) *ty = nullptr;
				return TYPEIDX_NIL;
			}
			
			fields = &mu.first->type.structDecl->fields; i = 1;
		} else {
			// Otherwise start looking at root table
			fields = &env.root->fields; i = 0;
		}
		
		for (; i < levels.size(); ++i) {
			for (field = fields->cbegin(), lastField = fields->cend(); field != lastField; ++field) {
				if (field->name == levels[i])
					break;
			}
			
			if (field == lastField) // not found, runtime type is unknown at the time of compilation
				return TYPEIDX_NIL;
			
			if (i == (levels.size() - 1))
				break;
			
			if (field->type != TYPEIDX_CLASSINST && field->type != TYPEIDX_TABLE) // invalid identifier or dynamic value, can't go further
				return TYPEIDX_NIL;
			
			fields = &field->type.structDecl->fields;
		}
		
		if (ty) *ty = &field->type;
		return field->type;
	}
	
	Value* NIdentifier::Codegen(CodegenContext& context) const {
		// Look first in the locals of the current block, then in globals(TODO), then in the root
		// NOTE: Lots of shared code with assignments
		
		vector<string>::size_type levelsSize = levels.size();
		vector<Value*> GEPIdxList(3);
		GEPIdxList[0] = ConstantFieldIndex(0); GEPIdxList[1] = ConstantFieldIndex(1);
		
		vector<string>::size_type level;
		
		Value* LevelPtr;
		__TalesTypeIndex levelType;
		const FieldList* levelFields;
		
		// FIrst find out if one of the locals/arguments/upvalues/class fields matches the first level
		pair<const NMutable*, Value*> mu(context.env.semanticStack.FindMutable(levels[0]));
		if (mu.first != nullptr) {
			// A local/upvalue/argument/field was found
			
			if (levels.size() == 1)
				return context.builder.CreateLoad(mu.second);
			
			if (mu.first->type.structDecl == nullptr && mu.first->type != TYPEIDX_NIL)  // If neither a dynamic value, a class instance or a table, warns that the identifier is wrong
				return ErrorV("Invalid identifier, expected children but mutable is statically typed as a number/string/..");
			
			level = 1;
			LevelPtr = context.builder.CreateLoad(mu.second);
			levelType = mu.first->type;
			levelFields = &mu.first->type.structDecl->fields; 
		} else {
			// Otherwise start looking at root table
			
			level = 0;
			LevelPtr = context.env.RootV;
			levelType = context.env.root->RuntimeType(context.env);
			levelFields = &context.env.root->fields;
		}
		
		// We go as far up the struct tree as possible
		FieldIndex fieldIdx; Value* V;
		for (; level < levelsSize; ++level) {
			if (levelFields->FindFieldIndex(levels[level], fieldIdx)) {
				GEPIdxList[2] = ConstantFieldIndex(fieldIdx);
				
				V = context.builder.CreateGEP(LevelPtr, GEPIdxList);
				
				if (level == (levelsSize - 1))  // structs all the way up, fastest and simplest case
					return context.builder.CreateLoad(V);
				
				// If there are still levels to go, check whether the type of the level allows it, otherwise generates an error and returns nullptr (the assignment must be invalidated)
				levelType = levelFields->at(fieldIdx).type;
				if (levelType != TYPEIDX_CLASSINST && levelType != TYPEIDX_TABLE) {
					cerr << "Invalid identifier, ... is a number, string or function, it doesn't hold any children" << endl;
					return nullptr;
				}
				
				LevelPtr = context.builder.CreateLoad(V);
				levelFields = &levelFields->at(fieldIdx).type.structDecl->fields;
			} else {
				// The structure tree ended at this level so the compiler can't go any further, it's now up to the LLVM instructions to search for the pair, and create new pairs and tables if needed.
				break;
			}
		}
		
		// At this point we know we're expected to return a dynamic value
		
		Value* HeaderGEP[2] = { ConstantFieldIndex(0), ConstantFieldIndex(0) };
		LevelPtr = context.builder.CreateGEP(LevelPtr, HeaderGEP);
			
		vector<Constant*> RemainingLevels(levelsSize - level);
		for (vector<Constant*>::size_type I = 0; I < RemainingLevels.size(); ++I)
			RemainingLevels[I] = ConstantString(context, levels[level+I]);
		Constant* RemainingLevelsArray = ConstantArray::get(ArrayType::get(RemainingLevels[0]->getType(), RemainingLevels.size()), RemainingLevels);
		Value* RemainingLevelsGV = new GlobalVariable(*context.talesModule.module, RemainingLevelsArray->getType(), true, GlobalValue::PrivateLinkage, RemainingLevelsArray);
		Value* Idxs[2] = { ConstantFieldIndex(0), ConstantFieldIndex(0) };
		RemainingLevelsGV = context.builder.CreateGEP(RemainingLevelsGV, Idxs);
			
		if (levelType == TYPEIDX_TABLE) {
			Value* GetTA[3] = {
				LevelPtr,
				RemainingLevelsGV,
				ConstantInt::get(context.talesModule.GetTF->getFunctionType()->getParamType(2), RemainingLevels.size())
			};
			
// 			return context.builder.CreateCall(context.talesModule.GetTF, GetTA);
			
			// NOTE: Clang would you stop this bullshit?
			const unsigned int __IDX0[1] = {0}, __IDX1[1] = {1}; Value* DV;
			Value* PainInTheShellV = context.builder.CreateCall(context.talesModule.GetTF, GetTA);
			Value* PainInTheShellV0 = context.builder.CreateExtractValue(PainInTheShellV, __IDX0);
			Value* PainInTheShellV1 = context.builder.CreateExtractValue(PainInTheShellV, __IDX1);
			
			DV = context.builder.CreateInsertValue(UndefValue::get(context.talesModule.DynamicValueT), PainInTheShellV0, __IDX0);
			DV = context.builder.CreateInsertValue(DV, ToFPU(context, PainInTheShellV1), __IDX1);
			
			return DV;
		}
		
		return ErrorV("FIXME");
	}
	
	Value* NNumber::Codegen(CodegenContext& context) const {
		return ConstantFP::get(getGlobalContext(), APFloat(value));
	}
	
	Value* NString::Codegen(CodegenContext& context) const {
		return ConstantString(context, text);
	}
	
	Value* NTable::Codegen(CodegenContext& context) const {
		StructType* TableT = cast<StructType>(cast<PointerType>(Typegen(context))->getElementType());
		StructType* TableHeaderT = context.talesModule.TableHeaderT;
		const StructLayout* TableLayout = context.dataLayout.getStructLayout(TableT);
		
		Value* TablePtr = context.builder.CreateAlloca(PointerType::getUnqual(TableT));
		
		Value* MallocA[1] = { ConstantInt::get(context.talesModule.MallocF->getFunctionType()->getParamType(0), TableLayout->getSizeInBytes()) };
		Value* MallocR = context.builder.CreateCall(context.talesModule.MallocF, MallocA, "malloctable");
		context.builder.CreateStore(context.builder.CreatePointerCast(MallocR, TableT->getPointerTo()), TablePtr);
		
		// TODO: insert instructions to test TablePtr?
		
		// NOTE: I have considered making a table initialization function in TalesRuntime.c, but having to Codegen the ipairs and spairs expressions is a show-stopper.
		
		TablePtr = context.builder.CreateLoad(TablePtr);
		
		Value* GEPTableT[3];
		GEPTableT[0] = ConstantFieldIndex(0); // always
		
		// Struct pairs
		GEPTableT[1] = ConstantFieldIndex(1); // actual spairs
		for (FieldList::size_type i = 0, l = fields.size(); i < l; ++i) {
			GEPTableT[2] = ConstantFieldIndex(i);
			context.builder.CreateStore(CheckAndConvert(context, fields[i].type, fields[i].initialAssignment->Codegen(context), fields[i].initialAssignment->RuntimeType(context.env)), context.builder.CreateGEP(TablePtr, GEPTableT));
		}
		
		GEPTableT[1] = ConstantFieldIndex(0); // header
		// Pair chain -- empty at initialization
		GEPTableT[2] = ConstantFieldIndex(0);
		context.builder.CreateStore(ConstantPointerNull::get(context.talesModule.ChainPairT->getPointerTo()), context.builder.CreateGEP(TablePtr, GEPTableT));
		
		// IPairs 
		GEPTableT[2] = ConstantFieldIndex(1);
		context.builder.CreateStore(ConstantInt::get(TableHeaderT->getElementType(1), ipairs.size()), context.builder.CreateGEP(TablePtr, GEPTableT));
		
		GEPTableT[2] = ConstantFieldIndex(2);
		if (ipairs.size() != 0) {
			// Allocate the ipairs array
			MallocA[0] = ConstantInt::get(context.talesModule.MallocF->getFunctionType()->getParamType(0), context.dataLayout.getTypeAllocSize(context.talesModule.DynamicValueT) * ipairs.size());
			Value* IPairs = context.builder.CreatePointerCast(context.builder.CreateCall(context.talesModule.MallocF, MallocA, "ipairs"), context.talesModule.DynamicValueT->getPointerTo());
			context.builder.CreateStore(IPairs, context.builder.CreateGEP(TablePtr, GEPTableT));
			 
			Value* IPairEP[1];
			for (ExpressionList::size_type i = 0, l = ipairs.size(); i < l; ++i) {
				IPairEP[0] = ConstantFieldIndex(i); // ipair index
				Value* IPair = context.builder.CreateGEP(IPairs, IPairEP);
				
				context.builder.CreateStore(CheckAndConvert(context, TYPEIDX_NIL, ipairs[i]->Codegen(context), ipairs[i]->RuntimeType(context.env)), IPair);
			}
		} else {
			context.builder.CreateStore(ConstantPointerNull::get(context.talesModule.DynamicValueT->getPointerTo()), context.builder.CreateGEP(TablePtr, GEPTableT));
		}
		
		// Runtime spair info
		GEPTableT[2] = ConstantFieldIndex(3);
		context.builder.CreateStore(ConstantInt::get(TableHeaderT->getElementType(3), fields.size()), context.builder.CreateGEP(TablePtr, GEPTableT));
		
		GEPTableT[2] = ConstantFieldIndex(4);
		if (fields.size() != 0) {
			MallocA[0] = ConstantInt::get(context.talesModule.MallocF->getFunctionType()->getParamType(0), context.dataLayout.getTypeAllocSize(context.talesModule.StructFieldT) * fields.size());
			Value* SPairsInfo = context.builder.CreateCall(context.talesModule.MallocF, MallocA, "spairsinfo");
			SPairsInfo = context.builder.CreatePointerCast(SPairsInfo, context.talesModule.StructFieldT->getPointerTo());
			context.builder.CreateStore(SPairsInfo, context.builder.CreateGEP(TablePtr, GEPTableT));
			
			Value* GEPSpairsInfo[2];
			
			for (FieldList::size_type i = 0, l = fields.size(); i < l; ++i) {
				GEPSpairsInfo[0] = ConstantFieldIndex(i);
				
				GEPSpairsInfo[1] = ConstantFieldIndex(0); // name
				context.builder.CreateStore(ConstantString(context, fields[i].name), context.builder.CreateGEP(SPairsInfo, GEPSpairsInfo));
				GEPSpairsInfo[1] = ConstantFieldIndex(1); // typeIdx
				context.builder.CreateStore(ConstantInt::get(context.talesModule.StructFieldT->getElementType(1), fields[i].type), context.builder.CreateGEP(SPairsInfo, GEPSpairsInfo));
				
				GEPSpairsInfo[1] = ConstantFieldIndex(2); // offset, NOTE: GEP/DataLayout::getIndexedOffset has to be used, don't forget the padding
				Value* GEPOffsetForSField[3] = {
					ConstantFieldIndex(0),
					ConstantFieldIndex(1),
					ConstantFieldIndex(i)
				};
				context.builder.CreateStore(ConstantInt::get(context.talesModule.StructFieldT->getElementType(2), context.dataLayout.getIndexedOffset(TableT->getPointerTo(), GEPOffsetForSField)), context.builder.CreateGEP(SPairsInfo, GEPSpairsInfo));
			}
		} else {
			context.builder.CreateStore(ConstantPointerNull::get(context.talesModule.StructFieldT->getPointerTo()), context.builder.CreateGEP(TablePtr, GEPTableT));
		}
		
		return TablePtr;
	}
	
	Value* NBinaryOperation::Codegen(CodegenContext& context) const {
		Value* L = CheckAndConvert(context, TYPEIDX_NUMBER, lhs->Codegen(context), lhs->RuntimeType(context.env));
		Value* R = CheckAndConvert(context, TYPEIDX_NUMBER, rhs->Codegen(context), rhs->RuntimeType(context.env));
		
		if (!L || !R)
			return ConstantFP::get(context.talesModule.NumberT, 0.0);
		
		// NOTE: if either L or R's type cannot be converted into a number, L or R will be 0 so beware of divs
		switch (op) {
			case PLUS: return context.builder.CreateFAdd(L, R, "addtmp");
			case MINUS: return context.builder.CreateFSub(L, R, "subtmp");
			case MULT: return context.builder.CreateFMul(L, R, "multmp");
			case LT: return context.builder.CreateFCmpULT(L, R, "letmp");
			default: return ErrorV("invalid binary operator");
		}
	}
	
	Value* NBlock::Codegen(CodegenContext& context) const {
		vector<Value*> Allocas(locals.size());
		
		// Allocas and initial assignments
		LocalList::const_iterator l = locals.cbegin();
		for (vector<Value*>::iterator A = Allocas.begin(), AL = Allocas.end(); A != AL; ++A, ++l) {
			*A = context.builder.CreateAlloca(l->type.Typegen(context), 0, l->name);
			
			if (l->initialAssignment != nullptr) 
				context.builder.CreateStore(CheckAndConvert(context, l->type, l->initialAssignment->Codegen(context), l->initialAssignment->RuntimeType(context.env)), *A);
		}
		
		context.env.semanticStack.emplace(locals, Allocas);
		
		// Statements
		for (StatementList::const_iterator S = statements.cbegin(), SL = statements.cend(); S != SL; ++S) {
			(*S)->Codegen(context);
		}
		
		context.env.semanticStack.pop();
		
		// NOTE: Lua blocks are more like anonymous functions, they can return a value but that won't make their function return
		
		return nullptr;
	}
	
	Function* NFunctionDeclaration::CodegenFunc(CodegenContext& context) const {
		vector<Type*> ArgTypes(ftype.args.size());
		NFunctionType::ArgumentList::size_type Idx = 0;
		for (; Idx < ftype.args.size(); ++Idx)
			ArgTypes[Idx] = ftype.args[Idx].type.Typegen(context);
		FunctionType *FT = FunctionType::get(ftype.rtype.Typegen(context), ArgTypes, false);

		Function *F = Function::Create(FT, Function::ExternalLinkage, debugName, context.talesModule.module);

		if (!F)
			return nullptr;
		
		BasicBlock *PrevBB = context.builder.GetInsertBlock();
		
		// Create a new basic block to start insertion into.
		BasicBlock *BB = BasicBlock::Create(getGlobalContext(), "entry", F);
		context.builder.SetInsertPoint(BB);
		
		// If we're the 
		if (!context.env.RootV)
			context.env.RootV = context.env.root->Codegen(context);

		// Set names for all arguments.
		Idx = 0;
		vector<Value*> ArgAllocas(ftype.args.size());
		for (Function::arg_iterator AI = F->arg_begin(); Idx != ftype.args.size(); ++AI, ++Idx) {
			AI->setName(ftype.args[Idx].name);
			
			// Create an alloca for this variable.
			ArgAllocas[Idx] = context.builder.CreateAlloca(ArgTypes[Idx], 0, ftype.args[Idx].name);
			
			// Store the initial value into the alloca.
			context.builder.CreateStore(AI, ArgAllocas[Idx]);
		}
		
		context.funcStack.push(this);
		context.env.semanticStack.emplace(ftype.args, ArgAllocas);
		
		block.Codegen(context);
		
		context.env.semanticStack.pop();
		context.funcStack.pop();
		
		// Create a return instruction that might be redundant but will be pruned by LLVM if useless
		context.builder.CreateRet(NilValue(context, ftype.rtype));
		
// 		F->dump();
		
		// Validate the generated code, checking for consistency.
		assert(!verifyFunction(*F) && "Function verificiation failed, shouldn't happen!");
		
		// Optimize the function.
		context.fpm.run(*F);
		
		if (PrevBB)
			context.builder.SetInsertPoint(PrevBB);
		
		return F;
	}
	
	// "Manual GEP" to get an element pointer to the first argument type which follows the header in memory
	inline Value* FuncVFirstArgEP(CodegenContext& context, Value* FuncV) {
		return context.builder.CreateIntToPtr(
			context.builder.CreateAdd(
				context.builder.CreatePtrToInt(FuncV, context.dataLayout.getIntPtrType(getGlobalContext())),
				ConstantInt::get(context.dataLayout.getIntPtrType(getGlobalContext()), context.dataLayout.getTypeAllocSize(context.talesModule.FunctionValueT))
			),
			context.talesModule.TypeIndexT->getPointerTo()
		);
	}

	Value* NFunctionDeclaration::Codegen(CodegenContext& context) const {
		// Generation of the actual function
		Function* F = CodegenFunc(context);
		
		// Generation of the type-agnostic "glue" function
		vector<Type*> ArgTypes(ftype.args.size()); NFunctionType::ArgumentList::size_type Idx;
		for (Idx = 0; Idx < ftype.args.size(); ++Idx)
			ArgTypes[Idx] = context.talesModule.DynamicValueT;
		FunctionType* GlueFT = FunctionType::get(context.talesModule.DynamicValueT, ArgTypes, false);
		
		Function* GlueF = Function::Create(GlueFT, Function::ExternalLinkage, "", context.talesModule.module);
		
		BasicBlock *PrevBB = context.builder.GetInsertBlock();
		
		BasicBlock* GlueBB = BasicBlock::Create(getGlobalContext(), "entry", GlueF);
		context.builder.SetInsertPoint(GlueBB);
		
		vector<Value*> ArgsCnC(ftype.args.size());
		Idx = 0;
		for (Function::arg_iterator AI = F->arg_begin(); Idx != ftype.args.size(); ++AI, ++Idx)
			ArgsCnC[Idx] = CheckAndConvert(context, ftype.args[Idx].type, AI, TYPEIDX_NIL);
		
		Value* RetV = CheckAndConvert(context, TYPEIDX_NIL, context.builder.CreateCall(F, ArgsCnC), ftype.rtype);
		
		context.builder.CreateRet(RetV);
		
		assert(!verifyFunction(*GlueF) && "Glue function verificiation failed!");
		context.fpm.run(*GlueF);
		
		context.builder.SetInsertPoint(PrevBB);
		
		// Generation of the function value structure, which contains runtime type info about the return value and arguments along with pointers to functions
		StructType* FunctionValueT = context.talesModule.FunctionValueT;
		unsigned int allocSize = context.dataLayout.getTypeAllocSize(FunctionValueT) + ftype.args.size() * context.dataLayout.getTypeAllocSize(context.talesModule.TypeIndexT);
		// TODO: why not use a LLVM type { header, array } like for tables?
		
		Value* FuncV = context.builder.CreateAlloca(PointerType::getUnqual(FunctionValueT));
		
		Value* MallocA[1] = { ConstantInt::get(context.talesModule.MallocF->getFunctionType()->getParamType(0), allocSize) };
		Value* MallocR = context.builder.CreateCall(context.talesModule.MallocF, MallocA, "malloctable");
		context.builder.CreateStore(context.builder.CreatePointerCast(MallocR, PointerType::getUnqual(FunctionValueT)), FuncV);
		
		FuncV = context.builder.CreateLoad(FuncV);
		
		Value* GEPFuncV[2] = { ConstantFieldIndex(0), ConstantFieldIndex(0) };
		context.builder.CreateStore(ConstantTypeIdx(context, ftype.rtype), context.builder.CreateGEP(FuncV, GEPFuncV));
		GEPFuncV[1] = ConstantFieldIndex(1);
		context.builder.CreateStore(context.builder.CreatePointerCast(F, FunctionValueT->getElementType(1)), context.builder.CreateGEP(FuncV, GEPFuncV));
		GEPFuncV[1] = ConstantFieldIndex(2);
		context.builder.CreateStore(context.builder.CreatePointerCast(GlueF, FunctionValueT->getElementType(1)), context.builder.CreateGEP(FuncV, GEPFuncV));
		GEPFuncV[1] = ConstantFieldIndex(3);
		context.builder.CreateStore(ConstantInt::get(FunctionValueT->getElementType(3), ftype.args.size()), context.builder.CreateGEP(FuncV, GEPFuncV));
		
		Value* ArgT_EP = FuncVFirstArgEP(context, FuncV);
		for (Idx = 0; Idx < ftype.args.size(); ++Idx) {
			context.builder.CreateStore(ConstantTypeIdx(context, ftype.args[Idx].type), ArgT_EP);
			
			ArgT_EP = context.builder.CreateIntToPtr( // Increment the pointer to next arg typeidx
				context.builder.CreateAdd(
					context.builder.CreatePtrToInt(ArgT_EP, context.dataLayout.getIntPtrType(getGlobalContext())),
					ConstantInt::get(context.dataLayout.getIntPtrType(getGlobalContext()), context.dataLayout.getTypeAllocSize(context.talesModule.TypeIndexT))
				),
				context.talesModule.TypeIndexT->getPointerTo()
			);
		}
		
		return FuncV;
	}
	
	FunctionType* NFunctionType::Typegen(CodegenContext& context) const {
		Type* RetT = rtype.Typegen(context);
		vector<Type*> ArgsT(args.size());
		
		for (NFunctionType::ArgumentList::size_type i = 0, e = args.size(); i != e; ++i)
			ArgsT[i] = args[i].type.Typegen(context);
		
		return FunctionType::get(RetT, ArgsT, false); // FIXME? getPointerTo?
	}
	
	// CheckAndConvert for external functions, ExpectedTy being a LLVM type this time
	inline Value* CheckAndConvertExternal(CodegenContext& context, Type* ExpectedTy, Value* V, __TalesTypeIndex VTy) {
		// « Note that only one instance of a particular type is ever created. Thus seeing if two types are equal is a matter of doing a trivial pointer comparison. »
		if (ExpectedTy == NType(VTy).Typegen(context))
			return V;
		
		// TODO: take the Number type choice into account instead of assuming Number == float
		
		if (VTy == TYPEIDX_NIL) { // V is a dynamic value
			Value* DVto_A[2]; DVto_A[1] = V;
			ReachForDV(context, DVto_A[1], TYPEIDX_NIL, DVto_A[0]);
			
			if (ExpectedTy->isFloatingPointTy() || ExpectedTy->isIntegerTy()) { // Expected type is a number, either integer or floating-point
				V = context.builder.CreateCall(context.talesModule.DVtoNF, DVto_A, "dvton");
				if (ExpectedTy == context.builder.getFloatTy())
					return V;
				else if (ExpectedTy == context.builder.getDoubleTy())
					return context.builder.CreateFPExt(V, ExpectedTy); // FIXME: it'd be better to do a dvtodouble
				else
					return context.builder.CreateFPToSI(V, ExpectedTy);
			} else if (ExpectedTy == context.builder.getInt8PtrTy()) {  // Expecting a char* TODO: std::string and wchar
				return context.builder.CreateCall(context.talesModule.DVtoSF, DVto_A, "dvtos");
			} else {
				return ErrorV("FIXME");
			}
		}
		
		if (ExpectedTy->isIntegerTy() && VTy == TYPEIDX_NUMBER) {
			return context.builder.CreateFPToSI(V, ExpectedTy);
		} else if (ExpectedTy == context.builder.getDoubleTy()  && VTy == TYPEIDX_NUMBER) {
			return context.builder.CreateFPExt(V, ExpectedTy);
		} else if (ExpectedTy == context.builder.getFloatTy() && VTy == TYPEIDX_STRING) {
			Value* AtofA[1] = { V }; V = context.builder.CreateCall(context.talesModule.AtofF, AtofA, "atof");
			return context.builder.CreateFPTrunc(V, ExpectedTy);
		} else if (ExpectedTy == context.builder.getDoubleTy() && VTy == TYPEIDX_STRING) {
			Value* AtofA[1] = { V }; return context.builder.CreateCall(context.talesModule.AtofF, AtofA, "atof");
		} else if (ExpectedTy == context.talesModule.AtoiF->getReturnType() && VTy == TYPEIDX_STRING) {
			Value* AtoiA[1] = { V }; return context.builder.CreateCall(context.talesModule.AtoiF, AtoiA, "atoi");
		} else if (ExpectedTy == context.talesModule.AtolF->getReturnType() && VTy == TYPEIDX_STRING) {
			Value* AtolA[1] = { V }; return context.builder.CreateCall(context.talesModule.AtolF, AtolA, "atol");
		} else if (ExpectedTy == context.builder.getInt8PtrTy() && VTy == TYPEIDX_NUMBER) {
			Value* NtoSA[1] = { V };
			return context.builder.CreateCall(context.talesModule.NtoSF, NtoSA, "ntos"); // calls the inline __TalesNtoS function
		} else {
			return ErrorV("FIXME?");
		}
	}
	
	Value* NFunctionCall::Codegen(CodegenContext& context) const {
// 		// First look into the external functions
// 		Function* ExtF = context.talesModule.GetExternalFunction(funcId);
		
		// Look up the name in the global module table.
		Function *ExtF = context.execEngine->FindFunctionNamed(funcId.FullPath().c_str());  // TODO: it shouldn't be ExecutionEngine::FindFunctionNamed but a specific symbol table, d'ailleurs ce n'est pas InstallLazyFunctionCreator non plus, on a besoin de malloc par exemple mais le code Tales n'est pas autorisé à l'utiliser
		if (ExtF) {
			vector<Value*> ExtA(callArgs.size());
			for (CallArgumentList::size_type i = 0, e = callArgs.size(); i != e; ++i)
				ExtA[i] = CheckAndConvertExternal(context, ExtF->getFunctionType()->getParamType(i), callArgs[i]->Codegen(context), callArgs[i]->RuntimeType(context.env));

			return context.builder.CreateCall(ExtF, ExtA);
		}
		
		const NType* idTy;
		__TalesTypeIndex idTyR = funcId.RuntimeType(context.env, &idTy);
		
		if (idTyR != TYPEIDX_FUNCTION && idTyR != TYPEIDX_NIL)
			return ErrorV("Not a function");
		
		if (idTyR == TYPEIDX_FUNCTION) {
			// Function is reachable in the s-tree
			const NFunctionType* fTy = idTy->funcType;
			
			// If argument mismatch error.
			if (fTy->args.size() != callArgs.size())
				return ErrorV("Incorrect number of arguments passed");
			
			Value* Idxs[2] = { ConstantFieldIndex(0), ConstantFieldIndex(1) };
			Value* CalleeV = context.builder.CreateLoad(context.builder.CreateGEP(funcId.Codegen(context), Idxs));
			Function *CalleeF = cast<Function>(context.builder.CreatePointerCast(CalleeV, fTy->Typegen(context)));

			vector<Value*> ArgsV(callArgs.size());
			for (CallArgumentList::size_type i = 0, e = callArgs.size(); i != e; ++i)
				ArgsV[i] = CheckAndConvert(context, fTy->args[i].type, callArgs[i]->Codegen(context), callArgs[i]->RuntimeType(context.env));

			return context.builder.CreateCall(CalleeF, ArgsV);
		} else {
			// The identifier returned a dynamic value, so first we've got to tell the IR to check if the argument types match
			Value* FuncV = funcId.Codegen(context),* FuncVT;
			ReachForDV(context, FuncV, TYPEIDX_NIL, FuncVT);
			
			Value* ArgTV_EP = FuncVFirstArgEP(context, FuncV);
			
			vector<Constant*> CallArgsTC(callArgs.size());
			for (unsigned i = 0; i < callArgs.size(); ++i)
				CallArgsTC[i] = ConstantTypeIdx(context, callArgs[i]->RuntimeType(context.env));
			Constant* CallArgsTCArray = ConstantArray::get(ArrayType::get(context.talesModule.TypeIndexT, callArgs.size()), CallArgsTC);
			Value* CallArgsTV = new GlobalVariable(*context.talesModule.module, CallArgsTCArray->getType(), true, GlobalValue::PrivateLinkage, CallArgsTCArray);
		
			Value* FunctionTypecheckA[5] = {
				FuncV, FuncVT, ArgTV_EP,
				ConstantInt::get(context.talesModule.FunctionValueT->getTypeAtIndex(2), callArgs.size()), CallArgsTV
			};
			Value* CheckV = context.builder.CreateCall(context.talesModule.FunctionTypecheckF, FunctionTypecheckA);
			
			Function *CurrentFunction = context.builder.GetInsertBlock()->getParent(); // NOTE: is this necessary? it's in the Kaleidoscope tutorial, but what happens if Parent is left default?

			BasicBlock *ThenBB = BasicBlock::Create(getGlobalContext(), "then", CurrentFunction);
			BasicBlock *IfcontBB = BasicBlock::Create(getGlobalContext(), "ifcont");

			context.builder.CreateCondBr(CheckV, ThenBB, IfcontBB);
			
			
			vector<Value*> ArgsV(callArgs.size());
			for (CallArgumentList::size_type i = 0, e = callArgs.size(); i != e; ++i)
				ArgsV[i] = CheckAndConvert(context, TYPEIDX_NIL, callArgs[i]->Codegen(context), callArgs[i]->RuntimeType(context.env));
			
// 			Value* Idxs[2] = { ConstantFieldIndex(0), ConstantFieldIndex(2) };  // ->agnosticFunc
// 			Value* CalleeV = context.builder.CreateLoad(context.builder.CreateGEP(FuncV, Idxs)); // TODO FIXME test if funcId.Codegen(context) == nil
// 			Function *CalleeF = cast<Function>(context.builder.CreatePointerCast(CalleeV, ));
// 			
// 			return context.builder.CreateCall(CalleeF, ArgsV);
		}
	}
	
	Value* NReturn::Codegen(CodegenContext& context) const {
		const NFunctionDeclaration* currentFunc = context.funcStack.top();
		
		return context.builder.CreateRet(CheckAndConvert(context, currentFunc->ftype.rtype, returnExpr->Codegen(context), returnExpr->RuntimeType(context.env)));
		// TODO: Tail calls
	}
	
	Value* NAssignment::Codegen(CodegenContext& context) const {
		// NOTE: the code for an LHS identifier to be assigned is different from RHS "read-only" identifier
		
		vector<string>::size_type levelsSize = lhs.levels.size();
		vector<Value*> GEPIdxList(3);
		GEPIdxList[0] = ConstantFieldIndex(0); GEPIdxList[1] = ConstantFieldIndex(1);
	
		vector<string>::size_type level;
		
		Value* LevelPtr;
		__TalesTypeIndex levelType;
		const FieldList* levelFields;
		
		// FIrst find out if one of the locals/arguments/upvalues/class fields matches the first level
		pair<const NMutable*, Value*> mu(context.env.semanticStack.FindMutable(lhs.levels[0]));
		if (mu.first != nullptr) {
			// A local/upvalue/argument/field was found
			
			if (levelsSize == 1)
				return context.builder.CreateStore(CheckAndConvert(context, mu.first->type, rhs->Codegen(context), rhs->RuntimeType(context.env)), mu.second);
			
			if (mu.first->type.structDecl == nullptr && mu.first->type != TYPEIDX_NIL)  // If neither a dynamic value, a class instance or a table, warns that the identifier is wrong
				return ErrorV("Invalid LHS identifier, expected children but mutable is statically typed as a number/string/..");
			
			level = 1;
			LevelPtr = context.builder.CreateLoad(mu.second);
			levelType = mu.first->type;
			levelFields = &mu.first->type.structDecl->fields;
		} else {
			// Otherwise start looking at root table
			
			level = 0;
			LevelPtr = context.env.RootV;
			levelType = context.env.root->RuntimeType(context.env);
			levelFields = &context.env.root->fields;
		}
		
		// We go as far up the struct tree as possible
		FieldIndex fieldIdx; Value* LHSPtr;
		for (; level < levelsSize; ++level) {
			if (levelFields->FindFieldIndex(lhs.levels[level], fieldIdx)) {
				GEPIdxList[2] = ConstantFieldIndex(fieldIdx);
				
				LHSPtr = context.builder.CreateGEP(LevelPtr, GEPIdxList);
				
				if (level == (levelsSize - 1))  // structs all the way up, fastest and simplest case
					return context.builder.CreateStore(CheckAndConvert(context, levelFields->at(fieldIdx).type, rhs->Codegen(context), rhs->RuntimeType(context.env)), LHSPtr);
				
				// If there are still levels to go, check whether the type of the level allows it, otherwise generates an error and returns nullptr (the assignment must be invalidated)
				levelType = levelFields->at(fieldIdx).type;
				if (levelType != TYPEIDX_CLASSINST && levelType != TYPEIDX_TABLE) {
					cerr << "Invalid identifier, ... is a number, string or function, it doesn't hold any children" << endl;
					return nullptr;
				}
				
				LevelPtr = context.builder.CreateLoad(LHSPtr);
				levelFields = &levelFields->at(fieldIdx).type.structDecl->fields;
			} else {
				// The structure tree ended at this level so the compiler can't go any further, it's now up to the LLVM instructions to search for the pair, and create new pairs and tables if needed.
				break;
			}
		}
		
		Value* RHS = rhs->Codegen(context), * RHSTI;
		ReachForDV(context, RHS, rhs->RuntimeType(context.env), RHSTI);
		
		if (levelType == TYPEIDX_TABLE) {
			Value* HeaderGEP[2] = { ConstantFieldIndex(0), ConstantFieldIndex(0) };
			LevelPtr = context.builder.CreateGEP(LevelPtr, HeaderGEP); // needed because AssignRuntime expects a pointer to the header, not the whole structure even if there is no spair
			
			vector<Constant*> RemainingLevels(levelsSize - level);
			for (vector<Constant*>::size_type I = 0; I < RemainingLevels.size(); ++I)
				RemainingLevels[I] =  ConstantString(context, lhs.levels[level+I]);
			Constant* RemainingLevelsArray = ConstantArray::get(ArrayType::get(RemainingLevels[0]->getType(), RemainingLevels.size()), RemainingLevels);
			Value* RemainingLevelsGV = new GlobalVariable(*context.talesModule.module, RemainingLevelsArray->getType(), true, GlobalValue::PrivateLinkage, RemainingLevelsArray);
			Value* Idxs[2] = { ConstantFieldIndex(0), ConstantFieldIndex(0) };
			RemainingLevelsGV = context.builder.CreateGEP(RemainingLevelsGV, Idxs);
			
			Value* AssignRuntimeTA[5] = {
				LevelPtr,
				RemainingLevelsGV,
				ConstantInt::get(context.talesModule.AssignRuntimeTF->getFunctionType()->getParamType(2), RemainingLevels.size()),
				ToFPUE(context, RHS),
				RHSTI
			};
			
			return context.builder.CreateCall(context.talesModule.AssignRuntimeTF, AssignRuntimeTA);
		}
		
		llvm_unreachable("Assignment FIXME");
	}
}