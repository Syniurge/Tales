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

#include <type_traits>

#include "TalesAST.hpp"
#include "TalesCodegen.hpp"

namespace Tales {
	using llvm::cast;
	using llvm::dyn_cast;

	bool RuntimeModule::_Load() {
		// Retrieve LLVM types and functions from the TalesRuntime.c IR
		module = llvm::ParseIRFile("TalesRuntime.s", smDiag, llvm::getGlobalContext());

		if (!module)
			return false;

		DynamicValueT = getTypeByName("struct.__TalesDynamicValue");
		ChainPairT = getTypeByName("struct.__TalesChainPair");
		StructFieldT = getTypeByName("struct.__TalesStructField");
		TableHeaderT = getTypeByName("struct.__TalesTableHeader");
		FunctionValueT = getTypeByName("struct.__TalesFunctionValue");

		FPUT = getTypeByName("union.__TalesFPU"); // Clang's union are LLVM structs
		TypeIndexT = llvm::IntegerType::getInt32Ty(llvm::getGlobalContext());
				// NOTE: Clang doesn't define an enum type in the IR, it merely uses i32
		NumberT = llvm::Type::getFloatTy(llvm::getGlobalContext());

		EmptySF = getFunction("__TalesEmptyS");
		EmptyTF = getFunction("__TalesEmptyT");

		NtoSF = getFunction("__TalesNtoS");
		BtoNF = getFunction("__TalesBtoN");
		BtoSF = getFunction("__TalesBtoS");
		DVtoNF = getFunction("__TalesDVtoN");
		DVtoSF = getFunction("__TalesDVtoS");
		DVtoBF = getFunction("__TalesDVtoB");
		DVtoPF = getFunction("__TalesDVtoP");

		AssignRuntimeTF = getFunction("__TalesAssignRuntimeT");
		GetTF = getFunction("__TalesGetT");
		FunctionTypecheckF = getFunction("__TalesFunctionTypecheck");

		MallocF = getFunction("malloc");
		AtofF = getFunction("__Tales_atof");
		AtoiF = getFunction("__Tales_atoi");
		AtolF = getFunction("__Tales_atol");
		StrdupF = getFunction("strdup");

		pm.add(llvm::createAlwaysInlinerPass());

		return true;
	}

	CodegenContext::CodegenContext(RuntimeModule& runtimeModule,
																 llvm::ExecutionEngine* execEngine)
				: runtimeModule(runtimeModule), execEngine(execEngine),
				  dataLayout(*execEngine->getDataLayout()),
				  builder(llvm::getGlobalContext()), fpm(runtimeModule.module) {
		// TODO: Only allow whitelisted external functions (and classes through Clang demangler)
// 			execEngine->DisableSymbolSearching();
// 			execEngine->InstallLazyFunctionCreator();

		// Set up the optimizer pipeline.  Start with registering info about how the target lays out data structures.
		fpm.add(new llvm::DataLayout(*execEngine->getDataLayout()));

		// Provide basic AliasAnalysis support for GVN.
		fpm.add(llvm::createBasicAliasAnalysisPass());
		// Do simple "peephole" optimizations and bit-twiddling optzns.
		fpm.add(llvm::createInstructionCombiningPass());
		// Reassociate expressions.
		fpm.add(llvm::createReassociatePass());
		// Eliminate Common SubExpressions.
		fpm.add(llvm::createGVNPass());

		// Simplify the control flow graph (deleting unreachable blocks, etc).
		fpm.add(llvm::createCFGSimplificationPass());

		// Promote allocas to registers.
		fpm.add(llvm::createPromoteMemoryToRegisterPass());
		// Do simple "peephole" optimizations and bit-twiddling optzns.
		fpm.add(llvm::createInstructionCombiningPass());
		// Reassociate expressions.
		fpm.add(llvm::createReassociatePass());

		fpm.doInitialization();
	}


	inline llvm::Value *ErrorV(const char *str) {
		llvm::errs() << str << "\n";
		return nullptr;
	}
	inline llvm::Constant* ConstantFieldIndex(uint64_t Idx) {
		return llvm::ConstantInt::get(
						llvm::IntegerType::getInt32Ty(llvm::getGlobalContext()), Idx);
	}
	inline llvm::Constant* ConstantTypeIdx(CodegenContext& context,
																					__TalesTypeIndex ty) {
		return llvm::ConstantInt::get(context.runtimeModule.TypeIndexT, ty);
	}
	inline llvm::Constant* ConstantString(CodegenContext& context, const string& s) {
		llvm::Constant* StrCV = llvm::ConstantDataArray::getString(llvm::getGlobalContext(),
																											s, true);
		llvm::Value* StrGV = new llvm::GlobalVariable(*context.runtimeModule.module,
							StrCV->getType(), true, llvm::GlobalValue::PrivateLinkage, StrCV);
		llvm::Value* Idxs[2] = { context.builder.getInt32(0),
					context.builder.getInt32(0) };
		return cast<llvm::Constant>(context.builder.CreateGEP(StrGV, Idxs));
	}

	// Helper class to construct GEP instructions
	class GEPIndexList : public vector<llvm::Value*> {
		inline void Emplace() {}

		template <typename... Args>
		inline void Emplace(uint64_t index, Args... args) {
			emplace_back(ConstantFieldIndex(index));
			Emplace(args...);
		}
	public:
		template <typename... Args>
		GEPIndexList(size_type n, Args... args) {
			reserve(n);
			Emplace(args...);
		}

		llvm::Value*& operator[](size_type n) {
			assert(n < capacity());
			if (n >= size())
				resize(n+1);
			return vector<llvm::Value*>::at(n);
		}
	};

	// Clang avoids struct types in function parameters and return values
	// NOTE: Actually I've found the piece of code that expand structs in CGCall.cpp
	// and Clang has ABIArgInfo::Direct that is said to stick to LLVM types but there's
	// not a single difference between Direct and Expand in the code (Clang BUG?)
	// This is something that deserves to be delved into since it's hard to breath
	// with all these expands (and even seems to make the inlining pass segfault)
	inline llvm::Value* ToFPUE(CodegenContext& context, llvm::Value* V) {
		if (!V->getType()->isPointerTy()) {
			V = context.builder.CreateZExtOrBitCast(V, context.builder.getInt32Ty());
			V = context.builder.CreateZExtOrBitCast(V,
										context.dataLayout.getIntPtrType(llvm::getGlobalContext()));
			V = context.builder.CreateIntToPtr(V,
																 context.runtimeModule.FPUT->getElementType(0));
		} else
			V = context.builder.CreatePointerCast(V,
																	context.runtimeModule.FPUT->getElementType(0));

		return V;
	}
	inline llvm::Value* ToFPU(CodegenContext& context, llvm::Value* V) {
		const unsigned int Idxs[1] = { 0 };
		return context.builder.CreateInsertValue(
			llvm::UndefValue::get(context.runtimeModule.FPUT), ToFPUE(context, V), Idxs);
	}

	// "Expand" a RHS DV, ie when an expression is TYPEIDX_NIL to the compiler, that means
	// that it returns a dynamic value and the real value and type index have to be extracted
	inline bool ReachForDV(CodegenContext& context, llvm::Value*& V,
												 __TalesTypeIndex vType, llvm::Value*& VTI) {
		if (vType == TYPEIDX_NIL) {
			const unsigned int VTIIdxs[1] = {0};
			VTI = context.builder.CreateExtractValue(V, VTIIdxs);
			const unsigned int VIdxs[2] = {1,0};
			V = context.builder.CreateExtractValue(V, VIdxs);
			return true;
		} else {
			VTI = ConstantTypeIdx(context, vType);
			return false;
		}
	}

	// Performs type conversions if possible
	inline llvm::Value* CheckAndConvert(CodegenContext& context,
																			__TalesTypeIndex expectedTy, llvm::Value* V,
																		 __TalesTypeIndex VTy) {
		if (expectedTy == VTy) // NOTE: not sure if we should use strdup for strings
			return V;

		if (expectedTy == TYPEIDX_NIL) {
			// V is statically typed and a dvalue is expected
			unsigned int Idxs[1] = { 0 };
			llvm::Value* DV = context.builder.CreateInsertValue(
											llvm::UndefValue::get(context.runtimeModule.DynamicValueT),
											llvm::ConstantInt::get(context.runtimeModule.TypeIndexT, VTy), Idxs);
			Idxs[0] = 1;
			DV = context.builder.CreateInsertValue(DV, ToFPU(context, V), Idxs);

			return DV;
		}

		// At this point we're expecting a statically-typed value

		if (VTy == TYPEIDX_NIL) {
			// V is a dynamic value, we don't know its type at compilation time
			if (expectedTy == TYPEIDX_NUMBER) {
				llvm::Value* DVtoNA[1] = { V };
				return context.builder.CreateCall(context.runtimeModule.DVtoNF,
																					DVtoNA, "dvton");
			} else if (expectedTy == TYPEIDX_BOOL) {
				llvm::Value* DVtoBA[1] = { V };
				return context.builder.CreateCall(context.runtimeModule.DVtoBF,
																					DVtoBA, "dvtob");
			} else {
				llvm::Value* DVtoPA[2] = { ConstantTypeIdx(context, expectedTy), V };
				return context.builder.CreateCall(context.runtimeModule.DVtoPF,
																					DVtoPA, "dvtop");
			}
		}

		if (expectedTy == TYPEIDX_BOOL) {
			if (VTy == TYPEIDX_NUMBER)
				return context.builder.CreateFCmpONE(V,
								llvm::ConstantFP::get(context.runtimeModule.NumberT, 0.0));
			else if (VTy == TYPEIDX_STRING) {
				llvm::Value* EmptySA[1] = { V };
				llvm::Value* IsEmpty = context.builder.CreateCall(context.runtimeModule.EmptySF,
																	 EmptySA, "emptys");
				return context.builder.CreateNot(IsEmpty, "notemptys");
			} else if (VTy == TYPEIDX_TABLE) {
				llvm::Value* EmptyTA[1] = { V };
				llvm::Value* IsEmpty = context.builder.CreateCall(context.runtimeModule.EmptyTF,
																	 EmptyTA, "emptyt");
				return context.builder.CreateNot(IsEmpty, "notemptyt");
			} else
				return ErrorV("FIXME, not ->bool conversion not implemented"
								"for this type yet.\n");
		}

		if (expectedTy == TYPEIDX_NUMBER && VTy == TYPEIDX_STRING) {
			llvm::Value* AtofA[1] = { V };
			return context.builder.CreateFPTrunc(
					context.builder.CreateCall(context.runtimeModule.AtofF,
																		 AtofA, "atof"),
					Type(TYPEIDX_NUMBER).Typegen(context));
		} else if (expectedTy == TYPEIDX_STRING && VTy == TYPEIDX_NUMBER) {
			llvm::Value* NtoSA[1] = { V };
			return context.builder.CreateCall(context.runtimeModule.NtoSF,
																				NtoSA, "ntos");
		} else if (expectedTy == TYPEIDX_NUMBER && VTy == TYPEIDX_BOOL) {
			llvm::Value* BtoNA[1] = { V };
			return context.builder.CreateCall(context.runtimeModule.BtoNF,
																				BtoNA, "bton");
		} else if (expectedTy == TYPEIDX_STRING && VTy == TYPEIDX_BOOL) {
			llvm::Value* BtoSA[1] = { V };
			return context.builder.CreateCall(context.runtimeModule.BtoSF,
																				BtoSA, "btos");
		} else {
			return ErrorV("llvm::Type mismatch");
		}
	}

	// Returns a nil or 0 value of the right type, often used as an invalid value
	inline llvm::Value* NilValue(CodegenContext& context, __TalesTypeIndex ty) {
		if (ty == TYPEIDX_NIL) {
			unsigned int Idxs[1] = { 0 };
			llvm::Value* DV = context.builder.CreateInsertValue(
					llvm::UndefValue::get(context.runtimeModule.DynamicValueT),
					ConstantTypeIdx(context, TYPEIDX_NIL), Idxs);
			Idxs[0] = 1;
			DV = context.builder.CreateInsertValue(DV,
					ToFPU(context,llvm::ConstantPointerNull::get(context.builder.getInt8PtrTy())),
					Idxs);
			return DV;
		} else if (ty == TYPEIDX_NUMBER)
			return llvm::ConstantFP::get(context.runtimeModule.NumberT, 0.0);
		else if (ty == TYPEIDX_BOOL)
			return llvm::ConstantInt::getFalse(llvm::getGlobalContext());
		else
			return llvm::ConstantPointerNull::get(
				cast<llvm::PointerType>(Type(ty).Typegen(context)));
	}

	llvm::Type* Type::Typegen(CodegenContext& context) const {
		switch(kind) {
			case TYPEIDX_NUMBER:
				return context.runtimeModule.NumberT;
			case TYPEIDX_STRING:
				return context.builder.getInt8PtrTy();
			case TYPEIDX_TABLE:
				if (structDecl)
					return static_cast<const Table*>(structDecl)->Typegen(context);
				else
					return Table().Typegen(context);
			case TYPEIDX_CLASSINST:
// 				{ const NClassDeclaration* c = static_cast<const NClassDeclaration*>(structDecl);
// 				   return c->Typegen(context); }
			case TYPEIDX_FUNCTION:
				return funcType->Typegen(context);
			default: // Undefined, a.k.a dynamic value
				return context.runtimeModule.DynamicValueT;
			}
	}

	llvm::StructType* FieldList::Typegen(CodegenContext& context) const {
		vector<llvm::Type*> Elements(size());
		for (vector<llvm::Type*>::size_type Idx = 0, LIdx = Elements.size(); Idx != LIdx; ++Idx)
			Elements[Idx] = at(Idx).type.Typegen(context);

		return llvm::StructType::get(llvm::getGlobalContext(), Elements, true);
	}

	// LLVM StructType for tables
	// { { @firstchainpair, ipaircount, @ipairs, spairinfocount, @spairsinfo },
	//    {spairs} }
	llvm::Type* Table::Typegen(CodegenContext& context) const {
		llvm::Type* Elements[] = {
			context.runtimeModule.TableHeaderT,
			fields.Typegen(context)
		};
		return llvm::StructType::get(llvm::getGlobalContext(), Elements, true)
				->getPointerTo();
	}

// 	llvm::StructType* ClassDeclaration::Typegen(CodegenContext& context) const {
// 	}

	llvm::Value* Boolean::Codegen(CodegenContext& context) const {
		if (value)
			return llvm::ConstantInt::getTrue(llvm::getGlobalContext());
		else
			return llvm::ConstantInt::getFalse(llvm::getGlobalContext());
	}

	llvm::Value* Number::Codegen(CodegenContext& context) const {
		return llvm::ConstantFP::get(llvm::getGlobalContext(), llvm::APFloat(value));
	}

	llvm::Value* String::Codegen(CodegenContext& context) const {
		return ConstantString(context, value);
	}

	const Type& Identifier::RuntimeType(CodegenEnv& env) const {
		Levels::const_iterator level = levels.cbegin(),
				lastLevel = levels.cend();
		const FieldList* fields;
		FieldList::const_iterator field, lastField;

		if (!isa<String>(**level))
			return Type::DynamicValue;

		const string& firstLevelName = cast<String>(**level).value;

		// Look first for a local/argument/upvalue/class field
		pair<const Mutable*, llvm::Value*> mu(
						env.semanticStack.FindMutable(firstLevelName));
		if (mu.first != nullptr) {
			// A local/upvalue/argument/field was found

			if (levels.size() == 1)
				return mu.first->type;

			if (mu.first->type.structDecl == nullptr) {
				// If neither a dynamic value, a class instance or a table,
				// warns that the identifier is wrong
				if (mu.first->type != TYPEIDX_NIL)
					llvm::errs() << "Invalid identifier, expected children but stuck at"
							"number/string/..\n";

				return Type::DynamicValue;
			}

			fields = &mu.first->type.structDecl->fields;
			++level;
		} else {
			// Otherwise start looking at root table
			fields = &env.root->fields;
		}

		for (; level != lastLevel; ++level) {
			if (!isa<String>(**level))
				return Type::DynamicValue;

			const string& levelName = cast<String>(**level).value;

			for (field = fields->cbegin(), lastField = fields->cend(); field != lastField; ++field) {
				if (field->name == levelName)
					break;
			}

			if (field == lastField)
				// Not found, runtime type is unknown at the time of compilation
				return Type::DynamicValue;

			if (level == lastLevel -1)
				break;

			if (field->type != TYPEIDX_CLASSINST && field->type != TYPEIDX_TABLE)
				// Invalid identifier or dynamic value, can't go further
				return Type::DynamicValue;

			fields = &field->type.structDecl->fields;
		}

		return field->type;
	}

	// NOTE: C++ really lacks true metaprogramming, and I'll probably migrate to D in
	// the future since the core designers of C++ have stated their opposition to static if.
	// Assignment::Codegen, Identifier::Codegen and Identifier::RuntimeType share A LOT
	// of code, but I can't put it in the same function template without splitting it into
	// very unnatural parts. Extremely twisted illustration below.

	template <typename _ReturnType>
	struct ActionsOnIdentifier {
		static_assert(std::is_same<_ReturnType, llvm::Value*>::value ||
				std::is_same<_ReturnType, const Type&>::value,
				"Incorrect return type passed to ActionsOnIdentifier");

		typedef _ReturnType ReturnType;
	};

	// Lenghty function template that avoids duplicate code between Assignment::Codegen
	// and Identifier::Codegen.
	// As said above C++ unfortunately doesn't make things simple.
	// The Actions parameter must be a class containing three static public functions,
	// refer to examples afterwards.
	template <typename Actions>
	typename Actions::ReturnType EmitActionOnIdentifier(const Identifier& id,
															const Node* node, CodegenContext& context) {
		static_assert(std::is_base_of<ActionsOnIdentifier<typename Actions::ReturnType>,
				Actions>::value, "Actions must derive from ActionsOnIdentifier<ReturnType>");

		typedef Identifier::Levels Levels;
		const Levels& levels = id.levels;

		// Look first in the locals of the current block, then in globals(TODO), then in the root

		Levels::const_iterator level = levels.cbegin(),
				lastLevel = levels.cend();

		llvm::Value* LevelPtr;
		Type::Ref levelType;
		const FieldList* levelFields;

		if (isa<String>(**level)) {
			const string& firstLevelName = cast<String>(**level).value;

			// FIrst find out if one of the locals/arguments/upvalues/class fields
			// matches the first level
			pair<const Mutable*, llvm::Value*> mu(
						context.env.semanticStack.FindMutable(firstLevelName));
			if (mu.first != nullptr) {
				// A local/upvalue/argument/field was found

				if (levels.size() == 1)
					return Actions::OnStatic(node, context, mu.first->type, mu.second);

				if (mu.first->type.structDecl == nullptr && mu.first->type != TYPEIDX_NIL)
					// If neither a dynamic value, a class instance or a table,
					// warns that the identifier is wrong
					return Actions::OnInvalid(node, context);

				++level;
				levelType = mu.first->type;
				levelFields = &mu.first->type.structDecl->fields;
				LevelPtr = context.builder.CreateLoad(mu.second);
			} else {
				// Otherwise start looking at root table

				levelType = context.env.root->RuntimeType(context.env);
				levelFields = &context.env.root->fields;
				LevelPtr = context.env.RootV;
			}

			// We go as far up the struct tree as possible
			FieldList::Index fieldIdx;
			GEPIndexList MutableGEP(3, 0, 1);

			for (; level != lastLevel; ++level) {
				if (!isa<String>(**level))
					break;

				const string& levelName = cast<String>(**level).value;

				if (levelFields->FindFieldIndex(levelName, fieldIdx)) {
					llvm::Value* V;

					levelType = levelFields->at(fieldIdx).type;

					MutableGEP[2] = ConstantFieldIndex(fieldIdx);
					V = context.builder.CreateGEP(LevelPtr, MutableGEP);

					if (level == lastLevel - 1)  // structs all the way up, fastest and simplest case
						return Actions::OnStatic(node, context, levelType, V);

					// If there are still levels to go, check whether the type of the level allows it,
					// otherwise generates an error and returns nullptr (the assignment must
					// be invalidated)
					if (levelType != TYPEIDX_CLASSINST && levelType != TYPEIDX_TABLE)
						return Actions::OnInvalid(node, context);

					levelFields = &levelFields->at(fieldIdx).type.structDecl->fields;
					LevelPtr = context.builder.CreateLoad(V);
				} else {
					// The structure tree ended at this level so the compiler can't go any further,
					// it's now up to the LLVM instructions to search for the pair, and create new
					// pairs and tables if needed.
					break;
				}
			}
		}

		// At this point we know we've delved in the realm of dynamic values.

		GEPIndexList HeaderGEP(2, 0, 0);
		LevelPtr = context.builder.CreateGEP(LevelPtr, HeaderGEP);

		vector<llvm::Constant*> RemainingLevels(lastLevel - level);
		for (auto& RemLevel : RemainingLevels) {
			const string& levelName = cast<String>(**level).value;
						// FIXME FIXME non-string exprs
			RemLevel = ConstantString(context, levelName);
			++level;
		}
		llvm::Constant* RemainingLevelsArray = llvm::ConstantArray::get(
				llvm::ArrayType::get(RemainingLevels[0]->getType(), RemainingLevels.size()),
																																		RemainingLevels);
		llvm::Value* RemainingLevelsGV = new llvm::GlobalVariable(
				*context.runtimeModule.module, RemainingLevelsArray->getType(), true,
				llvm::GlobalValue::PrivateLinkage, RemainingLevelsArray);

		GEPIndexList RemainingLevelsGEP(2, 0, 0);
		RemainingLevelsGV = context.builder.CreateGEP(RemainingLevelsGV, RemainingLevelsGEP);

		return Actions::OnUnreachable(node, context, levelType, LevelPtr,
					RemainingLevelsGV, RemainingLevels.size());
	}

	struct GetValueOnIdentifier : public ActionsOnIdentifier<llvm::Value*> {
		inline static llvm::Value* OnStatic(const Node* node, CodegenContext& context,
																					 const Type& IdTy, llvm::Value* IdV) {
			return context.builder.CreateLoad(IdV);
		}

		inline static llvm::Value* OnInvalid(const Node* node, CodegenContext& context) {
			return ErrorV("Invalid identifier, expected children but mutable"
							"is statically typed as a number/string/..");
		}

		inline static llvm::Value* OnUnreachable(const Node* node, CodegenContext& context,
															const Type& levelType, llvm::Value* LevelPtr,
															llvm::Value* RemainingLevelsGV, size_t RemainingLevelsSize) {
			if (levelType == TYPEIDX_TABLE) {
				llvm::Value* GetTA[3] = {
					LevelPtr,
					RemainingLevelsGV,
					llvm::ConstantInt::get(context.runtimeModule.GetTF->getFunctionType()->getParamType(2),
								RemainingLevelsSize)
				};

	// 			return context.builder.CreateCall(context.runtimeModule.GetTF, GetTA);

				// NOTE: Clang would you stop this bullshit? I need to fix this horror someday.
				const unsigned int __IDX0[1] = {0}, __IDX1[1] = {1}; llvm::Value* DV;
				llvm::Value* PainInTheShellV = context.builder.CreateCall(context.runtimeModule.GetTF, GetTA);
				llvm::Value* PainInTheShellV0 = context.builder.CreateExtractValue(PainInTheShellV, __IDX0);
				llvm::Value* PainInTheShellV1 = context.builder.CreateExtractValue(PainInTheShellV, __IDX1);

				DV = context.builder.CreateInsertValue(
							llvm::UndefValue::get(context.runtimeModule.DynamicValueT),
							PainInTheShellV0, __IDX0);
				DV = context.builder.CreateInsertValue(DV, ToFPU(context, PainInTheShellV1),
							__IDX1);

				return DV;
			}

			llvm_unreachable("FIXME Identifier::Codegen");
		}
	};

	llvm::Value* Identifier::Codegen(CodegenContext& context) const {
		return EmitActionOnIdentifier<GetValueOnIdentifier>(*this, this, context);
	}

	llvm::Value* Table::Codegen(CodegenContext& context) const {
		llvm::StructType* TableT = cast<llvm::StructType>(
					cast<llvm::PointerType>(Typegen(context))->getElementType());
		llvm::StructType* TableHeaderT = context.runtimeModule.TableHeaderT;
		const llvm::StructLayout* TableLayout = context.dataLayout.getStructLayout(TableT);

		llvm::Value* TablePtr = context.builder.CreateAlloca(llvm::PointerType::getUnqual(TableT));

		llvm::Value* MallocA[1] = { llvm::ConstantInt::get(context.runtimeModule.MallocF->
								getFunctionType()->getParamType(0), TableLayout->getSizeInBytes()) };
		llvm::Value* MallocR = context.builder.CreateCall(context.runtimeModule.MallocF,
								MallocA, "malloctable");
		context.builder.CreateStore(context.builder.CreatePointerCast(MallocR,
								TableT->getPointerTo()), TablePtr);

		// TODO: insert instructions to test TablePtr?

		// NOTE: I have considered making a table initialization function in TalesRuntime.c,
		// but having to Codegen the ipairs and spairs expressions is a show-stopper.

		TablePtr = context.builder.CreateLoad(TablePtr);

		llvm::Value* GEPTableT[3];
		GEPTableT[0] = ConstantFieldIndex(0); // always

		// Struct pairs
		GEPTableT[1] = ConstantFieldIndex(1); // actual spairs
		for (FieldList::size_type i = 0, l = fields.size(); i < l; ++i) {
			GEPTableT[2] = ConstantFieldIndex(i);
			context.builder.CreateStore(CheckAndConvert(context, fields[i].type,
																				fields[i].initialAssignment->Codegen(context),
																				fields[i].initialAssignment->RuntimeType(context.env)),
															context.builder.CreateGEP(TablePtr, GEPTableT));
		}

		GEPTableT[1] = ConstantFieldIndex(0); // header
		// Pair chain -- empty at initialization
		GEPTableT[2] = ConstantFieldIndex(0);
		context.builder.CreateStore(llvm::ConstantPointerNull::get(
									context.runtimeModule.ChainPairT->getPointerTo()),
							context.builder.CreateGEP(TablePtr, GEPTableT));

		// IPairs
		GEPTableT[2] = ConstantFieldIndex(1);
		context.builder.CreateStore(
					llvm::ConstantInt::get(TableHeaderT->getElementType(1), ipairs.size()),
					context.builder.CreateGEP(TablePtr, GEPTableT));

		GEPTableT[2] = ConstantFieldIndex(2);
		if (ipairs.size() != 0) {
			// Allocate the ipairs array
			MallocA[0] = llvm::ConstantInt::get(
					context.runtimeModule.MallocF->getFunctionType()->getParamType(0),
					context.dataLayout.getTypeAllocSize(context.runtimeModule.DynamicValueT) * ipairs.size());
			llvm::Value* IPairs = context.builder.CreatePointerCast(
					context.builder.CreateCall(context.runtimeModule.MallocF, MallocA, "ipairs"),
					context.runtimeModule.DynamicValueT->getPointerTo());
			context.builder.CreateStore(IPairs, context.builder.CreateGEP(TablePtr, GEPTableT));

			llvm::Value* IPairEP[1];
			for (ExpressionList::size_type i = 0, l = ipairs.size(); i < l; ++i) {
				IPairEP[0] = ConstantFieldIndex(i); // ipair index
				llvm::Value* IPair = context.builder.CreateGEP(IPairs, IPairEP);

				context.builder.CreateStore(CheckAndConvert(context, TYPEIDX_NIL,
													ipairs[i]->Codegen(context),
													ipairs[i]->RuntimeType(context.env)),
											IPair);
			}
		} else {
			context.builder.CreateStore(llvm::ConstantPointerNull::get(
							context.runtimeModule.DynamicValueT->getPointerTo()),
					context.builder.CreateGEP(TablePtr, GEPTableT));
		}

		// Runtime spair info
		GEPTableT[2] = ConstantFieldIndex(3);
		context.builder.CreateStore(
						llvm::ConstantInt::get(TableHeaderT->getElementType(3), fields.size()),
						context.builder.CreateGEP(TablePtr, GEPTableT));

		GEPTableT[2] = ConstantFieldIndex(4);
		if (fields.size() != 0) {
			MallocA[0] = llvm::ConstantInt::get(
					context.runtimeModule.MallocF->getFunctionType()->getParamType(0),
					context.dataLayout.getTypeAllocSize(context.runtimeModule.StructFieldT) * fields.size());
			llvm::Value* SPairsInfo = context.builder.CreateCall(
																context.runtimeModule.MallocF, MallocA, "spairsinfo");
			SPairsInfo = context.builder.CreatePointerCast(SPairsInfo,
																context.runtimeModule.StructFieldT->getPointerTo());
			context.builder.CreateStore(SPairsInfo,
																	context.builder.CreateGEP(TablePtr, GEPTableT));

			llvm::Value* GEPSpairsInfo[2];

			for (FieldList::size_type i = 0, l = fields.size(); i < l; ++i) {
				GEPSpairsInfo[0] = ConstantFieldIndex(i);

				GEPSpairsInfo[1] = ConstantFieldIndex(0); // name
				context.builder.CreateStore(ConstantString(context, fields[i].name),
																		context.builder.CreateGEP(SPairsInfo, GEPSpairsInfo));
				GEPSpairsInfo[1] = ConstantFieldIndex(1); // typeIdx
				context.builder.CreateStore(llvm::ConstantInt::get(
									context.runtimeModule.StructFieldT->getElementType(1), fields[i].type),
							context.builder.CreateGEP(SPairsInfo, GEPSpairsInfo));

				// Offset, NOTE: GEP/DataLayout::getIndexedOffset has to be used,
				// don't forget the padding.
				GEPSpairsInfo[1] = ConstantFieldIndex(2);
				llvm::Value* GEPOffsetForSField[3] = {
					ConstantFieldIndex(0),
					ConstantFieldIndex(1),
					ConstantFieldIndex(i)
				};
				context.builder.CreateStore(llvm::ConstantInt::get(
							context.runtimeModule.StructFieldT->getElementType(2),
							context.dataLayout.getIndexedOffset(TableT->getPointerTo(),
																									GEPOffsetForSField)),
							context.builder.CreateGEP(SPairsInfo, GEPSpairsInfo));
			}
		} else {
			context.builder.CreateStore(llvm::ConstantPointerNull::get(
								context.runtimeModule.StructFieldT->getPointerTo()),
						context.builder.CreateGEP(TablePtr, GEPTableT));
		}

		return TablePtr;
	}

	llvm::Value* UnaryOperation::Codegen(CodegenContext& context) const {
		__TalesTypeIndex OperandTy;

		switch (op) {
			case UnaryOperator::NOT:
				OperandTy = TYPEIDX_BOOL;
				break;
			case UnaryOperator::NEG:
				OperandTy = TYPEIDX_NUMBER;
				break;
		}

		llvm::Value* S = CheckAndConvert(context, OperandTy,
																s->Codegen(context), s->RuntimeType(context.env));

		if (!S)
			return NilValue(context, RuntimeType(context.env));

		switch (op) {
			case UnaryOperator::NOT: return context.builder.CreateNot(S, "nottmp");
			case UnaryOperator::NEG: return context.builder.CreateFNeg(S, "negtmp");
		}
	}

	llvm::Value* BinaryOperation::Codegen(CodegenContext& context) const {
		__TalesTypeIndex OperandTy;

		switch (op) {
			case BinaryOperator::AND:
			case BinaryOperator::OR:
				OperandTy = TYPEIDX_BOOL;
				break;
			default:
				OperandTy = TYPEIDX_NUMBER;
		}

		llvm::Value* L = CheckAndConvert(context, OperandTy,
																lhs->Codegen(context), lhs->RuntimeType(context.env));
		llvm::Value* R = CheckAndConvert(context, OperandTy,
																rhs->Codegen(context), rhs->RuntimeType(context.env));

		if (!L || !R)
			return NilValue(context, RuntimeType(context.env));

		// NOTE: if either L or R's type cannot be converted into a number,
		// L or R will be 0 so beware of divs.
		switch (op) {
			case BinaryOperator::AND: return context.builder.CreateAnd(L, R, "andtmp");
			case BinaryOperator::OR: return context.builder.CreateOr(L, R, "ortmp");
			case BinaryOperator::EQ: return context.builder.CreateFCmpOEQ(L, R, "eqtmp");
			case BinaryOperator::NE: return context.builder.CreateFCmpONE(L, R, "netmp");
			case BinaryOperator::PLUS: return context.builder.CreateFAdd(L, R, "addtmp");
			case BinaryOperator::MINUS: return context.builder.CreateFSub(L, R, "subtmp");
			case BinaryOperator::MULT: return context.builder.CreateFMul(L, R, "multmp");
			case BinaryOperator::DIV: return context.builder.CreateFDiv(L, R, "divtmp");
			case BinaryOperator::GT: return context.builder.CreateFCmpOGT(L, R, "getmp");
			case BinaryOperator::GE: return context.builder.CreateFCmpOGE(L, R, "geemp");
			case BinaryOperator::LT: return context.builder.CreateFCmpOLT(L, R, "letmp");
			case BinaryOperator::LE: return context.builder.CreateFCmpOLE(L, R, "leemp");
			default: return ErrorV("FIXME implement MOD");
		}
	}

	llvm::Value* IfElse::Codegen(CodegenContext& context) const {
		llvm::Value* CondV = CheckAndConvert(context, TYPEIDX_BOOL,
												cond->Codegen(context), cond->RuntimeType(context.env));

		llvm::Function* F = context.builder.GetInsertBlock()->getParent();

		llvm::BasicBlock* ThenBB = llvm::BasicBlock::Create(llvm::getGlobalContext(), "then", F);
		llvm::BasicBlock* ElseBB = _else ?
				llvm::BasicBlock::Create(llvm::getGlobalContext(), "else") : nullptr;
		llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(llvm::getGlobalContext(), "ifcont");

		context.builder.CreateCondBr(CondV, ThenBB, ElseBB ? ElseBB : MergeBB);

		// Emit then block.
		context.builder.SetInsertPoint(ThenBB);
		then->Codegen(context);

		context.builder.CreateBr(MergeBB);
		ThenBB = context.builder.GetInsertBlock();

		// Emit else block if any.
		if (ElseBB) {
			F->getBasicBlockList().push_back(ElseBB);

			context.builder.SetInsertPoint(ElseBB);
			_else->Codegen(context);

			context.builder.CreateBr(MergeBB);
			ElseBB = context.builder.GetInsertBlock();
		}

		// Emit merge block.
		F->getBasicBlockList().push_back(MergeBB);
		context.builder.SetInsertPoint(MergeBB);

		return nullptr;
	}

	template<bool isRepeat>
	llvm::Value* WhileRepeat<isRepeat>::Codegen(CodegenContext& context) const {
		llvm::Value* CondV = CheckAndConvert(context, TYPEIDX_BOOL,
												cond->Codegen(context), cond->RuntimeType(context.env));

		llvm::Function* F = context.builder.GetInsertBlock()->getParent();

		llvm::BasicBlock* HeaderBB;
		if (!isRepeat)
			llvm::BasicBlock* HeaderBB = llvm::BasicBlock::Create(llvm::getGlobalContext(), "whileheader", F);
		llvm::BasicBlock* DoBB = llvm::BasicBlock::Create(llvm::getGlobalContext(), "do");
		llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(llvm::getGlobalContext(), "whilecont");

		if (!isRepeat) {
			context.builder.CreateBr(HeaderBB);

			context.builder.SetInsertPoint(HeaderBB);
			context.builder.CreateCondBr(CondV, DoBB, MergeBB);
		} else
			context.builder.CreateBr(DoBB);

		F->getBasicBlockList().push_back(DoBB);
		context.builder.SetInsertPoint(DoBB);
		_do->Codegen(context);

		if (!isRepeat)
			context.builder.CreateBr(HeaderBB);
		else
			context.builder.CreateCondBr(CondV, DoBB, MergeBB);

		F->getBasicBlockList().push_back(MergeBB);
		context.builder.SetInsertPoint(MergeBB);

		return nullptr;
	}
	template llvm::Value* WhileRepeat<false>::Codegen(CodegenContext& context) const;
	template llvm::Value* WhileRepeat<true>::Codegen(CodegenContext& context) const;

	llvm::Value* For::Codegen(CodegenContext& context) const {
		return nullptr;
	}

	llvm::Value* Block::Codegen(CodegenContext& context) const {
		vector<llvm::Value*> Allocas(locals.size());

		// Allocas and initial assignments
		LocalList::const_iterator l = locals.cbegin();
		for (vector<llvm::Value*>::iterator A = Allocas.begin(), AL = Allocas.end();
							A != AL; ++A, ++l) {
			*A = context.builder.CreateAlloca(l->type.Typegen(context), 0, l->name);

			if (l->initialAssignment != nullptr)
				context.builder.CreateStore(
						CheckAndConvert(context, l->type, l->initialAssignment->Codegen(context),
														l->initialAssignment->RuntimeType(context.env)),
						*A);
		}

		context.env.semanticStack.emplace(locals, Allocas);

		// Statements
		for (StatementList::const_iterator S = statements.cbegin(),
								SL = statements.cend(); S != SL; ++S) {
			(*S)->Codegen(context);
		}

		context.env.semanticStack.pop();

		// NOTE: Lua blocks are more like anonymous functions, they can return
		// a value but that won't make their function return.

		return nullptr;
	}

	llvm::Function* FunctionDeclaration::CodegenFunc(CodegenContext& context) const {
		vector<llvm::Type*> ArgTypes(ftype.args.size());
		ArgumentList::size_type Idx = 0;
		for (; Idx < ftype.args.size(); ++Idx)
			ArgTypes[Idx] = ftype.args[Idx].type.Typegen(context);
		llvm::FunctionType *FT = llvm::FunctionType::get(ftype.rtype.Typegen(context),
																										 ArgTypes, false);

		llvm::Function* F = llvm::Function::Create(FT, llvm::Function::ExternalLinkage,
																							 debugName, context.runtimeModule.module);

		if (!F)
			return nullptr;

		llvm::BasicBlock *PrevBB = context.builder.GetInsertBlock();

		// Create a new basic block to start insertion into.
		llvm::BasicBlock *BB = llvm::BasicBlock::Create(llvm::getGlobalContext(),
																										"entry", F);
		context.builder.SetInsertPoint(BB);

		// If we're the
		if (!context.env.RootV)
			context.env.RootV = context.env.root->Codegen(context);

		// Set names for all arguments.
		Idx = 0;
		vector<llvm::Value*> ArgAllocas(ftype.args.size());
		for (llvm::Function::arg_iterator AI = F->arg_begin(); Idx != ftype.args.size();
				++AI, ++Idx) {
			AI->setName(ftype.args[Idx].name);

			// Create an alloca for this variable.
			ArgAllocas[Idx] = context.builder.CreateAlloca(ArgTypes[Idx], 0,
																										 ftype.args[Idx].name);

			// Store the initial value into the alloca.
			context.builder.CreateStore(AI, ArgAllocas[Idx]);
		}

		context.funcStack.push(this);
		context.env.semanticStack.emplace(ftype.args, ArgAllocas);

		block.Codegen(context);

		context.env.semanticStack.pop();
		context.funcStack.pop();

		// Create a return instruction that might be redundant but will be pruned
		// by LLVM if useless.
		context.builder.CreateRet(NilValue(context, ftype.rtype));

// 		F->dump();

		// Validate the generated code, checking for consistency.
		assert(!verifyFunction(*F) && "llvm::Function verificiation failed, shouldn't happen!");

		// Optimize the function.
		context.fpm.run(*F);

		if (PrevBB)
			context.builder.SetInsertPoint(PrevBB);

		return F;
	}

	// "Manual GEP" to get an element pointer to the first argument type which follows
	// the header in memory.
	inline llvm::Value* FuncVFirstArgEP(CodegenContext& context, llvm::Value* FuncV) {
		return context.builder.CreateIntToPtr(
			context.builder.CreateAdd(
				context.builder.CreatePtrToInt(
						FuncV, context.dataLayout.getIntPtrType(llvm::getGlobalContext())),
						llvm::ConstantInt::get(context.dataLayout.getIntPtrType(llvm::getGlobalContext()),
									context.dataLayout.getTypeAllocSize(context.runtimeModule.FunctionValueT))),
			context.runtimeModule.TypeIndexT->getPointerTo()
		);
	}

	llvm::Value* FunctionDeclaration::Codegen(CodegenContext& context) const {
		// Generation of the actual function
		llvm::Function* F = CodegenFunc(context);

		// Generation of the type-agnostic "glue" function
		vector<llvm::Type*> ArgTypes(ftype.args.size()); ArgumentList::size_type Idx;
		for (Idx = 0; Idx < ftype.args.size(); ++Idx)
			ArgTypes[Idx] = context.runtimeModule.DynamicValueT;
		llvm::FunctionType* GlueFT = llvm::FunctionType::get(
						context.runtimeModule.DynamicValueT, ArgTypes, false);

		llvm::Function* GlueF = llvm::Function::Create(GlueFT, llvm::Function::PrivateLinkage,
																									 "", context.runtimeModule.module);

		llvm::BasicBlock *PrevBB = context.builder.GetInsertBlock();

		llvm::BasicBlock* GlueBB = llvm::BasicBlock::Create(llvm::getGlobalContext(),
																												"entry", GlueF);
		context.builder.SetInsertPoint(GlueBB);

		vector<llvm::Value*> ArgsCnC(ftype.args.size());
		Idx = 0;
		for (llvm::Function::arg_iterator AI = F->arg_begin(); Idx != ftype.args.size();
				++AI, ++Idx)
			ArgsCnC[Idx] = CheckAndConvert(context, ftype.args[Idx].type, AI, TYPEIDX_NIL);

		llvm::Value* RetV = CheckAndConvert(context, TYPEIDX_NIL,
																				context.builder.CreateCall(F, ArgsCnC), ftype.rtype);

		context.builder.CreateRet(RetV);

		assert(!verifyFunction(*GlueF) && "Glue function verification failed!");
		context.fpm.run(*GlueF);

		context.builder.SetInsertPoint(PrevBB);

		// Generation of the function value structure, which contains runtime type info
		// about the return value and arguments along with pointers to functions.
		llvm::StructType* FunctionValueT = context.runtimeModule.FunctionValueT;
		unsigned int allocSize = context.dataLayout.getTypeAllocSize(FunctionValueT)
				+ ftype.args.size() * context.dataLayout.getTypeAllocSize(
																								context.runtimeModule.TypeIndexT);
		// TODO: why not use a LLVM type { header, array } like for tables?

		llvm::Value* FuncV = context.builder.CreateAlloca(
										llvm::PointerType::getUnqual(FunctionValueT));

		llvm::Value* MallocA[1] = { llvm::ConstantInt::get(
					context.runtimeModule.MallocF->getFunctionType()->getParamType(0), allocSize) };
		llvm::Value* MallocR = context.builder.CreateCall(context.runtimeModule.MallocF,
														MallocA, "malloctable");
		context.builder.CreateStore(context.builder.CreatePointerCast(MallocR,
																	llvm::PointerType::getUnqual(FunctionValueT)), FuncV);

		FuncV = context.builder.CreateLoad(FuncV);

		llvm::Value* GEPFuncV[2] = { ConstantFieldIndex(0), ConstantFieldIndex(0) };
		context.builder.CreateStore(ConstantTypeIdx(context, ftype.rtype),
																context.builder.CreateGEP(FuncV, GEPFuncV));
		GEPFuncV[1] = ConstantFieldIndex(1);
		context.builder.CreateStore(context.builder.CreatePointerCast(
								F, FunctionValueT->getElementType(1)),
						context.builder.CreateGEP(FuncV, GEPFuncV));
		GEPFuncV[1] = ConstantFieldIndex(2);
		context.builder.CreateStore(context.builder.CreatePointerCast(
								GlueF, FunctionValueT->getElementType(1)),
						context.builder.CreateGEP(FuncV, GEPFuncV));
		GEPFuncV[1] = ConstantFieldIndex(3);
		context.builder.CreateStore(llvm::ConstantInt::get(
								FunctionValueT->getElementType(3), ftype.args.size()),
						context.builder.CreateGEP(FuncV, GEPFuncV));

		llvm::Value* ArgT_EP = FuncVFirstArgEP(context, FuncV);
		for (Idx = 0; Idx < ftype.args.size(); ++Idx) {
			context.builder.CreateStore(ConstantTypeIdx(context, ftype.args[Idx].type), ArgT_EP);

			ArgT_EP = context.builder.CreateIntToPtr( // Increment the pointer to next arg typeidx
				context.builder.CreateAdd(
					context.builder.CreatePtrToInt(ArgT_EP,
										context.dataLayout.getIntPtrType(llvm::getGlobalContext())),
					llvm::ConstantInt::get(
										context.dataLayout.getIntPtrType(llvm::getGlobalContext()),
										context.dataLayout.getTypeAllocSize(context.runtimeModule.TypeIndexT))
				),
				context.runtimeModule.TypeIndexT->getPointerTo()
			);
		}

		return FuncV;
	}

	llvm::FunctionType* FunctionType::Typegen(CodegenContext& context) const {
		llvm::Type* RetT = rtype.Typegen(context);
		vector<llvm::Type*> ArgsT(args.size());

		for (unsigned i = 0, e = args.size(); i != e; ++i)
			ArgsT[i] = args[i].type.Typegen(context);

		return llvm::FunctionType::get(RetT, ArgsT, false); // FIXME? getPointerTo?
	}

	// CheckAndConvert for external functions, ExpectedTy being a LLVM type this time
	inline llvm::Value* CheckAndConvertExternal(CodegenContext& context,
										llvm::Type* ExpectedTy, llvm::Value* V, __TalesTypeIndex VTy) {
		// « Note that only one instance of a particular type is ever created.
		//   Thus seeing if two types are equal is a matter of doing a trivial pointer
		//   comparison. »
		if (ExpectedTy == Type(VTy).Typegen(context))
			return V;

		// TODO: take the __TalesNumber type choice into account instead of
		// assuming __TalesNumber == float, see type_traits

		if (VTy == TYPEIDX_NIL) { // V is a dynamic value
			llvm::Value* DVto_A[2]; DVto_A[1] = V;
			ReachForDV(context, DVto_A[1], TYPEIDX_NIL, DVto_A[0]);

			if (ExpectedTy->isFloatingPointTy() || ExpectedTy->isIntegerTy()) {
				// Expected type is a number, either integer or floating-point
				V = context.builder.CreateCall(context.runtimeModule.DVtoNF, DVto_A, "dvton");
				if (ExpectedTy == context.builder.getFloatTy())
					return V;
				else if (ExpectedTy == context.builder.getDoubleTy())
					// FIXME: it'd be better to do a dvtodouble
					return context.builder.CreateFPExt(V, ExpectedTy);
				else
					return context.builder.CreateFPToSI(V, ExpectedTy);
			} else if (ExpectedTy == context.builder.getInt8PtrTy()) {
				// We're expecting a char* TODO: std::string and wchar
				return context.builder.CreateCall(context.runtimeModule.DVtoSF, DVto_A, "dvtos");
			} else {
				return ErrorV("FIXME");
			}
		}

		if (ExpectedTy->isIntegerTy() && VTy == TYPEIDX_NUMBER) {
			return context.builder.CreateFPToSI(V, ExpectedTy);
		} else if (ExpectedTy == context.builder.getDoubleTy()  && VTy == TYPEIDX_NUMBER) {
			return context.builder.CreateFPExt(V, ExpectedTy);
		} else if (ExpectedTy == context.builder.getFloatTy() && VTy == TYPEIDX_STRING) {
			llvm::Value* AtofA[1] = { V };
			V = context.builder.CreateCall(context.runtimeModule.AtofF, AtofA, "atof");
			return context.builder.CreateFPTrunc(V, ExpectedTy);
		} else if (ExpectedTy == context.builder.getDoubleTy() && VTy == TYPEIDX_STRING) {
			llvm::Value* AtofA[1] = { V };
			return context.builder.CreateCall(context.runtimeModule.AtofF, AtofA, "atof");
		} else if (ExpectedTy == context.runtimeModule.AtoiF->getReturnType() && VTy == TYPEIDX_STRING) {
			llvm::Value* AtoiA[1] = { V };
			return context.builder.CreateCall(context.runtimeModule.AtoiF, AtoiA, "atoi");
		} else if (ExpectedTy == context.runtimeModule.AtolF->getReturnType() && VTy == TYPEIDX_STRING) {
			llvm::Value* AtolA[1] = { V };
			return context.builder.CreateCall(context.runtimeModule.AtolF, AtolA, "atol");
		} else if (ExpectedTy == context.builder.getInt8PtrTy() && VTy == TYPEIDX_NUMBER) {
			llvm::Value* NtoSA[1] = { V };
			return context.builder.CreateCall(context.runtimeModule.NtoSF, NtoSA, "ntos");
		} else if (ExpectedTy == context.builder.getInt8PtrTy() && VTy == TYPEIDX_BOOL) {
			llvm::Value* BtoSA[1] = { V };
			return context.builder.CreateCall(context.runtimeModule.BtoSF, BtoSA, "btos");
		} else {
			return ErrorV("FIXME?");
		}
	}

	llvm::Value* FunctionCall::Codegen(CodegenContext& context) const {
// 		// First look into the external functions
// 		llvm::Function* ExtF = context.runtimeModule.GetExternalFunction(funcId);

		// Look up the name in the global module table.
		llvm::Function *ExtF = context.execEngine->FindFunctionNamed(funcId.FullPath().c_str());
		// TODO: it shouldn't be ExecutionEngine::FindFunctionNamed but a specific symbol table,
		// d'ailleurs ce n'est pas InstallLazyFunctionCreator non plus, on a besoin de malloc
		// par exemple mais le code Tales n'est pas autorisé à l'utiliser.
		if (ExtF) {
			vector<llvm::Value*> ExtA(callArgs.size());
			for (CallArgumentList::size_type i = 0, e = callArgs.size(); i != e; ++i)
				ExtA[i] = CheckAndConvertExternal(context, ExtF->getFunctionType()->getParamType(i),
													callArgs[i]->Codegen(context), callArgs[i]->RuntimeType(context.env));

			return context.builder.CreateCall(ExtF, ExtA);
		}

		// No external function match, it has to be a Tales function
		const Type& idTy = funcId.RuntimeType(context.env);

		if (idTy != TYPEIDX_FUNCTION && idTy != TYPEIDX_NIL)
			return ErrorV("Not a function");

		if (idTy == TYPEIDX_FUNCTION) {
			// llvm::Function is reachable in the s-tree
			const FunctionType* fTy = idTy.funcType;

			// If argument mismatch error.
			if (fTy->args.size() != callArgs.size())
				return ErrorV("Incorrect number of arguments passed");

			llvm::Value* Idxs[2] = { ConstantFieldIndex(0), ConstantFieldIndex(1) };
			llvm::Value* CalleeV = context.builder.CreateLoad(context.builder.CreateGEP(
																										funcId.Codegen(context), Idxs));
			llvm::Function *CalleeF = cast<llvm::Function>(context.builder.CreatePointerCast(
																										CalleeV, fTy->Typegen(context)));

			vector<llvm::Value*> ArgsV(callArgs.size());
			for (CallArgumentList::size_type i = 0, e = callArgs.size(); i != e; ++i)
				ArgsV[i] = CheckAndConvert(context, fTy->args[i].type,
																	 callArgs[i]->Codegen(context),
																	 callArgs[i]->RuntimeType(context.env));

			return context.builder.CreateCall(CalleeF, ArgsV);
		} else {
			// The identifier returned a dynamic value, so first we've got to tell the IR to
			// check if the argument types match
			llvm::Value* FuncV = funcId.Codegen(context),* FuncVT;
			ReachForDV(context, FuncV, TYPEIDX_NIL, FuncVT);

			llvm::Value* ArgTV_EP = FuncVFirstArgEP(context, FuncV);

			vector<llvm::Constant*> CallArgsTC(callArgs.size());
			for (unsigned i = 0; i < callArgs.size(); ++i)
				CallArgsTC[i] = ConstantTypeIdx(context, callArgs[i]->RuntimeType(context.env));
			llvm::Constant* CallArgsTCArray = llvm::ConstantArray::get(
					llvm::ArrayType::get(context.runtimeModule.TypeIndexT, callArgs.size()),
					CallArgsTC);
			llvm::Value* CallArgsTV = new llvm::GlobalVariable(*context.runtimeModule.module,
																											CallArgsTCArray->getType(), true,
																											llvm::GlobalValue::PrivateLinkage,
																											CallArgsTCArray);

			llvm::Value* FunctionTypecheckA[5] = {
				FuncV, FuncVT, ArgTV_EP,
				llvm::ConstantInt::get(context.runtimeModule.FunctionValueT->getTypeAtIndex(2),
															 callArgs.size()),
				CallArgsTV
			};
			llvm::Value* CheckV = context.builder.CreateCall(
						context.runtimeModule.FunctionTypecheckF,
						FunctionTypecheckA);

			llvm::Function *CurrentFunction = context.builder.GetInsertBlock()->getParent();
			// NOTE: is this necessary? it's in the Kaleidoscope tutorial, but what happens
			// if Parent is left default?

			llvm::BasicBlock *ThenBB = llvm::BasicBlock::Create(llvm::getGlobalContext(),
																													"then", CurrentFunction);
			llvm::BasicBlock *IfcontBB = llvm::BasicBlock::Create(llvm::getGlobalContext(),
																													"ifcont");

			context.builder.CreateCondBr(CheckV, ThenBB, IfcontBB);


			vector<llvm::Value*> ArgsV(callArgs.size());
			for (CallArgumentList::size_type i = 0, e = callArgs.size(); i != e; ++i)
				ArgsV[i] = CheckAndConvert(context, TYPEIDX_NIL, callArgs[i]->Codegen(context),
																	 callArgs[i]->RuntimeType(context.env));

// 			GEPIndexList AgnosticFuncGEP(2, 0, 2);
// 			llvm::Value* CalleeV = context.builder.CreateLoad(context.builder.CreateGEP(FuncV, AgnosticFuncGEP)); // TODO FIXME test if funcId.Codegen(context) == nil
// 			llvm::Function *CalleeF = cast<llvm::Function>(context.builder.CreatePointerCast(CalleeV, ));

// 			assert(false && "OUYA, Agnostic function calls incomplete!");

// 			return context.builder.CreateCall(CalleeF, ArgsV);
		}
	}

	llvm::Value* Return::Codegen(CodegenContext& context) const {
		const FunctionDeclaration* currentFunc = context.funcStack.top();

		return context.builder.CreateRet(CheckAndConvert(context, currentFunc->ftype.rtype,
							returnExpr->Codegen(context), returnExpr->RuntimeType(context.env)));
		// TODO: Tail calls
	}

	struct AssignOnIdentifier : public ActionsOnIdentifier<llvm::Value*> {
		inline static llvm::Value* OnStatic(const Node* node, CodegenContext& context,
																					 const Type& IdTy, llvm::Value* IdV) {
			const ObjectUniquePtr<Expression>& rhs = cast<Assignment>(node)->rhs;

			return context.builder.CreateStore(CheckAndConvert(context, IdTy,
													rhs->Codegen(context), rhs->RuntimeType(context.env)), IdV);
		}

		inline static llvm::Value* OnInvalid(const Node* node, CodegenContext& context) {
			return GetValueOnIdentifier::OnInvalid(node, context);
		}

		inline static llvm::Value* OnUnreachable(const Node* node, CodegenContext& context,
															const Type& levelType, llvm::Value* LevelPtr,
															llvm::Value* RemainingLevelsGV, size_t RemainingLevelsSize) {
			const ObjectUniquePtr<Expression>& rhs = cast<Assignment>(node)->rhs;

			llvm::Value* RHS = rhs->Codegen(context), * RHSTI;
			ReachForDV(context, RHS, rhs->RuntimeType(context.env), RHSTI);

			if (levelType == TYPEIDX_TABLE) {
				llvm::Value* AssignRuntimeTA[5] = {
					LevelPtr,
					RemainingLevelsGV,
					llvm::ConstantInt::get(
								context.runtimeModule.AssignRuntimeTF->getFunctionType()->getParamType(2),
								RemainingLevelsSize),
					ToFPUE(context, RHS),
					RHSTI
				};

				return context.builder.CreateCall(context.runtimeModule.AssignRuntimeTF,
																					AssignRuntimeTA);
			}

			llvm_unreachable("FIXME Assignment::Codegen");
		}
	};

	llvm::Value* Assignment::Codegen(CodegenContext& context) const {
		return EmitActionOnIdentifier<AssignOnIdentifier>(lhs, this, context);
	}
}
