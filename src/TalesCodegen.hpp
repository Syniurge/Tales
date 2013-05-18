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

#pragma once

#include "TalesAST.hpp"

#include <stack>
#include <utility>

#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/JIT.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/PassManager.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Transforms/IPO.h"

namespace Tales {
	using namespace std;
	using namespace llvm;
	
	struct TalesModule {
		Module* module;
		SMDiagnostic smDiag;
		
		// LLVM Struct types retrieved after parsing TalesRuntime.c IR
		StructType* FPUT;
		StructType* DynamicValueT;
		StructType* ChainPairT;
		StructType* StructFieldT;
		StructType* TableHeaderT;
		StructType* FunctionValueT;
		
		IntegerType* TypeIndexT;
		Type* NumberT;
		
		// LLVM Functions from TalesRuntime.c IR, most of them are inlined
		
		Function* NtoSF;
		Function* DVtoNF;
		Function* DVtoSF;
		Function* DVtoPF;
		
		Function* CheckAndConvertP;
		Function* CheckAndConvertN;
		
		Function* AssignRuntimeTF;
		Function* GetTF;
		Function* FunctionTypecheckF;
		
		Function* MallocF;
		Function* AtofF;
		Function* AtoiF;
		Function* AtolF;
		Function* StrdupF;
		
		PassManager pm;
		
	private:
		bool _Load();
		TalesModule() {}
		
	public:
		static TalesModule* Load() {
			TalesModule* talesModule = new TalesModule;
			
			if (!talesModule->_Load()) {
				delete talesModule;
				return nullptr;
			}
			
			return talesModule;
		}
	};
	
	struct SemanticLevel { // or is it lexical? it never sticks..
		const SemanticLevel* parent;
		
		const LexicalMutables& mutables;
		vector<Value*>& MutablesV;
		
		SemanticLevel(const LexicalMutables& mutables, vector<Value*>& MutablesV, const SemanticLevel* parent) : mutables(mutables), MutablesV(MutablesV), parent(parent) {}
// 		SemanticLevel(SemanticLevel &&o) noexcept : mutables(move(o.mutables)), MutablesV(move(o.MutablesV)), parent(o.parent) {}
	};
	
	struct SemanticStack : public stack<unique_ptr<SemanticLevel>> {
		void emplace(const LexicalMutables& mutables, vector<Value*>& MutablesV) { stack<unique_ptr<SemanticLevel>>::emplace(new SemanticLevel(mutables, MutablesV,  empty()?nullptr:top().get())); }
		
		pair<const NMutable*, Value*> FindMutable(const string& name) {
			if (empty()) return pair<const NMutable*, Value*>(nullptr, nullptr);
			
			const SemanticLevel* sLevel = top().get();
			do {
				for (LexicalMutables::const_iterator m = sLevel->mutables.cbegin(), ml = sLevel->mutables.cend(); m != ml; ++m) {
					if (m->name == name)
						return pair<const NMutable*, Value*>(&(*m), sLevel->MutablesV[m - sLevel->mutables.cbegin()]); // WARNING: found the &(*const_iterator) trick on stackoverflow, kinda scary
				}
				sLevel = sLevel->parent;
			} while (sLevel != nullptr);
			
			return pair<const NMutable*, Value*>(nullptr, nullptr); // no local/upvalue/argument/class field was found
		}
	};
	
	// As with Lua, be aware that the compilation of a function varies with the root table
	// NOTE FIXME: but the upvalues/fields/... are always the same, so maybe they shouldn't be at codegen time but at parsing time
	struct CodegenEnv {
		const NTable* root;
		Value* RootV;
		
		SemanticStack semanticStack;
		
		CodegenEnv() : RootV(nullptr) {}
	};
	
	struct CodegenContext {
		TalesModule& talesModule;
		
		IRBuilder<> builder;
		FunctionPassManager fpm;
		ExecutionEngine* execEngine;
		const DataLayout& dataLayout;
		
		CodegenEnv env;
		stack<const NFunctionDeclaration*> funcStack;
	
		CodegenContext(TalesModule& talesModule, ExecutionEngine* execEngine);
	};
}