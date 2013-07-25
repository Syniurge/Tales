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

#include <utility>
#include <stack>

#include "TalesAST.hpp"

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
#include "llvm/Support/raw_ostream.h"

namespace Tales {
	using std::stack;
	using std::pair;
	using std::unique_ptr;

	using llvm::StringRef;

	struct RuntimeModule {
		llvm::Module* module;
		llvm::SMDiagnostic smDiag;

		// LLVM Struct types retrieved after parsing TalesRuntime.c IR
		llvm::StructType* FPUT;
		llvm::StructType* DynamicValueT;
		llvm::StructType* ChainPairT;
		llvm::StructType* StructFieldT;
		llvm::StructType* TableHeaderT;
		llvm::StructType* FunctionValueT;

		llvm::IntegerType* TypeIndexT;
		llvm::Type* NumberT;

		// LLVM Functions from TalesRuntime.c IR, most of them are inlined

		llvm::Function* EmptySF;
		llvm::Function* EmptyTF;

		llvm::Function* NtoSF;
		llvm::Function* BtoNF;
		llvm::Function* BtoSF;
		llvm::Function* DVtoNF;
		llvm::Function* DVtoSF;
		llvm::Function* DVtoBF;
		llvm::Function* DVtoPF;

		llvm::Function* AssignRuntimeTF;
		llvm::Function* GetTF;
		llvm::Function* FunctionTypecheckF;

		llvm::Function* MallocF;
		llvm::Function* AtofF;
		llvm::Function* AtoiF;
		llvm::Function* AtolF;
		llvm::Function* StrdupF;

		llvm::PassManager pm;

	private:
		bool _Load();
		RuntimeModule() {}

		inline llvm::StructType* getTypeByName(StringRef Name) {
			llvm::StructType* result = module->getTypeByName(Name);
			assert(result && "RuntimeModule::getTypeByName failed!");

			return result;
		}

		inline llvm::Function* getFunction(StringRef Name) {
			llvm::Function* result = module->getFunction(Name);
			assert(result && "RuntimeModule::getFunction failed!");

			return result;
		}

	public:
		static RuntimeModule* Load() {
			RuntimeModule* talesModule = new RuntimeModule;

			if (!talesModule->_Load()) {
				delete talesModule;
				return nullptr;
			}

			return talesModule;
		}
	};

	struct SemanticLevel { // or is it lexical? it never sticks..
		const SemanticLevel* parent;

		const LexicalContext& mutables;
		vector<llvm::Value*>& MutablesV;

		SemanticLevel(const LexicalContext& mutables,
									vector<llvm::Value*>& MutablesV,
									const SemanticLevel* parent)
				: mutables(mutables), MutablesV(MutablesV), parent(parent) {}
		SemanticLevel(SemanticLevel &&o) noexcept = default;
	};

	struct SemanticStack : public stack<unique_ptr<SemanticLevel>> {
		void emplace(const LexicalContext& mutables,
								 vector<llvm::Value*>& MutablesV) {
			stack<unique_ptr<SemanticLevel>>::emplace
				( new SemanticLevel(mutables, MutablesV,
													empty() ? nullptr : top().get()) );
		}

		pair<const Mutable*, llvm::Value*> FindMutable(const string& name) {
			if (empty()) return pair<const Mutable*, llvm::Value*>(nullptr, nullptr);

			const SemanticLevel* sLevel = top().get();
			do {
				for (LexicalContext::const_iterator m = sLevel->mutables.cbegin(),
								ml = sLevel->mutables.cend(); m != ml; ++m) {
					if (m->name == name)
						return pair<const Mutable*, llvm::Value*>
							(&(*m), sLevel->MutablesV[m - sLevel->mutables.cbegin()]);
						// WARNING: I found the &(*const_iterator) trick on stackoverflow, kinda scary
				}
				sLevel = sLevel->parent;
			} while (sLevel != nullptr);

			// No local/upvalue/argument/class field was found
			return pair<const Mutable*, llvm::Value*>(nullptr, nullptr);
		}
	};

	// As with Lua, be aware that the compilation of a function varies with the root table
	// NOTE FIXME: but the upvalues/fields/... are always the same, so maybe they
	// shouldn't be at codegen time but at parsing time.
	struct CodegenEnv {
		const Table* root;
		llvm::Value* RootV;

		SemanticStack semanticStack;

		CodegenEnv() : RootV(nullptr) {}
	};

	struct CodegenContext {
		RuntimeModule& runtimeModule;

		llvm::IRBuilder<> builder;
		llvm::FunctionPassManager fpm;
		llvm::ExecutionEngine* execEngine;
		const llvm::DataLayout& dataLayout;

		CodegenEnv env;

		stack<const FunctionDeclaration*> funcStack;

		CodegenContext(RuntimeModule& talesModule,
									llvm::ExecutionEngine* execEngine);
	};
}

