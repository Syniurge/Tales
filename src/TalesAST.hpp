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

#include <cstddef>
#include <stdlib.h>

#include <memory>
#include <array>
#include <map>
#include <vector>
#include <algorithm>

#include <llvm/IR/Value.h>
#include <llvm/IR/DerivedTypes.h>

#include "TalesRuntime.h"

namespace Tales {
	using namespace std;
	using namespace llvm;
	
	struct ASTContext; // parsing time
	struct CodegenEnv;
	struct CodegenContext;

	struct Node {
		virtual ~Node() {}
		virtual Value* Codegen(CodegenContext& context) const = 0;
	};

	struct NExpression : public Node {
		virtual __TalesTypeIndex RuntimeType(CodegenEnv& env) const = 0;
	};
	struct NStatement : public Node {};
	typedef vector<unique_ptr<NExpression>> ExpressionList;
	typedef vector<unique_ptr<NStatement>> StatementList;

	struct NType;
	struct NIdentifier : public NExpression {
		vector<string> levels;
		
		NIdentifier() {}
		NIdentifier(string&& firstLevel) { levels.emplace_back(move(firstLevel)); }
		NIdentifier(NIdentifier &&o) noexcept : levels(move(o.levels)) {}
		NIdentifier& operator=(NIdentifier &&o) { levels = move(o.levels); return *this; }
		
		virtual Value* Codegen(CodegenContext& context) const;
		__TalesTypeIndex RuntimeType(CodegenEnv& env, const NType** ty) const;
		virtual __TalesTypeIndex RuntimeType(CodegenEnv& env) const { return RuntimeType(env, nullptr); }
		
		inline string FullPath() const {
			string fullPath = "";
			for (vector<string>::const_iterator level = levels.cbegin(), last = levels.cend() ; level != last ; level++) {
				fullPath += *level;
			}
			return fullPath;
		}
	};
	
	struct NStructLike;
	struct NFunctionType;
	
	struct NType {
		typedef __TalesTypeIndex Kind;
		
		Kind kind;
		const NStructLike* structDecl;
		const NFunctionType* funcType;
		
		NType() : kind(TYPEIDX_NIL), structDecl(nullptr) {}
		NType(const Kind kind) : kind(kind), structDecl(nullptr) {}
		NType(const Kind kind, const NStructLike* structDecl) : kind(kind), structDecl(structDecl) {}
		NType(NFunctionType* funcType) : kind(TYPEIDX_FUNCTION), structDecl(nullptr), funcType(funcType) {}
		NType(NType&& o) noexcept : kind(o.kind), structDecl(o.structDecl), funcType(o.funcType) {}
		
		operator __TalesTypeIndex() const { return kind; }
		
		NType& operator =(NType&& o) { kind = o.kind; structDecl = o.structDecl; funcType = o.funcType; return *this; }
// 		NType& operator =(NType& o) { kind = o.kind; structDecl = o.structDecl; funcType = o.funcType; return *this; }
		
		NType(ASTContext& ast, const string& className);
		void SetClass(ASTContext& ast, const string& className);
		
		// Deduces the structDecl and funcType from the initial assignments of locals, table spairs, class fields
		inline void RefineFromExpr(const NExpression* expr);
		
		Type* Typegen(CodegenContext& context) const;
	};

	struct NNumber : public NExpression {
		const Number value;
		
		NNumber(Number value) : value(value) {}
		virtual Value* Codegen(CodegenContext& context) const;
		virtual __TalesTypeIndex RuntimeType(CodegenEnv& env) const { return TYPEIDX_NUMBER; }
	};

	struct NString : public NExpression {
		const string text;
		
		NString(string&& text) : text(move(text)) {}
		virtual Value* Codegen(CodegenContext& context) const;
		virtual __TalesTypeIndex RuntimeType(CodegenEnv& env) const { return TYPEIDX_STRING; }
	};
	
	// Class fields, table spairs, block locals or function arguments, aka all mutable values that are known at compilation time
	struct NMutable {
		NType type;
		string name;
		unique_ptr<NExpression> initialAssignment;
		
		NMutable() {}
		NMutable(NType&& type, string&& name) : type(move(type)), name(move(name)), initialAssignment(nullptr) {}
		NMutable(NType&& type, string&& name, unique_ptr<NExpression>&& initialAssignment) : type(move(type)), name(move(name)), initialAssignment(move(initialAssignment)) {}
		NMutable(NMutable&& o) noexcept : type(move(o.type)), name(move(o.name)), initialAssignment(move(o.initialAssignment)) {}
	};
	
	struct LexicalMutables : public vector<NMutable> {}; // "lexical/semantic level"
	
	typedef LexicalMutables::size_type FieldIndex;
	struct FieldList : public LexicalMutables {
		// Generates the LLVM struct type following the header in tables and class instances
		StructType* Typegen(CodegenContext& context) const;
		
// 		// Generates the LLVM IR instructions to initialize the fields, it has to be provided the pointer to the struct in memory (follows a table or class header)
// 		Value* Codegen(CodegenContext& context) const;
		
		// Returns the right index for the GEP instruction
		bool FindFieldIndex(const string& fieldName, FieldIndex& fieldIndex) const {
			for (fieldIndex = 0; fieldIndex < size(); ++fieldIndex)
				if (at(fieldIndex).name == fieldName)
					return true;
			return false;
		}
		
		FieldList& operator =(FieldList&&) = default;
		FieldList& operator =(FieldList&) = delete; // a safeguard
	};
	
	struct NStructLike : public NExpression {
		FieldList fields; // a.k.a spairs in Tales' tables there are the fixed struct-like pairs, and the LUA-like pairs
	};
	
	struct NTable : public NStructLike {
		ExpressionList ipairs;
		
		virtual Value* Codegen(CodegenContext& context) const;
		Type* Typegen(Tales::CodegenContext& context) const;
		virtual __TalesTypeIndex RuntimeType(CodegenEnv& env) const { return TYPEIDX_TABLE; }
	};
	
	struct NClassDeclaration : public NStructLike {
		NClassDeclaration(FieldList&& _fields) { fields = move(_fields); }
		virtual Value* Codegen(CodegenContext& context) const { return nullptr; }
		StructType* Typegen(CodegenContext& context) const { return nullptr; }
		virtual __TalesTypeIndex RuntimeType(CodegenEnv& env) const { return TYPEIDX_CLASSINST; }
	};

	struct NBinaryOperation : public NExpression {
		enum Operator {
			NOT, AND, OR,
			EQ, NE, LT, LE, GT, GE,
			PLUS, MINUS, MULT, DIV, MOD
		};
		const Operator op;
		const unique_ptr<NExpression> lhs;
		const unique_ptr<NExpression> rhs;
		
		NBinaryOperation(unique_ptr<NExpression>&& lhs, const Operator op, unique_ptr<NExpression>&& rhs) : lhs(move(lhs)), rhs(move(rhs)), op(op) {}
		virtual Value* Codegen(CodegenContext& context) const;
		virtual __TalesTypeIndex RuntimeType(CodegenEnv& env) const {
			switch(op) {
				default:
					return TYPEIDX_NUMBER;
			}
		}
	};

	struct NBlock : public NStatement {
		typedef LexicalMutables LocalList;
		
		LocalList locals;
		StatementList statements;
		
		NBlock() {}
		NBlock(NBlock&& o) : locals(move(o.locals)), statements(move(o.statements)) {}
		NBlock& operator=(NBlock &&o) { locals = move(o.locals); statements = move(o.statements); return *this; }
		virtual Value* Codegen(CodegenContext& context) const;
	};

	struct NAssignment : public NStatement {
		const NIdentifier lhs;
		const unique_ptr<NExpression> rhs;
		
		NAssignment(NIdentifier&& lhs, unique_ptr<NExpression>&& rhs) : lhs(move(lhs)), rhs(move(rhs)) {}
		virtual Value* Codegen(CodegenContext& context) const;
	};
	
	struct NReturn : public NStatement {
		const unique_ptr<NExpression> returnExpr;
		
		NReturn(unique_ptr<NExpression>&& returnExpr) : returnExpr(move(returnExpr)) {}
		virtual Value* Codegen(CodegenContext& context) const;
	};
	
	struct NIfElse : public NStatement {
		const unique_ptr<NExpression> cond;
		const unique_ptr<NStatement> then;
		const unique_ptr<NStatement> _else;
		
		NIfElse(unique_ptr<NExpression>&& cond, unique_ptr<NStatement>&& then, unique_ptr<NStatement>&& _else) : cond(move(cond)), then(move(then)), _else(move(_else)) {}
		virtual Value *Codegen(CodegenContext& context) const;
	};
	
	struct NFunctionType {
		typedef LexicalMutables ArgumentList;
	
		const NType rtype;
		const ArgumentList args;
		
		NFunctionType() : args() {}
		NFunctionType(ArgumentList&& args) : args(move(args)) {}
		NFunctionType(NType&& rtype, ArgumentList&& args) : rtype(move(rtype)), args(move(args)) {}
		FunctionType* Typegen(CodegenContext& context) const;  // returns the function pointer type
	};

	struct NFunctionDeclaration : public NExpression {
		string debugName;
		
		const NFunctionType ftype;
		const NBlock block;
		
		NFunctionDeclaration(NBlock&& block) : block(move(block)) {}
		NFunctionDeclaration(NFunctionType::ArgumentList&& args, NBlock&& block) : ftype(move(args)), block(move(block)) {}
		NFunctionDeclaration(NType&& type, NFunctionType::ArgumentList&& args, NBlock&& block) : ftype(move(type), move(args)), block(move(block)) {}
		Function* CodegenFunc(CodegenContext& context) const; // this is the function that must be called for the main block
		virtual Value* Codegen(CodegenContext& context) const;
		virtual __TalesTypeIndex RuntimeType(CodegenEnv& env) const { return TYPEIDX_FUNCTION; }
	};

	struct NFunctionCall : public NExpression, public NStatement {
		typedef ExpressionList CallArgumentList;
		
		const NIdentifier funcId;
		const CallArgumentList callArgs;
		
		NFunctionCall(NIdentifier&& funcId, CallArgumentList&& callArgs) : funcId(move(funcId)), callArgs(move(callArgs)) {}
		virtual Value* Codegen(CodegenContext& context) const;
		virtual __TalesTypeIndex RuntimeType(CodegenEnv& env) const { return funcId.RuntimeType(env); } // function return type if sfield (reachable), determined from the prototype header at runtime otherwise
	};
	
	struct ASTContext {
		map<string, NClassDeclaration*> classMap;
		
		unique_ptr<NTable> root;  // will be initialized as an empty table by the parser if nullptr
		unique_ptr<NFunctionDeclaration> parsedChunk;
		
		ASTContext() {}
		ASTContext(NTable* root) : root(root) {}
	};
	
	// FIXME: const or not const is a bit of a mess and should be flattened to const everywhere someday if possible
	
	inline void NType::RefineFromExpr(const NExpression* expr) { 
		if (const NStructLike* _structDecl = dynamic_cast<const NStructLike*>(expr))
			structDecl = _structDecl;
		if (const NFunctionDeclaration* func = dynamic_cast<const NFunctionDeclaration*>(expr))
			funcType = &func->ftype;
	}

#ifdef TalesCodegen
	NType::NType(ASTContext& ast, const string& className) : kind(TYPEIDX_CLASSINST) {
		structDecl = ast.classMap[className];
	}
	void NType::SetClass(ASTContext& ast, const string& className) {
		kind = TYPEIDX_CLASSINST;
		structDecl = ast.classMap[className];
	}
#endif
}