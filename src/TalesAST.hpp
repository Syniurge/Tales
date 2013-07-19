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

#include <array>
#include <map>
#include <vector>
#include <algorithm>

#include <llvm/Support/Casting.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/DerivedTypes.h>

#include "TalesRTTI.hpp"
#include "TalesRuntime.h"

namespace Tales {
	using std::string;
	using std::vector;
	using std::map;
	using std::move;

	using llvm::cast;
	using llvm::dyn_cast;
	
	struct ASTContext;
	struct CodegenContext;
	struct CodegenEnv;

	struct Node : public Base {
		Node(ObjectKind K) : Base(K) {}
		virtual llvm::Value* Codegen(CodegenContext& context) const = 0;
	};

	struct Expression : public Node {
		Expression(ObjectKind K) : Node(K) {}
		virtual __TalesTypeIndex RuntimeType(CodegenEnv& env) const = 0;

		static bool classof(const Base *B) {
			return B->getKind() >= ObjectKind::Expression
					&& B->getKind() <= ObjectKind::BinaryOperation;
		}
	};
	struct Statement : public Node {
		Statement(ObjectKind K) : Node(K) {}

		static bool classof(const Base *B) {
			return B->getKind() >= ObjectKind::Statement
					&& B->getKind() <= ObjectKind::IfElse;
		}
	};
	typedef vector<ObjectUniquePtr<Expression>> ExpressionList;
	typedef vector<ObjectUniquePtr<Statement>> StatementList;

	struct Type;
	struct Identifier : public Expression {
		struct Level {
			string name;
			ObjectUniquePtr<Expression> expr;

			Level() = delete;
			Level(string&& name) : name(move(name)) {}
			Level(string&& name, ObjectUniquePtrHandle&& expr) : name(move(name)),
					expr(move(expr)) {}
			Level(Level&& o) noexcept = default;
		};
		typedef vector<Level> Levels;
		Levels levels;
		
		Identifier() : Expression(ObjectKind::Identifier) {}
		Identifier(Identifier&& o) noexcept = default;
		static bool classof(const Base *B) {
			return B->getKind() == ObjectKind::Identifier;
		}

// 		Identifier& operator=(Identifier&& o) { levels = move(o.levels); return *this; }
		
		virtual llvm::Value* Codegen(CodegenContext& context) const;
		__TalesTypeIndex RuntimeType(CodegenEnv& env, const Type** ty) const;
		virtual __TalesTypeIndex RuntimeType(CodegenEnv& env) const {
			return RuntimeType(env, nullptr);
		}
		
		inline string FullPath() const {
			string fullPath = "";
			for (auto& level : levels) {
				fullPath += level.name;
// 				if (level.expr) {
// 					fullPath += "[";
// 					fullPath +=
// 					fullPath += "]";
// 				}
			}
			return fullPath;
		}
	};
	
	struct StructLike;
	struct FunctionType;
	
	struct Type : public Base {
		typedef __TalesTypeIndex Kind;
		
		const Kind kind = TYPEIDX_NIL;
		const StructLike* structDecl = nullptr;
		const FunctionType* funcType = nullptr;
		
		Type() : Base(ObjectKind::Type) {}
		Type(Kind kind) : Base(ObjectKind::Type), kind(kind) {}

		// Deduces the structDecl and funcType from the initial assignments of locals,
		// table spairs, class fields
		Type(Kind kind, const Expression* expr);

		Type(Type&& o) noexcept = default;

		static bool classof(const Base *B) {
			return B->getKind() == ObjectKind::Type;
		}
		
		operator __TalesTypeIndex() const { return kind; }
		
		Type(ASTContext& ast, const string& className);
// 		void SetClass(ASTContext& ast, const string& className);
		
		llvm::Type* Typegen(CodegenContext& context) const;
	};

	struct Number : public Expression {
		__TalesNumber value = 0.0f;
		
		Number(__TalesNumber value) : Expression(ObjectKind::Number), value(value) {}
		static bool classof(const Base *B) {
			return B->getKind() == ObjectKind::Number;
		}

		virtual llvm::Value* Codegen(CodegenContext& context) const;
		virtual __TalesTypeIndex RuntimeType(CodegenEnv& env) const {
			return TYPEIDX_NUMBER;
		}
	};

	struct String : public Expression {
		string text;
		
		String(string&& text) : Expression(ObjectKind::String), text(text) {}
		static bool classof(const Base *B) { return B->getKind() == ObjectKind::String; }

		virtual llvm::Value* Codegen(CodegenContext& context) const;
		virtual __TalesTypeIndex RuntimeType(CodegenEnv& env) const { return TYPEIDX_STRING; }
	};
	
	// Class fields, table spairs, block locals or function arguments, aka all mutable values
	// that are known at compilation time.
	struct Mutable : public Base {
		string name;
		ObjectUniquePtr<Expression> initialAssignment;
		Type type;
		
		Mutable(string&& name) : Base(ObjectKind::Mutable), name(move(name)) {}
		Mutable(Type&& type, string&& name) : Base(ObjectKind::Mutable),
				type(move(type)), name(move(name)) {}
		Mutable(string&& name, ObjectUniquePtrHandle&& initialAssignment)
				: Base(ObjectKind::Mutable), name(move(name)),
				  initialAssignment(move(initialAssignment)) {}
		Mutable(Type&& type, string&& name, ObjectUniquePtrHandle&& initialAssignment)
				: Base(ObjectKind::Mutable), name(move(name)),
				  initialAssignment(move(initialAssignment)), type(type.kind, Mutable::initialAssignment.get()) {}
		Mutable(Mutable&& o) noexcept = default;

		static bool classof(const Base *B) { return B->getKind() == ObjectKind::Mutable;	}
	};
	
	struct LexicalContext : public vector<Mutable> { // a "lexical/semantic level"
// 		LexicalContext* parent;
//
// 		LexicalContext(LexicalContext* parent) : parent(parent) {}
		LexicalContext() = default;
		LexicalContext(LexicalContext&) = delete;
		LexicalContext(LexicalContext&&) = default;
		LexicalContext& operator=(LexicalContext&) = delete;
		LexicalContext& operator=(LexicalContext&&) = default;
	};

	struct FieldList : public LexicalContext, public Base {
		typedef LexicalContext::size_type Index;

		FieldList() : Base(ObjectKind::FieldList) {}
		FieldList(FieldList&& o) = default;

		// Generates the LLVM struct type following the header in tables and class instances
		llvm::StructType* Typegen(CodegenContext& context) const;
		
// 		// Generates the LLVM IR instructions to initialize the fields, it has to be provided the pointer to the struct in memory (follows a table or class header)
// 		llvm::Value* Codegen(CodegenContext& context) const;
		
		// Returns the right index for the GEP instruction
		bool FindFieldIndex(const string& fieldName, Index& fieldIndex) const {
			for (fieldIndex = 0; fieldIndex < size(); ++fieldIndex)
				if (at(fieldIndex).name == fieldName)
					return true;
			return false;
		}
		
		FieldList& operator=(FieldList&&) = default;
		FieldList& operator=(FieldList&) = delete; // a safeguard
	};
	
	struct StructLike : public Expression {
		FieldList fields; // a.k.a spairs in Tales' tables there are the fixed struct-like pairs,
				// and the LUA-like pairs

		StructLike(ObjectKind K) : Expression(K) {}
		StructLike(ObjectKind K, FieldList&& fields) : Expression(K), fields(move(fields)) {}

		static bool classof(const Base *B) {
			return B->getKind() >= ObjectKind::Table
					&& B->getKind() <= ObjectKind::ClassDeclaration;
		}
	};
	
	struct Table : public StructLike {
		ExpressionList ipairs;

		Table() : StructLike(ObjectKind::Table) {}
		static bool classof(const Base *B) {
			return B->getKind() == ObjectKind::Table;
		}
		
		virtual llvm::Value* Codegen(CodegenContext& context) const;
		llvm::Type* Typegen(Tales::CodegenContext& context) const;
		virtual __TalesTypeIndex RuntimeType(CodegenEnv& env) const {
			return TYPEIDX_TABLE;
		}
	};
	
	struct ClassDeclaration : public StructLike {
		ClassDeclaration(FieldList&& fields)
				: StructLike(ObjectKind::ClassDeclaration, move(fields)) {}
		static bool classof(const Base *B) {
			return B->getKind() == ObjectKind::ClassDeclaration;
		}

		virtual llvm::Value* Codegen(CodegenContext& context) const { return nullptr; }
		llvm::StructType* Typegen(CodegenContext& context) const { return nullptr; }
		virtual __TalesTypeIndex RuntimeType(CodegenEnv& env) const {
			return TYPEIDX_CLASSINST;
		}
	};

	enum class Operator {
		NOT, AND, OR,
		EQ, NE, LT, LE, GT, GE,
		PLUS, MINUS, MULT, DIV, MOD
	};

	struct BinaryOperation : public Expression {
		const Operator op;
		ObjectUniquePtr<Expression> lhs;
		ObjectUniquePtr<Expression> rhs;
		
		BinaryOperation(ObjectUniquePtrHandle&& lhs, const Operator op,
										ObjectUniquePtrHandle&& rhs)
			: Expression(ObjectKind::BinaryOperation), lhs(move(lhs)), rhs(move(rhs)), op(op) {}
		static bool classof(const Base *B) {
			return B->getKind() == ObjectKind::BinaryOperation;
		}

		virtual llvm::Value* Codegen(CodegenContext& context) const;
		virtual __TalesTypeIndex RuntimeType(CodegenEnv& env) const {
			switch(op) {
				default:
					return TYPEIDX_NUMBER;
			}
		}
	};

	struct Block : public Statement {
		typedef LexicalContext LocalList;
		
		LocalList locals;
		StatementList statements;
		
		Block() : Statement(ObjectKind::Block) {}
		Block(Block&& o) = default;
		static bool classof(const Base *B) {
			return B->getKind() == ObjectKind::Block;
		}

		virtual llvm::Value* Codegen(CodegenContext& context) const;
	};

	struct Assignment : public Statement {
		Identifier lhs;
		ObjectUniquePtr<Expression> rhs;
		
		Assignment(Identifier&& lhs, ObjectUniquePtrHandle&& rhs)
			: Statement(ObjectKind::Assignment), lhs(move(lhs)), rhs(move(rhs)) {}
		static bool classof(const Base *B) {
			return B->getKind() == ObjectKind::Assignment;
		}

		virtual llvm::Value* Codegen(CodegenContext& context) const;
	};
	
	struct Return : public Statement {
		ObjectUniquePtr<Expression> returnExpr;
		
		Return(ObjectUniquePtrHandle&& returnExpr) : Statement(ObjectKind::Return),
							returnExpr(move(returnExpr)) {}
		static bool classof(const Base *B) {
			return B->getKind() == ObjectKind::Return;
		}

		virtual llvm::Value* Codegen(CodegenContext& context) const;
	};
	
	struct IfElse : public Statement {
		ObjectUniquePtr<Expression> cond;
		ObjectUniquePtr<Statement> then;
		ObjectUniquePtr<Statement> _else;
		
		IfElse(ObjectUniquePtr<Expression>&& cond, ObjectUniquePtr<Statement>&& then,
											ObjectUniquePtr<Statement>&& _else)
				: Statement(ObjectKind::IfElse), cond(move(cond)),
				  then(move(then)), _else(move(_else)) {}
		static bool classof(const Base *B) {
			return B->getKind() == ObjectKind::IfElse;
		}

		virtual llvm::Value *Codegen(CodegenContext& context) const;
	};

	struct ArgumentList : public LexicalContext, public Base {
		ArgumentList() : Base(ObjectKind::ArgumentList) {}

		static bool classof(const Base *B) {
			return B->getKind() == ObjectKind::ArgumentList;
		}
	};

	struct FunctionType {
		Type rtype;
		ArgumentList args;
		
		FunctionType() : args() {}
		FunctionType(ArgumentList&& args) : args(move(args)) {}
		FunctionType(Type&& rtype, ArgumentList&& args) : rtype(move(rtype)),
				args(move(args)) {}

		llvm::FunctionType* Typegen(CodegenContext& context) const;
	};

	struct FunctionDeclaration : public Expression {
		string debugName;
		
		FunctionType ftype;
		Block block;
		
		FunctionDeclaration(Block&& block, const string debugName = "")
				: Expression(ObjectKind::FunctionDeclaration), block(move(block)),
								debugName(debugName) {}
		FunctionDeclaration(ArgumentList&& args, Block&& block,
												const string debugName = "")
				: Expression(ObjectKind::FunctionDeclaration), ftype(move(args)),
								block(move(block)), debugName(debugName) {}
		FunctionDeclaration(Type&& type, ArgumentList&& args,
												Block&& block, const string debugName = "")
				: Expression(ObjectKind::FunctionDeclaration), ftype(move(type), move(args)),
								block(move(block)), debugName(debugName) {}
		static bool classof(const Base *B) {
			return B->getKind() == ObjectKind::FunctionDeclaration;
		}

		// Called by Codegen to generate the "strongly typed" function
		llvm::Function* CodegenFunc(CodegenContext& context) const;
		virtual llvm::Value* Codegen(CodegenContext& context) const;
		virtual __TalesTypeIndex RuntimeType(CodegenEnv& env) const { return TYPEIDX_FUNCTION; }
	};

	struct CallArgumentList : public ExpressionList, public Base {
		CallArgumentList() : Base(ObjectKind::CallArgumentList) {}

		static bool classof(const Base *B) {
			return B->getKind() == ObjectKind::CallArgumentList;
		}
	};

	// NOTE: multiple inheritance poses problems with LLVM RTTI, hence
	// the split of FunctionCall into two very similar classes.

	struct FunctionCall {
		Identifier funcId;
		CallArgumentList callArgs;
		
		FunctionCall(Identifier&& funcId, CallArgumentList&& callArgs)
				: funcId(move(funcId)), callArgs(move(callArgs)) {}

		llvm::Value* Codegen(CodegenContext& context) const;
	};

	struct FunctionCallExpr : public FunctionCall, public Expression {
		FunctionCallExpr(Identifier&& funcId, CallArgumentList&& callArgs)
				: Expression(ObjectKind::FunctionCallExpr),
					FunctionCall(move(funcId), move(callArgs)) {}

		static bool classof(const Base *B) {
			return B->getKind() == ObjectKind::FunctionCallExpr;
		}

		virtual llvm::Value* Codegen(CodegenContext& context) const {
			return FunctionCall::Codegen(context);
		}

		virtual __TalesTypeIndex RuntimeType(CodegenEnv& env) const {
			// Function return type if sfield (reachable), determined from
			// the prototype header at runtime otherwise.
			return funcId.RuntimeType(env);
		}
	};

	struct FunctionCallStmt : public FunctionCall, public Statement {
		FunctionCallStmt(Identifier&& funcId, CallArgumentList&& callArgs)
				: Statement(ObjectKind::FunctionCallStmt),
					FunctionCall(move(funcId), move(callArgs)) {}

		static bool classof(const Base *B) {
			return B->getKind() == ObjectKind::FunctionCallStmt;
		}

		virtual llvm::Value* Codegen(CodegenContext& context) const {
			return FunctionCall::Codegen(context);
		}
	};
	
	struct ASTContext {
		map<string, ClassDeclaration*> classMap;
		
		ObjectUniquePtr<Table> root;  // will be initialized as an empty table by the parser if nullptr
		ObjectUniquePtr<FunctionDeclaration> parsedChunk;
		
		ASTContext() {}
		ASTContext(ObjectUniquePtrHandle&& root) : root(move(root)) {}
	};
}
