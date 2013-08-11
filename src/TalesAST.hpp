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

#include "llvm/Support/Casting.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/DerivedTypes.h"

#include "TalesRTTI.hpp"
#include "TalesRuntime.h"

namespace Tales {
  using std::string;
  using std::vector;
  using std::map;
  using std::move;
  using std::pair;

  using llvm::isa;
  using llvm::cast;
  using llvm::dyn_cast;

  struct ASTContext;
  struct CodegenContext;
  struct CodegenEnv;

  struct Type;

  struct Node : public Base {
    Node(ObjectKind K) : Base(K) {}
    virtual llvm::Value* Codegen(CodegenContext& context) const = 0;
  };

  struct Expression : public Node {
    Expression(ObjectKind K) : Node(K) {}
    virtual const Type& RuntimeType(CodegenEnv& env) const = 0;

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

  struct StructLike;
  struct FunctionType;

  struct Type : public Base {
    typedef __TalesTypeIndex Kind;

    const Kind kind = TYPEIDX_NIL;
    const StructLike* structDecl = nullptr;
    const FunctionType* funcType = nullptr;

    Type() : Base(ObjectKind::Type) {}
    Type(Kind kind) : Base(ObjectKind::Type), kind(kind) {}

// 		Type(ASTContext& ast, const string& className);

    // Deduces the structDecl and funcType from the initial assignments of locals,
    // table spairs, class fields
    Type(Kind kind, const Expression* expr);

    Type(Type&& o) noexcept = default;

    static bool classof(const Base *B) {
      return B->getKind() == ObjectKind::Type;
    }

    operator __TalesTypeIndex() const { return kind; }

    llvm::Type* Typegen(CodegenContext& context) const;

    static const Type DynamicValue;
    static const Type Boolean;
    static const Type Number;
    static const Type String;

    // A reference type that can be reassigned.
    // If I ever need more than one it could be put inside a template.
    struct Ref {
      const Type* type = nullptr;

      Ref() {}
      Ref(const Type& type) : type(&type) {}

      Ref& operator=(const Type& type) {
        Ref::type = &type; return *this;
      }

      operator const Type&() const {
        assert(type && "Null reference");
        return *type;
      }

      operator __TalesTypeIndex() const {
        assert(type && "Null reference");
        return type->kind;
      }
    };
  };

  // The only boolean values are actually "true" and "false", the boolean type
  // is internal the rest of the time.
  struct Boolean : public Expression {
    bool value = false;

    Boolean(bool value) : Expression(ObjectKind::Boolean), value(value) {}

    static bool classof(const Base *B) {
      return B->getKind() == ObjectKind::Boolean;
    }

    llvm::Value* Codegen(CodegenContext& context) const;
    const Type& RuntimeType(CodegenEnv& env) const {
      return Type::Boolean;
    }
  };

  struct Number : public Expression {
    __TalesNumber value = 0.0f;

    Number(__TalesNumber value) : Expression(ObjectKind::Number), value(value) {}

    static bool classof(const Base *B) {
      return B->getKind() == ObjectKind::Number;
    }

    llvm::Value* Codegen(CodegenContext& context) const;
    const Type& RuntimeType(CodegenEnv& env) const {
      return Type::Number;
    }
  };

  struct String : public Expression {
    string value;

    String(string&& value) : Expression(ObjectKind::String), value(value) {}

    static bool classof(const Base *B) {
      return B->getKind() == ObjectKind::String;
    }

    llvm::Value* Codegen(CodegenContext& context) const;
    const Type& RuntimeType(CodegenEnv& env) const {
      return Type::String;
    }
  };

  // An identifier is basically an array of expressions
  // However string constants are a special case since the compiler
  // may be able to find a corresponding structField.
  struct Identifier : public Expression {
    typedef vector<ObjectUniquePtr<Expression>> Levels;
    Levels levels;

    void EmplaceLevel(ObjectUniquePtr<Expression>&& level) {
      levels.emplace_back(move(level));
    }

    Identifier(ObjectUniquePtr<Expression>&& firstLevel)
        : Expression(ObjectKind::Identifier) {
      EmplaceLevel(move(firstLevel));
    }
    Identifier(Identifier&& o) noexcept = default;

    static bool classof(const Base *B) {
      return B->getKind() == ObjectKind::Identifier;
    }

    llvm::Value* Codegen(CodegenContext& context) const;

    const Type& RuntimeType(CodegenEnv& env) const;

    // Emit an array alloca containing the remaining levels
    llvm::Value* EmitArray(unsigned firstLevel) const;

    inline string FullPath() const {
      string fullPath = "";
      for (Levels::const_iterator level = levels.cbegin(), lend = levels.cend();
           level != lend; ++level) {
        // TODO: pretty print of the AST
        if (String* name = dyn_cast<String>(level->get()))
          fullPath += name->value;
        else
          fullPath += "[FIXME]";

        if (level != lend-1)
          fullPath += ".";
      }
      return fullPath;
    }
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

    static bool classof(const Base *B) {
      return B->getKind() == ObjectKind::Mutable;
    }
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
    // It only looks for fields accessible at compile time.
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
    const Type type = Type(TYPEIDX_TABLE, this);

    Table() : StructLike(ObjectKind::Table) {}

    static bool classof(const Base *B) {
      return B->getKind() == ObjectKind::Table;
    }

    llvm::Value* Codegen(CodegenContext& context) const;
    llvm::Type* Typegen(Tales::CodegenContext& context) const;
    const Type& RuntimeType(CodegenEnv& env) const {
      return type;
    }
  };

  struct ClassDeclaration : public StructLike {
    const Type type = Type(TYPEIDX_CLASSINST, this);

    ClassDeclaration(FieldList&& fields)
        : StructLike(ObjectKind::ClassDeclaration, move(fields)) {}

    static bool classof(const Base *B) {
      return B->getKind() == ObjectKind::ClassDeclaration;
    }

    llvm::Value* Codegen(CodegenContext& context) const { return nullptr; }
    llvm::StructType* Typegen(CodegenContext& context) const { return nullptr; }

    const Type& RuntimeType(CodegenEnv& env) const {
      return type;
    }
  };

  enum class UnaryOperator {
    NOT, NEG, LEN
  };

  enum class BinaryOperator {
    AND, OR,
    EQ, NE, LT, LE, GT, GE,
    PLUS, MINUS, MULT, DIV, MOD
  };

  struct UnaryOperation : public Expression {
    const UnaryOperator op;
    ObjectUniquePtr<Expression> s;

    UnaryOperation(const UnaryOperator op, ObjectUniquePtrHandle&& s)
      : Expression(ObjectKind::UnaryOperation),
        op(op), s(move(s)) {}

    static bool classof(const Base *B) {
      return B->getKind() == ObjectKind::UnaryOperation;
    }

    llvm::Value* Codegen(CodegenContext& context) const;

    const Type& RuntimeType(CodegenEnv& env) const {
      switch(op) {
        case UnaryOperator::NOT:
          return Type::Boolean;
        case UnaryOperator::NEG:
        case UnaryOperator::LEN:
          return Type::Number;
      }
    }
  };

  struct BinaryOperation : public Expression {
    const BinaryOperator op;
    ObjectUniquePtr<Expression> lhs;
    ObjectUniquePtr<Expression> rhs;

    BinaryOperation(ObjectUniquePtrHandle&& lhs, const BinaryOperator op,
                    ObjectUniquePtrHandle&& rhs)
      : Expression(ObjectKind::BinaryOperation),
        lhs(move(lhs)), rhs(move(rhs)), op(op) {}

    static bool classof(const Base *B) {
      return B->getKind() == ObjectKind::BinaryOperation;
    }

    llvm::Value* Codegen(CodegenContext& context) const;

    const Type& RuntimeType(CodegenEnv& env) const {
      switch(op) {
        case BinaryOperator::EQ:
        case BinaryOperator::NE:
        case BinaryOperator::LT:
        case BinaryOperator::LE:
        case BinaryOperator::GT:
        case BinaryOperator::GE:
        case BinaryOperator::AND:
        case BinaryOperator::OR:
          return Type::Boolean;
        default:
          return Type::Number;
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

    llvm::Value* Codegen(CodegenContext& context) const;
  };

  struct Assignment : public Statement {
    Identifier lhs;
    ObjectUniquePtr<Expression> rhs;

    Assignment(Identifier&& lhs, ObjectUniquePtrHandle&& rhs)
      : Statement(ObjectKind::Assignment), lhs(move(lhs)), rhs(move(rhs)) {}

    static bool classof(const Base *B) {
      return B->getKind() == ObjectKind::Assignment;
    }

    llvm::Value* Codegen(CodegenContext& context) const;
  };

  struct Return : public Statement {
    ObjectUniquePtr<Expression> returnExpr;

    Return(ObjectUniquePtrHandle&& returnExpr) : Statement(ObjectKind::Return),
              returnExpr(move(returnExpr)) {
      assert(Return::returnExpr);
    }

    static bool classof(const Base *B) {
      return B->getKind() == ObjectKind::Return;
    }

    llvm::Value* Codegen(CodegenContext& context) const;
  };

  // NOTE: Any reason why C++ doesn't support conditional class members in templates?
  // There is an ugly way to do it using inheritance:
  // http://stackoverflow.com/questions/12117912/add-remove-data-members-with-template-parameters
  // http://stackoverflow.com/questions/15473643/c11-conditional-compilation-members
  // but I'd still need to rewrite the entire Codegen function (Clang complains that
  // the member doesn't exist)
  struct IfElse : public Statement {
    ObjectUniquePtr<Expression> cond;
    ObjectUniquePtr<Block> then;
    ObjectUniquePtr<Block> _else;

    IfElse(ObjectUniquePtr<Expression>&& cond, ObjectUniquePtr<Block>&& then)
        : Statement(ObjectKind::IfElse), cond(move(cond)),
          then(move(then)) {
      assert(IfElse::cond && IfElse::then);
    }
    IfElse(ObjectUniquePtr<Expression>&& cond, ObjectUniquePtr<Block>&& then,
                      ObjectUniquePtr<Block>&& _else)
        : Statement(ObjectKind::IfElse), cond(move(cond)),
          then(move(then)), _else(move(_else)) {
      assert(IfElse::cond && IfElse::then && IfElse::_else);
    }

    static bool classof(const Base *B) {
      return B->getKind() == ObjectKind::IfElse;
    }

    llvm::Value *Codegen(CodegenContext& context) const;
  };

  template<bool isRepeat>
  struct WhileRepeat : public Statement {
    constexpr static ObjectKind kind() {
      return isRepeat ? ObjectKind::While : ObjectKind::Repeat;
    }

    ObjectUniquePtr<Expression> cond;
    ObjectUniquePtr<Block> _do;

    WhileRepeat(ObjectUniquePtr<Expression>&& cond, ObjectUniquePtr<Block>&& _do)
        : Statement(kind()), cond(move(cond)), _do(move(_do)) {
      assert(WhileRepeat::cond && WhileRepeat::_do);
    }

    static bool classof(const Base *B) {
      return B->getKind() == kind();
    }

    llvm::Value *Codegen(CodegenContext& context) const;
  };
  typedef WhileRepeat<false> While;
  typedef WhileRepeat<true> Repeat;

  struct For : public Statement {
    Identifier var;
    ObjectUniquePtr<Expression> init, limit, step;
    ObjectUniquePtr<Block> _do;

    For(Identifier&& var, ObjectUniquePtr<Expression>&& init,
            ObjectUniquePtr<Expression>&& limit, ObjectUniquePtr<Block>&& _do,
            ObjectUniquePtr<Expression>&& step = new Number(1.0))
        : Statement(ObjectKind::For), var(move(var)), init(move(init)),
          limit(move(limit)), step(move(step)), _do(move(_do)) {
      assert(For::init && For::limit && For::step && For::_do);
    }

    static bool classof(const Base *B) {
      return B->getKind() == ObjectKind::For;
    }

    llvm::Value *Codegen(CodegenContext& context) const;
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

    const Type type = Type(TYPEIDX_FUNCTION, this);

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
    llvm::Value* Codegen(CodegenContext& context) const;

    const Type& RuntimeType(CodegenEnv& env) const {
      return type;
    }
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

    llvm::Value* Codegen(CodegenContext& context) const {
      return FunctionCall::Codegen(context);
    }

    const Type& RuntimeType(CodegenEnv& env) const {
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

    llvm::Value* Codegen(CodegenContext& context) const {
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
