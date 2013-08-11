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

/*
 * LLVM-style RTTI used by the AST and the Bisonc++ polymorphic class.
 */

#pragma once

#include <memory>
#include <type_traits>
#include <llvm/Support/Casting.h>

namespace Tales {
  enum class ObjectKind {
    // Expressions
    Expression,
    Identifier,
    Boolean,
    Number,
    String,
    Table,
    ClassDeclaration,
    FunctionDeclaration,
    FunctionCallExpr,
    UnaryOperation,
    BinaryOperation,

    // Statements
    Statement,
    Block,
    FunctionCallStmt,
    Assignment,
    Return,
    IfElse,
    If,
    Else,
    While,
    Repeat,
    For,

    // Misc
    Mutable,
    Type,
    FieldList,
    ArgumentList,
    CallArgumentList,

    // Built-in types
    ParserString,
    // NOTE: the tag system of the original bisonc++ skeletons which also checks
    // if the type is built-in boils down to the same thing, except that here we're
    // avoiding the use of the program-wide RTTI)
  };

  class Base {
    const ObjectKind Kind;
  public:
    ObjectKind getKind() const { return Kind; }

    Base(ObjectKind K) : Kind(K) {}
    Base(Base &&o) noexcept = default;
    virtual ~Base() {}

    static bool classof(const Base *B) { return true; }
  };

  // A unique pointer class that supports type casting and multiple "handles".
  // We still perform limited compile-time type checking, and full runtime
  // type checking in debug mode.

  class ObjectUniquePtrHandle {
  public:
    virtual Base* release() = 0;
  };

  template <typename T>
  class ObjectUniquePtr : public std::unique_ptr<T>, public ObjectUniquePtrHandle {
    static_assert(std::is_base_of<Base, T>::value,
            			"T in ObjectUniquePtr<T> must inherit from Base");
  public:
    ObjectUniquePtr() noexcept : std::unique_ptr<T>() {}
    ObjectUniquePtr(T* obj) noexcept : std::unique_ptr<T>(obj) {}
    ObjectUniquePtr(ObjectUniquePtr&&) noexcept = default;

    ObjectUniquePtr(ObjectUniquePtrHandle&& o) noexcept :
        std::unique_ptr<T>(llvm::cast<T>(o.release())) {}

    Base* release() { return std::unique_ptr<T>::release(); }

    ObjectUniquePtr& operator=(ObjectUniquePtr&& rhs) noexcept = default;

    ObjectUniquePtr& operator=(ObjectUniquePtrHandle&& o) {
      std::unique_ptr<T>::reset(llvm::cast<T>(o.release()));
      return *this;
    }

    template <typename D>
    ObjectUniquePtr& operator=(D* p) {
      static_assert(std::is_base_of<T, D>::value,
            				"Attempt to assign an object with an incompatible type");
      std::unique_ptr<T>::reset(llvm::cast<T>(p));
      return *this;
    }
  };
}
