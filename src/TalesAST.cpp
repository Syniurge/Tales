#include "TalesAST.hpp"

namespace Tales {
	const Type Type::DynamicValue = Type();
	const Type Type::Boolean = Type(TYPEIDX_BOOL);
	const Type Type::Number = Type(TYPEIDX_NUMBER);
	const Type Type::String = Type(TYPEIDX_STRING);

	Type::Type(Kind kind, const Expression* expr) :
			Base(ObjectKind::Type),
			kind(kind),
			structDecl(dyn_cast<const StructLike>(expr)),
			funcType(dyn_cast<const FunctionDeclaration>(expr) ? &dyn_cast<const FunctionDeclaration>(expr)->ftype : nullptr) {}
}
