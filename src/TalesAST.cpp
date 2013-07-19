#include "TalesAST.hpp"

namespace Tales {
		Type::Type(Kind kind, const Expression* expr) :
			Base(ObjectKind::Type),
			kind(kind),
			structDecl(dyn_cast<const StructLike>(expr)),
			funcType(dyn_cast<const FunctionDeclaration>(expr) ? &dyn_cast<const FunctionDeclaration>(expr)->ftype : nullptr) {}
}
