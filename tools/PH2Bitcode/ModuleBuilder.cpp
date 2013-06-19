//===--- ModuleBuilder.cpp - Emit LLVM Code from ASTs ---------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This builds an AST and converts it to LLVM Code.
//
//===----------------------------------------------------------------------===//

#include "clang/CodeGen/ModuleBuilder.h"
#include "CodeGenModule.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclObjC.h"
#include "clang/AST/Expr.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Frontend/CodeGenOptions.h"
#include "llvm/ADT/OwningPtr.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include <vector>
using namespace clang;

namespace {
  class CodeGeneratorImpl : public CodeGenerator {
    DiagnosticsEngine &Diags;
    OwningPtr<const llvm::DataLayout> TD;
    ASTContext *Ctx;
    const CodeGenOptions CodeGenOpts;  // Intentionally copied in.

    // === Tales === //

    // Vanilla Clang doesn't wait for the entire AST to be parsed to consume it.
    // We have to defer the AST consumption so that we can whitelist the declarations
    // to be exposed before the Codegen.

    // Maybe a proper way to do this would be traversing the TranslationUnitDecl in
    // HandleTranslationUnit, but the order wouldn't be preserved (do we need to handle the
    // tag decls first?), so to minimize headaches and keep the differences between Clang
    // and TalesClang low I'll stick to deferring the Codegen calls for now.

    enum AKind {
      ATopLevelDecl,
      ATagDecl,
      ACXXStaticMemberVarInstantiation,
      ACompleteTentativeDefinition,
      AVTable
    };

    struct DeferredAction : public std::pair<AKind, Decl*> {
      bool VTableDefinitionRequired;

      DeferredAction() : VTableDefinitionRequired(false) { }
      DeferredAction(AKind first, Decl* second,
                     bool VTableDefinitionRequired = false) :
               std::pair<AKind, Decl*>(first, second),
               VTableDefinitionRequired(VTableDefinitionRequired) { }
    };

    struct DeferredActions {
      unsigned Pow2;
      std::vector<DeferredAction> Decls;
      typedef std::vector<DeferredAction>::iterator iterator;

      DeferredActions() : Pow2(1) {}

      inline void Push(AKind K, Decl* D, bool VTableDefinitionRequired) {
        if (Decls.size() == Decls.capacity()) {
          Pow2 *= 2;
          Decls.reserve(Pow2);
        }

        Decls.push_back(DeferredAction(K, D, VTableDefinitionRequired));
      }
      inline void Push(AKind K, Decl* D) { Push(K, D, false); }

      void Reset() {
        Pow2 = 1;
        Decls.resize(0);
      }
    };

    DeferredActions TheDeferredActions;

    // === Tales === //

  protected:
    OwningPtr<llvm::Module> M;
    OwningPtr<CodeGen::CodeGenModule> Builder;
  public:
    CodeGeneratorImpl(DiagnosticsEngine &diags, const std::string& ModuleName,
                      const CodeGenOptions &CGO, llvm::LLVMContext& C)
      : Diags(diags), CodeGenOpts(CGO),
        M(new llvm::Module(ModuleName, C)) {}

    virtual ~CodeGeneratorImpl() {}

    virtual llvm::Module* GetModule() {
      return M.get();
    }

    virtual llvm::Module* ReleaseModule() {
      return M.take();
    }

    virtual void Initialize(ASTContext &Context) {
      Ctx = &Context;

      M->setTargetTriple(Ctx->getTargetInfo().getTriple().getTriple());
      M->setDataLayout(Ctx->getTargetInfo().getTargetDescription());
      TD.reset(new llvm::DataLayout(Ctx->getTargetInfo().getTargetDescription()));
      Builder.reset(new CodeGen::CodeGenModule(Context, CodeGenOpts, *M, *TD,
                                               Diags, WhitelistedOnly));

      TheDeferredActions.Reset();
    }

    virtual void HandleCXXStaticMemberVarInstantiation(VarDecl *VD) {
      TheDeferredActions.Push(ACXXStaticMemberVarInstantiation, VD);
    }

    virtual bool HandleTopLevelDecl(DeclGroupRef DG) {
      // Make sure to emit all elements of a Decl.
      for (DeclGroupRef::iterator I = DG.begin(), E = DG.end(); I != E; ++I)
        TheDeferredActions.Push(ATopLevelDecl, *I);
      return true;
    }

    /// HandleTagDeclDefinition - This callback is invoked each time a TagDecl
    /// to (e.g. struct, union, enum, class) is completed. This allows the
    /// client hack on the type, which can occur at any point in the file
    /// (because these can be defined in declspecs).
    virtual void HandleTagDeclDefinition(TagDecl *D) {
      TheDeferredActions.Push(ATagDecl, D);
    }

    virtual void HandleTranslationUnit(ASTContext &Ctx) {
      // Passing deferred declarations to Codegen
      for (DeferredActions::iterator I = TheDeferredActions.Decls.begin(), E = TheDeferredActions.Decls.end(); I != E; ++I) {
        switch (I->first) {
          case ATopLevelDecl:
            Builder->EmitTopLevelDecl(I->second); break;
          case ATagDecl:
          {
            TagDecl *D = cast<TagDecl>(I->second);

            Builder->UpdateCompletedType(D);

            // In C++, we may have member functions that need to be emitted at this
            // point.
            if (Ctx.getLangOpts().CPlusPlus && !D->isDependentContext()) {
              for (DeclContext::decl_iterator M = D->decls_begin(),
                                          MEnd = D->decls_end();
                  M != MEnd; ++M)
                if (CXXMethodDecl *Method = dyn_cast<CXXMethodDecl>(*M))
                  if (Method->doesThisDeclarationHaveABody() &&
                      (Method->hasAttr<UsedAttr>() ||
                      Method->hasAttr<ConstructorAttr>()))
                    Builder->EmitTopLevelDecl(Method);
            }
          }
          break;
          case ACXXStaticMemberVarInstantiation:
            Builder->HandleCXXStaticMemberVarInstantiation(cast<VarDecl>(I->second)); break;
          case ACompleteTentativeDefinition:
            Builder->EmitTentativeDefinition(cast<VarDecl>(I->second)); break;
          case AVTable:
            Builder->EmitVTable(cast<CXXRecordDecl>(I->second), I->VTableDefinitionRequired); break;
        }
      }

      if (Diags.hasErrorOccurred()) {
        M.reset();
        return;
      }

      if (Builder)
        Builder->Release();
    }

    virtual void CompleteTentativeDefinition(VarDecl *D) {
      if (Diags.hasErrorOccurred())
        return;

      TheDeferredActions.Push(ACompleteTentativeDefinition, D);
    }

    virtual void HandleVTable(CXXRecordDecl *RD, bool DefinitionRequired) {
      if (Diags.hasErrorOccurred())
        return;

      TheDeferredActions.Push(AVTable, RD, DefinitionRequired);
    }
  };
}

void CodeGenerator::anchor() { }

CodeGenerator *clang::CreateLLVMCodeGen(DiagnosticsEngine &Diags,
                                        const std::string& ModuleName,
                                        const CodeGenOptions &CGO,
                                        const TargetOptions &/*TO*/,
                                        llvm::LLVMContext& C) {
  return new CodeGeneratorImpl(Diags, ModuleName, CGO, C);
}
