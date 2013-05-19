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

#include <string>

#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "clang/Basic/SourceManager.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Tooling/Tooling.h"
#include "clang/Tooling/CommonOptionsParser.h"


// Port of a function (that doesn't even exist in boost::filesystem (but should!), taken from https://svn.boost.org/trac/boost/ticket/1976) to LLVM's exception-less equivalent implementation
namespace llvm { namespace sys { namespace path {
	error_code naive_uncomplete (StringRef path, StringRef base, SmallVectorImpl<char> &result) { // NOTE: result should be cleared before calling, it can't be cleared in the function because of the recursive calls
		if (has_root_path(path)){
			if (root_path(path) != root_path(base)) {
				append(result, path);
				return error_code::success();
			} else {
				return naive_uncomplete(relative_path(path), relative_path(base), result);
			}
		} else {
			if (has_root_path(base)) {
				errs() << "cannot uncomplete a path relative path from a rooted base\n"; // not so LLVM codelines
				return make_error_code(errc::operation_canceled); // should a new error code be created?
			} else {
				const_iterator path_it = begin(path);
				const_iterator base_it = begin(base);
				while (path_it != end(path) && base_it != end(base)) {
					if (*path_it != *base_it) break;
					++path_it; ++base_it;
				}
				for (; base_it != end(base); ++base_it) {
					append(result, "..");
				}
				for (; path_it != end(path); ++path_it) {
					append(result, *path_it);
				}
				return error_code::success();
			}
		}
	}
} } }

using std::string;

using namespace llvm;
using namespace sys::fs;
using namespace sys::path;

using namespace clang;
using namespace clang::tooling;

struct FolderParm {
	// 1st command line parameter, output folder
	SmallString<256> outputFolder;

	// 2nd command line parameter, will only process declarations that are inside this folder
	SmallString<256> inputFolder;

	FolderParm(int argc, const char **argv) : outputFolder(argv[1]), inputFolder(argv[2]) {
		make_absolute(outputFolder);
		make_absolute(inputFolder);

		bool isDirectory; is_directory(Twine(inputFolder), isDirectory);
		if (!isDirectory) {
			errs() << "Second argument(input folder) must be a folder!\n";
			exit(2); // FIXME
		}
	}

	inline bool IsInsideInputFolder(StringRef filename) {
		return inputFolder.compare(filename.substr(0, inputFolder.size())) == 0;
	}
};


class PHGVisitor;
class PHGConsumer;

class PHGVisitor : public RecursiveASTVisitor<PHGVisitor> {
	friend PHGConsumer;
public:
	explicit PHGVisitor(ASTContext *Context, PHGConsumer* PHGContext) : Context(Context), PHGContext(PHGContext) {}

	bool TraverseNamespaceDecl(NamespaceDecl* D);

private:
	ASTContext *Context;
	PHGConsumer* PHGContext;

	inline bool ShouldTraverseDecl(Decl* D);
};

class PHGConsumer : public FolderParm, public ASTConsumer {
	friend PHGVisitor;
public:
	explicit PHGConsumer(ASTContext *Context, int argc, const char **argv) : FolderParm(argc, argv), outputFile(nullptr), Visitor(Context, this) {}
	virtual ~PHGConsumer() {
		if (outputFile != nullptr)
			delete outputFile;
	}

	inline void ChangeOutputFileIfNeeded (StringRef inputFilenameNew) {
		SmallString<256> inputFilenameNewAbsolute(inputFilenameNew);
		make_absolute(inputFilenameNewAbsolute);

		if (inputFilename.compare(inputFilenameNewAbsolute) == 0)
			return;

		if (outputFile != nullptr)
			delete outputFile;

		inputFilename = inputFilenameNewAbsolute;

		SmallString<256> outputFilename(outputFolder);
		naive_uncomplete(inputFilename, inputFolder, outputFilename);

        std::string ErrorInfo;
		outputFile = new raw_fd_ostream(outputFilename.c_str(), ErrorInfo, raw_fd_ostream::F_Binary);
	}

	virtual void HandleTranslationUnit(ASTContext &Context) {
		Visitor.TraverseDecl(Context.getTranslationUnitDecl());
	}

    inline void Print(Decl* D);

    raw_fd_ostream* outputFile;  // FIXME: friend doesn't work?

private:
	PHGVisitor Visitor;

	// Input file being parsed
	SmallString<256> inputFilename;
};

#include "DeclPrinter.hpp" // Customized DeclPrinter.cpp, had to be renamed to .hpp for llvm_check_source_file_list

inline bool PHGVisitor::ShouldTraverseDecl(Decl* D) {
	PresumedLoc PLoc = Context->getSourceManager().getPresumedLoc(D->getLocation());
	if (PLoc.isInvalid() || !PHGContext->IsInsideInputFolder(PLoc.getFilename()))
		return false;

	PHGContext->ChangeOutputFileIfNeeded(PLoc.getFilename());

	return true;
}

inline void PHGConsumer::Print(Decl* D) {
	PHGDeclPrinter Printer(outputFile, Visitor.Context->getPrintingPolicy());
	Printer.Visit(const_cast<Decl*>(D));
}

bool PHGVisitor::TraverseNamespaceDecl(NamespaceDecl* D) {
	if (ShouldTraverseDecl(D))
		PHGContext->Print(D);

	return true;
}


class PHGAction : public ASTFrontendAction {
public:
	PHGAction(int argc, const char **argv) : argc(argc), argv(argv) {}

	virtual ASTConsumer *CreateASTConsumer(CompilerInstance &Compiler, StringRef InFile) {
		return new PHGConsumer(&Compiler.getASTContext(), argc, argv);
	}

private:
	int argc;
	const char** argv;
};

class PHGActionFactory : public tooling::FrontendActionFactory {
public:
	PHGActionFactory(int argc, const char **argv) : argc(argc), argv(argv) {}

	virtual FrontendAction* create() { return new PHGAction(argc, argv); }

private:
	int argc;
	const char** argv;
};


// CommonOptionsParser declares HelpMessage with a description of the common
// command-line options related to the compilation database and input files.
// It's nice to have this help message in all tools.
static cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);

static cl::extrahelp MoreHelp("\nPseudoHeaderGen creates C++-like headers (accessors may be added later, hence they won't be pure C++ anymore) that mirrors files referenced in the translation unit and which are inside the input folder 'constraint'. Those C++-like headers are to be consumed by another tool, TalesPH2LLVMBitcode to produce the symbol table for a Tales application.\n\nAll comments are removed, and the output is 'standardized' so you can quickly comment out lines (it's better to leave commented instead of removing them, so that the pseudo-headers can be automatically updated later if the headers of the library to bind are modified.");

int main(int argc, const char **argv) {
	if (argc < 4) {
		outs() << "Usage: TalesPseudoHeaderGen <output folder> <input folder constraint> <source files>... -- <clang command line>...\n";
		return 1;
	}

	// TODO: Find a proper way to add the -ccc-cxx flag automatically.;

	int _argc = argc - 2;
	CommonOptionsParser OptionsParser(_argc, &argv[2]);
	ClangTool Tool(OptionsParser.getCompilations(),
					OptionsParser.getSourcePathList());

	return Tool.run(new PHGActionFactory(argc, argv));
}