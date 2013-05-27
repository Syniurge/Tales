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
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Tooling/Tooling.h"
#include "clang/Tooling/CommonOptionsParser.h"


// Port of a function (that doesn't even exist in boost::filesystem (but should!), taken from https://svn.boost.org/trac/boost/ticket/1976) to LLVM's exception-less equivalent implementation
namespace llvm { namespace sys { namespace path {
	error_code naive_uncomplete (StringRef path, StringRef base, SmallVectorImpl<char> &result) { // NOTE: result should be cleared before calling, the function doesn't clear it because of the recursive calls
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
	SmallString<128> outputFolder;

	// 2nd command line parameter, will only process declarations that are inside that file or folder
	SmallString<128> inputConstraint;

	StringRef inputFolder;

	FolderParm(int argc, const char **argv) : outputFolder(argv[1]), inputConstraint(argv[2]) {
		make_absolute(outputFolder);
		make_absolute(inputConstraint);

		bool isDirectory; is_directory(Twine(outputFolder), isDirectory);
		if (exists(Twine(outputFolder)) && !isDirectory) {
			errs() << "First argument(output folder) exists but isn't a folder!\n";
			exit(2); // FIXME
		}

		if (!exists(Twine(inputConstraint))) {
			errs() << "Second argument(input constraint) must be an existing file or folder!\n";
			exit(2);
		}

		is_directory(Twine(inputConstraint), isDirectory);
		inputFolder = isDirectory ? inputConstraint.str() : parent_path(inputConstraint);
	}

	inline bool IsInsideInputConstraint(StringRef filename) const {
		return inputConstraint.compare(filename.substr(0, inputConstraint.size())) == 0;
	}
};


#include "DeclPrinter.hpp" // Customized DeclPrinter.cpp, had to be renamed to .hpp for llvm_check_source_file_list

class PHGConsumer : public FolderParm, public ASTConsumer {
public:
	explicit PHGConsumer(ASTContext *Context, int argc, const char **argv) : FolderParm(argc, argv), Visitor(Context, *this) {}

	virtual void HandleTranslationUnit(ASTContext &Context) {
		Visitor.Visit(const_cast<TranslationUnitDecl*>(Context.getTranslationUnitDecl()));
	}

private:
	PHGDeclPrinter Visitor;
};

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


// CommonOptionsParser declares HelpMessage with a description of the common command-line options related to the compilation database and input files.
// It's nice to have this help message in all tools.
static cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);

static cl::extrahelp MoreHelp("PseudoHeaderGen creates C++-like headers (accessors may be added later, hence they won't be pure C++ anymore) that mirrors files that are \nreferenced in the translation unit and are inside the input 'constraint' (which can be a folder). Those C++-like headers are to be consumed \nby another tool, TalesPH2LLVMBitcode to produce the symbol table for a Tales application.\n\nAll comments are removed, and the output is 'standardized' so you can quickly comment out lines. It's better to leave commented instead of \nremoving them, so that the pseudo-headers can be automatically updated later if the headers of the library to bind are modified.\n");

int main(int argc, const char **argv) {
	if (argc < 4) {
// 		cl::PrintHelpMessage(true);
		outs() << MoreHelp.morehelp << "\n";
		outs() << "Usage: TalesPseudoHeaderGen <output folder> <input constraint> <source files>... -- <Clang command line options>...\n";
		return 1;
	}

	// TODO: Find a proper way to add the -ccc-cxx flag automatically.;

	int _argc = argc - 2;
	CommonOptionsParser OptionsParser(_argc, &argv[2]);
	ClangTool Tool(OptionsParser.getCompilations(),
					OptionsParser.getSourcePathList());

	return Tool.run(new PHGActionFactory(argc, argv));
}