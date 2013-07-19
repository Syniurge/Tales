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
 * Simple evaluator for Tales, for testing
 */

#include <iostream>
#include <fstream>

#include <bobcat/readlinestream>

#include "TalesParser.h"
#include "TalesAST.hpp"
#include "TalesCodegen.hpp"

using namespace std;
using namespace FBB;
using namespace llvm;
using namespace Tales;

int main(int argc, const char **argv)
{
	istream* in;

	if (argc > 1)
		in = new ifstream(argv[1]);
	else
		in = new ReadLineStream("> ");

	ASTContext astContext;
	Parser parser(astContext, *in);

	parser.parse();

	InitializeNativeTarget();

	RuntimeModule* runtimeModule = RuntimeModule::Load();
	if (!runtimeModule) {
		cerr << "Module creation and Tales Runtime IR loading failed" << endl;
		return 1;
	}

	std::string errorStr;
	ExecutionEngine* execEngine = EngineBuilder(runtimeModule->module).setErrorStr(&errorStr).create();

	if (!execEngine) {
		cerr << "JIT engine creation failed: " << errorStr << endl;
		return 1;
	}

	CodegenContext codegenContext(*runtimeModule, execEngine);

	codegenContext.env.root = astContext.root.get();

	Function* ChunkF = cast<Function>(astContext.parsedChunk->CodegenFunc(codegenContext));
	if (!ChunkF) {
		cerr << "Codegen failed" << endl;
		return 1;
	}

//  runtimeModule->module->dump();

	runtimeModule->pm.run(*runtimeModule->module);

	void *FPtr = execEngine->getPointerToFunction(ChunkF);
	void (*FP)() = (void (*)())(intptr_t)FPtr;

	FP();

	return 0;
}
