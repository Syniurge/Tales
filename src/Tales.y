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
 * Tales.y - Bisonc++ parser made with the help of the awesome Flex-Bison-LLVM toy compiler tutorial by Loren Segal: http://gnuu.org/2009/09/18/writing-your-own-toy-compiler/
 * "flex & bison" by John R. Levine was also of great help to understand Bison 
 * and thanks to Frank B. Brokken for making bisonc++, allowing some fine C++ 11 wizardry.
 */

 /*
 * Remarks: the issues that come with an union (only accepts Plain Old Data types as members) are resolved by the polymorphic class approach of bisonc++
 * However it comes at the cost of making many heaps allocations.. it's not the perfect solution yet but it's confusing enough to make me accept heap allocs for now.
 * The optimal way as far as I can see is to have a class with a storage similar to llvm::SmallVector and big enough to hold all the types specified by
 * %polymorphic AND other types that may derive from Expression, Statement, ... (so needs a new directive), and the tag. Then the get() function takes care
 * of initializing that storage correctly. But I have no idea if C++ provide the tools for this, I haven't found anything yet.
 */

%baseclass-preinclude "TalesAST.hpp"

%namespace Tales
%filenames TalesParser

%scanner TalesLexer.h


// bisonc++ takes care of choosing enum values that do not conflict with chars
%token NAME NUMBERVALUE STRINGVALUE

%token AND BREAK DO ELSEIF ELSE END FALSE FOR FUNCTION IF IN LOCAL NIL NOT OR REPEAT RETURN THEN TRUE UNTIL WHILE
%token CLASS NUMBERTYPE STRINGTYPE TABLETYPE

%token DOTS CONCAT EQ GE LE NE

// NOTE: Bisonc++'s skeletons were modified so that instead of generating its own tags it reuses the LLVM-style RTTI
%polymorphic
		ParserString: ParserString;
		Type: Type;
		Mutable: Mutable;
		Expression: Expression;
		Statement: Statement;
		Identifier: Identifier;
		Block: Block;
		Table: Table;
		FieldList: FieldList;
		ArgumentList: ArgumentList;
		CallArgumentList: CallArgumentList;
		ParserOperator: ParserOperator

%type <ParserString> NAME NUMBERVALUE STRINGVALUE

%type <Type> type
%type <Expression> expr class_expr table_expr func_expr
%type <Statement> stmt class_stmt func_stmt

%type <Identifier> ident
%type <Mutable> local class_decl_field table_spair func_decl_arg
%type <Block> block stmts
%type <Table> table_pairs
%type <FieldList> class_decl_fields
%type <ArgumentList> func_decl_args
%type <CallArgumentList> func_call_args
%type <ParserOperator> binary_op


%left '+' '-'
%left '*' '%' '/'


%start program

%%

program : stmts {
			if (!context.root)
				context.root = new Table;

			context.parsedChunk = new FunctionDeclaration(move(*$1), "main");
		}
		;

block : stmts END { $$ = move($1); }
		;

stmts : local {
				$$ = new Block;
				$$->locals.emplace_back(move(*$1));
			}
		| stmt {
				$$ = new Block;
				$$->statements.emplace_back(move($1));
			}
		| stmts local { $1->locals.emplace_back(move(*$2)); }
		| stmts stmt { $1->statements.emplace_back(move($2)); }
		;

local : LOCAL NAME { $$ = new Mutable(move(*$2)); }
		| LOCAL NAME '=' expr { $$ = new Mutable(move(*$2), move($4)); }
		| LOCAL type NAME { $$ = new Mutable(move(*$2), move(*$3)); }
		| LOCAL type NAME '=' expr {
				// TODO: Check if the initialAssignment type matches the specified type?
				/*Type::Kind exprTypeKind = $5->RuntimeType();

				// Check if the type of expr matches the specified type
				if (exprTypeKind != move(*$2).kind)
					llvm::errs() << "The specified type doesn't match the type of the expression\n";
				else*/
					$$ = new Mutable(move(*$2), move(*$3), move($5));
			}
		;

stmt : class_stmt | func_stmt { $$ = move($1); }
		| RETURN expr { $$ = new Return(move($2)); }
		| ident '=' expr { $$ = new Assignment(move(*$1), move($3)); }
		| ident '(' func_call_args ')' { $$ = new FunctionCallStmt(move(*$1), move(*$3)); }
		;

expr : NUMBERVALUE { $$ = new Number( atof( $1->c_str() ) ); }
		| STRINGVALUE { $$ = new String(move(*$1)); }
		| class_expr | table_expr | func_expr { $$ = move($1); }
		| ident '(' func_call_args ')' { $$ = new FunctionCallExpr(move(*$1), move(*$3)); }
		| ident { $$ = move($1); }
		| expr binary_op expr { $$ = new BinaryOperation(move($1), move(*$2), move($3)); }
		| '(' expr ')' { $$ = move($2); }
		;

type : NUMBERTYPE { $$ = new Type(TYPEIDX_NUMBER); }
		| STRINGTYPE { $$ = new Type(TYPEIDX_STRING); }
		| TABLETYPE { $$ = new Type(TYPEIDX_TABLE); }
/* 		| ident { (&$$).SetClass(context, $1->FullPath()); } */
		;

ident : NAME {
				$$ = new Identifier;
				$$->levels.emplace_back(move(*$1));
			}
		| NAME '[' expr ']' {
				$$ = new Identifier;
				$$->levels.emplace_back(move(*$1), move($3));
			}
		| ident '.' NAME { $1->levels.emplace_back(move(*$3)); }
		| ident '.' NAME '[' expr ']' { $1->levels.emplace_back(move(*$3), move($5)); }
		;

class_expr : CLASS '{' class_decl_fields '}' { $$ = new ClassDeclaration(move(*$3)); }
		;

class_stmt : CLASS ident '{' class_decl_fields '}' {
				ObjectUniquePtr<ClassDeclaration> rhs(new ClassDeclaration(move(*$4)));
				$$ = new Assignment(move(*$2), move(rhs));
			}
		;

class_decl_fields : { }
		| class_decl_field { $$->emplace_back(move(*$1)); }
		| class_decl_fields ',' class_decl_field { $1->emplace_back(move(*$3)); }
		;

class_decl_field : /*NAME { (&$$).name = move(*$1); }
		| NAME '=' expr {(&$$).name = move(*$1); (&$$).initialAssignment = move(*$3); }
		|*/ type NAME { $$ = new Mutable(move(*$1), move(*$2)); }
		| type NAME '=' expr { $$ = new Mutable(move(*$1), move(*$2), move($4)); }
		;

table_expr : '{' table_pairs '}' { $$ = move($2); }
		;

table_pairs : { $$ = new Table; }
		| table_spair {
				$$ = new Table;
				$$->fields.emplace_back(move(*$1));
			}
		| expr {
				$$ = new Table;
				$$->ipairs.emplace_back(move($1));
			}
		| table_pairs ',' table_spair { $1->fields.emplace_back(move(*$3)); }
		| table_pairs ',' expr { $1->ipairs.emplace_back(move($3)); }
		;

table_spair : NAME { $$ = new Mutable(move(*$1)); }
		| NAME '=' expr { $$ = new Mutable(move(*$1), move($3)); }
		| type NAME { $$ = new Mutable(move(*$1), move(*$2)); }
		| type NAME '=' expr { $$ = new Mutable(move(*$1), move(*$2), move($4)); }
		;

func_expr : FUNCTION '(' func_decl_args ')' block {
				$$ = new FunctionDeclaration(move(*$3), move(*$5));
			}
		| type FUNCTION '(' func_decl_args ')' block {
				$$ = new FunctionDeclaration(move(*$1), move(*$4), move(*$6));
			}
		;

func_stmt : FUNCTION ident '(' func_decl_args ')' block {
				ObjectUniquePtr<FunctionDeclaration> funcDecl (new FunctionDeclaration(move(*$4), move(*$6), $2->FullPath()));
				$$ = new Assignment(move(*$2), move(funcDecl));
			}
		| type FUNCTION ident '(' func_decl_args ')' block {
				ObjectUniquePtr<FunctionDeclaration> funcDecl (new FunctionDeclaration(move(*$1), move(*$5), move(*$7), $3->FullPath()));
				$$ = new Assignment(move(*$3), move(funcDecl));
			}
		;

func_decl_args : { $$ = new ArgumentList; }
		| func_decl_arg {
				$$ = new ArgumentList;
				$$->emplace_back(move(*$1));
			}
		| func_decl_args ',' func_decl_arg { $1->emplace_back(move(*$3)); }
		;

func_decl_arg : NAME { $$ = new Mutable(move(*$1)); }
		| type NAME { $$ = new Mutable(move(*$1), move(*$2)); }
		;
    
func_call_args : { $$ = new CallArgumentList; }
		| expr {
				$$ = new CallArgumentList;
				$$->emplace_back(move($1));
			}
		| func_call_args ',' expr  { $1->emplace_back(move($3)); }
		;

binary_op : EQ | NE | GE | '>' | LE | '<' | AND | OR | NOT { $$ = new ParserOperator(Operator::NOT); }
		| '+' { $$ = new ParserOperator(Operator::PLUS); }
		| '-' { $$ = new ParserOperator(Operator::MINUS); }
		| '*' { $$ = new ParserOperator(Operator::MULT); }
		| '/' | '%' | CONCAT { $$ = new ParserOperator(Operator::DIV); }
		;
