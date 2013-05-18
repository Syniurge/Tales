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
 * and thanks to Frank B. Brokken for making bisonc++, allowing some cool C++ 11 wizardry (well it turned the code into an horrendous mess.. but at least it's safe and fast?)
 */

%baseclass-preinclude "TalesAST.hpp"

%namespace Tales
%filenames TalesParser

%scanner TalesLexer.h


// NOTE: bisonc++ takes care of choosing enum values that do not conflict with chars
%token NAME NUMBERVALUE STRINGVALUE

%token AND BREAK DO ELSEIF ELSE END FALSE FOR FUNCTION IF IN LOCAL NIL NOT OR REPEAT RETURN THEN TRUE UNTIL WHILE
%token CLASS NUMBERTYPE STRINGTYPE TABLETYPE

%token DOTS CONCAT EQ GE LE NE


%polymorphic
		STRING: string;
		TYPE: NType;
		MUTABLE: NMutable;
		EXPRESSION: unique_ptr<NExpression>;
		STATEMENT: unique_ptr<NStatement>;
		IDENTIFIER: unique_ptr<NIdentifier>;
		BLOCK: unique_ptr<NBlock>;
		TABLE_PAIRS: unique_ptr<NTable>;
		CLASS_DECL_FIELDS: unique_ptr<FieldList>;
		FUNC_DECL_ARGS: unique_ptr<NFunctionType::ArgumentList>;
		FUNC_CALL_ARGS: unique_ptr<NFunctionCall::CallArgumentList>;
		BINARY_OP: NBinaryOperation::Operator

%type <STRING> NAME NUMBERVALUE STRINGVALUE

%type <TYPE> type
%type <EXPRESSION> expr class_expr table_expr func_expr
%type <STATEMENT> stmt class_stmt func_stmt

%type <IDENTIFIER> ident
%type <MUTABLE> local class_decl_field table_spair func_decl_arg
%type <BLOCK> block stmts
%type <TABLE_PAIRS> table_pairs
%type <CLASS_DECL_FIELDS> class_decl_fields
%type <FUNC_DECL_ARGS> func_decl_args
%type <FUNC_CALL_ARGS> func_call_args
%type <BINARY_OP> binary_op


%left '+' '-'
%left '*' '%' '/'


%start program

%%

program : stmts
			{
				if (!context.root)
					context.root.reset(new NTable);

				NFunctionDeclaration* mainFuncDecl = new NFunctionDeclaration(move(*$1)); mainFuncDecl->debugName = "main";
				context.parsedChunk.reset(mainFuncDecl);
			}
		;

block : stmts END { $$ = move($1); }
		;

stmts : local { ($$).reset(new NBlock); $$->locals.emplace_back(move($1)); }
		| stmt { ($$).reset(new NBlock); $$->statements.push_back(move($1)); }
		| stmts local { $1->locals.emplace_back(move($2)); }
		| stmts stmt { $1->statements.push_back(move($2)); }
		;

local : LOCAL NAME { ($$).name = move($2); }
		| LOCAL NAME '=' expr {($$).name = move($2); ($$).initialAssignment = move($4); }
		| LOCAL type NAME { ($$).type = move($2); ($$).name = move($3); }
		| LOCAL type NAME '=' expr { ($$).type = move($2); ($$).name = move($3); ($$).initialAssignment = move($5); ($$).type.RefineFromExpr(($$).initialAssignment.get()); }
		;

stmt : class_stmt | func_stmt { $$ = move($1); }
		| RETURN expr { ($$).reset(new NReturn(move($2))); }
		| ident '=' expr { ($$).reset(new NAssignment(move(*$1), move($3))); }
		| ident '(' func_call_args ')' { ($$).reset(new NFunctionCall(move(*$1), move(*$3))); }
		;

expr : NUMBERVALUE { ($$).reset(new NNumber(atof(($1).c_str()))); }
		| STRINGVALUE { ($$).reset(new NString(move($1))); }
		| class_expr | table_expr | func_expr { $$ = move($1); }
		| ident '(' func_call_args ')' { ($$).reset(new NFunctionCall(move(*$1), move(*$3))); }
		| ident { $$ = move($1); }
		| expr binary_op expr { ($$).reset(new NBinaryOperation(move($1), $2, move($3))); }
		| '(' expr ')' { $$ = move($2); }
		;

type : NUMBERTYPE { ($$).kind = TYPEIDX_NUMBER; }
		| STRINGTYPE { ($$).kind = TYPEIDX_STRING; }
		| TABLETYPE { ($$).kind = TYPEIDX_TABLE; }
		| ident { ($$).SetClass(context, $1->FullPath()); }
		;

ident : NAME { ($$).reset(new NIdentifier(move($1))); }
		| ident '.' NAME { $1->levels.emplace_back($3); }
		;

class_expr : CLASS '{' class_decl_fields '}' { ($$).reset(new NClassDeclaration(move(*$3))); }
		;

class_stmt : CLASS ident '{' class_decl_fields '}' { ($$).reset(new NAssignment(move(*$2), unique_ptr<NExpression>(new NClassDeclaration(move(*$4))))); }
		;

class_decl_fields : { ($$).reset(new FieldList); }
		| class_decl_field { ($$).reset(new FieldList); $$->emplace_back(move($1)); }
		| class_decl_fields ',' class_decl_field { $1->emplace_back(move($3)); }
		;

class_decl_field : /*NAME { ($$).name = move($1); }
		| NAME '=' expr {($$).name = move($1); ($$).initialAssignment = move($3); }
		|*/ type NAME { ($$).type = move($1); ($$).name = move($2); }
		| type NAME '=' expr { ($$).type = move($1); ($$).name = move($2); ($$).initialAssignment = move($4); ($$).type.RefineFromExpr(($$).initialAssignment.get()); }
		;

table_expr : '{' table_pairs '}' { $$ = move($2); }
		;

table_pairs : { ($$).reset(new NTable); }
		| table_spair { ($$).reset(new NTable); $$->fields.emplace_back(move($1)); }
		| expr { ($$).reset(new NTable()); $$->ipairs.emplace_back(move($1)); }
		| table_pairs ',' table_spair { $1->fields.emplace_back(move($3)); }
		| table_pairs ',' expr { $1->ipairs.emplace_back(move($3)); }
		;

table_spair : NAME { ($$).name = move($1); }
		| NAME '=' expr {($$).name = move($1); ($$).initialAssignment = move($3); }
		| type NAME { ($$).type = move($1); ($$).name = move($2); }
		| type NAME '=' expr { ($$).type = move($1); ($$).name = move($2); ($$).initialAssignment = move($4); ($$).type.RefineFromExpr(($$).initialAssignment.get()); }
		;

func_expr : FUNCTION '(' func_decl_args ')' block { ($$).reset(new NFunctionDeclaration(move(*$3), move(*$5))); }
		| type FUNCTION '(' func_decl_args ')' block { ($$).reset(new NFunctionDeclaration(move($1), move(*$4), move(*$6))); }
		;

func_stmt : FUNCTION ident '(' func_decl_args ')' block
			{
				NFunctionDeclaration* funcDecl = new NFunctionDeclaration(move(*$4), move(*$6));
				funcDecl->debugName = $2->FullPath();
				($$).reset(new NAssignment(move(*$2), unique_ptr<NExpression>(funcDecl)));
			}
		| type FUNCTION ident '(' func_decl_args ')' block
			{
				NFunctionDeclaration* funcDecl = new NFunctionDeclaration(move($1), move(*$5), move(*$7));
				funcDecl->debugName = $3->FullPath();
				($$).reset(new NAssignment(move(*$3), unique_ptr<NExpression>(funcDecl)));
			}
		;

func_decl_args : { ($$).reset(new NFunctionType::ArgumentList); }
		| func_decl_arg { ($$).reset(new NFunctionType::ArgumentList); $$->emplace_back(move($1)); }
		| func_decl_args ',' func_decl_arg { $1->emplace_back(move($3)); }
		;

func_decl_arg : NAME { ($$).name = move($1); }
		| type NAME { ($$).type = move($1); ($$).name = move($2); }
		;
    
func_call_args : { ($$).reset(new NFunctionCall::CallArgumentList); }
		| expr { ($$).reset(new NFunctionCall::CallArgumentList); $$->push_back(move($1)); }
		| func_call_args ',' expr  { $1->push_back(move($3)); }
		;

binary_op : EQ | NE | GE | '>' | LE | '<' | AND | OR | NOT { $$ = NBinaryOperation::NOT; }
		| '+' { $$ = NBinaryOperation::PLUS; }
		| '-' { $$ = NBinaryOperation::MINUS; }
		| '*' { $$ = NBinaryOperation::MULT; }
		| '/' | '%' | CONCAT { $$ = NBinaryOperation::DIV; }
		;
