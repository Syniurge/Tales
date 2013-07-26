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
 * Tales.l - flexc++ lexer for Tales, initially based on the incomplete flex lexer for Lua 5.1 by Carlos Smith, simplified and optimized (avoiding yymore() for single characters)
 */

%namespace Tales
%filenames TalesLexer

w           [ \t\v\a]+
name        [_a-zA-Z][_a-zA-Z0-9]*
n           [0-9]+
exp         [Ee][+-]?{n}
number      -?{n}([.]{n})?{exp}?
eol         [\n\r]

%x XLONGSTRING
%x XLONGCOMMENT
%x XSTRINGQ
%x XSTRINGA

%%

and                 return Parser::AND;
break               return Parser::BREAK;
do                  return Parser::DO;
else                return Parser::ELSE;
elseif              return Parser::ELSEIF;
end                 return Parser::END;
false               return Parser::FALSE;
for                 return Parser::FOR;
function            return Parser::FUNCTION;
if                  return Parser::IF;
in                  return Parser::IN;
local               return Parser::LOCAL;
nil                 return Parser::NIL;
not                 return Parser::NOT;
or                  return Parser::OR;
repeat              return Parser::REPEAT;
return              return Parser::RETURN;
then                return Parser::THEN;
true                return Parser::TRUE;
until               return Parser::UNTIL;
while               return Parser::WHILE;

class               return Parser::CLASS;
number              return Parser::NUMBERTYPE;
string              return Parser::STRINGTYPE;
table               return Parser::TABLETYPE;

{name}              d_val__ = new ParserString(matched()); return Parser::NAME;
{number}            d_val__ = new ParserString(matched()); return Parser::NUMBERVALUE;
"--[["              begin(StartCondition__::XLONGCOMMENT);
"[["                more(); begin(StartCondition__::XLONGSTRING);
"--"[^\n\r]*        ;
\"                  d_val__ = new ParserString; begin(StartCondition__::XSTRINGQ);
'                   d_val__ = new ParserString; begin(StartCondition__::XSTRINGA);
{w}                 ;
{eol}               /*cout << "EOL\n"*/;

"..."               return Parser::DOTS;
".."                return Parser::CONCAT;
"=="                return Parser::EQ;
">="                return Parser::GE;
"<="                return Parser::LE;
"~="                return Parser::NE;

"." |
"," |
"+" |
"-" |
"*" |
"/" |
"%" |
"^" |
">" |
"<" |
"=" |
"(" |
")" |
"[" |
"]" |
"{" |
"}"                 return matched()[0];

.                   llvm::errs() << "unknown token\"" << matched() << "\" has been ignored.\n";

<XSTRINGQ>
{
    \"\"            ;
    \"              begin(StartCondition__::INITIAL); return Parser::STRINGVALUE;
    {eol}           {
                        llvm::errs() << "unterminated \"string\".\n";
                        begin(StartCondition__::INITIAL);
                        return Parser::STRINGVALUE;
                    }
    \\n             d_val__.get<Tag__::ParserString>()->append("\n");
    \\              d_val__.get<Tag__::ParserString>()->append("\\");
    [^\"\n\r\\]+    d_val__.get<Tag__::ParserString>()->append(matched());
}

<XSTRINGA>
{
    ''              ;
    '               begin(StartCondition__::INITIAL); return Parser::STRINGVALUE;
    {eol}           {
                        llvm::errs() << "unterminated 'string'.\n";
                        begin(StartCondition__::INITIAL);
                        return Parser::STRINGVALUE;
                    }
    \\n             d_val__.get<Tag__::ParserString>()->append("\n");
    \\              d_val__.get<Tag__::ParserString>()->append("\\");
    [^'\n\r\\]+     d_val__.get<Tag__::ParserString>()->append(matched());
}

<XLONGSTRING>
{
    "]]"            begin(StartCondition__::INITIAL); d_val__ = new ParserString(matched().c_str() + 1, matched().size() - 2); return Parser::STRINGVALUE;
    [^\]]+          more();
}

<XLONGCOMMENT>
{
    "]]--"          begin(StartCondition__::INITIAL);
    [^\]]+          ;
}
