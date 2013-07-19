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

// Generated by Flexc++ V1.04.00 on Sun, 21 Apr 2013 19:32:12 +0200

#ifndef Scanner_H_INCLUDED_
#define Scanner_H_INCLUDED_

#include "llvm/Support/raw_ostream.h"

// $insert baseclass_h
#include "TalesLexerbase.h"

#include "TalesParserbase.h"

// $insert namespace-open
namespace Tales
{

// $insert classHead
class Scanner: public ScannerBase
{
    public:
        explicit Scanner(ParserBase::STYPE__ &d_val,
																std::istream &in = std::cin,
                                std::ostream &out = std::cout);

        Scanner(ParserBase::STYPE__ &d_val,
									std::string const &infile,
									std::string const &outfile);
        
        // $insert lexFunctionDecl
        int lex();

    private:
        ParserBase::STYPE__ &d_val__;

        int lex__();
        int executeAction__(size_t ruleNr);

        void print();
        void preCode();     // re-implement this function for code that must 
                            // be exec'ed before the patternmatching starts
};

// $insert scannerConstructors
inline Scanner::Scanner(ParserBase::STYPE__ &d_val, std::istream &in, std::ostream &out)
:
    ScannerBase(in, out),
    d_val__(d_val)
{}

inline Scanner::Scanner(ParserBase::STYPE__ &d_val, std::string const &infile, std::string const &outfile)
:
    ScannerBase(infile, outfile),
    d_val__(d_val)
{}

// $insert inlineLexFunction
inline int Scanner::lex()
{
    return lex__();
}

inline void Scanner::preCode() 
{
    // optionally replace by your own code
}

inline void Scanner::print() 
{
    print__();
}

// $insert namespace-close
}

#endif // Scanner_H_INCLUDED_

