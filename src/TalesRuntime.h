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

#ifndef TalesRuntime_H_INCLUDED_
#define TalesRuntime_H_INCLUDED_

typedef float __TalesNumber;

#define NUM2STR_DIGITS 7
    // Lua uses 12, NOTE: look up (again) how many digits before the truncated value isn't exact anymore

// Macro arcanes beyond my mere mortal comprehension
#define STR_EXPAND(tok) #tok
#define STR(tok) STR_EXPAND(tok)
 
enum __TalesTypeIndex {
  TYPEIDX_NIL = 0,  // Undefined or dynamic value, depending on the context
// 	TYPEIDX_DVALUE,
  TYPEIDX_NUMBER,
  TYPEIDX_STRING,
  TYPEIDX_TABLE,
  TYPEIDX_CLASSINST,
  TYPEIDX_FUNCTION,
  TYPEIDX_BOOL   // Only used internally to avoid unnecessary bool->float conversions
  // Followed by the declared classes
};

#endif
