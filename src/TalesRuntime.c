/*
 * TalesRuntime.c - To save the trouble of IR creation for complex operations such as
 * 		finding a pair in a table, we use simpler C11 code compiled by Clang into LLVM IR.
 * 		The resulting IR is then parsed through llvm::ParseIR() to retrieve the struct types
 * 		e.g for the GEP instructions and malloc calls.
 * 		LLVM inlines many of these functions, so the CFG simplification ensures that the
 * 		generated IR is almost the same it would be than if we were generating it manually.
 */

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

#define _POSIX_C_SOURCE 200809L

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>

#include "TalesRuntime.h"

// The GNU C attribute 'used' tells Clang to keep unused inline functions
#define INLINE __attribute__((always_inline, used)) inline
#define INLINE_INTERNAL __attribute__((always_inline)) inline

// testing
void talesprint(const char* s) { printf("%s", s); }

struct __TalesClassDeclaration;
struct __TalesGlobalContext {
	unsigned int classCount;
	struct __TalesClassDeclaration* classes;
};
struct __TalesGlobalContext globalContext;

// Float-Pointer union
union __TalesFPU { void* p; __TalesNumber n; bool b; };

struct __TalesDynamicValue {
	enum __TalesTypeIndex typeIdx;
	union __TalesFPU value; // an union is needed to ensure the pointer size is
							// big enough to hold a float and inversely
};

struct __TalesChainPair {
	struct __TalesDynamicValue key;
	struct __TalesDynamicValue pvalue;

	struct __TalesChainPair* next;
};

// Once an identifier cannot go further in the struct tree, GEP won't help anymore
// and the struct info/fields must be available for runtime lookups.
struct __TalesStructField {
	char* name;
	enum __TalesTypeIndex typeIdx;
	ptrdiff_t offset; // from the header (NOTE: there's no way to determine the offset
			// of the spairs if you don't know the full table type, i.e not just the header,
			// since it varies with the alignment)
};

struct __TalesTableHeader { // NOTE: The real LLVM structure used to allocate Tables is
				// { { header }, { spairs } } where spairs differs from one table to another.
	// First chain pair (simply "pair" to keep Lua terms)
	struct __TalesChainPair* firstPair;

	// Ipairs, all are dynamic values
	unsigned int ipairCount;
	struct __TalesDynamicValue* ipairs;

	// Runtime info about the struct fields (spairs)
	unsigned int spairCount;
	struct __TalesStructField* spairs;
};

// NOTE: the pointer to the class declaration is fixed at compilation time, including
// for return values which are functions/closures.
struct __TalesClassDeclaration {
	unsigned int fieldCount;
	struct __TalesStructField* fields;
};

// Header of a class instance
struct __TalesClassHeader {
	struct __TalesChainPair* firstCPair;
};

struct __TalesFunctionValue { // { __TalesTypeIndex, unsigned int, void* } typearg1 typearg2 ...
			// (they follow but are outside the structure)
	enum __TalesTypeIndex retTy;

	void* function;
	void* agnosticFunc; // "glue" function that takes dynamic values as parameters and
			// returns one as well, this is necessary as there's no way to determine which
			// CheckAndConvert function to use at compilation time in a call to a non-s-reachable
			// function (LLVM is rather strict on types, and there are good reasons for that).

	unsigned int argCount;
};

INLINE void __TalesGlobalContextInit() {
	globalContext.classCount = 0;
	globalContext.classes = NULL;
}

INLINE bool __TalesEmptyS(char* s) {
	return strlen(s) == 0;
}

INLINE bool __TalesEmptyT(struct __TalesTableHeader* header) {
	return header->firstPair || header->ipairCount || header->spairCount;
}

static char sprintfBuffer[NUM2STR_DIGITS+4];

INLINE char* __TalesNtoS(__TalesNumber n) {
	snprintf(sprintfBuffer, NUM2STR_DIGITS+3, "%."STR(NUM2STR_DIGITS)"g", n);
	return strdup(sprintfBuffer);
}

INLINE __TalesNumber __TalesBtoN(bool b) {
	return b ? 1.0 : 0.0;
}

INLINE char* __TalesBtoS(bool b) {
	snprintf(sprintfBuffer, NUM2STR_DIGITS+3, b ? "true" : "false");
	return strdup(sprintfBuffer);
}

INLINE __TalesNumber __TalesDVtoN(struct __TalesDynamicValue dv) {
	if (dv.typeIdx == TYPEIDX_NUMBER)
		return dv.value.n;
	else if (dv.typeIdx == TYPEIDX_STRING)
		return atof(dv.value.p);
	else {
		fprintf(stderr, "Type mismatch, expected a number or a string\n");
		return 0.0;
	}
}

INLINE char* __TalesDVtoS(struct __TalesDynamicValue dv) {
	if (dv.typeIdx == TYPEIDX_STRING)
		return dv.value.p;
	else if (dv.typeIdx == TYPEIDX_NUMBER)
		return __TalesNtoS(dv.value.n);
	else {
		fprintf(stderr, "Type mismatch, expected a string or at least a number\n");
		return NULL;
	}
}

// Booleans are only used internally
INLINE bool __TalesDVtoB(struct __TalesDynamicValue dv) {
	if (dv.typeIdx == TYPEIDX_NUMBER)
		return dv.value.n != 0.0f;
	else if (dv.typeIdx == TYPEIDX_STRING)
		return strlen(dv.value.p) != 0;
	else if (dv.typeIdx == TYPEIDX_TABLE)
		return __TalesEmptyT(dv.value.p);
	else {
		fprintf(stderr, "FIXME, implement DVtoB for other types\n");
		return false;
	}
}

INLINE void* __TalesDVtoP(enum __TalesTypeIndex expectedTy, struct __TalesDynamicValue dv) {
	if (expectedTy == TYPEIDX_STRING) {
		return __TalesDVtoS(dv);
	} else if (expectedTy == TYPEIDX_TABLE) {
		if (dv.typeIdx == TYPEIDX_TABLE)
			return dv.value.p;
		else {
			fprintf(stderr, "Type mismatch, expected a table\n");
			return NULL;
		}
	}

	return NULL; // FIXME
}


// NOTE: Many functions used to take a __TalesFPU union value as a parameter,
// so only one function was needed. But that meant ZExt-ing 32 bits floats to 64 bits
// values (i.e casting them to zero-filled 64 bits values) which was inefficient.
// The only way out I can see is to split every function and get rid of all union parameters
// and return values

INLINE_INTERNAL void __TalesAssign(void* lhs, enum __TalesTypeIndex* lhsType,
																	 bool isLhsDyn, union __TalesFPU rhs,
																	 enum __TalesTypeIndex rhsType) {
	if (isLhsDyn) {
		// LHS is a dynamic value
		union __TalesFPU* lhsFPU = (union __TalesFPU*) lhs;

		if (rhsType == TYPEIDX_STRING)
			lhsFPU->p = strdup(rhs.p);
		else if (rhsType == TYPEIDX_BOOL)
			lhsFPU->n = __TalesBtoN(rhs.b);
		else
			*lhsFPU = rhs;

		if (rhsType == TYPEIDX_BOOL)
			rhsType = TYPEIDX_NUMBER;

		*lhsType = rhsType;
	} else {
		// LHS is a statically-typed sfield, we'll have to perform the right conversions if needed

		if (*lhsType == rhsType) {
			if (rhsType == TYPEIDX_NUMBER) *(__TalesNumber*) lhs = rhs.n;
			else if (rhsType == TYPEIDX_STRING) *(void**) lhs = strdup(rhs.p);
			else *(void**) lhs = rhs.p;
		} else if (*lhsType == TYPEIDX_NUMBER && rhsType == TYPEIDX_STRING) {
			*(__TalesNumber*) lhs = atof((const char *) rhs.p);
		} else if (*lhsType == TYPEIDX_STRING && rhsType == TYPEIDX_NUMBER) {
			*(char**) lhs = __TalesNtoS(rhs.n);
		} else if (*lhsType == TYPEIDX_NUMBER && rhsType == TYPEIDX_BOOL) {
			*(__TalesNumber*) lhs = __TalesBtoN(rhs.b);
		} else if (*lhsType == TYPEIDX_STRING && rhsType == TYPEIDX_BOOL) {
			*(char**) lhs = __TalesBtoS(rhs.b);
		} else
			fprintf(stderr, "Type mismatch, can't assign value to statically-typed LHS\n");
	}
}


INLINE_INTERNAL struct __TalesStructField*
__TalesTableFindSPairS(struct __TalesTableHeader* header, const char* wanted) {
	for (unsigned int i = 0; i < header->spairCount; ++i) {
		if (strcmp(header->spairs[i].name, wanted) == 0)
			return &header->spairs[i];
	}
	return NULL;
}

// Look for the cpair named "wanted", this is called for the first level which we couldn't
// find a spair for, hence we don't need to test the spairs again, only for the next levels.
// for LHS identifiers in assignments, create the pair if it doesn't exist (NOTE: unlike Lua,
// Tales creates as many levels as needed, each level < lastLevel being created as a table).
INLINE_INTERNAL struct __TalesDynamicValue*
__TalesTableGetCPairS(struct __TalesTableHeader* header, const char* wanted, bool createPair) {
	struct __TalesChainPair* pair = header->firstPair,* prevPair = NULL;

	while (pair != NULL) {
		if (pair->key.typeIdx == TYPEIDX_STRING)
			if (strcmp(pair->key.value.p, wanted) == 0)
				return &pair->pvalue;
		prevPair = pair; pair = pair->next;
	}

	if (!createPair)
		return NULL;

	// Pair wasn't found, create it
	pair = malloc(sizeof(struct __TalesChainPair));
	pair->key.typeIdx = TYPEIDX_STRING; pair->key.value.p = strdup(wanted);
	pair->pvalue.typeIdx = TYPEIDX_NIL;
	pair->next = NULL;

	if (prevPair != NULL)
		prevPair->next = pair;
	else // the table had no cpair previously
		header->firstPair = pair;

	return &pair->pvalue;
}

// Look for a spair, then for a cpair. And for LHS assignments, create a cpair if it doesn't exist.
INLINE_INTERNAL void* __TalesTableGetLevel(struct __TalesTableHeader* header,
																					 const char* wanted, enum __TalesTypeIndex* typeIdx,
																					 bool createCPair) {
	// First look for a struct field
	struct __TalesStructField* spairInfo = __TalesTableFindSPairS(header, wanted);

	if (spairInfo) {
		*typeIdx = spairInfo->typeIdx;
		return header + spairInfo->offset;
	}

	// Then among the chain pairs, or create it, typeIdx will always be TYPEIDX_NIL,
	// to make the caller know it's a a pointer to a dynamic value.
	*typeIdx = TYPEIDX_NIL;
	return __TalesTableGetCPairS(header, wanted, createCPair);
}


INLINE_INTERNAL struct __TalesTableHeader* __TalesNewPureTable() {
	struct __TalesTableHeader* t = malloc(sizeof(struct __TalesTableHeader));

	t->firstPair = NULL;
	t->ipairCount = 0; t->ipairs = NULL;
	t->spairCount = 0; t->spairs = NULL;

	return t;
}

INLINE_INTERNAL bool __TalesRiseLevel(enum __TalesTypeIndex** levelTy,
						union __TalesFPU** levelV, enum __TalesTypeIndex* getLevelTy, const char* expr) {
	void* getLevel;

	if (**levelTy == TYPEIDX_NIL) {
		**levelTy = TYPEIDX_TABLE;
		(*levelV)->p = __TalesNewPureTable();
	}

	switch(**levelTy) {
		case TYPEIDX_NUMBER:
		case TYPEIDX_STRING:
		case TYPEIDX_BOOL:
		case TYPEIDX_FUNCTION:
			fprintf(stderr, "Invalid identifier, must go up but one of the levels "
					"can't hold children");
			return false;

		case TYPEIDX_TABLE:
			getLevel = __TalesTableGetLevel((*levelV)->p, expr, getLevelTy, true);
			if (*getLevelTy != TYPEIDX_NIL) {
				// a sfield was found
				*levelTy = getLevelTy;
				*levelV = getLevel;
			} else {
				// no sfield, it's a cpair's dv
				struct __TalesDynamicValue* levelDV = (struct __TalesDynamicValue*) getLevel;

				*levelTy = &levelDV->typeIdx;
				*levelV = &levelDV->value;
			}
			return true;

		default:
			fprintf(stderr, "Unsupported yet."); // FIXME
			return false;
	}
}

// Once the compiler reaches a level without a sfield matching the LHS level, it cannot
// know the LHS type in advance and make a simple Store statement, we need to look
// for the LHS value location at runtime.
INLINE void __TalesAssignRuntimeT(struct __TalesTableHeader* lastSLevel, const char** remLevels,
													 unsigned int numRemLevels, union __TalesFPU rhs,
													 enum __TalesTypeIndex rhsType) {
	// Since the compiler just went through the spairs, we only look inside cpairs at this level
	struct __TalesDynamicValue* lastSLevelPairDV = __TalesTableGetCPairS(lastSLevel, remLevels[0], true);

	enum __TalesTypeIndex* levelTy = &lastSLevelPairDV->typeIdx;
	union __TalesFPU* levelV = &lastSLevelPairDV->value;

	if (numRemLevels == 1) {
		__TalesAssign(levelV, levelTy, true, rhs, rhsType);
		return;
	}

	enum __TalesTypeIndex getLevelTy;

	for (unsigned int i = 1; i < numRemLevels; ++i) {
		// If the pair was just allocated and we still need to go up, create a table
		if (!__TalesRiseLevel(&levelTy, &levelV, &getLevelTy, remLevels[i]))
			return;
	}

	__TalesAssign(levelV, levelTy, (getLevelTy == TYPEIDX_NIL), rhs, rhsType);
}

// It's more straightforward than Assign, because we're expected to return a dynamic value
INLINE struct __TalesDynamicValue __TalesGetT(struct __TalesTableHeader* lastSLevel,
																			 const char** remLevels, unsigned int numRemLevels) {
	struct __TalesDynamicValue NIL = { TYPEIDX_NIL, NULL };

	// Since the compiler just went through the spairs, we only look inside cpairs at this level
	struct __TalesDynamicValue* lastSLevelPairDV = __TalesTableGetCPairS(lastSLevel,
																																			 remLevels[0], false);

	if (!lastSLevelPairDV) {
		fprintf(stderr, "Invalid identifier\n");
		return NIL;
	}

	struct __TalesDynamicValue retVal = *lastSLevelPairDV;
	if (numRemLevels == 1) return retVal;

	for (unsigned int i = 1; i < numRemLevels; ++i) {
		if (retVal.typeIdx != TYPEIDX_TABLE && retVal.typeIdx <= TYPEIDX_FUNCTION) {
			fprintf(stderr, "Invalid identifier, level exists but isn't a class instance "
																	"nor a table\n");
			return NIL;
		}

		if (retVal.typeIdx == TYPEIDX_TABLE) {
			void* getLevel = __TalesTableGetLevel(retVal.value.p, remLevels[i], &retVal.typeIdx, false);
			if (retVal.typeIdx != TYPEIDX_NIL) {
				// a sfield was found
				retVal.value = *(union __TalesFPU*) getLevel;
			} else {
				// no sfield, it's a cpair's dv
				struct __TalesDynamicValue* levelDV = (struct __TalesDynamicValue*) getLevel;

				retVal.typeIdx = levelDV->typeIdx;
				retVal.value = levelDV->value;
			}
		} else {
			printf("FIXME!");
		}

		if (i == (numRemLevels - 1))
			return retVal;
	}

	fprintf(stderr, "Invalid identifier\n");
	return NIL;
}


INLINE bool __TalesFunctionTypecheck(union __TalesFPU v, enum __TalesTypeIndex vTy,
																		 enum __TalesTypeIndex* vArgs, unsigned int argCount,
																		 enum __TalesTypeIndex* args) {
	if (vTy != TYPEIDX_FUNCTION)
		return false;

	struct __TalesFunctionValue* func = (struct __TalesFunctionValue*) v.p;

	if (func->argCount != argCount) {
		fprintf(stderr, "Wrong number of arguments\n");
		return false;
	}

	for (unsigned int i = 0; i < argCount;++i) {
		if ((vArgs[i] == TYPEIDX_NUMBER && args[i] == TYPEIDX_STRING)
			|| (vArgs[i] == TYPEIDX_STRING && args[i] == TYPEIDX_NUMBER))
			continue;

		if (vArgs[i] != args[i]) {
			fprintf(stderr, "Incompatible argument type\n");
			return false;
		}
	}

	return true;
}


INLINE double __Tales_atof (const char *__nptr) { return strtod (__nptr, (char **) NULL); }
INLINE int __Tales_atoi (const char *__nptr) { return (int) strtol (__nptr, (char **) NULL, 10); }
INLINE long int __Tales_atol (const char *__nptr) { return strtol (__nptr, (char **) NULL, 10); }
