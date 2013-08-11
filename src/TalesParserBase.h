/*
 * Modified bisonc++base.h skeleton to reuse the LLVM-style RTTI instead of generating tags
 */

#ifndef \@$Base_h_included
#define \@$Base_h_included

#include <exception>
#include <vector>
#include <iostream>

$insert preincludes
$insert debugincludes

namespace // anonymous
{
    struct PI__;
}

$insert namespace-open

typedef ObjectKind Tag__;

    // NOTE: insert polymorphic defines Tag__ and no way to tell bisonc++
    // not to do that without altering its code (maybe later).
    // So for now we manually copy paste the code through CMakeLists.txt
///@includepolymorphic

class \@Base
{
    public:
$insert tokens
$insert LTYPE
$insert STYPE

    private:
        int d_stackIdx__;
        std::vector<size_t>   d_stateStack__;
        std::vector<STYPE__>  d_valueStack__;
$insert LTYPEstack

    protected:
        enum Return__
        {
            PARSE_ACCEPT__ = 0,   // values used as parse()'s return values
            PARSE_ABORT__  = 1
        };
        enum ErrorRecovery__
        {
            DEFAULT_RECOVERY_MODE__,
            UNEXPECTED_TOKEN__,
        };
        bool        d_debug__;
        size_t      d_nErrors__;
        size_t      d_requiredTokens__;
        size_t      d_acceptedTokens__;
        int         d_token__;
        int         d_nextToken__;
        size_t      d_state__;
        STYPE__    *d_vsp__;
        STYPE__     d_val__;
        STYPE__    d_nextVal__;
$insert LTYPEdata

        \@Base();

$insert debugdecl
        void ABORT() const;
        void ACCEPT() const;
        void ERROR() const;
        void clearin();
        bool debug() const;
        void pop__(size_t count = 1);
        void push__(size_t nextState);
        void popToken__();
        void pushToken__(int token);
        void reduce__(PI__ const &productionInfo);
        void errorVerbose__();
        size_t top__() const;

    public:
        void setDebug(bool mode);
}; 

inline bool \@Base::debug() const
{
    return d_debug__;
}

inline void \@Base::setDebug(bool mode)
{
    d_debug__ = mode;
}

$insert polymorphicInline

// As a convenience, when including ParserBase.h its symbols are available as
// symbols in the class Parser, too.
#define \@ \@Base

$insert namespace-close

#endif


