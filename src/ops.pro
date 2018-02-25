% Copyright 2013 by M. Ben-Ari. GNU GPL.

%  Logical operators

%  Alphabetic operators for internal use

:- op(650, xfy, xor).        /* exclusive or */
:- op(650, xfy, eqv).        /* equivalence  */ 
:- op(640, xfy, imp).        /* implication  */ 
:- op(630, xfy, or).         /* disjunction  */ 
:- op(620, xfy, and).        /* conjunction  */ 
:- op(610, fy,  neg).        /* negation     */

%  Symbolic operators for external use

:- op(650, xfy, +).          /* exclusive or */
:- op(650, xfy, <->).        /* equivalence  */ 
:- op(640, xfy, -->).        /* implication  */ 
:- op(630, xfy, v).          /* disjunction  */ 
:- op(620, xfy, ^).          /* conjunction  */ 
:- op(610, fy,  ~).          /* negation     */ 
