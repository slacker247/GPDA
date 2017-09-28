/* hello.f -- translated by f2c (version 19991025).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__9 = 9;
static integer c__1 = 1;

/* Main program */ MAIN__()
{
    /* Builtin functions */
    integer s_wsle(), do_lio(), e_wsle();

    /* Fortran I/O blocks */
    static cilist io___1 = { 0, 6, 0, 0, 0 };


    s_wsle(&io___1);
    do_lio(&c__9, &c__1, "Hello, world.", (ftnlen)13);
    e_wsle();
} /* MAIN__ */

/* Main program alias */ int hello_ () { MAIN__ (); }
