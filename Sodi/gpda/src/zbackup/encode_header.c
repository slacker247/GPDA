#include <stdio.h>
#include <rpc/xdr.h>

#include "demo_header.h"

void encode_header (XDR *xdrs_en, struct header *en, int *nargs)
{
   extern int		toprttr;

   *nargs = 0;

   /*  Build the message header, nine values.  */

   if (!xdr_int (xdrs_en, &(en->SrcID)))
      {printf ("XDR_INT encode, Source ID, returns error.\n");}
   else
      {*nargs += 1;}

   if (!xdr_int (xdrs_en, &(en->DstID)))
      {printf ("XDR_INT encode, Destination ID, returns error.\n");}
   else
      {*nargs += 1;}

   if (!xdr_int (xdrs_en, &(en->SCID)))
      {printf ("XDR_INT encode, Sim Cmdr ID, returns error.\n");}
   else
      {*nargs += 1;}

   if (!xdr_int (xdrs_en, &(en->opcode)))
      {printf ("XDR_INT encode, opcode, returns error.\n");}
   else
      {*nargs += 1;}

   if (!xdr_int (xdrs_en, &(en->SCactive)))
      {printf ("XDR_INT encode, Sim Cmdr active, returns error.\n");}
   else
      {*nargs += 1;}

   if (!xdr_float (xdrs_en, &(en->gvt)))
      {printf ("XDR_FLOAT encode, GVT, returns error.\n");}
   else
      {*nargs += 1;}

   if (!xdr_int (xdrs_en, &(en->reserved7)))
      {printf ("XDR_INT encode, Reserved Field 7, returns error.\n");}
   else
      {*nargs += 1;}

   if (!xdr_int (xdrs_en, &(en->reserved8)))
      {printf ("XDR_INT encode, Reserved Field 8, returns error.\n");}
   else
      {*nargs += 1;}

   if (!xdr_int (xdrs_en, &(en->reserved9)))
      {printf ("XDR_INT encode, Reserved Field 9, returns error.\n");}
   else
      {*nargs += 1;}

   if (toprttr == 1)
      printf ("\nEncoded %d fields in the header.\n", *nargs);
 
}










