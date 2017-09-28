#include <stdio.h>
#include <rpc/xdr.h>

#include "demo_header.h"

void decode_header (XDR *xdrs_de, struct header *de, int *nargs)
{
   extern  int   	toprttr;

   *nargs = 0;

   /*  Retrieve the message header, nine values.  */

   if (!xdr_int (xdrs_de, &(de->SrcID)))
      {printf ("XDR_INT decode, Source ID, returns error.\n");}
   else
      {*nargs += 1;
       if (toprttr == 1)
          printf ("Source ID recieved is %d .\n", de->SrcID);}

   if (!xdr_int (xdrs_de, &(de->DstID)))
      {printf ("XDR_INT decode, Destination ID, returns error.\n");}
   else
      {*nargs += 1;
       if (toprttr ==1)
          printf ("Destination ID recieved is %d .\n", de->DstID);}

   if (!xdr_int (xdrs_de, &(de->SCID)))
      {printf ("XDR_INT decode, Sim Cmdr ID, returns error.\n");}
   else
      {*nargs += 1;
       if (toprttr == 1)
          printf ("Sim Cmdr ID recieved is %d .\n", de->SCID);}

   if (!xdr_int (xdrs_de, &(de->opcode)))
      {printf ("XDR_INT decode, opcode, returns error.\n");}
   else
      {*nargs += 1;
       if (toprttr == 1)
          printf ("Opcode recieved is %d .\n", de->opcode);}

   if (!xdr_int (xdrs_de, &(de->SCactive)))
      {printf ("XDR_INT decode, Sim Cmdr active, returns error.\n");}
   else
      {*nargs += 1;
       if (toprttr == 1)
          printf ("Sim Cmdr Active recieved is %d .\n", de->SCactive);}

   if (!xdr_float (xdrs_de, &(de->gvt)))
      {printf ("XDR_INT decode, GVT, returns error.\n");}
   else
      {*nargs += 1;
       if (toprttr == 1)
          printf ("GVT recieved is %f .\n", de->gvt);}

   if (!xdr_int (xdrs_de, &(de->reserved7)))
      {printf ("XDR_INT decode, Reserved Field 7, returns error.\n");}
   else
      {*nargs += 1;
       if (toprttr == 1)
          printf ("Reserved # 7 recieved is %d .\n", de->reserved7);}

   if (!xdr_int (xdrs_de, &(de->reserved8)))
      {printf ("XDR_INT decode, Reserved Field 8, returns error.\n");}
   else
      {*nargs += 1;
       if (toprttr == 1)
          printf ("Reserved # 8 recieved is %d .\n", de->reserved8);}

   if (!xdr_int (xdrs_de, &(de->reserved9)))
      {printf ("XDR_INT decode, Reserved Field 9, returns error.\n");}
   else
      {*nargs += 1;
       if (toprttr == 1)
          printf ("Reserved # 9 recieved is %d .\n", de->reserved9);}

   if (toprttr == 1)
      printf ("Decoded %d fields in the header.\n", *nargs);

}
