#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <rpc/xdr.h>

#include "demo_opcodes.h"
#include "demo_msg_body.h"
#include "demo_strings.h"

int decode_body (XDR *xdrs_de, struct RTRN_ALL *de, int *opcode, int *nargs)
{
   extern    int		toprttr;

   unsigned int		four = 4, eight = 8, twelve = 12, twenty = 20;
   char   		*dpc;
   int			*dpi, return_val;

   struct RTRN_ALL	*dp_all;

   /*  Decode the body based on the opcode.  */

   return_val = 0;

   dp_all = (struct RTRN_ALL *) de;

   switch (*opcode)
   {

      case OP_RTRN_ACK	:
         break;

      case OP_RTRN_DCN	:

         if (!xdr_int (xdrs_de, &(dp_all->dcn_body_struct.DEFCON)))
            {printf ("XDR_INT decode, New DEFCON, returns error.\n");}
         else
            {*nargs += 1;
             if (toprttr == 1)
                printf ("DEFCON recieved is %d .\n", dp_all->dcn_body_struct.DEFCON);}

         if (!xdr_int (xdrs_de, &(dp_all->dcn_body_struct.RP)))
            {printf ("XDR_INT decode, New RP, returns error.\n");}
         else
            {*nargs += 1;
             if (toprttr == 1)
                printf ("RP recieved is %d .\n", dp_all->dcn_body_struct.RP);}
 
         dpc = (char *) &(dp_all->dcn_body_struct.DEA);
         if (!xdr_string (xdrs_de, &dpc, eight))
            {printf ("XDR_STRING decode, New DEA, returns error.\n");}
         else
            {*nargs += 1;
             if (toprttr == 1)
                printf ("DEA recieved is %s .\n", dp_all->dcn_body_struct.DEA);}

         dpc = (char *) &(dp_all->dcn_body_struct.ROE);
         if (!xdr_string (xdrs_de, &dpc, twenty))
            {printf ("XDR_STRING decode, New ROE, returns error.\n");}
         else
            {*nargs += 1;
             if (toprttr == 1)
                printf ("ROE recieved is %s .\n", dp_all->dcn_body_struct.ROE);}

         dpc = (char *) &(dp_all->dcn_body_struct.Msn_Obj);
         if (!xdr_string (xdrs_de, &dpc, twenty))
            {printf ("XDR_STRING decode, New Msn Obj, returns error.\n");}
         else
            {*nargs += 1;
             if (toprttr == 1)
                printf ("Msn Obj recieved is %s .\n", dp_all->dcn_body_struct.Msn_Obj);}

         dpc = (char *) &(dp_all->dcn_body_struct.BP);
         if (!xdr_string (xdrs_de, &dpc, twenty))
            {printf ("XDR_STRING decode, New BP, returns error.\n");}
         else
            {*nargs += 1;
             if (toprttr == 1)
                printf ("BP recieved is %s .\n", dp_all->dcn_body_struct.BP);}

         break;

      case OP_RTRN_ERR	:

         dpc = (char *) &(dp_all->err_body_struct.ERR_TXT);
         if (!xdr_string (xdrs_de, &dpc, twenty))
            {printf ("XDR_STRING decode, unexpeced error, returns error.\n");}
         else
            {*nargs += 1;
             if (toprttr == 1)
                printf ("Error generated is %s .\n", dp_all->err_body_struct.ERR_TXT);}

         break;

      default		:
         printf ("Body decoder passed an illegal opcode of %d .\n", *opcode);
         return_val = 1;
         break;

   }

   if (toprttr == 1)
      printf ("Decoded %d fields for opcode %d.\n", *nargs, *opcode);

   return return_val;

}

