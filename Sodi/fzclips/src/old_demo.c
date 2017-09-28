#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <rpc/xdr.h>

#include "demo_opcodes.h"
#include "demo_players.h"
#include "demo_header.h"
#include "demo_msg_body.h"
#include "demo_strings.h"

void translator (char *messages, char *decisions, unsigned int *xdrsize);

void encode_header (XDR *xdrs_en, struct header *en, int *nargs);
void decode_header (XDR *xdrs_de, struct header *de, int *nargs);

int  encode_body (XDR *xdrs_en, struct SEND_ALL *en, int *opcode, int *nargs);
int  decode_body (XDR *xdrs_de, struct RTRN_ALL *de, int *opcode, int *nargs);

main ()
{

   struct SEND_ALL      BPen_body;
   struct RTRN_ALL      BPde_body;
   struct header        BPen, BPde;
   int                  nargs_in, nargs_out;

   unsigned int		xdrsize;
   char   		*xdrbuf_en, *xdrbuf_de, buf[2000];
   XDR			xdrs;


   /*  Show opcode   0.  */
   /*  Build a message to the translator, set up the buffers. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.SrcID     = BtlP;
   BPen.DstID     = BMDC1_DR1;
   BPen.SCID      = Sim_Cmdr_ID;
   BPen.opcode    = OP_INIT_SC;
   BPen.SCactive  = Sim_Cmdr_Active;
   BPen.gvt       = 1234.5;
   BPen.reserved7 = 0;
   BPen.reserved8 = 0;
   BPen.reserved9 = 0;

   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);

   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Messages done, remove all traces of them.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 502.  */

   /*  Show opcode 301.  */
   /*  Build a message to the translator, set up the buffer. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.opcode    = OP_INIT_MO;
   BPen.gvt       = 1234.6;

   BPen_body.init_mo_body_struct.DEFCON_Level                 = 5;
   BPen_body.init_mo_body_struct.RP                           = 2;
   BPen_body.init_mo_body_struct.Msn_Obj_Pms                  = 0.78;
   BPen_body.init_mo_body_struct.Msn_Obj_Mode                 = 2;
   BPen_body.init_mo_body_struct.Msn_Obj_Withhold             = 4;
   BPen_body.init_mo_body_struct.Msn_Obj_Add_Bstr             = 2;

   strcpy (BPen_body.init_mo_body_struct.DEA,                     DEA_HOLD);
   strcpy (BPen_body.init_mo_body_struct.ROE,                     ROE_NA);
   strcpy (BPen_body.init_mo_body_struct.Msn_Obj_Name,            MSN_OBJ_CUR);
   strcpy (BPen_body.init_mo_body_struct.Msn_Obj_Strategy,        "ASSURED");
   strcpy (BPen_body.init_mo_body_struct.Msn_Obj_Tactic,          "ASSURED");

   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);

   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 502.  */

   /*  Show opcode 301.  */
   /*  Build a message to the translator, set up the buffer. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.opcode    = OP_INIT_MO;
   BPen.gvt       = 1234.7;

   BPen_body.init_mo_body_struct.DEFCON_Level                 = 5;
   BPen_body.init_mo_body_struct.RP                           = 2;
   BPen_body.init_mo_body_struct.Msn_Obj_Pms                  = 0.85;
   BPen_body.init_mo_body_struct.Msn_Obj_Mode                 = 3;
   BPen_body.init_mo_body_struct.Msn_Obj_Withhold             = 2;
   BPen_body.init_mo_body_struct.Msn_Obj_Add_Bstr             = 2;

   strcpy (BPen_body.init_mo_body_struct.DEA,                     DEA_HOLD);
   strcpy (BPen_body.init_mo_body_struct.ROE,                     ROE_CONUS);
   strcpy (BPen_body.init_mo_body_struct.Msn_Obj_Name,            MSN_OBJ_ALT);
   strcpy (BPen_body.init_mo_body_struct.Msn_Obj_Strategy,        "ASSURED");
   strcpy (BPen_body.init_mo_body_struct.Msn_Obj_Tactic,          "ASSURED");

   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);

   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 502.  */

   /*  Show opcode 302.  */
   /*  Build a message to the translator, set up the buffer. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.opcode    = OP_INIT_BP;
   BPen.gvt       = 1234.8;

   BPen_body.init_bp_body_struct.DEFCON_Level                 = 5;
   BPen_body.init_bp_body_struct.RP                           = 2;
   BPen_body.init_bp_body_struct.BP_Override_Salvo            = 1;
   BPen_body.init_bp_body_struct.BP_Launch_Mode               = 1;
   BPen_body.init_bp_body_struct.BP_RV_Likelihood_Threshold   = 0.75;
   BPen_body.init_bp_body_struct.BP_Pk_Cutoff                 = 0.2200;
   BPen_body.init_bp_body_struct.BP_Asset_Weight_Population   = 1;
   BPen_body.init_bp_body_struct.BP_Asset_Weight_Military     = 0;
   BPen_body.init_bp_body_struct.BP_Asset_Weight_Self_Defense = 2;
   BPen_body.init_bp_body_struct.BP_Asset_Weight_NCA          = 0;
   BPen_body.init_bp_body_struct.BP_Asset_Weight_Industrial   = 2;

   strcpy (BPen_body.init_bp_body_struct.DEA,                     DEA_HOLD);
   strcpy (BPen_body.init_bp_body_struct.ROE,                     ROE_NA);
   strcpy (BPen_body.init_bp_body_struct.BP_Name,                 BP_GEN_UMB);
   strcpy (BPen_body.init_bp_body_struct.BP_Mode,                 "ASSURED");
   strcpy (BPen_body.init_bp_body_struct.BP_Tgt_Val_Cut_Off,      "Medium");
   strcpy (BPen_body.init_bp_body_struct.BP_Accept_Kill_Criteria, "Kill");

   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);

   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 502.  */

   /*  Show opcode 302.  */
   /*  Build a message to the translator, set up the buffer. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.opcode    = OP_INIT_BP;
   BPen.gvt       = 1234.9;

   BPen_body.init_bp_body_struct.DEFCON_Level                 = 4;
   BPen_body.init_bp_body_struct.RP                           = 2;
   BPen_body.init_bp_body_struct.BP_Override_Salvo            = 0;
   BPen_body.init_bp_body_struct.BP_Launch_Mode               = 2;
   BPen_body.init_bp_body_struct.BP_RV_Likelihood_Threshold   = 0.60;
   BPen_body.init_bp_body_struct.BP_Pk_Cutoff                 = 0.0001;
   BPen_body.init_bp_body_struct.BP_Asset_Weight_Population   = 1;
   BPen_body.init_bp_body_struct.BP_Asset_Weight_Military     = 2;
   BPen_body.init_bp_body_struct.BP_Asset_Weight_Self_Defense = 2;
   BPen_body.init_bp_body_struct.BP_Asset_Weight_NCA          = 1;
   BPen_body.init_bp_body_struct.BP_Asset_Weight_Industrial   = 3;

   strcpy (BPen_body.init_bp_body_struct.DEA,                     DEA_HOLD);
   strcpy (BPen_body.init_bp_body_struct.ROE,                     ROE_CONUS);
   strcpy (BPen_body.init_bp_body_struct.BP_Name,                 BP_AR_DEF);
   strcpy (BPen_body.init_bp_body_struct.BP_Mode,                 "ASSURED");
   strcpy (BPen_body.init_bp_body_struct.BP_Tgt_Val_Cut_Off,      "Low");
   strcpy (BPen_body.init_bp_body_struct.BP_Accept_Kill_Criteria, "Kill");

   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);

   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 502.  */

   /*  Show opcode 303.  */
   /*  Build a message to the translator, set up the buffer. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.opcode    = OP_INIT_WA;
   BPen.gvt       = 1235.0;

   BPen_body.wa_body_struct.Num_GBI_Farms = 5;

   BPen_body.wa_body_struct.Farm_Val[ 0][0] = 1101;
   BPen_body.wa_body_struct.Farm_Val[ 0][1] = 6;
   BPen_body.wa_body_struct.Farm_Val[ 0][2] = 4;

   BPen_body.wa_body_struct.Farm_Val[ 1][0] = 3101;
   BPen_body.wa_body_struct.Farm_Val[ 1][1] = 2;
   BPen_body.wa_body_struct.Farm_Val[ 1][2] = 1;

   BPen_body.wa_body_struct.Farm_Val[ 2][0] = 3111;
   BPen_body.wa_body_struct.Farm_Val[ 2][1] = 12;
   BPen_body.wa_body_struct.Farm_Val[ 2][2] = 6;

   BPen_body.wa_body_struct.Farm_Val[ 3][0] = 3121;
   BPen_body.wa_body_struct.Farm_Val[ 3][1] = 50;
   BPen_body.wa_body_struct.Farm_Val[ 3][2] = 25;

   BPen_body.wa_body_struct.Farm_Val[ 4][0] = 3131;
   BPen_body.wa_body_struct.Farm_Val[ 4][1] = 80;
   BPen_body.wa_body_struct.Farm_Val[ 4][2] = 20;

   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);

   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 502.  */

   /*  Show opcode 101.  */
   /*  Build a message to the translator, set up the buffer. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.opcode    = OP_DEFCON;
   BPen.gvt       = 1234.7;

   BPen_body.defcon_body_struct.DEFCON = 5;

   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);

   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Show how the six returned field values can be retrieved.  */

       printf ("\nDEFCON recieved is %d .\n", BPde_body.dcn_body_struct.DEFCON);

       printf ("RP recieved is %d .\n", BPde_body.dcn_body_struct.RP);
 
       printf ("DEA recieved is %s .\n", BPde_body.dcn_body_struct.DEA);

       printf ("ROE recieved is %s .\n", BPde_body.dcn_body_struct.ROE);

       printf ("Msn Obj recieved is %s .\n", BPde_body.dcn_body_struct.Msn_Obj);

       printf ("BP recieved is %s .\n", BPde_body.dcn_body_struct.BP);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 501.  */

   /*  Show opcode 102.  */
   /*  Build a message to the translator, set up the buffer. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.opcode    = OP_RP;
   BPen.gvt       = 1234.8;

   BPen_body.rp_body_struct.RP = 2;

   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);

   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 501.  */

   /*  Show opcode 103.  */
   /*  Build a message to the translator, set up the buffer. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.opcode    = OP_DEA;
   BPen.gvt       = 1234.9;

   strcpy (BPen_body.dea_body_struct.DEA, DEA_HOLD);

   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);

   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 501.  */

   /*  Show opcode 200, an error causer.  */
   /*  Build a message to the translator, set up the buffer. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.opcode    = 200;
   BPen.gvt       = 2345.9;

   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);

   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Show how the error message can be retrieved.  */

       printf ("\nError generated is %s .\n", BPde_body.err_body_struct.ERR_TXT);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 503.  */

   /*  Show opcode 104.  */
   /*  Build a message to the translator, set up the buffer. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.opcode    = OP_ROE;
   BPen.gvt       = 1236.0;

   strcpy (BPen_body.roe_body_struct.ROE, ROE_NA);

   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);

   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 501.  */

   /*  Show opcode 105.  */
   /*  Build a message to the translator, set up the buffer. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.opcode    = OP_MSN_OBJ;
   BPen.gvt       = 1236.2;

   strcpy (BPen_body.msn_obj_body_struct.Msn_Obj, MSN_OBJ_CUR);

   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);

   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 501.  */

   /*  Show opcode 106.  */
   /*  Build a message to the translator, set up the buffer. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.opcode    = OP_BP;
   BPen.gvt       = 1236.4;

   strcpy (BPen_body.bp_body_struct.BP, BP_GEN_UMB);

   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);

   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 501.  */

   /*  Show opcode 100.  */
   /*  Build a message to the translator, set up the buffer. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.opcode    = OP_DB_STATUS;
   BPen.gvt       = 1254.4;

   BPen_body.db_status_body_struct.DEFCON_Level                 = 4;
   BPen_body.db_status_body_struct.RP                           = 2;
   BPen_body.db_status_body_struct.Msn_Obj_Pms                  = 0.73;
   BPen_body.db_status_body_struct.Msn_Obj_Mode                 = 2;
   BPen_body.db_status_body_struct.Msn_Obj_Withhold             = 4;
   BPen_body.db_status_body_struct.Msn_Obj_Add_Bstr             = 2;
   BPen_body.db_status_body_struct.BP_Override_Salvo            = 1;
   BPen_body.db_status_body_struct.BP_Launch_Mode               = 1;
   BPen_body.db_status_body_struct.BP_RV_Likelihood_Threshold   = 0.68;
   BPen_body.db_status_body_struct.BP_Pk_Cutoff                 = 0.2200;
   BPen_body.db_status_body_struct.BP_Asset_Weight_Population   = 1;
   BPen_body.db_status_body_struct.BP_Asset_Weight_Military     = 0;
   BPen_body.db_status_body_struct.BP_Asset_Weight_Self_Defense = 2;
   BPen_body.db_status_body_struct.BP_Asset_Weight_NCA          = 0;
   BPen_body.db_status_body_struct.BP_Asset_Weight_Industrial   = 2;

   strcpy (BPen_body.db_status_body_struct.DEA,                     DEA_HOLD);
   strcpy (BPen_body.db_status_body_struct.ROE,                     ROE_NA);
   strcpy (BPen_body.db_status_body_struct.Msn_Obj_Name,            MSN_OBJ_CUR);
   strcpy (BPen_body.db_status_body_struct.Msn_Obj_Strategy,        "ASSURED");
   strcpy (BPen_body.db_status_body_struct.Msn_Obj_Tactic,          "ASSURED");
   strcpy (BPen_body.db_status_body_struct.BP_Name,                 BP_GEN_UMB);
   strcpy (BPen_body.db_status_body_struct.BP_Mode,                 "ASSURED");
   strcpy (BPen_body.db_status_body_struct.BP_Tgt_Val_Cut_Off,      "Medium");
   strcpy (BPen_body.db_status_body_struct.BP_Accept_Kill_Criteria, "Kill");

   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);

   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 502.  */

   /*  Show opcode 108.  */
   /*  Build a message to the translator, set up the buffer. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.opcode    = OP_TRK_ENGMT;
   BPen.gvt       = 1258.4;

   BPen_body.trk_engmt_body_struct.Cur_Eng_Num_Wea = 0;
   BPen_body.trk_engmt_body_struct.Cur_Eng_TTI     = BPen.gvt + 1800.0;
   BPen_body.trk_engmt_body_struct.Cur_Eng_Pk      = 0.50;
   BPen_body.trk_engmt_body_struct.Eng_Opp_Remain  = 2;

   strcpy (BPen_body.trk_engmt_body_struct.Eng_Status, "Pre Plan");

   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);

   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 502.  */

   /*  Show opcode 130.  */
   /*  Build a message to the translator, set up the buffer. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.opcode    = OP_POT_EVENT;
   BPen.gvt       = 1259.7;

   BPen_body.pot_event_body_struct.Exp_Tgts          = 1;
   BPen_body.pot_event_body_struct.Leth_Val          = 0.81;
   BPen_body.pot_event_body_struct.Launch_Time       = BPen.gvt - 800.0;
   BPen_body.pot_event_body_struct.Impact_Lat        = 28.73;
   BPen_body.pot_event_body_struct.Impact_Lon        = 111.07;
   BPen_body.pot_event_body_struct.Earl_Impact_Time  = BPen.gvt + 1000.0;
   BPen_body.pot_event_body_struct.Num_Boosters      = 1;
   BPen_body.pot_event_body_struct.Leth_Exp          = 1;

   strcpy (BPen_body.pot_event_body_struct.Track_ID,           "11:22:33");
   strcpy (BPen_body.pot_event_body_struct.Object_Type,        "Cluster");
   strcpy (BPen_body.pot_event_body_struct.Missile_Type,       "SS-88");
   strcpy (BPen_body.pot_event_body_struct.Missile_Class,      "ICBM");
   strcpy (BPen_body.pot_event_body_struct.Launch_Country,     "China");
   strcpy (BPen_body.pot_event_body_struct.Launch_Site,        "Hough Ling");
   strcpy (BPen_body.pot_event_body_struct.Pred_Impact_Region, "Unknown");
   strcpy (BPen_body.pot_event_body_struct.Trk_Status,         "In Trk");

   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);

   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 501.  */

   /*  Show opcode 107.  */
   /*  Build a message to the translator, set up the buffer. */

   xdrbuf_en = (char *)malloc(2000);
   xdrbuf_de = (char *)malloc(2000);
   xdrmem_create (&xdrs, xdrbuf_en, (unsigned)2000, XDR_ENCODE);

   BPen.opcode    = OP_WA;
   BPen.gvt       = 1261.9;

   BPen_body.wa_body_struct.Num_GBI_Farms = 6;

   BPen_body.wa_body_struct.Farm_Val[ 0][0] = 1101;
   BPen_body.wa_body_struct.Farm_Val[ 0][1] = 6;
   BPen_body.wa_body_struct.Farm_Val[ 0][2] = 4;

   BPen_body.wa_body_struct.Farm_Val[ 1][0] = 3101;
   BPen_body.wa_body_struct.Farm_Val[ 1][1] = 2;
   BPen_body.wa_body_struct.Farm_Val[ 1][2] = 1;

   BPen_body.wa_body_struct.Farm_Val[ 2][0] = 3111;
   BPen_body.wa_body_struct.Farm_Val[ 2][1] = 12;
   BPen_body.wa_body_struct.Farm_Val[ 2][2] = 6;

   BPen_body.wa_body_struct.Farm_Val[ 3][0] = 3121;
   BPen_body.wa_body_struct.Farm_Val[ 3][1] = 50;
   BPen_body.wa_body_struct.Farm_Val[ 3][2] = 25;

   BPen_body.wa_body_struct.Farm_Val[ 4][0] = 3131;
   BPen_body.wa_body_struct.Farm_Val[ 4][1] = 80;
   BPen_body.wa_body_struct.Farm_Val[ 4][2] = 20;

   BPen_body.wa_body_struct.Farm_Val[ 5][0] = 4131;
   BPen_body.wa_body_struct.Farm_Val[ 5][1] = 1234;
   BPen_body.wa_body_struct.Farm_Val[ 5][2] = 617;

   encode_header (&xdrs, &BPen, &nargs_in);

   if (encode_body (&xdrs, &BPen_body, &BPen.opcode, &nargs_in) != 0)
      printf ("Encode failed for opcode %d .\n", BPen.opcode);

   translator (xdrbuf_en, xdrbuf_de, &xdrsize);

   /*  Decode in the battle planner now.  */

   xdrmem_create (&xdrs, xdrbuf_de, (unsigned)2000, XDR_DECODE);

   decode_header (&xdrs, &BPde, &nargs_out);

   if (decode_body (&xdrs, &BPde_body, &BPde.opcode, &nargs_out) != 0)
      printf ("Decode failed for opcode %d .\n", BPde.opcode);

   /*  Message done, remove all traces of it.  */

   xdr_destroy (&xdrs);
   free (xdrbuf_en);
   free (xdrbuf_de);
   /*  Show opcode 501.  */

}

