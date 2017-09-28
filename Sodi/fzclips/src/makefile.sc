
#-------------------------------------------------------
# This is the version for Solaris 2.4 workstation
#
#		 FZ_CLIPS 6.04 
#-------------------------------------------------------
#
#-------------------------------------------------------
# The paths to the X libs and include files should
# be changed to whatever appropriate for the system
# that xclips color are made.
# ------------------------------------------------------
CC = gcc
WHERE_XLIBS_ARE =	/usr/openwin/lib
#WHERE_UCBLIBS_ARE =	/usr/ucblib
WHERE_INCL_XFILES_ARE =	/usr/openwin/include
#WHERE_INCL_UCBFILES_ARE = /usr/ucbinclude

#--------------------------------------------------------------------
#  Please! do not have -O on because there is an unresolved bug either
#  in the interface code or the optimizer that cause xclips to crash
#--------------------------------------------------------------------

DEBUG = 
#DEBUG = -g
CFLAGS = $(DEBUG) $(INCLUDES)
BINDIR = /usr/local/bin
#LDFLAGS = -L$(WHERE_UCBLIBS_ARE)
#INCLUDES = -I$(FUZZY_Sources) -I$(WHERE_INCL_UCBFILES_ARE)  
INCLUDES = -I$(FUZZY_Sources)   
#LIBS = -lm -ltermcap -lucb -L. -lfclips
LIBS = -lm -ltermcap 

FUZZY_Sources = /home/mlcheek/NMD_demo/FZCLIPS


#----------------------------------------------------------
#  Things related to Shower Problem example - deleted 
#----------------------------------------------------------

#
# Standard Fuzzy Clips stuff
#

SRCS =  $(FUZZY_Sources)/agenda.c\
        $(FUZZY_Sources)/analysis.c\
        $(FUZZY_Sources)/bsave.c\
        $(FUZZY_Sources)/cfdef.c\
        $(FUZZY_Sources)/conscomp.c\
        $(FUZZY_Sources)/engine.c\
        $(FUZZY_Sources)/evaluatn.c\
        $(FUZZY_Sources)/exprnbin.c\
        $(FUZZY_Sources)/factbld.c\
        $(FUZZY_Sources)/factcom.c\
        $(FUZZY_Sources)/factgen.c\
        $(FUZZY_Sources)/facthsh.c\
        $(FUZZY_Sources)/factmch.c\
        $(FUZZY_Sources)/factmngr.c\
        $(FUZZY_Sources)/factprt.c\
        $(FUZZY_Sources)/factrete.c\
        $(FUZZY_Sources)/factrhs.c\
        $(FUZZY_Sources)/fuzzycom.c\
        $(FUZZY_Sources)/fuzzydef.c\
        $(FUZZY_Sources)/fuzzylhs.c\
        $(FUZZY_Sources)/fuzzymod.c\
        $(FUZZY_Sources)/fuzzypsr.c\
        $(FUZZY_Sources)/fuzzyrhs.c\
        $(FUZZY_Sources)/fuzzyutl.c\
        $(FUZZY_Sources)/prntutil.c\
        $(FUZZY_Sources)/reorder.c\
        $(FUZZY_Sources)/rulebin.c\
        $(FUZZY_Sources)/rulecmp.c\
        $(FUZZY_Sources)/ruledlt.c\
        $(FUZZY_Sources)/rulelhs.c\
        $(FUZZY_Sources)/rulepsr.c\
        $(FUZZY_Sources)/scanner.c\
        $(FUZZY_Sources)/symblbin.c\
        $(FUZZY_Sources)/symblcmp.c\
        $(FUZZY_Sources)/symbol.c\
        $(FUZZY_Sources)/sysdep.c\
        $(FUZZY_Sources)/tmpltbin.c\
        $(FUZZY_Sources)/tmpltcmp.c\
        $(FUZZY_Sources)/tmpltdef.c\
        $(FUZZY_Sources)/tmpltlhs.c\
        $(FUZZY_Sources)/tmpltpsr.c\
	$(FUZZY_Sources)/tmpltutl.c\
        $(FUZZY_Sources)/xclipstext.c\
        $(FUZZY_Sources)/xmain.c\
	\
	$(FUZZY_Sources)/decode_body.c\
	$(FUZZY_Sources)/decode_header.c\
	$(FUZZY_Sources)/encode_body.c\
	$(FUZZY_Sources)/encode_header.c\
	$(FUZZY_Sources)/char_2_num.c\
	$(FUZZY_Sources)/num_2_char.c\
	$(FUZZY_Sources)/replan.c\
	$(FUZZY_Sources)/roe.c\
	$(FUZZY_Sources)/trace.c\
	\
	$(FUZZY_Sources)/argacces.c\
	$(FUZZY_Sources)/bload.c\
        $(FUZZY_Sources)/bmathfun.c\
        $(FUZZY_Sources)/classcom.c\
        $(FUZZY_Sources)/classexm.c\
        $(FUZZY_Sources)/classfun.c\
        $(FUZZY_Sources)/classinf.c\
        $(FUZZY_Sources)/classini.c\
        $(FUZZY_Sources)/classpsr.c\
        $(FUZZY_Sources)/clsltpsr.c\
        $(FUZZY_Sources)/commline.c\
        $(FUZZY_Sources)/constrct.c\
        $(FUZZY_Sources)/constrnt.c\
        $(FUZZY_Sources)/crstrtgy.c\
        $(FUZZY_Sources)/cstrcbin.c\
        $(FUZZY_Sources)/cstrccom.c\
        $(FUZZY_Sources)/cstrcpsr.c\
        $(FUZZY_Sources)/cstrnbin.c\
        $(FUZZY_Sources)/cstrnchk.c\
        $(FUZZY_Sources)/cstrncmp.c\
        $(FUZZY_Sources)/cstrnops.c\
        $(FUZZY_Sources)/cstrnpsr.c\
        $(FUZZY_Sources)/cstrnutl.c\
        $(FUZZY_Sources)/default.c\
        $(FUZZY_Sources)/defins.c\
        $(FUZZY_Sources)/developr.c\
        $(FUZZY_Sources)/dffctbin.c\
        $(FUZZY_Sources)/dffctbsc.c\
        $(FUZZY_Sources)/dffctcmp.c\
        $(FUZZY_Sources)/dffctdef.c\
        $(FUZZY_Sources)/dffctpsr.c\
        $(FUZZY_Sources)/dffnxbin.c\
        $(FUZZY_Sources)/dffnxcmp.c\
        $(FUZZY_Sources)/dffnxexe.c\
        $(FUZZY_Sources)/dffnxfun.c\
        $(FUZZY_Sources)/dffnxpsr.c\
        $(FUZZY_Sources)/dfinsbin.c\
        $(FUZZY_Sources)/dfinscmp.c\
        $(FUZZY_Sources)/drive.c\
	$(FUZZY_Sources)/edmain.c\
        $(FUZZY_Sources)/emathfun.c\
        $(FUZZY_Sources)/expressn.c\
        $(FUZZY_Sources)/exprnops.c\
        $(FUZZY_Sources)/exprnpsr.c\
        $(FUZZY_Sources)/extnfunc.c\
        $(FUZZY_Sources)/factbin.c\
        $(FUZZY_Sources)/factcmp.c\
	$(FUZZY_Sources)/factfun.c\
        $(FUZZY_Sources)/factlhs.c\
        $(FUZZY_Sources)/filecom.c\
        $(FUZZY_Sources)/filertr.c\
        $(FUZZY_Sources)/generate.c\
        $(FUZZY_Sources)/genrcbin.c\
        $(FUZZY_Sources)/genrccmp.c\
        $(FUZZY_Sources)/genrccom.c\
        $(FUZZY_Sources)/genrcexe.c\
        $(FUZZY_Sources)/genrcfun.c\
        $(FUZZY_Sources)/genrcpsr.c\
        $(FUZZY_Sources)/globlbin.c\
        $(FUZZY_Sources)/globlbsc.c\
        $(FUZZY_Sources)/globlcmp.c\
        $(FUZZY_Sources)/globlcom.c\
        $(FUZZY_Sources)/globldef.c\
        $(FUZZY_Sources)/globlpsr.c\
        $(FUZZY_Sources)/immthpsr.c\
        $(FUZZY_Sources)/incrrset.c\
        $(FUZZY_Sources)/inherpsr.c\
        $(FUZZY_Sources)/inscom.c\
        $(FUZZY_Sources)/insfile.c\
        $(FUZZY_Sources)/insfun.c\
        $(FUZZY_Sources)/insmngr.c\
        $(FUZZY_Sources)/insmoddp.c\
        $(FUZZY_Sources)/insmult.c\
        $(FUZZY_Sources)/inspsr.c\
        $(FUZZY_Sources)/insquery.c\
        $(FUZZY_Sources)/insqypsr.c\
        $(FUZZY_Sources)/iofun.c\
        $(FUZZY_Sources)/lgcldpnd.c\
        $(FUZZY_Sources)/memory.c\
        $(FUZZY_Sources)/miscfun.c\
        $(FUZZY_Sources)/modulbin.c\
        $(FUZZY_Sources)/modulbsc.c\
        $(FUZZY_Sources)/modulcmp.c\
        $(FUZZY_Sources)/moduldef.c\
        $(FUZZY_Sources)/modulpsr.c\
        $(FUZZY_Sources)/modulutl.c\
        $(FUZZY_Sources)/msgcom.c\
        $(FUZZY_Sources)/msgfun.c\
        $(FUZZY_Sources)/msgpass.c\
        $(FUZZY_Sources)/msgpsr.c\
        $(FUZZY_Sources)/multifld.c\
        $(FUZZY_Sources)/multifun.c\
        $(FUZZY_Sources)/objbin.c\
        $(FUZZY_Sources)/objcmp.c\
        $(FUZZY_Sources)/objrtbin.c\
        $(FUZZY_Sources)/objrtbld.c\
        $(FUZZY_Sources)/objrtcmp.c\
        $(FUZZY_Sources)/objrtfnx.c\
        $(FUZZY_Sources)/objrtgen.c\
        $(FUZZY_Sources)/objrtmch.c\
        $(FUZZY_Sources)/pattern.c\
        $(FUZZY_Sources)/pprint.c\
        $(FUZZY_Sources)/prccode.c\
        $(FUZZY_Sources)/prcdrfun.c\
        $(FUZZY_Sources)/prcdrpsr.c\
        $(FUZZY_Sources)/prdctfun.c\
        $(FUZZY_Sources)/reteutil.c\
        $(FUZZY_Sources)/retract.c\
        $(FUZZY_Sources)/router.c\
        $(FUZZY_Sources)/rulebld.c\
        $(FUZZY_Sources)/rulebsc.c\
        $(FUZZY_Sources)/rulecom.c\
        $(FUZZY_Sources)/rulecstr.c\
        $(FUZZY_Sources)/ruledef.c\
        $(FUZZY_Sources)/strngfun.c\
        $(FUZZY_Sources)/strngrtr.c\
        $(FUZZY_Sources)/textpro.c\
        $(FUZZY_Sources)/translator.c\
	$(FUZZY_Sources)/tmpltbsc.c\
        $(FUZZY_Sources)/tmpltfun.c\
        $(FUZZY_Sources)/tmpltrhs.c\
        $(FUZZY_Sources)/utility.c\
        $(FUZZY_Sources)/watch.c\
      
OBJS =  $(FUZZY_Sources)/agenda.o\
        $(FUZZY_Sources)/analysis.o\
        $(FUZZY_Sources)/bsave.o\
        $(FUZZY_Sources)/cfdef.o\
        $(FUZZY_Sources)/conscomp.o\
        $(FUZZY_Sources)/engine.o\
        $(FUZZY_Sources)/evaluatn.o\
        $(FUZZY_Sources)/exprnbin.o\
        $(FUZZY_Sources)/factbld.o\
        $(FUZZY_Sources)/factcom.o\
        $(FUZZY_Sources)/factgen.o\
        $(FUZZY_Sources)/facthsh.o\
        $(FUZZY_Sources)/factmch.o\
        $(FUZZY_Sources)/factmngr.o\
        $(FUZZY_Sources)/factprt.o\
        $(FUZZY_Sources)/factrete.o\
        $(FUZZY_Sources)/factrhs.o\
        $(FUZZY_Sources)/fuzzycom.o\
        $(FUZZY_Sources)/fuzzydef.o\
        $(FUZZY_Sources)/fuzzylhs.o\
        $(FUZZY_Sources)/fuzzymod.o\
        $(FUZZY_Sources)/fuzzypsr.o\
        $(FUZZY_Sources)/fuzzyrhs.o\
        $(FUZZY_Sources)/fuzzyutl.o\
	\
	$(FUZZY_Sources)/decode_body.o\
	$(FUZZY_Sources)/decode_header.o\
	$(FUZZY_Sources)/encode_body.o\
	$(FUZZY_Sources)/encode_header.o\
	$(FUZZY_Sources)/char_2_num.o\
	$(FUZZY_Sources)/num_2_char.o\
 	$(FUZZY_Sources)/translator.o\
	$(FUZZY_Sources)/replan.o\
	$(FUZZY_Sources)/roe.o\
	$(FUZZY_Sources)/trace.o\
	\
        $(FUZZY_Sources)/prntutil.o\
        $(FUZZY_Sources)/reorder.o\
        $(FUZZY_Sources)/rulebin.o\
        $(FUZZY_Sources)/rulecmp.o\
        $(FUZZY_Sources)/ruledlt.o\
        $(FUZZY_Sources)/rulelhs.o\
        $(FUZZY_Sources)/rulepsr.o\
        $(FUZZY_Sources)/scanner.o\
        $(FUZZY_Sources)/symblbin.o\
        $(FUZZY_Sources)/symblcmp.o\
        $(FUZZY_Sources)/symbol.o\
        $(FUZZY_Sources)/sysdep.o\
        $(FUZZY_Sources)/tmpltbin.o\
        $(FUZZY_Sources)/tmpltcmp.o\
        $(FUZZY_Sources)/tmpltdef.o\
        $(FUZZY_Sources)/tmpltlhs.o\
        $(FUZZY_Sources)/tmpltpsr.o\
        $(FUZZY_Sources)/argacces.o\
        $(FUZZY_Sources)/bload.o\
        $(FUZZY_Sources)/bmathfun.o\
        $(FUZZY_Sources)/classcom.o\
        $(FUZZY_Sources)/classexm.o\
        $(FUZZY_Sources)/classfun.o\
        $(FUZZY_Sources)/classinf.o\
        $(FUZZY_Sources)/classini.o\
        $(FUZZY_Sources)/classpsr.o\
        $(FUZZY_Sources)/clsltpsr.o\
        $(FUZZY_Sources)/commline.o\
        $(FUZZY_Sources)/constrct.o\
        $(FUZZY_Sources)/constrnt.o\
        $(FUZZY_Sources)/crstrtgy.o\
        $(FUZZY_Sources)/cstrcbin.o\
        $(FUZZY_Sources)/cstrccom.o\
        $(FUZZY_Sources)/cstrcpsr.o\
        $(FUZZY_Sources)/cstrnbin.o\
        $(FUZZY_Sources)/cstrnchk.o\
        $(FUZZY_Sources)/cstrncmp.o\
        $(FUZZY_Sources)/cstrnops.o\
        $(FUZZY_Sources)/cstrnpsr.o\
        $(FUZZY_Sources)/cstrnutl.o\
        $(FUZZY_Sources)/default.o\
        $(FUZZY_Sources)/defins.o\
        $(FUZZY_Sources)/developr.o\
        $(FUZZY_Sources)/dffctbin.o\
        $(FUZZY_Sources)/dffctbsc.o\
        $(FUZZY_Sources)/dffctcmp.o\
        $(FUZZY_Sources)/dffctdef.o\
        $(FUZZY_Sources)/dffctpsr.o\
        $(FUZZY_Sources)/dffnxbin.o\
        $(FUZZY_Sources)/dffnxcmp.o\
        $(FUZZY_Sources)/dffnxexe.o\
        $(FUZZY_Sources)/dffnxfun.o\
        $(FUZZY_Sources)/dffnxpsr.o\
        $(FUZZY_Sources)/dfinsbin.o\
        $(FUZZY_Sources)/dfinscmp.o\
        $(FUZZY_Sources)/drive.o\
        $(FUZZY_Sources)/emathfun.o\
        $(FUZZY_Sources)/expressn.o\
        $(FUZZY_Sources)/exprnops.o\
        $(FUZZY_Sources)/exprnpsr.o\
        $(FUZZY_Sources)/extnfunc.o\
        $(FUZZY_Sources)/factbin.o\
        $(FUZZY_Sources)/factcmp.o\
        $(FUZZY_Sources)/factfun.o\
        $(FUZZY_Sources)/factlhs.o\
        $(FUZZY_Sources)/filecom.o\
        $(FUZZY_Sources)/filertr.o\
        $(FUZZY_Sources)/generate.o\
        $(FUZZY_Sources)/genrcbin.o\
        $(FUZZY_Sources)/genrccmp.o\
        $(FUZZY_Sources)/genrccom.o\
        $(FUZZY_Sources)/genrcexe.o\
        $(FUZZY_Sources)/genrcfun.o\
        $(FUZZY_Sources)/genrcpsr.o\
        $(FUZZY_Sources)/globlbin.o\
        $(FUZZY_Sources)/globlbsc.o\
        $(FUZZY_Sources)/globlcmp.o\
        $(FUZZY_Sources)/globlcom.o\
        $(FUZZY_Sources)/globldef.o\
        $(FUZZY_Sources)/globlpsr.o\
        $(FUZZY_Sources)/immthpsr.o\
        $(FUZZY_Sources)/incrrset.o\
        $(FUZZY_Sources)/inherpsr.o\
        $(FUZZY_Sources)/inscom.o\
        $(FUZZY_Sources)/insfile.o\
        $(FUZZY_Sources)/insfun.o\
        $(FUZZY_Sources)/insmngr.o\
        $(FUZZY_Sources)/insmoddp.o\
        $(FUZZY_Sources)/insmult.o\
        $(FUZZY_Sources)/inspsr.o\
        $(FUZZY_Sources)/insquery.o\
        $(FUZZY_Sources)/insqypsr.o\
        $(FUZZY_Sources)/iofun.o\
        $(FUZZY_Sources)/lgcldpnd.o\
        $(FUZZY_Sources)/memory.o\
        $(FUZZY_Sources)/miscfun.o\
        $(FUZZY_Sources)/modulbin.o\
        $(FUZZY_Sources)/modulbsc.o\
        $(FUZZY_Sources)/modulcmp.o\
        $(FUZZY_Sources)/moduldef.o\
        $(FUZZY_Sources)/modulpsr.o\
        $(FUZZY_Sources)/modulutl.o\
        $(FUZZY_Sources)/msgcom.o\
        $(FUZZY_Sources)/msgfun.o\
        $(FUZZY_Sources)/msgpass.o\
        $(FUZZY_Sources)/msgpsr.o\
        $(FUZZY_Sources)/multifld.o\
        $(FUZZY_Sources)/multifun.o\
        $(FUZZY_Sources)/objbin.o\
        $(FUZZY_Sources)/objcmp.o\
        $(FUZZY_Sources)/objrtbin.o\
        $(FUZZY_Sources)/objrtbld.o\
        $(FUZZY_Sources)/objrtcmp.o\
        $(FUZZY_Sources)/objrtfnx.o\
        $(FUZZY_Sources)/objrtgen.o\
        $(FUZZY_Sources)/objrtmch.o\
        $(FUZZY_Sources)/pattern.o\
        $(FUZZY_Sources)/pprint.o\
        $(FUZZY_Sources)/prccode.o\
        $(FUZZY_Sources)/prcdrfun.o\
        $(FUZZY_Sources)/prcdrpsr.o\
        $(FUZZY_Sources)/prdctfun.o\
        $(FUZZY_Sources)/reteutil.o\
        $(FUZZY_Sources)/retract.o\
        $(FUZZY_Sources)/router.o\
        $(FUZZY_Sources)/rulebld.o\
        $(FUZZY_Sources)/rulebsc.o\
        $(FUZZY_Sources)/rulecom.o\
        $(FUZZY_Sources)/rulecstr.o\
        $(FUZZY_Sources)/ruledef.o\
        $(FUZZY_Sources)/strngfun.o\
        $(FUZZY_Sources)/strngrtr.o\
        $(FUZZY_Sources)/textpro.o\
        $(FUZZY_Sources)/tmpltbsc.o\
        $(FUZZY_Sources)/tmpltfun.o\
        $(FUZZY_Sources)/tmpltutl.o\
        $(FUZZY_Sources)/tmpltrhs.o\
        $(FUZZY_Sources)/utility.o\
        $(FUZZY_Sources)/watch.o

# Commandline version - no X interface 
# 
# Make sure WINDOW_INTERFACE is set to 0 in setup.h
#
FZ_SC: $(OBJS) $(FUZZY_Sources)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(FUZZY_Sources) $(LDFLAGS) $(LIBS)
	ar -rv libfclips.a *.o
	cp libfclips.a $(HOME)/lib







