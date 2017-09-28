/*@@@
File:		dtreg.hxx
Version:	1.00
Description:	Class DTREG - Document Type Registry
Author:		Nassib Nassar, nrn@cnidr.org
@@@*/

#ifndef DTREG_HXX
#define DTREG_HXX

#include "defs.hxx"
#include "../doctype/doctype.hxx"
#include "../doctype/simple.hxx"
#include "../doctype/firstline.hxx"
#include "../doctype/colondoc.hxx"
#include "../doctype/iafadoc.hxx"
#include "../doctype/mailfolder.hxx"
#include "../doctype/referbib.hxx"
#include "../doctype/irlist.hxx"
#include "../doctype/listdigest.hxx"
#include "../doctype/maildigest.hxx"
#include "../doctype/medline.hxx"
#include "../doctype/filmline.hxx"
#include "../doctype/memodoc.hxx"
#include "../doctype/sgmlnorm.hxx"
#include "../doctype/html.hxx"
#include "../doctype/oneline.hxx"
#include "../doctype/para.hxx"
#include "../doctype/filename.hxx"
#include "../doctype/ftp.hxx"
#include "../doctype/emacsinfo.hxx"
#include "../doctype/gopher.hxx"
#include "../doctype/bibtex.hxx"
#include "../doctype/usmarc.hxx"
#include "../doctype/dif.hxx"

class DTREG {
public:
	DTREG(PIDBOBJ DbParent);
	PDOCTYPE GetDocTypePtr(const STRING& DocType);
	void GetDocTypeList(PSTRLIST StringListBuffer) const;
	~DTREG();
private:
	PIDBOBJ Db;
	PDOCTYPE DtDocType;
	PSIMPLE DtSIMPLE;
	PFIRSTLINE DtFIRSTLINE;
	PCOLONDOC DtCOLONDOC;
	PIAFADOC DtIAFADOC;
	PMAILFOLDER DtMAILFOLDER;
	PREFERBIB DtREFERBIB;
	PIRLIST DtIRLIST;
	PLISTDIGEST DtLISTDIGEST;
	PMAILDIGEST DtMAILDIGEST;
	PMEDLINE DtMEDLINE;
	PFILMLINE DtFILMLINE;
	PMEMODOC DtMEMODOC;
	PSGMLNORM DtSGMLNORM;
	PHTML DtHTML;
	PONELINE DtONELINE;
	PPARA DtPARA;
	PFILENAME DtFILENAME;
	PFTP DtFTP;
	PEMACSINFO DtEMACSINFO;
	PGOPHER DtGOPHER;
	PBIBTEX DtBIBTEX;
	PUSMARC DtUSMARC;
	PDIF DtDIF;
};

typedef DTREG* PDTREG;

#endif
