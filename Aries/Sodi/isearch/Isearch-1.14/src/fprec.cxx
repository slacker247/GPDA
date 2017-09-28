/************************************************************************
Copyright Notice

Copyright (c) MCNC, Clearinghouse for Networked Information Discovery and
Retrieval, 1994. 

Permission to use, copy, modify, distribute, and sell this software and
its documentation, in whole or in part, for any purpose is hereby granted
without fee, provided that

1. The above copyright notice and this permission notice appear in all
copies of the software and related documentation. Notices of copyright
and/or attribution which appear at the beginning of any file included in
this distribution must remain intact. 

2. Users of this software agree to make their best efforts (a) to return
to MCNC any improvements or extensions that they make, so that these may
be included in future releases; and (b) to inform MCNC/CNIDR of noteworthy
uses of this software. 

3. The names of MCNC and Clearinghouse for Networked Information Discovery
and Retrieval may not be used in any advertising or publicity relating to
the software without the specific, prior written permission of MCNC/CNIDR. 

THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND,
EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY WARRANTY
OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. 

IN NO EVENT SHALL MCNC/CNIDR BE LIABLE FOR ANY SPECIAL, INCIDENTAL,
INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND, OR ANY DAMAGES WHATSOEVER
RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER OR NOT ADVISED OF THE
POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF LIABILITY, ARISING OUT OF OR
IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. 
************************************************************************/

/*@@@
File:		fprec.cxx
Version:	1.00
Description:	Class FPREC - File Pointer Record
Author:		Nassib Nassar, nrn@cnidr.org
@@@*/

#include "fprec.hxx"
#include "common.hxx"

FPREC::FPREC() {
	FilePointer = 0;
	Priority = 0;
	Closed = GDT_FALSE;
}

FPREC& FPREC::operator=(const FPREC& OtherFprec) {
	FileName = OtherFprec.FileName;
	FilePointer = OtherFprec.FilePointer;
	OpenMode = OtherFprec.OpenMode;
	return *this;
}

void FPREC::SetFileName(const STRING& NewFileName) {
	FileName = NewFileName;
	ExpandFileSpec(&FileName);
}

void FPREC::GetFileName(PSTRING StringBuffer) const {
	*StringBuffer = FileName;
}

void FPREC::SetFilePointer(const PFILE NewFilePointer) {
	FilePointer = NewFilePointer;
}

PFILE FPREC::GetFilePointer() const {
	return FilePointer;
}

void FPREC::SetPriority(const INT NewPriority) {
	Priority = NewPriority;
}

INT FPREC::GetPriority() const {
	return Priority;
}

void FPREC::SetClosed(const GDT_BOOLEAN NewClosed) {
	Closed = NewClosed;
}

GDT_BOOLEAN FPREC::GetClosed() const {
	return Closed;
}

void FPREC::SetOpenMode(const STRING& NewOpenMode) {
	OpenMode = NewOpenMode;
}

void FPREC::GetOpenMode(PSTRING StringBuffer) const {
	*StringBuffer = OpenMode;
}

FPREC::~FPREC() {
}
