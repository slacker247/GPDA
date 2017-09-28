Important Information
=====================

Please read the licensing information contained in license.txt

A version of the SHIQ reasoner now comes bundled with OilEd. This
should be used with OilEd when checking and verifying models. 

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

Read Manual.doc for a brief introduction to the Editor.

Compatability with earlier releases
===================================

Some DAML+OIL RDFS models produced using earlier version of OilEd may
not be read properly and may cause warning and/or error messages
during parsing. If you find this happening, one possible solution to
this is to change the rdf:ID attributes in the model to
rdf:about. This will often fix the problem.

Out of Environment Space Error
==============================

Sometimes on Windows, you may find an "out of environment space" error
message when trying to run the scripts that lauch OilEd or the
reasoner. This is caused by the fact that the default space for
environment variables is quite small. To work round this, you will
need to increase this space. This can be done either via editing
config.sys or autoexec.bat depending on your OS version or altering
the properties of the MS-DOS Prompt. If you do alter configurations,
be careful and we take no responsibility for any damage you may do. 

The Microsoft support site at http://support.microsoft.com/ has more
information on this topic. 

