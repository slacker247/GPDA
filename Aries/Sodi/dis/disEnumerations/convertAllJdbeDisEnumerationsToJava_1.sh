#/cygnus/cygwin-b20/H-i586-cygwin32/bin/sh
#/usr/bin/csh
#
#  NAME
#    convertAllJdbeDisEnumerationsToJava.sh
#
#  SYNOPSIS
#    convertAllJdbeDisEnumerationsToJava.sh 
#    (pretty name uh ?)
#  DESCRIPTION
#    	This csh script will generate the java files for the enumerations. 
#    	It takes no argument as it finds the right input files on its own...
# 	It also generates the report.txt which is builds a report of the operation for each file.
#
#  HOW TO USE
#    convertAllJdbeDisEnumerationsToJava.sh (in the disEnumerations directory, 
#    all the html sources being in JdbeHtmlFiles subdirectories entity and pdu.
#    make sure that only the right html files are in the directory...
#
#  AUTHOR
#    Ronan Fauglas
#
#  NOTES
#    Send your suggestions to Ronan Fauglas fauglas@stl.nps.navy.mil
#    there are some trouble with the report.txt files (actually there seem to be a bug in the flush of output in perl...)
#
#  REFERENCES
#  man csh...
#  
#  HISTORY
#  26janv98/	Ronan Fauglas	/new
#  30mar99/	Don Brutzman	/more enumerations, available in entity & pdu subdirectories
#
#

echo  "Report file for the enumeration class generation" >!  report.txt
echo  "------------------------------------------------" >>! report.txt



# set list=`ls JdbeHtmlFiles/*.htm`
set list=`ls JdbeHtmlFiles/entity/*.htm JdbeHtmlFiles/pdu/*.htm`

foreach file ($list)
	 convertJdbeDisEnumerationsToJava.pl $file >>! report.txt
end

echo Note : the execution report is in report.txt !

