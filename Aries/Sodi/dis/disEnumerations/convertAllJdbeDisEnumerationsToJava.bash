#
#  NAME
#    convertAllJdbeDisEnumerationsToJava.bash
#
#  SYNOPSIS
#    C:\vrtp\mil\navy\nps\disEnumerations> bash convertAllJdbeDisEnumerationsToJava.bash
#    (pretty name uh ?)
#
#  DESCRIPTION
#    	This csh script will generate the java files for the enumerations. 
#    	It takes no argument as it finds the right input files on its own...
# 	It also generates the DisEnumerations.html which is builds a report of the operation for each file.
#
#  HOW TO USE
#    convertAllJdbeDisEnumerationsToJava.bash (in the disEnumerations directory, all the html sources being in JdbeHtmlFiles.
#    make sure that only the right html files are in the directory...
#
#  AUTHORS
#    Ronan Fauglas
#    Don McGregor
#    Don Brutzman
#
#  REFERENCES
#  man csh...
#  _Learning the bash Shell_, Newham & Rosenblatt, O'Reilly & Associates, 1995.
#  
#  HISTORY
#  26janv98/	Ronan Fauglas	/new
#  28Jan98/	Don McGregor	/Bash-ified and csh-isms removed.
#  30mar99/	Don Brutzman	/more enumerations (recently obtained & placed in entity & pdu subdirectories),
#				/ and outputs HTML in file DisEnumerationsAutogenerated.html
#
#

#list=$(ls JdbeHtmlFiles/*.htm)

list=$(ls JdbeHtmlFiles/entity/*.htm JdbeHtmlFiles/pdu/*.htm)

echo  "<HTML>"	>  DisEnumerationsAutogenerated.html
echo  "<HEAD>"	>> DisEnumerationsAutogenerated.html
echo  "<TITLE> DIS-Java-VRML Working Group:  Autogenerated DIS Enumerations</TITLE>"	>> DisEnumerationsAutogenerated.html
echo  "</HEAD>"	>> DisEnumerationsAutogenerated.html
echo  "<BODY bgcolor=\"#ffffff\" alink=\"#FF0000\" link=\"#FF0000\" vlink=\"#880000\" text=\"#000000\">"	>> DisEnumerationsAutogenerated.html
echo  ""	>> DisEnumerationsAutogenerated.html
echo  "<TABLE>"	>> DisEnumerationsAutogenerated.html
echo  "<TR>"	>> DisEnumerationsAutogenerated.html
echo  "<TD>"	>> DisEnumerationsAutogenerated.html
echo  "<H2>Distributed Interactive Simulation<BR>DIS-Java-VRML Working Group</H2><BR>"	>> DisEnumerationsAutogenerated.html
echo  "<TD ALIGN=\"CENTER\">"	>> DisEnumerationsAutogenerated.html
echo  "<A HREF=\"dis-java-vrml.html\">"	>> DisEnumerationsAutogenerated.html
echo  "<IMG SRC=\"images/Duty_Now_Logo.gif\" BORDER=\"0\" width=184 height=192 ALT=\"click for Dis-Java-VRML home page\"></A>"	>> DisEnumerationsAutogenerated.html
echo  "</TABLE>"	>> DisEnumerationsAutogenerated.html
echo  ""	>> DisEnumerationsAutogenerated.html
echo  "<H2>Autogenerated DIS Enumerations</H2>"	>> DisEnumerationsAutogenerated.html
echo  ""	>> DisEnumerationsAutogenerated.html
echo  "<CENTER>"	>> DisEnumerationsAutogenerated.html
echo  "<TABLE BORDER=2 ALIGN=\"CENTER\">"	>> DisEnumerationsAutogenerated.html
echo  "<tr ALIGN=\"CENTER\" valign=middle>"	>> DisEnumerationsAutogenerated.html
echo  "<td ALIGN=\"CENTER\" valign=middle> <IMG SRC = \"images/view.jpg\"		WIDTH=74  HEIGHT=43 	ALT=\"[view text, HTML, VRML etc.]\">"	>> DisEnumerationsAutogenerated.html
echo  "<td ALIGN=\"CENTER\" valign=middle> <IMG SRC = \"images/documentation.jpg\"	WIDTH=226 HEIGHT=42 	ALT=\"[Javadoc documentation]\">"	>> DisEnumerationsAutogenerated.html
echo  "<td ALIGN=\"CENTER\" valign=middle> <IMG SRC = \"images/Javasource.jpg\"		WIDTH=182 HEIGHT=42 	ALT=\"[Java source code]\">"		>> DisEnumerationsAutogenerated.html
echo  "<TR>"	>> DisEnumerationsAutogenerated.html
echo  "<TD ALIGN="CENTER"> <FONT size=\"+2\">DIS Data Dictionary Entries </FONT>"	>> DisEnumerationsAutogenerated.html
echo  "<TD ALIGN="CENTER"><A HREF=\"../javadoc/dis-java-vrml/mil/navy/nps/DisEnumerationsAutogenerated/package-summary.html\"><B>Package Summary</B></A>"	>> DisEnumerationsAutogenerated.html
echo  "<P>"	>> DisEnumerationsAutogenerated.html
echo  "<A HREF=\"../javadoc/dis-java-vrml/index-all.html\"><B>Index of all Fields &amp; Methods</B></A>"	>> DisEnumerationsAutogenerated.html
echo  "<TD ALIGN=\"CENTER\"> &nbsp;"	>> DisEnumerationsAutogenerated.html


for file in $list
 do
  perl ./convertJdbeDisEnumerationsToJava.pl $file	>> DisEnumerationsAutogenerated.html
 done

echo  ""	>> DisEnumerationsAutogenerated.html
echo  "</TABLE>">> DisEnumerationsAutogenerated.html
echo  "</CENTER>"	>> DisEnumerationsAutogenerated.html
echo  ""	>> DisEnumerationsAutogenerated.html
echo  "</BODY>"	>> DisEnumerationsAutogenerated.html
echo  "</HTML>"	>> DisEnumerationsAutogenerated.html
echo  ""	>> DisEnumerationsAutogenerated.html

mv    DisEnumerationsAutogenerated.html ../../../../dis-java-vrml/
ls -l ../../../../dis-java-vrml/DisEnumerationsAutogenerated.html

echo "Complete.  The conversion report is vrtp/dis-java-vrml/DisEnumerationsAutogenerated.html"


