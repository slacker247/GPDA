#!/usr/local/bin/perl
#
#  NAME
#    convertJdbeDisEnumerationsToJava.pl
#
#  SYNOPSIS
#    convertJdbeDisEnumerationsToJava.pl fileName.html
#    
#  DESCRIPTION
#    This perl script will generate java files from html files given in argument.
#
#  HOW TO USE
#  convertJdbeDisEnumerationsToJava.pl fileName.htm will generate a
#  java class EnumField.java 
#  (the name is EnumField parsed from the html source )
#  Note that fileName.html has to be the name of the html file
#  as on the Dis data dictionnary site:
#  http://SISO.sc.ist.ucf.edu/dis/ so that the URL
#  http://SISO.sc.ist.ucf.edu/dis/dis-dd/fileName.htm
#  can be inserted automatically
#
#  AUTHORS
#    Ronan Fauglas
#    Don Brutzman (updates)
#      
#  NOTES
#    Please note that this script has not been optimised for speed concision but for flexibility and reusability.
#
#    Longish disquisition on types used for enumerations: Originally, we had the actual enumerations
#    be UnsignedByte, UnsignedShort, or UnsignedInt. This turned out to be a problem when constructing
#    switch statements. The compiler wants the case label statement to be: 1) a Java primitive cardinal
#    type, such as int, short, or byte; and 2) a constant value expression. Using objects, such
#    as UnsignedByte, fail on both counts. The first is obvious; the second, less so. If you do
#    an value.getInt() on the object, it returns a primitive type, but this can change at runtime.
#    So the compiler is unable to compile things down in the switch statement. The constant expression
#    must be entirely determined at runtime. As a result, we use shorts and ints, the primitive types,
#    for UnsignedBytes and UnsignedShorts. We automatically promote UnsignedBytes to shorts, so the
#    full range can be covered. 
#   
#    This isn't all that bad; mostly the enumerations are used to make code more readable, and aren't
#    really type-safety measures.
#
#  REFERENCES
#  See http://web.nps.navy.mil/~brutzman/perl for more information on Perl.
#  
#  HISTORY
#  21janv98/	Ronan Fauglas	/new
#  30mar99/	Don Brutzman	/more data files, more error corrections, HTML output
#				/ (which still requires manual re-alphabetization)
#

$filename = shift(ARGV);
if ($filename) 
{
    	$fullfilename = $filename;
	$|=0;
}
else {
    print STDERR "!! usage: convertJdbeDisEnumerationsToJava.pl fileName.html\n";
    exit;
}

#
# Open the html file
#

if (!open(handleIn , "<$filename")) {
	print STDERR "!! impossible d'ouvrir le fichier (this won't be opened that easily !)";
	exit;
}

$subdirfilename = $filename;
$subdirfilename =~ s/JdbeHtmlFiles\///;

$filename =~ s/JdbeHtmlFiles\/entity\///;
$filename =~ s/JdbeHtmlFiles\/pdu\///;

# print "\n";
# print "<!-- File:  $fullfilename -->\n";  # $filename

#
# Parse the file and extract the relevant enumeration fields
# The result are stored in $toString $title, $summary and $enum
#

$emptyenumcounter=0;   	#This variable counts keeps a counter on the empty enumerations...
$|=0;			#Forces a flush on the output.

LINE: while ($_=<handleIn>) 
{
	chop;
	chop;

	if ( /<B>Size: <\/B>([0-9]*)<BR>/)
	{
    # Each type is promoted to its next-higher type to prevent overflow.
		$type = "long"    if ($1 <= 64);
		$type = "int"     if ($1 <= 32);
		$type = "short"   if ($1 <= 16);
	#	$type = "byte"    if ($1 <=  8);  # byte is too short for some enumeration values
	}

	if (( /<b>Current standard:  IEEE 1278.1<\/b>/) and
	 (<handleIn> =~ /<p><BR><H4>(.*)<\/H4>/) )
 	{
		$title=$1;
		$classname=$1;
		$classname =~ s/\s+//g; # make the class name from the extracted text by simply removing spaces...
		$classname =~ s/\///g;  # removing slashes...
		$classname =~ s/-//g;   # removing hyphens...
		$classname =~ s/(.*)(\(.*\))/$1/; #removing parentheses
# 		print "<!-- Enumeration name:  $title, class name: $classname -->\n";
		$|=0;
#		if ($type) { print "<!-- Type of data:  $type -->\n"; }
	}



	if (/<B>Description: <\/B>(.*)<BR>/) 
	{
#		print "<!-- Description:  $1 -->\n";
		$summary=$1;
	}

	if ( /^<H3>Enumerations:<\/H3>/ 
	and (<handleIn> =~ /^<TABLE BORDER> <TR><TH ALIGN=CENTER>Value<\/TH>/ ) 
	and (<handleIn> =~ /^<TH ALIGN=CENTER>Description<\/TH>/)
	and (<handleIn> =~ /^<\/TR>/) ) # These regular expressions match the beginning of the table...
	{
		$enumerationsFound=1;
		while ($_=<handleIn>) 
		{	
			last if /<\/TABLE>/  ;
			if ( /^<TR><TD>([0-9]+)<\/TD>/ ) # match if the next line contains the beginning of an enumeration...
			{
				$value= $1;
				if ( <handleIn> =~ /<TD>(.*)<\/TD>/ ) #(\w)*,*(\(*)* this match an country name 
										#(more specifically an area name) and the mother country that might be specified between brackets.
				{
					$temp=$1; # temporary variable used to store the original name of the country 
						  # in case we might proceed changes
					$name=$1; # this variable will store the enumeration variable name

					$name =~ s/(.*)(\(.*\))/$1/; #removing parentheses

					$name =~ s/([a-z])/\U$1/g ; #converting characters from lower case to upper case

					$name =~ s/[\s\"\/.]//g; #change \s, ", \/, . in _
			
					#$name =~ s/_+//g;	#suppressing multiple _, NOTE THAT _ CAN'T BE USED AS  A SEPARATOR SINCE JAVADOC IS BUGGED 
								# AND WON'T GENERATE AN INDEX...

					$name =~ s/^([0-9]+)/ENUMERATION$1/; #insert _ if the first character(s) is number

					$name =~ s/(\W|$)//g; #removing junk characters if any left

					if ( $varname{$name} ) 	# We test here if we have already met a variable of that name.
								# Varname is an associative whose key are the variable names
								# The associated value is the number of occurences.
					{
						$varname{$name}++;
						$name.="$varname{$name}";
						print "<!-- duplication of variable name:: $varname{$name}, $name -->\n";
					}
					else # never seen anything of that name...
					{		
						if ($name eq "") # we test here if the name we have generated is an empty string
						{ 
							$emptyenumcounter ++;
							$name= "ENUMERATION$emptyenumcounter\n";
							print "<!-- Empty name found:  $name -->\n";
						}
						else #we insert the variable in the associative array
						{
							$varname{$name}=1;
						}
					}
					
					#
					#creating the corresponding variable definition line in the class
					#

					$temp=~ s/\.//g;
					$temp=~ s/"/\\"/g;
					
					 
					$enum .= "/**\n *($value) $temp\n */\n"; #inserting javadoc
					$enum .= "public static final $type $name = $value;\n\n";

					if (! $toString) # First enumeration found, begin $toString
					{
#
#Here comes the toString construction.
#

$toString= <<FOO;
/**
 * Returns a string containing the enumeration name which corresponds to an enumeration value,
 * as in <b><code>$classname.toString ($value)</code></b> returns the string "<b><code>$name</code></b>"
 */

public static String toString(int idNumber) 
{
	switch (idNumber) {
FOO
					}

					#
					#append the corresponding case of in toString()
					#
					$toString .="\t\tcase $value: return \"$temp\"; \n"; #creates an entry in the 'case of'
				}
			}		
		}

	}
}

close handleIn;

#
# Here comes the header....
#

$header= <<FOO;
/*
 File:		$classname.java
 CVS Info:      \$Id\$
 Compiler:	jdk 1.2.2
 */

package mil.navy.nps.disEnumerations;

import mil.navy.nps.dis.*;
import mil.navy.nps.util.*;

/**
 * $title -- $summary
 *\@version 1.1
 *\@author Ronan Fauglas
 *\@author <a href="mailto:brutzman\@nps.navy.mil?subject=dis-java-vrml: disEnumerations.$classname feedback ">Don Brutzman</a>
 *
 *<dt><b>References:</b>
 *<dd>		DIS Data Dictionary: <A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/$subdirfilename">$title</A> (local)
 *				     <A href="http://SISO.sc.ist.ucf.edu/dis/dis-dd/pdu/$filename">$title</A> (SISO)
 *<dd>          JDBE:<a href="http://208.145.129.4/jdbe/proj/dis_cd/dis-dd/">DIS Data Dictionary Version 1.0a (DIS-DD)</A>
 *<dd>		Perl script (converting html enumerations to java enumerations)
 *		<A href="../../../../../../mil/navy/nps/disEnumerations/convertJdbeDisEnumerationsToJava.pl"><i>convertJdbeDisEnumerationsToJava.pl</i></A> (local) or
 *		<A href="http://web.3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/convertJdbeDisEnumerationsToJava.pl">
 *	 	      <i>http://web.3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/convertJdbeDisEnumerationsToJava.pl</i></A>
 *<dd>		"Named Constants," <i>The Java Programming Language</i>, Gosling & Arnold.
 *
 *<dt><b>Explanation:</b>
 *<dd>This file has been automatically generated from a local copy of the
 *  <A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/dis-dd.html">DIS Data Dictionary</A> at
 *  <A href="http://SISO.sc.ist.ucf.edu/dis/dis-dd/">http://SISO.sc.ist.ucf.edu/dis/dis-dd/</A>
 *  html source file by
 *  <A href="../../../../../../mil/navy/nps/disEnumerations/convertJdbeDisEnumerationsToJava.pl">convertJdbeDisEnumerationsToJava.pl</a> (local) or
 *  <A href="http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/convertJdbeDisEnumerationsToJava.pl">http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/convertJdbeDisEnumerationsToJava.pl</a>.
 *  <P>
 *  This is effectively a C-style enumeration. Java doesn't do enumerations
 *  like C, so you have to wrap a class around it. It's a bit more typing,
 *  but pretty simple-minded. 
 *  Note that the variables are declared public. The default for access 
 *  is package-wide, but these variables might need to be accessed from
 *  outside the package. Since all the variables are final (i.e. constant), nobody can
 *  change anything anyway, so this is no biggie.<p>
 *  To use these enumerations in your Java code, import the package first:
 *         <b><PRE>import mil.navy.nps.disEnumerations.*;</PRE></b>
 *  You access this via something like <b><code>$classname.$name</code></b>, i.e. combine
 *  the class name, a period, and a class variable (enumeration) name.<P>
 *
 *<dt><b>History:</b>
 *<dd>		21jan98   /Ronan Fauglas   	/New
 *<dd>		30mar99   /Don Brutzman   	/Revised Javadoc, many more enumeration classes
 *
 *<dt><b>Location:</b>
 *<dd><a href="../../../../../../mil/navy/nps/disEnumerations/$classname.java"><i>$classname.java</i></A> (local)
 *<dd><a href="http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/$classname.java">
 *          <i>http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/$classname.java</i></a>
 *
 */
public class $classname extends Object
{
FOO

#
# Rebuild the whole file from $header, $enum and $toString
#

$toString .= "\t\tdefault : return \"\";\n\t}\n}//end of toString\n";

$file = $header.$enum.$toString;

$file.="}//End of class \n";

#
# Generate the resulting output file
#

if ($classname && $enumerationsFound) # don't save file if either class or enumerations are not found
{
	if (! open(handleOut,">$classname.java") ) 
	{
		print STDERR "!! can't create $classname.java\n";
		exit;
	}

	print "<TR>\n";
	print "\t<TD ALIGN=\"CENTER\">\t";
	print "<A HREF=\"../mil/navy/nps/disEnumerations/$fullfilename\">$title</A>\n"; # $subdirfilename
	print "\t<TD ALIGN=\"CENTER\">\t";
	print "<A HREF=\"../javadoc/dis-java-vrml/mil/navy/nps/disEnumerations/$classname.html\">$classname.html</A>\n";
	print "\t<TD ALIGN=\"CENTER\">\t";
	print "<A HREF=\"../mil/navy/nps/disEnumerations/$classname.java\">$classname.java</A>\n";

	print handleOut "$file" ;

	close $handleOut;
}

exit;

