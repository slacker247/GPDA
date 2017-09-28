/*
 File:		EntityAppearanceMunitionLaunchFlashField.java
 CVS Info:      $Id$
 Compiler:	jdk 1.2 
 */

package disEnumerations;

import mil.navy.nps.dis.*;
import mil.navy.nps.util.*;

/**
 * Entity Appearance Munition Launch Flash Field -- Describes the presence of a guided missile launch flash
 *@version 1.1
 *@author Ronan Fauglas
 *@author <a href="mailto:brutzman@nps.navy.mil?subject=dis-java-vrml: disEnumerations.EntityAppearanceMunitionLaunchFlashField feedback ">Don Brutzman</a>
 *
 *<dt><b>References:</b>
 *<dd>		DIS Data Dictionary: <A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/58.htm">Entity Appearance Munition Launch Flash Field</A> (local)
 *				     <A href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/58.htm">Entity Appearance Munition Launch Flash Field</A> (SISO)
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
 *  <A href="http://SISO.sc.ist.ucf.edu/dis-dd/">http://SISO.sc.ist.ucf.edu/dis-dd/</A>
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
 *  You access this via something like <b><code>EntityAppearanceMunitionLaunchFlashField.LANUCHFLASHPRESENT;</code></b>
 *  the class name, a period, and a class variable (enumeration) name.<P>
 *
 *<dt><b>History:</b>
 *<dd>		21jan98   /Ronan Fauglas   	/New
 *<dd>		30mar99   /Don Brutzman   	/Revised Javadoc, many more enumeration classes
 *
 *<dt><b>Location:</b>
 *<dd><a href="../../../../../../mil/navy/nps/disEnumerations/EntityAppearanceMunitionLaunchFlashField.java"><i>EntityAppearanceMunitionLaunchFlashField.java</i></A> (local)
 *<dd><a href="http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/EntityAppearanceMunitionLaunchFlashField.java">
 *          <i>http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/EntityAppearanceMunitionLaunchFlashField.java</i></a>
 *
 */
public class EntityAppearanceMunitionLaunchFlashField extends Object
{
/**
 *(0) No launch flash present
 */
public final static long NOLAUNCHFLASHPRESENT = 0;

/**
 *(1) Lanuch flash present
 */
public final static long LANUCHFLASHPRESENT = 1;

public String toString(int idNumber) 
{
	switch (idNumber) {
	case 0 : return "No launch flash present"; 
	case 1 : return "Lanuch flash present"; 
	default : return "";	
}
}//end of toString
}//End of class 
