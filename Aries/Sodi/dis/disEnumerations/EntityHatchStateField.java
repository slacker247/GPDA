/*
 File:		EntityHatchStateField.java
 CVS Info:      $Id$
 Compiler:	jdk 1.2.2
 */

package disEnumerations;

import mil.navy.nps.dis.*;
import mil.navy.nps.util.*;

/**
 * Entity Hatch State Field -- Describes the state of the hatch
 *@version 1.1
 *@author Ronan Fauglas
 *@author <a href="mailto:brutzman@nps.navy.mil?subject=dis-java-vrml: disEnumerations.EntityHatchStateField feedback ">Don Brutzman</a>
 *
 *<dt><b>References:</b>
 *<dd>		DIS Data Dictionary: <A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/43.htm">Entity Hatch State Field</A> (local)
 *				     <A href="http://SISO.sc.ist.ucf.edu/dis/dis-dd/pdu/43.htm">Entity Hatch State Field</A> (SISO)
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
 *  You access this via something like <b><code>EntityHatchStateField.UNUSED2</code></b>, i.e. combine
 *  the class name, a period, and a class variable (enumeration) name.<P>
 *
 *<dt><b>History:</b>
 *<dd>		21jan98   /Ronan Fauglas   	/New
 *<dd>		30mar99   /Don Brutzman   	/Revised Javadoc, many more enumeration classes
 *
 *<dt><b>Location:</b>
 *<dd><a href="../../../../../../mil/navy/nps/disEnumerations/EntityHatchStateField.java"><i>EntityHatchStateField.java</i></A> (local)
 *<dd><a href="http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/EntityHatchStateField.java">
 *          <i>http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/EntityHatchStateField.java</i></a>
 *
 */
public class EntityHatchStateField extends Object
{
/**
 *(0) Not applicable
 */
public static final short NOTAPPLICABLE = 0;

/**
 *(1) Primary hatch is closed
 */
public static final short PRIMARYHATCHISCLOSED = 1;

/**
 *(2) Primary hatch is popped
 */
public static final short PRIMARYHATCHISPOPPED = 2;

/**
 *(3) Primary hatch is popped and a person is visible under hatch
 */
public static final short PRIMARYHATCHISPOPPEDANDAPERSONISVISIBLEUNDERHATCH = 3;

/**
 *(4) Primary hatch is open
 */
public static final short PRIMARYHATCHISOPEN = 4;

/**
 *(5) Primary hatch is open and person is visible
 */
public static final short PRIMARYHATCHISOPENANDPERSONISVISIBLE = 5;

/**
 *(6) Unused
 */
public static final short UNUSED = 6;

/**
 *(7) Unused
 */
public static final short UNUSED2 = 7;

/**
 * Returns a string containing the enumeration name which corresponds to an enumeration value,
 * as in <b><code>EntityHatchStateField.toString (0)</code></b> returns the string "<b><code>NOTAPPLICABLE</code></b>"
 */

public static String toString(int idNumber) 
{
	switch (idNumber) {
		case 0: return "Not applicable"; 
		case 1: return "Primary hatch is closed"; 
		case 2: return "Primary hatch is popped"; 
		case 3: return "Primary hatch is popped and a person is visible under hatch"; 
		case 4: return "Primary hatch is open"; 
		case 5: return "Primary hatch is open and person is visible"; 
		case 6: return "Unused"; 
		case 7: return "Unused"; 
		default : return "";
	}
}//end of toString
}//End of class 
