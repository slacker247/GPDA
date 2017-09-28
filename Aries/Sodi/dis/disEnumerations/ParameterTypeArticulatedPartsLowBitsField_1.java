/*
 File:		ParameterTypeArticulatedPartsLowBitsField.java
 CVS Info:      $Id$
 Compiler:	jdk 1.2.2
 */

package disEnumerations;

import mil.navy.nps.dis.*;
import mil.navy.nps.util.*;

/**
 * Parameter Type Articulated Parts LowBits Field -- These Enumerations are used to describe a part of the Articulated Parameter Enumeration.  They are combined with the Parameter Type Articulated Parts HighBits enumeration to make a complete 32 bit Enumeration.  i.e.
 *@version 1.1
 *@author Ronan Fauglas
 *@author <a href="mailto:brutzman@nps.navy.mil?subject=dis-java-vrml: disEnumerations.ParameterTypeArticulatedPartsLowBitsField feedback ">Don Brutzman</a>
 *
 *<dt><b>References:</b>
 *<dd>		DIS Data Dictionary: <A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/6e.htm">Parameter Type Articulated Parts LowBits Field</A> (local)
 *				     <A href="http://SISO.sc.ist.ucf.edu/dis/dis-dd/pdu/6e.htm">Parameter Type Articulated Parts LowBits Field</A> (SISO)
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
 *  You access this via something like <b><code>ParameterTypeArticulatedPartsLowBitsField.Z</code></b>, i.e. combine
 *  the class name, a period, and a class variable (enumeration) name.<P>
 *
 *<dt><b>History:</b>
 *<dd>		21jan98   /Ronan Fauglas   	/New
 *<dd>		30mar99   /Don Brutzman   	/Revised Javadoc, many more enumeration classes
 *
 *<dt><b>Location:</b>
 *<dd><a href="../../../../../../mil/navy/nps/disEnumerations/ParameterTypeArticulatedPartsLowBitsField.java"><i>ParameterTypeArticulatedPartsLowBitsField.java</i></A> (local)
 *<dd><a href="http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/ParameterTypeArticulatedPartsLowBitsField.java">
 *          <i>http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/ParameterTypeArticulatedPartsLowBitsField.java</i></a>
 *
 */
public class ParameterTypeArticulatedPartsLowBitsField_1 extends Object
{
/**
 *(1) Position
 */
public static final short POSITION = 1;

/**
 *(10) Z rate
 */
public static final short ZRATE = 10;

/**
 *(11) Azimuth
 */
public static final short AZIMUTH = 11;

/**
 *(12) Azimuth rate
 */
public static final short AZIMUTHRATE = 12;

/**
 *(13) Elevation
 */
public static final short ELEVATION = 13;

/**
 *(14) Elevation Rate
 */
public static final short ELEVATIONRATE = 14;

/**
 *(15) Rotation
 */
public static final short ROTATION = 15;

/**
 *(16) Rotation Rate
 */
public static final short ROTATIONRATE = 16;

/**
 *(2) Position Rate
 */
public static final short POSITIONRATE = 2;

/**
 *(3) Extension
 */
public static final short EXTENSION = 3;

/**
 *(4) Extension Rate
 */
public static final short EXTENSIONRATE = 4;

/**
 *(5) X
 */
public static final short X = 5;

/**
 *(6) X rate
 */
public static final short XRATE = 6;

/**
 *(7) Y
 */
public static final short Y = 7;

/**
 *(8) Y rate
 */
public static final short YRATE = 8;

/**
 *(9) Z
 */
public static final short Z = 9;

/**
 * Returns a string containing the enumeration name which corresponds to an enumeration value,
 * as in <b><code>ParameterTypeArticulatedPartsLowBitsField.toString (1)</code></b> returns the string "<b><code>POSITION</code></b>"
 */

public static String toString(int idNumber) 
{
	switch (idNumber) {
		case 1: return "Position"; 
		case 10: return "Z rate"; 
		case 11: return "Azimuth"; 
		case 12: return "Azimuth rate"; 
		case 13: return "Elevation"; 
		case 14: return "Elevation Rate"; 
		case 15: return "Rotation"; 
		case 16: return "Rotation Rate"; 
		case 2: return "Position Rate"; 
		case 3: return "Extension"; 
		case 4: return "Extension Rate"; 
		case 5: return "X"; 
		case 6: return "X rate"; 
		case 7: return "Y"; 
		case 8: return "Y rate"; 
		case 9: return "Z"; 
		default : return "";
	}
}//end of toString
}//End of class 
