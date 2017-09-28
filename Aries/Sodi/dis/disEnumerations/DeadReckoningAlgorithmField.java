/*
 File:		DeadReckoningAlgorithmField.java
 CVS Info:      $Id$
 Compiler:	jdk 1.2.2
 */

package disEnumerations;

import mil.navy.nps.dis.*;
import mil.navy.nps.util.*;

/**
 * Dead Reckoning Algorithm Field -- Indicates the type of dead reckoning algorithm used by an entity.
 *@version 1.1
 *@author Ronan Fauglas
 *@author <a href="mailto:brutzman@nps.navy.mil?subject=dis-java-vrml: disEnumerations.DeadReckoningAlgorithmField feedback ">Don Brutzman</a>
 *
 *<dt><b>References:</b>
 *<dd>		DIS Data Dictionary: <A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/38.htm">Dead Reckoning Algorithm Field</A> (local)
 *				     <A href="http://SISO.sc.ist.ucf.edu/dis/dis-dd/pdu/38.htm">Dead Reckoning Algorithm Field</A> (SISO)
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
 *  You access this via something like <b><code>DeadReckoningAlgorithmField.DRM8</code></b>, i.e. combine
 *  the class name, a period, and a class variable (enumeration) name.<P>
 *
 *<dt><b>History:</b>
 *<dd>		21jan98   /Ronan Fauglas   	/New
 *<dd>		30mar99   /Don Brutzman   	/Revised Javadoc, many more enumeration classes
 *
 *<dt><b>Location:</b>
 *<dd><a href="../../../../../../mil/navy/nps/disEnumerations/DeadReckoningAlgorithmField.java"><i>DeadReckoningAlgorithmField.java</i></A> (local)
 *<dd><a href="http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/DeadReckoningAlgorithmField.java">
 *          <i>http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/DeadReckoningAlgorithmField.java</i></a>
 *
 */
public class DeadReckoningAlgorithmField extends Object
{
/**
 *(0) Other
 */
public static final short OTHER = 0;

/**
 *(1) Static (Entity does not move)
 */
public static final short STATIC = 1;

/**
 *(2) DRM(F, P, W)
 */
public static final short DRM = 2;

/**
 *(3) DRM(R, P, W)
 */
public static final short DRM2 = 3;

/**
 *(4) DRM(R, V, W)
 */
public static final short DRM3 = 4;

/**
 *(5) DRM(F, V, W)
 */
public static final short DRM4 = 5;

/**
 *(6) DRM(F, P, B)
 */
public static final short DRM5 = 6;

/**
 *(7) DRM(R, P, B)
 */
public static final short DRM6 = 7;

/**
 *(8) DRM(R, V, B)
 */
public static final short DRM7 = 8;

/**
 *(9) DRM(F, V, B)
 */
public static final short DRM8 = 9;

/**
 * Returns a string containing the enumeration name which corresponds to an enumeration value,
 * as in <b><code>DeadReckoningAlgorithmField.toString (0)</code></b> returns the string "<b><code>OTHER</code></b>"
 */

public static String toString(int idNumber) 
{
	switch (idNumber) {
		case 0: return "Other"; 
		case 1: return "Static (Entity does not move)"; 
		case 2: return "DRM(F, P, W)"; 
		case 3: return "DRM(R, P, W)"; 
		case 4: return "DRM(R, V, W)"; 
		case 5: return "DRM(F, V, W)"; 
		case 6: return "DRM(F, P, B)"; 
		case 7: return "DRM(R, P, B)"; 
		case 8: return "DRM(R, V, B)"; 
		case 9: return "DRM(F, V, B)"; 
		default : return "";
	}
}//end of toString
}//End of class 
