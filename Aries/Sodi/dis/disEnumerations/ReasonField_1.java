/*
 File:		ReasonField.java
 CVS Info:      $Id$
 Compiler:	jdk 1.2.2
 */

package disEnumerations;

import mil.navy.nps.dis.*;
import mil.navy.nps.util.*;

/**
 * Reason Field -- This field shall specify the reason that an entity or exercies was stopped/frozen. This field shall be represented by an 8-bit enumberation (see Section 7 in EBV-DOC)
 *@version 1.1
 *@author Ronan Fauglas
 *@author <a href="mailto:brutzman@nps.navy.mil?subject=dis-java-vrml: disEnumerations.ReasonField feedback ">Don Brutzman</a>
 *
 *<dt><b>References:</b>
 *<dd>		DIS Data Dictionary: <A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/a0.htm">Reason Field</A> (local)
 *				     <A href="http://SISO.sc.ist.ucf.edu/dis/dis-dd/pdu/a0.htm">Reason Field</A> (SISO)
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
 *  You access this via something like <b><code>ReasonField.ABORTTRAININGRETURNTOTACTICALOPERATIONS</code></b>, i.e. combine
 *  the class name, a period, and a class variable (enumeration) name.<P>
 *
 *<dt><b>History:</b>
 *<dd>		21jan98   /Ronan Fauglas   	/New
 *<dd>		30mar99   /Don Brutzman   	/Revised Javadoc, many more enumeration classes
 *
 *<dt><b>Location:</b>
 *<dd><a href="../../../../../../mil/navy/nps/disEnumerations/ReasonField.java"><i>ReasonField.java</i></A> (local)
 *<dd><a href="http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/ReasonField.java">
 *          <i>http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/ReasonField.java</i></a>
 *
 */
public class ReasonField_1 extends Object
{
/**
 *(0) Other
 */
public static final short OTHER = 0;

/**
 *(1) Recess
 */
public static final short RECESS = 1;

/**
 *(2) Termination
 */
public static final short TERMINATION = 2;

/**
 *(3) System Failure
 */
public static final short SYSTEMFAILURE = 3;

/**
 *(4) Security Violation
 */
public static final short SECURITYVIOLATION = 4;

/**
 *(5) Entity Reconstitution
 */
public static final short ENTITYRECONSTITUTION = 5;

/**
 *(6) Stop for reset
 */
public static final short STOPFORRESET = 6;

/**
 *(7) Stop for restart
 */
public static final short STOPFORRESTART = 7;

/**
 *(8) Abort Training Return to Tactical Operations
 */
public static final short ABORTTRAININGRETURNTOTACTICALOPERATIONS = 8;

/**
 * Returns a string containing the enumeration name which corresponds to an enumeration value,
 * as in <b><code>ReasonField.toString (0)</code></b> returns the string "<b><code>OTHER</code></b>"
 */

public static String toString(int idNumber) 
{
	switch (idNumber) {
		case 0: return "Other"; 
		case 1: return "Recess"; 
		case 2: return "Termination"; 
		case 3: return "System Failure"; 
		case 4: return "Security Violation"; 
		case 5: return "Entity Reconstitution"; 
		case 6: return "Stop for reset"; 
		case 7: return "Stop for restart"; 
		case 8: return "Abort Training Return to Tactical Operations"; 
		default : return "";
	}
}//end of toString
}//End of class 
