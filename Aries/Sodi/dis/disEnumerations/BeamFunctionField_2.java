/*
 File:		BeamFunctionField.java
 CVS Info:      $Id$
 Compiler:	jdk 1.2.2
 */

package disEnumerations;

import mil.navy.nps.dis.*;
import mil.navy.nps.util.*;

/**
 * Beam Function Field -- This field shall specify the function of a particular beam. This field shall be represented by an 8-bit enumberation (see Section 8 in EBV-DOC).
 *@version 1.1
 *@author Ronan Fauglas
 *@author <a href="mailto:brutzman@nps.navy.mil?subject=dis-java-vrml: disEnumerations.BeamFunctionField feedback ">Don Brutzman</a>
 *
 *<dt><b>References:</b>
 *<dd>		DIS Data Dictionary: <A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/ce.htm">Beam Function Field</A> (local)
 *				     <A href="http://SISO.sc.ist.ucf.edu/dis/dis-dd/pdu/ce.htm">Beam Function Field</A> (SISO)
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
 *  You access this via something like <b><code>BeamFunctionField.MISSILEBEACON</code></b>, i.e. combine
 *  the class name, a period, and a class variable (enumeration) name.<P>
 *
 *<dt><b>History:</b>
 *<dd>		21jan98   /Ronan Fauglas   	/New
 *<dd>		30mar99   /Don Brutzman   	/Revised Javadoc, many more enumeration classes
 *
 *<dt><b>Location:</b>
 *<dd><a href="../../../../../../mil/navy/nps/disEnumerations/BeamFunctionField.java"><i>BeamFunctionField.java</i></A> (local)
 *<dd><a href="http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/BeamFunctionField.java">
 *          <i>http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/BeamFunctionField.java</i></a>
 *
 */
public class BeamFunctionField_2 extends Object
{
/**
 *(0) Other
 */
public static final short OTHER = 0;

/**
 *(1) Search
 */
public static final short SEARCH = 1;

/**
 *(10) Missile fuze
 */
public static final short MISSILEFUZE = 10;

/**
 *(11) Active radar missile seeker
 */
public static final short ACTIVERADARMISSILESEEKER = 11;

/**
 *(12) Jammer
 */
public static final short JAMMER = 12;

/**
 *(2) Height finder
 */
public static final short HEIGHTFINDER = 2;

/**
 *(3) Acquisition
 */
public static final short ACQUISITION = 3;

/**
 *(4) Tracking
 */
public static final short TRACKING = 4;

/**
 *(5) Acquisition and tracking
 */
public static final short ACQUISITIONANDTRACKING = 5;

/**
 *(6) Command guidance
 */
public static final short COMMANDGUIDANCE = 6;

/**
 *(7) Illumination
 */
public static final short ILLUMINATION = 7;

/**
 *(8) Range only radar
 */
public static final short RANGEONLYRADAR = 8;

/**
 *(9) Missile beacon
 */
public static final short MISSILEBEACON = 9;

/**
 * Returns a string containing the enumeration name which corresponds to an enumeration value,
 * as in <b><code>BeamFunctionField.toString (0)</code></b> returns the string "<b><code>OTHER</code></b>"
 */

public static String toString(int idNumber) 
{
	switch (idNumber) {
		case 0: return "Other"; 
		case 1: return "Search"; 
		case 10: return "Missile fuze"; 
		case 11: return "Active radar missile seeker"; 
		case 12: return "Jammer"; 
		case 2: return "Height finder"; 
		case 3: return "Acquisition"; 
		case 4: return "Tracking"; 
		case 5: return "Acquisition and tracking"; 
		case 6: return "Command guidance"; 
		case 7: return "Illumination"; 
		case 8: return "Range only radar"; 
		case 9: return "Missile beacon"; 
		default : return "";
	}
}//end of toString
}//End of class 
