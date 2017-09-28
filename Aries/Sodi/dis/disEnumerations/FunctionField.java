/*
 File:		FunctionField.java
 CVS Info:      $Id$
 Compiler:	jdk 1.2.2
 */

package disEnumerations;

import mil.navy.nps.dis.*;
import mil.navy.nps.util.*;

/**
 * Function Field -- This field shall specify the function for a particular emitter. Typical functions include airborne fire control, ground surveillance radar, etc. This field is intended to help receiving entities determine if the Electromagnetic Emission PDU is of interest to the systems simulated by that entity. This field shall be represented by an 8-bit enumeration (see Section 8 in EBV-DOC).
 *@version 1.1
 *@author Ronan Fauglas
 *@author <a href="mailto:brutzman@nps.navy.mil?subject=dis-java-vrml: disEnumerations.FunctionField feedback ">Don Brutzman</a>
 *
 *<dt><b>References:</b>
 *<dd>		DIS Data Dictionary: <A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/be.htm">Function Field</A> (local)
 *				     <A href="http://SISO.sc.ist.ucf.edu/dis/dis-dd/pdu/be.htm">Function Field</A> (SISO)
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
 *  You access this via something like <b><code>FunctionField.WEAPONLETHAL</code></b>, i.e. combine
 *  the class name, a period, and a class variable (enumeration) name.<P>
 *
 *<dt><b>History:</b>
 *<dd>		21jan98   /Ronan Fauglas   	/New
 *<dd>		30mar99   /Don Brutzman   	/Revised Javadoc, many more enumeration classes
 *
 *<dt><b>Location:</b>
 *<dd><a href="../../../../../../mil/navy/nps/disEnumerations/FunctionField.java"><i>FunctionField.java</i></A> (local)
 *<dd><a href="http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/FunctionField.java">
 *          <i>http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/FunctionField.java</i></a>
 *
 */
public class FunctionField extends Object
{
/**
 *(0) Other
 */
public static final short OTHER = 0;

/**
 *(1) Multi-function
 */
public static final short MULTIFUNCTION = 1;

/**
 *(10) Radar Altimeter
 */
public static final short RADARALTIMETER = 10;

/**
 *(11) Imaging
 */
public static final short IMAGING = 11;

/**
 *(12) Motion Detection
 */
public static final short MOTIONDETECTION = 12;

/**
 *(13) Navigation
 */
public static final short NAVIGATION = 13;

/**
 *(14) Weather
 */
public static final short WEATHER = 14;

/**
 *(15) Instrumentation
 */
public static final short INSTRUMENTATION = 15;

/**
 *(16) Identification/Classification
 */
public static final short IDENTIFICATIONCLASSIFICATION = 16;

/**
 *(2) Early Warning/Surveillance
 */
public static final short EARLYWARNINGSURVEILLANCE = 2;

/**
 *(3) Height Finding
 */
public static final short HEIGHTFINDING = 3;

/**
 *(4) Fire Control
 */
public static final short FIRECONTROL = 4;

/**
 *(5) Acquisition/Detection
 */
public static final short ACQUISITIONDETECTION = 5;

/**
 *(6) Tracking
 */
public static final short TRACKING = 6;

/**
 *(64) Jamming, noise
 */
public static final short JAMMINGNOISE = 64;

/**
 *(65) Jamming, deception
 */
public static final short JAMMINGDECEPTION = 65;

/**
 *(66) Decoy
 */
public static final short DECOY = 66;

/**
 *(7) Guidance/Illumination
 */
public static final short GUIDANCEILLUMINATION = 7;

/**
 *(8) Firing point/launch point location
 */
public static final short FIRINGPOINTLAUNCHPOINTLOCATION = 8;

/**
 *(9) Ranging
 */
public static final short RANGING = 9;

/**
 *(96) Weapon, non-lethal
 */
public static final short WEAPONNONLETHAL = 96;

/**
 *(97) Weapon, lethal
 */
public static final short WEAPONLETHAL = 97;

/**
 * Returns a string containing the enumeration name which corresponds to an enumeration value,
 * as in <b><code>FunctionField.toString (0)</code></b> returns the string "<b><code>OTHER</code></b>"
 */

public static String toString(int idNumber) 
{
	switch (idNumber) {
		case 0: return "Other"; 
		case 1: return "Multi-function"; 
		case 10: return "Radar Altimeter"; 
		case 11: return "Imaging"; 
		case 12: return "Motion Detection"; 
		case 13: return "Navigation"; 
		case 14: return "Weather"; 
		case 15: return "Instrumentation"; 
		case 16: return "Identification/Classification"; 
		case 2: return "Early Warning/Surveillance"; 
		case 3: return "Height Finding"; 
		case 4: return "Fire Control"; 
		case 5: return "Acquisition/Detection"; 
		case 6: return "Tracking"; 
		case 64: return "Jamming, noise"; 
		case 65: return "Jamming, deception"; 
		case 66: return "Decoy"; 
		case 7: return "Guidance/Illumination"; 
		case 8: return "Firing point/launch point location"; 
		case 9: return "Ranging"; 
		case 96: return "Weapon, non-lethal"; 
		case 97: return "Weapon, lethal"; 
		default : return "";
	}
}//end of toString
}//End of class 
