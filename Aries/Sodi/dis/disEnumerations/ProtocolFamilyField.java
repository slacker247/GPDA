/*
 File:		ProtocolFamilyField.java
 CVS Info:      $Id$
 Compiler:	jdk 1.2.2
 */

package disEnumerations;

import mil.navy.nps.dis.*;
import mil.navy.nps.util.*;

/**
 * Protocol Family Field -- This field shall indicate the family of protocols to which the PDU belongs
 *@version 1.1
 *@author Ronan Fauglas
 *@author <a href="mailto:brutzman@nps.navy.mil?subject=dis-java-vrml: disEnumerations.ProtocolFamilyField feedback ">Don Brutzman</a>
 *
 *<dt><b>References:</b>
 *<dd>		DIS Data Dictionary: <A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/9.htm">Protocol Family Field</A> (local)
 *				     <A href="http://SISO.sc.ist.ucf.edu/dis/dis-dd/pdu/9.htm">Protocol Family Field</A> (SISO)
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
 *  You access this via something like <b><code>ProtocolFamilyField.DISTRIBUTEDEMISSIONREGENERATION</code></b>, i.e. combine
 *  the class name, a period, and a class variable (enumeration) name.<P>
 *
 *<dt><b>History:</b>
 *<dd>		21jan98   /Ronan Fauglas   	/New
 *<dd>		30mar99   /Don Brutzman   	/Revised Javadoc, many more enumeration classes
 *
 *<dt><b>Location:</b>
 *<dd><a href="../../../../../../mil/navy/nps/disEnumerations/ProtocolFamilyField.java"><i>ProtocolFamilyField.java</i></A> (local)
 *<dd><a href="http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/ProtocolFamilyField.java">
 *          <i>http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/ProtocolFamilyField.java</i></a>
 *
 */
public class ProtocolFamilyField extends Object
{
/**
 *(0) other
 */
public static final short OTHER = 0;

/**
 *(1) Entity Information/Interaction
 */
public static final short ENTITYINFORMATIONINTERACTION = 1;

/**
 *(129) Experimental - CGF
 */
public static final short EXPERIMENTALCGF = 129;

/**
 *(130) Experimental - Entity Interaction/Information - Field Instrumentation
 */
public static final short EXPERIMENTALENTITYINTERACTIONINFORMATIONFIELDINSTRUMENTATION = 130;

/**
 *(131) Experimental - Warfare Field Instrumentation
 */
public static final short EXPERIMENTALWARFAREFIELDINSTRUMENTATION = 131;

/**
 *(132) Experimental - Environment Object Information/Interaction
 */
public static final short EXPERIMENTALENVIRONMENTOBJECTINFORMATIONINTERACTION = 132;

/**
 *(133) Experimental - Entity Management
 */
public static final short EXPERIMENTALENTITYMANAGEMENT = 133;

/**
 *(2) Warfare
 */
public static final short WARFARE = 2;

/**
 *(3) Logistics
 */
public static final short LOGISTICS = 3;

/**
 *(4) Radio Communication
 */
public static final short RADIOCOMMUNICATION = 4;

/**
 *(5) Simulation Management
 */
public static final short SIMULATIONMANAGEMENT = 5;

/**
 *(6) Distributed Emission Regeneration
 */
public static final short DISTRIBUTEDEMISSIONREGENERATION = 6;

/**
 * Returns a string containing the enumeration name which corresponds to an enumeration value,
 * as in <b><code>ProtocolFamilyField.toString (0)</code></b> returns the string "<b><code>OTHER</code></b>"
 */

public static String toString(int idNumber) 
{
	switch (idNumber) {
		case 0: return "other"; 
		case 1: return "Entity Information/Interaction"; 
		case 129: return "Experimental - CGF"; 
		case 130: return "Experimental - Entity Interaction/Information - Field Instrumentation"; 
		case 131: return "Experimental - Warfare Field Instrumentation"; 
		case 132: return "Experimental - Environment Object Information/Interaction"; 
		case 133: return "Experimental - Entity Management"; 
		case 2: return "Warfare"; 
		case 3: return "Logistics"; 
		case 4: return "Radio Communication"; 
		case 5: return "Simulation Management"; 
		case 6: return "Distributed Emission Regeneration"; 
		default : return "";
	}
}//end of toString
}//End of class 
