/*
 File:		PduTypeField.java
 CVS Info:      $Id$
 Compiler:	jdk 1.3 
 */

package disEnumerations;

import mil.navy.nps.dis.*;
import mil.navy.nps.util.*;

/**
 * PDU Type Field -- This field shall indicate the  type of PDU that follows.
 *@version 1.1
 *@author Ronan Fauglas
 *@author <a href="mailto:brutzman@nps.navy.mil?subject=dis-java-vrml: disEnumerations.PduTypeField feedback ">Don Brutzman</a>
 *
 *<dt><b>References:</b>
 *<dd>		DIS Data Dictionary: <A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/8.htm">PDU Type Field</A> (local)
 *				     <A href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/8.htm">PDU Type Field</A> (SISO)
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
 *  You access this via something like <b><code>PduTypeField.REPAIRCOMPLETE</code></b>, i.e. combine
 *  the class name, a period, and a class variable (enumeration) name.<P>
 *
 *<dt><b>History:</b>
 *<dd>		21jan98   /Ronan Fauglas   	/New
 *<dd>		30mar99   /Don Brutzman   	/Revised Javadoc, many more enumeration classes
 *<dd>		4oct2000   /Don Brutzman   	/renamed class from PDUTypeField to PduTypeField for consistency
 *
 *<dt><b>Location:</b>
 *<dd><a href="../../../../../../mil/navy/nps/disEnumerations/PduTypeField.java"><i>PduTypeField.java</i></A> (local)
 *<dd><a href="http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/PduTypeField.java">
 *          <i>http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/PduTypeField.java</i></a>
 *
 */
public class PduTypeField_2 extends Object
{
/**
 *(0) Other
 */
public static final short OTHER = 0;

/**
 *(1) Entity State
 */
public static final short ENTITYSTATE = 1;

/**
 *(10) Repair Response
 */
public static final short REPAIRRESPONSE = 10;

/**
 *(11) Create Entity
 */
public static final short CREATEENTITY = 11;

/**
 *(12) Remove Entity
 */
public static final short REMOVEENTITY = 12;

/**
 *(129) Announce Object
 */
public static final short ANNOUNCEOBJECT = 129;

/**
 *(13) Start/Resume
 */
public static final short STARTRESUME = 13;

/**
 *(130) Delete Object
 */
public static final short DELETEOBJECT = 130;

/**
 *(131) Describe Application
 */
public static final short DESCRIBEAPPLICATION = 131;

/**
 *(132) Describe Event
 */
public static final short DESCRIBEEVENT = 132;

/**
 *(133) Describe Object
 */
public static final short DESCRIBEOBJECT = 133;

/**
 *(134) Request Event
 */
public static final short REQUESTEVENT = 134;

/**
 *(135) Request Object
 */
public static final short REQUESTOBJECT = 135;

/**
 *(14) Stop/Freeze
 */
public static final short STOPFREEZE = 14;

/**
 *(140) Time Space Position Indicator - FI
 */
public static final short TIMESPACEPOSITIONINDICATORFI = 140;

/**
 *(141) Appearance-FI
 */
public static final short APPEARANCEFI = 141;

/**
 *(142) Articulated Parts - FI
 */
public static final short ARTICULATEDPARTSFI = 142;

/**
 *(143) Fire - FI
 */
public static final short FIREFI = 143;

/**
 *(144) Detonation - FI
 */
public static final short DETONATIONFI = 144;

/**
 *(15) Acknowledge
 */
public static final short ACKNOWLEDGE = 15;

/**
 *(150) Point Object State
 */
public static final short POINTOBJECTSTATE = 150;

/**
 *(151) Linear Object State
 */
public static final short LINEAROBJECTSTATE = 151;

/**
 *(152) Areal Object State
 */
public static final short AREALOBJECTSTATE = 152;

/**
 *(153) Environment
 */
public static final short ENVIRONMENT = 153;

/**
 *(155) Transfer Control Request
 */
public static final short TRANSFERCONTROLREQUEST = 155;

/**
 *(156) Transfer Control
 */
public static final short TRANSFERCONTROL = 156;

/**
 *(157) Transfer Control Acknowledge
 */
public static final short TRANSFERCONTROLACKNOWLEDGE = 157;

/**
 *(16) Action Request
 */
public static final short ACTIONREQUEST = 16;

/**
 *(160) Intercom Control
 */
public static final short INTERCOMCONTROL = 160;

/**
 *(161) Intercom Signal
 */
public static final short INTERCOMSIGNAL = 161;

/**
 *(17) Action Response
 */
public static final short ACTIONRESPONSE = 17;

/**
 *(170) Aggregate
 */
public static final short AGGREGATE = 170;

/**
 *(18) Data Query
 */
public static final short DATAQUERY = 18;

/**
 *(19) Set Data
 */
public static final short SETDATA = 19;

/**
 *(2) Fire
 */
public static final short FIRE = 2;

/**
 *(20) Data
 */
public static final short DATA = 20;

/**
 *(21) Event Report
 */
public static final short EVENTREPORT = 21;

/**
 *(22) Comment
 */
public static final short COMMENT = 22;

/**
 *(23) Electromagnetic Emission
 */
public static final short ELECTROMAGNETICEMISSION = 23;

/**
 *(24) Designator
 */
public static final short DESIGNATOR = 24;

/**
 *(25) Transmitter
 */
public static final short TRANSMITTER = 25;

/**
 *(26) Signal
 */
public static final short SIGNAL = 26;

/**
 *(27) Receiver
 */
public static final short RECEIVER = 27;

/**
 *(3) Detonation
 */
public static final short DETONATION = 3;

/**
 *(4) Collision
 */
public static final short COLLISION = 4;

/**
 *(5) Service Request
 */
public static final short SERVICEREQUEST = 5;

/**
 *(6) Resupply Offer
 */
public static final short RESUPPLYOFFER = 6;

/**
 *(7) Resupply Received
 */
public static final short RESUPPLYRECEIVED = 7;

/**
 *(8) Resupply Cancel
 */
public static final short RESUPPLYCANCEL = 8;

/**
 *(9) Repair Complete
 */
public static final short REPAIRCOMPLETE = 9;

/**
 * Returns a string containing the enumeration name which corresponds to an enumeration value,
 * as in <b><code>PduTypeField.toString (0)</code></b> returns the string "<b><code>OTHER</code></b>"
 */

public static String toString(int idNumber) 
{
	switch (idNumber) {
		case 0: return "Other"; 
		case 1: return "Entity State"; 
		case 10: return "Repair Response"; 
		case 11: return "Create Entity"; 
		case 12: return "Remove Entity"; 
		case 129: return "Announce Object"; 
		case 13: return "Start/Resume"; 
		case 130: return "Delete Object"; 
		case 131: return "Describe Application"; 
		case 132: return "Describe Event"; 
		case 133: return "Describe Object"; 
		case 134: return "Request Event"; 
		case 135: return "Request Object"; 
		case 14: return "Stop/Freeze"; 
		case 140: return "Time Space Position Indicator - FI"; 
		case 141: return "Appearance-FI"; 
		case 142: return "Articulated Parts - FI"; 
		case 143: return "Fire - FI"; 
		case 144: return "Detonation - FI"; 
		case 15: return "Acknowledge"; 
		case 150: return "Point Object State"; 
		case 151: return "Linear Object State"; 
		case 152: return "Areal Object State"; 
		case 153: return "Environment"; 
		case 155: return "Transfer Control Request"; 
		case 156: return "Transfer Control"; 
		case 157: return "Transfer Control Acknowledge"; 
		case 16: return "Action Request"; 
		case 160: return "Intercom Control"; 
		case 161: return "Intercom Signal"; 
		case 17: return "Action Response"; 
		case 170: return "Aggregate"; 
		case 18: return "Data Query"; 
		case 19: return "Set Data"; 
		case 2: return "Fire"; 
		case 20: return "Data"; 
		case 21: return "Event Report"; 
		case 22: return "Message"; 
		case 23: return "Electromagnetic Emission"; 
		case 24: return "Designator"; 
		case 25: return "Transmitter"; 
		case 26: return "Signal"; 
		case 27: return "Receiver"; 
		case 3: return "Detonation"; 
		case 4: return "Collision"; 
		case 5: return "Service Request"; 
		case 6: return "Resupply Offer"; 
		case 7: return "Resupply Received"; 
		case 8: return "Resupply Cancel"; 
		case 9: return "Repair Complete"; 
		default : return "";
	}
}//end of toString
}//End of class 
