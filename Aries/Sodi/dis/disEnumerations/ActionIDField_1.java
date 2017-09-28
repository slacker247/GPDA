/*
 File:		ActionIDField.java
 CVS Info:      $Id$
 Compiler:	jdk 1.2.2
 */

package disEnumerations;

import mil.navy.nps.dis.*;
import mil.navy.nps.util.*;

/**
 * Action ID Field -- This field shall specify the particular action that is requested by the simulation manager. This field shall be represented by a32-bit enumeration (see Section 7 in EBV-DOC).
 *@version 1.1
 *@author Ronan Fauglas
 *@author <a href="mailto:brutzman@nps.navy.mil?subject=dis-java-vrml: disEnumerations.ActionIDField feedback ">Don Brutzman</a>
 *
 *<dt><b>References:</b>
 *<dd>		DIS Data Dictionary: <A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/a7.htm">Action ID Field</A> (local)
 *				     <A href="http://SISO.sc.ist.ucf.edu/dis/dis-dd/pdu/a7.htm">Action ID Field</A> (SISO)
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
 *  You access this via something like <b><code>ActionIDField.RECALLINITIALPARAMETERS</code></b>, i.e. combine
 *  the class name, a period, and a class variable (enumeration) name.<P>
 *
 *<dt><b>History:</b>
 *<dd>		21jan98   /Ronan Fauglas   	/New
 *<dd>		30mar99   /Don Brutzman   	/Revised Javadoc, many more enumeration classes
 *
 *<dt><b>Location:</b>
 *<dd><a href="../../../../../../mil/navy/nps/disEnumerations/ActionIDField.java"><i>ActionIDField.java</i></A> (local)
 *<dd><a href="http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/ActionIDField.java">
 *          <i>http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/ActionIDField.java</i></a>
 *
 */
public class ActionIDField_1 extends Object
{
/**
 *(0) Other
 */
public static final int OTHER = 0;

/**
 *(1) Local storage of the requested information
 */
public static final int LOCALSTORAGEOFTHEREQUESTEDINFORMATION = 1;

/**
 *(10) Initiate tether-lead
 */
public static final int INITIATETETHERLEAD = 10;

/**
 *(11) Initiate tether-follow
 */
public static final int INITIATETETHERFOLLOW = 11;

/**
 *(12) Untether
 */
public static final int UNTETHER = 12;

/**
 *(13) Initiate service station resupply
 */
public static final int INITIATESERVICESTATIONRESUPPLY = 13;

/**
 *(14) Initiate tailgate resupply
 */
public static final int INITIATETAILGATERESUPPLY = 14;

/**
 *(15) Initiate hitch lead
 */
public static final int INITIATEHITCHLEAD = 15;

/**
 *(16) Initiate hitch follow
 */
public static final int INITIATEHITCHFOLLOW = 16;

/**
 *(17) Unhitch
 */
public static final int UNHITCH = 17;

/**
 *(18) Mount
 */
public static final int MOUNT = 18;

/**
 *(19) Dismount
 */
public static final int DISMOUNT = 19;

/**
 *(2) Inform SM of event \"ran out of ammunition\"
 */
public static final int INFORMSMOFEVENTRANOUTOFAMMUNITION = 2;

/**
 *(20) Start DRC (Daily Readiness Check)
 */
public static final int STARTDRC = 20;

/**
 *(21) Stop DRC
 */
public static final int STOPDRC = 21;

/**
 *(22) Data Query
 */
public static final int DATAQUERY = 22;

/**
 *(23) Status Request
 */
public static final int STATUSREQUEST = 23;

/**
 *(24) Send Object State Data
 */
public static final int SENDOBJECTSTATEDATA = 24;

/**
 *(25) Reconstitute
 */
public static final int RECONSTITUTE = 25;

/**
 *(26) Lock Site Configuration
 */
public static final int LOCKSITECONFIGURATION = 26;

/**
 *(27) Unlock Site Configuration
 */
public static final int UNLOCKSITECONFIGURATION = 27;

/**
 *(28) Update Site Configuration
 */
public static final int UPDATESITECONFIGURATION = 28;

/**
 *(29) Query Site Configuration
 */
public static final int QUERYSITECONFIGURATION = 29;

/**
 *(3) Inform SM of event \"killed in action\"
 */
public static final int INFORMSMOFEVENTKILLEDINACTION = 3;

/**
 *(30) Tethering Information
 */
public static final int TETHERINGINFORMATION = 30;

/**
 *(31) Mount Intent
 */
public static final int MOUNTINTENT = 31;

/**
 *(33) Accept Subscription
 */
public static final int ACCEPTSUBSCRIPTION = 33;

/**
 *(34) Unsubscribe
 */
public static final int UNSUBSCRIBE = 34;

/**
 *(35) Teleport entity
 */
public static final int TELEPORTENTITY = 35;

/**
 *(36) Change aggregate state
 */
public static final int CHANGEAGGREGATESTATE = 36;

/**
 *(4) Inform SM of event \"damage\"
 */
public static final int INFORMSMOFEVENTDAMAGE = 4;

/**
 *(5) Inform SM of event \"mobility disabled\"
 */
public static final int INFORMSMOFEVENTMOBILITYDISABLED = 5;

/**
 *(6) Inform SM of event \"fire disabled\"
 */
public static final int INFORMSMOFEVENTFIREDISABLED = 6;

/**
 *(7) Inform SM of event \"ran out of fuel\"
 */
public static final int INFORMSMOFEVENTRANOUTOFFUEL = 7;

/**
 *(8) Recall checkpoint data
 */
public static final int RECALLCHECKPOINTDATA = 8;

/**
 *(9) Recall initial parameters
 */
public static final int RECALLINITIALPARAMETERS = 9;

/**
 * Returns a string containing the enumeration name which corresponds to an enumeration value,
 * as in <b><code>ActionIDField.toString (0)</code></b> returns the string "<b><code>OTHER</code></b>"
 */

public static String toString(int idNumber) 
{
	switch (idNumber) {
		case 0: return "Other"; 
		case 1: return "Local storage of the requested information"; 
		case 10: return "Initiate tether-lead"; 
		case 11: return "Initiate tether-follow"; 
		case 12: return "Untether"; 
		case 13: return "Initiate service station resupply"; 
		case 14: return "Initiate tailgate resupply"; 
		case 15: return "Initiate hitch lead"; 
		case 16: return "Initiate hitch follow"; 
		case 17: return "Unhitch"; 
		case 18: return "Mount"; 
		case 19: return "Dismount"; 
		case 2: return "Inform SM of event \"ran out of ammunition\""; 
		case 20: return "Start DRC (Daily Readiness Check)"; 
		case 21: return "Stop DRC"; 
		case 22: return "Data Query"; 
		case 23: return "Status Request"; 
		case 24: return "Send Object State Data"; 
		case 25: return "Reconstitute"; 
		case 26: return "Lock Site Configuration"; 
		case 27: return "Unlock Site Configuration"; 
		case 28: return "Update Site Configuration"; 
		case 29: return "Query Site Configuration"; 
		case 3: return "Inform SM of event \"killed in action\""; 
		case 30: return "Tethering Information"; 
		case 31: return "Mount Intent"; 
		case 33: return "Accept Subscription"; 
		case 34: return "Unsubscribe"; 
		case 35: return "Teleport entity"; 
		case 36: return "Change aggregate state"; 
		case 4: return "Inform SM of event \"damage\""; 
		case 5: return "Inform SM of event \"mobility disabled\""; 
		case 6: return "Inform SM of event \"fire disabled\""; 
		case 7: return "Inform SM of event \"ran out of fuel\""; 
		case 8: return "Recall checkpoint data"; 
		case 9: return "Recall initial parameters"; 
		default : return "";
	}
}//end of toString
}//End of class 
