/*
 File:		EspduTransform.java
 CVS Info:	$Id: EspduTransform.java,v 1.3 1998/02/15 15:29:20 brutzman Exp $
 Compiler:	jdk 1.3
 */

package mil.navy.nps.dis;

import java.util.*;
import java.io.*;

import java.lang.Math.*;  // for PI

// Java JSAI classes for VRML

import vrml.*;
import vrml.field.*;
import vrml.node.*;

// NPS DIS library classes

import mil.navy.nps.disEnumerations.*;
import mil.navy.nps.math.*;
import mil.navy.nps.util.*;

import org.web3d.vrtp.security.*;

/**
 * This Java class provides the communicatons interface between the EspduTransform Script node
 * (in the VRML scene) and the rest of the DIS class library used for entities.
 *
 *@version 1.2
 *@author <a href="mailto:brutzman@nps.navy.mil">Don Brutzman</a>
 *(<a href="http://web.nps.navy.mil/~brutzman">http://web.nps.navy.mil/~brutzman</a>)
 * This code includes parts of Kent Watsen's EAI-based World.java/Ownship.java
 * and Don McGregors's testing/BehaviorStreamBufferTest.java
 *
 *<dt><b>Location:</b>
 *<dd><a href="../../../../../../mil/navy/nps/dis/EspduTransform.java">EspduTransform.java</A>
 *<dd><a href="http://web.nps.navy.mil/~brutzman/vrtp/mil/navy/nps/dis/EspduTransform.java">web.nps.navy.mil/~brutzman/vrtp/mil/navy/nps/dis/EspduTransform.java</a>
 *<dd><a href="http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/dis/EspduTransform.java">www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/dis/EspduTransform.java</a>
 *
 *<dt><b>Hierarchy Diagram:</b>
 *<dd><a href="../../../../../../dis-java-vrml/images/PduClassHierarchy.jpg"><IMG SRC="../../../../../../dis-java-vrml/images/PduClassHierarchyButton.jpg" ALIGN=ABSCENTER WIDTH=150 HEIGHT=21></a>
 *
 *<dt><b>History:</b>
 *<TABLE>
 *<tr>
 *	<td>	1 February 2000
 *	<td>	Olivier Doucy, Don Brutzman
 *	<td>	added timestamp as an eventOut
 *<tr>
 *	<td>	3 September 99
 *	<td>	Don Brutzman
 *	<td>	added rtpEnabled boolean flag
 *<tr>
 *	<td>	19 June 99
 *	<td>	Don Brutzman
 *	<td>	Removed articulationParameterValue15 to stay within VRML browser conformance limit of 25 eventOuts
 *<tr>
 *	<td>	1 June 99
 *	<td>	Don Brutzman, Quay Jones
 *	<td>	Added detonated and detonateTime
 *<tr>
 *	<td>	18 May 99
 *	<td>	Don Brutzman, Quay Jones
 *	<td>	Changed fired to fired1 and fired2 in VRML scene to enable primary and secondary weapons.
 *<tr>
 *	<td>	6 Apr 99
 *	<td>	Don Brutzman
 *	<td>	Adding ArticulatedParameter eventOuts, shutdown, various thread/trace fixes.
 *		Ignore received PDUs when sending.  Changed MFVec3f LineOfFirePointArray code to
 *		SFVec3f's munitionStartPoint & munitionEndPoint for run-time reliability.
 *<tr>
 *	<td>	20 Feb 99, 14 Mar 99
 *	<td>	Don Brutzman, Don McGregor, Bill Bohman
 *	<td>	Added collided, active, fired, multicastRelayHost, multicastRelayPort.
 *		Also added FirePdu geometry to show munitions.
 *		Incorporated entityDispatcher.checkForTrafficAndFallBackToTunnel (...)
 *		for automatic unicast connectivity when multicast is not locally available.
 *<tr>
 *	<td>	6 Feb 99
 *	<td>	Don Brutzman
 *	<td>	Move VRML EspduTransformPROTO to same directory as dis package
 *		<br>
 *		Fix write capability
 * <tr>
 *	<td>	02 Dec 98
 *	<td>	Scott Heller
 *	<td>	Added smoothing.  See
 *		<a href="../../../../../../dis-java-vrml/SmoothingDesign.pdf">SmoothingDesign.pdf</a>
 *<tr>
 *	<td>	25 Oct 98
 *	<td>	Don Brutzman
 *	<td>	Javadoc updates for jdk1.2b4
 *		<br>
 *		Restructure classes, move EspduTransform to dis package
 *<tr>
 *	<td>	12 Feb 98
 *	<td>	Don Brutzman
 *	<td>	changes for documentation templates + complements in documentation
 *<tr>
 *	<td>	5 Feb 98
 *	<td>	Don McGregor
 *	<td>	changes for multiple entities, using EntityDispatcher
 *<tr>
 *	<td>	1 Aug 97
 *	<td>	Don Brutzman
 *	<td>	New
 *</TABLE>
 *<P>
 *
 *<dt><b>Associated VRML Files:</b>
 *<dd>	<a href="../../../../../../mil/navy/nps/dis/EspduTransformPROTO.wrl">EspduTransformPROTO.wrl</A>
 *(PROTO definition)
 *<dd>	<a href="../../../../../../mil/navy/nps/dis/EspduTransformEXAMPLE.wrl">EspduTransformEXAMPLE.wrl</A>
 *(Example Use)
 *
 *<P>
 *
 *<dt><B>Entity coordinate systems (right-hand rule applies):</B>
 *<PRE>
 *
 *          DIS                 VRML
 *
 *      -Z entity up         Y entity up
 *           ^                   ^
 *           |                   |
 *           |                   |
 *           |                   |
 *           +------> X          +-------> X    nose of entity body
 *          /                   /
 *         /                   /
 *        /                   /
 *       Y                   Z
 *         right-hand side
 *         of entity body
 *
 *  Rotation angle axes (right-hand rule applies):
 *
 *           DIS        VRML      Angle of rotation
 *
 *  Roll      X          X         phi
 *  Pitch     Y          Z        theta
 *  Yaw       Z         -Y         psi
 *</PRE>
 *
 *<dt><b>Unfinished business:</b>
 *<dd>	Need to verify PDU identity and match that given in scene?
 *(already done by EntityDispatcher)
 *<dd>	Need to verify Euler angle order & quaternion conversion
 *<dd>	Is there another callback alternative to trigger processEvent
 *originating from DIS rather than VRML?  Apparently not...  readPdu and writePdu events
 *generated by read/write TimeSensors adjacent to the Script node trigger this script's processEvent script.
 *<dd>	Is VRML timeSensor processEvent trigger best?  Likely so,
 * since it ensures VRML scene redraw doesn't undergo starvation, and
 * it also prevents "simultaneous" data access/modification of Transform
 *  node values by VRML browser/Java script.
 *
 *<dd>	Exercise clock synchronization or relative local time? dead
 * 	reckon interval is currently estimated by accumulating the
 * 	number of seconds since the last pdu update.
 *
 *<dd>	Add official DIS algorithms for dead-reckoning.
 *<P>
 *
 *<dt><b>Debugging notes:</b>
 *<dd>	System.out.println text appears on WorldView's VRML console
 *<dd>	or on the Netscape Java console when using CosmoPlayer
 *
 *@see EntityDispatcher
 *@see BehaviorStreamBuffer
 *@see RunningAverage
 *@see CollisionPdu
 *@see DetonationPdu
 *@see EntityStatePdu
 *@see FirePdu
 */


public class EspduTransform extends Script implements PduSubscriber, NetworkCommBadge // ---------------
{
/**
 * Declare eventIns, fields and eventOuts from VRML scene
 */

/**
 * field, hook to VRML scene
 */

	private SFNode 		transformNode;

/**
 * field, hook to VRML scene
 */

	private Node		transformNodeAccess;

/**
 * eventOut: timestamp
 */

private SFTime   timestamp;

/**
 *  field, 11-character DIS marking.  Can't be <CODE>static</CODE> or all entities look the same.
 */

	private SFString marking;
/**
 * field, how often to look for DIS PDUs. Units are seconds in VRML, converted to msec in Java in initialize ().
 *  readInterval=0 means no reading.  Longs are used because VRML SFFloat units are seconds.
 */

	private long readInterval;

/**
 * field, how often to write DIS PDUs. Units are seconds in VRML, converted to msec in Java in initialize ().
 *  writeInterval=0 means no writing (which is the default).  Longs are used because VRML SFFloat units are seconds.
 */

	private long writeInterval;

/**
 * field, multicast address, or "unicast"
 */

	private SFString	address;

/**
 * field, port to listen on
 */

	private SFInt32		port;

/**
 * field, relay host name or IP address (used only if no multicast heard)
 */

	private SFString	multicastRelayHost;

/**
 * field, relay host port to connect to (used only if no multicast heard)
 */

	private SFInt32		multicastRelayPort;

/**
 * field, are RTP headers expected?  Only set by VRML scene at initialization (because it is a field)
 * but can be reset at runtime if the opposite situation is encountered.
 */

	private SFBool		rtpHeaderExpected;

/**
 * eventOut, are RTP headers being heard on the wire?
 */

	private SFBool		rtpHeaderHeard = new SFBool (false);

/**
 * field, entityID triplet: site ID
 */

private SFInt32   siteID;

/**
 * field, entityID triplet: unique app ID at that site
 */

private SFInt32   applicationID;

/**
 * field, entity ID triplet: ID within that app
 */

private SFInt32   entityID;

/**
 * EspduTransformPROTO VRML scene exposedField
 */
	private SFVec3f		translation;

/**
 *  EspduTransformPROTO VRML scene exposedField
 */
	private SFRotation 	rotation;

/**
 * field: enable Java trace statements to console
 */

private SFBool   traceJava;

/**
 * eventOut: recent active ESPDUs heard
 */

private SFBool   active = new SFBool (false);

/**
 * eventOut: Collision PDU heard
 */

private SFBool   collided = new SFBool (false);

/**
 * eventOut: time of Collision
 */

private SFTime   collideTime;

/**
 * eventOut: Detonation PDU heard
 */

private SFBool   detonated = new SFBool (false);

/**
 * eventOut: time of detonation
 */

private SFTime   detonateTime;

/**
 * eventOut: have we fired a Fire (weapon) PDU for primary (major) weapon?
 */

private SFBool   fired1 = new SFBool (false);

/**
 * eventOut: have we fired a Fire (weapon) PDU for secondary (minor) weapon?
 */

private SFBool   fired2 = new SFBool (false);

/**
 * eventOut: when did we fire a Fire (weapon) PDU?
 */

private SFTime   firedTime;

/**
 *  local variable
 */
	private SFVec3f		position;		// set

//  local variables

	private SFRotation 	orientation;	// set

	private Class		espduClass	= null;
	private	int 		pduCount	= 0;
	private Quaternion	quaternion	= null;
	private float		rot[]		= null;
	private float		eulers[]	= null;

	private EntityID    	entityIDObject	= null;	// unique triplet that identifies entity

	private Vector      	receivedPDUsList= new Vector();  // holds PDUs that have come in

	protected static      	EntityDispatcher entityDispatcher = null;   // sends/receives PDUs
	protected static	Thread entityDispatcherThread;

	private static Boolean	guard = new Boolean(true);  // guard for entityDispatcher

	private boolean     		registered = false; // true=>has been placed in hash table

	private boolean     		entityDispatcherThreadStarted = false;

	protected Browser		browser		= null; // VRML browser

	private boolean			PduTrafficLastTime = false;

	private int 			pduVectorSize;
	protected ProtocolDataUnit	pdu		= null;

	protected EntityStatePdu	espdu		= new EntityStatePdu ();

	private boolean     		espduFirstHeard = true;

	protected final int MAX_ARTICULATION_PARAMETERS = 15;
	protected SFInt32 articulationParameterCount;	// read from network->VRML only, no write from VRML->network yet
	protected SFFloat articulationParameterValue0;
	protected SFFloat articulationParameterValue1;
	protected SFFloat articulationParameterValue2;
	protected SFFloat articulationParameterValue3;
	protected SFFloat articulationParameterValue4;
	protected SFFloat articulationParameterValue5;
	protected SFFloat articulationParameterValue6;
	protected SFFloat articulationParameterValue7;
	protected SFFloat articulationParameterValue8;
	protected SFFloat articulationParameterValue9;
	protected SFFloat articulationParameterValue10;
	protected SFFloat articulationParameterValue11;
	protected SFFloat articulationParameterValue12;
	protected SFFloat articulationParameterValue13;
	protected SFFloat articulationParameterValue14;
	private int previousChangeIndicator0  = -2;
	private int previousChangeIndicator1  = -2;
	private int previousChangeIndicator2  = -2;
	private int previousChangeIndicator3  = -2;
	private int previousChangeIndicator4  = -2;
	private int previousChangeIndicator5  = -2;
	private int previousChangeIndicator6  = -2;
	private int previousChangeIndicator7  = -2;
	private int previousChangeIndicator8  = -2;
	private int previousChangeIndicator9  = -2;
	private int previousChangeIndicator10 = -2;
	private int previousChangeIndicator11 = -2;
	private int previousChangeIndicator12 = -2;
	private int previousChangeIndicator13 = -2;
	private int previousChangeIndicator14 = -2;
	private int changeIndicator0          = -1;
	private int changeIndicator1          = -1;
	private int changeIndicator2          = -1;
	private int changeIndicator3          = -1;
	private int changeIndicator4          = -1;
	private int changeIndicator5          = -1;
	private int changeIndicator6          = -1;
	private int changeIndicator7          = -1;
	private int changeIndicator8          = -1;
	private int changeIndicator9          = -1;
	private int changeIndicator10         = -1;
	private int changeIndicator11         = -1;
	private int changeIndicator12         = -1;
	private int changeIndicator13         = -1;
	private int changeIndicator14         = -1;

/**
 *  added by Scott Heller 21 Nov 98 for smoothing
 */
	private EntityStatePdu	previousEspdu			= null;
	private float		prev_dt				= 0.0f;
	private RunningAverage	averageUpdateTime = new RunningAverage(5);
	private RunningAverage	aveSecPerFrame 	= new RunningAverage(5);
	private float[] 	positionArray   = new float[3];
	private final int	ROLL = 2, PITCH = 1, YAW = 0;
	private	boolean		DoNotSmooth	= false;
	private Timer           secSinceLastPdu	= new Timer();  // seconds
	private Timer           secPerFrame	= new Timer();  // seconds per browser frame
	private boolean         SMOOTH_LINVELOCITY = false; // should velocity be smoothed?
/**
 * what portion of an averageupdateTime should we smooth for? For example 0.5 will cause the
 * the smoothing algorithm to reaquire the most recent DR track with in half the
 * PDU averageUpdateTime.
 */
	protected final float	SMOOTH_LIMIT = 1.0f;	// controls how fast the smoothed update
			     				// SMOOTH_LIMIT * aveSecPerFrame = how fast.
																			// 1.0f seems to be the only pleasing value.
	protected final float	DR_LIMIT     = 5.0f;	// seconds, max time to dead reckon without updates
/**
 * are the DR and smooth close enough to stop smoothing?
 */
	private   boolean	closeEnough 		= false;

/**
 * End variable additions by Scott Heller
 */

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	private float  initPos[]     = {0f, 5f, 0f}; // x, y, z
	private float  initPosDt[]   = {0f, 0f, 0f}; // aka: velocity
	private float  initAng[]     = {0f, 0f, 0f}; // heading, pitch, roll
	private float  initAngDt[]   = {0f, 0f, 0f}; // aka: angular rates

	private Quaternion          tmpQuat       = null;

	private float	pos[]         = null; // x, y, z
	private float	posDt[]       = null; // aka: velocity
	private float	ang[]         = null; // heading, pitch, roll
	private float	angDt[]       = null; // aka: angular rates
	private float	drPos[]       = null; // x, y, z
	private float	drPosDt[]     = null; // aka: velocity
	private float	drAng[]       = null; // heading, pitch, roll
	private float	drAngDt[]     = null; // aka: angular rates

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

/**
 * munitionStartPoint is node's field (hook to VRML scene) obtained from FirePdu matching launch point of munition.
 */

	protected SFVec3f		munitionStartPoint = new SFVec3f (0, 0, 0);

/**
 * munitionEndPoint is node's field (hook to VRML scene) computed from FirePdu matching detonation point of munition.
 */

	protected SFVec3f		munitionEndPoint   = new SFVec3f (0, 0, 0);

/**
 * local count of firePdu's
 */

	private	int 	firePduCount	= 0;

/**
 *   Get platform-specific security strategy
 */
 
 protected SecurityStrategy strategy;  // = SecurityStrategy.getSecurityStrategy();
 
/**
 * Turn Smoothing on or off. Returns the new state of DoNotSmooth.
 */
	public boolean toggleSmoothing()
	{
		if (DoNotSmooth)	DoNotSmooth = false;
		else			DoNotSmooth = true;

		return DoNotSmooth;
	}

/**
 * Get Smoothing flag.
 */
	public boolean getDoNotSmooth()
	{
		trace ("getDoNotSmooth = " + DoNotSmooth);

		return DoNotSmooth;
	}

/**
 * Set Smoothing flag.
 */
	public void setDoNotSmooth(boolean pDoNotSmooth)
	{
		DoNotSmooth = pDoNotSmooth;
		debug ("setDoNotSmooth = " + DoNotSmooth);

		return;
	}

/**
 * When DEBUG is true, console text messages
 * trace the internals of script operation.
 */

	protected boolean DEBUG = false;  // diagnostic messages on (true) or off (false)

/**
 * Access DEBUG trace variable.
 */
	public boolean getDEBUG ()
	{
		debug ("getDEBUG " + DEBUG);

		return DEBUG;
	}

/**
 * Modify DEBUG trace variable.
 */
	public void setDEBUG (boolean pDEBUG)
	{
		DEBUG = pDEBUG;

		traceJava.setValue (pDEBUG);

		trace ("setDEBUG " + pDEBUG);  // send confirming VRML console trace message
	}

/**
 * Debugging output routine. Pass in a string, and it gets printed out on the console.
 * You can pass in strings such as "foo " + bar.getName().
 * Text output appears in the Java Console (CosmoPlayer browser)
 * or in the VRML console (WorldView browser).
 */

	protected void debug (String pDiagnostic)
	{
		if (DEBUG) System.out.println("  EspduTransform " + marking + " " + pDiagnostic);
		System.out.flush ();
	}

/**
 * Guaranteed trace output routine. Pass in a string, and it gets printed out on the console.
 * You can pass in strings such as "foo " + bar.getName().
 * Text output appears in the Java Console (CosmoPlayer browser)
 * or in the VRML console (WorldView browser).
 * <P>
 * Can't be <CODE>static</CODE> or all entities look the same.
 */

	protected void trace (String pDiagnostic)
	{
		System.out.println("  EspduTransform " + marking + " " + pDiagnostic);
		System.out.flush ();
	}

/**
 * Simple name for the ESPDU is the marking field.
 */

	public String getName () { return "EspduTransform " + marking; }

	protected Timer localFireTimer1 = new Timer ();
	protected Timer localFireTimer2 = new Timer ();


/** Constructor that includes setting security permissions since
 * Internet Explorer will throw a com.ms.security.SecurityExceptionEx
 * if initialize () is called from an external source (i.e. the IE browser)
 * and security isn't already set up.
 * Reference:
 * <A HREF="http://www.suitable.com/CodeSigningCodeExp.shtml">Code Signing for Java Applets</A>
 * by Daniel Griscom, griscom@suitable.com
 * at
 * <A HREF="http://www.suitable.com/CodeSigningCodeExp.shtml">http://www.suitable.com/CodeSigningCodeExp.shtml</A>
 *
 */
 
public EspduTransform ()
{
  debug ("*** constructor for mil.navy.nps.dis.EspduTransform");
 	  
  debug ("Get platform-specific security strategy");
  strategy = SecurityStrategy.getSecurityStrategy();
}

/**
 * initialize () gets needed information for setup from the invoking scene entity and VRML browser,
 * and also prints the values if DEBUG is true.
 * For a good initialize () discussion, see example 3.15, pp. 95-97,
 * "Java for 3D and VRML Worlds," Lea/Matsuda/Miyashita, New Riders Press, 1996.
 */
public void initialize ()
{
   try { //  catchAllException to diagnose run-time errors while VRML browser continues

	debug ("begin initialize ()");

	quaternion  = new Quaternion ();
	orientation = new SFRotation (0, 0, 0, 0);
	eulers      = new float[3];
	rot         = new float[4];

	traceJava  = (SFBool)   getField ("traceJava");     // initialize
	if     (traceJava.getValue()) setDEBUG (traceJava.getValue());
	debug ("traceJava.getValue() " + traceJava.getValue());

	browser = getBrowser ();
	if (DEBUG) { // avoid expensive browser calls unless needed
		debug ("browser.getName    () = " + browser.getName ());
		debug ("browser.getVersion () = " + browser.getVersion ());
	}
	// ensure dis library visible, otherwise report problem:
	try
	{
		espduClass = Class.forName ("mil.navy.nps.dis.EntityStatePdu");
		debug ("mil.navy.nps.dis.EntityStatePdu class found");
	}
	catch (ClassNotFoundException e)
	{
		trace ("mil.navy.nps.dis.EntityStatePdu class not found");
		trace ("  " + e);
	}
	try
	{
		Class firePduClass = Class.forName ("mil.navy.nps.dis.FirePdu");
		debug ("mil.navy.nps.dis.FirePdu class found");
	}
	catch (ClassNotFoundException e)
	{
		trace ("mil.navy.nps.dis.FirePdu class not found");
		trace ("  " + e);
	}
	// see examples 3.11/3.12 pp. 83-86
	// "Java for 3D and VRML Worlds," Lea/Matsuda/Miyashita

	transformNode = (SFNode) getField ("transformNode"); // instantiate
	transformNodeAccess = (Node) (transformNode.getValue());	// access Transform node

	translation = (SFVec3f) transformNodeAccess.getExposedField ("translation");	// get
	debug ("initial transformNode.translation = " + translation);
	rotation = (SFRotation) transformNodeAccess.getExposedField ("rotation");	// get
	debug ("initial transformNode.rotation    = " + rotation);

	// Do not set eventOuts during initialization!!

	timestamp          = (SFTime)   getEventOut ("timestamp");        // instantiate
	rtpHeaderExpected  = (SFBool)   getField ("rtpHeaderExpected");   // instantiate
	rtpHeaderHeard     = (SFBool)   getEventOut ("rtpHeaderHeard");   // instantiate

	address            = (SFString) getField ("address");             // instantiate
	port               = (SFInt32)  getField ("port");                // instantiate

	marking            = (SFString) getField ("marking");             // instantiate before trace
	espdu.setMarking (marking.getValue());

	debug ("EspduTransform initialize:");
	debug ("marking = " + marking ); // expected marking is prefixed in output statements
	trace ("address, port = " +  address + " " + port);

	multicastRelayHost = (SFString) getField ("multicastRelayHost");  // instantiate
	multicastRelayPort = (SFInt32)  getField ("multicastRelayPort");  // instantiate
		
	debug ("multicastRelayHost, Port = " + multicastRelayHost + " " + multicastRelayPort);
	debug ("rtpHeaderExpected = " + rtpHeaderExpected);

	siteID        = (SFInt32)  getField ("siteID");        // instantiate
	applicationID = (SFInt32)  getField ("applicationID"); // instantiate
	entityID      = (SFInt32)  getField ("entityID");      // instantiate

	espdu.setEntityID ((short)   siteID.getValue(), (short) applicationID.getValue(),
	                   (short) entityID.getValue());

	trace ("siteID, applicationID, entityID = " + siteID + " " + applicationID + " " + entityID);

	active       = (SFBool) getEventOut ("active");       // instantiate
	collided     = (SFBool) getEventOut ("collided");     // instantiate
	detonated    = (SFBool) getEventOut ("detonated");    // instantiate
	fired1       = (SFBool) getEventOut ("fired1");       // instantiate
	fired2       = (SFBool) getEventOut ("fired2");       // instantiate
	collideTime  = (SFTime) getEventOut ("collideTime");  // instantiate
	detonateTime = (SFTime) getEventOut ("detonateTime"); // instantiate
	firedTime    = (SFTime) getEventOut ("firedTime");    // instantiate

	articulationParameterCount   = (SFInt32) getEventOut ("articulationParameterCount");   // instantiate
	articulationParameterValue0  = (SFFloat) getEventOut ("articulationParameterValue0");  // instantiate
	articulationParameterValue1  = (SFFloat) getEventOut ("articulationParameterValue1");  // instantiate
	articulationParameterValue2  = (SFFloat) getEventOut ("articulationParameterValue2");  // instantiate
	articulationParameterValue3  = (SFFloat) getEventOut ("articulationParameterValue3");  // instantiate
	articulationParameterValue4  = (SFFloat) getEventOut ("articulationParameterValue4");  // instantiate
	articulationParameterValue5  = (SFFloat) getEventOut ("articulationParameterValue5");  // instantiate
	articulationParameterValue6  = (SFFloat) getEventOut ("articulationParameterValue6");  // instantiate
	articulationParameterValue7  = (SFFloat) getEventOut ("articulationParameterValue7");  // instantiate
	articulationParameterValue8  = (SFFloat) getEventOut ("articulationParameterValue8");  // instantiate
	articulationParameterValue9  = (SFFloat) getEventOut ("articulationParameterValue9");  // instantiate
	articulationParameterValue10 = (SFFloat) getEventOut ("articulationParameterValue10"); // instantiate
	articulationParameterValue11 = (SFFloat) getEventOut ("articulationParameterValue11"); // instantiate
	articulationParameterValue12 = (SFFloat) getEventOut ("articulationParameterValue12"); // instantiate
	articulationParameterValue13 = (SFFloat) getEventOut ("articulationParameterValue13"); // instantiate
	articulationParameterValue14 = (SFFloat) getEventOut ("articulationParameterValue14"); // instantiate

	// * l000.0 is conversion to msec, then cast float to long
	readInterval  = (long) (((SFTime) getField ("readInterval")).getValue() * 1000.0); // instantiate
	writeInterval = (long) (((SFTime) getField ("writeInterval")).getValue()* 1000.0); // instantiate

	// use half of readInterval (or half of writeInterval) as recommended sleepInterval
	//   for entityDispatcher.
	if      (readInterval > 0l)
	{
		if (writeInterval > 0l)
		{
			trace ("both readInterval & writeInterval > 0, " +
					"resetting writeInterval = 0");
		}
		writeInterval = 0l;  // ensure zero, not negative
		debug ("readInterval = " + readInterval + " msec");
	}
	else if (writeInterval > 0l)
	{
		readInterval = 0l;   // ensure zero, not negative
		espdu.setRtpHeaderEnabled (rtpHeaderExpected.getValue());
		trace ("writeInterval = " + writeInterval + " msec, " + 
			"rtpHeaderExpected = " + rtpHeaderExpected);
	}
	else
	{
		trace ("read & write intervals both <= 0, " +
					"setting readInterval = 1000 msec, writeInterval = 0");
		readInterval  = 1000l;
		writeInterval = 0l;  // ensure zero, not negative
	}

	// from Ownship.java contructor:

//	short entityID [] = {-1, -1, -1};

	// hi-fi vars
	pos	= new float[3]; // x, y, z
	posDt   = new float[3]; // aka: velocity
	ang	= new float[3]; // heading, pitch, roll
	angDt   = new float[3]; // aka: angular rates

	// lo-fi vars
	drPos   = new float[3]; // x, y, z
	drPosDt = new float[3]; // aka: velocity
	drAng   = new float[3]; // heading, pitch, roll
	drAngDt = new float[3]; // aka: angular rates

	// these are used for hpr->axis/angle conversions
	tmpQuat = new Quaternion();
	rot	= new float[4]; // axis/angle

	// now set init vars
	System.arraycopy(initPos, 0, pos, 0, 3);
	System.arraycopy(initPosDt, 0, posDt, 0, 3);
	System.arraycopy(initAng, 0, ang, 0, 3);
	System.arraycopy(initAngDt, 0, angDt, 0, 3);

	// now convert hpr (ang) to axis/angle (rot)
	tmpQuat.setEulers(ang);
	tmpQuat.getAxisAngle(rot);

	// Do not set eventOuts during initialization!!
	debug ("getting LineOfFirePointArray eventOut");
	munitionStartPoint = (SFVec3f) getEventOut ("munitionStartPoint");      // instantiate
	munitionEndPoint   = (SFVec3f) getEventOut ("munitionEndPoint");        // instantiate

	setDoNotSmooth (true); // turn smoothing off while diagnosing position jitter.  unverified!

	debug ("initialize:  entityDispatcher instantiation:");

	if (address.getValue().equalsIgnoreCase  ("unicast") ||
	    address.getValue().equalsIgnoreCase  ("localhost"))
		entityDispatcher = EntityDispatcher.getEntityDispatcher (null, port.getValue());   // unicast
	else
		entityDispatcher = EntityDispatcher.getEntityDispatcher (address.getValue(), port.getValue()); // multicast

// move to EntityDispatcher
//      	entityDispatcher.checkForTrafficAndFallBackToTunnel	(multicastRelayHost.getValue(),
//     	       								 multicastRelayPort.getValue(), 8040);

	debug ("initialize:  entityDispatcher instantiation complete");

	debug ("turn on tracing in entityDispatcher if tracing turned on by VRML scene:  " + traceJava.getValue());
	if (traceJava.getValue()) entityDispatcher.setDEBUG (traceJava.getValue());

	if      (readInterval > 0l)
	{
		debug ("invoke entityDispatcher.adviseSleepInterval (" + readInterval/2 + ")");
		entityDispatcher.adviseSleepInterval (readInterval/2);
	}
	else if (writeInterval > 0l)
	{
		debug ("invoke entityDispatcher.adviseSleepInterval (" + writeInterval/2 + ")");
		entityDispatcher.adviseSleepInterval (writeInterval/2);
	}
	// (guaranteed earlier that either readInterval or writeInterval > 0)

	entityIDObject = new EntityID((short)siteID.getValue(),
		(short)applicationID.getValue(), (short)entityID.getValue());
	debug ("entityIDObject=" + entityIDObject);

	debug ("Register ourselves using entityDispatcher.addListener to receive PDUs");
	entityDispatcher.addListener(this, entityIDObject); // *** this.getEntityIDObject());

//	if (DEBUG) ClassUtilities.showActiveThreads(); // throws exception!!  :(
   }
   catch (Exception catchAllException)
   {
	trace ("initialize () exception: " + catchAllException);
	catchAllException.printStackTrace();
	// can't re-throw (catchAllException); since not supported by class vrml.node.Script
   }

   debug ("initialize () complete");
	
   return;
}

/**
 * Simple method to launch thread because Microsoft puts security restraints on everything.
 * <P>
 * EntityDispatcher is static; only one copy is shared between all instances
 * of the EspduTransform.  If it's null the first time through here, create
 * and thread it.
 * <P>
 * This is synchronized, since we may have N EspduTransforms attempting
 * to instantiate themselves at once, all accessing this shared class
 * variable. The synchronization prevents multiple access.
 * <P>
 * This is actually some magic code. Java keeps several mutex locks around,
 * one for every instance of a class and one for the class itself. We
 * can't sychronize on entityDispatcher because that's null the first
 * time through, which will fail silently. We also can't use a java primitive
 * type. So I created a completely arbitrary instance of an object as
 * a class variable, and synchronize on that. It's a standin for the
 * EntityDispatcher in a way.
 */


public void startEntityDispatcherThread()
{
	debug ("startEntityDispatcherThread() method started...");
   
 	if (entityDispatcherThreadStarted == true)
	{
		trace ("startEntityDispatcherThread: attempting to startEntityDispatcherThread again, ignored...");
		return;
	}
	try
	{
		// crash?? debug ("EspduTransform ThreadGroup = " + Thread.currentThread().getThreadGroup()); // crashes Netscape

		// Thread used to run the entityDispatcher
		debug ("create entityDispatcher thread... *** IE security crash is here ***");
		entityDispatcherThread = new Thread(
					//	Thread.currentThread().getThreadGroup(), // crashes Netscape
						entityDispatcher, 
						"DebuggingEntityDispatcher-" + ClassUtilities.nextSerialNum()
						);
		
		debug ("entityDispatcher thread starting...");
		entityDispatcherThread.start();
	//	trace ("entityDispatcherThread.start(); ThreadGroup = " + entityDispatcherThread.getThreadGroup()); // crashes Netscape

		debug (entityDispatcherThread.getName() + " thread started, startEntityDispatcherThread() complete");
		entityDispatcherThreadStarted = true;
	}
	catch (Exception catchAllException)
	{
		trace ("startEntityDispatcherThread: exception\n" + catchAllException);
		trace ("startEntityDispatcherThread: attempting to continue without entityDispatcher threaded...");
	}
	return;
}

///** Ignore all but latest VRML event so only one set of PDU network reads
// * and one set of dead-reckon calculations occur.
// * This allows frequent updating triggered by the script eventIn.
// */

//	public void processEvents ( int count, Event events[])
//	{
//		debug ("VRML processEvents has " + count + " events queued, process one");
//		processEvent (events[count-1]);
//	}

/**
 * receivePDU () is called by the entity dispatcher when a PDU for the node
 * arrives from the network. The PDU is added to the list of PDUs that have already arrived,
 * and is later sent up to the 3D scene graph when processEvent() is called by the VRML scene.
 */

	public void receivePDU (ProtocolDataUnit pPDU)
	{
		debug ("receivePDU got a PDU from EntityDispatcher");

		if (writeInterval > 0)
		{
			debug ("receivePDU () ignored since we are sending PDUs with this EntityID");
			return;
		}
		synchronized(receivedPDUsList)
		{
			receivedPDUsList.addElement(pPDU);
		}
		return;
	}

/**
	gets the entity ID, which is the unique triplet that identifies the entity, 
	consisting of (site, application, entity) ID numbers. This is set in the VRML node, and
	retrieved for use here.
 */

	public EntityID getEntityIDObject()
	{
		return entityIDObject;
	}

/**
	resets the entity ID triplet = (site, application, entity) ID numbers. 
 */

	public void setEntityIDObject(EntityID pEIO)
	{
		entityIDObject = pEIO;
	}

/**
 * This is the main method, called by the VRML scene with periodicity
 * readInterval (or writeInterval).
 */

public void processEvent (Event event)
{
	debug ("===== VRML processEvent" + event);
	ProtocolDataUnit nextPdu = (ProtocolDataUnit) new EntityStatePdu (); // ProtocolDataUnit is abstract class
	FirePdu          firePdu       = new FirePdu ();
	DetonationPdu    detonationPdu = new DetonationPdu ();
	CollisionPdu     collisionPdu  = new CollisionPdu ();
	float		 x, y, z, roll, pitch, yaw;
	int              espduIndex;
	
	if (PduTrafficLastTime)
	{
		debug  ("Garbage collecting...");
		System.gc ();	// do this first for minimum latency between received PDUs and rendering later
		PduTrafficLastTime = false;
	}

	try { //  catchAllException to diagnose run-time writer errors while VRML browser continues

		// thread-start block - - - - - - - - - - - - - - - - - - - - -

		if (entityDispatcherThreadStarted == false)
		{
			try{
				debug ("initial processEvent:  strategy.invokePrivilege(this, \"startEntityDispatcherThread\");");
				strategy.invokePrivilege(this, "startEntityDispatcherThread");
			}
			catch (Exception catchAllException)
			{
				trace ("initialize:   exception from strategy.invokePrivilege " + catchAllException);
			// 	catchAllException.printStackTrace();
			}
		}
		debug ("entityDispatcherThreadStarted = " + entityDispatcherThreadStarted);
		
		if (entityDispatcherThreadStarted == false)
		{
			// only used if entityDispatcher not threaded (& thus start-ed) !
			entityDispatcher.singleReadLoop ();
		}

		// write block - - - - - - - - - - - - - - - - - - - - - - - -

		if (writeInterval > 0)	//      *** articulation parameters not yet included here....
		{
			debug ("writeInterval=" + writeInterval);
			x = translation.getX ();
			y = translation.getY ();
			z = translation.getZ ();

			espdu.setEntityLocationX (x);
			espdu.setEntityLocationY (y);
			espdu.setEntityLocationZ (z);

			debug ("position = (" + x + ", " + y + ", " + z + ")");

			espdu.setEntityLinearVelocityX (0.0f);
			espdu.setEntityLinearVelocityY (0.0f);
			espdu.setEntityLinearVelocityZ (0.0f);
			espdu.setEntityLinearAcceleration (0.0f, 0.0f, 0.0f);

			orientation = (SFRotation) transformNodeAccess.getExposedField ("rotation");	// get
			debug ("transformNode.rotation = " + orientation);

			orientation.getValue (rot);

			debug ("rotation values = (" + rot[0] + ", " + rot[1] + ", " + 
			                               rot[2] + ", " + rot[3] + ")" );

			quaternion.setAxisAngle (rot);

			debug ("quaternion:");
			if (DEBUG) quaternion.print();

			quaternion.getEulers (eulers);

			// note Kent's quaternion code has irregular ordering of Euler angles
			// which (by whatever method :) accomplishes the angle transformation
			// desired...   (needs to be verified using NPS AUV)

			yaw   = -eulers [0];
			roll  =  eulers [1];
			pitch =  eulers [2];

			// set roll, pitch, and yaw in the PDU

			espdu.setEntityOrientationPsi (roll);
			espdu.setEntityOrientationTheta (pitch);
			espdu.setEntityOrientationPhi (yaw);

			debug  ("phi/theta/psi = " + roll  * 180.0 / Math.PI + ", "
				+ pitch * 180.0 / Math.PI + ", " + yaw   * 180.0 / Math.PI + " degrees");

			debug ("espdu marking = " + espdu.getMarking() ); // expected marking is prefixed in output statements
			debug ("espdu " + espdu.getEntityID().toString());

			// This method is called at writeInterval durations
			// no dead reckoning needed, just use values in the VRML scene
			// we are here in the processEvent method so it is OK to send the PDU

	//		espdu.printValues (2, System.out);  // (indentLevel, printStream)
	//		entityDispatcher.sendPduMulticast (espdu);
			PduTrafficLastTime = true;	// remember so garbage collection gets triggered next time

			entityDispatcher.sendPdu (espdu, address.getValue(), port.getValue());
			debug  ("entityDispatcher.sendPdu (...) complete");
		}
		// read block - - - - - - - - - - - - - - - - - - - - - - - -

		else synchronized (receivedPDUsList)
		{
		
			debug ("process read events, readInterval=" + readInterval);
			/*
			* Since this code is called every time the browser wishes to redraw the
			* screen we can use this as a data point to figure out the time per frame
			*/
			aveSecPerFrame.addDataPoint( secPerFrame.getDuration() );
			secPerFrame.reset();
	
			/*
			* Dead reckon:  calculate VRML position/orientation from DIS position/orientation
			*/
			if  (secSinceLastPdu.getDuration() > 5.0f)      previousEspdu = null;
	
			if ((secSinceLastPdu.getDuration() > 0.01f) && (previousEspdu != null))
			{
				DRandSmooth();
			}

			// put timers on reset events
			
			if ((active.getValue() == true) && (secSinceLastPdu.getDuration() > 15.0f))
			{
				active.setValue (false);
				trace ("no ESPDU heard for 15 seconds, not active!");
			}

	//		if ( collided.getValue ()) collided.setValue (false);	// reset scene

			if (fired1.getValue ())
			  debug ("fired1.getValue ()=" + (new Boolean(fired1.getValue ())).toString() +
				 ", localFireTimer1.getDuration()=" + localFireTimer1.toString());

			if (fired2.getValue ())
			  debug ("fired2.getValue ()=" + (new Boolean(fired2.getValue ())).toString() +
				 ", localFireTimer2.getDuration()=" + localFireTimer2.toString());

			if ((fired1.getValue () == true) && (localFireTimer1.getDuration() >= 5.0f)) // seconds
			{
				trace ("fired1 timeout: " + localFireTimer1.getDuration() + "sec");
				fired1.setValue(false);	// notify scene done (but currently causes another explosion)
				debug ("fired1.setValue(false);");
				munitionStartPoint.setValue (0.0f, 0.0f, 0.0f);
				munitionEndPoint.setValue   (0.0f, 0.0f, 0.0f);
				debug ("munitionStartPoint, munitionEndPoint reset to (0 0 0)");
			}

			if ((fired2.getValue () == true) && (localFireTimer2.getDuration() >= 5.0f)) // seconds
			{
				trace ("fired2 timeout: " + localFireTimer2.getDuration() + "sec");
				fired2.setValue(false);	// notify scene done (but currently causes another explosion)
				debug ("fired2.setValue(false);");
				munitionStartPoint.setValue (0.0f, 0.0f, 0.0f);
				munitionEndPoint.setValue   (0.0f, 0.0f, 0.0f);
				debug ("munitionStartPoint, munitionEndPoint reset to (0 0 0)");
			}

			pduVectorSize = receivedPDUsList.size ();
			pduCount += pduVectorSize;

			if (pduVectorSize < 0)
			{
				trace ("error!  pduVectorSize < 0, received 0 PDUs [" + pduCount + " total]");
				return;
			}
			else if (pduVectorSize == 0)
			{
				debug ("received 0 PDUs [" + pduCount + " total]");
				return;
			}
			else
			{
				debug ("received " + pduVectorSize + " PDUs [" + pduCount + " total]");
				PduTrafficLastTime = true;	// remember so garbage collection gets triggered next time
			}

			// process all received PDUs

			// find the most recently timestamped ESPDU, process Collision and Fire
			espduIndex = -1;
			for (int i = 0; i < pduVectorSize; i++)
			{
				nextPdu = (ProtocolDataUnit) receivedPDUsList.elementAt (i);
// debug (" :)  got nextPdu");
// debug (" ((ProtocolDataUnit) receivedPDUsList.elementAt(i)).getPduType()=" + ((ProtocolDataUnit) receivedPDUsList.elementAt (i)).getPduType());
// debug (" ((ProtocolDataUnit) receivedPDUsList.elementAt(i)).getPduType().shortValue()=" + ((ProtocolDataUnit) receivedPDUsList.elementAt (i)).getPduType().shortValue());

				if (nextPdu.getRtpHeaderEnabled() != rtpHeaderExpected.getValue())
				{
					debug ("RTP headers " + nextPdu.getRtpHeaderEnabled() + " rather than " +
						rtpHeaderExpected + " as expected!");
					rtpHeaderHeard.setValue(nextPdu.getRtpHeaderEnabled());    // send eventOut
					rtpHeaderExpected.setValue(nextPdu.getRtpHeaderEnabled()); // remember
					debug ("unexpected RTP header status, rtpHeader = " + nextPdu.getRtpHeaderEnabled());
				}
				if (nextPdu.getPduType().shortValue() == PduTypeField.ENTITYSTATE)
				{
					if     (espduIndex == -1) // first espdu found
					{
						espduIndex = i;
						pdu = (ProtocolDataUnit) receivedPDUsList.elementAt (i);
						debug ("-- ESPDU of interest is now element " + i + " of receivedPDUsList");
					}
					else if (pdu.getTimestamp().longValue() < nextPdu.getTimestamp().longValue())
					{
						espduIndex = i;
						pdu = (ProtocolDataUnit) receivedPDUsList.elementAt (i);
						debug ("-- ESPDU of interest is now element " + i + " of receivedPDUsList");
					}
					else    debug ("-- ESPDU at element " + i + " of receivedPDUsList is less recent than pdu, ignored");
				}
				else if (nextPdu.getPduType().shortValue() == PduTypeField.FIRE) // ** need intersection check? **
				{
					debug (">> FirePdu detected...");
					firePdu = (FirePdu) receivedPDUsList.elementAt (i);
					// make decision based on munition, fired1 or fired2 ***
					//*******Quay's decision code**************
					if ((int)WarheadField.HEDARTS == firePdu.getBurstDescriptor().getWarhead().intValue())
                                        {
						trace ("firePdu: fired2, found WarheadField.HEDARTS == " + WarheadField.HEDARTS);
						fired2.setValue(true);
						localFireTimer2.reset();
					}
                                        else{
						fired1.setValue(true);
						localFireTimer1.reset();
					}				// notify scene
                                        //*******ends Quay's changes*******
					// notify scene of time, but use local clock so explosion sounds
					firedTime.setValue (firePdu.getVRMLTimestamp()); // event.getTimeStamp() didn't work :(
					float fireRange = firePdu.getRange();
					if (fireRange <= 10.0f)
					{
						debug ("fireRange = " + fireRange + ", reset to 10m for visibility");
						fireRange = 10.0f; // meters
					}
					trace ("fired!  fireRange=" + fireRange);
					debug ("firedTime=" + firedTime.getValue());

					// include conversion from DIS-to-VRML world coordinates (VRML Y gets DIS -Z, VRML Z gets DIS Y)
					// need to add time of flight & munition handling to these calculations
					
					float  launchX, launchY, launchZ, launchVelocityX, launchVelocityY, launchVelocityZ,
					       targetX, targetY, targetZ = 0.0f;
					if (firePdu.getLocationInWorldCoordinate() != null)
					{
						launchX =  (new Double ((firePdu.getLocationInWorldCoordinate()).getX()).floatValue());      // VRML coordinate system
						launchY = -(new Double ((firePdu.getLocationInWorldCoordinate()).getZ()).floatValue());
						launchZ =  (new Double ((firePdu.getLocationInWorldCoordinate()).getY()).floatValue());
						
						// need proper calculations for actual aim point ***
 						// include conversion from DIS-to-VRML world coordinates (VRML Y gets DIS -Z, VRML Z gets DIS Y)
						launchVelocityX =   firePdu.getVelocity().getX();      // VRML coordinate system
 						launchVelocityY = - firePdu.getVelocity().getZ();
 						launchVelocityZ =   firePdu.getVelocity().getY();
						double horizontalVelocity = Math.sqrt (launchVelocityX*launchVelocityX + launchVelocityZ*launchVelocityZ);
						if (horizontalVelocity == 0.0) horizontalVelocity = 0.0001; // avoid divide by zero exception
 						double psi   = Math.atan2(launchVelocityX, launchVelocityZ);     // VRML coordinate system, horizontal plane
 						double theta = Math.atan (launchVelocityY / horizontalVelocity); // VRML coordinate system, vertical plane
 						debug ("(VRML launchVelocityX=" + launchVelocityX + ", launchVelocityY=" + launchVelocityY + ")");
  						debug (" launchVelocityZ=" + launchVelocityZ + ", theta =" + theta * Math.PI / 180.0 + " degrees, psi = "
  							 + psi * Math.PI / 180.0 + " degrees)");
 
 						targetX =  launchX + (float) (fireRange * Math.cos (theta) * Math.sin(psi));
 						targetY =  launchY + (float) (fireRange * Math.sin (theta));
 						targetZ =  launchZ + (float) (fireRange * Math.cos (theta) * Math.cos(psi));
						debug ("calculations complete, setting (crash-prone) munitionStartPoint, munitionEndPoint ...");
						munitionStartPoint.setValue (launchX, launchY, launchZ);	// crash!!
						munitionEndPoint.setValue (targetX, targetY, targetZ);	// crash!!
						debug ("munitionStartPoint set to (" + launchX + ", " + launchY + ", " + launchZ + ")");
						debug ("  munitionEndPoint set to (" + targetX + ", " + targetY + ", " + targetZ + ")");
					}
					else trace ("*** error, firePdu.getLocationInWorldCoordinate() == null");
				}
				else if (nextPdu.getPduType().shortValue() == PduTypeField.DETONATION)
				{
					trace ("<< DetonationPdu detected...");
					detonationPdu = (DetonationPdu) receivedPDUsList.elementAt (i);
					detonated.setValue (true);     	// notify scene
					detonateTime.setValue (new SFTime (detonationPdu.getVRMLTimestamp()));	// notify scene
					trace ("detonated!  timeStamp " + detonateTime.getValue());
				}
				else if (nextPdu.getPduType().shortValue() == PduTypeField.COLLISION)
				{
					trace ("<< CollisionPdu detected...");
					collisionPdu = (CollisionPdu) receivedPDUsList.elementAt (i);
					collided.setValue (true);     	// notify scene
					collideTime.setValue (new SFTime (collisionPdu.getVRMLTimestamp()));	// notify scene
					trace ("collided!  timeStamp " + collideTime.getValue());
				}
				else // unusable PDU type
				{
					trace ("not a usable ESPDU, element " + i + " of receivedPDUsList is type " +
						nextPdu.getPduType() + " = " +
						PduTypeField.toString (nextPdu.getPduType().intValue()));
				     //	trace ("see dis-java-vrml/docs/dis-java-vrml/mil/navy/nps/disEnumerations/PduTypeField.html");
				}
			}
			if (espduIndex == -1) // no ESPDUs received
			{
				debug ("have no ESPDUs");
			}
			else // process the most current ESPDU which is 'pdu'
			{
				debug ("have one or more ESPDUs");
				if (active.getValue() == false)
				{
					active.setValue (true);
					trace ("espdu(s) received - active!  RTP headers: " + nextPdu.getRtpHeaderEnabled());
				//	DEBUG=true;
				}

				prev_dt = secSinceLastPdu.getDuration() - secPerFrame.getDuration();
				// allow smoothing until the DR and smoothing results are closeEnough.
				closeEnough = false;
				// this is how long the espdu has been used (i.e. time between pdu updates)
				averageUpdateTime.addDataPoint( secSinceLastPdu.getDuration() );

				if (pdu == null) trace ("pdu lost scope!!");
				
				espdu = (EntityStatePdu) pdu;

				timestamp.setValue(new SFTime (espdu.getVRMLTimestamp()));  // send eventOut

				// save the utilized previousEspdu for smoothing.
				// need partial deep copy to get DR values and eliminate multiple reference count
				previousEspdu = new EntityStatePdu ();
				previousEspdu.setDeadReckoningAlgorithm   (espdu.getDeadReckoningAlgorithm ());
				previousEspdu.setDeadReckoningParameters  (espdu.getDeadReckoningParameters ());
				previousEspdu.setEntityAngularVelocity    (espdu.getEntityAngularVelocity ());
				previousEspdu.setEntityLinearAcceleration (espdu.getEntityLinearAcceleration ());
				previousEspdu.setEntityLinearVelocity     (espdu.getEntityLinearVelocity ());
				previousEspdu.setEntityLocation           (espdu.getEntityLocation ());
				previousEspdu.setEntityOrientation        (espdu.getEntityOrientation ());

				secSinceLastPdu.reset(); // reset local dead-reckoning clock since new pdu is now in use

				// browser bug trap, espdu location at origin is unlikely
				if ((espdu.getEntityLocationX() == 0.0) &&
				    (espdu.getEntityLocationY() == 0.0) &&
				    (espdu.getEntityLocationZ() == 0.0))
				     trace ("warning! suspicious ESPDU location <0, 0, 0> received");
				else debug ("ESPDU-at-origin bug trap complete.");

				int APCount = espdu.articulationParameterCount().intValue();
				if (APCount != articulationParameterCount.getValue())
					articulationParameterCount.setValue(APCount);  // send eventOut
				if (APCount > MAX_ARTICULATION_PARAMETERS)
				{
					trace (	APCount +
						" articulation parameters received, maximum supported is " +
						MAX_ARTICULATION_PARAMETERS);
				}
	if (APCount > 0)  changeIndicator0 =  espdu.getArticulationParameterAt(0).getChangeIndicator().intValue();
	if (APCount > 1)  changeIndicator1 =  espdu.getArticulationParameterAt(1).getChangeIndicator().intValue();
	if (APCount > 2)  changeIndicator2 =  espdu.getArticulationParameterAt(2).getChangeIndicator().intValue();
	if (APCount > 3)  changeIndicator3 =  espdu.getArticulationParameterAt(3).getChangeIndicator().intValue();
	if (APCount > 4)  changeIndicator4 =  espdu.getArticulationParameterAt(4).getChangeIndicator().intValue();
	if (APCount > 5)  changeIndicator5 =  espdu.getArticulationParameterAt(5).getChangeIndicator().intValue();
	if (APCount > 6)  changeIndicator6 =  espdu.getArticulationParameterAt(6).getChangeIndicator().intValue();
	if (APCount > 7)  changeIndicator7 =  espdu.getArticulationParameterAt(7).getChangeIndicator().intValue();
	if (APCount > 8)  changeIndicator8 =  espdu.getArticulationParameterAt(8).getChangeIndicator().intValue();
	if (APCount > 9)  changeIndicator9 =  espdu.getArticulationParameterAt(9).getChangeIndicator().intValue();
	if (APCount >10) changeIndicator10 = espdu.getArticulationParameterAt(10).getChangeIndicator().intValue();
	if (APCount >11) changeIndicator11 = espdu.getArticulationParameterAt(11).getChangeIndicator().intValue();
	if (APCount >12) changeIndicator12 = espdu.getArticulationParameterAt(12).getChangeIndicator().intValue();
	if (APCount >13) changeIndicator13 = espdu.getArticulationParameterAt(13).getChangeIndicator().intValue();
	if (APCount >14) changeIndicator14 = espdu.getArticulationParameterAt(14).getChangeIndicator().intValue();

				// for articulationParameterValues, note that doubles are passed over the wire,
				//     but they must be downcasted to floats so VRML can accept them :(
				if (APCount > 0)
				{			
	// the following if statements check for validity and then whether a change has occurred from prior value,
	//   prior to accessing ArticulationParameter member values and then sending (via assignment) eventOuts
	if ((APCount > 0) && ( changeIndicator0 !=  previousChangeIndicator0))  articulationParameterValue0.setValue  ((float)espdu.getArticulationParameterAt(0).getParameterValue());
	if ((APCount > 1) && ( changeIndicator1 !=  previousChangeIndicator1))  articulationParameterValue1.setValue  ((float)espdu.getArticulationParameterAt(1).getParameterValue());
	if ((APCount > 2) && ( changeIndicator2 !=  previousChangeIndicator2))  articulationParameterValue2.setValue  ((float)espdu.getArticulationParameterAt(2).getParameterValue());
	if ((APCount > 3) && ( changeIndicator3 !=  previousChangeIndicator3))  articulationParameterValue3.setValue  ((float)espdu.getArticulationParameterAt(3).getParameterValue());
	if ((APCount > 4) && ( changeIndicator4 !=  previousChangeIndicator4))  articulationParameterValue4.setValue  ((float)espdu.getArticulationParameterAt(4).getParameterValue());
	if ((APCount > 5) && ( changeIndicator5 !=  previousChangeIndicator5))  articulationParameterValue5.setValue  ((float)espdu.getArticulationParameterAt(5).getParameterValue());
	if ((APCount > 6) && ( changeIndicator6 !=  previousChangeIndicator6))  articulationParameterValue6.setValue  ((float)espdu.getArticulationParameterAt(6).getParameterValue());
	if ((APCount > 7) && ( changeIndicator7 !=  previousChangeIndicator7))  articulationParameterValue7.setValue  ((float)espdu.getArticulationParameterAt(7).getParameterValue());
	if ((APCount > 8) && ( changeIndicator8 !=  previousChangeIndicator8))  articulationParameterValue8.setValue  ((float)espdu.getArticulationParameterAt(8).getParameterValue());
	if ((APCount > 9) && ( changeIndicator9 !=  previousChangeIndicator9))  articulationParameterValue9.setValue  ((float)espdu.getArticulationParameterAt(9).getParameterValue());
	if ((APCount >10) && (changeIndicator10 != previousChangeIndicator10)) articulationParameterValue10.setValue ((float)espdu.getArticulationParameterAt(10).getParameterValue());
	if ((APCount >11) && (changeIndicator11 != previousChangeIndicator11)) articulationParameterValue11.setValue ((float)espdu.getArticulationParameterAt(11).getParameterValue());
	if ((APCount >12) && (changeIndicator12 != previousChangeIndicator12)) articulationParameterValue12.setValue ((float)espdu.getArticulationParameterAt(12).getParameterValue());
	if ((APCount >13) && (changeIndicator13 != previousChangeIndicator13)) articulationParameterValue13.setValue ((float)espdu.getArticulationParameterAt(13).getParameterValue());
	if ((APCount >14) && (changeIndicator13 != previousChangeIndicator14)) articulationParameterValue14.setValue ((float)espdu.getArticulationParameterAt(14).getParameterValue());
				}
				 previousChangeIndicator0 =  changeIndicator0;
				 previousChangeIndicator1 =  changeIndicator1;
				 previousChangeIndicator2 =  changeIndicator2;
				 previousChangeIndicator3 =  changeIndicator3;
				 previousChangeIndicator4 =  changeIndicator4;
				 previousChangeIndicator5 =  changeIndicator5;
				 previousChangeIndicator6 =  changeIndicator6;
				 previousChangeIndicator7 =  changeIndicator7;
				 previousChangeIndicator8 =  changeIndicator8;
				 previousChangeIndicator9 =  changeIndicator9;
				previousChangeIndicator10 = changeIndicator10;
				previousChangeIndicator11 = changeIndicator11;
				previousChangeIndicator12 = changeIndicator12;
				previousChangeIndicator13 = changeIndicator13;
				previousChangeIndicator14 = changeIndicator14;

				if (espduFirstHeard == true)
				{
					espduFirstHeard = false;
					trace ("espduFirstHeard, marking = " + espdu.getMarking ());
				}
		// crash!!	if ((espduFirstHeard == true) && espdu.getMarking().compareToIgnoreCase (marking.getValue()) != 0)
		//		{
		//			trace ("warning!  VRML scene marking (" + marking + ") differs from ESPDU marking (" + ")" );
		//			espduFirstHeard = false;
		//		}
			}

			debug ("starting receivedPDUsList.removeAllElements ()...");
			receivedPDUsList.removeAllElements ();  // garbage collection will reclaim this Vector
			debug ("finished receivedPDUsList.removeAllElements ()");
			pdu = null; nextPdu = null; firePdu = null; collisionPdu = null;  // dereference list PDUs
			debug ("receivedPDUsList.removeAllElements (); isEmpty() = " + receivedPDUsList.isEmpty ());
			if (!receivedPDUsList.isEmpty ()) trace ("receivedPDUsList.removeAllElements (); failed!");

		} // end synchronized (receivedPDUsList)
	}
	catch (Exception catchAllException)
	{
		trace ("*** processEvent exception: " + catchAllException);
		catchAllException.printStackTrace();
//		setDEBUG (true);
		// can't re-throw (catchAllException); since not supported by class vrml.node.Script
	}

	debug ("finished processEvent");

	return;
}

/****************************************************
 *                                                   *
 *   functions added for smoothing by Scott Heller   *
 *                                                   *
 ****************************************************/

	private float[] smooth3Floats(	float[] drCurrent, float[] drPrevUpdate,
					float currentTime, float   averageUpdateTime )
	{
		float[] result = new float[3];

		// the ratio of how long since the most recent update time to the averageUpdateTime
		float factor = currentTime / averageUpdateTime;

		result[0] = drPrevUpdate[0] + ( drCurrent[0] - drPrevUpdate[0] ) * factor;
		result[1] = drPrevUpdate[1] + ( drCurrent[1] - drPrevUpdate[1] ) * factor;
		result[2] = drPrevUpdate[2] + ( drCurrent[2] - drPrevUpdate[2] ) * factor;

		return result;
	}

/**
 * Calculate the deadreckon position of a EntityStatePDU
 */
	private float[] DRPosition ( EntityStatePdu espdu, float dt )
	{
		float[] dRposition = new float[3];

		if( dt < DR_LIMIT )
		{
			dRposition[0] = (float) ( espdu.getEntityLocationX()  +
				dt    * espdu.getEntityLinearVelocityX()      +
				dt*dt * espdu.getEntityLinearAccelerationX());
			dRposition[1] = (float) (-espdu.getEntityLocationZ()  -
				dt    * espdu.getEntityLinearVelocityZ()      -
				dt*dt * espdu.getEntityLinearAccelerationZ());
			dRposition[2] = (float) ( espdu.getEntityLocationY()  +
				dt    * espdu.getEntityLinearVelocityY()  +
				dt*dt * espdu.getEntityLinearAccelerationY());
		} else
		{
			dRposition[0] = (float) ( espdu.getEntityLocationX());
			dRposition[1] = (float) (-espdu.getEntityLocationZ());
			dRposition[2] = (float) ( espdu.getEntityLocationY());
		}
		return dRposition;
	}

/*
 * Calculate the deadreckon orientation of a EntityStatePDU
 */
	private float[] DROrientation ( EntityStatePdu espdu, float dt )
	{
		float[] dRorientation = new float[3];
		float   yaw, pitch, roll;

		if ( dt < DR_LIMIT ) // no angular acceleration in ESPDU
		{
			roll  = espdu.getEntityOrientationPhi()   + dt * espdu.getEntityAngularVelocityX();
			pitch = espdu.getEntityOrientationTheta() + dt * espdu.getEntityAngularVelocityY();
			yaw   = espdu.getEntityOrientationPsi()   + dt * espdu.getEntityAngularVelocityZ();

		} else
		{
			roll   = espdu.getEntityOrientationPhi();
			pitch = espdu.getEntityOrientationTheta();
			yaw   = espdu.getEntityOrientationPsi();
		}

		// note Kent's quaternion code has irregular ordering of Euler angles
		// which (by whatever method :) accomplishes the angle transformation
		// desired...   (results verified using NPS AUV)

		dRorientation[0] = -yaw;
		dRorientation[1] =  roll;
		dRorientation[2] =  pitch;

		return dRorientation;

	}// end DROrientation()



/**
 * Sum of the squares of the diffs between two float arrays.
 */
	private float SqrDeltaFloats( float[] first, float[] second )
	{
		float sumOfSqrs = 0.0f;

		for( int idx = 0; idx < first.length; idx++ )
		{
			sumOfSqrs += (first[idx] - second[idx]) * (first[idx] - second[idx]);
		}

		return sumOfSqrs;
	}

/**
 * Dead reckon and smooth if appropriate.
 */
	private void DRandSmooth()
	{
		if (espdu == null) // trap error condition
		{
			trace ("DRandSmooth: espdu == null !");
			return;
		}
		if (previousEspdu == null) // trap error condition
		{
			trace ("DRandSmooth: previousEspdu == null !");
			return;
		}
		
		float aveUpdateTime 	= averageUpdateTime.average();
		float stopSmoothTime 	= aveUpdateTime * SMOOTH_LIMIT;
		float updateTime 	= secSinceLastPdu.getDuration();

		// calculate the DR Position and Orientation
		float[] tempPositionArray = DRPosition( espdu, updateTime );
		float[] goalOrientation   = DROrientation( espdu, updateTime );
		float[] currOrientation; // only used when smoothing.
		float[] smoothedLinearVel;

		if( DoNotSmooth || closeEnough ||  updateTime > aveUpdateTime )
		{	// just DR since we have already rejoined the new DR track.

			positionArray = tempPositionArray;
			eulers = goalOrientation;
		} else
		{
			// DR and smooth position
			if( SMOOTH_LINVELOCITY )
			{
				smoothedLinearVel = smoothLinVelocity (espdu, previousEspdu, updateTime, aveUpdateTime);
				tempPositionArray = DRPosition (espdu, smoothedLinearVel, updateTime);
				positionArray = smooth3Floats (tempPositionArray,
 				DRPosition (previousEspdu,smoothedLinearVel, prev_dt + updateTime), 
						updateTime, aveUpdateTime * SMOOTH_LIMIT);
			}
			else
			{
				positionArray = smooth3Floats (tempPositionArray,
							DRPosition (previousEspdu, prev_dt + updateTime), 
						 	updateTime, aveUpdateTime * SMOOTH_LIMIT);
			}
			currOrientation = DROrientation (previousEspdu, prev_dt + updateTime);

			goalOrientation = fixEulers( goalOrientation , currOrientation );
			// goalOrientation should now contain values such that the entity will
			//   head the short way around the circle.
																	
			eulers = smooth3Floats( goalOrientation, currOrientation,
						updateTime, aveUpdateTime * SMOOTH_LIMIT );

			if(	SqrDeltaFloats (tempPositionArray, positionArray ) < 0.02 &&
				SqrDeltaFloats (tempPositionArray, positionArray ) < 0.02 &&
				SqrDeltaFloats (goalOrientation, currOrientation ) < 0.02  )
			{
				// stop smoothing. Only use DR calculations until next pdu arrives.
				closeEnough = true;
			}

		}

		position    = new SFVec3f (positionArray[0], positionArray[1], positionArray[2]);
		debug("position = (" + positionArray[0] + ", " + positionArray[1] + ", " + positionArray[2] + ")");

		quaternion.setEulers (eulers);

		quaternion.getAxisAngle (rot);
		orientation.setValue 	(rot);
		debug("rotation values = ("	+ rot[0] + ", " + rot[1] + ", "
			+ rot[2] + ", " + rot[3] + ")" );

		translation.setValue (position);  // set VRML scene value
		rotation.setValue (orientation);  // set VRML scene value

	} // END: Dead reckon and smooth

	private float normalize(float input_angle)
	{
		float angle = input_angle;
		float twoPI = (float)Math.PI * 2.0f;

		while( angle < 0.0f )
		{
			angle += twoPI;
		}
		while( angle >= twoPI )
		{
			angle -= twoPI;
		}
		return angle;

	}// end normalize

	private float[] normalize( float[] inputAngles )
	{
		float[] normal = new float[inputAngles.length];
		for( int idx = 0; idx < inputAngles.length; idx++ )
		{
			normal[idx] = normalize2( inputAngles[idx] );
		}

		return normal;
	} //end normalize float array => ( 0, 2PI ]

	private float[] normalize2( float[] inputAngles )
	{
		float[] normal = new float[inputAngles.length];
		for( int idx = 0; idx < inputAngles.length; idx++ )
		{
			normal[idx] = normalize2( inputAngles[idx] );
		}

		return normal;
	} //end normalize2 float array => ( -PI, PI ]

	private float normalize2( float input_angle )
	{
		float angle = input_angle;
		float twoPI = (float)Math.PI * 2.0f;

		while( angle > Math.PI )
		{
			angle -= twoPI;
		}
		while(angle <= -Math.PI )
		{
			angle += twoPI;
		}
		return angle;
	}// end normalize2 => ( -PI, PI ]

	// returns: the magnitude and direction of the angle change.
	private float[] fixEulers( float[] goalOrientation, float[] currOrientation )
	{
		float[] goalOrien = goalOrientation;

		for( int idx = 0; idx < goalOrientation.length; idx++ )
		{
			goalOrien[idx] = currOrientation[idx] +
				normalize2( normalize2( goalOrien[idx] ) - normalize2( currOrientation[idx] ) );
		}
		return goalOrien;
	}

/**
 * Calculate the dead reckoning position of a EntityStatePDU
 */
	private float[] DRPosition ( EntityStatePdu espdu, float[] smoothedVelocity, float dt )
	{
		float[] dRposition = new float[3];

		if( dt < DR_LIMIT )
		{
			dRposition[0] = (float) ( espdu.getEntityLocationX()  + dt * smoothedVelocity[0]);
			dRposition[1] = (float) (-espdu.getEntityLocationZ()  - dt * smoothedVelocity[1]);
			dRposition[2] = (float) ( espdu.getEntityLocationY()  + dt * smoothedVelocity[2]);

		}
		else
		{
			dRposition[0] = (float) ( espdu.getEntityLocationX());
			dRposition[1] = (float) (-espdu.getEntityLocationZ());
			dRposition[2] = (float) ( espdu.getEntityLocationY());
		}

		return dRposition;

	}

	private float[] smoothLinVelocity( EntityStatePdu espdu, EntityStatePdu previousEspdu,
					float currentTime, float   averageUpdateTime  )
	{
		float[] ELV 		= new float[3];
		float[] prevELV 	= new float[3];

		ELV[0] 		= espdu.getEntityLinearVelocityX();
		ELV[1] 		= espdu.getEntityLinearVelocityZ();
		ELV[2] 		= espdu.getEntityLinearVelocityY();
		prevELV[0] 	= previousEspdu.getEntityLinearVelocityX();
		prevELV[1] 	= previousEspdu.getEntityLinearVelocityZ();
		prevELV[2] 	= previousEspdu.getEntityLinearVelocityY();

		return smooth3Floats( ELV, prevELV, currentTime, averageUpdateTime );

	}// end smoothVelocity

	private float[] smoothAngVelocity( EntityStatePdu espdu, EntityStatePdu previousEspdu,
						float currentTime, float   averageUpdateTime  )
	{
		float[] EAV 		= new float[3];
		float[] prevEAV 	= new float[3];

		EAV[0] 		= espdu.getEntityAngularVelocityX();
		EAV[1] 		= espdu.getEntityAngularVelocityY();
		EAV[2] 		= espdu.getEntityAngularVelocityZ();
		prevEAV[0] 	= previousEspdu.getEntityAngularVelocityX();
		prevEAV[1] 	= previousEspdu.getEntityAngularVelocityY();
		prevEAV[2] 	= previousEspdu.getEntityAngularVelocityZ();

		return smooth3Floats( EAV, prevEAV, currentTime, averageUpdateTime );

	} // end smoothVelocity

/**
 * shutdown() is called when VRML scene exits.
 */

public void shutdown () {

	try { //  catchAllException to diagnose run-time errors while browser continues

		if (entityDispatcher != null) entityDispatcher.shutdown ();
		else trace ("entityDispatcher is null, no shutdown ()");
	}
	catch (Exception catchAllException)
	{
		trace ("entityDispatcher.shutdown () exception: " + catchAllException);
	// can't re-throw (catchAllException); since not supported by class vrml.node.Script
	}

	trace ("shutdown () complete\n"); // easier to find trace restarts in console
}

/**
 * main method was originally intended to be used for debugging, to determine if EntityDispatcher
 * really is sending things to us.
 * <P>
 * Sigh. We can't actually use main(), since that's declared in Script, and
 * we can't override static methods. So as an alternative we rename this
 * something else and then call it from another class that can have a
 * main method.  Reverify that this block of code is correct if you do resurrect it.
 */

	public static void main(String args[])
	{
		EspduTransform transformTester = new EspduTransform();
		EntityID       eio;

		if (entityDispatcher == null)
		{
			entityDispatcher = EntityDispatcher.getEntityDispatcher ("224.2.181.145", 62040); // multicast
			
			// Thread used to run the entityDispatcher
			Thread aThread = new Thread(entityDispatcher, "DebuggingEntityDispatcher-" + ClassUtilities.nextSerialNum());
			// can't use trace() here since not static
			System.out.println ("EspduTransform main:  entityDispatcher thread starting...");
			aThread.start();
			System.out.println ("EspduTransform main:  " + aThread.getName() + " entityDispatcher thread started...");
		}

		// DIS entityID object, which has a few more smarts.
		eio = new EntityID(0,1,2);
		transformTester.setEntityIDObject(eio);

		// Add ourselves to the Hashtable of espduTransforms that can receive PDUs.
		//entityDispatcher.addEspduTransform(transformTester);
	} // end of main
}

