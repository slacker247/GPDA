/*
 File:		RadioCommunicationsPduScriptNode.java
 CVS Info:	$Id: RadioCommunicationsPduScriptNode.java,v 1.3 1998/02/15 15:29:20 brutzman Exp $
 Compiler:	jdk 1.3
 */

package mil.navy.nps.dis;

import java.util.*;
import java.io.*;

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
 * This Java class provides the communications interface between the radio-communications Script node
 * (in the VRML scene) and the DIS ReceiverPdu, SignalPdu and TransmitterPdu class libraries.
 *
 *@version 1.0
 *@author <a href="mailto:brutzman@nps.navy.mil">Don Brutzman</a> (<a href="http://web.nps.navy.mil/~brutzman">http://web.nps.navy.mil/~brutzman</a>)
 *@author <a href="mailto:dave@laflam.net">David W. Laflam</a> (<a href="http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/">http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/</a>)
 *
 *<dt><b>Location:</b>
 *<dd><a href="../../../../../../mil/navy/nps/dis/RadioCommunicationsPduScriptNode.java">RadioCommunicationsPduScriptNode.java</A>
 *<dd><a href="http://web.nps.navy.mil/~brutzman/vrtp/mil/navy/nps/dis/RadioCommunicationsPduScriptNode.java">web.nps.navy.mil/~brutzman/vrtp/mil/navy/nps/dis/RadioCommunicationsPduScriptNode.java</a>
 *<dd><a href="http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/dis/EspduTransform.java">www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/dis/EspduTransform.java</a>
 *
 *<dt><b>Hierarchy Diagram:</b>
 *<dd><a href="../../../../../../dis-java-vrml/images/PduClassHierarchy.jpg"><IMG SRC="../../../../../../dis-java-vrml/images/PduClassHierarchyButton.jpg" ALIGN=ABSCENTER WIDTH=150 HEIGHT=21></a>
 *
 *<dt><b>History:</b>
 *<TABLE>
 *<tr>
 *	<td>	14 September 2000
 *	<td>	Don Brutzman, Dave Laflam
 *	<td>	New, patterned after EspduTransform
 *</TABLE>
 *<P>
 *
 *<dt><b>Associated VRML/X3D files:</b>
 *<dd>	<a href="../../../../../../demo/helicopter/RAUAntennaPROTO.xml">RAUAntennaPROTO.xml</A>,
 *	<a href="../../../../../../demo/helicopter/RAUAntennaPROTO.wrl">RAUAntennaPROTO.wrl</A>
 *(PROTO definition)
 *<dd>	<a href="../../../../../../demo/helicopter/SHFAntennaPROTO.xml">SHFAntennaPROTO.xml</A>,
 *	<a href="../../../../../../demo/helicopter/SHFAntennaPROTO.wrl">SHFAntennaPROTO.wrl</A>
 *(PROTO definition)
 *<dd>	<a href="../../../../../../demo/helicopter/UHFAntennaPROTO.xml">UHFAntennaPROTO.xml</A>,
 *	<a href="../../../../../../demo/helicopter/UHFAntennaPROTO.wrl">UHFAntennaPROTO.wrl</A>
 *(PROTO definition)
 *<dd>	<a href="../../../../../../demo/helicopter/AntennaWorldFortIrwin.xml">AntennaWorldFortIrwin.xml</A>,
 *	<a href="../../../../../../demo/helicopter/AntennaWorldFortIrwin.wrl">AntennaWorldFortIrwin.wrl</A>
 *(Example use)
 *
 *<P>
 *
 *<dt><b>Debugging notes:</b>
 *<dd>	System.out.println text appears in the Netscape Java console when using CosmoPlayer
 *
 *@see EspduTransform
 *@see EntityDispatcher
 *@see RadioCommunicationsFamily
 *@see ReceiverPdu
 *@see SignalPdu
 *@see TransmitterPdu
 */


public class RadioCommunicationsPduScriptNode extends Script implements PduSubscriber, NetworkCommBadge // ---------------
{
/**
 * Declare eventIns, fields and eventOuts from VRML scene
 */

/**
 * eventOut: timestamp
 */

protected SFTime   timestamp;

/**
 * field, how often to look for DIS PDUs. Units are seconds in VRML, converted to msec in Java in initialize ().
 *  readInterval=0 means no reading.  Longs are used because VRML SFFloat units are seconds.
 */

	protected long readInterval;

/**
 * field, how often to write DIS PDUs. Units are seconds in VRML, converted to msec in Java in initialize ().
 *  writeInterval=0 means no writing (which is the default).  Longs are used because VRML SFFloat units are seconds.
 */

	protected long writeInterval;

/**
 * field, multicast address, or "unicast"
 */

	protected SFString	address;

/**
 * field, port to listen on
 */

	protected SFInt32		port;

/**
 * field, relay host name or IP address (used only if no multicast heard)
 */

	protected SFString	multicastRelayHost;

/**
 * field, relay host port to connect to (used only if no multicast heard)
 */

	protected SFInt32		multicastRelayPort;

/**
 * field, are RTP headers expected?  Only set by VRML scene at initialization (because it is a field)
 * but can be reset at runtime if the opposite situation is encountered.
 */

	protected SFBool		rtpHeaderExpected;

/**
 * eventOut, are RTP headers being heard on the wire?
 */

	protected SFBool		rtpHeaderHeard = new SFBool (false);

/**
 * field, entityID triplet: site ID
 */

protected SFInt32   siteID;

/**
 * field, entityID triplet: unique application ID at that site
 */

protected SFInt32   applicationID;

/**
 * field, entity ID triplet: ID within that app
 */

protected SFInt32   entityID;

/**
 *  VRML scene field to specify which RadioCommunicationsFamily PDU is being utilized:  <b>ReceiverPdu</b>, <b>SignalPdu</b> or <b>TransmitterPdu</b>.
 */
	protected SFString	radioPduType;

	private boolean     	radioPduTypeValid; // checked at instantiation

/**
 * VRML scene eventOut for {@link RadioCommunicationsFamily#radioID}
 */
	protected SFInt32		radioID;

/**
 *  VRML scene eventOut for {@link SignalPdu#encodingScheme}
 */
	protected SFInt32 	encodingScheme;

/**
 *  VRML scene eventOut for {@link SignalPdu#tdlType}
 */
	protected SFInt32 	tdlType;

/**
 *  VRML scene eventOut for {@link SignalPdu#sampleRate}
 */
	protected SFInt32 	sampleRate;

/**
 *  VRML scene eventOut for {@link SignalPdu#samples}
 */
	protected SFInt32 	samples;

/**
 *  VRML scene eventOut for {@link SignalPdu#dataLength}
 */
	protected SFInt32 	dataLength;

/**
 * VRML scene eventOut for {@link SignalPdu#data00}
 */
	protected SFInt32 		data00;

/**
 * VRML scene eventOut for {@link SignalPdu#data01}
 */
	protected SFInt32 		data01;

/**
 * VRML scene eventOut for {@link SignalPdu#data02}
 */
	protected SFInt32 		data02;

/**
 * VRML scene eventOut for {@link SignalPdu#data03}
 */
	protected SFInt32 		data03;

/**
 * VRML scene eventOut for {@link SignalPdu#data04}
 */
	protected SFInt32 		data04;

/**
 * VRML scene eventOut for {@link SignalPdu#data05}
 */
	protected SFInt32 		data05;

/**
 * VRML scene eventOut for {@link SignalPdu#data06}
 */
	protected SFInt32 		data06;

/**
 * VRML scene eventOut for {@link SignalPdu#data07}
 */
	protected SFInt32 		data07;

/**
 * VRML scene eventOut for {@link SignalPdu#data08}
 */
	protected SFInt32 		data08;

/**
 * VRML scene eventOut for {@link SignalPdu#data09}
 */
	protected SFInt32 		data09;

/**
 * VRML scene eventOut for {@link SignalPdu#data10}
 */
	protected SFInt32 		data10;

/**
 * VRML scene eventOut for {@link ReceiverPdu#receiverPower}
 */
	protected SFFloat 		receiverPower;

/**
 * VRML scene eventOut for {@link ReceiverPdu#receiverState}
 */
	protected SFInt32 		receiverState;

/**
 * field, entityID triplet: transmitterSiteID
 * (as part of {@link ReceiverPdu#transmitterEntityID transmitterEntityID} member object for {@link ReceiverPdu})
 */

protected SFInt32   transmitterSiteID;

/**
 * field, entityID triplet: transmitterApplicationID
 * (as part of {@link ReceiverPdu#transmitterEntityID transmitterEntityID} member object for {@link ReceiverPdu})
 */

protected SFInt32   transmitterApplicationID;

/**
 * field, entity ID triplet: transmitterEntityID
 * (as part of {@link ReceiverPdu#transmitterEntityID transmitterEntityID} member object for {@link ReceiverPdu})
 */

protected SFInt32   transmitterEntityID;

/**
 * VRML scene eventOut for {@link ReceiverPdu#transmitterRadioID}
 */
protected SFInt32   transmitterRadioID;

/**
 * VRML scene eventOut for {@link TransmitterPdu#antennaLocation}
 */
protected SFVec3f   antennaLocation;

/**
 * VRML scene eventOut for {@link TransmitterPdu#antennaPatternLength}
 */
protected SFInt32   antennaPatternLength;

/**
 * VRML scene eventOut for {@link TransmitterPdu#antennaPatternType}
 */
protected SFInt32   antennaPatternType;

/**
 * VRML scene eventOut for {@link TransmitterPdu#cryptoKeyId}
 */
protected SFInt32    cryptoKeyId;

/**
 * VRML scene eventOut for {@link TransmitterPdu#cryptoSytem}
 */
protected SFInt32   cryptoSytem;

/**
 * VRML scene eventOut for {@link TransmitterPdu#frequency}
 */
protected SFInt32   frequency;

/**
 * VRML scene eventOut for {@link TransmitterPdu#inputSource}
 */
protected SFInt32   inputSource;

/**
 * VRML scene eventOut for {@link TransmitterPdu#lengthOfModulationParameters}
 */
protected SFInt32   lengthOfModulationParameters;

/**
 * VRML scene eventOut for {@link TransmitterPdu#modulationTypeDetail}
 */
protected SFInt32   modulationTypeDetail;

/**
 * VRML scene eventOut for {@link TransmitterPdu#modulationTypeMajor}
 */
protected SFInt32   modulationTypeMajor;

/**
 * VRML scene eventOut for {@link TransmitterPdu#modulationTypeSpreadSpectrum}
 */
protected SFInt32   modulationTypeSpreadSpectrum;

/**
 * VRML scene eventOut for {@link TransmitterPdu#modulationTypeSystem}
 */
protected SFInt32   modulationTypeSystem;

/**
 * VRML scene eventOut for {@link TransmitterPdu#power}
 */
protected SFInt32   power;

/**
 * VRML scene eventOut for {@link TransmitterPdu#radioEntityTypeCategory}
 */
protected SFInt32   radioEntityTypeCategory;

/**
 * VRML scene eventOut for {@link TransmitterPdu#radioEntityTypeCountry}
 */
protected SFInt32   radioEntityTypeCountry;

/**
 * VRML scene eventOut for {@link TransmitterPdu#radioEntityTypeDomain}
 */
protected SFInt32   radioEntityTypeDomain;

/**
 * VRML scene eventOut for {@link TransmitterPdu#radioEntityTypeKind}
 */
protected SFInt32   radioEntityTypeKind;

/**
 * VRML scene eventOut for {@link TransmitterPdu#radioEntityTypeNomenclature}
 */
protected SFInt32   radioEntityTypeNomenclature;

/**
 * VRML scene eventOut for {@link TransmitterPdu#radioEntityTypeNomenclatureVersion}
 */
protected SFInt32   radioEntityTypeNomenclatureVersion;

/**
 * VRML scene eventOut for {@link TransmitterPdu#relativeAntennaLocation}
 */
protected SFVec3f   relativeAntennaLocation;

/**
 * VRML scene eventOut for {@link TransmitterPdu#transmitFrequencyBandwidth}
 */
protected SFInt32   transmitFrequencyBandwidth;

/**
 * VRML scene eventOut for {@link TransmitterPdu#transmitState}
 */
protected SFInt32   transmitState;

 
/**
 * field: enable Java trace statements to console
 */

protected SFBool   traceJava;

/**
 * eventOut: recent active signalPdu's heard
 */

protected SFBool   active = new SFBool (false);

/**
 *  ProtocolDataUnit is an abstract class used to inspect each incoming PDU for type information, do not initialize per se.
 */
	protected ProtocolDataUnit	nextPdu;

/**
 *  Carries receiver information among simulation participants
 */
	protected ReceiverPdu		receiverPdu	= new ReceiverPdu ();

/**
 *  Carries signal information among simulation participants
 */
	protected SignalPdu		signalPdu	= new SignalPdu ();

/**
 *  Carries transmitterPdu information among simulation participants
 */
	protected TransmitterPdu	transmitterPdu	= new TransmitterPdu ();
/**
 *  Report via Java console when a PDU is first heard
 */
	protected boolean     	pduFirstHeard = true;

/*
 *  local variables
 */

	private Class		signalPduClass	= null;
	private	int 		pduCount	= 0;

	protected EntityID    	entityIDObject	= null;	// unique triplet that identifies entity

	private Vector      	receivedPDUsList= new Vector();  // holds PDUs that have come in

	protected static      	EntityDispatcher entityDispatcher = null;   // sends/receives PDUs
	protected static	Thread entityDispatcherThread;

	private static Boolean	guard = new Boolean(true);  // guard for entityDispatcher

	private boolean     	registered = false; // true=>has been placed in hash table

	private boolean     	entityDispatcherThreadStarted = false;

	private Browser		browser		= null; // VRML browser

	private int 			pduVectorSize;
	private ProtocolDataUnit	pdu	 = null;

/**
 *   No more than 11 data elements.  This is very clumsy and needs to be replaced by an MFFloat array.
 */
 
	protected final int MAX_DATA_PARAMETERS = 11;

/**
 *   Get platform-specific security strategy
 */
 
 protected SecurityStrategy strategy;  // = SecurityStrategy.getSecurityStrategy();
 
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
		if (DEBUG) System.out.println("RadioCommunicationsPduScriptNode " + siteID + applicationID + entityID + " " + pDiagnostic);
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
		System.out.println("RadioCommunicationsPduScriptNode " + siteID + applicationID + entityID + " " + pDiagnostic);
	}

/**
 * Simple identifying name for the signalPdu is entityID
 */

	public String getName () { return "RadioCommunicationsPduScriptNode " + entityID; }


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
 
public RadioCommunicationsPduScriptNode ()
{
  debug ("*** constructor for mil.navy.nps.dis.RadioCommunicationsPduScriptNode");
 	  
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

	traceJava  = (SFBool)   getField ("traceJava");     // initialize (from VRML scene)
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
		signalPduClass = Class.forName ("mil.navy.nps.dis.SignalPdu");
		debug ("mil.navy.nps.dis.SignalPdu class found");
	}
	catch (ClassNotFoundException e)
	{
		trace ("mil.navy.nps.dis.SignalPdu class not found");
		trace ("  " + e);
	}
	// see examples 3.11/3.12 pp. 83-86
	// "Java for 3D and VRML Worlds," Lea/Matsuda/Miyashita

	timestamp          = (SFTime)   getEventOut ("timestamp");	// instantiate
	rtpHeaderExpected  = (SFBool)   getField ("rtpHeaderExpected");	// instantiate
	rtpHeaderHeard     = (SFBool)   getEventOut ("rtpHeaderHeard");	// instantiate

	address            = (SFString) getField ("address");		// instantiate
	port               = (SFInt32)  getField ("port");		// instantiate

	trace ("RadioCommunicationsPduScriptNode initialize:");
	trace ("address, port = " + address + " " + port);

	multicastRelayHost = (SFString) getField ("multicastRelayHost");	// instantiate
	multicastRelayPort = (SFInt32)  getField ("multicastRelayPort");	// instantiate

	siteID        = (SFInt32)  getField ("siteID");        // instantiate
	applicationID = (SFInt32)  getField ("applicationID"); // instantiate
	entityID      = (SFInt32)  getField ("entityID");      // instantiate

	signalPdu.setEntityID ((short)   siteID.getValue(), (short) applicationID.getValue(),
	                       (short) entityID.getValue());

	debug ("siteID, applicationID, entityID = " + siteID + " " + applicationID + " " + entityID);

	radioPduType = (SFString) getField ("radioPduType");	// instantiate
	

	if (radioPduType.getValue().equalsIgnoreCase ("ReceiverPdu") ||
	    radioPduType.getValue().equalsIgnoreCase ("SignalPdu")   ||
	    radioPduType.getValue().equalsIgnoreCase ("TransmitterPdu"))
	{
		debug ("radioPduType=" + radioPduType + " in Script node is valid");
		radioPduTypeValid = true;
	}
	else
	{
		trace ("radioPduType=" + radioPduType + " in Script node is not valid!  Ignoring further operations.");
		radioPduTypeValid = false;
		return;
	}

	// radioID is common to all RadioCommunicationFamily PDUs
	// note that these are VRML types, not DIS types!  careful conversion is essential
	radioID		= (SFInt32) getEventOut ("radioID");	// instantiate

	if (radioPduType.getValue().equalsIgnoreCase ("ReceiverPdu"))
	{
		// note that these are VRML types, not DIS types!  careful conversion is essential
		receiverPower			= (SFFloat) getEventOut ("receiverPower");		// instantiate
		receiverState			= (SFInt32) getEventOut ("receiverState");		// instantiate
		transmitterSiteID		= (SFInt32) getEventOut ("transmitterSiteID");		// instantiate
		transmitterApplicationID	= (SFInt32) getEventOut ("transmitterApplicationID");	// instantiate
		transmitterEntityID		= (SFInt32) getEventOut ("transmitterEntityID");	// instantiate
		transmitterRadioID		= (SFInt32) getEventOut ("transmitterRadioID");		// instantiate
		debug ("initialized ReceiverPdu-specific eventOuts");
	}
	else if (radioPduType.getValue().equalsIgnoreCase ("SignalPdu"))
	{
		// note that these are VRML types, not DIS types!  careful conversion is essential
		encodingScheme	= (SFInt32) getEventOut ("encodingScheme");	// instantiate
		tdlType		= (SFInt32) getEventOut ("tdlType");		// instantiate
		sampleRate	= (SFInt32) getEventOut ("sampleRate");		// instantiate
		samples		= (SFInt32) getEventOut ("samples");		// instantiate
		dataLength	= (SFInt32) getEventOut ("dataLength");		// instantiate
		data00		= (SFInt32) getEventOut ("data00");		// instantiate
		data01		= (SFInt32) getEventOut ("data01");		// instantiate
		data02		= (SFInt32) getEventOut ("data02");		// instantiate
		data03		= (SFInt32) getEventOut ("data03");		// instantiate
		data04		= (SFInt32) getEventOut ("data04");		// instantiate
		data05		= (SFInt32) getEventOut ("data05");		// instantiate
		data06		= (SFInt32) getEventOut ("data06");		// instantiate
		data07		= (SFInt32) getEventOut ("data07");		// instantiate
		data08		= (SFInt32) getEventOut ("data08");		// instantiate
		data09		= (SFInt32) getEventOut ("data09");		// instantiate
		data10		= (SFInt32) getEventOut ("data10");		// instantiate
		debug ("initialized SignalPdu-specific eventOuts");
	}
	else if (radioPduType.getValue().equalsIgnoreCase ("TransmitterPdu"))
	{
		// note that these are VRML types, not DIS types!  careful conversion is essential
		antennaLocation				= (SFVec3f) getEventOut ("antennaLocation");			// instantiate
		antennaPatternLength			= (SFInt32) getEventOut ("antennaPatternLength");		// instantiate
		antennaPatternType			= (SFInt32) getEventOut ("antennaPatternType");			// instantiate
		cryptoKeyId				= (SFInt32) getEventOut ("cryptoKeyId");			// instantiate
		cryptoSytem				= (SFInt32) getEventOut ("cryptoSytem");			// instantiate
		frequency				= (SFInt32) getEventOut ("frequency");				// instantiate
		inputSource				= (SFInt32) getEventOut ("inputSource");			// instantiate
		lengthOfModulationParameters		= (SFInt32) getEventOut ("lengthOfModulationParameters");	// instantiate
		modulationTypeDetail			= (SFInt32) getEventOut ("modulationTypeDetail");		// instantiate
		modulationTypeMajor			= (SFInt32) getEventOut ("modulationTypeMajor");		// instantiate
		modulationTypeSpreadSpectrum		= (SFInt32) getEventOut ("modulationTypeSpreadSpectrum");	// instantiate
		modulationTypeSystem			= (SFInt32) getEventOut ("modulationTypeSystem");		// instantiate
		power					= (SFInt32) getEventOut ("power");				// instantiate
		radioEntityTypeCategory			= (SFInt32) getEventOut ("radioEntityTypeCategory");		// instantiate
		radioEntityTypeCountry			= (SFInt32) getEventOut ("radioEntityTypeCountry");		// instantiate
		radioEntityTypeDomain			= (SFInt32) getEventOut ("radioEntityTypeDomain");		// instantiate
		radioEntityTypeKind			= (SFInt32) getEventOut ("radioEntityTypeKind");		// instantiate
		radioEntityTypeNomenclature		= (SFInt32) getEventOut ("radioEntityTypeNomenclature");	// instantiate
		radioEntityTypeNomenclatureVersion	= (SFInt32) getEventOut ("radioEntityTypeNomenclatureVersion");	// instantiate
		relativeAntennaLocation			= (SFVec3f) getEventOut ("relativeAntennaLocation");		// instantiate
		transmitFrequencyBandwidth		= (SFInt32) getEventOut ("transmitFrequencyBandwidth");		// instantiate
		transmitState				= (SFInt32) getEventOut ("transmitState");			// instantiate
		debug ("initialized TransmitterPdu-specific eventOuts");
	}
	else // this case should have been trapped earlier, but maybe sumthin' bad happened..
	{
		trace ("radioPduType=" + radioPduType + " in Script node is not valid!  Ignoring further operations.");
		radioPduTypeValid = false;
		return;
	}

	// Do not set eventOuts during initialization!!  ??

	active  = (SFBool)  getEventOut ("active");	// instantiate
	active.setValue (false);			// initialize

	debug ("multicastRelayHost, Port = " + multicastRelayHost + " " + multicastRelayPort);
	debug ("rtpHeaderExpected = " + rtpHeaderExpected);

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
		signalPdu.setRtpHeaderEnabled (rtpHeaderExpected.getValue());
		debug ("writeInterval = " + writeInterval + " msec, " + 
			"rtpHeaderExpected = " + rtpHeaderExpected.getValue());
	}
	else
	{
		trace ("read & write intervals both <= 0, " +
					"setting readInterval = 1000 msec, writeInterval = 0");
		readInterval  = 1000l;
		writeInterval = 0l;  // ensure zero, not negative
	}
	
	if (address.getValue().equalsIgnoreCase  ("unicast") ||
	    address.getValue().equalsIgnoreCase  ("localhost"))
		entityDispatcher = EntityDispatcher.getEntityDispatcher (null, port.getValue());   // unicast
	else
		entityDispatcher = EntityDispatcher.getEntityDispatcher (address.getValue(), port.getValue()); // multicast
	
	entityDispatcher.checkForTrafficAndFallBackToTunnel	(multicastRelayHost.getValue(),
								 multicastRelayPort.getValue(), 8040);

	debug ("initialize():  entityDispatcher instantiation complete");


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
 * of the RadioCommunicationsPduScriptNode.  If it's null the first time through here, create
 * and thread it.
 * <P>
 * This is synchronized, since we may have N RadioCommunicationsPduScriptNodes attempting
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
		trace ("startEntityDispatcherThread: call attempted to startEntityDispatcherThread again, ignored...");
		return;
	}
	try
	{
		// crash?? debug ("RadioCommunicationsPduScriptNode ThreadGroup = " + Thread.currentThread().getThreadGroup()); // crashes Netscape

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
	//	debug (entityDispatcherThread.getName() + " thread started");

		debug ("startEntityDispatcherThread() complete");
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
	// debug ("===== VRML processEvent" + event);

	if (radioPduTypeValid == false)
	{
		debug ("radioPduTypeValid failed, ignoring network and all VRML events");
		return;
	}

	int	radioPduIndex;

	try {	//  catchAllException to diagnose run-time errors while VRML browser continues

		// initialize entityDispatcher block

		// debug ("entityDispatcherThreadStarted = " + entityDispatcherThreadStarted);
		if (entityDispatcherThreadStarted == false)  // start thread if not already started
		{
			try{
				debug ("initial processEvent received from VRML scene (" + event + ")");
				debug ("strategy.invokePrivilege(this, \"startEntityDispatcherThread\");");
				strategy.invokePrivilege(this, "startEntityDispatcherThread");
				// if startEntityDispatcherThread () succeeds, entityDispatcherThreadStarted is set true
			}
			catch (Exception catchAllException)
			{
				trace ("initialize:   exception from strategy.invokePrivilege " + catchAllException);
			 	catchAllException.printStackTrace();
	   		}
		}

		// only used if entityDispatcher threading failed (& thus not start-ed) !
		if (entityDispatcherThreadStarted == false)
		{
			entityDispatcher.singleReadLoop ();
		}

		// write block
		
		if (writeInterval > 0)
		{
			debug ("writeInterval=" + writeInterval);  // This method is called at writeInterval durations
			
			// we are here in the processEvent method so it is OK to send the PDU

			if (radioPduType.getValue().equalsIgnoreCase ("ReceiverPdu"))
			{
				debug ("send receiverPdu " + receiverPdu.getEntityID().toString());
				entityDispatcher.sendPdu (receiverPdu, address.getValue(), port.getValue());
			}
			else if (radioPduType.getValue().equalsIgnoreCase ("SignalPdu"))
			{
				debug ("send signalPdu " + signalPdu.getEntityID().toString());
				entityDispatcher.sendPdu (signalPdu, address.getValue(), port.getValue());
			}
			else if (radioPduType.getValue().equalsIgnoreCase ("TransmitterPdu"))
			{
				debug ("send transmitterPdu " + signalPdu.getEntityID().toString());
				entityDispatcher.sendPdu (transmitterPdu, address.getValue(), port.getValue());
			}
			else
			{
				trace (	"signalPdu.getEntityID().toString()=" +
					 signalPdu.getEntityID().toString() +
					" garbled, ignoring processEvent() write command");
				return;
			}
			
	//		signalPdu.printValues (2, System.out);  // (indentLevel, printStream)
	//		entityDispatcher.sendPduMulticast (signalPdu);

			debug  ("entityDispatcher.sendPdu (...) complete");
			return;
		}
	}
	catch (Exception catchAllException)
	{
		trace ("*** processEvent writer exception: " + catchAllException);
		catchAllException.printStackTrace();
		setDEBUG (true);
		// can't re-throw (catchAllException); since not supported by class vrml.node.Script
	}

	// read block

	synchronized (receivedPDUsList)
	{
		// debug ("read/process receivedPDUsList");

		try { //  catchAllException to diagnose run-time errors while VRML browser continues

			// debug ("process read events, readInterval=" + readInterval);

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
			else debug ("received " + pduVectorSize + " PDUs [" + pduCount + " total]");

			// process all received PDUs

			// all Pdu's are of interest, no timestamp ordering/omissions performed
			
			radioPduIndex = -1;
			for (int i = 0; i < pduVectorSize; i++)
			{
				nextPdu = (ProtocolDataUnit) receivedPDUsList.elementAt (i);
debug (" :)  got nextPdu");
debug (" ((ProtocolDataUnit) receivedPDUsList.elementAt(i)).getPduType()=" + ((ProtocolDataUnit) receivedPDUsList.elementAt (i)).getPduType());
debug (" ((ProtocolDataUnit) receivedPDUsList.elementAt(i)).getPduType().shortValue()=" + ((ProtocolDataUnit) receivedPDUsList.elementAt (i)).getPduType().shortValue());

				if (pduFirstHeard == true)
				{
					pduFirstHeard = false;
					trace ("pduFirstHeard");
				}
				
				if (nextPdu.getRtpHeaderEnabled() != rtpHeaderExpected.getValue())
				{
					debug ("RTP headers " + nextPdu.getRtpHeaderEnabled() + " rather than " +
						rtpHeaderExpected + " as expected!");
					rtpHeaderHeard.setValue(nextPdu.getRtpHeaderEnabled());    // send eventOut
					rtpHeaderExpected.setValue(nextPdu.getRtpHeaderEnabled()); // remember
					debug ("unexpected RTP header status, rtpHeader = " + nextPdu.getRtpHeaderEnabled());
				}

				if ((nextPdu.getPduType().shortValue() == PduTypeField.RECEIVER) &&
				    radioPduType.getValue().equalsIgnoreCase ("ReceiverPdu"))
				{
					radioPduIndex = i;
					pdu = (ProtocolDataUnit) receivedPDUsList.elementAt (i);
					debug ("-- receiverPdu of interest is now element " + i + " of receivedPDUsList");
					receiverPdu = (ReceiverPdu) pdu;

					// update values now
					timestamp.setValue(new SFTime (receiverPdu.getVRMLTimestamp()));  // send eventOut

					debug ("radioID=" +	 receiverPdu.getRadioID().intValue());
					radioID.setValue	(receiverPdu.getRadioID().intValue());
				
					debug ("receiverPower=" + receiverPdu.getReceiverPower());
					receiverPower.setValue	  (receiverPdu.getReceiverPower());
				
					debug ("receiverState=" + receiverPdu.getReceiverState().intValue());
					receiverState.setValue	 (receiverPdu.getReceiverState().intValue());
				
					debug ("transmitterSiteID=" +	 receiverPdu.getTransmitterEntityID().getSiteID().intValue());
					transmitterSiteID.setValue	(receiverPdu.getTransmitterEntityID().getSiteID().intValue());
				
					debug ("transmitterApplicationID=" +	 receiverPdu.getTransmitterEntityID().getApplicationID().intValue());
					transmitterApplicationID.setValue	(receiverPdu.getTransmitterEntityID().getApplicationID().intValue());

					debug ("transmitterEntityID=" +	 receiverPdu.getTransmitterEntityID().getEntityID().intValue());
					transmitterEntityID.setValue	(receiverPdu.getTransmitterEntityID().getEntityID().intValue());

					debug ("transmitterRadioID=" +	 receiverPdu.getTransmitterRadioID().intValue());
					transmitterRadioID.setValue	(receiverPdu.getTransmitterRadioID().intValue());
				}
				else if ((nextPdu.getPduType().shortValue() == PduTypeField.SIGNAL) &&
				    	 radioPduType.getValue().equalsIgnoreCase ("SignalPdu"))
				{
					radioPduIndex = i;
					pdu = (ProtocolDataUnit) receivedPDUsList.elementAt (i);
					debug ("-- signalPdu of interest is now element " + i + " of receivedPDUsList");
					signalPdu = (SignalPdu) pdu;

					// update values now
					timestamp.setValue(new SFTime (signalPdu.getVRMLTimestamp()));  // send eventOut

					debug ("radioID=" +	 signalPdu.getRadioID().intValue());
					radioID.setValue	(signalPdu.getRadioID().intValue());
				
					debug ("encodingScheme=" + signalPdu.getEncodingScheme().intValue());
					encodingScheme.setValue	  (signalPdu.getEncodingScheme().intValue());
				
					debug ("tdlType=" +	 signalPdu.getTdlType().intValue());
					tdlType.setValue	(signalPdu.getTdlType().intValue());
				
					debug ("sampleRate=" +	 (int) signalPdu.getSampleRate().doubleValue());
					sampleRate.setValue	((int) signalPdu.getSampleRate().doubleValue());
				
					debug ("samples=" +	 signalPdu.getSamples().intValue());
					samples.setValue	(signalPdu.getSamples().intValue());

					if (signalPdu.getDataLength().intValue() > MAX_DATA_PARAMETERS)
					{
						trace (	"signalPdu.dataLength " + signalPdu.getDataLength().intValue() + " received, maximum supported is " +
							MAX_DATA_PARAMETERS);
						dataLength.setValue 	(MAX_DATA_PARAMETERS);
					}
					else
					{
						debug ("dataLength=" + signalPdu.getDataLength().intValue());
						dataLength.setValue 	(signalPdu.getDataLength().intValue());
					}

					debug ("data00=" +	 signalPdu.getData00().intValue());
					data00.setValue		(signalPdu.getData00().intValue());
					debug ("data01=" + 	 signalPdu.getData01().intValue());
					data01.setValue		(signalPdu.getData01().intValue());
					debug ("data02=" +	 signalPdu.getData02().intValue());
					data02.setValue		(signalPdu.getData02().intValue());
					debug ("data03=" + 	 signalPdu.getData03().intValue());
					data03.setValue		(signalPdu.getData03().intValue());
					debug ("data04=" +	 signalPdu.getData04().intValue());
					data04.setValue		(signalPdu.getData04().intValue());
					debug ("data05=" + 	 signalPdu.getData05().intValue());
					data05.setValue		(signalPdu.getData05().intValue());
					debug ("data06=" +	 signalPdu.getData06().intValue());
					data06.setValue		(signalPdu.getData06().intValue());
					debug ("data07=" + 	 signalPdu.getData07().intValue());
					data07.setValue		(signalPdu.getData07().intValue());
					debug ("data08=" +	 signalPdu.getData08().intValue());
					data08.setValue		(signalPdu.getData08().intValue());
					debug ("data09=" + 	 signalPdu.getData09().intValue());
					data09.setValue		(signalPdu.getData09().intValue());
					debug ("data10=" +	 signalPdu.getData10().intValue());
					data10.setValue		(signalPdu.getData10().intValue());
					if (pduFirstHeard == true)
					{
						pduFirstHeard = false;
						trace ("pduFirstHeard");
					}
				}
				else if ((nextPdu.getPduType().shortValue() == PduTypeField.TRANSMITTER) &&
				    	radioPduType.getValue().equalsIgnoreCase ("TransmitterPdu"))
				{
					radioPduIndex = i;
					pdu = (ProtocolDataUnit) receivedPDUsList.elementAt (i);
					debug ("-- transmitterPdu of interest is now element " + i + " of receivedPDUsList");
					transmitterPdu = (TransmitterPdu) pdu;

					// update values now
					timestamp.setValue(new SFTime (transmitterPdu.getVRMLTimestamp()));  // send eventOut

					debug ("radioID=" +	 transmitterPdu.getRadioID().intValue());
					radioID.setValue	(transmitterPdu.getRadioID().intValue());

					debug ("antennaLocation=" +
						transmitterPdu.getAntennaLocation().getX() + ", " +
						transmitterPdu.getAntennaLocation().getY() + ", " +
						transmitterPdu.getAntennaLocation().getZ());
					// type WorldCoordinate
					antennaLocation.setValue   (
						(float) transmitterPdu.getAntennaLocation().getX(),
						(float) transmitterPdu.getAntennaLocation().getY(),
						(float) transmitterPdu.getAntennaLocation().getZ());

					debug ("antennaPatternLength=" +	 transmitterPdu.getAntennaPatternLength().intValue());
					antennaPatternLength.setValue	(transmitterPdu.getAntennaPatternLength().intValue());

					debug ("antennaPatternType=" +	 transmitterPdu.getAntennaPatternType().intValue());
					antennaPatternType.setValue	(transmitterPdu.getAntennaPatternType().intValue());

					debug ("cryptoKeyId=" +	 transmitterPdu.getCryptoKeyId().intValue());
					cryptoKeyId.setValue	(transmitterPdu.getCryptoKeyId().intValue());

					debug ("cryptoSytem=" +	 transmitterPdu.getCryptoSytem().intValue());
					cryptoSytem.setValue	(transmitterPdu.getCryptoSytem().intValue());

					debug ("frequency=" +	 transmitterPdu.getFrequency().intValue());
					frequency.setValue	(transmitterPdu.getFrequency().intValue());

					debug ("inputSource=" +	 transmitterPdu.getInputSource().intValue());
					inputSource.setValue	(transmitterPdu.getInputSource().intValue());

					debug ("lengthOfModulationParameters=" + transmitterPdu.getLengthOfModulationParameters().intValue());
					lengthOfModulationParameters.setValue	(transmitterPdu.getLengthOfModulationParameters().intValue());

					debug ("modulationTypeDetail=" + transmitterPdu.getModulationType().getDetail().intValue());
					modulationTypeDetail.setValue	(transmitterPdu.getModulationType().getDetail().intValue());

					debug ("modulationTypeMajor=" +	 transmitterPdu.getModulationType().getMajor().intValue());
					modulationTypeMajor.setValue	(transmitterPdu.getModulationType().getMajor().intValue());

					debug ("modulationTypeSpreadSpectrum=" + transmitterPdu.getModulationType().getSpreadSpectrum());
					modulationTypeSpreadSpectrum.setValue	(transmitterPdu.getModulationType().getSpreadSpectrum());

					debug ("modulationTypeSystem=" + transmitterPdu.getModulationType().getSystem().intValue());
					modulationTypeSystem.setValue	(transmitterPdu.getModulationType().getSystem().intValue());

					debug ("power=" +	 transmitterPdu.getPower().intValue());
					power.setValue		(transmitterPdu.getPower().intValue());

					debug ("radioEntityTypeCategory=" +	 transmitterPdu.getRadioEntityType().getCategory().intValue());
					radioEntityTypeCategory.setValue	(transmitterPdu.getRadioEntityType().getCategory().intValue());

					debug ("radioEntityTypeCountry=" + transmitterPdu.getRadioEntityType().getCountry().intValue());
					radioEntityTypeCountry.setValue	  (transmitterPdu.getRadioEntityType().getCountry().intValue());

					debug ("radioEntityTypeDomain=" + transmitterPdu.getRadioEntityType().getDomain().intValue());
					radioEntityTypeDomain.setValue	 (transmitterPdu.getRadioEntityType().getDomain().intValue());

					debug ("radioEntityTypeKind=" +	 transmitterPdu.getRadioEntityType().getEntityKind().intValue());
					radioEntityTypeKind.setValue	(transmitterPdu.getRadioEntityType().getEntityKind().intValue());

					debug ("radioEntityTypeNomenclature=" +	 transmitterPdu.getRadioEntityType().getNomenclature().intValue());
					radioEntityTypeNomenclature.setValue	(transmitterPdu.getRadioEntityType().getNomenclature().intValue());

					debug ("radioEntityTypeNomenclatureVersion=" +	 transmitterPdu.getRadioEntityType().getNomenclatureVersion().intValue());
					radioEntityTypeNomenclatureVersion.setValue	(transmitterPdu.getRadioEntityType().getNomenclatureVersion().intValue());

					debug ("relativeAntennaLocation=" +
						transmitterPdu.getRelativeAntennaLocation().getX() + ", " +
						transmitterPdu.getRelativeAntennaLocation().getY() + ", " +
						transmitterPdu.getRelativeAntennaLocation().getZ());
					// type EntityCoordinate
					relativeAntennaLocation.setValue   (
						transmitterPdu.getRelativeAntennaLocation().getX(),
						transmitterPdu.getRelativeAntennaLocation().getY(),
						transmitterPdu.getRelativeAntennaLocation().getZ());

					debug ("transmitFrequencyBandwidth=" +	 transmitterPdu.getTransmitFrequencyBandwidth().intValue());
					transmitFrequencyBandwidth.setValue	(transmitterPdu.getTransmitFrequencyBandwidth().intValue());

					debug ("transmitState=" + transmitterPdu.getTransmitState().intValue());
					transmitState.setValue	 (transmitterPdu.getTransmitState().intValue());
				}
				else if (nextPdu.getPduType().shortValue() == PduTypeField.RECEIVER)
				{
					 debug ("mismatched PDU for this Script node, " + nextPdu.pduName () +
					 	" (" + ((ReceiverPdu)nextPdu).getEntityID().getSiteID().intValue() +
						", " + ((ReceiverPdu)nextPdu).getEntityID().getApplicationID().intValue() +
						", " + ((ReceiverPdu)nextPdu).getEntityID().getEntityID().intValue() + ")" );
				}
				else if (nextPdu.getPduType().shortValue() == PduTypeField.SIGNAL)
				{
					 debug ("mismatched PDU for this Script node, " + nextPdu.pduName () +
					 	" (" + ((SignalPdu)nextPdu).getEntityID().getSiteID().intValue() +
						", " + ((SignalPdu)nextPdu).getEntityID().getApplicationID().intValue() +
						", " + ((SignalPdu)nextPdu).getEntityID().getEntityID().intValue() + ")" );
				}
				else if (nextPdu.getPduType().shortValue() == PduTypeField.TRANSMITTER)
				{
					 debug ("mismatched PDU for this Script node, " + nextPdu.pduName () +
					 	" (" + ((TransmitterPdu)nextPdu).getEntityID().getSiteID().intValue() +
						", " + ((TransmitterPdu)nextPdu).getEntityID().getApplicationID().intValue() +
						", " + ((TransmitterPdu)nextPdu).getEntityID().getEntityID().intValue() + ")" );
				}
				else // unusable PDU type
				{
					trace ("not a usable pdu, element " + i + " of receivedPDUsList is type " +
						nextPdu.getPduType() + " = " +
						PduTypeField.toString (nextPdu.getPduType().intValue()));
				     //	trace ("see dis-java-vrml/docs/dis-java-vrml/mil/navy/nps/disEnumerations/PduTypeField.html");
				}
			}
			if (radioPduIndex == -1) // no radio communications family PDUs received
			{
				debug ("heard no radio communications family PDUs on this channel");
			}
			else // process the most current signalPdu which is 'pdu' - we are trying to visualize flows, not recreate them
			{
				// debug ("have one or more RadioCOmmunicationsFamily Pdus");
				if (active.getValue() == false)
				{
					active.setValue (true);
					trace ("signalPdu(s) received - active!  RTP headers: " + nextPdu.getRtpHeaderEnabled());
					rtpHeaderHeard.setValue(nextPdu.getRtpHeaderEnabled());    // send eventOut
				//	DEBUG=true;
				}

				if (pdu == null) trace ("pdu lost scope!!");
			}
			debug ("starting receivedPDUsList.removeAllElements ()...");
			receivedPDUsList.removeAllElements ();  // garbage collection will reclaim this Vector
			debug ("finished receivedPDUsList.removeAllElements ()");
			pdu = null; nextPdu = null;  // dereference list PDUs
			debug ("receivedPDUsList.removeAllElements (); isEmpty() = " + receivedPDUsList.isEmpty ());
			if (!receivedPDUsList.isEmpty ()) trace ("receivedPDUsList.removeAllElements (); failed!");
		}
		catch (Exception catchAllException)
		{
			trace ("*** processEvent reader exception: " + catchAllException);
			catchAllException.printStackTrace();
			setDEBUG (true);

			receivedPDUsList.removeAllElements ();  // garbage collection will reclaim this Vector
			if (!receivedPDUsList.isEmpty ()) trace ("cleanup: receivedPDUsList.removeAllElements (); failed!");
			else                              debug ("cleanup: receivedPDUsList.removeAllElements (); succeeded!");
			// can't re-throw (catchAllException); since not supported by class vrml.node.Script
		}

	} // end synchronized (receivedPDUsList)

	debug ("finished processEvent");

	return;
}

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
		RadioCommunicationsPduScriptNode transformTester = new RadioCommunicationsPduScriptNode();
		EntityID       eio;

		if (entityDispatcher == null)
		{
			entityDispatcher = EntityDispatcher.getEntityDispatcher ("224.2.181.145", 62040); // multicast

			// Thread used to run the entityDispatcher
			Thread aThread = new Thread(entityDispatcher, "DebuggingEntityDispatcher-" + ClassUtilities.nextSerialNum());
			// can't use trace() here since not static
			System.out.println ("RadioCommunicationsPduScriptNode main:  entityDispatcher thread starting...");
			aThread.start();
			System.out.println ("RadioCommunicationsPduScriptNode main:  " + aThread.getName() + " entityDispatcher thread started...");

		}

		// DIS entityID object, which has a few more smarts.
		eio = new EntityID(0,1,2);
		transformTester.setEntityIDObject(eio);

		// Add ourselves to the Hashtable of RadioCommunicationsPduScriptNodes that can receive PDUs.
		//entityDispatcher.addRadioCommunicationsPduScriptNode(transformTester);
	} // end of main
}
