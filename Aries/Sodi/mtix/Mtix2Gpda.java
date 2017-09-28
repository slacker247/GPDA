//-------------------------------------------------------------------------------
//	FILE: Mtix2Gpda.java
//
//      PURPOSE: Pull Xml Track data from JMS broker and send to GPDA
//
//	CONTACT: Dennis Ellis - Northrop Grumman Mission Systems
//
//	MODS:	2003-10-09	Initial Release
//
//-------------------------------------------------------------------------------

import java.text.*;
import java.util.*;
import java.io.*;
import java.sql.*;
import java.net.*;
import xml.*;
import sama.subscriber.Subscriber;
import sama.subscriber.SubscriberAdapter;
import sama.subscriber.SubscriberEvent;


public class Mtix2Gpda
{
    static String               strFileName;
    static boolean              fbDebug = false;
    static boolean              fbFileRead = false;
    private static final String strTABLE_NAME = "SJCOHN.DATASEND";
    private static final String strDATABASE = "pretzel:1521:LCWEST";
    private static final String strUSERID = "c3dvuser";
    private static final String strPASSWORD = "c3dvuser";
    private static final String strJMS_SUB_TRACK = "ssrv:localhost:9999:MTIX:GMTI:60000";
    private ToDb                dbOut = null;
    private ToGpda              gpdaOut = null;

    public static void main (String args[])
    {
	String strTemp;

	for (int i = 0; i < args.length; i++)
	    {
		strTemp = args[i];
		if (strTemp.charAt(0) == '-')
		    {
			fbDebug = true;
		    }
		else
		    {
			strFileName = args[i];
			fbFileRead = true;
		    }
	    }

	if (fbFileRead)
	    System.out.println ("Reading from file");
	else
	    System.out.println ("Using JMS");

	new Mtix2Gpda().myBegin();
    }

    public void myBegin()
    {
	String strXmlData;
		
	dbOut = new ToDb ();
	try
	    {
	        dbOut.openDb (strDATABASE, strUSERID, strPASSWORD);
	    }
	catch (Exception e)
	    {
		System.out.println ("Exception " + e);
		System.exit (1);
	    }

	gpdaOut = new ToGpda ();
	try
	    {
	        gpdaOut.openGpda (strDATABASE, strUSERID, strPASSWORD);
	    }
	catch (Exception e)
	    {
		System.out.println ("Exception " + e);
		System.exit (1);
	    }

	if (fbFileRead)
	    {
		try
		    {
			strXmlData = myReadXml (strFileName);
		    }
		catch (IOException e)
		    {
			System.out.println ("File read exception " + e);
			return;
		    }
		myTrackParser (strXmlData);
	    }
	else
	    {
		Subscriber jmsTrackSubscriber = new Subscriber (strJMS_SUB_TRACK);
		Subscriber jmsElintSubscriber = new Subscriber (strJMS_SUB_TRACK);
		MyTrackListener jmsTrackListener = new MyTrackListener();
		MyElintListener jmsElintListener = new MyElintListener();
	
		try
		    {
			jmsTrackSubscriber.open();
		    }
		catch (Exception oe)
		    {
			oe.printStackTrace();
			return;
		    }	
		jmsTrackSubscriber.addSubscriberListener(jmsTrackListener);
		jmsTrackSubscriber.waitForTimeout();
		jmsTrackSubscriber.removeSubscriberListener(jmsTrackListener);
		jmsTrackSubscriber.close();
	
		try
		    {
			jmsElintSubscriber.open();
		    }
		catch (Exception oe)
		    {
			oe.printStackTrace();
			return;
		    }	
		jmsElintSubscriber.addSubscriberListener(jmsElintListener);
		jmsElintSubscriber.waitForTimeout();
		jmsElintSubscriber.removeSubscriberListener(jmsElintListener);
		jmsElintSubscriber.close();
	    }
    }

    public String myReadXml (String strFileName) throws IOException
    {
	char chrBuffer[] = new char[64 * 1024];
	String strXmlData;
	File fileXml = new File (strFileName);

	if (!fileXml.exists () || !fileXml.canRead () || !fileXml.isFile ())
	    {
		throw new IOException ("File open error");
	    }

        try
	    {
		FileReader fileReader = new FileReader (fileXml);
		int iSize = fileReader.read (chrBuffer);
		strXmlData = (String.valueOf (chrBuffer)).substring (0, iSize);
	    }
	catch (IOException e)
	    {
		IOException fe = new IOException ("File read error");
		fe.initCause (e);
		throw fe;
	    }

	return (strXmlData);
    }

    class MyTrackListener extends SubscriberAdapter
    {
	public void subscriberDataReceived (SubscriberEvent jmsEvent)
	{
	    String strXmlData = new String (((Subscriber) jmsEvent.getSource()).getMessage());
	    printDebug("MTIX Track message received");
			
	    myTrackParser (strXmlData);
	}
    }

    public void myTrackParser (String strXml)
    {
	String strTrackId;
	String strDateTime;
	double dlLong;
	double dlLat;
	double dlAltitude;
	double dlSpeed;
	double dlCourse;
	double dlAngle;
	double dlEast;
	double dlNorth;
	boolean	fbWrite = false;

	// Create an XMLDocument into which parsing results will be stored.
	XMLDocument xmlDocument = new XMLDocument();
		
	// Initiate the SAX parsing process. Take the string that was received,
	// and produce an XmlDocument object that can be programmatically
	// traversed.

	try
	    {
		xmlDocument.parseSAX (strXml);
	    }
	catch (XMLException xe)
	    {
		xe.printStackTrace();
		return;
	    }
		
	XMLtracks xmlTracks = (XMLtracks) xmlDocument.getRootXmlElement();
	printDebug("Version:      " + xmlTracks.getVersion());

	for (int i=0; i < xmlTracks.numberOfCollections(); i++)
	    {
		XMLtrackCollection xmlTrackCollection = xmlTracks.getCollection(i);
		if (false)
		    {
			if (xmlTrackCollection.existsProjection())
			    {
				XMLprojection xmlProjection = xmlTrackCollection.getProjection();
				printDebug(  "Projection:   " + xmlProjection.getProjectionType());
			    }
			if (xmlTrackCollection.existsTracker())
			    {
				XMLtracker xmlTracker = xmlTrackCollection.getTracker();
				printDebug(  "TrackerId:    " + xmlTracker.getId());
			    }
		    }

		for (int j = 0; j < xmlTrackCollection.numberOfTracks (); j++)
		    {
			printDebug(" ");
			XMLtrack xmlTrack = xmlTrackCollection.getTrack(j);

			strTrackId = xmlTrack.getTrackId();
			printDebug("TrackId:      " + strTrackId);
			dbOut.setTrackId (strTrackId);

			if (false)
			    {
				if (xmlTrack.existsClassification())
				    {
					printDebug("SourceTN:     " + xmlTrack.getClassification());
				    }
	
				if (xmlTrack.existsQuality())
				    {
					printDebug("Quality:      " + xmlTrack.getQuality());
				    }
	
				if (xmlTrack.existsIdentification())
					{
					    printDebug("Ident:        " + xmlTrack.getIdentification());
					}
	
				if (xmlTrack.existsType())
				    {
					printDebug("Type:         " + xmlTrack.getType());
				    }
	
				if (xmlTrack.existsNumMembers())
				    {
					printDebug("Strength:     " + xmlTrack.getNumMembers());
				    }
			    }
				
			XMLtime xmlTime = xmlTrack.getTime();
			int iZone = xmlTime.getZone();
				
				// Date/time string is a little clumsy to extract.

			StringBuffer sbDate = new StringBuffer();
//			SimpleDateFormat sdfDate = new SimpleDateFormat(XMLtime.DEFAULT_FORMAT);
//			SimpleDateFormat sdfDate = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS");
			SimpleDateFormat sdfDate = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
			sdfDate.setTimeZone ( TimeZone.getTimeZone (XMLtime.getZoneString ( iZone ) ) );
			sbDate.append(sdfDate.format(xmlTime.getDate()));
			strDateTime = sbDate.toString();

//			printDebug("Raw Time:     " + xmlTime.getDate ());
			printDebug("Time:         " + strDateTime);
			dbOut.setDateTime (strDateTime);
				
		        // Even though this is a for loop, there should only be one estimate
			for (int k=0; k <xmlTrack.numberOfNormalEstimates(); k++)
			    {
				XMLnormalEstimate xmlNormalEstimate = xmlTrack.getNormalEstimate(k);
				XMLposition xmlPosition = xmlNormalEstimate.getPosition();
				XMLpt xmlPtPos = xmlPosition.getPt();
				dlLong = xmlPtPos.getX ();
				dlLat = xmlPtPos.getY ();
				dlAltitude = xmlPtPos.getZ ();
				printDebug("Lat/Lon/Alt:  " + dlLat + " " + dlLong + " " + dlAltitude);
				dbOut.setPosition (dlLat, dlLong, dlAltitude);
					
				if (xmlNormalEstimate.existsVelocity ())
				    {
					XMLvelocity xmlVelocity = xmlNormalEstimate.getVelocity();
					// This is not the expected case NOT TESTED!
					if (xmlVelocity.getVType().equals(XMLvelocity.RADIAL))
					    {
						dlCourse = 0;
						if(xmlVelocity.existsHeading())
						    {
							dlCourse = xmlVelocity.getHeading();
							printDebug("Radical Course: " + dlCourse);
						    }
						XMLpt xmlPtVel = xmlVelocity.getPt();
						dlSpeed = xmlPtVel.getX();
						printDebug ("Radical Speed:  " + dlSpeed);
						dbOut.setCourseSpeed (dlCourse, dlSpeed);
						fbWrite = true;
						System.out.println ("Warning untested path Radical Velocity");
					    }
					// This is the expect case
					else if (xmlVelocity.getVType().equals(XMLvelocity.VECTOR))
					    {
						if(xmlVelocity.existsHeading())
						    {
							dlCourse = xmlVelocity.getHeading();
							printDebug ("Vector Course: " + dlCourse);
						    }
						XMLpt xmlPtVel = xmlVelocity.getPt();
						dlEast = xmlPtVel.getX();
						dlNorth = xmlPtVel.getY();

						// 3DV required an course angle 0 - 360 starting from North
						// and going clockwise using
						// tan = opposite / adjustent
						// this gives us an angle relative to East in quad A
						// we then need to modify to relative to North and put in
						// the right quad

						dlAngle = 0;

						if (dlNorth == 0)
						    {
							dlAngle = 0;
						    }
						else if (dlEast == 0)
						    {
							dlAngle = 90;
						    }
						else
						    {
							dlAngle = Math.toDegrees(Math.atan (Math.abs (dlNorth / dlEast) ));
						    }

						//	Plot North (y) and East (x) 
						//		D | A
						//		__|__
						//		  |
						//		C | B
						//	Modify answer based on quad

						if ((dlNorth >= 0) && (dlEast >= 0))		// A
						    {
							dlAngle = 90 - dlAngle;
						    }
						else if ((dlNorth < 0) && (dlEast > 0))		// B
						    {
							dlAngle = 90 + dlAngle;
						    }
						else if ((dlNorth <= 0) && (dlEast <= 0))	// C
						    {
							dlAngle = 270 - dlAngle;
						    }
						else // if ((dlNorth > 0) && (dlEast < 0))	// D
						    {
							dlAngle = 270 + dlAngle;
						    }
						printDebug ("Vector Course: " + dlAngle + "ø");
							
						// calculate speed which is the hypotenuse
						// a * a + b * b = c * c
						dlSpeed = Math.sqrt (dlNorth * dlNorth + dlEast * dlEast);
						printDebug ("Vector Speed:  " + dlSpeed + "m/s");
						dbOut.setCourseSpeed (dlAngle, dlSpeed);
						fbWrite = true;
					    }
				    }

				if (fbWrite)
				    {
					fbWrite = false;
					try
					    {
						dbOut.insertDb ();
						gpdaOut.sendGpda ();
					    }
					catch (Exception e)
					    {
						System.out.println ("Exception " + e);
						System.exit (1);
					    }
				    }
			    }

			if (false)
			    {
				if (xmlTrack.existsStatus())
				    {
					XMLstatus xmlStatus = xmlTrack.getStatus();
					if (xmlStatus.getDropped())
					    {
						printDebug ("Status:       DROPPED");
					    }
					if (xmlStatus.getConfirmed())
					    {
						printDebug ("Status:       CONFIRMED");
					    }
				    }
			    }
		    }
	    }
    }

    class MyElintListener extends SubscriberAdapter
    {
	public void subscriberDataReceived (SubscriberEvent jmsEvent)
	{
	    String strXmlData = new String (((Subscriber) jmsEvent.getSource()).getMessage());

	    printDebug("MTIX ELINT message received");
			
	    myElintParser (strXmlData);
	}
    }

    public void myElintParser (String strXml)
    {
	// Create an XMLDocument into which parsing results will be stored.
	XMLDocument xmlDocument = new XMLDocument();
		
	// Initiate the SAX parsing process. Take the string that was received,
	// and produce an XmlDocument object that can be programmatically
	// traversed.

	try
	    {
		xmlDocument.parseSAX (strXml);
	    }
	catch (XMLException xe)
	    {
		xe.printStackTrace();
		return;
	    }
    }

    public void printDebug (String strMessage)
    {
	if(fbDebug)
	    System.out.println (strMessage);
    }
}

class ToDb
{
    private Connection connDb = null;
    private Statement stmtDb = null;
    private Exception exMine;

    private String strTrackId;
    private double dlLatitude;
    private double dlLongitude;
    private double dlAltitude;
    private double dlCourse;
    private double dlSpeed;
    private String strDateTime;

    void openDb (String strDatabase, String strUserId, String strPassword) throws Exception
    {
	/*
	try
	    {
		Class.forName ("oracle.jdbc.driver.OracleDriver");
	    }
	catch (java.lang.ClassNotFoundException e)
	    {
		exMine = new Exception ("ToDb.registerDriver " + e.getMessage());
		exMine.initCause (e);
		throw exMine;
	    }

	try
	    {
		connDb = DriverManager.getConnection ("jdbc:oracle:thin:@" + strDatabase,
					strUserId, strPassword);
	    }
	catch (java.sql.SQLException e)
	    {
		exMine = new Exception ("ToDb.getConnection " + e.getMessage());
		exMine.initCause (e);
		throw exMine;
	    }

	try
	    {
		stmtDb = connDb.createStatement (ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_UPDATABLE);
	    }
	catch (java.sql.SQLException e)
	    {
		exMine = new Exception ("ToDb.createStatement " + e.getMessage());
		exMine.initCause (e);
		throw exMine;
	    }
	*/
	initData ();
    }

    void initData ()
    {
	double dlLatitude  = 0;
	double dlLongitude = 0;
	double dlAltitude  = 0;
	String strTrackId  = "";
	double dlCourse    = 0;
	double dlSpeed     = 0;
	String strDateTime = "";
    }

    void setTrackId (String strTrackId)
    {
	this.strTrackId = strTrackId;
    }

    void setPosition (double dlLatitude, double dlLongitude, double dlAltitude)
    {
	this.dlLongitude = dlLongitude;
	this.dlLatitude = dlLatitude;
	this.dlAltitude = dlAltitude;
    }

    void setCourseSpeed (double dlCourse, double dlSpeed)
    {
	this.dlCourse= dlCourse;
	this.dlSpeed = dlSpeed;
    }

    void setDateTime (String strDateTime)
    {
	this.strDateTime = strDateTime;
    }

    void insertDb () throws Exception
    {
	int iSeqNo;
	String strDb = "???";
	ResultSet rsDb;
	String strCastMemberId;

	if (strTrackId.length () == 0 || strDateTime.length () == 0)
	    {
		exMine = new Exception ("ToDb.insertDb TrackId = [" +
					strTrackId + "] DateTime = [" +
					strDateTime + "]");
		initData ();
		throw exMine;
	    }
	/*
	try		// There is a unique RecordId for each unique TrackId
	    {
		strDb = "SELECT RecordId FROM CastMember WHERE Designator = '" + strTrackId + "'";
		rsDb = stmtDb.executeQuery (strDb);
		if  (rsDb.next())	// Does TrackId already exist in CastMember?
		    {
			strCastMemberId = rsDb.getString ("RecordId");	
			strDb = "UPDATE CastPositionCurrent SET Latitude = " + dlLatitude +
			        ", Longitude = " + dlLongitude +
				", Altitude = " + dlAltitude +
				", Course = " + dlCourse +
				", Speed = " + dlSpeed +
				" WHERE CastMemberID = " + strCastMemberId;	
			stmtDb.executeUpdate (strDb);
		    }
		else	// No, then insert
		    {
			strDb = "SELECT CastMember_RecordId_SeqNo.NextVal AS RecordId_SeqNo FROM DUAL";
			rsDb = stmtDb.executeQuery (strDb);
			if  (rsDb.next())
			    {
				strCastMemberId = rsDb.getString ("RecordId_SeqNo");
	
				//entry date is default sysdate	
				strDb = "INSERT INTO CastMember(RecordID, ObjectID, AffinityID, Mil2525BID, Designator)" +
				    " VALUES(" +
				    strCastMemberId +
				    ", 2, 0, 2114, '" +
				    strTrackId + "')";	
				stmtDb.executeUpdate (strDb);	
				strDb = "INSERT INTO CastPositionCurrent(RecordID, CastMemberID, Latitude, Longitude, Altitude, Course, Speed)" +
				    " VALUES(CastPosCurrent_RecordID_SeqNo.NextVal, " + 
				    strCastMemberId + ", " +
				    dlLatitude + ", " +
				    dlLongitude + ", " +
				    dlAltitude + ",  " +
				    dlCourse + ", " +
				    dlSpeed + ")";	
				stmtDb.executeUpdate (strDb);
			    }
			else
			    {
				exMine = new Exception ("ToDb.nextDb [" + strDb + "]");
				initData ();
				throw exMine;
			    }
		    }
	
		// Ok lets now create an Archive record, SeqNo will inc for each record
		// with same TrackId
	
		strDb = "SELECT MAX(SeqNo) as SeqNo_Max FROM CastPositionArchive WHERE CastMemberID = " +
		        strCastMemberId;
	
		rsDb = stmtDb.executeQuery (strDb);

		if  (rsDb.next())
		    iSeqNo = rsDb.getInt ("SeqNo_Max");
		else iSeqNo = 0;
		iSeqNo +=  1;
	
		strDb = "INSERT INTO CastPositionArchive(RecordID, CastMemberID, SeqNo, Latitude, Longitude, Altitude, Course, Speed, ArchiveTime)" +
		    " VALUES(CastPosArchive_RecordID_SeqNo.NextVal, " + 
		    strCastMemberId + ", " +
		    iSeqNo + ",  " +
		    dlLatitude + ", " +
		    dlLongitude + ", " +
		    dlAltitude + ",  " +
		    dlCourse + ", " +
		    dlSpeed + ", " +
		    "TO_DATE ('" + strDateTime + "', 'YYYY-MM-DD HH24:MI:SS'))";
	
		stmtDb.executeUpdate(strDb);
	    }

	catch (java.sql.SQLException e)
	    {
		exMine = new Exception ("ToDb.execute [" + strDb + "]" + e.getMessage());
		exMine.initCause (e);
		initData ();
		throw exMine;
	    }
	*/

	initData ();
    }
}

class ToGpda
{
    private Connection connDb = null;
    private Statement stmtDb = null;
    private Exception exMine;

    private String strTrackId;
    private double dlLatitude;
    private double dlLongitude;
    private double dlAltitude;
    private double dlCourse;
    private double dlSpeed;
    private String strDateTime;

    public java.util.Date  Time = new java.util.Date();
    private String         IPDestination = "127.0.0.1";
    private String         PortDestination = "8127";

    private int iYear = 2003;
    private int iMonth = 1;
    private int iDay = 24;
    private int iHour = 6;
    private int iMinute = 24;
    private int iSecond = 0;

    void openGpda (String strDatabase, String strUserId, String strPassword) throws Exception
    {

	initData ();
    }

    void initData ()
    {
	double dlLatitude  = 0;
	double dlLongitude = 0;
	double dlAltitude  = 0;
	String strTrackId  = "";
	double dlCourse    = 0;
	double dlSpeed     = 0;
	String strDateTime = "";
    }

    void setTrackId (String strTrackId)
    {
	this.strTrackId = strTrackId;
    }

    void setPosition (double dlLatitude, double dlLongitude, double dlAltitude)
    {
	this.dlLongitude = dlLongitude;
	this.dlLatitude = dlLatitude;
	this.dlAltitude = dlAltitude;
    }

    void setCourseSpeed (double dlCourse, double dlSpeed)
    {
	this.dlCourse= dlCourse;
	this.dlSpeed = dlSpeed;
    }

    void setDateTime (String strDateTime)
    {
	this.strDateTime = strDateTime;
    }

    void sendGpda () throws Exception
    {
	int iSeqNo;
	String strDb = "???";
	ResultSet rsDb;
	String strCastMemberId;
	/*
	if (strTrackId.length () == 0 || strDateTime.length () == 0)
	    {
		exMine = new Exception ("ToGpda.sendGpda TrackId = [" +
					strTrackId +
					"] DateTime = [" +
					strDateTime + "]");
		initData ();
		throw exMine;
	    }
	*/
	try
	    {
		float disbelief = 0.0f;
		float belief = 0.65f;
		int duration = 0;

		Time.setYear(iYear);
		Time.setMonth(iMonth);
		Time.setDate(iDay);
		Time.setHours(iHour);
		Time.setMinutes(iMinute);
		Time.setSeconds(iSecond);
		Time.setTime(Time.getTime() + 0);
		System.out.println("TimeStamp: " +
				   Time.getYear() + ":" +
				   Time.getMonth() + ":" +
				   Time.getDate() + ":" +
				   Time.getHours() + ":" +
				   Time.getMinutes() + ":" +
				   Time.getSeconds());
		String time = Time.getYear() + ":" +
		    Time.getMonth() + ":" +
		    Time.getDate() + ":" +
		    Time.getHours() + ":" +
		    Time.getMinutes() + ":" +
		    Time.getSeconds();

		String msg = "Damage_24 " +
		    belief + " " +
		    disbelief + " " +
		    time + " " +
		    duration;
		String byteStream = "MTIX GPDA " +
		    "TCT_24" +
		    " 105 0 " +
		    msg.length() + " " +
		    msg;

		iMinute = iMinute + 2;
		if (iMinute > 59) {
		    iMinute = 0;
		    iHour = iHour + 1;
		}
		if (iHour > 23) {
		    iHour = 0;
		    iDay = iDay + 1;
		}

		InetAddress host = null;
		host = host.getByName(IPDestination);
		int port = Integer.valueOf(PortDestination).intValue();
		System.out.println("Sending to Local Host on : " + host.getHostAddress() + "/" + port);
		DatagramSocket outSocket = new DatagramSocket();
		byte buf[] = byteStream.getBytes();
		DatagramPacket datagram = new DatagramPacket(buf, byteStream.length());
		datagram.setPort(port);
		datagram.setAddress(host);
		datagram.setLength(byteStream.length());
		outSocket.send(datagram);
		System.out.println("Data [" + byteStream.toString() + "] sent to " + host.toString());
	    } 
	catch(Exception e)
	    {
		exMine = new Exception ("ToGpda " + e.getMessage());
		exMine.initCause (e);
		exMine.printStackTrace();
		initData ();
		//throw exMine;
	    }

	initData ();
    }
}
