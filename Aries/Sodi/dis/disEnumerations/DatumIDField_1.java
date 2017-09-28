/*
 File:		DatumIDField.java
 CVS Info:      $Id$
 Compiler:	jdk 1.2.2
 */

package disEnumerations;

import mil.navy.nps.dis.*;
import mil.navy.nps.util.*;

/**
 * Datum ID Field -- The fixed datum id shall be represented by a 32-bit enumeration (see section 7 in the EBV-DOC).
 *@version 1.1
 *@author Ronan Fauglas
 *@author <a href="mailto:brutzman@nps.navy.mil?subject=dis-java-vrml: disEnumerations.DatumIDField feedback ">Don Brutzman</a>
 *
 *<dt><b>References:</b>
 *<dd>		DIS Data Dictionary: <A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/aa.htm">Datum ID Field</A> (local)
 *				     <A href="http://SISO.sc.ist.ucf.edu/dis/dis-dd/pdu/aa.htm">Datum ID Field</A> (SISO)
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
 *  You access this via something like <b><code>DatumIDField.ROTATIONRATE</code></b>, i.e. combine
 *  the class name, a period, and a class variable (enumeration) name.<P>
 *
 *<dt><b>History:</b>
 *<dd>		21jan98   /Ronan Fauglas   	/New
 *<dd>		30mar99   /Don Brutzman   	/Revised Javadoc, many more enumeration classes
 *
 *<dt><b>Location:</b>
 *<dd><a href="../../../../../../mil/navy/nps/disEnumerations/DatumIDField.java"><i>DatumIDField.java</i></A> (local)
 *<dd><a href="http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/DatumIDField.java">
 *          <i>http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/DatumIDField.java</i></a>
 *
 */
public class DatumIDField_1 extends Object
{
/**
 *(10000) Identification
 */
public static final int IDENTIFICATION = 10000;

/**
 *(100001) DRA Angular X-Velocity
 */
public static final int DRAANGULARXVELOCITY = 100001;

/**
 *(100002) DRA Angular Y-Velocity
 */
public static final int DRAANGULARYVELOCITY = 100002;

/**
 *(100003) DRA Angular Z-Velocity
 */
public static final int DRAANGULARZVELOCITY = 100003;

/**
 *(100004) Appearance, Trailing Effects
 */
public static final int APPEARANCETRAILINGEFFECTS = 100004;

/**
 *(100005) Appearance, Hatch
 */
public static final int APPEARANCEHATCH = 100005;

/**
 *(100008) Appearance, Character Set
 */
public static final int APPEARANCECHARACTERSET = 100008;

/**
 *(100010) Capability, Ammunition Supplier
 */
public static final int CAPABILITYAMMUNITIONSUPPLIER = 100010;

/**
 *(100011) Capability, Miscellaneous Supplier
 */
public static final int CAPABILITYMISCELLANEOUSSUPPLIER = 100011;

/**
 *(100012) Capability, Repair Provider
 */
public static final int CAPABILITYREPAIRPROVIDER = 100012;

/**
 *(100014) Articulation Parameter
 */
public static final int ARTICULATIONPARAMETER = 100014;

/**
 *(100047) Articulation Parameter Type
 */
public static final int ARTICULATIONPARAMETERTYPE = 100047;

/**
 *(100048) Articulation Parameter Value
 */
public static final int ARTICULATIONPARAMETERVALUE = 100048;

/**
 *(100058) Time of Day--Scene
 */
public static final int TIMEOFDAYSCENE = 100058;

/**
 *(100100) Sonar System Status
 */
public static final int SONARSYSTEMSTATUS = 100100;

/**
 *(100200) Weapon System Status
 */
public static final int WEAPONSYSTEMSTATUS = 100200;

/**
 *(100300) Entity/Track/Update Data
 */
public static final int ENTITYTRACKUPDATEDATA = 100300;

/**
 *(100400) Local/Force Training
 */
public static final int LOCALFORCETRAINING = 100400;

/**
 *(100500) Entity/Track Identity Data
 */
public static final int ENTITYTRACKIDENTITYDATA = 100500;

/**
 *(100600) Engagement Data
 */
public static final int ENGAGEMENTDATA = 100600;

/**
 *(100700) Entity/Track Equipment Data
 */
public static final int ENTITYTRACKEQUIPMENTDATA = 100700;

/**
 *(100800) Emission/EW Data
 */
public static final int EMISSIONEWDATA = 100800;

/**
 *(100900) Appearance Data
 */
public static final int APPEARANCEDATA = 100900;

/**
 *(101000) Command/Order Data
 */
public static final int COMMANDORDERDATA = 101000;

/**
 *(101100) Environmental Data
 */
public static final int ENVIRONMENTALDATA = 101100;

/**
 *(101200) Significant Event Data
 */
public static final int SIGNIFICANTEVENTDATA = 101200;

/**
 *(101300) Operator Action Data
 */
public static final int OPERATORACTIONDATA = 101300;

/**
 *(101400) Time Synchronization
 */
public static final int TIMESYNCHRONIZATION = 101400;

/**
 *(101500) Tomahawk Data
 */
public static final int TOMAHAWKDATA = 101500;

/**
 *(11000) Entity Type
 */
public static final int ENTITYTYPE = 11000;

/**
 *(11100) Concatenated
 */
public static final int CONCATENATED = 11100;

/**
 *(11110) Kind (unsigned 8)
 */
public static final int KIND = 11110;

/**
 *(11120) Domain (unsigned 8)
 */
public static final int DOMAIN = 11120;

/**
 *(11130) Country (unsigned 16)
 */
public static final int COUNTRY = 11130;

/**
 *(11140) Category (unsigned 8)
 */
public static final int CATEGORY = 11140;

/**
 *(11150) Subcategory (unsigned 8)
 */
public static final int SUBCATEGORY = 11150;

/**
 *(11160) Specific (unsigned 8)
 */
public static final int SPECIFIC = 11160;

/**
 *(11170) Extra (unsigned 8)
 */
public static final int EXTRA = 11170;

/**
 *(11200) Force ID (unsigned 8)
 */
public static final int FORCEID = 11200;

/**
 *(11300) Description
 */
public static final int DESCRIPTION = 11300;

/**
 *(12000) Alternative Entity Type
 */
public static final int ALTERNATIVEENTITYTYPE = 12000;

/**
 *(12110) Kind (unsigned 8)
 */
public static final int KIND2 = 12110;

/**
 *(12120) Domain (unsigned 8)
 */
public static final int DOMAIN2 = 12120;

/**
 *(12130) Country (unsigned 16)
 */
public static final int COUNTRY2 = 12130;

/**
 *(12140) Category (unsigned 8)
 */
public static final int CATEGORY2 = 12140;

/**
 *(12150) Subcategory (unsigned 8)
 */
public static final int SUBCATEGORY2 = 12150;

/**
 *(12160) Specific (unsigned 8)
 */
public static final int SPECIFIC2 = 12160;

/**
 *(12170) Extra (unsigned 8)
 */
public static final int EXTRA2 = 12170;

/**
 *(12300) Description
 */
public static final int DESCRIPTION2 = 12300;

/**
 *(13000) Entity Marking
 */
public static final int ENTITYMARKING = 13000;

/**
 *(13100) Entity Marking String (string 80)
 */
public static final int ENTITYMARKINGSTRING = 13100;

/**
 *(13200) Crew ID (string 80)
 */
public static final int CREWID = 13200;

/**
 *(14000) Task Organization
 */
public static final int TASKORGANIZATION = 14000;

/**
 *(14200) Regiment Name (string)
 */
public static final int REGIMENTNAME = 14200;

/**
 *(14300) Battalion Name (string)
 */
public static final int BATTALIONNAME = 14300;

/**
 *(14400) Company Name (string)
 */
public static final int COMPANYNAME = 14400;

/**
 *(14500) Platoon Name
 */
public static final int PLATOONNAME = 14500;

/**
 *(14520) Squad Name
 */
public static final int SQUADNAME = 14520;

/**
 *(14540) Team Name
 */
public static final int TEAMNAME = 14540;

/**
 *(14600) Bumper Name
 */
public static final int BUMPERNAME = 14600;

/**
 *(14700) Vehicle Name
 */
public static final int VEHICLENAME = 14700;

/**
 *(14800) Unit Number
 */
public static final int UNITNUMBER = 14800;

/**
 *(15000) DIS Identity
 */
public static final int DISIDENTITY = 15000;

/**
 *(15100) DIS Site ID
 */
public static final int DISSITEID = 15100;

/**
 *(15200) DIS Host ID
 */
public static final int DISHOSTID = 15200;

/**
 *(15300) DIS Entity ID
 */
public static final int DISENTITYID = 15300;

/**
 *(20000) Loads
 */
public static final int LOADS = 20000;

/**
 *(21000) Crew Members
 */
public static final int CREWMEMBERS = 21000;

/**
 *(21100) Crew Member ID
 */
public static final int CREWMEMBERID = 21100;

/**
 *(21200) Health
 */
public static final int HEALTH = 21200;

/**
 *(21300) Job Assignment (string)
 */
public static final int JOBASSIGNMENT = 21300;

/**
 *(23000) Fuel
 */
public static final int FUEL = 23000;

/**
 *(23100) Quantity (liters)
 */
public static final int QUANTITY = 23100;

/**
 *(23105) Quantity (gallons)
 */
public static final int QUANTITY2 = 23105;

/**
 *(24000) Ammunition
 */
public static final int AMMUNITION = 24000;

/**
 *(24001) 120-mm HEAT, quantity (Rounds)
 */
public static final int ENUMERATION120MMHEATQUANTITY = 24001;

/**
 *(24002) 120-mm SABOT, quantity (Rounds)
 */
public static final int ENUMERATION120MMSABOTQUANTITY = 24002;

/**
 *(24003) 127-mm M8, quantity (Rounds)
 */
public static final int ENUMERATION127MMM8QUANTITY = 24003;

/**
 *(24004) 127-mm M20, quantity (Rounds)
 */
public static final int ENUMERATION127MMM20QUANTITY = 24004;

/**
 *(24005) 762-mm M62, quantity (Rounds)
 */
public static final int ENUMERATION762MMM62QUANTITY = 24005;

/**
 *(24006) M250 UKL8A1, quantity (Grenades)
 */
public static final int M250UKL8A1QUANTITY = 24006;

/**
 *(24007) M250 UKL8A3, quantity (Grenades)
 */
public static final int M250UKL8A3QUANTITY = 24007;

/**
 *(24008) 762-mm M80, quantity (Rounds)
 */
public static final int ENUMERATION762MMM80QUANTITY = 24008;

/**
 *(24009) 127-mm, quantity (Rounds)
 */
public static final int ENUMERATION127MMQUANTITY = 24009;

/**
 *(24010) 762-mm, quantity (Rounds)
 */
public static final int ENUMERATION762MMQUANTITY = 24010;

/**
 *(24060) Mines, quantity (Mines)
 */
public static final int MINESQUANTITY = 24060;

/**
 *(24100) Type
 */
public static final int TYPE = 24100;

/**
 *(24110) Kind
 */
public static final int KIND3 = 24110;

/**
 *(24120) Domain
 */
public static final int DOMAIN3 = 24120;

/**
 *(24130) Country
 */
public static final int COUNTRY3 = 24130;

/**
 *(24140) Category
 */
public static final int CATEGORY3 = 24140;

/**
 *(24150) Subcategory
 */
public static final int SUBCATEGORY3 = 24150;

/**
 *(24160) Extra
 */
public static final int EXTRA3 = 24160;

/**
 *(24300) Description
 */
public static final int DESCRIPTION3 = 24300;

/**
 *(25000) Cargo
 */
public static final int CARGO = 25000;

/**
 *(26000) Vehicle Mass (unsigned 32 Kilograms)
 */
public static final int VEHICLEMASS = 26000;

/**
 *(27000) Supply Quantity
 */
public static final int SUPPLYQUANTITY = 27000;

/**
 *(28000) Armament
 */
public static final int ARMAMENT = 28000;

/**
 *(30000) Status
 */
public static final int STATUS = 30000;

/**
 *(31000) Position
 */
public static final int POSITION = 31000;

/**
 *(31100) MilGrid10
 */
public static final int MILGRID10 = 31100;

/**
 *(31200) Geocentric Coordinates
 */
public static final int GEOCENTRICCOORDINATES = 31200;

/**
 *(31210) X (unsigned 32 meters)
 */
public static final int X = 31210;

/**
 *(31220) Y (unsigned 32 meters)
 */
public static final int Y = 31220;

/**
 *(31230) Z (unsigned 32 meters)
 */
public static final int Z = 31230;

/**
 *(31300) Latitude
 */
public static final int LATITUDE = 31300;

/**
 *(31400) Longitude
 */
public static final int LONGITUDE = 31400;

/**
 *(31500) Line of Sight
 */
public static final int LINEOFSIGHT = 31500;

/**
 *(31510) X
 */
public static final int X2 = 31510;

/**
 *(31520) Y
 */
public static final int Y2 = 31520;

/**
 *(31530) Z
 */
public static final int Z2 = 31530;

/**
 *(32000) Orientation
 */
public static final int ORIENTATION = 32000;

/**
 *(32100) Hull Heading Angle (degrees)
 */
public static final int HULLHEADINGANGLE = 32100;

/**
 *(32200) Hull Pitch Angle
 */
public static final int HULLPITCHANGLE = 32200;

/**
 *(32300) Roll Angle
 */
public static final int ROLLANGLE = 32300;

/**
 *(32500) X (unsigned 32 degrees)
 */
public static final int X3 = 32500;

/**
 *(32600) Y (unsigned 32 degrees)
 */
public static final int Y3 = 32600;

/**
 *(32700) Z (unsigned 32 degrees)
 */
public static final int Z3 = 32700;

/**
 *(33000) Appearance
 */
public static final int APPEARANCE = 33000;

/**
 *(33100) Ambient Lighting
 */
public static final int AMBIENTLIGHTING = 33100;

/**
 *(33101) Lights
 */
public static final int LIGHTS = 33101;

/**
 *(33200) Paint Scheme
 */
public static final int PAINTSCHEME = 33200;

/**
 *(33300) Smoke
 */
public static final int SMOKE = 33300;

/**
 *(33400) Trailing Effects
 */
public static final int TRAILINGEFFECTS = 33400;

/**
 *(33500) Flaming
 */
public static final int FLAMING = 33500;

/**
 *(33600) Marking
 */
public static final int MARKING = 33600;

/**
 *(33710) Mine Plows Attached
 */
public static final int MINEPLOWSATTACHED = 33710;

/**
 *(33720) Mine Rollers Attached
 */
public static final int MINEROLLERSATTACHED = 33720;

/**
 *(33730) Tank Turret Azimuth (degrees Rel 2 Ion)
 */
public static final int TANKTURRETAZIMUTH = 33730;

/**
 *(34000) Failures and Malfunctions
 */
public static final int FAILURESANDMALFUNCTIONS = 34000;

/**
 *(34100) Age (miles)
 */
public static final int AGE = 34100;

/**
 *(34110) Kilometers
 */
public static final int KILOMETERS = 34110;

/**
 *(35000) Damage
 */
public static final int DAMAGE = 35000;

/**
 *(35050) Cause
 */
public static final int CAUSE = 35050;

/**
 *(35100) Mobility Kill
 */
public static final int MOBILITYKILL = 35100;

/**
 *(35200) Fire-Power Kill
 */
public static final int FIREPOWERKILL = 35200;

/**
 *(35300) Personnel Casualties
 */
public static final int PERSONNELCASUALTIES = 35300;

/**
 *(36000) Velocity
 */
public static final int VELOCITY = 36000;

/**
 *(36100) X-velocity (meters/second)
 */
public static final int XVELOCITY = 36100;

/**
 *(36200) Y-velocity (meters/second)
 */
public static final int YVELOCITY = 36200;

/**
 *(36300) Z-velocity (meters/second)
 */
public static final int ZVELOCITY = 36300;

/**
 *(37000) Acceleration
 */
public static final int ACCELERATION = 37000;

/**
 *(37100) X-acceleration
 */
public static final int XACCELERATION = 37100;

/**
 *(37200) Y-acceleration
 */
public static final int YACCELERATION = 37200;

/**
 *(37300) Z-acceleration
 */
public static final int ZACCELERATION = 37300;

/**
 *(38100) Engine Status
 */
public static final int ENGINESTATUS = 38100;

/**
 *(40000) Exercise
 */
public static final int EXERCISE = 40000;

/**
 *(40010) Exercise State
 */
public static final int EXERCISESTATE = 40010;

/**
 *(40020) AFATDS File Name
 */
public static final int AFATDSFILENAME = 40020;

/**
 *(41000) Terrain Database
 */
public static final int TERRAINDATABASE = 41000;

/**
 *(41001) 
 */
public static final int ENUMERATION1
 = 41001;

/**
 *(42000) Missions
 */
public static final int MISSIONS = 42000;

/**
 *(42100) Mission ID
 */
public static final int MISSIONID = 42100;

/**
 *(42200) Mission Type
 */
public static final int MISSIONTYPE = 42200;

/**
 *(42300) Mission Request Time Stamp
 */
public static final int MISSIONREQUESTTIMESTAMP = 42300;

/**
 *(43000) Exercise Description (string)
 */
public static final int EXERCISEDESCRIPTION = 43000;

/**
 *(43100) Name (string)
 */
public static final int NAME = 43100;

/**
 *(43200) Entities (integer)
 */
public static final int ENTITIES = 43200;

/**
 *(43300) Version
 */
public static final int VERSION = 43300;

/**
 *(43410) Guise Mode (Unsigned Integer 32)
 */
public static final int GUISEMODE = 43410;

/**
 *(43430) Simulation Application Role Record (Variable Record) 64
 */
public static final int SIMULATIONAPPLICATIONROLERECORD64 = 43430;

/**
 *(43440) Simulation Application State (Variable Record) 64
 */
public static final int SIMULATIONAPPLICATIONSTATE64 = 43440;

/**
 *(44000) Visual Output Mode 
 */
public static final int VISUALOUTPUTMODE = 44000;

/**
 *(44100) Simulation Manager Role (Variable Record)
 */
public static final int SIMULATIONMANAGERROLE = 44100;

/**
 *(44110) Simulation Manager Site ID (Unsigned Integer 16)
 */
public static final int SIMULATIONMANAGERSITEID = 44110;

/**
 *(44120) Simulation Manager Applic ID (Unsigned Integer 16)
 */
public static final int SIMULATIONMANAGERAPPLICID = 44120;

/**
 *(44130) Simulation Manager Entity ID (Unisgned Integer 16)
 */
public static final int SIMULATIONMANAGERENTITYID = 44130;

/**
 *(44140) Simulation Manager Active Status (Unsigned Integer 16; 0  Backup, 1  Primary)
 */
public static final int SIMULATIONMANAGERACTIVESTATUS = 44140;

/**
 *(44200) After Active Review Role (Variable Record)
 */
public static final int AFTERACTIVEREVIEWROLE = 44200;

/**
 *(44210) After Active Review Site ID (Unsigned Integer 16)
 */
public static final int AFTERACTIVEREVIEWSITEID = 44210;

/**
 *(44220) After Active Applic ID (Unsigned Integer 16)
 */
public static final int AFTERACTIVEAPPLICID = 44220;

/**
 *(44230) After Active Review Entity ID (Unsigned Integer 16)
 */
public static final int AFTERACTIVEREVIEWENTITYID = 44230;

/**
 *(44240) After Active Review Active Status (Unsigned Integer 16; 0  Backup, 1  Primary
 */
public static final int AFTERACTIVEREVIEWACTIVESTATUSUNSIGNEDINTEGER160BACKUP1PRIMARY = 44240;

/**
 *(44300) Exercise Logger Role (Variable Record)
 */
public static final int EXERCISELOGGERROLE = 44300;

/**
 *(44310) Exercise Logger Site ID (Unsigned Integer 16)
 */
public static final int EXERCISELOGGERSITEID = 44310;

/**
 *(44320) Exercise Logger Applic ID (Unsigned Integer 16)
 */
public static final int EXERCISELOGGERAPPLICID = 44320;

/**
 *(44330) Exercise Entity ID (Unsigned Integer 16)
 */
public static final int EXERCISEENTITYID = 44330;

/**
 *(44340) Exercise Logger Active Status (Unsigned Integer 16; 0  Backup, 1  Primary)
 */
public static final int EXERCISELOGGERACTIVESTATUS = 44340;

/**
 *(44400) Synthetic Environment Manager Role  (Variable Record)
 */
public static final int SYNTHETICENVIRONMENTMANAGERROLE = 44400;

/**
 *(44410) Synthetic Environment Manager Site ID (Unsigned Integer 16)
 */
public static final int SYNTHETICENVIRONMENTMANAGERSITEID = 44410;

/**
 *(44420) Synthetic Environment Manager Applic ID (Unsigned Integer 16)
 */
public static final int SYNTHETICENVIRONMENTMANAGERAPPLICID = 44420;

/**
 *(44430) Synthetic Environment Manager Entity ID (Unsigned Integer 16)
 */
public static final int SYNTHETICENVIRONMENTMANAGERENTITYID = 44430;

/**
 *(44440) Synthetic Environment Manager Active Status (Unsigned Integer 16 0  Backup, 1  Primary)
 */
public static final int SYNTHETICENVIRONMENTMANAGERACTIVESTATUS = 44440;

/**
 *(44500) SIMNET-DIS Translator Role (Variable Record)
 */
public static final int SIMNETDISTRANSLATORROLE = 44500;

/**
 *(44510) SIMNET-DIS Translator Site ID (Unsigned Integer 16)
 */
public static final int SIMNETDISTRANSLATORSITEID = 44510;

/**
 *(44520) SIMNET-DIS Translator Applic ID (Unsigned Integer 16)
 */
public static final int SIMNETDISTRANSLATORAPPLICID = 44520;

/**
 *(44530) SIMNET-DIS Translator Entity ID (Unsigned Integer 16)
 */
public static final int SIMNETDISTRANSLATORENTITYID = 44530;

/**
 *(44540) SIMNET-DIS Translator Active Status (Unsigned Integer 16 0  Backup, 1  Primary)
 */
public static final int SIMNETDISTRANSLATORACTIVESTATUS = 44540;

/**
 *(50000) Environment
 */
public static final int ENVIRONMENT = 50000;

/**
 *(51000) Weather
 */
public static final int WEATHER = 51000;

/**
 *(51100) Thermal Condition
 */
public static final int THERMALCONDITION = 51100;

/**
 *(52000) Time
 */
public static final int TIME = 52000;

/**
 *(52100) Time of Day, Discrete
 */
public static final int TIMEOFDAYDISCRETE = 52100;

/**
 *(52200) Time of Day, Continuous
 */
public static final int TIMEOFDAYCONTINUOUS = 52200;

/**
 *(52300) Time Mode
 */
public static final int TIMEMODE = 52300;

/**
 *(52305) Time Scene
 */
public static final int TIMESCENE = 52305;

/**
 *(52310) Current Hour
 */
public static final int CURRENTHOUR = 52310;

/**
 *(52320) Current Minute
 */
public static final int CURRENTMINUTE = 52320;

/**
 *(52330) Current Second
 */
public static final int CURRENTSECOND = 52330;

/**
 *(52340) Azimuth
 */
public static final int AZIMUTH = 52340;

/**
 *(52350) Maximum Elevation
 */
public static final int MAXIMUMELEVATION = 52350;

/**
 *(52360) Time Zone
 */
public static final int TIMEZONE = 52360;

/**
 *(52400) Time Sunrise Enabled
 */
public static final int TIMESUNRISEENABLED = 52400;

/**
 *(52410) Sunrise Hour
 */
public static final int SUNRISEHOUR = 52410;

/**
 *(52420) Sunrise Minute
 */
public static final int SUNRISEMINUTE = 52420;

/**
 *(52430) Sunrise Second
 */
public static final int SUNRISESECOND = 52430;

/**
 *(52440) Sunrise Azimuth
 */
public static final int SUNRISEAZIMUTH = 52440;

/**
 *(52500) Time Sunset Enabled
 */
public static final int TIMESUNSETENABLED = 52500;

/**
 *(52510) Sunset Hour
 */
public static final int SUNSETHOUR = 52510;

/**
 *(52511) Sunset Hour
 */
public static final int SUNSETHOUR2 = 52511;

/**
 *(52520) Sunset Minute
 */
public static final int SUNSETMINUTE = 52520;

/**
 *(52530) SunsetSecond
 */
public static final int SUNSETSECOND = 52530;

/**
 *(52531) 
 */
public static final int ENUMERATION2
 = 52531;

/**
 *(52600) Date
 */
public static final int DATE = 52600;

/**
 *(52610) Month
 */
public static final int MONTH = 52610;

/**
 *(52620) Day
 */
public static final int DAY = 52620;

/**
 *(52630) Year
 */
public static final int YEAR = 52630;

/**
 *(53000) Clouds
 */
public static final int CLOUDS = 53000;

/**
 *(53050) Cloud Layer Enable
 */
public static final int CLOUDLAYERENABLE = 53050;

/**
 *(53060) Cloud Layer Selection
 */
public static final int CLOUDLAYERSELECTION = 53060;

/**
 *(53100) Visibility
 */
public static final int VISIBILITY = 53100;

/**
 *(53200) Base Altitude (meters)
 */
public static final int BASEALTITUDE = 53200;

/**
 *(53250) Base Altitude (feet)
 */
public static final int BASEALTITUDE2 = 53250;

/**
 *(53300) Ceiling (meters)
 */
public static final int CEILING = 53300;

/**
 *(53350) Ceiling (feet)
 */
public static final int CEILING2 = 53350;

/**
 *(53400) Characterictics
 */
public static final int CHARACTERICTICS = 53400;

/**
 *(53410) Concentration Length (Floating Point 32) (milligrams/meter ²)
 */
public static final int CONCENTRATIONLENGTHFLOATINGPOINT32 = 53410;

/**
 *(53420) Transmittance (Floating Point 32)
 */
public static final int TRANSMITTANCE = 53420;

/**
 *(53430) Radiance (Floating Point 32) (microwatts/centimeter²/steradian)
 */
public static final int RADIANCEFLOATINGPOINT32 = 53430;

/**
 *(54000) Precipitation
 */
public static final int PRECIPITATION = 54000;

/**
 *(54100) Rain (boolean)
 */
public static final int RAIN = 54100;

/**
 *(55000) Fog (boolean)
 */
public static final int FOG = 55000;

/**
 *(55100) Visibility (meters)
 */
public static final int VISIBILITY2 = 55100;

/**
 *(55105) Visibility (miles)
 */
public static final int VISIBILITY3 = 55105;

/**
 *(55200) Density
 */
public static final int DENSITY = 55200;

/**
 *(55300) Base
 */
public static final int BASE = 55300;

/**
 *(55401) View Layer from above???
 */
public static final int VIEWLAYERFROMABOVE = 55401;

/**
 *(55410) Transition Range
 */
public static final int TRANSITIONRANGE = 55410;

/**
 *(55420) Bottom (meters)
 */
public static final int BOTTOM = 55420;

/**
 *(55425) Bottom (feet)
 */
public static final int BOTTOM2 = 55425;

/**
 *(55430) Ceiling (meters)
 */
public static final int CEILING3 = 55430;

/**
 *(55435) Ceiling (feet)
 */
public static final int CEILING4 = 55435;

/**
 *(56000) Heavenly Bodies
 */
public static final int HEAVENLYBODIES = 56000;

/**
 *(56100) Sun
 */
public static final int SUN = 56100;

/**
 *(56110) Position
 */
public static final int POSITION2 = 56110;

/**
 *(56120) Position Azimuth
 */
public static final int POSITIONAZIMUTH = 56120;

/**
 *(56130) Position Elevation
 */
public static final int POSITIONELEVATION = 56130;

/**
 *(56140) Position Intensity
 */
public static final int POSITIONINTENSITY = 56140;

/**
 *(56200) Moon
 */
public static final int MOON = 56200;

/**
 *(56210) Position
 */
public static final int POSITION3 = 56210;

/**
 *(56220) Position Azimuth
 */
public static final int POSITIONAZIMUTH2 = 56220;

/**
 *(56230) Position Elevation
 */
public static final int POSITIONELEVATION2 = 56230;

/**
 *(56240) Position Intensity
 */
public static final int POSITIONINTENSITY2 = 56240;

/**
 *(56310) Horizon
 */
public static final int HORIZON = 56310;

/**
 *(56320) Horizon Azimuth
 */
public static final int HORIZONAZIMUTH = 56320;

/**
 *(56330) Horizon Elevation
 */
public static final int HORIZONELEVATION = 56330;

/**
 *(56340) Horizon Heading
 */
public static final int HORIZONHEADING = 56340;

/**
 *(56350) Horizon Intensity
 */
public static final int HORIZONINTENSITY = 56350;

/**
 *(57000) Meteorological
 */
public static final int METEOROLOGICAL = 57000;

/**
 *(57100) Temperature
 */
public static final int TEMPERATURE = 57100;

/**
 *(57200) Humidity
 */
public static final int HUMIDITY = 57200;

/**
 *(57300) Visibility
 */
public static final int VISIBILITY4 = 57300;

/**
 *(57400) Winds
 */
public static final int WINDS = 57400;

/**
 *(57410) Speed
 */
public static final int SPEED = 57410;

/**
 *(57500) Rainsoak
 */
public static final int RAINSOAK = 57500;

/**
 *(58000) Haze (boolean)
 */
public static final int HAZE = 58000;

/**
 *(58100) Visibility (meters)
 */
public static final int VISIBILITY5 = 58100;

/**
 *(58105) Visibility (feet)
 */
public static final int VISIBILITY6 = 58105;

/**
 *(58200) Density
 */
public static final int DENSITY2 = 58200;

/**
 *(58430) Ceiling (meters)
 */
public static final int CEILING5 = 58430;

/**
 *(58435) Ceiling (feet)
 */
public static final int CEILING6 = 58435;

/**
 *(59000) Contaminants and Obsurants
 */
public static final int CONTAMINANTSANDOBSURANTS = 59000;

/**
 *(59100) Contaminant/Obscurant Type (Unsigned Integer 32)
 */
public static final int CONTAMINANTOBSCURANTTYPE = 59100;

/**
 *(59110) Persistence (Enumeration 8)
 */
public static final int PERSISTENCE = 59110;

/**
 *(59115) Chemical Dosage (Floating Point 32) (milligrams/meter³/minute)
 */
public static final int CHEMICALDOSAGEFLOATINGPOINT32 = 59115;

/**
 *(59120) Chemical Air Concentration (Floating Point 32) (milligrams/meter³)
 */
public static final int CHEMICALAIRCONCENTRATIONFLOATINGPOINT32 = 59120;

/**
 *(59125) Chemical Ground Deposition (Floating Point 32) (milligrams/meter²)
 */
public static final int CHEMICALGROUNDDEPOSITIONFLOATINGPOINT32 = 59125;

/**
 *(59130) Chemical Maximum Ground Deposition (Floating Point 32) (milligrams/meter²)
 */
public static final int CHEMICALMAXIMUMGROUNDDEPOSITIONFLOATINGPOINT32 = 59130;

/**
 *(59135) Chemical Dosage Threshold (Floating Point 32) (milligram/meter³/minute)
 */
public static final int CHEMICALDOSAGETHRESHOLDFLOATINGPOINT32 = 59135;

/**
 *(59140) Biological Dosage (Floating Point 32) (particles/liter of air/minute)
 */
public static final int BIOLOGICALDOSAGEFLOATINGPOINT32 = 59140;

/**
 *(59145) Biological Air Concentration (Floating Point 32) ( particles/liter of air)
 */
public static final int BIOLOGICALAIRCONCENTRATIONFLOATINGPOINT32 = 59145;

/**
 *(59150) Biological Dosage Threshold (Floating Point 32) (particles/liter of air/minute)
 */
public static final int BIOLOGICALDOSAGETHRESHOLDFLOATINGPOINT32 = 59150;

/**
 *(59155) Biological Binned Particle Count (Enumeration 8) ( 1  Low (5-2), 2  Detection (2-10), 3  High (10-15 )
 */
public static final int BIOLOGICALBINNEDPARTICLECOUNTENUMERATION81LOW522DETECTION2103HIGH = 59155;

/**
 *(59160) Radiological Dosage (Floating Point 32)
 */
public static final int RADIOLOGICALDOSAGE = 59160;

/**
 *(60000) Communications
 */
public static final int COMMUNICATIONS = 60000;

/**
 *(61100) Channel Type
 */
public static final int CHANNELTYPE = 61100;

/**
 *(61101) Channel Type
 */
public static final int CHANNELTYPE2 = 61101;

/**
 *(61200) Channel Identification
 */
public static final int CHANNELIDENTIFICATION = 61200;

/**
 *(61300) Alpha Identification
 */
public static final int ALPHAIDENTIFICATION = 61300;

/**
 *(61301) 
 */
public static final int ENUMERATION3
 = 61301;

/**
 *(61400) Radio Identification
 */
public static final int RADIOIDENTIFICATION = 61400;

/**
 *(61401) 
 */
public static final int ENUMERATION4
 = 61401;

/**
 *(61500) Land Line Identification
 */
public static final int LANDLINEIDENTIFICATION = 61500;

/**
 *(61600) Intercom Identification
 */
public static final int INTERCOMIDENTIFICATION = 61600;

/**
 *(61700) Group Network Channel Number
 */
public static final int GROUPNETWORKCHANNELNUMBER = 61700;

/**
 *(62100) Radio Communications Status
 */
public static final int RADIOCOMMUNICATIONSSTATUS = 62100;

/**
 *(62200) Stationary Radio Transmitters Default Time (unsigned)
 */
public static final int STATIONARYRADIOTRANSMITTERSDEFAULTTIME = 62200;

/**
 *(62300) Moving Radio Transmitters Default Time (unsigned)
 */
public static final int MOVINGRADIOTRANSMITTERSDEFAULTTIME = 62300;

/**
 *(62400) Stationary Radio Signals Default Time
 */
public static final int STATIONARYRADIOSIGNALSDEFAULTTIME = 62400;

/**
 *(62500) Moving Radio Signals Default Time
 */
public static final int MOVINGRADIOSIGNALSDEFAULTTIME = 62500;

/**
 *(63101) Radio Initialization Transec Security Key (record)
 */
public static final int RADIOINITIALIZATIONTRANSECSECURITYKEY = 63101;

/**
 *(63102) Radio Initialization Internal Noise Level (record)
 */
public static final int RADIOINITIALIZATIONINTERNALNOISELEVEL = 63102;

/**
 *(63103) Radio Initialization Squelch Threshold (record)
 */
public static final int RADIOINITIALIZATIONSQUELCHTHRESHOLD = 63103;

/**
 *(63104) Radio Initialization Antenna Location (record)
 */
public static final int RADIOINITIALIZATIONANTENNALOCATION = 63104;

/**
 *(63105) Radio Initialization Antenna Pattern Type (record)
 */
public static final int RADIOINITIALIZATIONANTENNAPATTERNTYPE = 63105;

/**
 *(63106) Radio Initialization Antenna Pattern Length (record)
 */
public static final int RADIOINITIALIZATIONANTENNAPATTERNLENGTH = 63106;

/**
 *(63107) Radio Initialization Beam Definition (record)
 */
public static final int RADIOINITIALIZATIONBEAMDEFINITION = 63107;

/**
 *(63108) Radio Initialization Transmit Heartbeat Time (record)
 */
public static final int RADIOINITIALIZATIONTRANSMITHEARTBEATTIME = 63108;

/**
 *(63109) Radio Initialization Transmit Threshold Distance (record)
 */
public static final int RADIOINITIALIZATIONTRANSMITTHRESHOLDDISTANCE = 63109;

/**
 *(63110) Radio Channel Initialization Lockout ID (record)
 */
public static final int RADIOCHANNELINITIALIZATIONLOCKOUTID = 63110;

/**
 *(63111) Radio Channel Initialization Hopset ID (record)
 */
public static final int RADIOCHANNELINITIALIZATIONHOPSETID = 63111;

/**
 *(63112) Radio Channel Initialization Preset Frequency (record)
 */
public static final int RADIOCHANNELINITIALIZATIONPRESETFREQUENCY = 63112;

/**
 *(63113) Radio Channel Initialization Frequency Sync Time (record)
 */
public static final int RADIOCHANNELINITIALIZATIONFREQUENCYSYNCTIME = 63113;

/**
 *(63114) Radio Channel Initialization Comsec Key (record)
 */
public static final int RADIOCHANNELINITIALIZATIONCOMSECKEY = 63114;

/**
 *(63115) Radio Channel Initialization Alpha (record)
 */
public static final int RADIOCHANNELINITIALIZATIONALPHA = 63115;

/**
 *(70000) Algorithm Parameters
 */
public static final int ALGORITHMPARAMETERS = 70000;

/**
 *(71000) Dead Reckoning Algorithm (DRA)
 */
public static final int DEADRECKONINGALGORITHM = 71000;

/**
 *(71100) DRA Location Threshold (unsigned 32)
 */
public static final int DRALOCATIONTHRESHOLD = 71100;

/**
 *(71200) DRA Orientation Threshold
 */
public static final int DRAORIENTATIONTHRESHOLD = 71200;

/**
 *(71300) DRA Time Threshold
 */
public static final int DRATIMETHRESHOLD = 71300;

/**
 *(72000) Simulation Management Parameters
 */
public static final int SIMULATIONMANAGEMENTPARAMETERS = 72000;

/**
 *(72100) Checkpoint Interval
 */
public static final int CHECKPOINTINTERVAL = 72100;

/**
 *(72600) Transmitter Time Threshold
 */
public static final int TRANSMITTERTIMETHRESHOLD = 72600;

/**
 *(72700) Receiver Time Threshold
 */
public static final int RECEIVERTIMETHRESHOLD = 72700;

/**
 *(73000) Interoperability Mode
 */
public static final int INTEROPERABILITYMODE = 73000;

/**
 *(74000) SIMNET Data Collection (record)
 */
public static final int SIMNETDATACOLLECTION = 74000;

/**
 *(75000) Event ID
 */
public static final int EVENTID = 75000;

/**
 *(75100) Source Site ID
 */
public static final int SOURCESITEID = 75100;

/**
 *(75200) Source Host ID
 */
public static final int SOURCEHOSTID = 75200;

/**
 *(90000) Articulated Parts
 */
public static final int ARTICULATEDPARTS = 90000;

/**
 *(90001) 
 */
public static final int ENUMERATION5
 = 90001;

/**
 *(90050) Part ID
 */
public static final int PARTID = 90050;

/**
 *(90070) Index (See 472)
 */
public static final int INDEX = 90070;

/**
 *(90100) Position
 */
public static final int POSITION4 = 90100;

/**
 *(90200) Position Rate
 */
public static final int POSITIONRATE = 90200;

/**
 *(90300) Extension
 */
public static final int EXTENSION = 90300;

/**
 *(90400) Extension Rate
 */
public static final int EXTENSIONRATE = 90400;

/**
 *(90500) X
 */
public static final int X4 = 90500;

/**
 *(90600) X-rate
 */
public static final int XRATE = 90600;

/**
 *(90700) Y
 */
public static final int Y4 = 90700;

/**
 *(90800) Y-rate
 */
public static final int YRATE = 90800;

/**
 *(90900) Z
 */
public static final int Z4 = 90900;

/**
 *(91000) Z-rate
 */
public static final int ZRATE = 91000;

/**
 *(91100) Azimuth
 */
public static final int AZIMUTH2 = 91100;

/**
 *(91200) Azimuth Rate
 */
public static final int AZIMUTHRATE = 91200;

/**
 *(91300) Elevation
 */
public static final int ELEVATION = 91300;

/**
 *(91400) Elevation Rate
 */
public static final int ELEVATIONRATE = 91400;

/**
 *(91500) Rotation
 */
public static final int ROTATION = 91500;

/**
 *(91600) Rotation Rate
 */
public static final int ROTATIONRATE = 91600;

/**
 * Returns a string containing the enumeration name which corresponds to an enumeration value,
 * as in <b><code>DatumIDField.toString (10000)</code></b> returns the string "<b><code>IDENTIFICATION</code></b>"
 */

public static String toString(int idNumber) 
{
	switch (idNumber) {
		case 10000: return "Identification"; 
		case 100001: return "DRA Angular X-Velocity"; 
		case 100002: return "DRA Angular Y-Velocity"; 
		case 100003: return "DRA Angular Z-Velocity"; 
		case 100004: return "Appearance, Trailing Effects"; 
		case 100005: return "Appearance, Hatch"; 
		case 100008: return "Appearance, Character Set"; 
		case 100010: return "Capability, Ammunition Supplier"; 
		case 100011: return "Capability, Miscellaneous Supplier"; 
		case 100012: return "Capability, Repair Provider"; 
		case 100014: return "Articulation Parameter"; 
		case 100047: return "Articulation Parameter Type"; 
		case 100048: return "Articulation Parameter Value"; 
		case 100058: return "Time of Day--Scene"; 
		case 100100: return "Sonar System Status"; 
		case 100200: return "Weapon System Status"; 
		case 100300: return "Entity/Track/Update Data"; 
		case 100400: return "Local/Force Training"; 
		case 100500: return "Entity/Track Identity Data"; 
		case 100600: return "Engagement Data"; 
		case 100700: return "Entity/Track Equipment Data"; 
		case 100800: return "Emission/EW Data"; 
		case 100900: return "Appearance Data"; 
		case 101000: return "Command/Order Data"; 
		case 101100: return "Environmental Data"; 
		case 101200: return "Significant Event Data"; 
		case 101300: return "Operator Action Data"; 
		case 101400: return "Time Synchronization"; 
		case 101500: return "Tomahawk Data"; 
		case 11000: return "Entity Type"; 
		case 11100: return "Concatenated"; 
		case 11110: return "Kind (unsigned 8)"; 
		case 11120: return "Domain (unsigned 8)"; 
		case 11130: return "Country (unsigned 16)"; 
		case 11140: return "Category (unsigned 8)"; 
		case 11150: return "Subcategory (unsigned 8)"; 
		case 11160: return "Specific (unsigned 8)"; 
		case 11170: return "Extra (unsigned 8)"; 
		case 11200: return "Force ID (unsigned 8)"; 
		case 11300: return "Description"; 
		case 12000: return "Alternative Entity Type"; 
		case 12110: return "Kind (unsigned 8)"; 
		case 12120: return "Domain (unsigned 8)"; 
		case 12130: return "Country (unsigned 16)"; 
		case 12140: return "Category (unsigned 8)"; 
		case 12150: return "Subcategory (unsigned 8)"; 
		case 12160: return "Specific (unsigned 8)"; 
		case 12170: return "Extra (unsigned 8)"; 
		case 12300: return "Description"; 
		case 13000: return "Entity Marking"; 
		case 13100: return "Entity Marking String (string 80)"; 
		case 13200: return "Crew ID (string 80)"; 
		case 14000: return "Task Organization"; 
		case 14200: return "Regiment Name (string)"; 
		case 14300: return "Battalion Name (string)"; 
		case 14400: return "Company Name (string)"; 
		case 14500: return "Platoon Name"; 
		case 14520: return "Squad Name"; 
		case 14540: return "Team Name"; 
		case 14600: return "Bumper Name"; 
		case 14700: return "Vehicle Name"; 
		case 14800: return "Unit Number"; 
		case 15000: return "DIS Identity"; 
		case 15100: return "DIS Site ID"; 
		case 15200: return "DIS Host ID"; 
		case 15300: return "DIS Entity ID"; 
		case 20000: return "Loads"; 
		case 21000: return "Crew Members"; 
		case 21100: return "Crew Member ID"; 
		case 21200: return "Health"; 
		case 21300: return "Job Assignment (string)"; 
		case 23000: return "Fuel"; 
		case 23100: return "Quantity (liters)"; 
		case 23105: return "Quantity (gallons)"; 
		case 24000: return "Ammunition"; 
		case 24001: return "120-mm HEAT, quantity (Rounds)"; 
		case 24002: return "120-mm SABOT, quantity (Rounds)"; 
		case 24003: return "127-mm M8, quantity (Rounds)"; 
		case 24004: return "127-mm M20, quantity (Rounds)"; 
		case 24005: return "762-mm M62, quantity (Rounds)"; 
		case 24006: return "M250 UKL8A1, quantity (Grenades)"; 
		case 24007: return "M250 UKL8A3, quantity (Grenades)"; 
		case 24008: return "762-mm M80, quantity (Rounds)"; 
		case 24009: return "127-mm, quantity (Rounds)"; 
		case 24010: return "762-mm, quantity (Rounds)"; 
		case 24060: return "Mines, quantity (Mines)"; 
		case 24100: return "Type"; 
		case 24110: return "Kind"; 
		case 24120: return "Domain"; 
		case 24130: return "Country"; 
		case 24140: return "Category"; 
		case 24150: return "Subcategory"; 
		case 24160: return "Extra"; 
		case 24300: return "Description"; 
		case 25000: return "Cargo"; 
		case 26000: return "Vehicle Mass (unsigned 32 Kilograms)"; 
		case 27000: return "Supply Quantity"; 
		case 28000: return "Armament"; 
		case 30000: return "Status"; 
		case 31000: return "Position"; 
		case 31100: return "MilGrid10"; 
		case 31200: return "Geocentric Coordinates"; 
		case 31210: return "X (unsigned 32 meters)"; 
		case 31220: return "Y (unsigned 32 meters)"; 
		case 31230: return "Z (unsigned 32 meters)"; 
		case 31300: return "Latitude"; 
		case 31400: return "Longitude"; 
		case 31500: return "Line of Sight"; 
		case 31510: return "X"; 
		case 31520: return "Y"; 
		case 31530: return "Z"; 
		case 32000: return "Orientation"; 
		case 32100: return "Hull Heading Angle (degrees)"; 
		case 32200: return "Hull Pitch Angle"; 
		case 32300: return "Roll Angle"; 
		case 32500: return "X (unsigned 32 degrees)"; 
		case 32600: return "Y (unsigned 32 degrees)"; 
		case 32700: return "Z (unsigned 32 degrees)"; 
		case 33000: return "Appearance"; 
		case 33100: return "Ambient Lighting"; 
		case 33101: return "Lights"; 
		case 33200: return "Paint Scheme"; 
		case 33300: return "Smoke"; 
		case 33400: return "Trailing Effects"; 
		case 33500: return "Flaming"; 
		case 33600: return "Marking"; 
		case 33710: return "Mine Plows Attached"; 
		case 33720: return "Mine Rollers Attached"; 
		case 33730: return "Tank Turret Azimuth (degrees Rel 2 Ion)"; 
		case 34000: return "Failures and Malfunctions"; 
		case 34100: return "Age (miles)"; 
		case 34110: return "Kilometers"; 
		case 35000: return "Damage"; 
		case 35050: return "Cause"; 
		case 35100: return "Mobility Kill"; 
		case 35200: return "Fire-Power Kill"; 
		case 35300: return "Personnel Casualties"; 
		case 36000: return "Velocity"; 
		case 36100: return "X-velocity (meters/second)"; 
		case 36200: return "Y-velocity (meters/second)"; 
		case 36300: return "Z-velocity (meters/second)"; 
		case 37000: return "Acceleration"; 
		case 37100: return "X-acceleration"; 
		case 37200: return "Y-acceleration"; 
		case 37300: return "Z-acceleration"; 
		case 38100: return "Engine Status"; 
		case 40000: return "Exercise"; 
		case 40010: return "Exercise State"; 
		case 40020: return "AFATDS File Name"; 
		case 41000: return "Terrain Database"; 
		case 41001: return ""; 
		case 42000: return "Missions"; 
		case 42100: return "Mission ID"; 
		case 42200: return "Mission Type"; 
		case 42300: return "Mission Request Time Stamp"; 
		case 43000: return "Exercise Description (string)"; 
		case 43100: return "Name (string)"; 
		case 43200: return "Entities (integer)"; 
		case 43300: return "Version"; 
		case 43410: return "Guise Mode (Unsigned Integer 32)"; 
		case 43430: return "Simulation Application Role Record (Variable Record) 64"; 
		case 43440: return "Simulation Application State (Variable Record) 64"; 
		case 44000: return "Visual Output Mode "; 
		case 44100: return "Simulation Manager Role (Variable Record)"; 
		case 44110: return "Simulation Manager Site ID (Unsigned Integer 16)"; 
		case 44120: return "Simulation Manager Applic ID (Unsigned Integer 16)"; 
		case 44130: return "Simulation Manager Entity ID (Unisgned Integer 16)"; 
		case 44140: return "Simulation Manager Active Status (Unsigned Integer 16; 0  Backup, 1  Primary)"; 
		case 44200: return "After Active Review Role (Variable Record)"; 
		case 44210: return "After Active Review Site ID (Unsigned Integer 16)"; 
		case 44220: return "After Active Applic ID (Unsigned Integer 16)"; 
		case 44230: return "After Active Review Entity ID (Unsigned Integer 16)"; 
		case 44240: return "After Active Review Active Status (Unsigned Integer 16; 0  Backup, 1  Primary"; 
		case 44300: return "Exercise Logger Role (Variable Record)"; 
		case 44310: return "Exercise Logger Site ID (Unsigned Integer 16)"; 
		case 44320: return "Exercise Logger Applic ID (Unsigned Integer 16)"; 
		case 44330: return "Exercise Entity ID (Unsigned Integer 16)"; 
		case 44340: return "Exercise Logger Active Status (Unsigned Integer 16; 0  Backup, 1  Primary)"; 
		case 44400: return "Synthetic Environment Manager Role  (Variable Record)"; 
		case 44410: return "Synthetic Environment Manager Site ID (Unsigned Integer 16)"; 
		case 44420: return "Synthetic Environment Manager Applic ID (Unsigned Integer 16)"; 
		case 44430: return "Synthetic Environment Manager Entity ID (Unsigned Integer 16)"; 
		case 44440: return "Synthetic Environment Manager Active Status (Unsigned Integer 16 0  Backup, 1  Primary)"; 
		case 44500: return "SIMNET-DIS Translator Role (Variable Record)"; 
		case 44510: return "SIMNET-DIS Translator Site ID (Unsigned Integer 16)"; 
		case 44520: return "SIMNET-DIS Translator Applic ID (Unsigned Integer 16)"; 
		case 44530: return "SIMNET-DIS Translator Entity ID (Unsigned Integer 16)"; 
		case 44540: return "SIMNET-DIS Translator Active Status (Unsigned Integer 16 0  Backup, 1  Primary)"; 
		case 50000: return "Environment"; 
		case 51000: return "Weather"; 
		case 51100: return "Thermal Condition"; 
		case 52000: return "Time"; 
		case 52100: return "Time of Day, Discrete"; 
		case 52200: return "Time of Day, Continuous"; 
		case 52300: return "Time Mode"; 
		case 52305: return "Time Scene"; 
		case 52310: return "Current Hour"; 
		case 52320: return "Current Minute"; 
		case 52330: return "Current Second"; 
		case 52340: return "Azimuth"; 
		case 52350: return "Maximum Elevation"; 
		case 52360: return "Time Zone"; 
		case 52400: return "Time Sunrise Enabled"; 
		case 52410: return "Sunrise Hour"; 
		case 52420: return "Sunrise Minute"; 
		case 52430: return "Sunrise Second"; 
		case 52440: return "Sunrise Azimuth"; 
		case 52500: return "Time Sunset Enabled"; 
		case 52510: return "Sunset Hour"; 
		case 52511: return "Sunset Hour"; 
		case 52520: return "Sunset Minute"; 
		case 52530: return "SunsetSecond"; 
		case 52531: return ""; 
		case 52600: return "Date"; 
		case 52610: return "Month"; 
		case 52620: return "Day"; 
		case 52630: return "Year"; 
		case 53000: return "Clouds"; 
		case 53050: return "Cloud Layer Enable"; 
		case 53060: return "Cloud Layer Selection"; 
		case 53100: return "Visibility"; 
		case 53200: return "Base Altitude (meters)"; 
		case 53250: return "Base Altitude (feet)"; 
		case 53300: return "Ceiling (meters)"; 
		case 53350: return "Ceiling (feet)"; 
		case 53400: return "Characterictics"; 
		case 53410: return "Concentration Length (Floating Point 32) (milligrams/meter ²)"; 
		case 53420: return "Transmittance (Floating Point 32)"; 
		case 53430: return "Radiance (Floating Point 32) (microwatts/centimeter²/steradian)"; 
		case 54000: return "Precipitation"; 
		case 54100: return "Rain (boolean)"; 
		case 55000: return "Fog (boolean)"; 
		case 55100: return "Visibility (meters)"; 
		case 55105: return "Visibility (miles)"; 
		case 55200: return "Density"; 
		case 55300: return "Base"; 
		case 55401: return "View Layer from above???"; 
		case 55410: return "Transition Range"; 
		case 55420: return "Bottom (meters)"; 
		case 55425: return "Bottom (feet)"; 
		case 55430: return "Ceiling (meters)"; 
		case 55435: return "Ceiling (feet)"; 
		case 56000: return "Heavenly Bodies"; 
		case 56100: return "Sun"; 
		case 56110: return "Position"; 
		case 56120: return "Position Azimuth"; 
		case 56130: return "Position Elevation"; 
		case 56140: return "Position Intensity"; 
		case 56200: return "Moon"; 
		case 56210: return "Position"; 
		case 56220: return "Position Azimuth"; 
		case 56230: return "Position Elevation"; 
		case 56240: return "Position Intensity"; 
		case 56310: return "Horizon"; 
		case 56320: return "Horizon Azimuth"; 
		case 56330: return "Horizon Elevation"; 
		case 56340: return "Horizon Heading"; 
		case 56350: return "Horizon Intensity"; 
		case 57000: return "Meteorological"; 
		case 57100: return "Temperature"; 
		case 57200: return "Humidity"; 
		case 57300: return "Visibility"; 
		case 57400: return "Winds"; 
		case 57410: return "Speed"; 
		case 57500: return "Rainsoak"; 
		case 58000: return "Haze (boolean)"; 
		case 58100: return "Visibility (meters)"; 
		case 58105: return "Visibility (feet)"; 
		case 58200: return "Density"; 
		case 58430: return "Ceiling (meters)"; 
		case 58435: return "Ceiling (feet)"; 
		case 59000: return "Contaminants and Obsurants"; 
		case 59100: return "Contaminant/Obscurant Type (Unsigned Integer 32)"; 
		case 59110: return "Persistence (Enumeration 8)"; 
		case 59115: return "Chemical Dosage (Floating Point 32) (milligrams/meter³/minute)"; 
		case 59120: return "Chemical Air Concentration (Floating Point 32) (milligrams/meter³)"; 
		case 59125: return "Chemical Ground Deposition (Floating Point 32) (milligrams/meter²)"; 
		case 59130: return "Chemical Maximum Ground Deposition (Floating Point 32) (milligrams/meter²)"; 
		case 59135: return "Chemical Dosage Threshold (Floating Point 32) (milligram/meter³/minute)"; 
		case 59140: return "Biological Dosage (Floating Point 32) (particles/liter of air/minute)"; 
		case 59145: return "Biological Air Concentration (Floating Point 32) ( particles/liter of air)"; 
		case 59150: return "Biological Dosage Threshold (Floating Point 32) (particles/liter of air/minute)"; 
		case 59155: return "Biological Binned Particle Count (Enumeration 8) ( 1  Low (5-2), 2  Detection (2-10), 3  High (10-15 )"; 
		case 59160: return "Radiological Dosage (Floating Point 32)"; 
		case 60000: return "Communications"; 
		case 61100: return "Channel Type"; 
		case 61101: return "Channel Type"; 
		case 61200: return "Channel Identification"; 
		case 61300: return "Alpha Identification"; 
		case 61301: return ""; 
		case 61400: return "Radio Identification"; 
		case 61401: return ""; 
		case 61500: return "Land Line Identification"; 
		case 61600: return "Intercom Identification"; 
		case 61700: return "Group Network Channel Number"; 
		case 62100: return "Radio Communications Status"; 
		case 62200: return "Stationary Radio Transmitters Default Time (unsigned)"; 
		case 62300: return "Moving Radio Transmitters Default Time (unsigned)"; 
		case 62400: return "Stationary Radio Signals Default Time"; 
		case 62500: return "Moving Radio Signals Default Time"; 
		case 63101: return "Radio Initialization Transec Security Key (record)"; 
		case 63102: return "Radio Initialization Internal Noise Level (record)"; 
		case 63103: return "Radio Initialization Squelch Threshold (record)"; 
		case 63104: return "Radio Initialization Antenna Location (record)"; 
		case 63105: return "Radio Initialization Antenna Pattern Type (record)"; 
		case 63106: return "Radio Initialization Antenna Pattern Length (record)"; 
		case 63107: return "Radio Initialization Beam Definition (record)"; 
		case 63108: return "Radio Initialization Transmit Heartbeat Time (record)"; 
		case 63109: return "Radio Initialization Transmit Threshold Distance (record)"; 
		case 63110: return "Radio Channel Initialization Lockout ID (record)"; 
		case 63111: return "Radio Channel Initialization Hopset ID (record)"; 
		case 63112: return "Radio Channel Initialization Preset Frequency (record)"; 
		case 63113: return "Radio Channel Initialization Frequency Sync Time (record)"; 
		case 63114: return "Radio Channel Initialization Comsec Key (record)"; 
		case 63115: return "Radio Channel Initialization Alpha (record)"; 
		case 70000: return "Algorithm Parameters"; 
		case 71000: return "Dead Reckoning Algorithm (DRA)"; 
		case 71100: return "DRA Location Threshold (unsigned 32)"; 
		case 71200: return "DRA Orientation Threshold"; 
		case 71300: return "DRA Time Threshold"; 
		case 72000: return "Simulation Management Parameters"; 
		case 72100: return "Checkpoint Interval"; 
		case 72600: return "Transmitter Time Threshold"; 
		case 72700: return "Receiver Time Threshold"; 
		case 73000: return "Interoperability Mode"; 
		case 74000: return "SIMNET Data Collection (record)"; 
		case 75000: return "Event ID"; 
		case 75100: return "Source Site ID"; 
		case 75200: return "Source Host ID"; 
		case 90000: return "Articulated Parts"; 
		case 90001: return ""; 
		case 90050: return "Part ID"; 
		case 90070: return "Index (See 472)"; 
		case 90100: return "Position"; 
		case 90200: return "Position Rate"; 
		case 90300: return "Extension"; 
		case 90400: return "Extension Rate"; 
		case 90500: return "X"; 
		case 90600: return "X-rate"; 
		case 90700: return "Y"; 
		case 90800: return "Y-rate"; 
		case 90900: return "Z"; 
		case 91000: return "Z-rate"; 
		case 91100: return "Azimuth"; 
		case 91200: return "Azimuth Rate"; 
		case 91300: return "Elevation"; 
		case 91400: return "Elevation Rate"; 
		case 91500: return "Rotation"; 
		case 91600: return "Rotation Rate"; 
		default : return "";
	}
}//end of toString
}//End of class 
