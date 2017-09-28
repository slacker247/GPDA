/*
 File:		FuseField.java
 CVS Info:      $Id$
 Compiler:	jdk 1.2.2
 */

package disEnumerations;

import mil.navy.nps.dis.*;
import mil.navy.nps.util.*;

/**
 * Fuse Field -- The fuse shall be specified by a 16-bit enumeration (see Section 5 in EBV-DOC)
 *@version 1.1
 *@author Ronan Fauglas
 *@author <a href="mailto:brutzman@nps.navy.mil?subject=dis-java-vrml: disEnumerations.FuseField feedback ">Don Brutzman</a>
 *
 *<dt><b>References:</b>
 *<dd>		DIS Data Dictionary: <A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/80.htm">Fuse Field</A> (local)
 *				     <A href="http://SISO.sc.ist.ucf.edu/dis/dis-dd/pdu/80.htm">Fuse Field</A> (SISO)
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
 *  You access this via something like <b><code>FuseField.MECHANICALTAIL</code></b>, i.e. combine
 *  the class name, a period, and a class variable (enumeration) name.<P>
 *
 *<dt><b>History:</b>
 *<dd>		21jan98   /Ronan Fauglas   	/New
 *<dd>		30mar99   /Don Brutzman   	/Revised Javadoc, many more enumeration classes
 *
 *<dt><b>Location:</b>
 *<dd><a href="../../../../../../mil/navy/nps/disEnumerations/FuseField.java"><i>FuseField.java</i></A> (local)
 *<dd><a href="http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/FuseField.java">
 *          <i>http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/FuseField.java</i></a>
 *
 */
public class FuseField extends Object
{
/**
 *(0000) Other
 */
public static final short OTHER = 0000;

/**
 *(0010) Intelligent Influence
 */
public static final short INTELLIGENTINFLUENCE = 0010;

/**
 *(0020) Sensor
 */
public static final short SENSOR = 0020;

/**
 *(0030) Self-destruct
 */
public static final short SELFDESTRUCT = 0030;

/**
 *(0040) Ultra Quick
 */
public static final short ULTRAQUICK = 0040;

/**
 *(0050) Body
 */
public static final short BODY = 0050;

/**
 *(0060) Deep Intrusion
 */
public static final short DEEPINTRUSION = 0060;

/**
 *(0100) Multifunction
 */
public static final short MULTIFUNCTION = 0100;

/**
 *(0200) Point Detonation (PD)
 */
public static final short POINTDETONATION = 0200;

/**
 *(0300) Base Detonation (BD)
 */
public static final short BASEDETONATION = 0300;

/**
 *(1000) Contact
 */
public static final short CONTACT = 1000;

/**
 *(1100) Contact, Instant (Impact)
 */
public static final short CONTACTINSTANT = 1100;

/**
 *(1200) Contact, Delayed
 */
public static final short CONTACTDELAYED = 1200;

/**
 *(1300) Contact, Electronic (Oblique Contact)
 */
public static final short CONTACTELECTRONIC = 1300;

/**
 *(1400) Contact, Graze
 */
public static final short CONTACTGRAZE = 1400;

/**
 *(1500) Contact, Crush
 */
public static final short CONTACTCRUSH = 1500;

/**
 *(1600) Contact, Hydrostatic
 */
public static final short CONTACTHYDROSTATIC = 1600;

/**
 *(1700) Contact, Mechanical
 */
public static final short CONTACTMECHANICAL = 1700;

/**
 *(1800) Contact, Chemical
 */
public static final short CONTACTCHEMICAL = 1800;

/**
 *(1900) Contact, Piezoelectric
 */
public static final short CONTACTPIEZOELECTRIC = 1900;

/**
 *(1910) Contact, Point Initiating
 */
public static final short CONTACTPOINTINITIATING = 1910;

/**
 *(1920) Contact, Point Initiating, Base Detonating
 */
public static final short CONTACTPOINTINITIATINGBASEDETONATING = 1920;

/**
 *(1930) Contact, Base Detonating
 */
public static final short CONTACTBASEDETONATING = 1930;

/**
 *(1940) Contact, Ballistic Cap and Base
 */
public static final short CONTACTBALLISTICCAPANDBASE = 1940;

/**
 *(1950) Contact, Base
 */
public static final short CONTACTBASE = 1950;

/**
 *(1960) Contact, Nose
 */
public static final short CONTACTNOSE = 1960;

/**
 *(1970) Contact, Fitted in Standoff Probe
 */
public static final short CONTACTFITTEDINSTANDOFFPROBE = 1970;

/**
 *(1980) Contact, Non-aligned
 */
public static final short CONTACTNONALIGNED = 1980;

/**
 *(2000) Timed
 */
public static final short TIMED = 2000;

/**
 *(2100) Timed, Programmable
 */
public static final short TIMEDPROGRAMMABLE = 2100;

/**
 *(2200) Timed, Burnout
 */
public static final short TIMEDBURNOUT = 2200;

/**
 *(2300) Timed, Pyrotechnic
 */
public static final short TIMEDPYROTECHNIC = 2300;

/**
 *(2400) Timed, Electronic
 */
public static final short TIMEDELECTRONIC = 2400;

/**
 *(2500) Timed, Base Delay
 */
public static final short TIMEDBASEDELAY = 2500;

/**
 *(2600) Timed, Reinforced Nose Impact Delay
 */
public static final short TIMEDREINFORCEDNOSEIMPACTDELAY = 2600;

/**
 *(2700) Timed, Short Delay Impact
 */
public static final short TIMEDSHORTDELAYIMPACT = 2700;

/**
 *(2800) Timed, Nose Mounted Variable Delay
 */
public static final short TIMEDNOSEMOUNTEDVARIABLEDELAY = 2800;

/**
 *(2900) Timed, Long Delay Side
 */
public static final short TIMEDLONGDELAYSIDE = 2900;

/**
 *(2910) Timed, Selectable Delay
 */
public static final short TIMEDSELECTABLEDELAY = 2910;

/**
 *(2920) Timed, Impact
 */
public static final short TIMEDIMPACT = 2920;

/**
 *(2930) Timed, Sequence
 */
public static final short TIMEDSEQUENCE = 2930;

/**
 *(3000) Proximity
 */
public static final short PROXIMITY = 3000;

/**
 *(3100) Proximity, Active Laser
 */
public static final short PROXIMITYACTIVELASER = 3100;

/**
 *(3200) Proximity, Magnetic (Magpolarity)
 */
public static final short PROXIMITYMAGNETIC = 3200;

/**
 *(3300) Proximity, Active Radar (Doppler Radar)
 */
public static final short PROXIMITYACTIVERADAR = 3300;

/**
 *(3400) Proximity, Radio Frequency (RF)
 */
public static final short PROXIMITYRADIOFREQUENCY = 3400;

/**
 *(3500) Proximity, Programmable
 */
public static final short PROXIMITYPROGRAMMABLE = 3500;

/**
 *(3600) Proximity, Programmable, Prefragmented
 */
public static final short PROXIMITYPROGRAMMABLEPREFRAGMENTED = 3600;

/**
 *(3700) Proximity, Infrared
 */
public static final short PROXIMITYINFRARED = 3700;

/**
 *(4000) Command
 */
public static final short COMMAND = 4000;

/**
 *(4100) Command, Electronic, Remotely Set
 */
public static final short COMMANDELECTRONICREMOTELYSET = 4100;

/**
 *(5000) Altitude
 */
public static final short ALTITUDE = 5000;

/**
 *(5100) Altitude, Radio Altimeter
 */
public static final short ALTITUDERADIOALTIMETER = 5100;

/**
 *(5200) Altitude, Air Burst
 */
public static final short ALTITUDEAIRBURST = 5200;

/**
 *(6000) Depth
 */
public static final short DEPTH = 6000;

/**
 *(7000) Acoustic
 */
public static final short ACOUSTIC = 7000;

/**
 *(8000) Pressure
 */
public static final short PRESSURE = 8000;

/**
 *(8010) Pressure, Delay
 */
public static final short PRESSUREDELAY = 8010;

/**
 *(8100) Inert
 */
public static final short INERT = 8100;

/**
 *(8110) Dummy
 */
public static final short DUMMY = 8110;

/**
 *(8120) Practice
 */
public static final short PRACTICE = 8120;

/**
 *(8130) Plug Representing
 */
public static final short PLUGREPRESENTING = 8130;

/**
 *(8150) Training
 */
public static final short TRAINING = 8150;

/**
 *(9000) Pyrotechnic
 */
public static final short PYROTECHNIC = 9000;

/**
 *(9010) Pyrotechnic, Delay
 */
public static final short PYROTECHNICDELAY = 9010;

/**
 *(9100) Electro-optical
 */
public static final short ELECTROOPTICAL = 9100;

/**
 *(9110) Electromechanical
 */
public static final short ELECTROMECHANICAL = 9110;

/**
 *(9120) Electromechanical, Nose
 */
public static final short ELECTROMECHANICALNOSE = 9120;

/**
 *(9200) Strikerless
 */
public static final short STRIKERLESS = 9200;

/**
 *(9210) Strikerless, Nose Impact
 */
public static final short STRIKERLESSNOSEIMPACT = 9210;

/**
 *(9220) Strikerless, Compression-Ignition
 */
public static final short STRIKERLESSCOMPRESSIONIGNITION = 9220;

/**
 *(9300) Compression-Ignition
 */
public static final short COMPRESSIONIGNITION = 9300;

/**
 *(9310) Compression-Ignition, Strikerless, Nose Impact
 */
public static final short COMPRESSIONIGNITIONSTRIKERLESSNOSEIMPACT = 9310;

/**
 *(9400) Percussion
 */
public static final short PERCUSSION = 9400;

/**
 *(9410) Percussion, Instantaneous
 */
public static final short PERCUSSIONINSTANTANEOUS = 9410;

/**
 *(9500) Electronic
 */
public static final short ELECTRONIC = 9500;

/**
 *(9510) Electronic, Internally Mounted
 */
public static final short ELECTRONICINTERNALLYMOUNTED = 9510;

/**
 *(9520) Electronic, Range Setting
 */
public static final short ELECTRONICRANGESETTING = 9520;

/**
 *(9530) Electronic, Programmed
 */
public static final short ELECTRONICPROGRAMMED = 9530;

/**
 *(9600) Mechanical
 */
public static final short MECHANICAL = 9600;

/**
 *(9610) Mechanical, Nose
 */
public static final short MECHANICALNOSE = 9610;

/**
 *(9620) Mechanical, Tail
 */
public static final short MECHANICALTAIL = 9620;

/**
 * Returns a string containing the enumeration name which corresponds to an enumeration value,
 * as in <b><code>FuseField.toString (0000)</code></b> returns the string "<b><code>OTHER</code></b>"
 */

public static String toString(int idNumber) 
{
	switch (idNumber) {
		case 0000: return "Other"; 
		case 0010: return "Intelligent Influence"; 
		case 0020: return "Sensor"; 
		case 0030: return "Self-destruct"; 
		case 0040: return "Ultra Quick"; 
		case 0050: return "Body"; 
		case 0060: return "Deep Intrusion"; 
		case 0100: return "Multifunction"; 
		case 0200: return "Point Detonation (PD)"; 
		case 0300: return "Base Detonation (BD)"; 
		case 1000: return "Contact"; 
		case 1100: return "Contact, Instant (Impact)"; 
		case 1200: return "Contact, Delayed"; 
		case 1300: return "Contact, Electronic (Oblique Contact)"; 
		case 1400: return "Contact, Graze"; 
		case 1500: return "Contact, Crush"; 
		case 1600: return "Contact, Hydrostatic"; 
		case 1700: return "Contact, Mechanical"; 
		case 1800: return "Contact, Chemical"; 
		case 1900: return "Contact, Piezoelectric"; 
		case 1910: return "Contact, Point Initiating"; 
		case 1920: return "Contact, Point Initiating, Base Detonating"; 
		case 1930: return "Contact, Base Detonating"; 
		case 1940: return "Contact, Ballistic Cap and Base"; 
		case 1950: return "Contact, Base"; 
		case 1960: return "Contact, Nose"; 
		case 1970: return "Contact, Fitted in Standoff Probe"; 
		case 1980: return "Contact, Non-aligned"; 
		case 2000: return "Timed"; 
		case 2100: return "Timed, Programmable"; 
		case 2200: return "Timed, Burnout"; 
		case 2300: return "Timed, Pyrotechnic"; 
		case 2400: return "Timed, Electronic"; 
		case 2500: return "Timed, Base Delay"; 
		case 2600: return "Timed, Reinforced Nose Impact Delay"; 
		case 2700: return "Timed, Short Delay Impact"; 
		case 2800: return "Timed, Nose Mounted Variable Delay"; 
		case 2900: return "Timed, Long Delay Side"; 
		case 2910: return "Timed, Selectable Delay"; 
		case 2920: return "Timed, Impact"; 
		case 2930: return "Timed, Sequence"; 
		case 3000: return "Proximity"; 
		case 3100: return "Proximity, Active Laser"; 
		case 3200: return "Proximity, Magnetic (Magpolarity)"; 
		case 3300: return "Proximity, Active Radar (Doppler Radar)"; 
		case 3400: return "Proximity, Radio Frequency (RF)"; 
		case 3500: return "Proximity, Programmable"; 
		case 3600: return "Proximity, Programmable, Prefragmented"; 
		case 3700: return "Proximity, Infrared"; 
		case 4000: return "Command"; 
		case 4100: return "Command, Electronic, Remotely Set"; 
		case 5000: return "Altitude"; 
		case 5100: return "Altitude, Radio Altimeter"; 
		case 5200: return "Altitude, Air Burst"; 
		case 6000: return "Depth"; 
		case 7000: return "Acoustic"; 
		case 8000: return "Pressure"; 
		case 8010: return "Pressure, Delay"; 
		case 8100: return "Inert"; 
		case 8110: return "Dummy"; 
		case 8120: return "Practice"; 
		case 8130: return "Plug Representing"; 
		case 8150: return "Training"; 
		case 9000: return "Pyrotechnic"; 
		case 9010: return "Pyrotechnic, Delay"; 
		case 9100: return "Electro-optical"; 
		case 9110: return "Electromechanical"; 
		case 9120: return "Electromechanical, Nose"; 
		case 9200: return "Strikerless"; 
		case 9210: return "Strikerless, Nose Impact"; 
		case 9220: return "Strikerless, Compression-Ignition"; 
		case 9300: return "Compression-Ignition"; 
		case 9310: return "Compression-Ignition, Strikerless, Nose Impact"; 
		case 9400: return "Percussion"; 
		case 9410: return "Percussion, Instantaneous"; 
		case 9500: return "Electronic"; 
		case 9510: return "Electronic, Internally Mounted"; 
		case 9520: return "Electronic, Range Setting"; 
		case 9530: return "Electronic, Programmed"; 
		case 9600: return "Mechanical"; 
		case 9610: return "Mechanical, Nose"; 
		case 9620: return "Mechanical, Tail"; 
		default : return "";
	}
}//end of toString
}//End of class 
