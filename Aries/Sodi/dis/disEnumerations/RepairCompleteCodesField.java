/*
 File:		RepairCompleteCodesField.java
 CVS Info:      $Id$
 Compiler:	jdk 1.2.2
 */

package disEnumerations;

import mil.navy.nps.dis.*;
import mil.navy.nps.util.*;

/**
 * Repair Complete Codes Field -- Repair types shall be specified by a 16-bit enumeration. Values defined for this field are found in Section 6 in EBV-DOC.
 *@version 1.1
 *@author Ronan Fauglas
 *@author <a href="mailto:brutzman@nps.navy.mil?subject=dis-java-vrml: disEnumerations.RepairCompleteCodesField feedback ">Don Brutzman</a>
 *
 *<dt><b>References:</b>
 *<dd>		DIS Data Dictionary: <A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/95.htm">Repair Complete Codes Field</A> (local)
 *				     <A href="http://SISO.sc.ist.ucf.edu/dis/dis-dd/pdu/95.htm">Repair Complete Codes Field</A> (SISO)
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
 *  You access this via something like <b><code>RepairCompleteCodesField.LAUNCHERS</code></b>, i.e. combine
 *  the class name, a period, and a class variable (enumeration) name.<P>
 *
 *<dt><b>History:</b>
 *<dd>		21jan98   /Ronan Fauglas   	/New
 *<dd>		30mar99   /Don Brutzman   	/Revised Javadoc, many more enumeration classes
 *
 *<dt><b>Location:</b>
 *<dd><a href="../../../../../../mil/navy/nps/disEnumerations/RepairCompleteCodesField.java"><i>RepairCompleteCodesField.java</i></A> (local)
 *<dd><a href="http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/RepairCompleteCodesField.java">
 *          <i>http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/RepairCompleteCodesField.java</i></a>
 *
 */
public class RepairCompleteCodesField extends Object
{
/**
 *(0) no repairs performed
 */
public static final short NOREPAIRSPERFORMED = 0;

/**
 *(1) all requested repairs performed
 */
public static final short ALLREQUESTEDREPAIRSPERFORMED = 1;

/**
 *(10) motor / engine
 */
public static final short MOTORENGINE = 10;

/**
 *(100) pumps
 */
public static final short PUMPS = 100;

/**
 *(1000) hull
 */
public static final short HULL = 1000;

/**
 *(10000) life boats
 */
public static final short LIFEBOATS = 10000;

/**
 *(10010) landing craft
 */
public static final short LANDINGCRAFT = 10010;

/**
 *(10020) ejection seats
 */
public static final short EJECTIONSEATS = 10020;

/**
 *(1010) airframe
 */
public static final short AIRFRAME = 1010;

/**
 *(1020) truck body
 */
public static final short TRUCKBODY = 1020;

/**
 *(1030) tank body
 */
public static final short TANKBODY = 1030;

/**
 *(1040) trailer body
 */
public static final short TRAILERBODY = 1040;

/**
 *(1050) turret
 */
public static final short TURRET = 1050;

/**
 *(110) filters
 */
public static final short FILTERS = 110;

/**
 *(120) transmission
 */
public static final short TRANSMISSION = 120;

/**
 *(130) brakes
 */
public static final short BRAKES = 130;

/**
 *(140) suspension system
 */
public static final short SUSPENSIONSYSTEM = 140;

/**
 *(150) oil filter
 */
public static final short OILFILTER = 150;

/**
 *(1500) propeller
 */
public static final short PROPELLER = 1500;

/**
 *(1520) filters
 */
public static final short FILTERS2 = 1520;

/**
 *(1540) wheels
 */
public static final short WHEELS = 1540;

/**
 *(1550) tire
 */
public static final short TIRE = 1550;

/**
 *(1560) track
 */
public static final short TRACK = 1560;

/**
 *(20) starter
 */
public static final short STARTER = 20;

/**
 *(2000) gun elevation drive
 */
public static final short GUNELEVATIONDRIVE = 2000;

/**
 *(2010) gun stabilization system
 */
public static final short GUNSTABILIZATIONSYSTEM = 2010;

/**
 *(2020) gunner's primary isght (GPS)
 */
public static final short GUNNERSPRIMARYISGHT = 2020;

/**
 *(2030) commander's extension to the GPS
 */
public static final short COMMANDERSEXTENSIONTOTHEGPS = 2030;

/**
 *(2040) loading mechanism
 */
public static final short LOADINGMECHANISM = 2040;

/**
 *(2050) gunner's auxiliary sight
 */
public static final short GUNNERSAUXILIARYSIGHT = 2050;

/**
 *(2060) gunner's control panel
 */
public static final short GUNNERSCONTROLPANEL = 2060;

/**
 *(2070) gunner's control assembly handle(s)
 */
public static final short GUNNERSCONTROLASSEMBLYHANDLE = 2070;

/**
 *(2090) commander's control handles/assembly
 */
public static final short COMMANDERSCONTROLHANDLESASSEMBLY = 2090;

/**
 *(2100) commander's weapon station
 */
public static final short COMMANDERSWEAPONSTATION = 2100;

/**
 *(2110) commander's independent thermal viewer (CITV)
 */
public static final short COMMANDERSINDEPENDENTTHERMALVIEWER = 2110;

/**
 *(2120) general weapons
 */
public static final short GENERALWEAPONS = 2120;

/**
 *(30) alternator
 */
public static final short ALTERNATOR = 30;

/**
 *(40) generator
 */
public static final short GENERATOR = 40;

/**
 *(4000) fuel transfer pump
 */
public static final short FUELTRANSFERPUMP = 4000;

/**
 *(4010) fuel lines
 */
public static final short FUELLINES = 4010;

/**
 *(4020) gauges
 */
public static final short GAUGES = 4020;

/**
 *(4030) general fuel system
 */
public static final short GENERALFUELSYSTEM = 4030;

/**
 *(4500) electronic warfare systems
 */
public static final short ELECTRONICWARFARESYSTEMS = 4500;

/**
 *(4600) detection systems
 */
public static final short DETECTIONSYSTEMS = 4600;

/**
 *(4610) radio frequency
 */
public static final short RADIOFREQUENCY = 4610;

/**
 *(4620) microwave
 */
public static final short MICROWAVE = 4620;

/**
 *(4630) infrared
 */
public static final short INFRARED = 4630;

/**
 *(4640) laser
 */
public static final short LASER = 4640;

/**
 *(4700) range finders
 */
public static final short RANGEFINDERS = 4700;

/**
 *(4710) range-only radar
 */
public static final short RANGEONLYRADAR = 4710;

/**
 *(4720) laser range finder
 */
public static final short LASERRANGEFINDER = 4720;

/**
 *(4800) electronic systems
 */
public static final short ELECTRONICSYSTEMS = 4800;

/**
 *(4810) radio frequency
 */
public static final short RADIOFREQUENCY2 = 4810;

/**
 *(4820) microwave
 */
public static final short MICROWAVE2 = 4820;

/**
 *(4830) infrared
 */
public static final short INFRARED2 = 4830;

/**
 *(4840) laser
 */
public static final short LASER2 = 4840;

/**
 *(50) battery
 */
public static final short BATTERY = 50;

/**
 *(5000) radios
 */
public static final short RADIOS = 5000;

/**
 *(5010) communication systems
 */
public static final short COMMUNICATIONSYSTEMS = 5010;

/**
 *(5100) intercoms
 */
public static final short INTERCOMS = 5100;

/**
 *(5200) encoders
 */
public static final short ENCODERS = 5200;

/**
 *(5250) encryption devices
 */
public static final short ENCRYPTIONDEVICES = 5250;

/**
 *(5300) decoders
 */
public static final short DECODERS = 5300;

/**
 *(5350) decryption devices
 */
public static final short DECRYPTIONDEVICES = 5350;

/**
 *(5500) computers
 */
public static final short COMPUTERS = 5500;

/**
 *(60) engine-coolant leak
 */
public static final short ENGINECOOLANTLEAK = 60;

/**
 *(6000) navigation and control systems
 */
public static final short NAVIGATIONANDCONTROLSYSTEMS = 6000;

/**
 *(6500) fire control systems
 */
public static final short FIRECONTROLSYSTEMS = 6500;

/**
 *(70) fuel filter
 */
public static final short FUELFILTER = 70;

/**
 *(80) transmission-oil leak
 */
public static final short TRANSMISSIONOILLEAK = 80;

/**
 *(8000) air supply
 */
public static final short AIRSUPPLY = 8000;

/**
 *(8010) filters
 */
public static final short FILTERS3 = 8010;

/**
 *(8020) water supply
 */
public static final short WATERSUPPLY = 8020;

/**
 *(8030) refrigeration system
 */
public static final short REFRIGERATIONSYSTEM = 8030;

/**
 *(8040) chemical, biological, and radiologic protection
 */
public static final short CHEMICALBIOLOGICALANDRADIOLOGICPROTECTION = 8040;

/**
 *(8050) water wash down systems
 */
public static final short WATERWASHDOWNSYSTEMS = 8050;

/**
 *(8060) decontamination systems
 */
public static final short DECONTAMINATIONSYSTEMS = 8060;

/**
 *(90) engine-oil leak
 */
public static final short ENGINEOILLEAK = 90;

/**
 *(9000) water supply
 */
public static final short WATERSUPPLY2 = 9000;

/**
 *(9010) cooling system
 */
public static final short COOLINGSYSTEM = 9010;

/**
 *(9020) winches
 */
public static final short WINCHES = 9020;

/**
 *(9030) catapults
 */
public static final short CATAPULTS = 9030;

/**
 *(9040) cranes
 */
public static final short CRANES = 9040;

/**
 *(9050) launchers
 */
public static final short LAUNCHERS = 9050;

/**
 * Returns a string containing the enumeration name which corresponds to an enumeration value,
 * as in <b><code>RepairCompleteCodesField.toString (0)</code></b> returns the string "<b><code>NOREPAIRSPERFORMED</code></b>"
 */

public static String toString(int idNumber) 
{
	switch (idNumber) {
		case 0: return "no repairs performed"; 
		case 1: return "all requested repairs performed"; 
		case 10: return "motor / engine"; 
		case 100: return "pumps"; 
		case 1000: return "hull"; 
		case 10000: return "life boats"; 
		case 10010: return "landing craft"; 
		case 10020: return "ejection seats"; 
		case 1010: return "airframe"; 
		case 1020: return "truck body"; 
		case 1030: return "tank body"; 
		case 1040: return "trailer body"; 
		case 1050: return "turret"; 
		case 110: return "filters"; 
		case 120: return "transmission"; 
		case 130: return "brakes"; 
		case 140: return "suspension system"; 
		case 150: return "oil filter"; 
		case 1500: return "propeller"; 
		case 1520: return "filters"; 
		case 1540: return "wheels"; 
		case 1550: return "tire"; 
		case 1560: return "track"; 
		case 20: return "starter"; 
		case 2000: return "gun elevation drive"; 
		case 2010: return "gun stabilization system"; 
		case 2020: return "gunner's primary isght (GPS)"; 
		case 2030: return "commander's extension to the GPS"; 
		case 2040: return "loading mechanism"; 
		case 2050: return "gunner's auxiliary sight"; 
		case 2060: return "gunner's control panel"; 
		case 2070: return "gunner's control assembly handle(s)"; 
		case 2090: return "commander's control handles/assembly"; 
		case 2100: return "commander's weapon station"; 
		case 2110: return "commander's independent thermal viewer (CITV)"; 
		case 2120: return "general weapons"; 
		case 30: return "alternator"; 
		case 40: return "generator"; 
		case 4000: return "fuel transfer pump"; 
		case 4010: return "fuel lines"; 
		case 4020: return "gauges"; 
		case 4030: return "general fuel system"; 
		case 4500: return "electronic warfare systems"; 
		case 4600: return "detection systems"; 
		case 4610: return "radio frequency"; 
		case 4620: return "microwave"; 
		case 4630: return "infrared"; 
		case 4640: return "laser"; 
		case 4700: return "range finders"; 
		case 4710: return "range-only radar"; 
		case 4720: return "laser range finder"; 
		case 4800: return "electronic systems"; 
		case 4810: return "radio frequency"; 
		case 4820: return "microwave"; 
		case 4830: return "infrared"; 
		case 4840: return "laser"; 
		case 50: return "battery"; 
		case 5000: return "radios"; 
		case 5010: return "communication systems"; 
		case 5100: return "intercoms"; 
		case 5200: return "encoders"; 
		case 5250: return "encryption devices"; 
		case 5300: return "decoders"; 
		case 5350: return "decryption devices"; 
		case 5500: return "computers"; 
		case 60: return "engine-coolant leak"; 
		case 6000: return "navigation and control systems"; 
		case 6500: return "fire control systems"; 
		case 70: return "fuel filter"; 
		case 80: return "transmission-oil leak"; 
		case 8000: return "air supply"; 
		case 8010: return "filters"; 
		case 8020: return "water supply"; 
		case 8030: return "refrigeration system"; 
		case 8040: return "chemical, biological, and radiologic protection"; 
		case 8050: return "water wash down systems"; 
		case 8060: return "decontamination systems"; 
		case 90: return "engine-oil leak"; 
		case 9000: return "water supply"; 
		case 9010: return "cooling system"; 
		case 9020: return "winches"; 
		case 9030: return "catapults"; 
		case 9040: return "cranes"; 
		case 9050: return "launchers"; 
		default : return "";
	}
}//end of toString
}//End of class 
