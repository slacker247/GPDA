/*
 File:		WarheadField.java
 CVS Info:      $Id$
 Compiler:	jdk 1.2.2
 */

package disEnumerations;

import mil.navy.nps.dis.*;
import mil.navy.nps.util.*;

/**
 * Warhead Field -- The warhead shall be specified by a 16-bit enumeration(see Section 5 in EBV-DOC)
 *@version 1.1
 *@author Ronan Fauglas
 *@author <a href="mailto:brutzman@nps.navy.mil?subject=dis-java-vrml: disEnumerations.WarheadField feedback ">Don Brutzman</a>
 *
 *<dt><b>References:</b>
 *<dd>		DIS Data Dictionary: <A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/7f.htm">Warhead Field</A> (local)
 *				     <A href="http://SISO.sc.ist.ucf.edu/dis/dis-dd/pdu/7f.htm">Warhead Field</A> (SISO)
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
 *  You access this via something like <b><code>WarheadField.BIOLOGICALTOXIN</code></b>, i.e. combine
 *  the class name, a period, and a class variable (enumeration) name.<P>
 *
 *<dt><b>History:</b>
 *<dd>		21jan98   /Ronan Fauglas   	/New
 *<dd>		30mar99   /Don Brutzman   	/Revised Javadoc, many more enumeration classes
 *
 *<dt><b>Location:</b>
 *<dd><a href="../../../../../../mil/navy/nps/disEnumerations/WarheadField.java"><i>WarheadField.java</i></A> (local)
 *<dd><a href="http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/WarheadField.java">
 *          <i>http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/WarheadField.java</i></a>
 *
 */
public class WarheadField_2 extends Object
{
/**
 *(0000) Other
 */
public static final short OTHER = 0000;

/**
 *(0010) Cargo (Variable Submunitions)
 */
public static final short CARGO = 0010;

/**
 *(0020) Fuel/Air Explosive
 */
public static final short FUELAIREXPLOSIVE = 0020;

/**
 *(0030) Glass Blads
 */
public static final short GLASSBLADS = 0030;

/**
 *(0031) 1 um
 */
public static final short ENUMERATION1UM = 0031;

/**
 *(0032) 5 um
 */
public static final short ENUMERATION5UM = 0032;

/**
 *(0033) 10 um
 */
public static final short ENUMERATION10UM = 0033;

/**
 *(1000) High Explosive(HE)
 */
public static final short HIGHEXPLOSIVE = 1000;

/**
 *(1100) HE, Plastic
 */
public static final short HEPLASTIC = 1100;

/**
 *(1200) HE, Incendiary
 */
public static final short HEINCENDIARY = 1200;

/**
 *(1300) HE, Fragmentation
 */
public static final short HEFRAGMENTATION = 1300;

/**
 *(1400) HE, Antitank
 */
public static final short HEANTITANK = 1400;

/**
 *(1500) HE, Bomblets
 */
public static final short HEBOMBLETS = 1500;

/**
 *(1600) HE, Shaped Charge
 */
public static final short HESHAPEDCHARGE = 1600;

/**
 *(1610) HE, Continuous Rod
 */
public static final short HECONTINUOUSROD = 1610;

/**
 *(1615) HE, Tungsten Ball
 */
public static final short HETUNGSTENBALL = 1615;

/**
 *(1620) HE, Blast Fragmentation
 */
public static final short HEBLASTFRAGMENTATION = 1620;

/**
 *(1625) HE, Steerable Darts with HE
 */
public static final short HESTEERABLEDARTSWITHHE = 1625;

/**
 *(1630) HE, Darts
 */
public static final short HEDARTS = 1630;

/**
 *(1635) HE, Flechettes
 */
public static final short HEFLECHETTES = 1635;

/**
 *(1640) HE, Directed Fragmentation
 */
public static final short HEDIRECTEDFRAGMENTATION = 1640;

/**
 *(1645) HE, Semi-Armor Piercing (SAP)
 */
public static final short HESEMIARMORPIERCING = 1645;

/**
 *(1650) HE, Shaped Charge Fragmentation
 */
public static final short HESHAPEDCHARGEFRAGMENTATION = 1650;

/**
 *(1655) HE, Semi-Armor Piercing, Fragmentation
 */
public static final short HESEMIARMORPIERCINGFRAGMENTATION = 1655;

/**
 *(1660) HE, Hallow Charge
 */
public static final short HEHALLOWCHARGE = 1660;

/**
 *(1665) HE, Double Hallow Charge
 */
public static final short HEDOUBLEHALLOWCHARGE = 1665;

/**
 *(1670) HE, General Purpose
 */
public static final short HEGENERALPURPOSE = 1670;

/**
 *(1675) HE, Blast Penetrator
 */
public static final short HEBLASTPENETRATOR = 1675;

/**
 *(1680) HE, Rod Penetrator
 */
public static final short HERODPENETRATOR = 1680;

/**
 *(1685) HE, Antipersonnel
 */
public static final short HEANTIPERSONNEL = 1685;

/**
 *(2000) Smoke
 */
public static final short SMOKE = 2000;

/**
 *(3000) Illumination
 */
public static final short ILLUMINATION = 3000;

/**
 *(4000) Practice
 */
public static final short PRACTICE = 4000;

/**
 *(5000) Kinetic
 */
public static final short KINETIC = 5000;

/**
 *(6000) Mines
 */
public static final short MINES = 6000;

/**
 *(7000) Nuclear
 */
public static final short NUCLEAR = 7000;

/**
 *(7010) Nuclear, IMT
 */
public static final short NUCLEARIMT = 7010;

/**
 *(8000) Chemical, General
 */
public static final short CHEMICALGENERAL = 8000;

/**
 *(8100) Chemical, Blister Agent
 */
public static final short CHEMICALBLISTERAGENT = 8100;

/**
 *(8110) HD (Mustard)
 */
public static final short HD = 8110;

/**
 *(8115) Thickened HD (Mustard)
 */
public static final short THICKENEDHD = 8115;

/**
 *(8120) Dusty HD (Mustard)
 */
public static final short DUSTYHD = 8120;

/**
 *(8200) Chemical, Blood Agent
 */
public static final short CHEMICALBLOODAGENT = 8200;

/**
 *(8210) AC (HCN)
 */
public static final short AC = 8210;

/**
 *(8215) CK (CNCI)
 */
public static final short CK = 8215;

/**
 *(8220) CG (Phosgene)
 */
public static final short CG = 8220;

/**
 *(8300) Chemical, Nerve Agent
 */
public static final short CHEMICALNERVEAGENT = 8300;

/**
 *(8310) VX
 */
public static final short VX = 8310;

/**
 *(8315) Thickened VX
 */
public static final short THICKENEDVX = 8315;

/**
 *(8320) Dusty VX
 */
public static final short DUSTYVX = 8320;

/**
 *(8325) GA (Tabun)
 */
public static final short GA = 8325;

/**
 *(8330) Thickened GA (Tabun)
 */
public static final short THICKENEDGA = 8330;

/**
 *(8335) Dusty GA (Tabun)
 */
public static final short DUSTYGA = 8335;

/**
 *(8340) GB (Sarin)
 */
public static final short GB = 8340;

/**
 *(8345) Thickened GB (Sarin)
 */
public static final short THICKENEDGB = 8345;

/**
 *(8350) Dusty GB (Sarin)
 */
public static final short DUSTYGB = 8350;

/**
 *(8355) GD (Soman)
 */
public static final short GD = 8355;

/**
 *(8360) Thickened GD (Soman)
 */
public static final short THICKENEDGD = 8360;

/**
 *(8365) Dusty GD (Soman)
 */
public static final short DUSTYGD = 8365;

/**
 *(8370) GF
 */
public static final short GF = 8370;

/**
 *(8375) Thickened GF
 */
public static final short THICKENEDGF = 8375;

/**
 *(8380) Dusty GF
 */
public static final short DUSTYGF = 8380;

/**
 *(9000) Biological
 */
public static final short BIOLOGICAL = 9000;

/**
 *(9100) Biological, Virus
 */
public static final short BIOLOGICALVIRUS = 9100;

/**
 *(9200) Biological, Bacteria
 */
public static final short BIOLOGICALBACTERIA = 9200;

/**
 *(9300) Biological, Rickettsia
 */
public static final short BIOLOGICALRICKETTSIA = 9300;

/**
 *(9400) Biological, Genetically Modified Micro-organisms
 */
public static final short BIOLOGICALGENETICALLYMODIFIEDMICROORGANISMS = 9400;

/**
 *(9500) Biological, Toxin
 */
public static final short BIOLOGICALTOXIN = 9500;

/**
 * Returns a string containing the enumeration name which corresponds to an enumeration value,
 * as in <b><code>WarheadField.toString (0000)</code></b> returns the string "<b><code>OTHER</code></b>"
 */

public static String toString(int idNumber) 
{
	switch (idNumber) {
		case 0000: return "Other"; 
		case 0010: return "Cargo (Variable Submunitions)"; 
		case 0020: return "Fuel/Air Explosive"; 
		case 0030: return "Glass Blads"; 
		case 0031: return "1 um"; 
		case 0032: return "5 um"; 
		case 0033: return "10 um"; 
		case 1000: return "High Explosive(HE)"; 
		case 1100: return "HE, Plastic"; 
		case 1200: return "HE, Incendiary"; 
		case 1300: return "HE, Fragmentation"; 
		case 1400: return "HE, Antitank"; 
		case 1500: return "HE, Bomblets"; 
		case 1600: return "HE, Shaped Charge"; 
		case 1610: return "HE, Continuous Rod"; 
		case 1615: return "HE, Tungsten Ball"; 
		case 1620: return "HE, Blast Fragmentation"; 
		case 1625: return "HE, Steerable Darts with HE"; 
		case 1630: return "HE, Darts"; 
		case 1635: return "HE, Flechettes"; 
		case 1640: return "HE, Directed Fragmentation"; 
		case 1645: return "HE, Semi-Armor Piercing (SAP)"; 
		case 1650: return "HE, Shaped Charge Fragmentation"; 
		case 1655: return "HE, Semi-Armor Piercing, Fragmentation"; 
		case 1660: return "HE, Hallow Charge"; 
		case 1665: return "HE, Double Hallow Charge"; 
		case 1670: return "HE, General Purpose"; 
		case 1675: return "HE, Blast Penetrator"; 
		case 1680: return "HE, Rod Penetrator"; 
		case 1685: return "HE, Antipersonnel"; 
		case 2000: return "Smoke"; 
		case 3000: return "Illumination"; 
		case 4000: return "Practice"; 
		case 5000: return "Kinetic"; 
		case 6000: return "Mines"; 
		case 7000: return "Nuclear"; 
		case 7010: return "Nuclear, IMT"; 
		case 8000: return "Chemical, General"; 
		case 8100: return "Chemical, Blister Agent"; 
		case 8110: return "HD (Mustard)"; 
		case 8115: return "Thickened HD (Mustard)"; 
		case 8120: return "Dusty HD (Mustard)"; 
		case 8200: return "Chemical, Blood Agent"; 
		case 8210: return "AC (HCN)"; 
		case 8215: return "CK (CNCI)"; 
		case 8220: return "CG (Phosgene)"; 
		case 8300: return "Chemical, Nerve Agent"; 
		case 8310: return "VX"; 
		case 8315: return "Thickened VX"; 
		case 8320: return "Dusty VX"; 
		case 8325: return "GA (Tabun)"; 
		case 8330: return "Thickened GA (Tabun)"; 
		case 8335: return "Dusty GA (Tabun)"; 
		case 8340: return "GB (Sarin)"; 
		case 8345: return "Thickened GB (Sarin)"; 
		case 8350: return "Dusty GB (Sarin)"; 
		case 8355: return "GD (Soman)"; 
		case 8360: return "Thickened GD (Soman)"; 
		case 8365: return "Dusty GD (Soman)"; 
		case 8370: return "GF"; 
		case 8375: return "Thickened GF"; 
		case 8380: return "Dusty GF"; 
		case 9000: return "Biological"; 
		case 9100: return "Biological, Virus"; 
		case 9200: return "Biological, Bacteria"; 
		case 9300: return "Biological, Rickettsia"; 
		case 9400: return "Biological, Genetically Modified Micro-organisms"; 
		case 9500: return "Biological, Toxin"; 
		default : return "";
	}
}//end of toString
}//End of class 
