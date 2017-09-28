/*
 File:		DetonationResultField.java
 CVS Info:      $Id$
 Compiler:	jdk 1.2.2
 */

package disEnumerations;

import mil.navy.nps.dis.*;
import mil.navy.nps.util.*;

/**
 * Detonation Result Field -- This field shall specify the result of the detonation. This field shall be represented by an 8-bit enumberation (see Section 5 in EBV-DOC).
 *@version 1.1
 *@author Ronan Fauglas
 *@author <a href="mailto:brutzman@nps.navy.mil?subject=dis-java-vrml: disEnumerations.DetonationResultField feedback ">Don Brutzman</a>
 *
 *<dt><b>References:</b>
 *<dd>		DIS Data Dictionary: <A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/85.htm">Detonation Result Field</A> (local)
 *				     <A href="http://SISO.sc.ist.ucf.edu/dis/dis-dd/pdu/85.htm">Detonation Result Field</A> (SISO)
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
 *  You access this via something like <b><code>DetonationResultField.HEHITLARGE</code></b>, i.e. combine
 *  the class name, a period, and a class variable (enumeration) name.<P>
 *
 *<dt><b>History:</b>
 *<dd>		21jan98   /Ronan Fauglas   	/New
 *<dd>		30mar99   /Don Brutzman   	/Revised Javadoc, many more enumeration classes
 *
 *<dt><b>Location:</b>
 *<dd><a href="../../../../../../mil/navy/nps/disEnumerations/DetonationResultField.java"><i>DetonationResultField.java</i></A> (local)
 *<dd><a href="http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/DetonationResultField.java">
 *          <i>http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/DetonationResultField.java</i></a>
 *
 */
public class DetonationResultField_2 extends Object
{
/**
 *(0) Other
 */
public static final short OTHER = 0;

/**
 *(1) Entity Impact
 */
public static final short ENTITYIMPACT = 1;

/**
 *(10) Armor-piercing hit
 */
public static final short ARMORPIERCINGHIT = 10;

/**
 *(11) Dirt blast, small
 */
public static final short DIRTBLASTSMALL = 11;

/**
 *(12) Dirt blast, medium
 */
public static final short DIRTBLASTMEDIUM = 12;

/**
 *(13) Dirt blast, large
 */
public static final short DIRTBLASTLARGE = 13;

/**
 *(14) Water blast, small
 */
public static final short WATERBLASTSMALL = 14;

/**
 *(15) Water blast, medium
 */
public static final short WATERBLASTMEDIUM = 15;

/**
 *(16) Water blast, large
 */
public static final short WATERBLASTLARGE = 16;

/**
 *(17) Air hit
 */
public static final short AIRHIT = 17;

/**
 *(18) Building hit, small
 */
public static final short BUILDINGHITSMALL = 18;

/**
 *(19) Building hit, medium
 */
public static final short BUILDINGHITMEDIUM = 19;

/**
 *(2) Entity Proximate Detonation
 */
public static final short ENTITYPROXIMATEDETONATION = 2;

/**
 *(20) Building hit, large
 */
public static final short BUILDINGHITLARGE = 20;

/**
 *(21) Mine-clearing line charge
 */
public static final short MINECLEARINGLINECHARGE = 21;

/**
 *(22) Environment object impact
 */
public static final short ENVIRONMENTOBJECTIMPACT = 22;

/**
 *(23) Environment object proximate detonation
 */
public static final short ENVIRONMENTOBJECTPROXIMATEDETONATION = 23;

/**
 *(24) Water Impact
 */
public static final short WATERIMPACT = 24;

/**
 *(25) Air Burst
 */
public static final short AIRBURST = 25;

/**
 *(3) Ground Impact
 */
public static final short GROUNDIMPACT = 3;

/**
 *(4) Ground Proximate Detonation
 */
public static final short GROUNDPROXIMATEDETONATION = 4;

/**
 *(5) Detonation
 */
public static final short DETONATION = 5;

/**
 *(6) None
 */
public static final short NONE = 6;

/**
 *(7) HE hit, small
 */
public static final short HEHITSMALL = 7;

/**
 *(8) HE hit, medium
 */
public static final short HEHITMEDIUM = 8;

/**
 *(9) HE hit, large
 */
public static final short HEHITLARGE = 9;

/**
 * Returns a string containing the enumeration name which corresponds to an enumeration value,
 * as in <b><code>DetonationResultField.toString (0)</code></b> returns the string "<b><code>OTHER</code></b>"
 */

public static String toString(int idNumber) 
{
	switch (idNumber) {
		case 0: return "Other"; 
		case 1: return "Entity Impact"; 
		case 10: return "Armor-piercing hit"; 
		case 11: return "Dirt blast, small"; 
		case 12: return "Dirt blast, medium"; 
		case 13: return "Dirt blast, large"; 
		case 14: return "Water blast, small"; 
		case 15: return "Water blast, medium"; 
		case 16: return "Water blast, large"; 
		case 17: return "Air hit"; 
		case 18: return "Building hit, small"; 
		case 19: return "Building hit, medium"; 
		case 2: return "Entity Proximate Detonation"; 
		case 20: return "Building hit, large"; 
		case 21: return "Mine-clearing line charge"; 
		case 22: return "Environment object impact"; 
		case 23: return "Environment object proximate detonation"; 
		case 24: return "Water Impact"; 
		case 25: return "Air Burst"; 
		case 3: return "Ground Impact"; 
		case 4: return "Ground Proximate Detonation"; 
		case 5: return "Detonation"; 
		case 6: return "None"; 
		case 7: return "HE hit, small"; 
		case 8: return "HE hit, medium"; 
		case 9: return "HE hit, large"; 
		default : return "";
	}
}//end of toString
}//End of class 
