/*
 File:		ParameterTypeAttachedPartsField.java
 CVS Info:      $Id$
 Compiler:	jdk 1.2.2
 */

package disEnumerations;

import mil.navy.nps.dis.*;
import mil.navy.nps.util.*;

/**
 * Parameter Type Attached Parts Field -- This record represents one of the varients of the Parameter Type Varient, its values are used only when the Parameter Type is Attached Part (1).
 *@version 1.1
 *@author Ronan Fauglas
 *@author <a href="mailto:brutzman@nps.navy.mil?subject=dis-java-vrml: disEnumerations.ParameterTypeAttachedPartsField feedback ">Don Brutzman</a>
 *
 *<dt><b>References:</b>
 *<dd>		DIS Data Dictionary: <A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/71.htm">Parameter Type Attached Parts Field</A> (local)
 *				     <A href="http://SISO.sc.ist.ucf.edu/dis/dis-dd/pdu/71.htm">Parameter Type Attached Parts Field</A> (SISO)
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
 *  You access this via something like <b><code>ParameterTypeAttachedPartsField.M2MACHINEGUN</code></b>, i.e. combine
 *  the class name, a period, and a class variable (enumeration) name.<P>
 *
 *<dt><b>History:</b>
 *<dd>		21jan98   /Ronan Fauglas   	/New
 *<dd>		30mar99   /Don Brutzman   	/Revised Javadoc, many more enumeration classes
 *
 *<dt><b>Location:</b>
 *<dd><a href="../../../../../../mil/navy/nps/disEnumerations/ParameterTypeAttachedPartsField.java"><i>ParameterTypeAttachedPartsField.java</i></A> (local)
 *<dd><a href="http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/ParameterTypeAttachedPartsField.java">
 *          <i>http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/disEnumerations/ParameterTypeAttachedPartsField.java</i></a>
 *
 */
public class ParameterTypeAttachedPartsField extends Object
{
/**
 *(0) Nothing, Empty
 */
public static final int NOTHINGEMPTY = 0;

/**
 *(896)  M16A42 rifle
 */
public static final int M16A42RIFLE = 896;

/**
 *(897)  M249 SAW
 */
public static final int M249SAW = 897;

/**
 *(898)  M60 Machine gun
 */
public static final int M60MACHINEGUN = 898;

/**
 *(899) M203 Grenade Launcher
 */
public static final int M203GRENADELAUNCHER = 899;

/**
 *(900)  M136  AT4
 */
public static final int M136AT4 = 900;

/**
 *(901)  M47 Dragon
 */
public static final int M47DRAGON = 901;

/**
 *(902) AAWS-M Javelin
 */
public static final int AAWSMJAVELIN = 902;

/**
 *(903) M18A1 Claymore Mine
 */
public static final int M18A1CLAYMOREMINE = 903;

/**
 *(904) MK19 Grenade Launcher
 */
public static final int MK19GRENADELAUNCHER = 904;

/**
 *(905) M2 Machine Gun
 */
public static final int M2MACHINEGUN = 905;

/**
 * Returns a string containing the enumeration name which corresponds to an enumeration value,
 * as in <b><code>ParameterTypeAttachedPartsField.toString (0)</code></b> returns the string "<b><code>NOTHINGEMPTY</code></b>"
 */

public static String toString(int idNumber) 
{
	switch (idNumber) {
		case 0: return "Nothing, Empty"; 
		case 896: return " M16A42 rifle"; 
		case 897: return " M249 SAW"; 
		case 898: return " M60 Machine gun"; 
		case 899: return "M203 Grenade Launcher"; 
		case 900: return " M136  AT4"; 
		case 901: return " M47 Dragon"; 
		case 902: return "AAWS-M Javelin"; 
		case 903: return "M18A1 Claymore Mine"; 
		case 904: return "MK19 Grenade Launcher"; 
		case 905: return "M2 Machine Gun"; 
		default : return "";
	}
}//end of toString
}//End of class 
