/*
 File:		UnsignedLong.java
 CVS Info:	$Id: UnsignedLong.java,v 1 2000/07/10 22:48:51 laflam Exp $
 Compiler:	jdk 1.3
 */

package mil.navy.nps.util;                       // package we belong to

import java.io.*;
import java.lang.*;

/**
 *@version 1.0
 *@author <a href="mailto:mcgredo@cs.nps.navy.mil">Don McGregor</a> (<a href="http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/">http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/</a>)
 *
 *<dt><b>Location:</b>
 *<dd><a href="http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/mil/navy/nps/dis/UnsignedLong.java">
 *  http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/mil/navy/nps/dis/UnsignedLong.java</a>
 *
 *<dt><b>Summary:</b>
 *<dd>This class implements a 64-bit unsigned long, which Java doesn't natively handle.
 *<dt><b>Explanation:</b> 
 *<dd>This is an extension of Number to handle 64-bit unsigned integers. Java
 *  doesn't handle unsigned numbers, so we have to promote the unsigned integers to longs
 *  to be sure we have enough space to handle them. <P>
 *
 *  This implements the SerializationInterface, which means it can be
 *  written out to a stream in the standard DIS 64-bit format.<P>
 *
 *  See comments in UnsignedByte for description of how the mapping between
 *  negative values and unsigned positive values works.<P>
 *
 *  This implements the Cloneable interface, which means that when we clone()
 *  something we can do a bitwise field copy (shallow copy). This needs to
 *  change if we ever have any actual objects contained in this class.<P>
 *
 *<dt><b>History:</b>
 *<dd>		10Aug2000	initial internal representation double	/Dave Laflam and Don Brutzman		/New from UnsignedInt.java<BR>
 *<dd>		25Aug2000	internal representation long	/Don McGregor and Don Brutzman		/New from UnsignedInt.java<BR>
 *
 *
 *<dt><b>Test invocation:</b>
 *<dd>		<code>java mil.navy.nps.util.UnsignedLong</code>
 *
 *<dt><b>References:</b>
 *<dd>		DIS-Java-VRML Working Group: <a href="http://web.nps.navy.mil/~brutzman/vrtp/dis-java-vrml/"> 
 *		http://web.nps.navy.mil/~brutzman/vrtp/dis-java-vrml/</a>
 *
 *@see SerializationInterface
 *@see UnsignedByte
 *@see UnsignedShort
 *@see UnsignedInt
*/

public class UnsignedLong extends Number implements SerializationInterface, Cloneable
{
/**
 * Maximum value is (2^64 - 1) = 18446744073709551615, but currently support is limited to 63-bit maximum size of Long = java.lang.Long.MAX_VALUE
 */

//  public final static double        MAX_UNSIGNEDLONG_VALUE = 18446744073709551615.0d;    // The maximum size an unsigned long can be.

    public final static long          MAX_UNSIGNEDLONG_VALUE = java.lang.Long.MAX_VALUE;   // The maximum supported size an unsigned long can be.
/**
 * @serial
 */
    private long _data;       // Data we hold. Nobody should be touching this directly.

// Constructors


/**
 *Contructs an <code>UnsignedLong</code> object and initializes its value to 0.
 */
public UnsignedLong()
{ _data = 0;
}
 
 
/**
 *Constructs a <code>UnsignedLong</code> object from a signed byte,
 *throws an exception if the parameter is out of range. 
 *
 *@param pData &gt;=0,&lt;=MAX_UNSIGNEDLONG_VALUE
 *@exception RuntimeException if <code>pData</code> is out of range
 */
public UnsignedLong(byte pData)
{ 
    if ( pData < 0 )
        throw new 
            RuntimeException("Exception in UnsignedLong. Attempt to assign value beyond maximum range of an unsigned long.");
    _data = pData;
} 
/**
 *Constructs a <code>UnsignedLong</code> object from a signed short,
 *throws an exception if the paraneter is out of range. 
 *
 *@param pData &gt;=0
 *@exception RuntimeException if <code>pData</code> is out of range
 */
public UnsignedLong(short pData)
{
 if ( pData < 0 )
    throw new 
            RuntimeException("Exception in UnsignedLong. Attempt to assign value beyond maximum range of an unsigned long.");  
	_data = pData;
}
/**
 *Constructs an <code>UnsignedLong</code> object from a signed int,
 *throws an exception if the parameter is out of range. 
 *
 *@param pData &gt;=0
 *@exception RuntimeException if <code>pData</code> is out of range
 */
public UnsignedLong(int pData)
{ 
if (pData < 0)
        throw new 
            RuntimeException("Exception in UnsignedLong. Attempt to assign value beyond maximum range of an unsigned long.");
	_data = pData;
}

/**
 *Constructs an <code>UnsignedLong</code> object from a signed double,
 *throws an exception if the parameter is out of range. 
 *
 *@param pData &gt;=0
 *@exception RuntimeException if <code>pData</code> is out of range
 */
public UnsignedLong(double pData)
{ 
if(pData < 0)
        throw new 
            RuntimeException("Exception in UnsignedLong. Attempt to assign value beyond maximum range of an unsigned long.");
_data = (long) pData;
}
/**
 *Constructs an <code>UnsignedLong</code> object from a signed float,
 *throws an exception if the parameter is out of range. 
 *
 *@param pData >=0
 *@exception RuntimeException if <code>pData</code> is out of range
 */
public UnsignedLong(float pData)
{ 
if(pData < 0)
        throw new 
            RuntimeException("Exception in UnsignedLong. Attempt to assign value beyond maximum range of an unsigned long.");
_data = (long) pData;
}

/**
 *Constructs an <code>UnsignedLong</code> object from a signed long,
 *throws an exception if the parameter is out of range. 
 *
 *@param pData >=0
 *@exception RuntimeException if <code>pData</code> is out of range
 */
public UnsignedLong(long pData)
{ 
if(pData < 0)
        throw new 
            RuntimeException("Exception in UnsignedLong. Attempt to assign value beyond maximum range of an unsigned long.");
	_data = pData;
}

/**
 *Returns a the current value of this object as a float, after conversion.
 *
 *@return the current value of this object as a float
 */
public float floatValue()
{
	double temp;

	temp = (_data < 0 ? MAX_UNSIGNEDLONG_VALUE + _data + 1 : _data);
	return (float)temp;
}



/**
 *Returns the current value of this object as a double float, 
 *after conversion.
 *
 *@return the current value of this object as a double float
 */
public double doubleValue()
{ 
	double temp;

	temp = (_data < 0 ? MAX_UNSIGNEDLONG_VALUE + _data + 1 : _data);
	return temp;
}

/**
 *Returns the current value of this object as a long, after conversion.
 *
 *@return the current value of this object as a long
 */
public long longValue()
{
	double    temp;

	temp = (_data < 0 ? MAX_UNSIGNEDLONG_VALUE + _data + 1 : _data);
	return (long)temp;
}
 

/**
 *Don't use this !
 *
 *<dt><b>Explanation</b>
 *<dd>This is actually a bad thing, since we cannot represent the
 *full range of an unsigned long with an long. One option is to
 *throw an exception only when the long value is beyond the range,
 *eg above 2-billion odd. But it seems better to blow up in place,
 *rather than work sometimes and not other times. (We have to implement
 *the method because it's declared abstract in Number.) Moral of the
 *story: don't try to read the value of an unsigned long with an int.
 *
 *@exception RuntimeException <b>whenever one tries to use this method</b>. 
 */
public int intValue()
{
    if (((int)_data < 0) || (_data > Integer.MAX_VALUE))
    	System.err.println ("Warning: Unsignedlong above maximum allowed value for int, cast to int anyway.");

    return (int) _data;
}

// Implementation of serialization interface

/**
 *Writes out an UnsignedLong to an output stream.
 *
 *@param outputstream the targetted output stream for this object
 *@throws RuntimeException if an <code>IOException</code> occurs.
 *
 *@see SerializationInterface
 */
public void serialize(DataOutputStream pOutputStream)
{   
    try
     {
        pOutputStream.writeLong(_data);
     }
    catch (IOException ioError)
        {
            throw new 
                RuntimeException("Exception in UnsignedLong. Error writing to file.");
        }
}

/**
 *Reads an UnsignedLong input from DIS format.
 *
 *@param inputstream the input stream that builds the object.
 *@exception RuntimeException if an <code>IOException</code> occurs.
 *
 *@see SerializationInterface
 */
public void deSerialize(DataInputStream pInputStream)
{
    try
     {
        _data = pInputStream.readLong();
     }
     catch (IOException ioError)
        {
            throw new 
                RuntimeException("Exception in UnsignedLong. Error reading from file.");
        }

}

/**
 *Returns a String object representing this <code>UnsignedLong</code> value. 
 *
 *@return a string representation of the value of this object in base 10. 
 */
public String toString()
{
    Long    temp = new Long(this.longValue());

    return temp.toString();
}

/**
 *Makes a deep copy of the current <code>UnsignedLong</code> object.
 * 
 *@return the cloned object.
 *@exception RuntimeException if cloning fails
 */
public Object clone()
{
    Object  newObject;

    try{
        newObject = super.clone();  // shallow copy
       }
    catch(CloneNotSupportedException cloneError)
         {
            throw new 
                RuntimeException("Exception in UnsignedByte. Error cloning object.");
        }
    return newObject;
}

/**
 *Of debugging interest only. You shouldn't have to use this method.
 */
public static void debugTest()
{
    // Some testing to make sure things work right.

    UnsignedLong     zero, one, two, three, four, five, six;

    // test extreme values for bytes; zero, near the roll-over point, and the max value

    System.out.println("Testing functionality of unsigned long class");

    zero = new UnsignedLong((long)0); 
    one  = new UnsignedLong((long)2147483647 );
    two  = new UnsignedLong((long)2147483648L);
    three = new UnsignedLong((long)2147483649L );
    four  = new UnsignedLong((long)MAX_UNSIGNEDLONG_VALUE);

    System.out.println("Values should be 0, 2147483647, 2147483648L, 2147483649L, 4294907295L:");
    System.out.println("             >>> " + zero.toString() + " " + one.toString()
                     + " " + two.toString() + " " + three.toString() + " " + four.toString());

     System.out.println();
     
   System.out.println("Causes runtime exception: five = new UnsignedLong(-1);");
    try {
    	five = new UnsignedLong(-1);
    }
    catch (Exception e) { System.out.println(e); }

     System.out.println();
     
    System.out.println("Causes runtime exception: six = new UnsignedLong((long) MAX_UNSIGNEDLONG_VALUE + 1);");
    try {
    	six = new UnsignedLong((long) MAX_UNSIGNEDLONG_VALUE + 1);
    }
    catch (Exception e) { System.out.println(e); }
}

public static void main (String[] args)
{
	debugTest();
}

} // end of class UnsignedLong
