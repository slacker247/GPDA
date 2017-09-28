/*
 File:		UnsignedInt.java
 CVS Info:	$Id: UnsignedInt.java,v 1.1.1.1 1998/01/27 22:48:51 mcgredo Exp $
 Compiler:	jdk 1.3
 */

package mil.navy.nps.util;                       // package we belong to

import java.io.*;
import java.lang.*;

/**
 *@version 1.0
 *@author <a href="mailto:mcgredo@cs.nps.navy.mil">Don McGregor</a> (<a href="http://www.npsnet.org/~mcgredo">http://www.npsnet.org/~mcgredo</a>)
 *
 *<dt><b>Location:</b>
 *<dd><a href="http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/dis/UnsignedInt.java">
 *  http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/dis/UnsignedInt.java</a>
 *
 *<dt><b>Summary:</b>
 *<dd>This class implements a 32-bit unsigned integer, which Java doesn't natively handle.
 *<dt><b>Explanation:</b> 
 *<dd>This is an extension of Number to handle 32-bit unsigned integers. Java
 *  doesn't handle unsigned numbers, so we have to promote the unsigned integers to longs
 *  to be sure we have enough space to handle them. <P>
 *
 *  This implements the SerializationInterface, which means it can be
 *  written out to a stream in the standard DIS 32-bit format.<P>
 *
 *  See comments in UnsignedByte for description of how the mapping between
 *  negative values and unsigned positive values works.<P>
 *
 *  This implements the Cloneable interface, which means that when we clone()
 *  something we can do a bitwise field copy (shallow copy). This needs to
 *  change if we ever have any actual objects contained in this class.<P>
 *
 *<dt><b>History:</b>
 *<dd>		07Oct96	/Don McGregor		/New<BR>
 *<dd>		09Oct96	/Don McGregor		/Changed name to UnsignedInt, added testing, added to 
                    				mil.navy.nps.dis package
 *<dd>		10Mar97	/Don McGregor		/changes for javadoc<P>
 *<dd>		8Dec97	/Ronan Fauglas   	/changes for documentation templates + complements in documentation
 *<dd>		16Dec98	/Don Brutzman   	/tweak documentation
 *
 *<dt><b>References:</b>
 *<dd>		DIS-Java-VRML Working Group: <a href="http://www.web3D.org/WorkingGroups/vrtp/dis-java-vrml/"> 
 *		http://www.web3D.org/WorkingGroups/vrtp/dis-java-vrml/</a>
 *
 *@see SerializationInterface
 *@see UnsignedByte
 *@see UnsignedShort
*/

public class UnsignedInt extends Number implements SerializationInterface, Cloneable
{
    public final static int         MAX_INT_VALUE  = Integer.MAX_VALUE;    // The maximum int  size an unsigned int can be.
    public final static long        MAX_LONG_VALUE = Long.MAX_VALUE;    // The maximum long size an unsigned int can be.

/**
 * @serial
 */
    private int _data;                                              // Data we hold. Nobody should be touching this directly.

// Constructors


/**
 *Contructs an <code>UnsignedInt</code> object and intializes its value to 0.
 */
public UnsignedInt()
{ _data = 0;
}
    
/**
 *Constructs an <code>UnsignedInt</code> object from a signed int,
 *throws an exception if the parameter is out of range. 
 *
 *@param pData >=0
 *@exception RuntimeException if <code>pData</code> is out of range
 */
public UnsignedInt(int pData)
{ 
if(pData < 0)
        throw new 
            RuntimeException("Exception in UnsignedInt. Attempt to assign value beyond maximum range of an unsigned int.");
_data = pData;
}

/**
 *Constructs a <code>UnsignedInt</code> object from a signed short,
 *throws an exception if the paraneter is out of range. 
 *
 *@param pData >=0
 *@exception RuntimeException if <code>pData</code> is out of range
 */
public UnsignedInt(short pData)
{
 if( pData < 0)
    throw new 
            RuntimeException("Exception in UnsignedInt. Attempt to assign value beyond maximum range of an unsigned int.");  
_data = pData;
}

/**
 *Constructs a <code>UnsignedInt</code> object from a signed long,
 *throws an exception if the parameter is out of range. 
 *
 *@param pData >=0,<=MAX_INT_VALUE
 *@exception RuntimeException if <code>pData</code> is out of range
 */
public UnsignedInt(long pData)
{ 
    if((pData > MAX_INT_VALUE) || ( pData < 0))
        throw new 
            RuntimeException("Exception in UnsignedInt. Attempt to assign value beyond maximum range of an unsigned int.");
    _data = (int)pData;
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

    temp = (_data < 0 ? MAX_INT_VALUE + _data + 1 : _data);
    return temp;
}

/**
 *Returns a the current value of this object as a float, after conversion.
 *
 *@return the current value of this object as a float
 */
public float floatValue()
{
  float temp;

  temp = (_data < 0 ? MAX_INT_VALUE + _data + 1 : _data);
  return temp;
}

/**
 *Returns the current value of this object as a long, after conversion.
 *
 *@return the current value of this object as a long
 */
public long longValue()
{
    long    temp;

    temp = (_data < 0 ? MAX_INT_VALUE + _data + 1 : _data);
    return temp;
}

/**
 *Don't use this !
 *
 *<dt><b>Explanation</b>
 *<dd>This is actually a bad thing, since we cannot represent the
 *full range of an unsigned int with an int. One option is to
 *throw an exception only when the int value is beyond the range,
 *eg above 2-billion odd. But it seems better to blow up in place,
 *rather than work sometimes and not other times. (We have to implement
 *the method because it's declared abstract in Number.) Moral of the
 *story: don't try to read the value of an unsigned int with an int.
 *
 *@exception RuntimeException <b>whenever one tries to use this method</b>. 
 */
public int intValue()
{
    if (_data < 0) System.err.println ("Warning: UnsignedInt above maximum allowed value for int, cast to negative int anyway.");

    return (int) _data;
}

// Implementation of serialization interface

/**
 *Writes out a UnsignedInt to an output stream.
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
        pOutputStream.writeInt(_data);
     }
    catch (IOException ioError)
        {
            throw new 
                RuntimeException("Exception in UnsignedInt. Error writing to file.");
        }
}

/**
 *Reads a UnsignedInt in from DIS format.
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
        _data = pInputStream.readInt();
     }
     catch (IOException ioError)
        {
            throw new 
                RuntimeException("Exception in UnsignedInt. Error reading from file.");
        }

}

/**
 *Returns a String object representing this <code>UnsignedInt</code> value. 
 *
 *@return a string representation of the value of this object in base 10. 
 */
public String toString()
{
    Long    temp = new Long(this.longValue());

    return temp.toString();
}

/**
 *Makes a deep copy of the current <code>UnsignedInt</code> object.
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
public void debugTest()
{
    // Some testing to make sure things work right.

    UnsignedInt     zero, one, two, three, four;

    // test extreme values for bytes; zero, near the roll-over point, and the max value

    System.out.println("Testing functionality of unsigned int class");

    zero = new UnsignedInt((long)0); 
    one  = new UnsignedInt((long)2147483647 );
    two  = new UnsignedInt((long)2147483648L);
    three = new UnsignedInt((long)2147483649L );
    four  = new UnsignedInt((long)MAX_INT_VALUE);

    System.out.println("Values should be 0, 2147483647, 2147483648L, 2147483649L, 4294907295L:" + zero.toString() + " " + one.toString()
                     + " " + two.toString() + " " + three.toString() + " " + four.toString());

    // Causes runtime exception

    //zero = new UnsignedInt(-1);
    //one = new UnsignedInt((long) MAX_INT_VALUE + 1);

};

} // end of class UnsignedInt
