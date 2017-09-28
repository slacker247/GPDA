/*
 File:		UnsignedShort.java
 CVS Info:	$Id: UnsignedShort.java,v 1.2 1998/02/05 23:08:02 mcgredo Exp $
 Compiler:	jdk 1.3
 */

package mil.navy.nps.util;                       // package we belong to

import java.io.*;
import java.lang.*;

/**
 *@version 1.0
 *@author <a href="mailto:mcgredo@cs.nps.navy.mil"> Don McGregor</a> (<a href="http://www.npsnet.org/~mcgredo">http://www.npsnet.org/~mcgredo</a>)
 *
 *<dt><b>Location:</b>
 *<dd><a href="http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/dis/UnsignedShort.java">
 *  http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/dis/UnsignedShort.java</a>
 *
 *<dt><b>Summary:</b>
 *<dd>This class implements a 16-bit unsigned integer, which Java doesn't natively handle.
 *
 *<dt><b>Explanation:</b> 
 *<dd>This is an extension of Number to handle 16-bit unsigned shorts. Java
 *  doesn't do unsigned, so we have to promote the unsigned shorts to ints
 *  to be sure we have enough space to handle them. <P>
 *
 *  This implements the SerializationInterface, which means it can be
 *  written out to a stream in the standard DIS 32-bit format.<P>
 *
 *  See explanation in UnsignedByte for how the mapping of negative-to-positive
 *  numbers works.<P>
 *
 *  This implements the Cloneable interface, which means that when we clone()
 *  something we can do a bitwise field copy (shallow copy). This needs to
 *  change if we ever have any actual objects contained in this class.<P>
 *
 *<dt><b>History:</b>
 *<dd>		09Oct96	/Don McGregor    	/New
 *<dd>		10Mar97	/Don McGregor    	/changes for javadoc
 *<dd>		8Dec97	/Ronan Fauglas   	/changes for documentation templates + complements in documentation
 *
 *<dt><b>References:</b>
 *<dd>		DIS-Java-VRML Working Group: <a href="http://www.web3D.org/WorkingGroups/vrtp/dis-java-vrml/"> 
 *		http://www.web3D.org/WorkingGroups/vrtp/dis-java-vrml/</a>
 *
 *@see SerializationInterface
 *@see UnsignedByte
 *@see UnsignedInt
 */

public class UnsignedShort extends Number implements SerializationInterface, Cloneable
{
    
    public final static int MAX_SHORT_VALUE = 65535;    // Maximum size an unsigned short can be

/**
 * @serial
 */
    private short _data;                                // Data we hold. Nobody should be touching this directly.

// Constructors

/**
 *Contructs an <code>UnsignedShort</code> object and intializes its value to 0.
 */
public UnsignedShort()
{ _data = 0;
}

/** 
 *Constructs a an <code>UnsignedShort</code> object from a signed int,
 *throws an exception if the parameter is out of range.
 *
 *@param pData >=0, <=MAX_SHORT_VALUE
 *@exception RuntimeException if <code>pData</code> is out of range
 */
public UnsignedShort(int pData)
{ 
if((pData < 0) || (pData > MAX_SHORT_VALUE))
        throw new 
            RuntimeException("Exception in UnsignedShort. Attempt to assign value beyond maximum range of an unsigned short, value=" + pData);
_data = (short)pData;
}

/**
 *Constructs an <code>UnsignedShort</code> object from a signed short,
 *throws an exception if the parameter is out of range. 
 *
 *@param pData >=0
 *@exception RuntimeException if <code>pData</code> is out of range
 */
public UnsignedShort(short pData)
{
 if( pData < 0)
    throw new 
            RuntimeException("Exception in UnsignedShort. Attempt to assign value beyond maximum range of an unsigned short.");  
_data = pData;
}

/**
 *Constructs an <code>UnsignedShort</code> object from a signed long,
 *throws an exception if the parameter is out of range. 
 *
 *@param pData >=0, <=MAX_SHORT_VALUE
 *@exception RuntimeException if <code>pData</code> is out of range
 */
public UnsignedShort(long pData)
{ 
    if((pData > MAX_SHORT_VALUE) || ( pData < 0))
        throw new 
            RuntimeException("Exception in UnsignedShort. Attempt to assign value beyond maximum range of an unsigned short.");
    _data = (short)pData;
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

    temp = (_data < 0 ? MAX_SHORT_VALUE + _data + 1: _data);
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

  temp = (_data < 0 ? MAX_SHORT_VALUE + _data + 1: _data);
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

    temp = (_data < 0 ? MAX_SHORT_VALUE + _data + 1 : _data);
    return temp;
}

/**
 *Returns the current value of this object as an int, after conversion.
 *
 *@return the current value of this object as an int
 */
public int intValue()
{
    int temp;

    temp = (_data < 0 ? MAX_SHORT_VALUE + _data + 1: _data);
    return temp;
}

// Implementation of serialization interface

/**
 *Writes out a UnsignedShort to an output stream.
 *
 *@param outputstream the targetted output stream for this object
 *@throws RuntimeException if an <code>IOException</code> occurs.
 *
 *@see SerializationInterface
 */public void serialize(DataOutputStream pOutputStream)
{   
    try
     {
        pOutputStream.writeShort(_data);
     }
    catch (IOException ioError)
        {
            throw new 
                RuntimeException("Exception in UnsignedShort. Error writing to file.");
        }
}

/**
 *Reads an UnsignedByte in from DIS format.
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
        _data = pInputStream.readShort();
     }
     catch (IOException ioError)
        {
            throw new 
                RuntimeException("Exception in UnsignedShort. Error reading from file.");
        }
}

/**
 *Returns a String object representing this <code>UnsignedShort</code> value. 
 *
 *@return a string representation of the value of this object in base 10. 
 */
public String toString()
{
  Integer   temp = new Integer(this.intValue());

  return temp.toString();
}

/**
 *Makes a deep copy of the current <code>UnsignedShort</code> object.
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

public boolean equals(Object obj)
{
    // returns true if the values are equal.

    if(this.getClass() != obj.getClass())
        return false;

    int myValue = this.intValue(), objValue = ((UnsignedShort)obj).intValue();

    if(myValue == objValue)
        return true;

    return false;
}

/**
 *Of debugging interest only. You shouldn't have to use this method.
 */
public void debugTest()
{
    // Some testing to make sure things work right.

    UnsignedShort       zero, one, two, three, four;

    // test extreme values for bytes; zero, near the roll-over point, and the max value

    System.out.println("Testing functionality of unsigned short class");

    zero = new UnsignedShort((byte)0); 
    one  = new UnsignedShort(32766);
    two  = new UnsignedShort(32767);
    three = new UnsignedShort(32768);
    four  = new UnsignedShort(65535);

    System.out.println("Values should be 0, 32766, 32767, 32768, 65535:" + zero.toString() + " " + one.toString()
                     + " " + two.toString() + " " + three.toString() + " " + four.toString());

    zero = new UnsignedShort((int)0);
    one = new UnsignedShort((int)32766);
    two = new UnsignedShort((int)32767);
    three = new UnsignedShort((int)32768);
    four = new UnsignedShort((int)65535);

    System.out.println("Values should be 0, 32766, 32767, 32768, 65535:" + zero.toString() + " " + one.toString()
                     + " " + two.toString() + " " + three.toString() + " " + four.toString());

    zero = new UnsignedShort((short)0);
    one = new UnsignedShort((short)32766);
    two = new UnsignedShort(32767);
    three = new UnsignedShort(32768);
    four = new UnsignedShort(65535);

    System.out.println("Values should be 0, 32766, 32767, 32768, 65535:" + zero.toString() + " " + one.toString()
                     + " " + two.toString() + " " + three.toString() + " " + four.toString());

    // Causes runtime exception

    //zero = new UnsignedShort(-1);
    //one = new UnsignedShort(288);

};

} // end of class UnsignedShort
