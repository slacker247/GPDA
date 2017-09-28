/*
 File:		UnsignedByte.java
 CVS Info:	$Id: UnsignedByte.java,v 1.1.1.1 1998/01/27 22:48:51 mcgredo Exp $
 Compiler:	jdk 1.1.5 
 */

package mil.navy.nps.util;               // package this belongs to

import java.io.*;
import java.lang.*;

/**
 *@version 1.0
 *@author <a href="mailto:mcgredo@cs.nps.navy.mil"> Don McGregor</a> (<a href="http://www.npsnet.org/~mcgredo">http://www.npsnet.org/~mcgredo</a>)
 *
 *<dt><b>Location:</b>
 *<dd><a href="http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/dis/UnsignedByte.java">
 *  http://www.web3D.org/WorkingGroups/vrtp/mil/navy/nps/dis/UnsignedByte.java</a>
 *
 *<dt><b>Summary:</b>
 *<dd>This class implements a 8-bit unsigned integer, which Java doesn't natively handle.
 *
 *<dt><b>Explanation:</b> 
 *<dd>An 8-bit unsigned int, which Java doesn't natively handle. So
 *  we have to wrap this object around the data. When serialized,
 *  this will be written out as an 8-bit quantity.<p>
 *  
 *  Note that you can't retrieve the value of this as a byte, simply
 *  because a negative value for an unsigned byte (as the signed value
 *  242 would be) is invalid, and can't be represented. So we have to
 *  promote the bloody thing to an int.<P>
 *
 *  Hmmmmm....should the internal representation be a short or a byte?
 *  I vote for byte, since it requires less storage in memory, and we
 *  might have a lot of these floating about. But we're constantly
 *  promoting it to an int or short anyway... <P>
 *  
 *  This implementation works by exploiting the characteristics of how
 *  numbers are stored internally. Just about all systems, except for a
 *  few obsolete machines, use "two's-complement" format for storing
 *  integer values. A signed byte will have this sequence as more and
 *  more bits are flipped on:<P>
 *
 *  0,1,2,3....125,126,127,-128,-127,-127,...-1<P>
 *
 *  The values roll over from (2^7)-1 to -(2^7) at the midpoint. Adding
 *  one to 127 will result in -128, adding two results in -127, etc. We
 *  can fake up unsigned bytes by exploiting this behavior. The maximum
 *  an unsigned byte can represent is 255 (2^8 - 1) so we can translate
 *  between a negative value and its unsigned positive representation with
 *  this formula:<br>
 *<code><pre>
 *255 + _data + 1
 *</pre></code>
 *  which means that -128 is mapped to 128, -126 to 129, and so on.<P>
 *
 *  This implements the Cloneable interface, which means that when we clone()
 *  something we can do a bitwise field copy (shallow copy). This needs to
 *  change if we ever have any actual objects contained in this class.<P>
 *
 *<dt><b>History:</b>
 *<dd>		04Oct96	/Don McGregor		/New
 *<dd>		06Oct96	/Don McGregor		/changed name to UnsignedByte, added testing methods,
 *						placed in package mil.navy.nps.dis
 *<dd>		10Mar97	/Don McGregor		/changes for javadoc
 *<dd>		8Dec97	/Ronan Fauglas   	/changes for documentation templates + complements in documentation
 *
 *<dt><b>References:</b>
 *<dd>		DIS-Java-VRML Working Group: <a href="http://www.web3D.org/WorkingGroups/vrtp/dis-java-vrml/"> 
 *		http://www.web3D.org/WorkingGroups/vrtp/dis-java-vrml/</a>
 *
 *@see UnsignedShort
 *@see UnsignedInt
 *@see SerializationInterface
 */
public class UnsignedByte extends Number implements SerializationInterface, Cloneable
{
    
    public static final short MAX_UNSIGNED_BYTE_VALUE = 255;   // The maximum size an unsigned byte can be.

/**
 * @serial
 */
    private byte _data;                                 // Data we hold. Nobody should be touching this directly.

    // Constructors

    /**
     *Contructs a new <code>UnsignedByte</code> object and intializes its value to 0.
     */
    public UnsignedByte()
    {
        _data = 0;
    }

    /** 
     *Constructs an <code>UnsignedByte</code> object from a signed byte,
     *throws an exception if the paraneter is out of range. 
     *
     *@param pData >=0
     *@exception RuntimeException if <code>pData</code> is out of range
     */
    public UnsignedByte(byte pData)
    {
        if(pData < 0)
            throw new 
                RuntimeException("Exception in UnsignedByte. Attempt to assign value beyond maximum range of an unsigned byte.");
        _data = pData;
    }

    /**
     *Constructs an <code>UnsignedByte</code> object from a short,
     *throws an exception if the parameter is out of range.
     *
     *@param pData >=0, <=MAX_UNSIGNED_BYTE_VALUE
     *@exception RuntimeException if the parameter is out of range
     */
    public UnsignedByte(short pData)
    {
        if((pData < 0) || (pData > MAX_UNSIGNED_BYTE_VALUE))
            throw new 
                RuntimeException("Exception in UnsignedByte. Short value exceeds max value for an unsigned byte.");
        
        _data = (byte)pData;
    }

    /**
     *Constructs an <code>UnsignedByte</code> object from an int,
     *throws an exception if the parameter is out of range.
     *
     *@param pData >=0, <=MAX_UNSIGNED_BYTE_VALUE
     *@exception RuntimeException if <code>pData</code> is out of range
     */
    public UnsignedByte(int pData)
    { 
        if((pData < 0) || (pData > MAX_UNSIGNED_BYTE_VALUE))
            throw new 
                RuntimeException("Exception in UnsignedByte. Int value out of range for an unsigned byte.");
        
        _data = (byte)pData;
    }

    // Overrides for the abstract class Number

   /**
    *Returns the current value of this object as a double float, 
    *after conversion.
    *
    *@return the current value of this object as a double float
    */
   public double doubleValue()
    {
        // if _data is negative, we know we've overflowed the high bit.
        // If that's the case, return the "real" unsigned value.

        double  temp;

        temp = (_data < 0 ? MAX_UNSIGNED_BYTE_VALUE + _data + 1 : _data);
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

        temp = (_data < 0 ? MAX_UNSIGNED_BYTE_VALUE + _data + 1 : _data);
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

        temp = (_data < 0 ? MAX_UNSIGNED_BYTE_VALUE + _data + 1 : _data);
        return temp;
    }

   /**
    *Returns the current value of this object as a long, after conversion.
    *
    *@return the current value of this object as a long
    */
    public long longValue()
    {
        long temp;

        temp = (_data < 0 ? MAX_UNSIGNED_BYTE_VALUE + _data + 1 : _data);
        return temp;
    }

    // Implementation of the dis_SerializationInterface

    /**
     *Writes out an UnsignedByte to an output stream.
     *
     *@param outputstream the targetted output stream for this object
     *@throws a runtime Exception if an <code>IOException</code> occurs.
     *
     *@see SerializationInterface
     */
    public void serialize(DataOutputStream pOutputStream)
    {
        try
         {
            pOutputStream.writeByte( _data);
         }
        catch (IOException ioError)
        {
             throw new 
                RuntimeException("Exception in UnsignedByte. Error serializing to stream.");
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
            _data = pInputStream.readByte();
         }
        catch(IOException ioError)
         {
            throw new 
                RuntimeException("Exception in UnsignedByte. Error deSerializing from stream.");
        }
        
    }   // end of deSerialize

/**
 *Returns a String object representing this <code>UnsignedByte</code> value. 
 *
 *@return a string representation of the value of this object in base 10. 
 */
public String toString()
{
    Integer     temp = new Integer(this.intValue());

    return temp.toString();
}

/**
 *Of debugging interest only. You shouldn't have to use this method.
 */
public void debugTest()
{
    // Some testing to make sure things work right.

    UnsignedByte        zero, one, two, three, four;

    // test extreme values for bytes; zero, near the roll-over point, and the max value

    System.out.println("Testing functionality of unsigned byte class");

    zero = new UnsignedByte((byte)0); 
    one  = new UnsignedByte((byte)127);
    two  = new UnsignedByte(128);
    three = new UnsignedByte(129);
    four  = new UnsignedByte(255);

    System.out.println("Values should be 0, 127, 128, 129, 255:" + zero.toString() + " " + one.toString()
                     + " " + two.toString() + " " + three.toString() + " " + four.toString());

    zero = new UnsignedByte((int)0);
    one = new UnsignedByte((int)127);
    two = new UnsignedByte((int)128);
    three = new UnsignedByte((int)129);
    four = new UnsignedByte((int)255);

    System.out.println("Values should be 0, 127, 128, 129, 255:" + zero.toString() + " " + one.toString()
                     + " " + two.toString() + " " + three.toString() + " " + four.toString());

    zero = new UnsignedByte((short)0);
    one = new UnsignedByte((short)127);
    two = new UnsignedByte((short)128);
    three = new UnsignedByte((short)129);
    four = new UnsignedByte((short)255);

    System.out.println("Values should be 0, 127, 128, 129, 255:" + zero.toString() + " " + one.toString()
                     + " " + two.toString() + " " + three.toString() + " " + four.toString());

    // Causes runtime exception

    //zero = new UnsignedByte(-1);
    //one = new UnsignedByte(288);

};

/**
 *Makes a deep copy of the current <code>UnsignedByte</code> object.
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

}   // end of class UnsignedByte
