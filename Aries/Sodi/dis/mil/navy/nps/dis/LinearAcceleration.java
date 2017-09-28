/*
 File:		LinearAcceleration.java
 CVS Info:	$Id: LinearAcceleration.java,v 1.2 1998/01/27 18:44:17 mcgredo Exp $
 Compiler:	jdk1.3
 */

package mil.navy.nps.dis;

import mil.navy.nps.util.*;         // General-purpose utilities
import mil.navy.nps.disEnumerations.*;         // Enumerations for DIS

import java.lang.*;
import java.util.*;
import java.io.*;

/**
 * Linear acceleration of an entity.
 *
 *@version 1.0
 *@author <a href="mailto:mcgredo@cs.nps.navy.mil"> Don McGregor</a> (<a href="http://www.npsnet.org/~mcgredo">http://www.npsnet.org/~mcgredo</a>)
 *
 *<dt><b>Location:</b>
 *<dd>Web:  <a href="http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/LinearAcceleration.java">
 *  http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/LinearAcceleration.java</a>
 *
 *<dd>or locally:  <a href="../../../../../../mil/navy/nps/dis/LinearAcceleration.java">
 *  ~/mil/navy/nps/dis/LinearAcceleration.java</a>
 *
 *<dt><b>Hierarchy Diagram:</b>
 *<dd><a href="../../../../../../dis-java-vrml/images/PduClassHierarchy.jpg"><IMG SRC="../../../../../../dis-java-vrml/images/PduClassHierarchyButton.jpg" ALIGN=ABSCENTER WIDTH=150 HEIGHT=21></a>
 *
 *<dt><b>Summary:</b>
 *<dd>Linerar Acceleration shall be represented as a vector with components in either World Coordinate System or Entities Coordinate System
 *  depending on the value in the Dead Recoking Algorithm Field.Each vector component shall represent 
 *  acceleration in meters per second squared.
 *
 *<dt><b>Explanation</b>
 *<dd>Acceleration Describes the Acceleration of an object, in x, y, z 32 bit
 *  floating point terms expressed  in world's coordinate system or entit.
 *  In fact DIS uses the vector record where an entity coordinate vector, 
 *  a linear acceleration or a linear velocity vector stands.
 *  We have a particular class for each of those, we provides us with stronger typing, hence improves security.
 *
 *<dt><b>History:</b>
 *<dd>		07Mar97 /Don McGregor    	/New
 *<dd>		16Apr97 /Don McGregor    	/PrintStream passed to printValues<BR>
 *<dd>		12Aug97 /Don Brutzman		/elaborated printValues
 *<dd>		8Dec97	/Ronan Fauglas		/changes for documentation templates + complements in documentation
 *<dd>		11Dec97	/Ronan Fauglas		/changes access methods names from "variable()" to "getVariable()"
 *
 *<dt><b>References:</b>
 *<dd>		DIS Data Dictionary: <a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/33.htm">Linear Acceleration Vector Record</a>
 *<dd>		DIS specification : IEEE 1278.1, Section 5.3.33.2
 *
 *@see PduElement 
 *@see SerializationInterface
 *@see EntityCoordinate
 *@see LinearVelocity
 */

public class LinearAcceleration extends PduElement
{
    /**
     *First coordinate of acceleration along the X axis, in a cartesian coordinate system.
     *
     *<dl>
     *<dt><b>Value:</b>
     *<dd>In meters by second.
     *</dl>
     */
    protected float  x;     // x-position

    /**
     *Second coordinate of acceleration along the Y axis, in a cartesian coordinate system.
     *
     *<dl>
     *<dt><b>Value:</b>
     *<dd>In meters by second.
     *</dl>
     */
    protected float  y;     // y-position

    /**
     *Third coordinate of acceleration along the Z axis, in a cartesian coordinate system.
     *
     *<dl>
     *<dt><b>Value:</b>
     *<dd>In meters by second.
     *</dl>
     */
    protected float  z;     // z-position

   /**
     *Constant value--size of a Linear Acceleration record when written out; here :<code>sizeOf = 12 bytes</code>.
     */
    public final int sizeOf = 12;   // object is 12 bytes long 

/**
 *Constructs an new Acceleration Object, acceleration's value is 0.
 */
public LinearAcceleration()
{
    // default constructor

    x = 0; 
    y = 0;
    z = 0;

    return;
}

/**
 *Constructs a new Acceleration object whose coordinate values are passed in parameters.
 *
 *@param pX the first coordinate in the cartesian coordinate system
 *@param pY the second coordinate in the cartesian coordinate system
 *@param pZ the third coordinate in the cartesian coordinate system
 */
public LinearAcceleration(float pX, float pY, float pZ)
{
    x = pX;
    y = pY;
    z = pZ;

    return;
}

public Object clone()
{

 LinearAcceleration   newAcceleration = new LinearAcceleration(x, y, z);

 return newAcceleration;
}

/**
 *@exception RuntimeException when IO error occurs.
 */
public void serialize(DataOutputStream outputStream)
{
    // write out the data to an output stream. Order is important here, since
    // it needs to conform to the DIS standard.

    //super.serialize(outputStream);        // No need to call super, since no ivars are in our direct superclass
    
    try
     {
        outputStream.writeFloat(x);
        outputStream.writeFloat(y);
        outputStream.writeFloat(z);
     }
    catch (IOException ioError)
        {
            throw new 
                RuntimeException("Exception in Acceleration. Error writing to wire.");
        }
}

/**
 *@exception RuntimeException when IO error occurs.
 */
public void deSerialize(DataInputStream pInputStream)
{
    try
     {
        x = pInputStream.readFloat();
        y = pInputStream.readFloat();
        z = pInputStream.readFloat();
     }
     catch (IOException ioError)
        {
            throw new 
                RuntimeException("Exception in Acceleration. Error reading from wire.");
        }

}

public int length()
{
    return sizeOf;          // EntityTypes are this long, always.
}

public void printValues(int indentLevel, PrintStream printStream)
{
    // print the values of the object out, with correct level of
    // indentation on the page.
 
  StringBuffer indent = ProtocolDataUnit.getPaddingOfLength(indentLevel);
  int          idx, superclassIndent = indentLevel;

    printStream.println(indent + "acceleration x: "   + x);
    printStream.println(indent + "acceleration y: "   + y);
    printStream.println(indent + "acceleration z: "   + z);

    return;
}

// Accessor methods

public void setValues(float pX, float pY, float pZ)
{ x = pX;
  y = pY;
  z = pZ;
}

public float getX()
{   return x;
}
public void setX(float pX)
{x = pX;
}

public float getY()
{   return y;
}
public void setY(float pY)
{y = pY;
}

public float getZ()
{   return z;
}
public void setZ(float pZ)
{z = pZ;
}

} // end of class Acceleration
