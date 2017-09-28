/*
 File:		LinearVelocity.java
 CVS Info:	$Id: LinearVelocity.java,v 1.2 1998/01/27 18:44:18 mcgredo Exp $
 Compiler:	jdk 1.3
 */

package mil.navy.nps.dis;

import mil.navy.nps.util.*;         // General-purpose utilities
import mil.navy.nps.disEnumerations.*;         // Enumerations for DIS

import java.lang.*;
import java.util.*;
import java.io.*;

/**
 * Linear velocity of an entity.
 *
 *@version 1.0
 *@author <a href="mailto:mcgredo@cs.nps.navy.mil"> Don McGregor</a> (<a href="http://www.npsnet.org/~mcgredo">http://www.npsnet.org/~mcgredo</a>)
 *
 *<dt><b>Location:</b>
 *<dd>Web:  <a href="http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/LinearVelocity.java">
 *  http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/LinearVelocity.java</a>
 *
 *<dd>or locally:  <a href="../../../../../../mil/navy/nps/dis/LinearVelocity.java">
 *  ~/mil/navy/nps/dis/LinearVelocity.java</a>
 *
 *<dt><b>Hierarchy Diagram:</b>
 *<dd><a href="../../../../../../dis-java-vrml/images/PduClassHierarchy.jpg"><IMG SRC="../../../../../../dis-java-vrml/images/PduClassHierarchyButton.jpg" ALIGN=ABSCENTER WIDTH=150 HEIGHT=21></a>
 *
 *<dt><b>Summary:</b>
 *<dd>Linear Velocity shall be represented as a vector with three components. 
 *  Each vector component shall represent velocity in meters per second. 
 *<dt><b>Explanation</b>
 *<dd>Describes the velocity of an object, in x, y, z 32 bit
 *  floating point terms. There are several classes that consist
 *  only of three 32-bit quantities, but we split them out to 
 *  separate classes in the interests of type safety. 
 *
 *<dt><b>History:</b>
 *<dd>		16Dec96	/Don McGregor    	/New
 *<dd>		07Mar97	/Don McGregor    	/Javadoc changes, added setValues method
 *<dd>		16Apr97	/Don McGregor    	/PrintStream passed to printValues
 *<dd>		12Aug97	/Don McGregor    	/elaborated printValues
 *<dd>		8Dec97	/Ronan Fauglas		/changes for documentation templates + complements in documentation
 *<dd>		11Dec97	/Ronan Fauglas		/changes access methods names from "variable()" to "getVariable()"
 *
 *<dt><b>References:</b>
 *<dd>		DIS Data Dictionary: <a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/34.htm">Linear velocity vector</a>
 *<dd>		DIS specification : IEEE 1278.1, 5.3.33.3, 1.3.2	
 *
 *@see PduElement 
 *@see SerializationInterface
 *@see LinearAcceleration
 *@see EntityCoordinate
 */
public class LinearVelocity extends PduElement
{
    /**
     *First Velocity Component, along X axis
     */
    protected float  x;     // x-position

    /**
     *Second Velocity Component, along Y axis
     */
    protected float  y;     // y-position

    /**
     *Third Velocity Component, along Z axis
     */
    protected float  z;     // z-position

    /**
     *Constant value--size of a LinearVelocity Record when written out; here :<code>sizeOf = 12 bytes</code>.
     */
    public final int sizeOf = 12;   // object is 12 bytes long 

/**
 *Constructs an new LinearVelocity Vector, LinearVelocity's value is 0.
 */
public LinearVelocity()
{
    // default constructor

    x = 0; 
    y = 0;
    z = 0;

    return;
}

/**
 *Constructs a new LinearVelocity Vector with coordinate values passed in parameters.
 *
 *@param pX the first coordinate in the cartesian coordinate system
 *@param pY the second coordinate in the cartesian coordinate system
 *@param pZ the third coordinate in the cartesian coordinate system
 */
public LinearVelocity(float pX, float pY, float pZ)
{
    x = pX;
    y = pY;
    z = pZ;

    return;
}

public Object clone()
{
 LinearVelocity   newLinearVelocity = new LinearVelocity(x, y, z);

 return newLinearVelocity;
}

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
                RuntimeException("Exception in LinearVelocity. Error writing to wire.");
        }
}

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
                RuntimeException("Exception in LinearVelocity. Error reading from wire.");
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

    printStream.println(indent + "LinearVelocity x: "   + x);
    printStream.println(indent + "LinearVelocity y: "   + y);
    printStream.println(indent + "LinearVelocity z: "   + z);

    return;
}

// Accessor methods

public void setValues(float pX, float pY, float pZ)
{
  x = pX;
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

} // end of class LinearVelocity
