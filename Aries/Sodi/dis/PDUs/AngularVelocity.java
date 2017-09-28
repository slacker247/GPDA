/*
 File:		AngularVelocity.java
 CVS Info:	$Id: AngularVelocity.java,v 1.2 1998/01/27 18:43:53 mcgredo Exp $
 Compiler:	jdk 1.3
 */

package PDUs;

import mil.navy.nps.util.*;         // General-purpose utilities
import disEnumerations.*;         // Enumerations for DIS

import java.lang.*;
import java.util.*;
import java.io.*;

/**
 * Angular (rotational) velocity.
 *
 *@version 1.0
 *@author <a href="mailto:mcgredo@cs.nps.navy.mil"> Don McGregor</a> (<a href="http://www.npsnet.org/~mcgredo">http://www.npsnet.org/~mcgredo</a>)
 *
 *<dt><b>Location:</b>
 *<dd>Web:  <a href="http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/AngularVelocity.java">
 *  http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/AngularVelocity.java</a>
 *
 *<dd>or locally:  <a href="../../../../../../mil/navy/nps/dis/AngularVelocity.java">
 *  ~/mil/navy/nps/dis/AngularVelocity.java</a>
 *
 *<dt><b>Hierarchy Diagram:</b>
 *<dd><a href="../../../../../../dis-java-vrml/images/PduClassHierarchy.jpg"><IMG SRC="../../../../../../dis-java-vrml/images/PduClassHierarchyButton.jpg" ALIGN=ABSCENTER WIDTH=150 HEIGHT=21></a>
 *
 *<dt><b>Summary:</b>
 *<dd>The angular velocity of simulated entities shall be represented by the Angular Velocity Vector Record. 
 *  This record shall specify the rate at which an entity's orientation is changing. 
 *  This rate shall be measured in radians per second measured about 
 *  <b>each of the entity's own coordinate axes</b>. 
 *<dt><b>Explanation</b>
 *<dd>Describes the angular velocity of the object about the
 * x, y, and z axis with 32 floating points terms in radian's per second. 
 *
 *<dt><b>History:</b>
 *<dd>		07Mar97	/Don McGregor    	/New
 *<dd>		16Apr97	/Don McGregor    	/PrintStream passed to printValues
 *<dd>		12Aug97	/  Don Brutzman		/elaborated printValues
 *<dd>		8Dec97	/Ronan Fauglas		/changes for documentation templates + complements in documentation
 *<dd>		11Dec97	/Ronan Fauglas		/changes access methods names from "variable()" to "getVariable()"
 *
 *<dt><b>References:</b>
 *<dd>		DIS Data Dictionary: <a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/12.htm">Angular Velocity Vector Record</a>
 *<dd>		DIS specification : IEEE 1278.1, Section 1.3.2, 5.3.2	
 *
 *@see PduElement 
 *@see SerializationInterface
*/

public class AngularVelocity extends PduElement
{
    /**
     *Rate about x-axis.
     *<dl>
     *<dt><b>Value:</b>
     *<dd>In radian by second.
     *</dl>
     */
    protected float  x;     // x-position

    /**
     *Rate about y-axis.
     *<dl>
     *<dt><b>Value:</b>
     *<dd>In radian by second.
     *</dl>
     */
    protected float  y;     // y-position

    /**
     *Rate about z-axis.
     *<dl>
     *<dt><b>Value:</b>
     *<dd>In radian by second.
     *</dl>
     */
    protected float  z;     // z-position

    /**
     *Constant value--size of an Angular Velocity Record when written out; here :<code>sizeOf = 12 bytes</code>.
     */
    public final int sizeOf = 12;   // object is 12 bytes long 

/**
 *Constructs an new Angular Velocity Vector, velocity's value is 0.
 */
public AngularVelocity()
{
    // default constructor

    x = 0; 
    y = 0;
    z = 0;

    return;
}

/**
 *Constructs a new Angular Velocity Vector with coordinate values passed in parameters.
 *
 *@param pX the first coordinate in the cartesian coordinate system
 *@param pY the second coordinate in the cartesian coordinate system
 *@param pZ the third coordinate in the cartesian coordinate system
 */
public AngularVelocity(float pX, float pY, float pZ)
{
    x = pX;
    y = pY;
    z = pZ;

    return;
}

public Object clone()
{

 AngularVelocity   newAngularVelocity = new AngularVelocity(x, y, z);

 return newAngularVelocity;
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
                RuntimeException("Exception in AngularVelocity. Error writing to wire.");
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
                RuntimeException("Exception in AngularVelocity. Error reading from wire.");
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

    StringBuffer buf = ProtocolDataUnit.getPaddingOfLength(indentLevel);
    int idx, superclassIndent = indentLevel;

    printStream.println(buf + "AngularVelocity x: "   + x);
    printStream.println(buf + "AngularVelocity y: "   + y);
    printStream.println(buf + "AngularVelocity z: "   + z);

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

} // end of class AngularVelocity
