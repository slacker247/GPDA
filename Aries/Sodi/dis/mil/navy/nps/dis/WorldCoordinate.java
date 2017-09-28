/*
 File:		WorldCoordinate.java
 CVS Info:	$Id: WorldCoordinate.java,v 1.2 1998/01/27 18:44:35 mcgredo Exp $
 Compiler:	jdk 1.3 
 */

package mil.navy.nps.dis;

import mil.navy.nps.util.*;         // General-purpose utilities
import mil.navy.nps.disEnumerations.*;         // Enumerations for DIS

import java.lang.*;
import java.util.*;
import java.io.*;

/**
 * Entity location in world coordinates.
 *
 *@version 1.0
 *@author <a href="mailto:mcgredo@cs.nps.navy.mil"> Don McGregor</a> (<a href="http://www.npsnet.org/~mcgredo">http://www.npsnet.org/~mcgredo</a>)
 *
 *<dt><b>Location:</b>
 *<dd>Web:  <a href="http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/WorldCoordinate.java">
 *  http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/WorldCoordinate.java</a>
 *
 *<dd>or locally:  <a href="../../../../../../mil/navy/nps/dis/WorldCoordinate.java">
 *  ~/mil/navy/nps/dis/WorldCoordinate.java</a>
 *
 *<dt><b>Hierarchy Diagram:</b>
 *<dd><a href="../../../../../../dis-java-vrml/images/PduClassHierarchy.jpg"><IMG SRC="../../../../../../dis-java-vrml/images/PduClassHierarchyButton.jpg" ALIGN=ABSCENTER WIDTH=150 HEIGHT=21></a>
 *
 *<dt><b>Summary:</b>
 *<dd>Location of the origin of the entity's coordinate system shall be specified 
 *  by a set of three coordinates: X, Y, and Z. 
 *
 *<dt><b>Explanation</b>
 *<dd>The WorldCoordinate class describes the location of an entity in 
 *  64-bit x,y,z coordinates. This crops up often enough to warrant
 *  its own class. (This is also known as "World Coordinates", 
 *  as contrasted to "entity coordinates", which are 32-bit.<P>
 *
 *<dt><b>History:</b>
 *<dd>		12Dec96 /Don McGregor    	/New
 *<dd>		10Mar97 /Don McGregor    	/changes for javadoc
 *<dd>		16Apr97 /Don McGregor    	/PrintStream passed to printValues<BR>
 *<dd>		12Aug97 /Don McGregor    	/elaborated printValues
 *<dd>		8Dec97	/Ronan Fauglas		/changes for documentation templates + complements in documentation
 *<dd>		11Dec97	/Ronan Fauglas		/changes access methods names from "variable()" to "getVariable()"
 *<dd>		11Dec97	/Ronan Fauglas		/changed "  class" to "public class" (was private by default).
 *
 *<dt><b>References:</b>
 *<dd>	DIS Data Dictionary:
 *	<a href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/19.htm">World Coordinate Record</a>
 *<dd>	DIS specification : IEEE 1278.1, 5.3.34
 *
 *@see PduElement 
 *@see SerializationInterface
 *@see EntityCoordinate
 */
public class WorldCoordinate extends PduElement
{

    /**
     *First coordinate of the entity's coordinate system , along X axis
     */
    protected double  x;        // x-position

    /**
     *Second coordinate of the entity's coordinate system , along Y axis
     */
    protected double  y;        // y-position

    /**
     *Third coordinate of the entity's coordinate system , along Z axis
     */
    protected double  z;        // z-position

    /**
     *Constant value--size of a World Coordinate record when written out; here :<code>sizeOf = 24 bytes</code>.
     */
    public final int sizeOf = 24;   // object is 24 bytes long 


/**
 *Constructs an new WorldCoordinate Object, centered.
 */
public WorldCoordinate()
{
    // default constructor

    x = 0.0; 
    y = 0.0;
    z = 0.0;

    return;
}

/**
 *Constructs a new WorldCoordinate Object whose coordinate values are passed in parameters.
 *
 *@param pX the first  coordinate in the cartesian coordinate system
 *@param pY the second coordinate in the cartesian coordinate system
 *@param pZ the third  coordinate in the cartesian coordinate system
 */
public WorldCoordinate(double pX, double pY, double pZ)
{
    x = pX;
    y = pY;
    z = pZ;

    return;
}

public Object clone()
{

 WorldCoordinate  newWorldCoordinate = new WorldCoordinate(x, y, z);

 return newWorldCoordinate;
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
        outputStream.writeDouble(x);
        outputStream.writeDouble(y);
        outputStream.writeDouble(z);
     }
    catch (IOException ioError)
        {
            throw new 
                RuntimeException("Exception in WorldPosition. Error writing to wire.");
        }
}

/**
 *@exception RuntimeException when IO error occurs.
 */
public void deSerialize(DataInputStream pInputStream)
{
    try
     {
        x = pInputStream.readDouble();
        y = pInputStream.readDouble();
        z = pInputStream.readDouble();
     }
     catch (IOException ioError)
        {
            throw new 
                RuntimeException("Exception in WorldPosition. Error reading from wire.");
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

    printStream.println(indent + "WorldCoordinate x: "   + x);
    printStream.println(indent + "WorldCoordinate y: "   + y);
    printStream.println(indent + "WorldCoordinate z: "   + z);

    return;
}

// Accessor methods

public double getX()
{   return x;
}
public void setX(double pX)
{x = pX;
}

public double getY()
{   return y;
}
public void setY(double pY)
{y = pY;
}

public double getZ()
{   return z;
}
public void setZ(double pZ)
{z = pZ;
}

} // end of class WorldCoordinate
