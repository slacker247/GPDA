/*
 File:		SimulationManagementFamily.java
 CVS Info:	$Id: SimulationManagementFamily.java,v 1.2 1998/01/27 18:44:27 mcgredo Exp $
 Compiler:	jdk 1.3
 */

package PDUs;		// package to which we belong

import mil.navy.nps.util.*;		// General-purpose utilities
import disEnumerations.*;	// Enumerations for DIS

import java.util.*;			// utility stuff we need
import java.io.*;			// input/output for serialization

/**
 * Abstract (uninstantiated) parent class for Simulation Management PDU family.
 *
 *@version 1.0
 *@author Antonio Alexandre Rua (<A HREF="http://www.garfield.fe.up.pt/~alexrua">http://www.garfield.fe.up.pt/~alexrua</A>)
 *
 *<dt><b>Location:</b>
 *<dd>Web:  <a href="http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/SimulationManagementFamily.java">
 *  http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/SimulationManagementFamily.java</a>
 *
 *<dd>or locally:  <a href="../../../../../../mil/navy/nps/dis/SimulationManagementFamily.java">
 *  ~/mil/navy/nps/dis/SimulationManagementFamily.java</a>
 *
 *<dt><b>Hierarchy Diagram:</b>
 *<dd><a href="../../../../../../dis-java-vrml/images/PduClassHierarchy.jpg"><IMG SRC="../../../../../../dis-java-vrml/images/PduClassHierarchyButton.jpg" ALIGN=ABSCENTER WIDTH=150 HEIGHT=21></a>
 *
 *<dt><b>Summary:</b>
 *<dd>The Simulation Management Family is an abstract class, inherited by all simulation
 *  management family PDUs. 
 *
 *<dt><b>Explanation:</b>
 *<dd>It encapsulates the specific header for the simulation Management PDUs 
 *  which includes the PDU header, the originating entity identity, and
 *  The advantage is that, we define methods to deal with this header, 
 *  and all the classes inherit from it, avoiding
 *  rewriting them a lot of times.
 *
 *<dt><b>History:</b>
 *<dd>		16Sep97	/Antonio Alexandre Rua	/New
 *<dd>		8Dec97	/Ronan Fauglas		/changes for documentation templates + complements in documentation
 *<dd>		11Dec97	/Ronan Fauglas		/changed access methods: thisVariable() --> getThisVariable()
 *<dd>		17July2000	/Don Brutzman and Dave Laflam		/renamed SimulationManagementPdu -> SimulationManagementFamily
 *
 *<dt><b>References:</b>
 *<dd>		DIS-Java-VRML Working Group: <a href="http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/"> 
 *		http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/</a>
 *<dd>		DIS specification : IEEE 1278.1, Section 5.3.29, 4.4.5.1
 *
 *@see ProtocolDataUnit
 *@see PduElement 
 *@see SerializationInterface
 */
public abstract class SimulationManagementFamily_1 extends ProtocolDataUnit 
{
    /**
     *Identifier of the entity originating the PDU.
     *The semantics depend on derived class type.
     */
    protected EntityID      originatingEntityID;   // Originating Entity, semantics depend on derived class type

    /**
     *Identifier of the entity receiving the PDU.
     *The semantics depend on derived class type.
     */
    protected EntityID      receivingEntityID;     // Receiving Entity, semantics depend on derived class type


    /**
     *Constant value--size of a Simulation Management  PDU without headder.
     *<code>sizeOf = 12 bytes</code>
     */
    public static final int sizeOf = 12;
		// 12 bytes long (exclusive of ProtocolDataUnit general header data)
		// stands for originatingEntityID.sizeOf()+receivingEntityID.sizeOf()

  
    public SimulationManagementFamily_1()						// default constructor
{
  originatingEntityID = new EntityID();
  receivingEntityID   = new EntityID();

  return;
}

public void  serialize(DataOutputStream outputStream)
{
 /**
    Serialize our data out to the stream. We first call the superclass,
    so that all the data there is written out to the buffer first.
    Then we serialize each of our ivars to the stream, being careful
    about the order.
*/
  // takes care of all the header variables in superclass
  super.serialize(outputStream);
  
  // write out the entity IDs...
  originatingEntityID.serialize(outputStream);
  receivingEntityID.serialize(outputStream);

  return;
}

public void  deSerialize(DataInputStream inputStream)
{
 /**
    deserialize our data from the stream. We first call the superclass,
    so that all the data there is read into the buffer first.
    Then we deserialize each of our ivars from the stream, being careful
    about the order.
 */
  //read protocol data unit general header first
  super.deSerialize(inputStream);

  // Read in Entity IDs...
  originatingEntityID.deSerialize(inputStream);
  receivingEntityID.deSerialize(inputStream);

  return;
}

public Object clone()
{
 /**
    Clone the SimulationManagementFamily Pdu, being careful to not share any pointers between the
    new object and the old object.
  */

	SimulationManagementFamily newPdu = (SimulationManagementFamily)super.clone();

	newPdu.setOriginatingEntityID(this.getOriginatingEntityID());
	newPdu.setReceivingEntityID(this.getReceivingEntityID());

	return newPdu;
}

public int length()
{
  return (super.length() + sizeOf);
}

/**
 *Returns the name of this PDU
 *
 *@return the name of this PDU
 */
public abstract String pduName();


public void printValues(int indentLevel, PrintStream printStream)
{
  int superclassIndent = indentLevel;     // level of indentation for our superclass

  // ugly wart: get the superclass over to the left a couple pixels, if we have any to spare,
  // so the header info will be indented a bit less.
  if(superclassIndent > 0)
    superclassIndent -= 1;
  super.printValues(superclassIndent, printStream);

  originatingEntityID.printValues(indentLevel, printStream);
  receivingEntityID.printValues(indentLevel, printStream);

  return;
}


// Accessor methods

public EntityID getOriginatingEntityID()
{	return (EntityID)originatingEntityID.clone();
}
public void setOriginatingEntityID(EntityID pEntityID)
{	originatingEntityID = pEntityID;
}

public EntityID getReceivingEntityID()
{	return (EntityID)receivingEntityID.clone();
}
public void setReceivingEntityID(EntityID pEntityID)
{	receivingEntityID = pEntityID;
}

}   // end of class SimulationManagementFamily
