/*
 File:		TransmitterPdu.java
 CVS Info:	$Id: TransmitterPdu.java,v 1.0 2000/06/07 18:00:00 laflam Exp $
 Compiler:	jdk 1.3
 */

// This is the one to work on 


package PDUs;                // package for Naval Postgraduate School DIS Libaray

import mil.navy.nps.util.*;              // General-purpose utilities
import disEnumerations.*;   // Enumerations for DIS
import java.lang.*;
import java.util.*;                      // utility stuff we need
import java.io.*;                        // input/output for serialization

/**
 * Transmitter PDU for DIS .
 *
 *@version 1.0
 *@author <a href="mailto:dave@laflam.net">David W. Laflam</a> (<a href="http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/">http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/</a>)
 *<br>
 *@author <a href="mailto:brutzman@nps.navy.mil">Don Brutzman</a> (<a href="http://web.nps.navy.mil/~brutzman">http://web.nps.navy.mil/~brutzman</a>)
 *
 *<dt><b>Location:</b>
 *<dd>Web:  <a href="http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/TransmitterPdu.java">
 *  http://www.web3d.org/WorkingGroups/vrtp/mil/navy/nps/dis/FirePdu.java</a>
 *
 *<dd>or locally:  <a href="../../../../../../mil/navy/nps/dis/TransmitterPdu.java">
 *  ~/mil/navy/nps/dis/TransmitterPdu.java</a>
 *
 *<dt><b>Hierarchy Diagram:</b>
 *<dd><a href="../../../../../../dis-java-vrml/images/PduClassHierarchy.jpg"><IMG SRC="../../../../../../dis-java-vrml/images/PduClassHierarchyButton.jpg" ALIGN=ABSCENTER WIDTH=150 HEIGHT=21></a>
 *
 *<dt><b>Summary:</b>
 *<dd> Detailed information about a radio transmitter shall be communicated by issuing a Transmitter PDU.
 *
 *<dt><b>Explanation:</b>
 *<dd>The TransmitterPdu denotes the reciving of a transmission from a radio.
 *  It inherits the header information from ProtocolDataUnit,
 *  an abstract class that contains assorted protocol information.
 *  It implements the IDs of what's transmitting a signal,
 *<P>
 *
 *  As with other PDUs, it knows how to serialize and deserialize itself
 *  from the wire. It also knows how to clone itself, and knows how to
 *  calculate its size when sent to the wire.
 *<P>
 *
 *<dt><b>History:</b>
 *<dd>		15 May 2000	/Dave Laflam		/New
 *<dd>		17 August 2000 	/Dave Laflam		/Added toString method
 *<dd>		17 September 2000 	/Dave Laflam, Don Brutzman	/Improved Javadoc, integration/testing with other Radio PDUs
 *
 *<dt><b>References:</b>
 *<dd>		DIS Data Dictionary:
 *		<A href="../../../../../../mil/navy/nps/disEnumerations/JdbeHtmlFiles/pdu/dc.htm">CollisionTypeField PDU (local) and
 *    		<A href="http://SISO.sc.ist.ucf.edu/dis-dd/pdu/dc.htm">Transmitter PDU (SISO)</a>
 *<dd>		DIS-Java-VRML Working Group: <a href="http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/">
 *		http://www.web3d.org/WorkingGroups/vrtp/dis-java-vrml/</a>
 *<dd>		DIS specification : IEEE 1278.1-1995, Section 5.3.8.3
 *
 *<dt><b>Note:</b>
 *<dd>   Accessor methods to nested records are provided.
 *
 *@see EntityCoordinate
 *@see EntityID
 *@see ModulationType
 *@see PduElement
 *@see ProtocolDataUnit
 *@see RadioCommunicationsFamily
 *@see RadioCommunicationsPduScriptNode
 *@see RadioEntityType
 *@see ReceiverPdu
 *@see SerializationInterface
 *@see SignalPdu
 *@see WorldCoordinate
 *@see mil.navy.nps.disEnumerations.AntennaPatternTypefield
 *@see mil.navy.nps.disEnumerations.BeamFunctionField
 *@see mil.navy.nps.disEnumerations.CryptoSystemfield
 *@see mil.navy.nps.disEnumerations.EmitterNameField
 *@see mil.navy.nps.disEnumerations.MajorModulationTypefield
 *@see mil.navy.nps.disEnumerations.ModulationParameterfield
 *@see mil.navy.nps.disEnumerations.SpreadSpectrumfield
 *@see mil.navy.nps.disEnumerations.TransmitStatefield
 */
public class TransmitterPdu extends RadioCommunicationsFamily
{
    protected EntityID  entityID;     // (site,host,entity) 16 bit unsigned integer
                                      // ID of entity that's doing the shooting

    protected UnsignedShort  radioID;   // 16 bit unsigned integer
                                        // ID of entity that is undergoing darwinian selection
                                        // short 16 bit and ints are 32 bit

    protected RadioEntityType    radioEntityType   ;             // 64 Bit Radio Enity Type in RadioEnityType.java

    protected UnsignedByte       transmitState  ;                // 8-bit enumeration
    protected UnsignedByte       inputSource  ;                  // 8-bit enumeration
    protected UnsignedShort      padding1  ;                     // 16-bits unused
    
    protected WorldCoordinate    antennaLocation ;               // XYZ (64-bit floating point) WorldCoordinate.java
    
    protected EntityCoordinate   relativeAntennaLocation ;       // XYZ (32-bit floating point) EntityCoordinate.java
    
    protected UnsignedShort      antennaPatternType  ;           // 16-bits enumeration
    protected UnsignedShort      antennaPatternLength ;          // 16-unsigned integer
    protected UnsignedLong       frequency ;                     // 64-bit unsigned integer
    protected UnsignedInt        transmitFrequencyBandwidth ;    // 32-bit floating point
    protected UnsignedInt        power ;                         // 32-bit floating point
    
    protected ModulationType     modulationType ;                // spread spectrum 16 bit boolean 
                                                                 // (major,detailed,system) 
                                                                 //16 bit enumeration ModulationType.java
                                                                
    protected UnsignedShort      cryptoSytem ;                   // 16-bit enumeration
    protected UnsignedShort      cryptoKeyId ;                   // 16-bit unsigned integer
    protected UnsignedByte       lengthOfModulationParameters ;  // 8-bit unsigned integer
    protected UnsignedShort      padding2  ;                     // 24 bit unused (16 bits )
    protected UnsignedByte       padding3  ;                     //                (8 bits )
    // what about of the modulation parameters 
    
    
    /**
     *Constant value--size of TransmitterPDU with header. Here:
     *<code>sizeOf = 832 bytes</code>
     */
    public final static int     sizeOf = 832;       // is this the PDU Size Total DWL         // size of object as written to wire


public TransmitterPdu()
{
   super.setPduType(PduTypeField.TRANSMITTER);
      
    entityID = new EntityID();                             // (site,host,entity) 16 bit unsigned integer
    radioID = new UnsignedShort(0);                        // 16 bit unsigned integer
    
    radioEntityType = new RadioEntityType();               //
    
    transmitState = new UnsignedByte(0)   ;                // 8-bit enumeration
    inputSource = new UnsignedByte(0)  ;                   // 8-bit enumeration
    padding1 = new UnsignedShort(0);                       // 16-bits unused
    
    antennaLocation = new WorldCoordinate();               // XYZ (64-bit floating point) 
    relativeAntennaLocation = new EntityCoordinate() ;     // XYZ (32-bit floating point) 
    
    antennaPatternType =  new UnsignedShort(0);            // 16-bits enumeration
    antennaPatternLength = new UnsignedShort(0);           // 16-unsigned integer
    
    frequency =   new UnsignedLong(0);                     // 64-bit unsigned integer
    
    transmitFrequencyBandwidth = new  UnsignedInt(0);      // 32-bit floating point
    power = new  UnsignedInt(0);                           // 32-bit floating point
    
    modulationType = new ModulationType();                 // spread spectrum 16 bit boolean 
                                                           // (major,detailed,system) 
                                                           // 16 bit enumeration
    
    cryptoSytem = new UnsignedShort(0);                    // 16-bit enumeration
    cryptoKeyId = new UnsignedShort(0);                    // 16-bit unsigned integer
    lengthOfModulationParameters = new UnsignedByte(0) ;   // 8-bit unsigned integer

    padding2 = new UnsignedShort(0);                       // 16-bits unused
    padding3 = new UnsignedByte(0);                        // 16-bits unused
    
   return;
}// end public TransmitterPdu()

/**
 * Make a copy of the object. This requires a deep copy, so we don't have two
 * objects sharing pointers to the same data.
 * @return a new Fire PDU entity
 */
public Object clone()
{
 TransmitterPdu    newTransmitterPdu = (TransmitterPdu)super.clone(); // this will inherit from the super class //dwl

  newTransmitterPdu.setEntityID(this.getEntityID());
  newTransmitterPdu.setRadioID(this.getRadioID());
  newTransmitterPdu.setRadioEntityType(this.getRadioEntityType()); 
  newTransmitterPdu.setTransmitState(this.getTransmitState());
  newTransmitterPdu.setInputSource(this.getInputSource());
  newTransmitterPdu.setAntennaLocation(this.getAntennaLocation());  
  newTransmitterPdu.setRelativeAntennaLocation(this.getRelativeAntennaLocation());     
  newTransmitterPdu.setAntennaPatternType(this.getAntennaPatternType());       
  newTransmitterPdu.setAntennaPatternLength(this.getAntennaPatternLength());         
  newTransmitterPdu.setFrequency(this.getFrequency());
  newTransmitterPdu.setTransmitFrequencyBandwidth(this.getTransmitFrequencyBandwidth());
  newTransmitterPdu.setPower(this.getPower());  
  newTransmitterPdu.setModulationType(this.getModulationType());  
  newTransmitterPdu.setCryptoSytem(this.getCryptoSytem());  
  newTransmitterPdu.setCryptoKeyId(this.getCryptoKeyId());  
  newTransmitterPdu.setLengthOfModulationParameters(this.getLengthOfModulationParameters());    


 return newTransmitterPdu;
}// end public Object clone()

/**
 * Serialize and write out the output stream, order is important here since
 * it needs to conform to the DIS standard
 * @exception RuntimeException when IO error occurs.
 */
public void serialize(DataOutputStream outputStream)
{
    super.serialize(outputStream);      // write out header info

    try
    {
    
      entityID.serialize(outputStream); 
      radioID.serialize(outputStream); 
      radioEntityType.serialize(outputStream);  
      transmitState.serialize(outputStream);  
      inputSource.serialize(outputStream);  
      padding1.serialize(outputStream); 
      antennaLocation.serialize(outputStream);  
      relativeAntennaLocation.serialize(outputStream);  
      antennaPatternType.serialize(outputStream);  
      antennaPatternLength.serialize(outputStream);  
      frequency.serialize(outputStream);  
      transmitFrequencyBandwidth.serialize(outputStream);  
      power.serialize(outputStream);  
      modulationType.serialize(outputStream);  
      cryptoSytem.serialize(outputStream);  
      cryptoKeyId.serialize(outputStream);  
      lengthOfModulationParameters.serialize(outputStream);  
      padding2.serialize(outputStream); 
      padding3.serialize(outputStream); 
   

    }
    catch (Exception someError)
    {
        throw new
            RuntimeException("Exception in TransmitterPdu.serialize, error writing to wire.\n" + someError);
    }

    return;
} // end public void serialize()

/**
 * Deserialize the input stream, and order is important here, since we need to
 * read in the same order as specified by the DIS standard
 * @exception RuntimeException when IO error occurs.
 */
public void deSerialize(DataInputStream inputStream)
{
    super.deSerialize(inputStream);     // read in all the header info

    try
    {
      entityID.deSerialize(inputStream); 
      radioID.deSerialize(inputStream); 
      radioEntityType.deSerialize(inputStream); 
      transmitState.deSerialize(inputStream); 
      inputSource.deSerialize(inputStream); 
      padding1.deSerialize(inputStream); 
      antennaLocation.deSerialize(inputStream); 
      relativeAntennaLocation.deSerialize(inputStream); 
      antennaPatternType.deSerialize(inputStream); 
      antennaPatternLength.deSerialize(inputStream); 
      frequency.deSerialize(inputStream); 
      transmitFrequencyBandwidth.deSerialize(inputStream); 
      power.deSerialize(inputStream); 
      modulationType.deSerialize(inputStream); 
      cryptoSytem.deSerialize(inputStream); 
      cryptoKeyId.deSerialize(inputStream); 
      lengthOfModulationParameters.deSerialize(inputStream); 
      padding2.deSerialize(inputStream); 
      padding3.deSerialize(inputStream);  
     
    }
    catch (Exception someError)
    {
            throw new
                RuntimeException("Exception in TransmitterPdu.deSerialize, error reading from wire.\n" + someError);
    }
}// end public void deSerialize()


/**
 * Returns the length of the entity
 * @return an integer length of the entity
 */
public int length()
{
    return sizeOf;          // EntityTypes are this long, always.  This is the 288
}// end public int length()


/**
 * Returns the PDU name - Transmitter PDU
 * @return a string "Transmitter PDU"
 */
public String pduName()
{
  return new String("Transmitter PDU");
} //end public String pduName()


/**
 * Print the values of the following object out, with correct level of
 * indentation on the page.
 * firingEntityID, targetEntityID, munitionID, fireMissionIndex, locationInWorldCoordinate,
 * burstDescriptor, velocity, and range.
 */
public void printValues(int indentLevel, PrintStream printStream)
{

  StringBuffer indent = ProtocolDataUnit.getPaddingOfLength(indentLevel);
  int          idx, superclassIndent = indentLevel;

    printStream.println();
    printStream.println("Transmitter PDU-");

    // ugly wart: get the superclass over to the left a couple pixels, if we have any to spare,
    // so the header info will be indented a bit less.

    if(superclassIndent > 0)
      superclassIndent -= 1;

    super.printValues(superclassIndent, printStream);
    entityID.printValues(indentLevel, printStream);
    printStream.println(indent + "radioID: " + radioID);  // print the primitive type
   
    printStream.println(indent + "radioEntityType: " + radioEntityType);
    printStream.println(indent + "transmitState: " + transmitState);
    printStream.println(indent + "inputSource: " + inputSource);
    printStream.println(indent + "padding1: " + padding1);
    printStream.println(indent + "antennaLocation: " + antennaLocation);
    printStream.println(indent + "relativeAntennaLocation : " + relativeAntennaLocation );
    printStream.println(indent + "antennaPatternType: " + antennaPatternType);
    printStream.println(indent + "antennaPatternLength: " + antennaPatternLength);
    printStream.println(indent + "frequency: " + frequency);
    printStream.println(indent + "transmitFrequencyBandwidth: " + transmitFrequencyBandwidth);
    printStream.println(indent + "power: " + power);
    printStream.println(indent + "modulationType: " + modulationType);
    printStream.println(indent + "cryptoSytem: " + cryptoSytem);
    printStream.println(indent + "cryptoKeyId: " + cryptoKeyId);
    printStream.println(indent + "lengthOfModulationParameters: " + lengthOfModulationParameters);
    printStream.println(indent + "padding2: " + padding2);
    printStream.println(indent + "padding3: " + padding3);

    // printStream.println(indent + "transmitterRadioID: " + transmitterRadioID);  // print the primitive type   
    // transmitterRadioID.printValues(indentLevel, printStream);

    return;
}// end public void printValues(



//Accessor methods (Set and Get)

/**
 * Gets entity ID.
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @return a clone of the firing entity ID
 */
public EntityID getEntityID()
{
	return (EntityID)entityID.clone();
}

/**
 * Sets entity ID
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @param pFiringEntityID the firing entity ID
 */
public void setEntityID(EntityID pEntityID)
{
	entityID = pEntityID;
}

/**
 *Sets setEntityID(short pSiteID, short pApplicationID, short pEntityID),accessor method.
 *will create an new EntityID = entityID
 *This field shall identify the entity issuing the PDU,
 * and shall be represented by the PDU Header Record (see 5.2.24)
 */
public void setEntityID(short pSiteID, short pApplicationID, short pEntityID)
{ entityID = new EntityID(pSiteID, pApplicationID, pEntityID);
}


/**
 * Gets RadioID.
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @return a clone of the radioID
 */
public UnsignedShort getRadioID()
{
	return (UnsignedShort)radioID.clone();
}

/**
 * Sets RadioID.
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @param pRadioID of radioID
 */
public void setRadioID(UnsignedShort pRadioID)
{
	radioID = pRadioID;
}

/**
 * Gets RadioID.
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @return a clone of the radioID
 */

public RadioEntityType  getRadioEntityType()
{
	return (RadioEntityType)radioEntityType.clone();
}

/**
 * Sets RadioEntityType
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @param pRadioEntityType of RadioEntityType.
 */
public void setRadioEntityType(RadioEntityType pRadioEntityType)
{
	radioEntityType = pRadioEntityType;
	System.out.println("set a new radio entity type of " + radioEntityType);
}

/**
 * Gets TransmitState.
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @return a clone of the transmitState
 */

public UnsignedByte  getTransmitState()
{
	return (UnsignedByte)transmitState.clone();
}

/**
 * Sets TransmitState
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @param pTransmitState of TransmitState.
 */
public void setTransmitState(UnsignedByte pTransmitState)
{
	transmitState = pTransmitState;
}

/**
 * Gets InputSource.
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @return a clone of the inputSource
 */
public UnsignedByte  getInputSource()
{
	return (UnsignedByte)inputSource.clone();
}

/**
 * Sets InputSource
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @param pInputSource of InputSource.
 */
public void setInputSource(UnsignedByte pInputSource)
{
	inputSource = pInputSource;
}

// no need for a get and set for the padding //DWL
// protected UnsignedShort      padding                        // 16-bits unused


/**
 * Gets AntennaLocation.
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @return a clone of the antennaLocation
 */
public WorldCoordinate  getAntennaLocation()
{
	return (WorldCoordinate)antennaLocation.clone();
}

/**
 * Sets AntennaLocation
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @param pAntennaLocation of AntennaLocation.
 */
public void setAntennaLocation(WorldCoordinate pAntennaLocation)
{
	antennaLocation = pAntennaLocation;
}

/**
 * Sets AntennaLocation
 */
public void setAntennaLocation(double pX, double pY, double pZ)
{
	antennaLocation = new WorldCoordinate (pX, pY, pZ);
}

/**
 * Gets RelativeAntennaLocation.
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @return a clone of the relativeAntennaLocation
 */
public EntityCoordinate  getRelativeAntennaLocation()
{
	return (EntityCoordinate)relativeAntennaLocation.clone();
}

/**
 * Sets RelativeAntennaLocation
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @param pRelativeAntennaLocation of RelativeAntennaLocation.
 */
public void setRelativeAntennaLocation(EntityCoordinate pRelativeAntennaLocation)
{
	relativeAntennaLocation = pRelativeAntennaLocation;
}

/**
 * Gets AntennaPatternType.
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @return a clone of the antennaPatternType.
 */
public UnsignedShort  getAntennaPatternType()
{
	return (UnsignedShort)antennaPatternType.clone();
}

/**
 * Sets AntennaPatternType
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @param pAntennaPatternType of AntennaPatternType.
 */
public void setAntennaPatternType(UnsignedShort pAntennaPatternType)
{
	antennaPatternType = pAntennaPatternType;
}

/**
 * Gets AntennaPatternLength.
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @return a clone of the antennaPatternLength
 */
public UnsignedShort  getAntennaPatternLength()
{
	return (UnsignedShort)antennaPatternLength.clone();
}

/**
 * Sets AntennaPatternLength
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @param pAntennaPatternLength of AntennaPatternLength.
 */
public void setAntennaPatternLength(UnsignedShort pAntennaPatternLength)
{
	antennaPatternLength = pAntennaPatternLength;
}

/**
 * Gets Frequency.
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @return a clone of the frequency
 */
public UnsignedLong  getFrequency()
{
	return (UnsignedLong)frequency.clone();
}

/**
 * Sets Frequency
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @param pFrequency of Frequency.
 */
public void setFrequency(UnsignedLong pFrequency)
{
	frequency = pFrequency;
}

/**
 * Gets TransmitFrequencyBandwidth.
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @return a clone of the transmitFrequencyBandwidth
 */
public UnsignedInt  getTransmitFrequencyBandwidth()
{
	return (UnsignedInt)transmitFrequencyBandwidth.clone();
}

/**
 * Sets TransmitFrequencyBandwidth
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @param pTransmitFrequencyBandwidth of TransmitFrequencyBandwidth.
 */
public void setTransmitFrequencyBandwidth(UnsignedInt pTransmitFrequencyBandwidth)
{
	transmitFrequencyBandwidth = pTransmitFrequencyBandwidth;
}

/**
 * Gets Power.
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @return a clone of the power
 */
public UnsignedInt  getPower()
{
	return (UnsignedInt)power.clone();
	//return power;
}

/**
 * Sets Power
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @param pPower of Power.
 */
public void setPower(UnsignedInt pPower)
{
	power = pPower;
}

/**
 * Gets ModulationType.
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @return a clone of the modulationType
 */
public ModulationType  getModulationType()
{
	return (ModulationType)modulationType.clone();
}

/**
 * Sets ModulationType
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @param pModulationType of ModulationType.
 */
public void setModulationType(ModulationType pModulationType)
{
	modulationType = pModulationType;
}

/**
 * Gets CryptoSytem.
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @return a clone of the cryptoSytem
 */
public UnsignedShort  getCryptoSytem()
{
	return (UnsignedShort)cryptoSytem.clone();
}

/**
 * Sets CryptoSytem
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @param pCryptoSytem of CryptoSytem.
 */
public void setCryptoSytem(UnsignedShort pCryptoSytem)
{
	cryptoSytem = pCryptoSytem;
}

/**
 * Gets CryptoKeyId.
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @return a clone of the cryptoKeyId
 */
public UnsignedShort  getCryptoKeyId()
{
	return (UnsignedShort)cryptoKeyId.clone();
}

/**
 * Sets CryptoKeyId
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @param pCryptoKeyId of CryptoKeyId.
 */

public void setCryptoKeyId(UnsignedShort pCryptoKeyId)
{
	cryptoKeyId = pCryptoKeyId;
}

/**
 * Gets LengthOfModulationParameters.
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @return a clone of the lengthOfModulationParameters
 */
public UnsignedByte  getLengthOfModulationParameters()
{
	return (UnsignedByte)lengthOfModulationParameters.clone();
}

/**
 * Sets LengthOfModulationParameters
 * Each Entity in a given exercise executing on a DIS application shall be assigned an Entity Identifier Record
 * Unique to the exercise.
 * @param pLengthOfModulationParameters of LengthOfModulationParameters.
 */
public void setLengthOfModulationParameters(UnsignedByte pLengthOfModulationParameters)
{
	lengthOfModulationParameters = pLengthOfModulationParameters;
}


// no need for a get and set for the padding2 padding3 //DWL
// protected                                  padding  
                      // 24 bit unsued (write 3 bytes ?)
 
/**
  * String toString
  * Used for debuging  
  * System.out.println("Transmitter Object.  = " + transmitter); 
  * This print out all values for the fields for the NEW Transmitter object
  */ 
 
 public String toString ()
 {
 	String result;
 	result = "\nEntityID = " + entityID + " \nRadioID = " + radioID + " \nRadioEntityType = " + radioEntityType +
 	" \nTransmitState = " + transmitState + "\nInputSource = " + inputSource + "\nPadding1 = " + padding1 + 
 	" \nAntennaLocaction = " + antennaLocation + "\nRelativeAntennaLocation = " + relativeAntennaLocation + 
 	" \nAntennaPatternType = " + antennaPatternType + "\nAntennaPatternLength = " + antennaPatternLength + 
 	" \nFrequency = " + frequency + 
 	" \nTransmitFrequencyBandwidth = " + transmitFrequencyBandwidth +  "\nPower = " +  power ; 
 	
 return result ; 
 }
    


} // end of class TransmitterPdu.java

