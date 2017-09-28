(needs review & date)

DIS Package


This package implements a subset of the DIS protocol. A few highlights:

Java DIS Class Library

(This is slightly out of date, but valuable none the less.  The EntityManager
class described here is not used in the demo; see the BoidsDemoEai directory for details.)


This is a collection of files that (partially) implement the Distributed Interactive 
Simulation (DIS) P1278.1 protocol in Java. A Java implementation has a number of advantages, 
such as cross-platform compatibility and the prospect of using DIS in web browsers or 
interacting with VRML. The genesis of this project is an attempt to implement a VRML/DIS 
display in a web browser. For details on this project, see http://www.stl.nps.navy.mil/~brutzman. 

Since Java is fairly new to a number of people, I’ll attempt to explain things here clearly 
enough for a Java novice to understand. This will make things a bit verbose for those 
familiar with the language.

DIS

The DIS protocol is used to simulate large forces interoperating in a single virtual world. 
Each simulator writes DIS packets to the network describing the state of the objects it is 
simulating; in turn, the simulator reads DIS packets from the wire that describe the state 
of objects maintained by other simulators. All this information is integrated in the simulator’s 
display such that all objects appear to be seamlessly interoperating. Entities in one simulator 
can see and interact with entities controlled by another simulator.

Problem Decomposition

While this package is likely to be used with a web browser and VRML--at least that was the initial 
objective--DIS can potentially be used in many other situations. So it makes sense to make the DIS 
portion cleanly separate from any other part of our current project, particularly any VRML or browser 
elements. Java makes this fairly easy to do through the use of packages, which create smaller 
namespaces. Packages are typically used to isolate a number of classes that implement a related task. 
The java.io package, for example, implements input/output functionality for the Java language. A 
class name in the java.io package won’t conflict with another class that has the same name in another 
package.

There is an internet convention for naming packages implemented by programmers that are not part of 
the standard Java class library: reverse the internet domain name of your company, then add a descriptive 
name on the end. Since NPS’s domain name is nps.navy.mil, our packages will be named mil.navy.nps.*. 

Mil.navy.nps.dis Package

This package implements reading and writing to the network for DIS packets. Rather than attempt to document 
each and every class, I’ll instead concentrate on the concepts behind the class design. The comments in the 
source files give a reasonably good idea of what the classes are actually doing. For the most part, they are 
remarkably similar.

Each packet described in the P1278.1 spec can be implemented as a Java class. For example, the Entity State 
PDU (ESPDU) is described in the spec in part as follows:

ESPDU
Pdu Header	
Protocol Version  (8 bit enumeration)
Exercise ID       (8 bit unsigned integer)
PDU Type          (8 bit enumeration)
Protocol Family   (8 bit enumeration)
Time Stamp        (32 bit unsigned integer)
Length            (16 bit unsigned integer)
Padding           (16 bits unused)
Entity ID, Site   (16 bit Unsigned Integer)
Application       (16 bit unsigned integer)
Entity            (16 bit unsigned integer)


Java PDU classes are straightforward wrappers around this data. In this case, the ESPDU class would 
contain instance variables for the protocol version, packet length, and so on. The ESPDU class 
implements a standard interface for accessing and changing the instance variables.

An obvious opportunity for code reuse occurs here. It turns out that every DIS packet has the "PDU Header" 
information at the top, and many also use "Entity ID" information or other common blocks of data, such 
as position or velocity.  So it makes sense to create objects that contain this information, and then 
either compose higher level objects, such as the ESPDU, from these objects, or use inheritance to 
increase code reuse. It further turns out that the "PDU Header" and "ESPDU" objects share many qualities 
as well. Both need to know how to read and write themselves to the network, for example, and clone 
(copy) themselves.

The fallout of these observations is a class inheritance structure that looks like this:


                                        +---------------+
                                        | PDUElement    |
                                        +---------------+
                                               |
                                -------------------------------------
                                |                      |             |
                       +------------------+      +----------+  +----------+
                       | ProtocolDataUnit |      | Velocity |  | EntityID |
                       +------------------+      +----------+  +----------+
                                |
                          ---------------
                          |              |
                    +-------+         +----------+
                    | ESPDU | [...]   | Fire PDU |
                    +-------+         +----------+

 

The top-level class, PduElement, is an abstract class that implements an interface. Each class 
that inherits from it must know how to:

Serialize
Deserialize
Determine its length (as written to the network)
Clone (copy) itself
Print debugging information 

Serialization and deserialization are simply a process of writing the instance variables of the 
class objects to the network in the manner specified by the DIS standard. Elements take up a 
certain amount of space on the network, which may differ from the amount of space the object 
takes up in memory.  An element knows how to make a copy of itself, and can print debugging 
information.

The Velocity and EntityID classes inherit directly from PduElement. PDU classes, however, have 
much behavior that is common, but distinct from things such as the PduHeader; they can benefit 
from an abstract class of their own. As mentioned earlier, each PDU contains PDU header 
information. It makes sense, therefore, to place this information in the ProtocolDataUnit 
abstract class. This prevents having to re-implement this functionality in every instance of 
a PDU.

The concrete PDU classes, such as EntityStatePdu and FirePdu, inherit from the abstract 
ProtocolDataUnit class. Typically these classes are composed of one or more instances of 
PduElements, plus any unique information to that class.

The EntityStatePdu, for example, contains a count of the number of articulation parameters 
contained in the packet and a list of the actual articulation parameters. We implement this 
with a Java class that describes an articulation parameter and a Vector that contains the 
list of articulation class instances. (A Vector is a standard Java class that describes an 
ordered list of objects.) It is important to remember that the object hierarchy is distinct 
from the class hierarchy; the first is a has-a relationship, the second an is-a relationship.

The concrete PDU classes implement the various abstract methods defined in higher level 
abstract classes. The serialization and deserialization methods are at the heart of the 
PduElement classes, and are relatively straightforward. This is an example from the 
EntityStatePdu serialization method:

public void  serialize(DataOutputStream outputStream)
{
super.serialize(outputStream);

try		// catch-all for any exceptions in writing to stream
 {
	entityID.serialize(outputStream);
	forceID.serialize(outputStream);
[...]
	outputStream.writeFloat(entityAccelerationX);
	outputStream.writeFloat(entityAccelerationY);
	outputStream.writeFloat(entityAccelerationZ);
[...]
}
catch(IOException ioException)
{
  throw new 
	RuntimeException("Exception in EntityStatePdu. Error serializing unit.");
}

OutputStream, which is passed into the method, is a standard Java class; it is 
essentially a byte buffer with a number of methods wrapped around it to simplify 
I/O operations. We serialize our instance variables in the same order as that 
specified by the DIS standard. In this case, we first serialize the superclass, 
ProtocolDataUnit, which contains the header information. Then we write each of 
the instance variables to the buffer. Since the operation might fail, Java 
requires the failure exceptions to be handled. We don’t attempt to recover from 
the exception if we do fail, but rather simply terminate the program. If things 
are screwed up that badly, it’s unlikely we’ll be able to fix it. entityID and 
forceID are instance variables actually objects that represent several instance 
variables. They are classes that inherit directly from PduElement, and therefore 
know how to serialize themselves without any assistance from us. They too write 
to the outputStream. EntityAccelerationX, Y, and Z are native Java instance 
variables--floats that represent the entity’s current acceleration. We use the 
outputStream’s methods to write these variables to the stream.

The deSerialize methods work in an analogous fashion.

As mentioned, the Java classes that represent PDU elements have an interface for 
accessing or changing instance variables. This is simply good programming practice; 
I’ve been burned too many times by directly manipulating ivars. So, to change the 
entityID, we have setEntityID and entityID methods, which set and retrieve the 
value for the instance variable called entityID. 

Since entityID is an object, there are a couple interesting aspects to this 
operation. The first is the possibility of an orphaned object. In C++ or Objective-C, 
the following operation might result in a memory leak:

public void setEntityID(EntityID pEnitityID)
{ entityID = pEntityID;}

If there is already an object entityID, this operation will orphan it; there will, 
probably, no longer be any valid pointers to the old version of entityID, and no 
way to free the memory or other resources held by the object. This is not a problem 
in Java. Garbage collection ensures that even if we do orphan an object, the memory 
will be recovered for use by the system.

The other interesting aspect is retrieval of the object. One might assume that this 
is easily done with the following:

public EntityID entityID()
{ return entityID;
}

The problem, again, is that entityID is an object. This will return a pointer (effectively) 
to a single instance of entityID. This means that whomever retrieved the object now has 
access to the internal state of the object that contains the entityID. The caller can 
change the value of entityID, and hence the value of EntityStatePdu, without going through 
the interface to EntityStatePdu--a violation of encapsulation. 

The solution to this is to make a copy of the instance variable the caller requests, and 
return that, as follows:

public EntityID entityID()
{ return (EntityID)entityID.clone();
}

This creates a brand-new object that happens to have the same value as the instance variable, 
then returns it. If the caller modifies the entityID object it will have no effect on the 
EntityStatePDU that supplied it; encapsulation is preserved. The ubiquity of the clone 
operation was the reason it was specified in the abstract PduElement class.

Essentially, the whole of the DIS PDU classes are implemented using these simple concepts. 
Objects know how to serialize and deserialize themselves, and have accessor methods for each 
instance variable. Variable length lists are implemented as vectors that contain objects. 
That’s about it.

(In fact, this so simple that there are some interesting possibilities for making this even more 
general. This can be addressed as part of the "Dial-a-Protocol" effort.) The release notes and 
class listings define the currently implemented PDUs and PDU elements.

So far, there has been no discussion of the actual network code. The PDU classes know how to 
read and write from input and output streams, but how do they actually read from the network?

Java supports TCP/IP sockets, with some caveats. Therefore we can read and write packets to 
the network using the usual techniques. Java has a class for a DatagramPacket. We can 
serialize an instance of a PDU class fairly easily with something like the following:

espdu.serialize(dataOutputStream);
streamArray = outputStream.toByteArray();
dgramPacket = new DatagramPacket(outputStream.toByteArray(), outputStream.size(), dgramDest, 8242);
			
try
{
	dgramSocket.send(dgramPacket);
}
catch(IOException ioException)
{
	[Error handling]
}

A DataOutputStream (remember, essentially a wrapper of methods around a byte array) is used 
by the PDU to serialize its data into a valid PDU packet. We instantiate an instance of a 
datagram packet with the contents of the DataOutputStream, a socket number, and a datagram 
destination. Then we simply send the datagram packet through an instance of a socket.

The caveats for using sockets in Java are small, but fairly serious. Luckily, there are 
workarounds. First of all, the JDK 1.02 specification does not support multicast 
sockets. Official support does not appear until the Java 1.1 spec. There is an unofficial 
sun.net.MulticastSocket class, however, that does work. But not under Intel, where 
there is a byte-ordering bug. But there is a fix to that, too. See 
http://www.cdt.luth.se/~peppar/java/multicastWin32/ for details. The other problem 
is security. Applets running within a browser have certain restrictions on what 
they can do. One of the restrictions is opening a socket to a machine other than 
the machine that served up the applet. The way around this is to use bridges, as laid out
in the "mil.navy.nps.bridge" package.

There are some interesting problems to be solved to read DIS packets from the network. 
First, some background. We need to read from the socket in a reasonably timely manner 
to prevent the loss of packets due to buffer overflow. On the other hand, the program 
making use of the DIS library might require some lengthy, non-determinate amount of time 
to draw to the screen, handle user input, and do other tasks. 

A solution to this is to have a separate thread of execution handling network reads on the 
socket. Threads are essentially lightweight processes; they contain their own program counter 
and stack, but share access to the program’s global memory. Two threads can "simaltaneously" 
work within the same program, each proceeding as if it were the main path of execution. 
Java has language-level support for programming in threads, so this turns out to be fairly 
easy to implement. 

The solution I picked was to closely couple a socket and a thread in a single object, the 
NetworkMonitor. The thread continously reads from the socket unless blocked by lack of input. 
Packets that it reads are buffered up in local storage until the main event loop of the 
program requests them. The NetworkMonitor class turns over all the packets that have been 
read, then starts filling up another buffer.

Eventually the main program will ask for the PDUs. This is done in the receivedPdus() method by 
the main thread, as follows:

localDatagramBuffer = datagramBuffer;
newDatagramBuffer = new Vector(); 
synchronized(localDatagramBuffer)
{
	datagramBuffer = newDatagramBuffer;	
}

A new buffer is created, then access to the existing buffer is locked. This prevents the reader 
thread from adding new PDUs to the buffer while we’re in the process of taking it away. 
(Remember that this operation is being performed by the main thread, not the thread that is reading 
from the socket.) The new, empty buffer is put in its place and the reader thread begins to add 
PDUs to the new buffer.

The datagrams that have been read from the wire are, at this point, basically just byte 
arrays in the DIS format. They haven’t yet been promoted to full objects, such as the 
EntityStatePdu. This process is accomplished by the following code:

datagramEnumeration = localDatagramBuffer.elements();
while(datagramEnumeration.hasMoreElements())
{
  DatagramPacket		aDatagram = (DatagramPacket)datagramEnumeration.nextElement();
  ProtocolDataUnit	aPdu;

  aPdu = ProtocolDataUnit.datagramToPdu(aDatagram);
  if(aPdu != null)
  pduBuffer.addElement(aPdu);
}

Each datagram in the buffer is promoted to a full-fledged object. Then the list of promoted 
PDUs is returned to the caller. The datagramToPdu method is simply calling the deSerialize() 
method on a PDU object of the correct type, and returning a PDU.

As far as the program making use of the PDU library is concerned, it simply periodically asks 
NetworkMonitor instance for PDU objects, and the NetworkMonitor class responds with a collection 
of all the PDUs that have arrived since the last time. There can be more than one NetworkMonitor 
instance, each perhaps listening on a unique unicast socket, or listening on a particular multicast address. 

The rest of the code in the mil.navy.nps.dis package is fairly straightforward. There are a number 
of unsigned number classes--UnsignedInt, UnsignedShort, etc--since Java does not include these as 
native types or as objects. The unsigned numerics know how to serialize and deserialize themselves 
to the network.

DOCUMENTATION

Documentation is available in the "documentation" directory under the dis directory.
This was created by running javadoc on the java source files.

