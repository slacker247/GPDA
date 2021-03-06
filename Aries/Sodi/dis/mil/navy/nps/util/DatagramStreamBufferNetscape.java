/**
  DatagramStreamBufferNetscape

  Subclass of DatagramStreamBuffer, with additions for Netscape security.

  Essentially, all this does is add Netscape security capabilities to
  DatagramStreamBuffer. It does this by overriding certain critical
  methods; the technique is to simply make calls to the Netscape security
  classes that enable wider access, then call the superclass to perform
  the actual network operations.

  Other subclasses of DatagramStreamBuffer could implement security for
  MSIE, etc.

  @@@THIS IS NETSCAPE COMMUNICATOR SPECIFIC.  It basically
 switches on network access for everything under the sun,
 so you can attach or be attached to random machines on
 the internet. enablePrivilege() turns off the priv when
 the method it's called from exits.

  You need the netscape.security package, which can be gotten
  from netscape at 
  <A HREF="http://developer.netscape.com/products/zigbert/index.html">
  http://developer.netscape.com/products/zigbert/index.html</A>
  
  Also, you'll need to either 1) sign the applet with a digital
  certificate, or, more likely, 2) change your preferences in
  Netscape to allow sleazy security. this is called activating
  codebase principals. See 
  http://developer.netscape.com/products/zigbert/index.html
  */

package mil.navy.nps.util;

import java.net.*;
import java.util.*;

//import netscape.security.*;     // to enable netscape to read from random datagram sockets


public class DatagramStreamBufferNetscape extends DatagramStreamBuffer
{

  public static final boolean DEBUG = true;

//  PrivilegeManager privilegeManager;

  /**
  Unicast constructor just calls correct superclass constructor
  */

public DatagramStreamBufferNetscape(int pDatagramPort)            // INPUT: port we read from
{
  super(pDatagramPort);
  
//    privilegeManager = PrivilegeManager.getPrivilegeManager();

//    privilegeManager.enablePrivilege("UniversalMulticast");
//    privilegeManager.enablePrivilege("UniversalConnect");
//    privilegeManager.enablePrivilege("UniversalAccept");
//    privilegeManager.enablePrivilege("UniversalListen");
}

/**
Create a new unicast DSB on an ephemeral port, once picked
by the system.
*/

public DatagramStreamBufferNetscape()
{
  super();
  
//    privilegeManager = PrivilegeManager.getPrivilegeManager();

//    privilegeManager.enablePrivilege("UniversalMulticast");
//    privilegeManager.enablePrivilege("UniversalConnect");
//    privilegeManager.enablePrivilege("UniversalAccept");
//    privilegeManager.enablePrivilege("UniversalListen");
}

/**
Multicast constructor. Enables network access and calls the appropriate
superclass constructor. Ugly wart: due to Java requirements, the call
to super() has to be made before anything else, including security calls.
This results in a failure to join the socket in the superclass. We catch
that exception and re-try it in this constructor, after we've made the
appropriate security calls. If we fail here, too, we hard-fail.
*/

public DatagramStreamBufferNetscape(String pMulticastAddress, int pDatagramPort)
{
  super(pMulticastAddress, pDatagramPort);

  // This security stuff had to be moved into the constructor, since doing the join() requires
  // privliges.

 
//    privilegeManager = PrivilegeManager.getPrivilegeManager();

//    privilegeManager.enablePrivilege("UniversalMulticast");
//    privilegeManager.enablePrivilege("UniversalConnect");
//    privilegeManager.enablePrivilege("UniversalAccept");
//    privilegeManager.enablePrivilege("UniversalListen");

    if(DEBUG)
      this.dumpProperties();

    try
    {
        debug ("trying to join Address " + pMulticastAddress + " in sublcass");
        multicastSocket.joinGroup(InetAddress.getByName(pMulticastAddress));
        debug ("joined multicast address " + pMulticastAddress);
    }
    catch (Exception socketException)
     {
        System.out.println("Unable to join multicast address "  + pMulticastAddress);
        System.out.println("This is serious. You need to find out what's going wrong in DatagramStreamBufferNetscape");
        throw new RuntimeException("DatagramStreamBufferNetscape security problem on join");
     }

}

/**
 * dumpProperties prints out the properties of the JVM and system this
 * code is running on.
 */

public void dumpProperties()
{
 
//  PrivilegeManager privilegeManager = PrivilegeManager.getPrivilegeManager();

//  privilegeManager.enablePrivilege("UniversalPropertyWrite");
  System.out.println("Java VM properties:");
  System.out.println("---------------------------");
  Properties systemProps = System.getProperties();
  systemProps.list(System.out);
  System.out.println("---------------------------");
}


/**
The run method. Enables network access, then calls the superclass
for the actual implementation.
*/

public void run()
{
//    privilegeManager = PrivilegeManager.getPrivilegeManager();

//    privilegeManager.enablePrivilege("UniversalMulticast");
//    privilegeManager.enablePrivilege("UniversalConnect");
//    privilegeManager.enablePrivilege("UniversalAccept");
//    privilegeManager.enablePrivilege("UniversalListen");

    super.run();
}

/**
Enables security, then calls the superclass for the actual
send operation.
*/

public synchronized void sendDatagram(DatagramPacket pDatagram)
{

//    privilegeManager = PrivilegeManager.getPrivilegeManager();
//    privilegeManager.enablePrivilege("UniversalMulticast");
//    privilegeManager.enablePrivilege("UniversalConnect");
//    privilegeManager.enablePrivilege("UniversalAccept");
//    privilegeManager.enablePrivilege("UniversalListen");

    super.sendDatagram(pDatagram);
}


}
