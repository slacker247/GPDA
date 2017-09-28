/*
 * EmailDispatcher.java
 *
 * Created on November 11, 2003, 8:40 AM
 */

import java.util.*;
import java.net.*;
import java.io.*;
/**
 *
 * @author  s824685
 */
public class EmailDispatcher {
    
  private String IPDestination = "127.0.0.1";
  private String PortDestination = "8127";
    /** Creates a new instance of EmailDispatcher */
    public EmailDispatcher() {
    }
    
    /* This function allows the generation of canned data to be producted then
     *outputed to the GPDA testbed
     */
    protected void noInput()
    {
        long rnd = java.lang.Math.round(java.lang.Math.random());
        String hyp = "";
        
        if(rnd == 0)
            hyp = "Predator";
        else if(rnd == 1)
            hyp = "U2";
        else
            hyp = "Space";
        String belief = "0.67";
        String disbelief = "0.00";
        String time = "?";
        String duration = "0";
        String msg = hyp + " " + belief + " " + disbelief + " " + time + " " + duration;
        String byteStream = "MTIX GPDA " + "TCT_24" + " 105 0 " + msg.length() + " " + msg;
        sendMessage(byteStream);
    }
    
    /* This function takes in an email that was genereated by mtix.  this file
     *has a certain structure to it that will be utilized then converted for 
     *output to the GPDA testbed.
     */
     protected void parseEmail(String filename)
     {
         sendMessage("");
     }
     
    public void sendMessage(String msg)
    {
        String byteStream = msg;
        /** This one sends the data to the test bed via message passing and
        * that the test bed is local
        */
        try
        {
          System.out.println("Sending this to Local Host: " + byteStream.toString());
            InetAddress host = null;
            host = host.getByName(IPDestination);
            int port = Integer.valueOf(PortDestination).intValue();
          System.out.println("Sending to Local Host on : " + host.getHostAddress() + "/" + port);
          System.out.println("DataGram before");
            DatagramSocket outSocket = new DatagramSocket();
            byte buf[] = byteStream.getBytes();
            DatagramPacket datagram = new DatagramPacket(buf, byteStream.length());
            datagram.setPort(port);
            datagram.setAddress(host);
            datagram.setLength(byteStream.length());
            outSocket.send(datagram);
          System.out.println("Data [" + byteStream.toString() + "] sent to " + host.toString());
        }catch(Exception e)
        {
            e.printStackTrace();
        }
    }

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        
        EmailDispatcher ed = new EmailDispatcher();
        
        if(args.length > 0)
        // Switch to except no file for input but still output a result " - "
        if(args[0] == "-")
        {
            ed.noInput();
        }
        else
        {
            // open the file with the associated name
            ed.parseEmail(args[1]);
        }
    }
    
}
