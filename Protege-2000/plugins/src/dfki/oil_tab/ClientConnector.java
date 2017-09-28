package dfki.protege.oil_tab;

import java.util.*;
import java.io.*;
import java.awt.*;
import java.awt.event.*;
import java.applet.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;
import javax.swing.border.*;
import java.net.*;
import lispwrapper.*;
import org.omg.CORBA.*;
import org.omg.CosNaming.*;
import img.fact.sexpr.*;
import img.fact.*;

public class ClientConnector {

private String host;
private String port;
private String serverName;
private String clientName;

public ClientConnector(String hn, 
		       String pt,
		       String sn,
		       String cn) {
  host = hn;
  port = pt;
  serverName = sn;
  clientName = cn;
}

public void setClientName(String cn) {
  clientName = cn;
}

public String clientName() {
  return clientName;
}

public ClientHandler connect(JFrame parent) {

  ClientConnectionForm ccf;
  
  ccf = new ClientConnectionForm(parent, host, port, serverName, clientName);
  ccf.show();
  
  if (!ccf.aborted()) {
    if (ccf.getHostName()!=null) {host = ccf.getHostName();}
    if (ccf.getPort()!=null) {port = ccf.getPort();}
    if (ccf.getServerName()!=null) {serverName = ccf.getServerName();}
    if (ccf.getClientName()!=null) {clientName = ccf.getClientName();}
    return connect();
  }
  return null;
}

public ClientHandler connect() {    
  ORB orb;
  Classifier cl;
  ClientHandler ch = null;

  org.omg.CORBA.Object obj = null;
  
//   Properties props = new Properties();
//   props.setProperty("org.omg.CORBA.ORBInitialHost", host);
//   props.setProperty("org.omg.CORBA.ORBInitialPort", port);
//   orb = ORB.init(new String[0], props);
  orb = ORB.init(new String[0], new Properties());

  try {
    // Try and get the object from the naming service
    NamingContext nc = null;
    int portN;
    try {
      portN = Integer.parseInt(port);
    } catch (NumberFormatException e) {
      portN = -1;
    }
    URL ncURL = new URL("http", host, portN, "/CosNaming");
    BufferedReader bf = 
      new BufferedReader(new InputStreamReader(ncURL.openStream()));
    String ncIOR = bf.readLine();
    obj = orb.string_to_object(ncIOR);
    nc = 
      NamingContextHelper.narrow(obj);
    
    //     NamingContext nc = 
    //     NamingContextHelper.narrow(orb.resolve_initial_references("NameService"));
    NameComponent nc1 = new NameComponent(serverName, "text");
    NameComponent[] name1 = {nc1};
    obj = nc.resolve(name1);
  } catch (Exception e) {
    System.err.println("Name Resolve Failed!!");
    System.err.println("Server Name: " + serverName);
    System.err.println("Host: " + host);
    System.err.println("Port: " + port);
    System.err.println(e);
    return null;
  }
  if(obj == null)
    throw new RuntimeException();
  cl = ClassifierHelper.narrow(obj);
  if(cl == null)
    throw new RuntimeException();
  try {
    String myHost;
    try {
      myHost = java.net.InetAddress.getLocalHost().getHostName();
    } catch (java.net.UnknownHostException ex) {
      myHost = "unknown";
    }
    ch = cl.newHandler(clientName + "@" + myHost); 
  } catch (CommunicationException ex) {
    System.err.print("ERROR: " + ex.code + " ");
    System.err.println(ex.information);
    System.exit(1);
  }
  return ch;
}
}
