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

public class ClientConnectionForm extends JFrame {

private JTextField host;
private JTextField port;
private JTextField serverName;
private JTextField clientName;

  static final int CANCEL=0;
  static final int ACCEPT=1;
  
   // Login Dialog
  private   String userName;
  protected String userPassword;
  public JDialog jd;
  protected Frame parent;
  private ActionListener dAction;
  private boolean aborted=false;

public ClientConnectionForm(Frame fr, 
		       String hn, 
		       String pt, 
		       String sn, 
		       String cn) {

    jd = new JDialog(fr, "Login", true);
    parent = fr;

    JPanel jp = new JPanel();
    //    jp.setLayout(new BoxLayout(jp, BoxLayout.Y_AXIS));
    GridBagLayout gbl = new GridBagLayout();
    GridBagConstraints gbc;
    jp.setLayout(gbl);
    // jp.setLayout(new GridLayout(4,2));
    jp.setBorder(new TitledBorder("Connection Information"));
    JPanel jp1 = new JPanel(new FlowLayout());

    dAction = new ActionListener() {
      public void actionPerformed(ActionEvent e) { doAction(ACCEPT); }
    };    
    
    JLabel hostl = new JLabel("Host:");
    host = new JTextField(hn,15);
    host.addActionListener(dAction);
    host.addFocusListener(new FocusAdapter() {
      public void focusGained(FocusEvent e) { 
        host.selectAll();
      }
    });
    hostl.setLabelFor(host);
    hostl.setDisplayedMnemonic('h');
    addLabelAndField(0, hostl, host, gbl, jp);
    //jp.add(hostl); jp.add(host);

    JLabel portl = new JLabel("Port:");
    port = new JTextField(pt,15);
    port.addActionListener(dAction);
    port.addFocusListener(new FocusAdapter() {
      public void focusGained(FocusEvent e) { 
        port.selectAll();
      }
    });
    portl.setLabelFor(port);
    portl.setDisplayedMnemonic('p');
    addLabelAndField(1, portl, port, gbl, jp);
    //jp.add(portl); jp.add(port);

    JLabel serverNamel = new JLabel("Server Name:");
    serverName = new JTextField(sn,15);
    serverName.addActionListener(dAction);
    serverName.addFocusListener(new FocusAdapter() {
      public void focusGained(FocusEvent e) { 
        serverName.selectAll();
      }
    });
    serverNamel.setLabelFor(serverName);
    serverNamel.setDisplayedMnemonic('s');
    addLabelAndField(2, serverNamel, serverName, gbl, jp);
    //jp.add(serverNamel); jp.add(serverName);

    JLabel clientNamel = new JLabel("Client Name:");
    clientName = new JTextField(cn,15);
    clientName.addActionListener(dAction);
    clientName.addFocusListener(new FocusAdapter() {
      public void focusGained(FocusEvent e) { 
        clientName.selectAll();
      }
    });
    clientNamel.setLabelFor(clientName);
    clientNamel.setDisplayedMnemonic('c');
    addLabelAndField(3, clientNamel, clientName, gbl, jp);
    //jp.add(clientNamel); jp.add(clientName);
    
    JButton jok = new JButton("Ok");
    jok.setMnemonic('o');
    jok.addActionListener(dAction);
    jp1.add(jok);

    JButton jcancel = new JButton("CANCEL");
    jcancel.setMnemonic('a');
    jcancel.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) { doAction(CANCEL); }
    });
    jp1.add(jcancel);

    jd.getContentPane().add(jp, BorderLayout.CENTER);
    jd.getContentPane().add(jp1, BorderLayout.SOUTH);
    jd.pack();
    jd.setLocationRelativeTo(parent);
  }

  public void show() {
    jd.show();
  }

public boolean aborted()      { return aborted; }
public String getHostName() { return host.getText(); }
public String getPort() {return port.getText(); }
public String getServerName() { return serverName.getText(); }
public String getClientName() {return clientName.getText(); }

public void doAction(int action) {
  switch (action) {
  case ACCEPT:
    aborted = false;
    break;
  case CANCEL:
    aborted = true;
    break;
  }
  jd.setVisible(false);
}

private void addLabelAndField(int i, 
			      JLabel l, 
			      JTextField f, 
			      GridBagLayout gbl, 
			      Container c) {
  GridBagConstraints gbc;
  // do the stuff for the label;
  gbc = new GridBagConstraints();
  gbc.weightx = 0;
  gbc.weighty = 100;
  gbc.gridx = 0;
  gbc.gridy = i;
  gbc.gridwidth = 1;
  gbc.gridheight = 1;
  gbc.anchor = GridBagConstraints.NORTHEAST;
  gbl.setConstraints(l, gbc);
  c.add(l);
  // do the stuff for the field;
  gbc = new GridBagConstraints();
  gbc.weightx = 100;
  gbc.weighty = 100;
  gbc.gridx = 1;
  gbc.gridy = i;
  gbc.gridwidth = 3;
  gbc.gridheight = 1;
  //      gbc.anchor = GridBagConstraints.NORTHWEST;
  gbc.fill = GridBagConstraints.BOTH;
  gbl.setConstraints(f, gbc);
  c.add(f);
}
}
