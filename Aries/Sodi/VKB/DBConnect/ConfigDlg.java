package DBConnect;

import java.awt.*;
import javax.swing.*;
import java.awt.event.*;

/**
 * <p>Title: </p>
 * <p>Description: </p>
 * <p>Copyright: Copyright (c) 2002</p>
 * <p>Company: </p>
 * @author unascribed
 * @version 1.0
 */

public class ConfigDlg extends JDialog {
  private JPanel panel1 = new JPanel();
  private JTabbedPane jTabbedPane1 = new JTabbedPane();
  private JPanel jConfigRuleTree = new JPanel();
  private JPanel jConfigCluster = new JPanel();
  private JButton jCancelBTN = new JButton();
  private JButton jOKBTN = new JButton();
  private JTextField jTxtUserName = new JTextField();
  private JTextField jTxtPassword = new JTextField();
  private JTextField jTxtMission = new JTextField();
  private JTextField jTxtDBurl = new JTextField();
  private JTextField jTxtHost = new JTextField();
  private JTextField jTxtDBPort = new JTextField();
  private JTextField jTxtDBDriver = new JTextField();
  private JLabel jLabel15 = new JLabel();
  private JLabel jLabel16 = new JLabel();
  private JLabel jLabel17 = new JLabel();
  private JLabel jLabel18 = new JLabel();
  private JLabel jLabel19 = new JLabel();
  private JLabel jLabel20 = new JLabel();
  private JLabel jLabel21 = new JLabel();
  public boolean okBtn = false;

  public ConfigDlg(Frame frame, String title, boolean modal) {
    super(frame, title, modal);
    try {
      jbInit();
      pack();
    }
    catch(Exception ex) {
      ex.printStackTrace();
    }
  }

  public ConfigDlg() {
    this(null, "Configuration", false);
  }
  private void jbInit() throws Exception {
    ConfigData Config = new ConfigData();
    panel1.setLayout(null);

    jCancelBTN.setBounds(new Rectangle(335, 421, 95, 23));
    jCancelBTN.setText("Cancel");
    jCancelBTN.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        jCancelBTN_actionPerformed(e);
      }
    });

    jOKBTN.setBounds(new Rectangle(225, 422, 95, 23));
    jOKBTN.setText("OK");
    jOKBTN.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        jOKBTN_actionPerformed(e);
      }
    });

    jTxtUserName.setBounds(new Rectangle(274, 80, 206, 22));
    jTxtPassword.setBounds(new Rectangle(274, 120, 206, 22));
    jTxtMission.setBounds(new Rectangle(274, 160, 206, 22));
    jTxtDBurl.setBounds(new Rectangle(274, 200, 206, 22));
    jTxtHost.setBounds(new Rectangle(274, 240, 206, 22));
    jTxtDBPort.setBounds(new Rectangle(274, 280, 206, 22));
    jTxtDBDriver.setBounds(new Rectangle(274, 320, 206, 22));
    jLabel15.setText("User Name:");
    jLabel15.setBounds(new Rectangle(187, 83, 82, 13));
    jLabel16.setText("Password:");
    jLabel16.setBounds(new Rectangle(197, 124, 74, 13));
    jLabel17.setText("Mission:");
    jLabel17.setBounds(new Rectangle(207, 163, 61, 13));
    jLabel18.setText("DataBase Url:");
    jLabel18.setBounds(new Rectangle(173, 204, 96, 13));
    jLabel19.setText("DataBase Port:");
    jLabel19.setBounds(new Rectangle(165, 284, 105, 13));
    jLabel20.setText("DataBase Driver:");
    jLabel20.setBounds(new Rectangle(154, 324, 117, 13));
    jLabel21.setText("host:");
    jLabel21.setBounds(new Rectangle(232, 244, 36, 13));
    getContentPane().add(panel1);
    panel1.add(jTxtDBDriver, null);
    panel1.add(jTxtDBPort, null);
    panel1.add(jTxtHost, null);
    panel1.add(jTxtDBurl, null);
    panel1.add(jTxtMission, null);
    panel1.add(jTxtPassword, null);
    panel1.add(jTxtUserName, null);
    panel1.add(jLabel20, null);
    panel1.add(jLabel19, null);
    panel1.add(jLabel21, null);
    panel1.add(jLabel18, null);
    panel1.add(jLabel17, null);
    panel1.add(jLabel16, null);
    panel1.add(jLabel15, null);
    panel1.add(jCancelBTN, null);
    panel1.add(jOKBTN, null);
    jTxtDBDriver.setText(Config.getDB_Driver());
    jTxtDBPort.setText(Config.getPort());
    jTxtHost.setText(Config.getHost());
    jTxtDBurl.setText(Config.getDB_Url());
    jTxtMission.setText(Config.getMission());
    jTxtPassword.setText(Config.getPassword());
    jTxtUserName.setText(Config.getUser());
  }

  void jOKBTN_actionPerformed(ActionEvent e)
  {
    ConfigData Config = new ConfigData();
    okBtn = true;
    try
    {
      Config.updateValues(jTxtUserName.getText(), jTxtPassword.getText(), jTxtHost.getText(),
                        jTxtDBPort.getText(), jTxtDBurl.getText(), jTxtDBDriver.getText(), jTxtMission.getText());
    }catch(Exception ioe)
    {
      okBtn = false;
      ioe.printStackTrace();
    }
    try
    {
       Config.writeConfigFile();
    }catch(Exception eea)
    {
        okBtn = false;
        eea.printStackTrace();
    }
    this.setVisible(false);
  }

  void jCancelBTN_actionPerformed(ActionEvent e)
  {
    this.setVisible(false);
  }
}