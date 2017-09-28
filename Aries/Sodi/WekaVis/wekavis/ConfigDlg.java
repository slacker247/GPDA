package wekavis;

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
  private JLabel jLabel1 = new JLabel();
  private JLabel jLabel2 = new JLabel();
  private JLabel jLabel3 = new JLabel();
  private JLabel jLabel4 = new JLabel();
  private JLabel jLabel5 = new JLabel();
  private JLabel jLabel6 = new JLabel();
  private JLabel jLabel7 = new JLabel();
  private JLabel jLabel8 = new JLabel();
  private JLabel jLabel9 = new JLabel();
  private JTextField jTxt_ConfFactor_RuleTree = new JTextField();
  private JTextField jTxt_MinNumObj_RuleTree = new JTextField();
  private JTextField jTxt_NumFolds_RuleTree = new JTextField();
  private JComboBox jCB_BinarSplits_RuleTree = new JComboBox();
  private JComboBox jCB_ReduceErrPurning_RuleTree = new JComboBox();
  private JComboBox jCB_SaveInstData_RuleTree = new JComboBox();
  private JComboBox jCB_SubTreeRaising_RuleTree = new JComboBox();
  private JComboBox jCB_Unpruned_RuleTree = new JComboBox();
  private JComboBox jCB_UseLaplace_RuleTree = new JComboBox();
  private JLabel jLabel10 = new JLabel();
  private JLabel jLabel11 = new JLabel();
  private JLabel jLabel12 = new JLabel();
  private JLabel jLabel13 = new JLabel();
  private JLabel jLabel14 = new JLabel();
  private JComboBox jCB_Debug_Cluster = new JComboBox();
  private JTextField jTxt_MaxItr_Cluster = new JTextField();
  private JTextField jTxt_MinSdev_Cluster = new JTextField();
  private JTextField jTxt_Num_of_Cluster = new JTextField();
  private JTextField jTxt_Seed_Cluster = new JTextField();
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
    jTabbedPane1.setBounds(new Rectangle(26, 438, 499, 375));
    jConfigRuleTree.setLayout(null);
    jConfigCluster.setLayout(null);
    jCancelBTN.setBounds(new Rectangle(435, 421, 95, 23));
    jCancelBTN.setText("Cancel");
    jCancelBTN.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        jCancelBTN_actionPerformed(e);
      }
    });
    jOKBTN.setBounds(new Rectangle(325, 422, 95, 23));
    jOKBTN.setText("OK");
    jOKBTN.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        jOKBTN_actionPerformed(e);
      }
    });
    jLabel1.setText("Binary Splits");
    jLabel1.setBounds(new Rectangle(113, 17, 96, 13));
    jLabel2.setText("Confidence Factor");
    jLabel2.setBounds(new Rectangle(79, 59, 132, 13));
    jLabel3.setText("Minimum Number of Objects");
    jLabel3.setBounds(new Rectangle(4, 97, 208, 15));
    jLabel4.setText("Number of Folds");
    jLabel4.setBounds(new Rectangle(88, 130, 123, 13));
    jLabel5.setText("Reduce Error Pruning");
    jLabel5.setBounds(new Rectangle(60, 168, 154, 13));
    jLabel6.setText("Save Instance Data");
    jLabel6.setBounds(new Rectangle(75, 204, 138, 13));
    jLabel7.setText("Sub Tree Raising");
    jLabel7.setBounds(new Rectangle(93, 240, 125, 13));
    jLabel8.setText("Unpruned");
    jLabel8.setBounds(new Rectangle(144, 277, 69, 13));
    jLabel9.setText("Use Laplace");
    jLabel9.setBounds(new Rectangle(127, 313, 87, 13));
    jTxt_ConfFactor_RuleTree.setBounds(new Rectangle(213, 56, 127, 17));
    jTxt_MinNumObj_RuleTree.setBounds(new Rectangle(213, 93, 127, 17));
    jTxt_NumFolds_RuleTree.setBounds(new Rectangle(213, 127, 128, 17));
    jCB_BinarSplits_RuleTree.setBounds(new Rectangle(212, 14, 130, 22));
    jCB_ReduceErrPurning_RuleTree.setBounds(new Rectangle(212, 161, 130, 22));
    jCB_SaveInstData_RuleTree.setBounds(new Rectangle(213, 197, 130, 22));
    jCB_SubTreeRaising_RuleTree.setBounds(new Rectangle(214, 234, 130, 22));
    jCB_Unpruned_RuleTree.setBounds(new Rectangle(216, 270, 130, 22));
    jCB_UseLaplace_RuleTree.setBounds(new Rectangle(216, 306, 130, 22));
    jLabel10.setText("Debug");
    jLabel10.setBounds(new Rectangle(156, 46, 47, 13));
    jLabel11.setText("Max Iterations");
    jLabel11.setBounds(new Rectangle(99, 91, 106, 13));
    jLabel12.setText("Min Standard Deviation");
    jLabel12.setBounds(new Rectangle(34, 141, 169, 13));
    jLabel13.setText("Number of Clusters");
    jLabel13.setBounds(new Rectangle(66, 196, 143, 13));
    jLabel14.setText("Seed");
    jLabel14.setBounds(new Rectangle(171, 258, 36, 13));
    jCB_Debug_Cluster.setBounds(new Rectangle(210, 39, 130, 22));
    jTxt_MaxItr_Cluster.setBounds(new Rectangle(211, 86, 129, 17));
    jTxt_MinSdev_Cluster.setBounds(new Rectangle(210, 138, 130, 17));
    jTxt_Num_of_Cluster.setBounds(new Rectangle(209, 192, 132, 17));
    jTxt_Seed_Cluster.setBounds(new Rectangle(209, 254, 132, 17));
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
    jConfigRuleTree.add(jLabel3, null);
    jConfigRuleTree.add(jLabel2, null);
    jConfigRuleTree.add(jLabel1, null);
    jConfigRuleTree.add(jLabel4, null);
    jConfigRuleTree.add(jLabel5, null);
    jConfigRuleTree.add(jLabel6, null);
    jConfigRuleTree.add(jLabel7, null);
    jConfigRuleTree.add(jLabel8, null);
    jConfigRuleTree.add(jLabel9, null);
    jConfigRuleTree.add(jTxt_ConfFactor_RuleTree, null);
    jConfigRuleTree.add(jTxt_NumFolds_RuleTree, null);
    jConfigRuleTree.add(jTxt_MinNumObj_RuleTree, null);
    jConfigRuleTree.add(jCB_BinarSplits_RuleTree, null);
    jConfigRuleTree.add(jCB_ReduceErrPurning_RuleTree, null);
    jConfigRuleTree.add(jCB_SaveInstData_RuleTree, null);
    jConfigRuleTree.add(jCB_SubTreeRaising_RuleTree, null);
    jConfigRuleTree.add(jCB_Unpruned_RuleTree, null);
    jConfigRuleTree.add(jCB_UseLaplace_RuleTree, null);
    panel1.add(jTxtDBDriver, null);
    panel1.add(jTxtDBPort, null);
    panel1.add(jTxtHost, null);
    panel1.add(jTxtDBurl, null);
    panel1.add(jTxtMission, null);
    panel1.add(jTxtPassword, null);
    panel1.add(jTxtUserName, null);
    panel1.add(jLabel20, null);
    jConfigCluster.add(jLabel12, null);
    jConfigCluster.add(jLabel11, null);
    jConfigCluster.add(jLabel10, null);
    jConfigCluster.add(jLabel13, null);
    jConfigCluster.add(jLabel14, null);
    jConfigCluster.add(jCB_Debug_Cluster, null);
    jConfigCluster.add(jTxt_MaxItr_Cluster, null);
    jConfigCluster.add(jTxt_MinSdev_Cluster, null);
    jConfigCluster.add(jTxt_Num_of_Cluster, null);
    jConfigCluster.add(jTxt_Seed_Cluster, null);
    panel1.add(jLabel19, null);
    panel1.add(jLabel21, null);
    panel1.add(jLabel18, null);
    panel1.add(jLabel17, null);
    panel1.add(jLabel16, null);
    panel1.add(jLabel15, null);
    jTabbedPane1.add(jConfigRuleTree, "Rule Tree");
    jTabbedPane1.add(jConfigCluster, "Clusterer");
    panel1.add(jCancelBTN, null);
    panel1.add(jOKBTN, null);
    jTabbedPane1.setVisible(false);
    jTxtDBDriver.setText(Config.getDBDriver());
    jTxtDBPort.setText(Config.getPort());
    jTxtHost.setText(Config.getHost());
    jTxtDBurl.setText(Config.getDBURL());
    jTxtMission.setText(Config.getMission());
    jTxtPassword.setText(Config.getPassword());
    jTxtUserName.setText(Config.getUsername());
  }

  void jOKBTN_actionPerformed(ActionEvent e)
  {
    ConfigData Config = new ConfigData();
    try
    {
      Config.updateValues(jTxtUserName.getText(), jTxtPassword.getText(), jTxtHost.getText(),
                        jTxtDBPort.getText(), jTxtDBurl.getText(), jTxtDBDriver.getText(), jTxtMission.getText());
    }catch(Exception ioe)
    {
      ioe.printStackTrace();
    }
    Config.saveFile();
    this.setVisible(false);
  }

  void jCancelBTN_actionPerformed(ActionEvent e)
  {
    this.setVisible(false);
  }
}