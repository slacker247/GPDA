package wekavis;

import java.awt.*;
import javax.swing.*;

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
  private JTextField jTextField1 = new JTextField();
  private JTextField jTextField2 = new JTextField();
  private JTextField jTextField3 = new JTextField();
  private JComboBox jComboBox1 = new JComboBox();
  private JComboBox jComboBox2 = new JComboBox();
  private JComboBox jComboBox3 = new JComboBox();
  private JComboBox jComboBox4 = new JComboBox();
  private JComboBox jComboBox5 = new JComboBox();
  private JComboBox jComboBox6 = new JComboBox();
  private JLabel jLabel10 = new JLabel();
  private JLabel jLabel11 = new JLabel();
  private JLabel jLabel12 = new JLabel();
  private JLabel jLabel13 = new JLabel();
  private JLabel jLabel14 = new JLabel();
  private JComboBox jComboBox7 = new JComboBox();
  private JTextField jTextField4 = new JTextField();
  private JTextField jTextField5 = new JTextField();
  private JTextField jTextField6 = new JTextField();
  private JTextField jTextField7 = new JTextField();

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
    panel1.setLayout(null);
    jTabbedPane1.setBounds(new Rectangle(29, 23, 499, 375));
    jConfigRuleTree.setLayout(null);
    jConfigCluster.setLayout(null);
    jCancelBTN.setBounds(new Rectangle(435, 421, 95, 23));
    jCancelBTN.setText("Cancel");
    jOKBTN.setBounds(new Rectangle(325, 422, 95, 23));
    jOKBTN.setText("OK");
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
    jTextField1.setBounds(new Rectangle(213, 56, 127, 17));
    jTextField2.setBounds(new Rectangle(213, 93, 127, 17));
    jTextField3.setBounds(new Rectangle(213, 127, 128, 17));
    jComboBox1.setBounds(new Rectangle(212, 14, 130, 22));
    jComboBox2.setBounds(new Rectangle(212, 161, 130, 22));
    jComboBox3.setBounds(new Rectangle(213, 197, 130, 22));
    jComboBox4.setBounds(new Rectangle(214, 234, 130, 22));
    jComboBox5.setBounds(new Rectangle(216, 270, 130, 22));
    jComboBox6.setBounds(new Rectangle(216, 306, 130, 22));
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
    jComboBox7.setBounds(new Rectangle(210, 39, 130, 22));
    jTextField4.setBounds(new Rectangle(211, 86, 129, 17));
    jTextField5.setBounds(new Rectangle(210, 138, 130, 17));
    jTextField6.setBounds(new Rectangle(209, 192, 132, 17));
    jTextField7.setBounds(new Rectangle(209, 254, 132, 17));
    getContentPane().add(panel1);
    panel1.add(jTabbedPane1, null);
    jConfigRuleTree.add(jLabel3, null);
    jConfigRuleTree.add(jLabel2, null);
    jConfigRuleTree.add(jLabel1, null);
    jConfigRuleTree.add(jLabel4, null);
    jConfigRuleTree.add(jLabel5, null);
    jConfigRuleTree.add(jLabel6, null);
    jConfigRuleTree.add(jLabel7, null);
    jConfigRuleTree.add(jLabel8, null);
    jConfigRuleTree.add(jLabel9, null);
    jConfigRuleTree.add(jTextField1, null);
    jConfigRuleTree.add(jTextField3, null);
    jConfigRuleTree.add(jTextField2, null);
    jConfigRuleTree.add(jComboBox1, null);
    jConfigRuleTree.add(jComboBox2, null);
    jConfigRuleTree.add(jComboBox3, null);
    jConfigRuleTree.add(jComboBox4, null);
    jConfigRuleTree.add(jComboBox5, null);
    jConfigRuleTree.add(jComboBox6, null);
    jTabbedPane1.add(jConfigCluster,   "Cluster");
    jConfigCluster.add(jLabel12, null);
    jConfigCluster.add(jLabel11, null);
    jConfigCluster.add(jLabel10, null);
    jConfigCluster.add(jLabel13, null);
    jConfigCluster.add(jLabel14, null);
    jConfigCluster.add(jComboBox7, null);
    jConfigCluster.add(jTextField4, null);
    jConfigCluster.add(jTextField5, null);
    jConfigCluster.add(jTextField6, null);
    jConfigCluster.add(jTextField7, null);
    jTabbedPane1.add(jConfigRuleTree,  "Rule Tree");
    panel1.add(jCancelBTN, null);
    panel1.add(jOKBTN, null);
  }
}