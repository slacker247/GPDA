/*
 * RQ_Form.java
 *
 * Created on February 28, 2003, 2:46 PM
 */

package gui;

import java.awt.*;
import javax.swing.*;
import java.awt.event.*;

/**
 *
 * @author  jeffmac
 */
public class RQ_Form extends JDialog {

    private JPanel panel1 = new JPanel();
    private javax.swing.JLabel jL_Field2;
    private javax.swing.JLabel jL_Field3;
    private javax.swing.JLabel jL_Field4;
    private javax.swing.JLabel jL_Field5;
    private javax.swing.JLabel jL_Field6;
    private javax.swing.JLabel jL_Field7;
    private javax.swing.JLabel jL_Field8;
    private javax.swing.JLabel jL_Field9;
    private javax.swing.JLabel jL_Field10;
    private javax.swing.JLabel jL_Field11;
    private javax.swing.JLabel jL_Field12;
    private javax.swing.JLabel jL_Field13;
    private javax.swing.JLabel jL_Field14;
    private javax.swing.JLabel jL_Field15;
    private javax.swing.JLabel jL_Field16;
    private javax.swing.JLabel jL_Field17;
    private javax.swing.JLabel jL_Field18;
    private javax.swing.JLabel jL_Field19;
    private javax.swing.JLabel jL_Field20;
    private javax.swing.JLabel jL_Field21;
    private javax.swing.JLabel jL_Field22;
    private javax.swing.JLabel jL_Field23;
    private javax.swing.JLabel jL_Field24;
    private javax.swing.JLabel jL_Field25;
    private javax.swing.JLabel jL_Field26;
    private javax.swing.JLabel jL_Field27;
    private javax.swing.JLabel jL_Field28;
    private javax.swing.JLabel jL_Field29;
    private javax.swing.JLabel jL_Field30;
    private javax.swing.JLabel jL_Field31;
    private javax.swing.JLabel jL_Field32;
    private javax.swing.JLabel jL_Field33;
    private javax.swing.JLabel jL_Field34;
    private javax.swing.JLabel jL_Field35;
    private javax.swing.JLabel jL_Field36;
    private javax.swing.JLabel jL_Field37;
    private javax.swing.JLabel jL_Field38;
    private javax.swing.JLabel jL_Field39;
    private javax.swing.JLabel jL_Field40;
    private javax.swing.JLabel jL_Field41;
    private javax.swing.JLabel jL_Field42;
    private javax.swing.JLabel jL_Field43;
    private javax.swing.JLabel jL_Field44;
    private javax.swing.JLabel jL_Field45;
    private javax.swing.JLabel jL_Field46;
    private javax.swing.JLabel jL_Field47;
    private javax.swing.JLabel jL_Field48;
    private javax.swing.JLabel jL_Field49;
    private javax.swing.JLabel jL_Field50;
    private javax.swing.JLabel jL_Field51;
    private javax.swing.JLabel jL_Field52;
    private javax.swing.JTextField jT_Field2;
    private javax.swing.JTextField jT_Field3;
    private javax.swing.JTextField jT_Field4;
    private javax.swing.JTextField jT_Field5;
    private javax.swing.JTextField jT_Field6;
    private javax.swing.JTextField jT_Field7;
    private javax.swing.JTextField jT_Field8;
    private javax.swing.JTextField jT_Field9;
    private javax.swing.JTextField jT_Field10;
    private javax.swing.JTextField jT_Field11;
    private javax.swing.JTextField jT_Field12;
    private javax.swing.JTextField jT_Field13;
    private javax.swing.JTextField jT_Field14;
    private javax.swing.JTextField jT_Field15;
    private javax.swing.JTextField jT_Field16;
    private javax.swing.JTextField jT_Field17;
    private javax.swing.JTextField jT_Field18;
    private javax.swing.JTextField jT_Field19;
    private javax.swing.JTextField jT_Field20;
    private javax.swing.JTextField jT_Field21;
    private javax.swing.JTextField jT_Field22;
    private javax.swing.JTextField jT_Field23;
    private javax.swing.JTextField jT_Field24;
    private javax.swing.JTextField jT_Field25;
    private javax.swing.JTextField jT_Field26;
    private javax.swing.JTextField jT_Field27;
    private javax.swing.JTextField jT_Field28;
    private javax.swing.JTextField jT_Field29;
    private javax.swing.JTextField jT_Field30;
    private javax.swing.JTextField jT_Field31;
    private javax.swing.JTextField jT_Field32;
    private javax.swing.JTextField jT_Field33;
    private javax.swing.JTextField jT_Field34;
    private javax.swing.JTextField jT_Field35;
    private javax.swing.JTextField jT_Field36;
    private javax.swing.JTextField jT_Field37;
    private javax.swing.JTextField jT_Field38;
    private javax.swing.JTextField jT_Field39;
    private javax.swing.JTextField jT_Field40;
    private javax.swing.JTextField jT_Field41;
    private javax.swing.JTextField jT_Field42;
    private javax.swing.JTextField jT_Field43;
    private javax.swing.JTextField jT_Field44;
    private javax.swing.JTextField jT_Field45;
    private javax.swing.JTextField jT_Field46;
    private javax.swing.JTextField jT_Field47;
    private javax.swing.JTextField jT_Field48;
    private javax.swing.JTextField jT_Field49;
    private javax.swing.JTextField jT_Field50;
    private javax.swing.JTextField jT_Field51;
    private javax.swing.JTextField jT_Field52;
    private javax.swing.JTextField jT_Field53;
    private javax.swing.JLabel jL_Heading1;
    private javax.swing.JLabel jL_Heading2;
    private javax.swing.JLabel jL_Heading3;
    private javax.swing.JLabel jL_Heading4;
    private javax.swing.JLabel jL_Heading5;
    private javax.swing.JLabel jL_Heading6;
    private javax.swing.JLabel jL_Heading7;
    private javax.swing.JLabel jL_Field53;
    private javax.swing.JSeparator jSeparator2;
    private javax.swing.JSeparator jSeparator3;
    private javax.swing.JSeparator jSeparator31;
    private javax.swing.JSeparator jSeparator32;
    private javax.swing.JSeparator jSeparator33;
    private javax.swing.JSeparator jSeparator34;
    private javax.swing.JSeparator jSeparator35;
    private javax.swing.JSeparator jSeparator36;
    private javax.swing.JSeparator jSeparator321;
    private javax.swing.JSeparator jSeparator322;
    private javax.swing.JSeparator jSeparator323;
    private javax.swing.JSeparator jSeparator324;
    public javax.swing.JButton jBTN_OK_RQ_Form;
    public javax.swing.JButton jBTN_Cancel_RQ_Form;
    private javax.swing.ButtonGroup buttonGroup1;

    /** Creates a new instance of RQ_Form */
    public RQ_Form(Frame frame, String title, boolean modal) {
       super(frame, title, modal);
       try
       {
          jbInit();
          pack();
       }catch(Exception ex)
       {
          ex.printStackTrace();
       }
       this.setSize(820, 770);
       //Center the window
       Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
       Dimension frameSize = this.getSize();
       if (frameSize.height > screenSize.height) {
         frameSize.height = screenSize.height;
       }
       if (frameSize.width > screenSize.width) {
         frameSize.width = screenSize.width;
       }
       this.setLocation((screenSize.width - frameSize.width) / 2, (screenSize.height - frameSize.height) / 2);
    }

    public RQ_Form() {
       this(null, "Reporter's Questions Form", false);
    }

    private void jbInit() throws Exception {
      jL_Field2 = new javax.swing.JLabel();
      jL_Field3 = new javax.swing.JLabel();
      jL_Field4 = new javax.swing.JLabel();
      jL_Field5 = new javax.swing.JLabel();
      jL_Field6 = new javax.swing.JLabel();
      jL_Field7 = new javax.swing.JLabel();
      jL_Field8 = new javax.swing.JLabel();
      jL_Field9 = new javax.swing.JLabel();
      jL_Field10 = new javax.swing.JLabel();
      jL_Field11 = new javax.swing.JLabel();
      jL_Field12 = new javax.swing.JLabel();
      jL_Field13 = new javax.swing.JLabel();
      jL_Field14 = new javax.swing.JLabel();
      jL_Field15 = new javax.swing.JLabel();
      jL_Field16 = new javax.swing.JLabel();
      jL_Field17 = new javax.swing.JLabel();
      jL_Field18 = new javax.swing.JLabel();
      jL_Field19 = new javax.swing.JLabel();
      jL_Field20 = new javax.swing.JLabel();
      jL_Field21 = new javax.swing.JLabel();
      jL_Field22 = new javax.swing.JLabel();
      jL_Field23 = new javax.swing.JLabel();
      jL_Field24 = new javax.swing.JLabel();
      jL_Field25 = new javax.swing.JLabel();
      jL_Field26 = new javax.swing.JLabel();
      jL_Field27 = new javax.swing.JLabel();
      jL_Field28 = new javax.swing.JLabel();
      jL_Field29 = new javax.swing.JLabel();
      jL_Field30 = new javax.swing.JLabel();
      jL_Field31 = new javax.swing.JLabel();
      jL_Field32 = new javax.swing.JLabel();
      jL_Field33 = new javax.swing.JLabel();
      jL_Field34 = new javax.swing.JLabel();
      jL_Field35 = new javax.swing.JLabel();
      jL_Field36 = new javax.swing.JLabel();
      jL_Field37 = new javax.swing.JLabel();
      jL_Field38 = new javax.swing.JLabel();
      jL_Field39 = new javax.swing.JLabel();
      jL_Field40 = new javax.swing.JLabel();
      jL_Field41 = new javax.swing.JLabel();
      jL_Field42 = new javax.swing.JLabel();
      jL_Field43 = new javax.swing.JLabel();
      jL_Field44 = new javax.swing.JLabel();
      jL_Field45 = new javax.swing.JLabel();
      jL_Field46 = new javax.swing.JLabel();
      jL_Field47 = new javax.swing.JLabel();
      jL_Field48 = new javax.swing.JLabel();
      jL_Field49 = new javax.swing.JLabel();
      jL_Field50 = new javax.swing.JLabel();
      jL_Field51 = new javax.swing.JLabel();
      jL_Field52 = new javax.swing.JLabel();
      jT_Field2 = new javax.swing.JTextField();
      jT_Field3 = new javax.swing.JTextField();
      jT_Field4 = new javax.swing.JTextField();
      jT_Field5 = new javax.swing.JTextField();
      jT_Field6 = new javax.swing.JTextField();
      jT_Field7 = new javax.swing.JTextField();
      jT_Field8 = new javax.swing.JTextField();
      jT_Field9 = new javax.swing.JTextField();
      jT_Field10 = new javax.swing.JTextField();
      jT_Field11 = new javax.swing.JTextField();
      jT_Field12 = new javax.swing.JTextField();
      jT_Field13 = new javax.swing.JTextField();
      jT_Field14 = new javax.swing.JTextField();
      jT_Field15 = new javax.swing.JTextField();
      jT_Field16 = new javax.swing.JTextField();
      jT_Field17 = new javax.swing.JTextField();
      jT_Field18 = new javax.swing.JTextField();
      jT_Field19 = new javax.swing.JTextField();
      jT_Field20 = new javax.swing.JTextField();
      jT_Field21 = new javax.swing.JTextField();
      jT_Field22 = new javax.swing.JTextField();
      jT_Field23 = new javax.swing.JTextField();
      jT_Field24 = new javax.swing.JTextField();
      jT_Field25 = new javax.swing.JTextField();
      jT_Field26 = new javax.swing.JTextField();
      jT_Field27 = new javax.swing.JTextField();
      jT_Field28 = new javax.swing.JTextField();
      jT_Field29 = new javax.swing.JTextField();
      jT_Field30 = new javax.swing.JTextField();
      jT_Field31 = new javax.swing.JTextField();
      jT_Field32 = new javax.swing.JTextField();
      jT_Field33 = new javax.swing.JTextField();
      jT_Field34 = new javax.swing.JTextField();
      jT_Field35 = new javax.swing.JTextField();
      jT_Field36 = new javax.swing.JTextField();
      jT_Field37 = new javax.swing.JTextField();
      jT_Field38 = new javax.swing.JTextField();
      jT_Field39 = new javax.swing.JTextField();
      jT_Field40 = new javax.swing.JTextField();
      jT_Field41 = new javax.swing.JTextField();
      jT_Field42 = new javax.swing.JTextField();
      jT_Field43 = new javax.swing.JTextField();
      jT_Field44 = new javax.swing.JTextField();
      jT_Field45 = new javax.swing.JTextField();
      jT_Field46 = new javax.swing.JTextField();
      jT_Field47 = new javax.swing.JTextField();
      jT_Field48 = new javax.swing.JTextField();
      jT_Field49 = new javax.swing.JTextField();
      jT_Field50 = new javax.swing.JTextField();
      jT_Field51 = new javax.swing.JTextField();
      jT_Field52 = new javax.swing.JTextField();
      jT_Field53 = new javax.swing.JTextField();
      jL_Heading1 = new javax.swing.JLabel();
      jL_Heading2 = new javax.swing.JLabel();
      jL_Heading3 = new javax.swing.JLabel();
      jL_Heading4 = new javax.swing.JLabel();
      jL_Heading5 = new javax.swing.JLabel();
      jL_Heading6 = new javax.swing.JLabel();
      jL_Heading7 = new javax.swing.JLabel();
      jL_Field53 = new javax.swing.JLabel();
      jSeparator2 = new javax.swing.JSeparator();
      jSeparator3 = new javax.swing.JSeparator();
      jSeparator31 = new javax.swing.JSeparator();
      jSeparator32 = new javax.swing.JSeparator();
      jSeparator33 = new javax.swing.JSeparator();
      jSeparator34 = new javax.swing.JSeparator();
      jSeparator35 = new javax.swing.JSeparator();
      jSeparator36 = new javax.swing.JSeparator();
      jSeparator321 = new javax.swing.JSeparator();
      jSeparator322 = new javax.swing.JSeparator();
      jSeparator323 = new javax.swing.JSeparator();
      jSeparator324 = new javax.swing.JSeparator();
      jBTN_OK_RQ_Form = new javax.swing.JButton();
      jBTN_Cancel_RQ_Form = new javax.swing.JButton();
      buttonGroup1 = new javax.swing.ButtonGroup();

      panel1.setLayout(null);

      setTitle("Reporter's Question's Form");
      jL_Field2.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field2.setText("Case ID");
      panel1.add(jL_Field2);
      jL_Field2.setBounds(10, 20, 130, 16);

      jL_Field3.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field3.setText("Hypothesis");
      panel1.add(jL_Field3);
      jL_Field3.setBounds(10, 50, 130, 16);

      jL_Field4.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field4.setText("Belief");
      panel1.add(jL_Field4);
      jL_Field4.setBounds(10, 80, 130, 16);

      jL_Field5.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field5.setText("Disbelief");
      panel1.add(jL_Field5);
      jL_Field5.setBounds(10, 110, 130, 16);

      jL_Field6.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field6.setText("Report Org");
      panel1.add(jL_Field6);
      jL_Field6.setBounds(10, 180, 130, 16);

      jL_Field7.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field7.setText("Report Affil");
      panel1.add(jL_Field7);
      jL_Field7.setBounds(10, 210, 130, 16);

      jL_Field8.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field8.setText("Report Desc");
      panel1.add(jL_Field8);
      jL_Field8.setBounds(10, 240, 130, 16);

      jL_Field9.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field9.setText("Report Name");
      panel1.add(jL_Field9);
      jL_Field9.setBounds(10, 270, 130, 16);

      jL_Field10.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field10.setText("Enemy Org");
      panel1.add(jL_Field10);
      jL_Field10.setBounds(10, 300, 130, 16);

      jL_Field11.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field11.setText("Enemy Affil");
      panel1.add(jL_Field11);
      jL_Field11.setBounds(10, 330, 130, 16);

      jL_Field12.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field12.setText("Enemy Desc");
      panel1.add(jL_Field12);
      jL_Field12.setBounds(10, 360, 130, 16);

      jL_Field13.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field13.setText("Enemy Name");
      panel1.add(jL_Field13);
      jL_Field13.setBounds(10, 390, 130, 16);

      jL_Field14.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field14.setText("Ally Org");
      panel1.add(jL_Field14);
      jL_Field14.setBounds(10, 420, 130, 16);

      jL_Field15.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field15.setText("Ally Affil");
      panel1.add(jL_Field15);
      jL_Field15.setBounds(10, 450, 130, 16);

      jL_Field16.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field16.setText("Ally Desc");
      panel1.add(jL_Field16);
      jL_Field16.setBounds(10, 480, 130, 16);

      jL_Field17.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field17.setText("Ally Name");
      panel1.add(jL_Field17);
      jL_Field17.setBounds(10, 510, 130, 16);

      jL_Field18.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field18.setText("Party Org");
      panel1.add(jL_Field18);
      jL_Field18.setBounds(10, 580, 130, 16);

      jL_Field19.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field19.setText("Party Affil");
      panel1.add(jL_Field19);
      jL_Field19.setBounds(10, 610, 130, 16);

      jL_Field20.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field20.setText("Party Desc");
      panel1.add(jL_Field20);
      jL_Field20.setBounds(10, 640, 130, 16);

      jL_Field21.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field21.setText("Party Name");
      panel1.add(jL_Field21);
      jL_Field21.setBounds(10, 670, 130, 16);

      jL_Field22.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field22.setText("Region");
      jL_Field22.setBounds(new Rectangle(250, 142, 170, 16));

      jL_Field23.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field23.setText("Nation");
      jL_Field23.setBounds(new Rectangle(250, 172, 170, 16));

      jL_Field24.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field24.setText("Local");
      jL_Field24.setBounds(new Rectangle(250, 202, 170, 16));

      jL_Field25.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field25.setText("City");
      jL_Field25.setBounds(new Rectangle(250, 232, 170, 16));

      jL_Field26.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field26.setText("Structure");
      jL_Field26.setBounds(new Rectangle(250, 262, 170, 16));

      jL_Field27.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field27.setText("Latitude");
      jL_Field27.setBounds(new Rectangle(250, 292, 170, 16));

      jL_Field28.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field28.setText("Longitude");
      jL_Field28.setBounds(new Rectangle(250, 322, 170, 16));

      jL_Field29.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field29.setText("Altitude");
      jL_Field29.setBounds(new Rectangle(250, 352, 170, 16));

      jL_Field30.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field30.setText("Subject");
      jL_Field30.setBounds(new Rectangle(250, 442, 170, 16));

      jL_Field31.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field31.setText("Action");
      jL_Field31.setBounds(new Rectangle(250, 472, 170, 16));

      jL_Field32.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field32.setText("Objective");
      jL_Field32.setBounds(new Rectangle(250, 502, 170, 16));

      jL_Field33.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field33.setText("Strategic");
      jL_Field33.setBounds(new Rectangle(250, 582, 170, 16));

      jL_Field34.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field34.setText("Operation");
      jL_Field34.setBounds(new Rectangle(250, 612, 170, 16));

      jL_Field35.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field35.setText("Tactical");
      jL_Field35.setBounds(new Rectangle(250, 642, 170, 16));

      jL_Field36.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field36.setText("Task");
      jL_Field36.setBounds(new Rectangle(250, 672, 170, 16));

      jL_Field37.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field37.setText("Method");
      jL_Field37.setBounds(new Rectangle(535, 313, 140, 16));

      jL_Field38.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field38.setText("Desc 1");
      jL_Field38.setBounds(new Rectangle(535, 343, 140, 16));

      jL_Field39.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field39.setText("Desc 2");
      jL_Field39.setBounds(new Rectangle(535, 373, 140, 16));

      jL_Field40.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field40.setText("Desc 3");
      jL_Field40.setBounds(new Rectangle(535, 403, 140, 16));

      jL_Field41.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field41.setText("Casualties");
      jL_Field41.setBounds(new Rectangle(535, 433, 140, 16));

      jL_Field42.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field42.setText("Injuries");
      jL_Field42.setBounds(new Rectangle(535, 463, 140, 16));

      jL_Field43.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field43.setText("Cost");
      jL_Field43.setBounds(new Rectangle(535, 493, 140, 16));

      jL_Field44.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field44.setText("Adjective");
      jL_Field44.setBounds(new Rectangle(535, 523, 140, 16));

      jL_Field45.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field45.setText("Start Time");
      jL_Field45.setBounds(new Rectangle(535, 583, 140, 16));

      jL_Field46.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field46.setText("End Time");
      jL_Field46.setBounds(new Rectangle(535, 613, 140, 16));

      jL_Field47.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field47.setText("Duration");
      jL_Field47.setBounds(new Rectangle(535, 643, 140, 16));

      jL_Field48.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field48.setText("Time Frame");
      jL_Field48.setBounds(new Rectangle(535, 673, 140, 16));

      jL_Field49.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field49.setText("?");
      jL_Field49.setBounds(new Rectangle(531, 130, 140, 16));

      jL_Field50.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field50.setText("Info-ID");
      jL_Field50.setBounds(new Rectangle(531, 160, 140, 16));

      jL_Field51.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field51.setText("Prepared");
      jL_Field51.setBounds(new Rectangle(531, 190, 140, 16));

      jL_Field52.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field52.setText("Prep-Date");
      jL_Field52.setBounds(new Rectangle(531, 220, 140, 16));

      panel1.add(jT_Field2);
      jT_Field2.setBounds(150, 20, 90, 20);

      panel1.add(jT_Field3);
      jT_Field3.setBounds(150, 50, 90, 20);

      panel1.add(jT_Field4);
      jT_Field4.setBounds(150, 80, 90, 20);

      panel1.add(jT_Field5);
      jT_Field5.setBounds(150, 110, 90, 20);

      panel1.add(jT_Field6);
      jT_Field6.setBounds(150, 180, 90, 20);

      panel1.add(jT_Field7);
      jT_Field7.setBounds(150, 210, 90, 20);

      panel1.add(jT_Field8);
      jT_Field8.setBounds(150, 240, 90, 20);

      panel1.add(jT_Field9);
      jT_Field9.setBounds(150, 270, 90, 20);

      panel1.add(jT_Field10);
      jT_Field10.setBounds(150, 300, 90, 20);

      panel1.add(jT_Field11);
      jT_Field11.setBounds(150, 330, 90, 20);

      panel1.add(jT_Field12);
      jT_Field12.setBounds(150, 360, 90, 20);

      panel1.add(jT_Field13);
      jT_Field13.setBounds(150, 390, 90, 20);

      panel1.add(jT_Field14);
      jT_Field14.setBounds(150, 420, 90, 20);

      panel1.add(jT_Field15);
      jT_Field15.setBounds(150, 450, 90, 20);

      panel1.add(jT_Field16);
      jT_Field16.setBounds(150, 480, 90, 20);

      panel1.add(jT_Field17);
      jT_Field17.setBounds(150, 510, 90, 20);

      panel1.add(jT_Field18);
      jT_Field18.setBounds(150, 580, 90, 20);

      panel1.add(jT_Field19);
      jT_Field19.setBounds(150, 610, 90, 20);

      panel1.add(jT_Field20);
      jT_Field20.setBounds(150, 640, 90, 20);

      panel1.add(jT_Field21);
      jT_Field21.setBounds(150, 670, 90, 20);

      jT_Field22.setBounds(new Rectangle(430, 142, 90, 20));

      jT_Field23.setBounds(new Rectangle(430, 172, 90, 20));

      jT_Field24.setBounds(new Rectangle(430, 202, 90, 20));

      jT_Field25.setBounds(new Rectangle(430, 232, 90, 20));

      jT_Field26.setBounds(new Rectangle(430, 262, 90, 20));

      jT_Field27.setBounds(new Rectangle(430, 292, 90, 20));

      jT_Field28.setBounds(new Rectangle(430, 322, 90, 20));

      jT_Field29.setBounds(new Rectangle(430, 352, 90, 20));

      jT_Field30.setBounds(new Rectangle(430, 442, 90, 20));

      jT_Field31.setBounds(new Rectangle(430, 472, 90, 20));

      jT_Field32.setBounds(new Rectangle(430, 502, 90, 20));

      jT_Field33.setBounds(new Rectangle(430, 582, 90, 20));

      jT_Field34.setBounds(new Rectangle(430, 612, 90, 20));

      jT_Field35.setBounds(new Rectangle(430, 642, 90, 20));

      jT_Field36.setBounds(new Rectangle(430, 672, 90, 20));

      jT_Field37.setBounds(new Rectangle(685, 313, 90, 20));

      jT_Field38.setBounds(new Rectangle(685, 343, 90, 20));

      jT_Field39.setBounds(new Rectangle(685, 373, 90, 20));

      jT_Field40.setBounds(new Rectangle(685, 403, 90, 20));

      jT_Field41.setBounds(new Rectangle(685, 433, 90, 20));

      jT_Field42.setBounds(new Rectangle(685, 463, 90, 20));

      jT_Field43.setBounds(new Rectangle(685, 493, 90, 20));

      jT_Field44.setBounds(new Rectangle(685, 523, 90, 20));

      jT_Field45.setBounds(new Rectangle(685, 583, 90, 20));

      jT_Field46.setBounds(new Rectangle(685, 613, 90, 20));

      jT_Field47.setBounds(new Rectangle(685, 643, 90, 20));

      jT_Field48.setBounds(new Rectangle(685, 673, 90, 20));

      jT_Field49.setBounds(new Rectangle(681, 130, 90, 20));

      jT_Field50.setBounds(new Rectangle(681, 160, 90, 20));

      jT_Field51.setBounds(new Rectangle(681, 190, 90, 20));

      jT_Field52.setBounds(new Rectangle(681, 220, 90, 20));

      jL_Heading1.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
      jL_Heading1.setText("Who (People & Groups)");
      jL_Heading1.setBounds(new Rectangle(41, 150, 210, 16));

      jL_Heading2.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
      jL_Heading2.setText("3rd Party");
      panel1.add(jL_Heading2);
      jL_Heading2.setBounds(new Rectangle(42, 560, 207, 16));

      jL_Heading3.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
      jL_Heading3.setText("Where (Location)");
      jL_Heading3.setBounds(new Rectangle(321, 122, 211, 16));

      jL_Heading4.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
      jL_Heading4.setText("What (Activities & Events)");
      jL_Heading4.setBounds(new Rectangle(323, 412, 208, 16));

      jL_Heading5.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
      jL_Heading5.setText("Why (Goal/Intent/Purpose)");
      jL_Heading5.setBounds(new Rectangle(322, 562, 208, 16));

      jL_Heading6.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
      jL_Heading6.setText("How (Means & Methods)");
      jL_Heading6.setBounds(new Rectangle(595, 293, 190, 16));

      jL_Heading7.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
      jL_Heading7.setText("When (Time & Duration)");
      jL_Heading7.setBounds(new Rectangle(595, 563, 191, 16));

      jL_Field53.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
      jL_Field53.setText("Source");
      jL_Field53.setBounds(new Rectangle(531, 250, 140, 16));

      jT_Field53.setBounds(new Rectangle(681, 250, 90, 20));

      jBTN_OK_RQ_Form.setText("OK");

      jBTN_OK_RQ_Form.setBounds(new Rectangle(556, 711, 78, 26));

      jBTN_Cancel_RQ_Form.setText("Cancel");

      jBTN_Cancel_RQ_Form.setBounds(new Rectangle(671, 711, 97, 26));





      panel1.add(jSeparator2);
      jSeparator2.setBounds(40, 540, 210, 10);

      jSeparator3.setBounds(new Rectangle(595, 699, 190, 10));

      panel1.add(jSeparator31);
      jSeparator31.setBounds(40, 140, 210, 10);

      panel1.add(jSeparator32);
      jSeparator32.setBounds(40, 700, 210, 10);

      jSeparator33.setBounds(new Rectangle(320, 542, 210, 10));

      jSeparator34.setBounds(new Rectangle(591, 110, 190, 10));

      jSeparator35.setBounds(new Rectangle(595, 553, 190, 10));

      jSeparator36.setBounds(new Rectangle(595, 283, 190, 10));

      jSeparator321.setBounds(new Rectangle(320, 392, 210, 10));

      jSeparator322.setBounds(new Rectangle(320, 112, 210, 10));

      panel1.add(jSeparator323);
      jSeparator323.setBounds(40, 10, 210, 10);

      jSeparator324.setBounds(new Rectangle(320, 702, 210, 10));

      getContentPane().add(panel1);
      panel1.add(jBTN_Cancel_RQ_Form);
      panel1.add(jBTN_OK_RQ_Form);
      panel1.add(jL_Field22);
      panel1.add(jSeparator324);
      panel1.add(jT_Field36);
      panel1.add(jT_Field35);
      panel1.add(jT_Field34);
      panel1.add(jT_Field33);
      panel1.add(jL_Heading5);
      panel1.add(jL_Field33);
      panel1.add(jL_Field34);
      panel1.add(jL_Field35);
      panel1.add(jL_Field36);
      panel1.add(jT_Field32);
      panel1.add(jSeparator33);
      panel1.add(jL_Field32);
      panel1.add(jT_Field31);
      panel1.add(jL_Field31);
      panel1.add(jT_Field30);
      panel1.add(jL_Field30);
      panel1.add(jL_Heading4);
      panel1.add(jSeparator321);
      panel1.add(jT_Field29);
      panel1.add(jL_Field29);
      panel1.add(jT_Field28);
      panel1.add(jL_Field28);
      panel1.add(jL_Field27);
      panel1.add(jT_Field27);
      panel1.add(jT_Field26);
      panel1.add(jL_Field26);
      panel1.add(jL_Field25);
      panel1.add(jT_Field25);
      panel1.add(jT_Field24);
      panel1.add(jL_Field24);
      panel1.add(jL_Field23);
      panel1.add(jT_Field23);
      panel1.add(jT_Field22);
      panel1.add(jL_Heading3);
      panel1.add(jSeparator322);
      panel1.add(jSeparator3);
      panel1.add(jL_Heading1);
      panel1.add(jT_Field47);
      panel1.add(jT_Field40);
      panel1.add(jL_Field41);
      panel1.add(jT_Field41);
      panel1.add(jT_Field42);
      panel1.add(jT_Field44);
      panel1.add(jL_Field48);
      panel1.add(jL_Field42);
      panel1.add(jL_Field45);
      panel1.add(jT_Field45);
      panel1.add(jT_Field43);
      panel1.add(jSeparator35);
      panel1.add(jL_Heading7);
      panel1.add(jL_Field47);
      panel1.add(jT_Field48);
      panel1.add(jL_Field43);
      panel1.add(jT_Field46);
      panel1.add(jL_Field46);
      panel1.add(jL_Field44);
      panel1.add(jL_Field40);
      panel1.add(jT_Field39);
      panel1.add(jL_Field39);
      panel1.add(jL_Field38);
      panel1.add(jT_Field38);
      panel1.add(jT_Field37);
      panel1.add(jL_Field37);
      panel1.add(jL_Heading6);
      panel1.add(jSeparator36);
      panel1.add(jT_Field53);
      panel1.add(jL_Field53);
      panel1.add(jL_Field52);
      panel1.add(jT_Field52);
      panel1.add(jT_Field51);
      panel1.add(jL_Field51);
      panel1.add(jL_Field50);
      panel1.add(jT_Field50);
      panel1.add(jT_Field49);
      panel1.add(jL_Field49);
      panel1.add(jSeparator34);
    }

    /** Does absolutyly NO formating to
     * the incoming string.  String array must be
     * atleast 60 elements.
     */
    public void setFieldValues(String[] sRecord)
    {
      int count = 0;
      jT_Field2.setText(sRecord[count]);
      jT_Field3.setText(sRecord[++count]);
      jT_Field4.setText(sRecord[++count]);
      jT_Field5.setText(sRecord[++count]);
      jT_Field49.setText(sRecord[++count]);
      jT_Field50.setText(sRecord[++count]);
      jT_Field51.setText(sRecord[++count]);
      jT_Field52.setText(sRecord[++count]);
      jT_Field53.setText(sRecord[++count]);
      jT_Field6.setText(sRecord[++count]);
      jT_Field7.setText(sRecord[++count]);
      jT_Field8.setText(sRecord[++count]);
      jT_Field9.setText(sRecord[++count]);
      jT_Field10.setText(sRecord[++count]);
      jT_Field11.setText(sRecord[++count]);
      jT_Field12.setText(sRecord[++count]);
      jT_Field13.setText(sRecord[++count]);
      jT_Field14.setText(sRecord[++count]);
      jT_Field15.setText(sRecord[++count]);
      jT_Field16.setText(sRecord[++count]);
      jT_Field17.setText(sRecord[++count]);
      jT_Field18.setText(sRecord[++count]);
      jT_Field19.setText(sRecord[++count]);
      jT_Field20.setText(sRecord[++count]);
      jT_Field21.setText(sRecord[++count]);
      jT_Field22.setText(sRecord[++count]);
      jT_Field23.setText(sRecord[++count]);
      jT_Field24.setText(sRecord[++count]);
      jT_Field25.setText(sRecord[++count]);
      jT_Field26.setText(sRecord[++count]);
      jT_Field27.setText(sRecord[++count]);
      jT_Field28.setText(sRecord[++count]);
      jT_Field29.setText(sRecord[++count]);
      jT_Field30.setText(sRecord[++count]);
      jT_Field31.setText(sRecord[++count]);
      jT_Field32.setText(sRecord[++count]);
      jT_Field33.setText(sRecord[++count]);
      jT_Field34.setText(sRecord[++count]);
      jT_Field35.setText(sRecord[++count]);
      jT_Field36.setText(sRecord[++count]);
      jT_Field37.setText(sRecord[++count]);
      jT_Field38.setText(sRecord[++count]);
      jT_Field39.setText(sRecord[++count]);
      jT_Field40.setText(sRecord[++count]);
      jT_Field41.setText(sRecord[++count]);
      jT_Field42.setText(sRecord[++count]);
      jT_Field43.setText(sRecord[++count]);
      jT_Field44.setText(sRecord[++count]);
      jT_Field45.setText(sRecord[++count]);
      jT_Field46.setText(sRecord[++count]);
      jT_Field47.setText(sRecord[++count]);
      jT_Field48.setText(sRecord[++count]);
    }
    
    public String[] getLabelValues() {
      String labels[] = new String[60];
      int count = 0;
        
      labels[count] = jL_Field2.getText();
      labels[++count] = jL_Field3.getText();
      labels[++count] = jL_Field4.getText();
      labels[++count] = jL_Field5.getText();
      labels[++count] = jL_Field6.getText();
      labels[++count] = jL_Field7.getText();
      labels[++count] = jL_Field8.getText();
      labels[++count] = jL_Field9.getText();
      labels[++count] = jL_Field10.getText();
      labels[++count] = jL_Field11.getText();
      labels[++count] = jL_Field12.getText();
      labels[++count] = jL_Field13.getText();
      labels[++count] = jL_Field14.getText();
      labels[++count] = jL_Field15.getText();
      labels[++count] = jL_Field16.getText();
      labels[++count] = jL_Field17.getText();
      labels[++count] = jL_Field18.getText();
      labels[++count] = jL_Field19.getText();
      labels[++count] = jL_Field20.getText();
      labels[++count] = jL_Field21.getText();
      labels[++count] = jL_Field22.getText();
      labels[++count] = jL_Field23.getText();
      labels[++count] = jL_Field24.getText();
      labels[++count] = jL_Field25.getText();
      labels[++count] = jL_Field26.getText();
      labels[++count] = jL_Field27.getText();
      labels[++count] = jL_Field28.getText();
      labels[++count] = jL_Field29.getText();
      labels[++count] = jL_Field30.getText();
      labels[++count] = jL_Field31.getText();
      labels[++count] = jL_Field32.getText();
      labels[++count] = jL_Field33.getText();
      labels[++count] = jL_Field34.getText();
      labels[++count] = jL_Field35.getText();
      labels[++count] = jL_Field36.getText();
      labels[++count] = jL_Field37.getText();
      labels[++count] = jL_Field38.getText();
      labels[++count] = jL_Field39.getText();
      labels[++count] = jL_Field40.getText();
      labels[++count] = jL_Field41.getText();
      labels[++count] = jL_Field42.getText();
      labels[++count] = jL_Field43.getText();
      labels[++count] = jL_Field44.getText();
      labels[++count] = jL_Field45.getText();
      labels[++count] = jL_Field46.getText();
      labels[++count] = jL_Field47.getText();
      labels[++count] = jL_Field48.getText();
      labels[++count] = jL_Field49.getText();
      labels[++count] = jL_Field50.getText();
      labels[++count] = jL_Field51.getText();
      labels[++count] = jL_Field52.getText();
      labels[++count] = jL_Field53.getText();
        
      return labels;
    }

    /** Returns an array of 60
     * Not all values are used, only 52
     */
    public String[] getFieldValues()
    {
      String sRecord[] = new String[60];
      int count = 0;
      sRecord[count] = jT_Field2.getText();
      sRecord[++count] = jT_Field3.getText();
      sRecord[++count] = jT_Field4.getText();
      sRecord[++count] = jT_Field5.getText();
      sRecord[++count] = jT_Field49.getText();
      sRecord[++count] = jT_Field50.getText();
      sRecord[++count] = jT_Field51.getText();
      sRecord[++count] = jT_Field52.getText();
      sRecord[++count] = jT_Field53.getText();
      sRecord[++count] = jT_Field6.getText();
      sRecord[++count] = jT_Field7.getText();
      sRecord[++count] = jT_Field8.getText();
      sRecord[++count] = jT_Field9.getText();
      sRecord[++count] = jT_Field10.getText();
      sRecord[++count] = jT_Field11.getText();
      sRecord[++count] = jT_Field12.getText();
      sRecord[++count] = jT_Field13.getText();
      sRecord[++count] = jT_Field14.getText();
      sRecord[++count] = jT_Field15.getText();
      sRecord[++count] = jT_Field16.getText();
      sRecord[++count] = jT_Field17.getText();
      sRecord[++count] = jT_Field18.getText();
      sRecord[++count] = jT_Field19.getText();
      sRecord[++count] = jT_Field20.getText();
      sRecord[++count] = jT_Field21.getText();
      sRecord[++count] = jT_Field22.getText();
      sRecord[++count] = jT_Field23.getText();
      sRecord[++count] = jT_Field24.getText();
      sRecord[++count] = jT_Field25.getText();
      sRecord[++count] = jT_Field26.getText();
      sRecord[++count] = jT_Field27.getText();
      sRecord[++count] = jT_Field28.getText();
      sRecord[++count] = jT_Field29.getText();
      sRecord[++count] = jT_Field30.getText();
      sRecord[++count] = jT_Field31.getText();
      sRecord[++count] = jT_Field32.getText();
      sRecord[++count] = jT_Field33.getText();
      sRecord[++count] = jT_Field34.getText();
      sRecord[++count] = jT_Field35.getText();
      sRecord[++count] = jT_Field36.getText();
      sRecord[++count] = jT_Field37.getText();
      sRecord[++count] = jT_Field38.getText();
      sRecord[++count] = jT_Field39.getText();
      sRecord[++count] = jT_Field40.getText();
      sRecord[++count] = jT_Field41.getText();
      sRecord[++count] = jT_Field42.getText();
      sRecord[++count] = jT_Field43.getText();
      sRecord[++count] = jT_Field44.getText();
      sRecord[++count] = jT_Field45.getText();
      sRecord[++count] = jT_Field46.getText();
      sRecord[++count] = jT_Field47.getText();
      sRecord[++count] = jT_Field48.getText();

      return sRecord;
    }

    /** The array of strings is formated from the Database.
     * Each field is an element in the array.  The first char in each
     * string is removed and any _ are converted to spaces.
     */
    public void setLabels(String[] Labels)
    {
      int Count = 0;
      jL_Field2.setText(Labels[Count].substring(1).replace('_',  ' '));
      jL_Field2.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field3.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field4.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field5.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Heading1.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field6.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field7.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field8.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field9.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field10.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field11.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field12.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field13.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field14.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field15.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field16.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field17.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Heading2.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field18.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field19.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field20.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field21.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Heading3.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field22.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field23.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field24.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field25.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field26.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field27.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field28.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field29.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Heading4.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field30.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field31.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field32.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Heading5.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field33.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field34.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field35.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field36.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Heading6.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field37.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field38.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field39.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field40.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field41.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field42.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field43.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field44.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Heading7.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field45.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field46.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field47.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field48.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field49.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field50.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field51.setText(Labels[Count++].substring(1).replace('_',  ' '));
      jL_Field52.setText(Labels[Count++].substring(1).replace('_',  ' '));
    }

    public static void main(String [] args)
    {
      RQ_Form rf = new RQ_Form();
      rf.setVisible(true);
      rf.addWindowListener(new WindowAdapter(){
        public void windowClosing(WindowEvent e){
          System.exit(0);
        }
      });
    }
}

