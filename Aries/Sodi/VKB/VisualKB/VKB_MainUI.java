/*
 * VKB_MainUI.java
 *
 * Created on May 22, 2003, 8:56 AM
 */

package VisualKB;

import VisualKB.gui.*;
import DBConnect.*;
import java.awt.*;
import javax.swing.*;
import javax.swing.tree.*;
import javax.swing.event.*;
import java.util.Vector;
import com.appliedminds.hmv.*;
import java.util.logging.ConsoleHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;
import com.appliedminds.martinix.gapp.*;
import java.awt.event.*;
import com.ngms.ontology.*;
//import com.ngms.ie.*;

/**
 *
 * @author  Jeff McCartney
 */
public class VKB_MainUI extends javax.swing.JFrame {
    
    public RQ_Form RQform = new RQ_Form();
    public com.appliedminds.hmv.HMV hmv = new com.appliedminds.hmv.HMV();
    public javax.swing.JTable Records = new javax.swing.JTable();
    public javax.swing.JScrollPane jScrollPane2 = new javax.swing.JScrollPane();
    public JDialog DomainName = new JDialog();
    public JDialog StoryName = new JDialog();
    public JDialog LabelEditor = new JDialog();
    public JTextField InputField = new JTextField();
    public java.util.Date Time = new java.util.Date();
    public /*BNE_View*/ HMV BNEView = new HMV();//BNE_View();
    public JMenuItem DD_menuItem = new JMenuItem();
    public JMenuItem NS_menuItem = new JMenuItem();
    public JMenuItem DS_menuItem = new JMenuItem();
    public JMenuItem SS_menuItem = new JMenuItem();


    /** Creates new form VKB_MainUI */
    public VKB_MainUI() {
        try{
            ImageIcon icon = new ImageIcon("images/ARIES.gif", "ARIES");
            setIconImage(icon.getImage());
        }catch(Exception e){
            System.out.println("Didn't load Icon.?");
            e.printStackTrace();
        }
        initComponents();
        ToolTipManager.sharedInstance().registerComponent(jT_Hierarchy);
        changeView();
        fillTree();

        Records.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                RecordsMouseClicked(evt);
            }
        });

        BNEView.initGAppFrame(false);
        BNEView.setSize(800, 800);
        BNEView.show();
        BNEView.show(false);
        

        //Create the popup menu.
        JPopupMenu popup = new JPopupMenu();
        DD_menuItem = new JMenuItem("Delete Domain");
        DD_menuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                DeleteDomain_ActionPerformed(evt);
            }
        });
        popup.add(DD_menuItem);
        NS_menuItem = new JMenuItem("New Story");
        NS_menuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                AddStory_ActionPerformed(evt);
            }
        });
        popup.add(NS_menuItem);
        DS_menuItem = new JMenuItem("Delete Story");
        DS_menuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                DeleteStory_ActionPerformed(evt);
            }
        });
        popup.add(DS_menuItem);
        SS_menuItem = new JMenuItem("Save Story");
        SS_menuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                SaveStory_ActionPerformed(evt);
            }
        });
        popup.add(SS_menuItem);

        //Add listener to the text area so the popup menu can come up.
        MouseListener popupListener = new PopupListener(popup);
        jT_Hierarchy.addMouseListener(popupListener);

        JButton jBTN_Reset = new JButton();
        jBTN_Reset.setText("Clear Form");
        jBTN_Reset.setBounds(450, 707, 190, 26);
        jBTN_Reset.setBackground(RQform.jBTN_Cancel_RQ_Form.getBackground());
        jBTN_Reset.setFont(RQform.jBTN_Cancel_RQ_Form.getFont());
        jBTN_Reset.setBorder(RQform.jBTN_Cancel_RQ_Form.getBorder());
        jBTN_Reset.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                RQform.setFieldValues(new String[60]);
            }
        });
        RQform.panel1.add(jBTN_Reset);

        JButton jBTN_SaveLabels = new JButton();
        jBTN_SaveLabels.setText("Edit Label");
        jBTN_SaveLabels.setBounds(450, 737, 190, 26);
        jBTN_SaveLabels.setEnabled(false);
        jBTN_SaveLabels.setBackground(RQform.jBTN_Cancel_RQ_Form.getBackground());
        jBTN_SaveLabels.setFont(RQform.jBTN_Cancel_RQ_Form.getFont());
        jBTN_SaveLabels.setBorder(RQform.jBTN_Cancel_RQ_Form.getBorder());
        jBTN_SaveLabels.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                RQFormEditLabel_ActionPerformed(evt);
            }
        });
        RQform.panel1.add(jBTN_SaveLabels);

        JButton jBTN_ExecuteAssistRQ = new JButton();
        jBTN_ExecuteAssistRQ.setText("Process Text");
        jBTN_ExecuteAssistRQ.setBounds(640, 30, 190, 26);
        jBTN_ExecuteAssistRQ.setEnabled(false);
        jBTN_ExecuteAssistRQ.setBackground(RQform.jBTN_Cancel_RQ_Form.getBackground());
        jBTN_ExecuteAssistRQ.setFont(RQform.jBTN_Cancel_RQ_Form.getFont());
        jBTN_ExecuteAssistRQ.setBorder(RQform.jBTN_Cancel_RQ_Form.getBorder());
        jBTN_ExecuteAssistRQ.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                //RQFormEditLabel_ActionPerformed(evt);
		//infoExtract();
            }
        });
        RQform.panel1.add(jBTN_ExecuteAssistRQ);

        JButton jBTN_ExecuteTextClass = new JButton();
        jBTN_ExecuteTextClass.setText("Suggest Hypothesis");
        jBTN_ExecuteTextClass.setBounds(246, 50, 147, 22);
        jBTN_ExecuteTextClass.setEnabled(true);
        jBTN_ExecuteTextClass.setBackground(RQform.jBTN_Cancel_RQ_Form.getBackground());
        jBTN_ExecuteTextClass.setFont(RQform.jBTN_Cancel_RQ_Form.getFont());
        jBTN_ExecuteTextClass.setBorder(RQform.jBTN_Cancel_RQ_Form.getBorder());
        jBTN_ExecuteTextClass.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                //RQFormEditLabel_ActionPerformed(evt);
		classify();
            }
        });
        RQform.panel1.add(jBTN_ExecuteTextClass);

        JButton jBTN_ExecuteBDIdent = new JButton();
        jBTN_ExecuteBDIdent.setText("Suggest Belief/Disbelief");
        jBTN_ExecuteBDIdent.setBounds(246, 78, 147, 22);
        jBTN_ExecuteBDIdent.setEnabled(false);
        jBTN_ExecuteBDIdent.setBackground(RQform.jBTN_Cancel_RQ_Form.getBackground());
        jBTN_ExecuteBDIdent.setFont(RQform.jBTN_Cancel_RQ_Form.getFont());
        jBTN_ExecuteBDIdent.setBorder(RQform.jBTN_Cancel_RQ_Form.getBorder());
        jBTN_ExecuteBDIdent.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                //RQFormEditLabel_ActionPerformed(evt);
            }
        });
        RQform.panel1.add(jBTN_ExecuteBDIdent);

        RQform.jBTN_Cancel_RQ_Form.setText("Save As New Evidence");
        RQform.jBTN_Cancel_RQ_Form.setBounds(650, 707, 190, 26);
        RQform.jBTN_Cancel_RQ_Form.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                RQFormSaveNewEvidence_ActionPerformed(evt);
            }
        });
        RQform.jBTN_OK_RQ_Form.setText("Save Changes To Evidence");
        RQform.jBTN_OK_RQ_Form.setEnabled(false);
        RQform.jBTN_OK_RQ_Form.setBounds(650, 737, 190, 26);
        RQform.jBTN_OK_RQ_Form.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                RQFormSaveModifiedEvidence_ActionPerformed(evt);
            }
        });
        
        jTabbedPane1.setTitleAt(0, "User Log");

        Time.setTime(System.currentTimeMillis());
        jTA_Logger.setText(jTA_Logger.getText() + "\n\'User\' has logged on at " + Time.toLocaleString() + ".");

        jT_Hierarchy.setCellRenderer(new TreeIconRenderer());
    }

    /**
     * Suggest a hypothesis for a template
     *
     * (This was written by Andy Trimble and uses
     * the classification software written by the author)
    */
    public void classify() {
        TemplateManager templMan;
	OntologyManager ontMan;
	int index;
	DBConnect dbconn = new DBConnect();
        //System.out.print("Suggest hypothesis: ");
	String[] Evidence = RQform.getFieldValues();
	int caseID = (new Integer(Evidence[0])).intValue();
	String mission = dbconn.configuration.getMission();
	try {
	    templMan = new TemplateManager();
	    templMan.parse(dbconn.configuration.Port,
	                                 dbconn.configuration.Host,
					 dbconn.configuration.User,
					 dbconn.configuration.Password,
					 mission);
	    ontMan = new OntologyManager(dbconn.configuration.Port,
	                                 dbconn.configuration.Host,
					 dbconn.configuration.User,
					 dbconn.configuration.Password,
					 mission);

	    index = 0;

	    for(int i = 0; i < templMan.getInstanceNum(); i++) {
	       if((new Integer(templMan.elementAt(i, 0))).intValue() == caseID) {
	            index = i;
	            break;
	        }
	    }
	    ontMan.matchHits(templMan, index);
	    //System.out.print(ontMan.tagMean().getName() + "\n");
	    //System.out.flush();

	    String[] evid = RQform.getFieldValues();
	    evid[1] = ontMan.tagMean().getName();
	    RQform.setFieldValues(evid);
	} catch(Exception ex) {
	    ex.printStackTrace();
	}
    }

    public void infoExtract() {
        System.out.println("Extract Information");
	//AssistRQ arq = new AssistRQ();
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    private void initComponents() {//GEN-BEGIN:initComponents
        jScrollPane1 = new javax.swing.JScrollPane();
        jT_Hierarchy = new javax.swing.JTree();
        jTabbedPane1 = new javax.swing.JTabbedPane();
        jTA_Logger = new javax.swing.JTextArea();
        jMenuBar1 = new javax.swing.JMenuBar();
        jM_File = new javax.swing.JMenu();
        jM_FileNewDomain = new javax.swing.JMenuItem();
        jM_FileExit = new javax.swing.JMenuItem();
        jM_Edit = new javax.swing.JMenu();
        jMI_Configure = new javax.swing.JMenuItem();
        jM_View = new javax.swing.JMenu();
        jM_Zoom = new javax.swing.JMenu();
        jM_ZoomOut = new javax.swing.JMenuItem();
        jM_ZoomIn = new javax.swing.JMenuItem();
        jM_View_Refresh = new javax.swing.JMenuItem();

        getContentPane().setLayout(null);

        setTitle("ARIES: Visual Knowledge Base");
        addWindowListener(new java.awt.event.WindowAdapter() {
            public void windowClosing(java.awt.event.WindowEvent evt) {
                exitForm(evt);
            }
        });

        jScrollPane1.setPreferredSize(new java.awt.Dimension(95, 485));
        jT_Hierarchy.setFont(new java.awt.Font("Dialog", 0, 16));
        jT_Hierarchy.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jT_HierarchyMouseClicked(evt);
            }
        });

        jScrollPane1.setViewportView(jT_Hierarchy);

        getContentPane().add(jScrollPane1);
        jScrollPane1.setBounds(0, 0, 160, 790);

        jTA_Logger.setFont(new java.awt.Font("Microsoft Sans Serif", 0, 14));
        jTabbedPane1.addTab("tab1", jTA_Logger);

        getContentPane().add(jTabbedPane1);
        jTabbedPane1.setBounds(160, 0, 890, 790);

        jM_File.setText("File");
        jM_FileNewDomain.setText("New Domain");
        jM_FileNewDomain.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jM_FileNewDomainActionPerformed(evt);
            }
        });

        jM_File.add(jM_FileNewDomain);
        jM_FileExit.setText("Exit");
        jM_FileExit.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jM_FileExitActionPerformed(evt);
            }
        });

        jM_File.add(jM_FileExit);
        jMenuBar1.add(jM_File);
        jM_Edit.setText("Edit");
        jMI_Configure.setText("Configure ...");
        jMI_Configure.setToolTipText("help me");
        jMI_Configure.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMI_ConfigureActionPerformed(evt);
            }
        });

        jM_Edit.add(jMI_Configure);
        jMenuBar1.add(jM_Edit);
        jM_View.setText("View");
        jM_Zoom.setText("Zoom");
        jM_ZoomOut.setText("Zoom Out");
        jM_ZoomOut.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jM_ZoomOutActionPerformed(evt);
            }
        });

        jM_Zoom.add(jM_ZoomOut);
        jM_ZoomIn.setText("Zoom In");
        jM_ZoomIn.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jM_ZoomInActionPerformed(evt);
            }
        });

        jM_Zoom.add(jM_ZoomIn);
        jM_View.add(jM_Zoom);
        jM_View_Refresh.setText("Refresh");
        jM_View_Refresh.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jM_View_RefreshActionPerformed(evt);
            }
        });

        jM_View.add(jM_View_Refresh);
        jMenuBar1.add(jM_View);
        setJMenuBar(jMenuBar1);

        pack();
    }//GEN-END:initComponents

    private void jM_View_RefreshActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jM_View_RefreshActionPerformed
        // Add your handling code here:
    //    fillTree();
    }//GEN-LAST:event_jM_View_RefreshActionPerformed

    double scale = 1.0;
    private void jM_ZoomInActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jM_ZoomInActionPerformed
        // Add your handling code here:
        scale += 0.1;
        BNEView.getGraphPanel().setScale(scale);
        BNEView.getGraphPanel().redraw();
    }//GEN-LAST:event_jM_ZoomInActionPerformed

    private void jM_ZoomOutActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jM_ZoomOutActionPerformed
        // Add your handling code here:
        scale -= 0.1;
        BNEView.getGraphPanel().setScale(scale);
        BNEView.getGraphPanel().redraw();
    }//GEN-LAST:event_jM_ZoomOutActionPerformed

    private void jM_FileNewDomainActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jM_FileNewDomainActionPerformed
        // Add your handling code here:
        //Create a new database in the database server with an Evidence tabel and a
        // Label table.
        JLabel LabelName = new JLabel();
        LabelName.setText("Domain Name:");
        JButton BTN_OK = new JButton();
        BTN_OK.setText("Create");
        BTN_OK.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                NewDomain_OK_ActionPerformed(evt);
            }
        });
        JButton BTN_Cancel = new JButton();
        BTN_Cancel.setText("Cancel");
        BTN_Cancel.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                DomainName.setVisible(false);
                DomainName.removeAll();
            }
        });
        DomainName.setTitle("New Domain Name");
        DomainName.getContentPane().setLayout(null);
        DomainName.getContentPane().add(LabelName);
        LabelName.setBounds(2, 16, 85, 9);
        DomainName.getContentPane().add(InputField);
        InputField.setBounds(88, 12, 160, 20);
        DomainName.getContentPane().add(BTN_OK);
        BTN_OK.setBounds(30, 60, 90, 20);
        DomainName.getContentPane().add(BTN_Cancel);
        BTN_Cancel.setBounds(150, 60, 90, 20);

        Dimension screenSize = this.getSize();
        DomainName.setSize(300, 150);
        Dimension frameSize = DomainName.getSize();
        if (frameSize.height > screenSize.height) {
          frameSize.height = screenSize.height;
        }
        if (frameSize.width > screenSize.width) {
          frameSize.width = screenSize.width;
        }
        DomainName.setLocation(((screenSize.width - frameSize.width) / 2) + this.getLocationOnScreen().x, ((screenSize.height - frameSize.height) / 2) + this.getLocationOnScreen().y);
        DomainName.setModal(true);
        DomainName.show();
    }//GEN-LAST:event_jM_FileNewDomainActionPerformed

    private void NewDomain_OK_ActionPerformed(java.awt.event.ActionEvent evt)
    {
        String InputDomainName = InputField.getText().trim();
        boolean test = true;
        DBConnect DBQuery = new DBConnect();
        Vector Domains = DBQuery.runQuery("Show databases;");
        for(int i = 0; i < Domains.size(); i++)
        {
            if(InputDomainName.equalsIgnoreCase(Domains.get(i).toString().replace('[', ' ').replace(']', ' ').trim()))
            {
                test = false;
            }
        }
        if(test)
        {
            DomainName.setVisible(false);
            //Create database with the given name and an Evidence table and a Labels table.
            DBQuery.executeQuery("CREATE DATABASE " + InputDomainName + ";");
            DBQuery.configuration.setMission(InputDomainName);

            Time.setTime(System.currentTimeMillis());
            jTA_Logger.setText(jTA_Logger.getText() + "\n\'User\' has created the Domain " + InputDomainName + " at " + Time.toLocaleString() + ".");

            String Fields = new String();
            Fields = "";
            for(int i = 1; i < 60; i++)
            {
                Fields += "Field" + i + " VARCHAR(255)" + ", ";
            }
            Fields += "Field60 VARCHAR(255)";
            DBQuery.executeQuery("CREATE TABLE Labels (" + Fields + ");");
            String[] Values = new String[60];
            Values[0] = "Case_ID";
            Values[1] = "Hypothesis";
            Values[2] = "Belief";
            Values[3] = "Disbelief";
            for(int i = 4; i < 55; i++)
            {
                Values[i] = "Field";
            }
            Values[54] = "Evidence_Type";
            Values[55] = "Description";
            Values[56] = "Prepared";
            Values[57] = "Prep_Date";
            Values[58] = "Source";
            Values[59] = "Source";
            RQform.setLabels(Values);
            Values = RQform.getLabels();
            String Value = new String();
            for(int i = 0; i < 59; i++)
            {
                Value += "\'" + Values[i] + "\', ";
            }
            Value += "\'" + "-Source" + "\'";
            DBQuery.executeQuery("INSERT INTO Labels VALUES (" + Value + ");");
            int count = 1;
            DBQuery.executeQuery("CREATE TABLE Evidence (Case_ID INT(50) NOT NULL AUTO_INCREMENT, " + 
                                Values[1].substring(1) + " VARCHAR(255) DEFAULT '?', " + 
                                Values[2].substring(1) + " VARCHAR(255) DEFAULT '?', " + 
                                Values[3].substring(1) + " VARCHAR(255) DEFAULT '?', " + 
                                Values[5].substring(1).replace('?', ' ').trim() + "1 VARCHAR(255) DEFAULT '?', " + 
                                Values[6].substring(1).replace('?', ' ').trim() + "2 VARCHAR(255) DEFAULT '?', " + 
                                Values[7].substring(1).replace('?', ' ').trim() + "3 VARCHAR(255) DEFAULT '?', " + 
                                Values[8].substring(1).replace('?', ' ').trim() + "4 VARCHAR(255) DEFAULT '?', " + 
                                Values[9].substring(1).replace('?', ' ').trim() + "5 VARCHAR(255) DEFAULT '?', " + 
                                Values[10].substring(1).replace('?', ' ').trim() + "6 VARCHAR(255) DEFAULT '?', " + 
                                Values[11].substring(1).replace('?', ' ').trim() + "7 VARCHAR(255) DEFAULT '?', " + 
                                Values[12].substring(1).replace('?', ' ').trim() + "8 VARCHAR(255) DEFAULT '?', " + 
                                Values[13].substring(1).replace('?', ' ').trim() + "9 VARCHAR(255) DEFAULT '?', " + 
                                Values[14].substring(1).replace('?', ' ').trim() + "10 VARCHAR(255) DEFAULT '?', " + 
                                Values[15].substring(1).replace('?', ' ').trim() + "11 VARCHAR(255) DEFAULT '?', " + 
                                Values[16].substring(1).replace('?', ' ').trim() + "12 VARCHAR(255) DEFAULT '?', " + 
                                Values[18].substring(1).replace('?', ' ').trim() + "13 VARCHAR(255) DEFAULT '?', " + 
                                Values[19].substring(1).replace('?', ' ').trim() + "14 VARCHAR(255) DEFAULT '?', " + 
                                Values[20].substring(1).replace('?', ' ').trim() + "15 VARCHAR(255) DEFAULT '?', " + 
                                Values[21].substring(1).replace('?', ' ').trim() + "16 VARCHAR(255) DEFAULT '?', " + 
                                Values[23].substring(1).replace('?', ' ').trim() + "17 VARCHAR(255) DEFAULT '?', " + 
                                Values[24].substring(1).replace('?', ' ').trim() + "18 VARCHAR(255) DEFAULT '?', " + 
                                Values[25].substring(1).replace('?', ' ').trim() + "19 VARCHAR(255) DEFAULT '?', " + 
                                Values[26].substring(1).replace('?', ' ').trim() + "20 VARCHAR(255) DEFAULT '?', " + 
                                Values[27].substring(1).replace('?', ' ').trim() + "21 VARCHAR(255) DEFAULT '?', " + 
                                Values[28].substring(1).replace('?', ' ').trim() + "22 VARCHAR(255) DEFAULT '?', " + 
                                Values[29].substring(1).replace('?', ' ').trim() + "23 VARCHAR(255) DEFAULT '?', " + 
                                Values[30].substring(1).replace('?', ' ').trim() + "24 VARCHAR(255) DEFAULT '?', " + 
                                Values[32].substring(1).replace('?', ' ').trim() + "25 VARCHAR(255) DEFAULT '?', " + 
                                Values[33].substring(1).replace('?', ' ').trim() + "26 VARCHAR(255) DEFAULT '?', " + 
                                Values[34].substring(1).replace('?', ' ').trim() + "27 VARCHAR(255) DEFAULT '?', " + 
                                Values[36].substring(1).replace('?', ' ').trim() + "28 VARCHAR(255) DEFAULT '?', " + 
                                Values[37].substring(1).replace('?', ' ').trim() + "29 VARCHAR(255) DEFAULT '?', " + 
                                Values[38].substring(1).replace('?', ' ').trim() + "30 VARCHAR(255) DEFAULT '?', " + 
                                Values[39].substring(1).replace('?', ' ').trim() + "31 VARCHAR(255) DEFAULT '?', " + 
                                Values[41].substring(1).replace('?', ' ').trim() + "32 VARCHAR(255) DEFAULT '?', " + 
                                Values[42].substring(1).replace('?', ' ').trim() + "33 VARCHAR(255) DEFAULT '?', " + 
                                Values[43].substring(1).replace('?', ' ').trim() + "34 VARCHAR(255) DEFAULT '?', " + 
                                Values[44].substring(1).replace('?', ' ').trim() + "35 VARCHAR(255) DEFAULT '?', " + 
                                Values[45].substring(1).replace('?', ' ').trim() + "36 VARCHAR(255) DEFAULT '?', " + 
                                Values[46].substring(1).replace('?', ' ').trim() + "37 VARCHAR(255) DEFAULT '?', " + 
                                Values[47].substring(1).replace('?', ' ').trim() + "38 VARCHAR(255) DEFAULT '?', " +
                                Values[48].substring(1).replace('?', ' ').trim() + "39 VARCHAR(255) DEFAULT '?', " +
                                Values[50].substring(1).replace('?', ' ').trim() + "40 VARCHAR(255) DEFAULT '?', " +
                                Values[51].substring(1).replace('?', ' ').trim() + "41 VARCHAR(255) DEFAULT '?', " +
                                Values[52].substring(1).replace('?', ' ').trim() + "42 VARCHAR(255) DEFAULT '?', " +
                                Values[53].substring(1).replace('?', ' ').trim() + "43 VARCHAR(255) DEFAULT '?', " +
                                Values[54].substring(1) + " VARCHAR(255) DEFAULT '?', " +
                                Values[55].substring(1) + " VARCHAR(255) DEFAULT '?', " +
                                Values[56].substring(1) + " VARCHAR(255) DEFAULT '?', " +
                                Values[57].substring(1) + " VARCHAR(255) DEFAULT '?', " +
                                Values[58].substring(1) + " VARCHAR(255) DEFAULT '?'" + ", PRIMARY KEY (Case_ID));");

            fillTree();
        }else
        {
            //do nothing
        }
        InputField.setText("");
        DomainName.removeAll();
    }

    private void jM_FileExitActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jM_FileExitActionPerformed
        // Add your handling code here:
        System.exit(0);
    }//GEN-LAST:event_jM_FileExitActionPerformed

    private void jT_HierarchyMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jT_HierarchyMouseClicked
        // Add your handling code here:
        RQform.jBTN_OK_RQ_Form.setEnabled(false);
        RQform.setFieldValues(new String[60]);
        Object[][] Evid = new Object[0][0];
        String[] Label;
        
        DBConnect DBQuery = new DBConnect();
        TreePath SelectedPath = jT_Hierarchy.getSelectionPath();
        if(SelectedPath == null)
            return;
        Object SelectedNode = SelectedPath.getLastPathComponent();
        Object ParentSelected = SelectedPath.getPathComponent(SelectedPath.getPathCount()-2);
        if(SelectedNode.toString().equalsIgnoreCase("Evidence") && !ParentSelected.toString().equalsIgnoreCase("World"))
        {
            ((JButton)((JPanel)RQform.getContentPane().getComponent(0)).getComponent(126)).setEnabled(true);
            Records.setModel(new javax.swing.table.DefaultTableModel(10, 52));
            jScrollPane2.setViewportView(Records);
            jTabbedPane1.addTab("Record View", jScrollPane2);

            DBQuery.configuration.setMission(ParentSelected.toString());
            Vector Evidence = DBQuery.runQuery("Select * from Evidence;");

            try
            {
                //A loop to convert vectors to string array
                Vector record = (Vector)(Evidence.get(0));
                Evid = new Object[Evidence.size()][record.size()];
                for(int x = 0; x < Evidence.size(); x++)
                {
                    record = (Vector)(Evidence.get(x));
                    for(int y = 0; y < record.size(); y++)
                    {
                        Evid[x][y] = record.get(y);
                    }
                }
            }catch(Exception ioe)
            {
                System.out.println("There are no evidence records.");
            }

            //do check to see if labels table exists and if not create one.
            Vector Labels = DBQuery.runQuery("Select * from Labels;");
            if(Labels.isEmpty())
            {
                System.out.println("Creating new labels table");

                String Fields = new String();
                Fields = "";
                for(int i = 1; i < 60; i++)
                {
                    Fields += "Field" + i + " VARCHAR(255)" + ", ";
                }
                Fields += "Field60 VARCHAR(255)";
                DBQuery.executeQuery("CREATE TABLE Labels (" + Fields + ");");
                String[] Values = new String[60];
                Values[0] = "Case_ID";
                Values[1] = "Hypothesis";
                Values[2] = "Belief";
                Values[3] = "Disbelief";
                for(int i = 4; i < 55; i++)
                {
                    Values[i] = "?Undefined?";
                }
                Values[54] = "Evidence_Type";
                Values[55] = "Info_ID";
                Values[56] = "Prepared";
                Values[57] = "Prep-Date";
                Values[58] = "Source";
                Values[59] = "Source";
                RQform.setLabels(Values);
                Values = RQform.getLabels();
                String Value = new String();
                for(int i = 0; i < 59; i++)
                {
                    Value += "\'" + Values[i] + "\', ";
                }
                Value += "\'" + "-Source" + "\'";
                DBQuery.executeQuery("INSERT INTO Labels VALUES (" + Value + ")");
                Labels = DBQuery.runQuery("Select * from Labels;");
            }
            
            Vector l = (Vector)(Labels.get(0));
            Label = new String[l.size()];
            int LabelCount = 0;
            for(int q = 0; q < l.size()-1; q++)
            {
                String Temp = l.get(q).toString().replace('[', ' ').replace(']', ' ').trim();
                if(Temp.equalsIgnoreCase(""))
                {
                    Label[LabelCount] = "?Undefined?";
                    LabelCount++;
                    continue;
                }

                if(Temp.charAt(0) != '+')
                {
                    Label[LabelCount] = Temp.substring(1).replace('_', ' ');
                    LabelCount++;
                }
            }
            Records.setModel(new javax.swing.table.DefaultTableModel(Evid, Label));
            
            for(int q = 0; q < 59; q++)
            {
                String Temp = l.get(q).toString().replace('[', ' ').replace(']', ' ').trim();
                if(Temp.equalsIgnoreCase(""))
                {
                    Label[q] = "?Undefined?";
                    continue;
                }
                Label[q] = Temp.substring(1).replace('_', ' ');
            }
            RQform.setLabels(Label);
            Records.setAutoResizeMode(Records.AUTO_RESIZE_OFF);
            jScrollPane2.setViewportView(Records);
            jTabbedPane1.addTab("Record View", jScrollPane2);
            jTabbedPane1.setSelectedIndex(jTabbedPane1.indexOfTab("Record View"));
        }else if(!ParentSelected.toString().equalsIgnoreCase("World"))
        {
            showBNE(SelectedNode.toString(), ParentSelected.toString());
        }
    }//GEN-LAST:event_jT_HierarchyMouseClicked

    private void jMI_ConfigureActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMI_ConfigureActionPerformed
        // Add your handling code here:
        ConfigDlg config = new ConfigDlg();
        config.setSize(700, 500);
        config.show();
    }//GEN-LAST:event_jMI_ConfigureActionPerformed
    
    /** Exit the Application */
    private void exitForm(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_exitForm

        Time.setTime(System.currentTimeMillis());
        jTA_Logger.setText(jTA_Logger.getText() + "\n\'User\' has logged off at " + Time.toLocaleString() + ".");

        System.exit(0);
    }//GEN-LAST:event_exitForm
    
    /**
     * @param args the command line arguments
     */
    public static void main(String args[]) {
        VKB_MainUI VKB = new VKB_MainUI();
        VKB.setSize(1058, 840);
        VKB.show();
    }
    
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JMenu jM_Edit;
    private javax.swing.JTree jT_Hierarchy;
    private javax.swing.JMenuItem jM_FileNewDomain;
    private javax.swing.JTextArea jTA_Logger;
    private javax.swing.JMenuItem jMI_Configure;
    private javax.swing.JMenuItem jM_FileExit;
    private javax.swing.JMenu jM_File;
    private javax.swing.JMenu jM_Zoom;
    private javax.swing.JMenuItem jM_ZoomOut;
    private javax.swing.JMenuItem jM_View_Refresh;
    private javax.swing.JMenu jM_View;
    private javax.swing.JTabbedPane jTabbedPane1;
    private javax.swing.JMenuItem jM_ZoomIn;
    private javax.swing.JMenuBar jMenuBar1;
    // End of variables declaration//GEN-END:variables

    public void changeView()
    {
        jTabbedPane1.addTab("Evidence Form", RQform.getContentPane());
    }
    
    public void fillTree()
    {
        DBConnect DBQuery = new DBConnect();
        Vector Domains = DBQuery.runQuery("Show databases;");
        DefaultMutableTreeNode outNode = new DefaultMutableTreeNode("World");
        DefaultMutableTreeNode parent = new DefaultMutableTreeNode("Domain"),
            fr_child = new DefaultMutableTreeNode("Story"), sd_child = new DefaultMutableTreeNode("Hypothesis");

        for(int i = 0; i < Domains.size(); i++)
        {
            String temp = Domains.get(i).toString().replace('[', ' ').replace(']', ' ').trim();
            if(temp.equalsIgnoreCase("test") || temp.equalsIgnoreCase("mysql"))
                continue;
            parent = new DefaultMutableTreeNode(temp);
            DBQuery.configuration.setMission(temp);
            Vector Stories = DBQuery.runQuery("Show tables;");
            for(int x = 0; x < Stories.size(); x++)
            {
                String temp2 = Stories.get(x).toString().replace('[', ' ').replace(']', ' ').trim();
                if(temp2.equalsIgnoreCase("Labels") || temp2.equalsIgnoreCase("BKLabels"))
                    continue;
                fr_child = new DefaultMutableTreeNode(temp2);
                parent.add(fr_child);
            }
            outNode.add(parent);
        }

        RQform.jBTN_OK_RQ_Form.setEnabled(false);

        jT_Hierarchy.setModel(new DefaultTreeModel(outNode));  //If I want direct access to the tree make it a global variable
                                                               //instead of dynamically creating it inside of the func call.
    }

    private void RecordsMouseClicked(java.awt.event.MouseEvent evt) {
        // Add your handling code here:
        String[] Temp = new String[Records.getColumnCount()];
        for(int x = 0; x < Records.getColumnCount(); x++)
        {
            try
            {
                Temp[x] = Records.getValueAt(Records.getSelectedRow(), x).toString().replace('_', ' ');
            }catch(Exception err)
            {
                Temp[x] = "?";
            }
        }
        RQform.setFieldValues(Temp);
        jTabbedPane1.addTab("Evidence Form", RQform.getContentPane());
        jTabbedPane1.setSelectedIndex(jTabbedPane1.indexOfTab("Evidence Form"));
        RQform.jBTN_OK_RQ_Form.setEnabled(true);
    }
    
    String g_Story = "";
    String g_Domain = "";
    public void showBNE(String Story, String Domain)
    {
        final String szStoryName = g_Story = Story;
        final String szDomainName = g_Domain = Domain;

        jTabbedPane1.addTab("Story View", BNEView.getContentPane());
        jTabbedPane1.setSelectedIndex(jTabbedPane1.indexOfTab("Story View"));
        BNEView.loadStoryFromKB(szStoryName, szDomainName);
        scale = BNEView.getMinimumScale();
    }
    
    private void DeleteDomain_ActionPerformed(java.awt.event.ActionEvent evt)
    {
        TreePath SelectedPath = jT_Hierarchy.getSelectionPath();
        jT_Hierarchy.collapsePath(SelectedPath);
        DBConnect DBQuery = new DBConnect();
        String SelectedNode = SelectedPath.getLastPathComponent().toString();
        System.out.println("Deleting Domain:" + SelectedNode);
        DBQuery.executeQuery("DROP DATABASE IF EXISTS " + SelectedNode + ";");
        DBQuery.configuration.setMission("mysql");

        Time.setTime(System.currentTimeMillis());
        jTA_Logger.setText(jTA_Logger.getText() + "\n\'User\' has Deleted the Domain " + SelectedNode + " at " + Time.toLocaleString() + ".");

        jT_Hierarchy.setSelectionPath(null);
        fillTree();
    }

    public void AddStory_ActionPerformed(java.awt.event.ActionEvent evt)
    {
        JLabel LabelName = new JLabel();
        LabelName.setText("Story Name:");
        JButton BTN_OK = new JButton();
        BTN_OK.setText("Create");
        BTN_OK.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                NewStory_OK_ActionPerformed(evt);
            }
        });
        JButton BTN_Cancel = new JButton();
        BTN_Cancel.setText("Cancel");
        BTN_Cancel.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                StoryName.show(false);
            }
        });
        StoryName.setTitle("New Story Name");
        StoryName.getContentPane().setLayout(null);
        StoryName.getContentPane().add(LabelName);
        LabelName.setBounds(2, 16, 85, 9);
        StoryName.getContentPane().add(InputField);
        InputField.setBounds(88, 12, 160, 20);
        StoryName.getContentPane().add(BTN_OK);
        BTN_OK.setBounds(30, 60, 90, 20);
        StoryName.getContentPane().add(BTN_Cancel);
        BTN_Cancel.setBounds(150, 60, 90, 20);

        Dimension screenSize = this.getSize();
        StoryName.setSize(300, 150);
        Dimension frameSize = StoryName.getSize();
        if (frameSize.height > screenSize.height) {
          frameSize.height = screenSize.height;
        }
        if (frameSize.width > screenSize.width) {
          frameSize.width = screenSize.width;
        }
        StoryName.setLocation(((screenSize.width - frameSize.width) / 2) + this.getLocationOnScreen().x, ((screenSize.height - frameSize.height) / 2) + this.getLocationOnScreen().y);
        StoryName.setModal(true);
        StoryName.show();
    }

    public void DeleteStory_ActionPerformed(java.awt.event.ActionEvent evt)
    {
        DBConnect DBQuery = new DBConnect();
        DBQuery.configuration.setMission(g_Domain);
        DBQuery.executeQuery("DROP TABLE " + g_Story + ";");
        fillTree();
    }

    public void SaveStory_ActionPerformed(java.awt.event.ActionEvent evt)
    {
        com.appliedminds.jam.AccessKBDlg accessKB = new com.appliedminds.jam.AccessKBDlg();
        accessKB.DrawableGraphToStory(BNEView.getGraphPanel().getDrawableGraph(), g_Story, g_Domain);
    }
    
    public void NewStory_OK_ActionPerformed(java.awt.event.ActionEvent evt)
    {
        String szStoryName = InputField.getText();
        TreePath SelectedPath = jT_Hierarchy.getSelectionPath();
        String SelectedNode = SelectedPath.getLastPathComponent().toString();
        boolean test = false;
        DBConnect DBQuery = new DBConnect();
        Vector Domains = DBQuery.runQuery("Show databases;");
        for(int i = 0; i < Domains.size(); i++)
        {
            if(SelectedNode.equalsIgnoreCase(Domains.get(i).toString().replace('[', ' ').replace(']', ' ').trim()))
            {
                test = true;
            }
        }
        
        if(test)
        {
            DBQuery.configuration.setMission(SelectedNode);
            DBQuery.executeQuery("CREATE TABLE " + szStoryName + " (LEVEL_LABELS VARCHAR(255), LINK_LABELS VARCHAR(255), PARENTS VARCHAR(255), B_WEIGHTS VARCHAR(255), D_WEIGHTS VARCHAR(255), BELIEF VARCHAR(255), DISBELIEF VARCHAR(255), HYPOTHESIS VARCHAR(255), LEVEL VARCHAR(255), PHRASE VARCHAR(255), THRESHOLD VARCHAR(255), CUTOFF VARCHAR(255));");
        
            Time.setTime(System.currentTimeMillis());
            jTA_Logger.setText(jTA_Logger.getText() + "\n\'User\' has created a new network in the Domain " + SelectedNode + " at " + Time.toLocaleString() + ".");

            StoryName.show(false);
            fillTree();
        }
        InputField.setText("");
    }
    
    int count = 0;
    public void selectLabel()
    {
        if(count == 0)
        {
            RQform.jL_Heading7.setBorder(null);
            RQform.jL_Field6.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field6.getText());
            RQform.jL_Field7.setBorder(null);
        }
        if(count == 1)
        {
            RQform.jL_Field6.setBorder(null);
            RQform.jL_Field7.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field7.getText());
            RQform.jL_Field8.setBorder(null);
        }
        if(count == 2)
        {
            RQform.jL_Field7.setBorder(null);
            RQform.jL_Field8.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field8.getText());
            RQform.jL_Field9.setBorder(null);
        }
        if(count == 3)
        {
            RQform.jL_Field8.setBorder(null);
            RQform.jL_Field9.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field9.getText());
            RQform.jL_Field10.setBorder(null);
        }
        if(count == 4)
        {
            RQform.jL_Field9.setBorder(null);
            RQform.jL_Field10.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field10.getText());
            RQform.jL_Field11.setBorder(null);
        }
        if(count == 5)
        {
            RQform.jL_Field10.setBorder(null);
            RQform.jL_Field11.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field11.getText());
            RQform.jL_Field12.setBorder(null);
        }
        if(count == 6)
        {
            RQform.jL_Field11.setBorder(null);
            RQform.jL_Field12.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field12.getText());
            RQform.jL_Field13.setBorder(null);
        }
        if(count == 7)
        {
            RQform.jL_Field12.setBorder(null);
            RQform.jL_Field13.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field13.getText());
            RQform.jL_Field14.setBorder(null);
        }
        if(count == 8)
        {
            RQform.jL_Field13.setBorder(null);
            RQform.jL_Field14.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field14.getText());
            RQform.jL_Field15.setBorder(null);
        }
        if(count == 9)
        {
            RQform.jL_Field14.setBorder(null);
            RQform.jL_Field15.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field15.getText());
            RQform.jL_Field16.setBorder(null);
        }
        if(count == 10)
        {
            RQform.jL_Field15.setBorder(null);
            RQform.jL_Field16.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field16.getText());
            RQform.jL_Field17.setBorder(null);
        }
        if(count == 11)
        {
            RQform.jL_Field16.setBorder(null);
            RQform.jL_Field17.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field17.getText());
            RQform.jL_Field18.setBorder(null);
        }
        if(count == 12)
        {
            RQform.jL_Field17.setBorder(null);
            RQform.jL_Field18.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field18.getText());
            RQform.jL_Field19.setBorder(null);
        }
        if(count == 13)
        {
            RQform.jL_Field18.setBorder(null);
            RQform.jL_Field19.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field19.getText());
            RQform.jL_Field20.setBorder(null);
        }
        if(count == 14)
        {
            RQform.jL_Field19.setBorder(null);
            RQform.jL_Field20.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field20.getText());
            RQform.jL_Field21.setBorder(null);
        }
        if(count == 15)
        {
            RQform.jL_Field20.setBorder(null);
            RQform.jL_Field21.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field21.getText());
            RQform.jL_Field22.setBorder(null);
        }
        if(count == 16)
        {
            RQform.jL_Field21.setBorder(null);
            RQform.jL_Field22.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field22.getText());
            RQform.jL_Field23.setBorder(null);
        }
        if(count == 17)
        {
            RQform.jL_Field22.setBorder(null);
            RQform.jL_Field23.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field23.getText());
            RQform.jL_Field24.setBorder(null);
        }
        if(count == 18)
        {
            RQform.jL_Field23.setBorder(null);
            RQform.jL_Field24.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field24.getText());
            RQform.jL_Field25.setBorder(null);
        }
        if(count == 19)
        {
            RQform.jL_Field24.setBorder(null);
            RQform.jL_Field25.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field25.getText());
            RQform.jL_Field26.setBorder(null);
        }
        if(count == 20)
        {
            RQform.jL_Field25.setBorder(null);
            RQform.jL_Field26.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field26.getText());
            RQform.jL_Field27.setBorder(null);
        }
        if(count == 21)
        {
            RQform.jL_Field26.setBorder(null);
            RQform.jL_Field27.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field27.getText());
            RQform.jL_Field28.setBorder(null);
        }
        if(count == 22)
        {
            RQform.jL_Field27.setBorder(null);
            RQform.jL_Field28.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field28.getText());
            RQform.jL_Field29.setBorder(null);
        }
        if(count == 23)
        {
            RQform.jL_Field28.setBorder(null);
            RQform.jL_Field29.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field29.getText());
            RQform.jL_Field30.setBorder(null);
        }
        if(count == 24)
        {
            RQform.jL_Field29.setBorder(null);
            RQform.jL_Field30.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field30.getText());
            RQform.jL_Field31.setBorder(null);
        }
        if(count == 25)
        {
            RQform.jL_Field30.setBorder(null);
            RQform.jL_Field31.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field31.getText());
            RQform.jL_Field32.setBorder(null);
        }
        if(count == 26)
        {
            RQform.jL_Field31.setBorder(null);
            RQform.jL_Field32.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field32.getText());
            RQform.jL_Field33.setBorder(null);
        }
        if(count == 27)
        {
            RQform.jL_Field32.setBorder(null);
            RQform.jL_Field33.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field33.getText());
            RQform.jL_Field34.setBorder(null);
        }
        if(count == 28)
        {
            RQform.jL_Field33.setBorder(null);
            RQform.jL_Field34.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field34.getText());
            RQform.jL_Field35.setBorder(null);
        }
        if(count == 29)
        {
            RQform.jL_Field34.setBorder(null);
            RQform.jL_Field35.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field35.getText());
            RQform.jL_Field36.setBorder(null);
        }
        if(count == 30)
        {
            RQform.jL_Field35.setBorder(null);
            RQform.jL_Field36.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field36.getText());
            RQform.jL_Field37.setBorder(null);
        }
        if(count == 31)
        {
            RQform.jL_Field36.setBorder(null);
            RQform.jL_Field37.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field37.getText());
            RQform.jL_Field38.setBorder(null);
        }
        if(count == 32)
        {
            RQform.jL_Field37.setBorder(null);
            RQform.jL_Field38.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field38.getText());
            RQform.jL_Field39.setBorder(null);
        }
        if(count == 33)
        {
            RQform.jL_Field38.setBorder(null);
            RQform.jL_Field39.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field39.getText());
            RQform.jL_Field40.setBorder(null);
        }
        if(count == 34)
        {
            RQform.jL_Field39.setBorder(null);
            RQform.jL_Field40.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field40.getText());
            RQform.jL_Field41.setBorder(null);
        }
        if(count == 35)
        {
            RQform.jL_Field40.setBorder(null);
            RQform.jL_Field41.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field41.getText());
            RQform.jL_Field42.setBorder(null);
        }
        if(count == 36)
        {
            RQform.jL_Field41.setBorder(null);
            RQform.jL_Field42.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field42.getText());
            RQform.jL_Field43.setBorder(null);
        }
        if(count == 37)
        {
            RQform.jL_Field42.setBorder(null);
            RQform.jL_Field43.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field43.getText());
            RQform.jL_Field44.setBorder(null);
        }
        if(count == 38)
        {
            RQform.jL_Field43.setBorder(null);
            RQform.jL_Field44.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field44.getText());
            RQform.jL_Field45.setBorder(null);
        }
        if(count == 39)
        {
            RQform.jL_Field44.setBorder(null);
            RQform.jL_Field45.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field45.getText());
            RQform.jL_Field46.setBorder(null);
        }
        if(count == 40)
        {
            RQform.jL_Field45.setBorder(null);
            RQform.jL_Field46.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field46.getText());
            RQform.jL_Field47.setBorder(null);
        }
        if(count == 41)
        {
            RQform.jL_Field46.setBorder(null);
            RQform.jL_Field47.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field47.getText());
            RQform.jL_Field48.setBorder(null);
        }
        if(count == 42)
        {
            RQform.jL_Field47.setBorder(null);
            RQform.jL_Field48.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Field48.getText());
            RQform.jL_Heading1.setBorder(null);
        }
        if(count == 43)
        {
            RQform.jL_Field48.setBorder(null);
            RQform.jL_Heading1.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Heading1.getText());
            RQform.jL_Heading2.setBorder(null);
        }
        if(count == 44)
        {
            RQform.jL_Heading1.setBorder(null);
            RQform.jL_Heading2.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Heading2.getText());
            RQform.jL_Heading3.setBorder(null);
        }
        if(count == 45)
        {
            RQform.jL_Heading2.setBorder(null);
            RQform.jL_Heading3.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Heading3.getText());
            RQform.jL_Heading4.setBorder(null);
        }
        if(count == 46)
        {
            RQform.jL_Heading3.setBorder(null);
            RQform.jL_Heading4.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Heading4.getText());
            RQform.jL_Heading5.setBorder(null);
        }
        if(count == 47)
        {
            RQform.jL_Heading4.setBorder(null);
            RQform.jL_Heading5.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Heading5.getText());
            RQform.jL_Heading6.setBorder(null);
        }
        if(count == 48)
        {
            RQform.jL_Heading5.setBorder(null);
            RQform.jL_Heading6.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Heading6.getText());
            RQform.jL_Heading7.setBorder(null);
        }
        if(count == 49)
        {
            RQform.jL_Heading6.setBorder(null);
            RQform.jL_Heading7.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 2));
            InputField.setText(RQform.jL_Heading7.getText());
            RQform.jL_Field6.setBorder(null);
        }
    }
    
    public void RQFormEditLabel_ActionPerformed(java.awt.event.ActionEvent evt)
    {
        final JLabel jL_Label = new JLabel("Label");
        JButton jBTN_ChangeLabel = new JButton();
        jBTN_ChangeLabel.setText("Next >>");
        jBTN_ChangeLabel.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                if(count == 49)
                    count = -1;
                count++;
                selectLabel();
            }
        });
        JButton jBTN_ChangeLabelBack = new JButton();
        jBTN_ChangeLabelBack.setText("<< Back");
        jBTN_ChangeLabelBack.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                if(count == 0)
                    count = 50;
                count--;
                selectLabel();
            }
        });

        JButton jBTN_SetLabel = new JButton();
        jBTN_SetLabel.setText("Apply");
        jBTN_SetLabel.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                if(count == 0)
                {
                    RQform.jL_Field6.setText(InputField.getText());
                }
                if(count == 1)
                {
                    RQform.jL_Field7.setText(InputField.getText());
                }
                if(count == 2)
                {
                    RQform.jL_Field8.setText(InputField.getText());
                }
                if(count == 3)
                {
                    RQform.jL_Field9.setText(InputField.getText());
                }
                if(count == 4)
                {
                    RQform.jL_Field10.setText(InputField.getText());
                }
                if(count == 5)
                {
                    RQform.jL_Field11.setText(InputField.getText());
                }
                if(count == 6)
                {
                    RQform.jL_Field12.setText(InputField.getText());
                }
                if(count == 7)
                {
                    RQform.jL_Field13.setText(InputField.getText());
                }
                if(count == 8)
                {
                    RQform.jL_Field14.setText(InputField.getText());
                }
                if(count == 9)
                {
                    RQform.jL_Field15.setText(InputField.getText());
                }
                if(count == 10)
                {
                    RQform.jL_Field16.setText(InputField.getText());
                }
                if(count == 11)
                {
                    RQform.jL_Field17.setText(InputField.getText());
                }
                if(count == 12)
                {
                    RQform.jL_Field18.setText(InputField.getText());
                }
                if(count == 13)
                {
                    RQform.jL_Field19.setText(InputField.getText());
                }
                if(count == 14)
                {
                    RQform.jL_Field20.setText(InputField.getText());
                }
                if(count == 15)
                {
                    RQform.jL_Field21.setText(InputField.getText());
                }
                if(count == 16)
                {
                    RQform.jL_Field22.setText(InputField.getText());
                }
                if(count == 17)
                {
                    RQform.jL_Field23.setText(InputField.getText());
                }
                if(count == 18)
                {
                    RQform.jL_Field24.setText(InputField.getText());
                }
                if(count == 19)
                {
                    RQform.jL_Field25.setText(InputField.getText());
                }
                if(count == 20)
                {
                    RQform.jL_Field26.setText(InputField.getText());
                }
                if(count == 21)
                {
                    RQform.jL_Field27.setText(InputField.getText());
                }
                if(count == 22)
                {
                    RQform.jL_Field28.setText(InputField.getText());
                }
                if(count == 23)
                {
                    RQform.jL_Field29.setText(InputField.getText());
                }
                if(count == 24)
                {
                    RQform.jL_Field30.setText(InputField.getText());
                }
                if(count == 25)
                {
                    RQform.jL_Field31.setText(InputField.getText());
                }
                if(count == 26)
                {
                    RQform.jL_Field32.setText(InputField.getText());
                }
                if(count == 27)
                {
                    RQform.jL_Field33.setText(InputField.getText());
                }
                if(count == 28)
                {
                    RQform.jL_Field34.setText(InputField.getText());
                }
                if(count == 29)
                {
                    RQform.jL_Field35.setText(InputField.getText());
                }
                if(count == 30)
                {
                    RQform.jL_Field36.setText(InputField.getText());
                }
                if(count == 31)
                {
                    RQform.jL_Field37.setText(InputField.getText());
                }
                if(count == 32)
                {
                    RQform.jL_Field38.setText(InputField.getText());
                }
                if(count == 33)
                {
                    RQform.jL_Field39.setText(InputField.getText());
                }
                if(count == 34)
                {
                    RQform.jL_Field40.setText(InputField.getText());
                }
                if(count == 35)
                {
                    RQform.jL_Field41.setText(InputField.getText());
                }
                if(count == 36)
                {
                    RQform.jL_Field42.setText(InputField.getText());
                }
                if(count == 37)
                {
                    RQform.jL_Field43.setText(InputField.getText());
                }
                if(count == 38)
                {
                    RQform.jL_Field44.setText(InputField.getText());
                }
                if(count == 39)
                {
                    RQform.jL_Field45.setText(InputField.getText());
                }
                if(count == 40)
                {
                    RQform.jL_Field46.setText(InputField.getText());
                }
                if(count == 41)
                {
                    RQform.jL_Field47.setText(InputField.getText());
                }
                if(count == 42)
                {
                    RQform.jL_Field48.setText(InputField.getText());
                }
                if(count == 43)
                {
                    RQform.jL_Heading1.setText(InputField.getText());
                }
                if(count == 44)
                {
                    RQform.jL_Heading2.setText(InputField.getText());
                }
                if(count == 45)
                {
                    RQform.jL_Heading3.setText(InputField.getText());
                }
                if(count == 46)
                {
                    RQform.jL_Heading4.setText(InputField.getText());
                }
                if(count == 47)
                {
                    RQform.jL_Heading5.setText(InputField.getText());
                }
                if(count == 48)
                {
                    RQform.jL_Heading6.setText(InputField.getText());
                }
                if(count == 49)
                {
                    RQform.jL_Heading7.setText(InputField.getText());
                }
            }
        });

        JButton jBTN_Close = new JButton();
        jBTN_Close.setText("Close");
        jBTN_Close.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                    RQform.jL_Heading7.setBorder(null);
                    RQform.jL_Field6.setBorder(null);
                    RQform.jL_Field7.setBorder(null);
                    RQform.jL_Field8.setBorder(null);
                    RQform.jL_Field9.setBorder(null);
                    RQform.jL_Field10.setBorder(null);
                    RQform.jL_Field11.setBorder(null);
                    RQform.jL_Field12.setBorder(null);
                    RQform.jL_Field13.setBorder(null);
                    RQform.jL_Field14.setBorder(null);
                    RQform.jL_Field15.setBorder(null);
                    RQform.jL_Field16.setBorder(null);
                    RQform.jL_Field17.setBorder(null);
                    RQform.jL_Field18.setBorder(null);
                    RQform.jL_Field19.setBorder(null);
                    RQform.jL_Field20.setBorder(null);
                    RQform.jL_Field21.setBorder(null);
                    RQform.jL_Field22.setBorder(null);
                    RQform.jL_Field23.setBorder(null);
                    RQform.jL_Field24.setBorder(null);
                    RQform.jL_Field25.setBorder(null);
                    RQform.jL_Field26.setBorder(null);
                    RQform.jL_Field27.setBorder(null);
                    RQform.jL_Field28.setBorder(null);
                    RQform.jL_Field29.setBorder(null);
                    RQform.jL_Field30.setBorder(null);
                    RQform.jL_Field31.setBorder(null);
                    RQform.jL_Field32.setBorder(null);
                    RQform.jL_Field33.setBorder(null);
                    RQform.jL_Field34.setBorder(null);
                    RQform.jL_Field35.setBorder(null);
                    RQform.jL_Field36.setBorder(null);
                    RQform.jL_Field37.setBorder(null);
                    RQform.jL_Field38.setBorder(null);
                    RQform.jL_Field39.setBorder(null);
                    RQform.jL_Field40.setBorder(null);
                    RQform.jL_Field41.setBorder(null);
                    RQform.jL_Field42.setBorder(null);
                    RQform.jL_Field43.setBorder(null);
                    RQform.jL_Field44.setBorder(null);
                    RQform.jL_Field45.setBorder(null);
                    RQform.jL_Field46.setBorder(null);
                    RQform.jL_Field47.setBorder(null);
                    RQform.jL_Field48.setBorder(null);
                    RQform.jL_Heading1.setBorder(null);
                    RQform.jL_Heading2.setBorder(null);
                    RQform.jL_Heading3.setBorder(null);
                    RQform.jL_Heading4.setBorder(null);
                    RQform.jL_Heading5.setBorder(null);
                    RQform.jL_Heading6.setBorder(null);
                String[] Labels = new String[60];
                DBConnect DBQuery = new DBConnect();
                Labels = RQform.getLabels();
                String Value = new String();
                for(int i = 0; i < 59; i++)
                {
                    Value += "\'" + Labels[i] + "\', ";
                }
                Value += "\'" + "-Source" + "\'";
                DBQuery.executeQuery("DELETE FROM Labels;");
                DBQuery.executeQuery("INSERT INTO Labels VALUES (" + Value + ");");
                count = 0;
                InputField.setText("");
                LabelEditor.show(false);

                Time.setTime(System.currentTimeMillis());
                jTA_Logger.setText(jTA_Logger.getText() + "\n\'User\' has modified the labels in the Domain " + DBQuery.configuration.getMission() + " at " + Time.toLocaleString() + ".");
            }
        });

        LabelEditor.getContentPane().setLayout(null);
        LabelEditor.setSize(300, 145);
        jL_Label.setBounds(20, 12, 120, 20);
        LabelEditor.getContentPane().add(jL_Label);
        InputField.setBounds(88, 12, 180, 20);
        LabelEditor.getContentPane().add(InputField);
        jBTN_ChangeLabelBack.setBounds(20, 60, 80, 20);
        jBTN_ChangeLabel.setBounds(110, 60, 80, 20);
        jBTN_SetLabel.setBounds(200, 60, 80, 20);
        LabelEditor.getContentPane().add(jBTN_ChangeLabel);
        LabelEditor.getContentPane().add(jBTN_ChangeLabelBack);
        LabelEditor.getContentPane().add(jBTN_SetLabel);
        jBTN_Close.setBounds(200, 90, 80, 20);
        LabelEditor.getContentPane().add(jBTN_Close);
        selectLabel();
        LabelEditor.setModal(true);
        LabelEditor.show();
    }
    
    public void RQFormSaveNewEvidence_ActionPerformed(java.awt.event.ActionEvent evt)
    {
        DBConnect DBQuery = new DBConnect();
        String[] Evidences = RQform.getFieldValues();
        String Evids = new String();
        Evids = "\'\', ";
        Evidences[0] = "";
        for(int i = 1; i < 51; i++)
        {
            String temp = Evidences[i];
            if(temp == null || temp.equalsIgnoreCase("") || temp.equalsIgnoreCase(" "))
                temp = "?";
            Evids += "\'" + temp + "\'" + ", ";
        }
        String temp = Evidences[51];
        if(temp == null || temp.equalsIgnoreCase("") || temp.equalsIgnoreCase(" "))
            temp = "?";
        Evids += "\'" + temp + "\'";
        DBQuery.executeQuery("INSERT INTO Evidence Values(" + Evids + ");");
        UpdateRecordsTable();
        
        Time.setTime(System.currentTimeMillis());
        jTA_Logger.setText(jTA_Logger.getText() + "\n\'User\' has created new evidence in the Domain " + DBQuery.configuration.getMission() + " at " + Time.toLocaleString() + ".");

    }
    
    public void RQFormSaveModifiedEvidence_ActionPerformed(java.awt.event.ActionEvent evt)
    {
        DBConnect DBQuery = new DBConnect();
        //Bassed on the caseID do a DELETE on row then create a new record.
        String[] Evidence = RQform.getFieldValues();
        DBQuery.executeQuery("DELETE FROM Evidence WHERE Case_ID=" + Evidence[0] + ";");
        String Evids = new String();
        Evids = "\'\', ";
        Evidence[0] = "";
        for(int i = 1; i < 51; i++)
        {
            String temp = Evidence[i];
            if(temp == null || temp.equalsIgnoreCase("") || temp.equalsIgnoreCase(" "))
                temp = "?";
            Evids += "\'" + temp + "\'" + ", ";
        }
        String temp = Evidence[51];
        if(temp == null || temp.equalsIgnoreCase("") || temp.equalsIgnoreCase(" "))
            temp = "?";
        Evids += "\'" + temp + "\'";
        DBQuery.executeQuery("INSERT INTO Evidence Values(" + Evids + ");");
        UpdateRecordsTable();

        Time.setTime(System.currentTimeMillis());
        jTA_Logger.setText(jTA_Logger.getText() + "\n\'User\' has modified the evidence in the Domain " + DBQuery.configuration.getMission() + " at " + Time.toLocaleString() + ".");
    }

    private Object[][] getEvidence() {
        DBConnect DBQuery = new DBConnect();
        Object[][] Evid = new Object[0][0];


        Vector Evidence = DBQuery.runQuery("Select * from Evidence;");

        try
        {
            //A loop to convert vectors to string array
            Vector record = (Vector)(Evidence.get(0));
            Evid = new Object[Evidence.size()][record.size()];
            for(int x = 0; x < Evidence.size(); x++)
            {
                record = (Vector)(Evidence.get(x));
                for(int y = 0; y < record.size(); y++)
                {
                    Evid[x][y] = record.get(y);
                }
            }
	    return Evid;
        }catch(Exception ioe)
        {
            System.out.println("There are no evidence records.");
        }
	return null;
    }

    private String[] getLabels() {
        DBConnect DBQuery = new DBConnect();
        String[] Label;
        //do check to see if labels table exists and if not create one.
        Vector Labels = DBQuery.runQuery("Select * from Labels;");
        Vector l = (Vector)(Labels.get(0));
        Label = new String[l.size()];
        int LabelCount = 0;
        for(int q = 0; q < l.size()-1; q++)
        {
            String Temp = l.get(q).toString().replace('[', ' ').replace(']', ' ').trim();
            if(Temp.equalsIgnoreCase(""))
            {
                Label[LabelCount] = "?Undefined?";
                LabelCount++;
                continue;
            }

            if(Temp.charAt(0) != '+')
            {
                Label[LabelCount] = Temp.substring(1).replace('_', ' ');
                LabelCount++;
            }
        }
	return Label;
    }

    public void UpdateRecordsTable()
    {
        Records.setModel(new javax.swing.table.DefaultTableModel(getEvidence(), getLabels()));
    }

class PopupListener extends MouseAdapter {
        JPopupMenu popup;

        PopupListener(JPopupMenu popupMenu) {
            popup = popupMenu;
        }

        public void mousePressed(MouseEvent e) {
            maybeShowPopup(e);
        }

        public void mouseReleased(MouseEvent e) {
            maybeShowPopup(e);
        }

        private void maybeShowPopup(MouseEvent e) {
            if (e.isPopupTrigger())
            { 
                jT_Hierarchy.setSelectionPath(jT_Hierarchy.getClosestPathForLocation(e.getX(), e.getY()));
                TreePath SelectedPath = jT_Hierarchy.getSelectionPath();
                Object SelectedNode = SelectedPath.getLastPathComponent();
                Object ParentSelected = SelectedPath.getPathComponent(SelectedPath.getPathCount()-2);
                Object ParentSelected1 = null;
                if(!ParentSelected.toString().equalsIgnoreCase("World"))
                    ParentSelected1 = SelectedPath.getPathComponent(SelectedPath.getPathCount()-3);
                if(ParentSelected.toString().equalsIgnoreCase("World"))
                {
                    DS_menuItem.setEnabled(false);
                    SS_menuItem.setEnabled(false);
                    DD_menuItem.setEnabled(true);
                    NS_menuItem.setEnabled(true);
                    popup.show(e.getComponent(),
                           e.getX(), e.getY());
                }
                else if(ParentSelected1.toString().equalsIgnoreCase("World") && !SelectedNode.toString().equalsIgnoreCase("Evidence"))
                {
                    DS_menuItem.setEnabled(true);
                    SS_menuItem.setEnabled(true);
                    DD_menuItem.setEnabled(false);
                    NS_menuItem.setEnabled(false);
                    popup.show(e.getComponent(),
                           e.getX(), e.getY());
                }
            }
        }
    }

    class TreeIconRenderer extends DefaultTreeCellRenderer 
    {
        ImageIcon AriesIcon;
        ImageIcon EvidenceIcon;
        ImageIcon StructureIcon;
        ImageIcon DomainIcon;

        public TreeIconRenderer() 
        {
            AriesIcon = new ImageIcon("images/ARIES.gif");
            EvidenceIcon = new ImageIcon("images/msdn.gif");
            StructureIcon = new ImageIcon("images/Structure.gif");
            DomainIcon = new ImageIcon("images/Domain.gif");
        }

        public Component getTreeCellRendererComponent(
                            JTree tree,
                            Object value,
                            boolean sel,
                            boolean expanded,
                            boolean leaf,
                            int row,
                            boolean hasFocus) {

            super.getTreeCellRendererComponent(
                            tree, value, sel,
                            expanded, leaf, row,
                            hasFocus);
            if(!leaf){
                setIcon(DomainIcon);
                setToolTipText("This is a Domain.");
            }
            if(leaf){
                setIcon(StructureIcon);
                setToolTipText("This contains the Hierarchical Structure.");
            }
            if (value.toString().equalsIgnoreCase("Evidence")) {
                setIcon(EvidenceIcon);
                setToolTipText("This contains Evidence.");
            } else if (value.toString().equalsIgnoreCase("World"))
            {
                setIcon(AriesIcon);
                setToolTipText("This is the highest level.");
            }

            return this;
        }
    }
}