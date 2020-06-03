package wekavis;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

/**
 * <p>Title: </p>
 * <p>Description: </p>
 * <p>Copyright: Copyright (c) 2002</p>
 * <p>Company: </p>
 * @author unascribed
 * @version 1.0
 */

public class MainFrame extends JFrame {

  private JPanel contentPane;
  private JMenuBar jMenuBar1 = new JMenuBar();
  private JMenu jMenuFile = new JMenu();
  private JMenuItem jMenuFileExit = new JMenuItem();
  private JMenu jMenuHelp = new JMenu();
  private JMenuItem jMenuHelpAbout = new JMenuItem();
  private JMenuItem jMenuFileOpen = new JMenuItem();
  private JTabbedPane jTabbedPane1 = new JTabbedPane();
  private JPanel jPanelCluster = new JPanel();
  private JPanel jPanelRuleTree = new JPanel();
  private BorderLayout borderLayout1 = new BorderLayout();
  private BorderLayout borderLayout2 = new BorderLayout();
  public ClusterViz ClustPanel = new ClusterViz();
  public ClassViz RuleTreePanel = new ClassViz();
  private JMenuItem jMenuFileKBOpen = new JMenuItem();
  private JMenu jMenu1 = new JMenu();
  private JMenuItem jMenuEditConfig = new JMenuItem();

  //Construct the frame
  public MainFrame()
  {
    enableEvents(AWTEvent.WINDOW_EVENT_MASK);
    try
    {
      jbInit();
    }
    catch(Exception e)
    {
      e.printStackTrace();
    }
  }

  //Component initialization
  private void jbInit() throws Exception
  {
    //setIconImage(Toolkit.getDefaultToolkit().createImage(MainFrame.class.getResource("[Your Icon]")));
    contentPane = (JPanel) this.getContentPane();
    contentPane.setLayout(null);
    this.setSize(new Dimension(800, 745));
    this.setTitle("Weka Visualization");
    jMenuFile.setText("File");
    jMenuFileExit.setText("Exit");
    jMenuFileExit.addActionListener(new ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        jMenuFileExit_actionPerformed(e);
      }
    });
    jMenuHelp.setText("Help");
    jMenuHelpAbout.setText("About");
    jMenuHelpAbout.addActionListener(new ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        jMenuHelpAbout_actionPerformed(e);
      }
    });
    jMenuFileOpen.setText("Open From File");
    jMenuFileOpen.addActionListener(new java.awt.event.ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        jMenuFileOpen_actionPerformed(e);
      }
    });
    contentPane.setPreferredSize(new Dimension(800, 700));
    jTabbedPane1.setBounds(new Rectangle(6, 7, 784, 684));
    jPanelCluster.setLayout(borderLayout1);
    jPanelRuleTree.setLayout(borderLayout2);
    jMenuFileKBOpen.setText("Open From KB");
    jMenuFileKBOpen.addActionListener(new java.awt.event.ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        jMenuFileKBOpen_actionPerformed(e);
      }
    });
    jMenu1.setText("Edit");
    jMenuEditConfig.setText("Configuration");
    jMenuEditConfig.addActionListener(new java.awt.event.ActionListener()
    {
      public void actionPerformed(ActionEvent e)
      {
        jMenuEditConfig_actionPerformed(e);
      }
    });
    jMenuFile.add(jMenuFileOpen);
    jMenuFile.add(jMenuFileKBOpen);
    jMenuFile.add(jMenuFileExit);
    jMenuHelp.add(jMenuHelpAbout);
    jMenuBar1.add(jMenuFile);
    jMenuBar1.add(jMenu1);
    jMenuBar1.add(jMenuHelp);
    ButtonGroup group = new ButtonGroup();
    contentPane.add(jTabbedPane1, null);
    jTabbedPane1.add(jPanelCluster,   "Clusterer");
    jTabbedPane1.add(jPanelRuleTree,   "Rule Tree");
    this.setJMenuBar(jMenuBar1);
    jPanelCluster.add(ClustPanel, BorderLayout.CENTER);
    jPanelRuleTree.add(RuleTreePanel, BorderLayout.CENTER);
    jMenu1.add(jMenuEditConfig);
    RuleTreePanel.m_setClassifier();
  }

  //File | Exit action performed
  public void jMenuFileExit_actionPerformed(ActionEvent e)
  {
    System.exit(0);
  }

  //Help | About action performed
  public void jMenuHelpAbout_actionPerformed(ActionEvent e)
  {
  }

  //Overridden so we can exit when window is closed
  protected void processWindowEvent(WindowEvent e)
  {
    super.processWindowEvent(e);
    if (e.getID() == WindowEvent.WINDOW_CLOSING)
    {
      jMenuFileExit_actionPerformed(null);
    }
  }

  void jMenuEditConfig_actionPerformed(ActionEvent e)
  {
    ConfigDlg dlg = new ConfigDlg();
    dlg.setSize(565, 490);
    dlg.show();
  }

  // Loads an ARFF file
  void jMenuFileOpen_actionPerformed(ActionEvent e)
  {
    JFileChooser OpenDlg = new JFileChooser();
    OpenDlg.showOpenDialog(this);
    try
    {
      java.io.FileReader inFile = new java.io.FileReader(OpenDlg.getSelectedFile().toString());
      java.io.BufferedReader buffFile = new java.io.BufferedReader(inFile);
      weka.core.Instances instances = new weka.core.Instances(buffFile);

      ClustPanel.setInstances(instances);
      RuleTreePanel.setInstances(instances);

      ClustPanel.m_start();
      RuleTreePanel.m_start();

    }catch(Exception ioe)
    {
      ioe.printStackTrace();
    }
  }

  void jMenuFileKBOpen_actionPerformed(ActionEvent e)
  {
    Convert_For_Weka CFW = new Convert_For_Weka();
    try
    {
      CFW.createCSVFile("Select * from Evidence;", "Data.tmp");
      weka.core.Instances instances = CFW.readCSVFile("Data.tmp");

      ClustPanel.setInstances(instances);
      RuleTreePanel.setInstances(instances);

      ClustPanel.m_start();
      RuleTreePanel.m_start();

    }catch(Exception ioe)
    {
      System.out.println("it crashed.");
      ioe.printStackTrace();
    }
  }
}