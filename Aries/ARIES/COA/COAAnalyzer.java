/*
 * COAAnalyzer.java
 *
 * Created on March 4, 2004, 11:06 AM
 */

package COA;

import java.util.*;
import java.net.*;
import java.io.*;
import java.text.*;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.AdjustmentEvent;
import java.awt.event.AdjustmentListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.TitledBorder;
import javax.swing.border.EtchedBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import BNE_View.*;
import com.appliedminds.martini.*;
import com.appliedminds.martini.io.*;
import com.appliedminds.martini.layout.*;
import com.appliedminds.martinix.greenpill.*;
import com.appliedminds.martinix.fader.*;
import com.appliedminds.martinix.ui.*;
import com.appliedminds.hmv.*;
import royere.cwi.structure.*;
import com.appliedminds.jam.*;
/**
 *
 * @author  Dennis R. Ellis
 */
public class COAAnalyzer extends JFrame {

  public COAAnalyzer()
  {
          Color           ColorBack;
    final int             slidervals[]   = { 100, 100, 30, 30, 50 };
    final String          borderTitles[] = { "Similarity",
                                             "Achieved",
                                             "rEA",
                                             "# Pairs",
                                             "Nuclear" };
    final String          sliderNames[]  = { "sliderA", "sliderB", "sliderC",
                                             "sliderD", "sliderE" };
/*------------------------------------------------------------------------------*/                                              
    initFrame();   
    //
    optimizer = new COAOptimizer();
/*
**   Set some preferences
*/   
    ColorBack = Color.lightGray;
/*
    setSize(800,700);
    getContentPane().setBackground(ColorBack);
    getContentPane().setLayout(new FlowLayout());
*/
/*
**   Define the control panel
*/ 
    FlowLayout lo = new FlowLayout(FlowLayout.LEFT);
    lo.setVgap(5);
    JPanel controlArea = new JPanel(lo);
    controlArea.setPreferredSize(new Dimension(910, 95));
    controlArea.setBackground(ColorBack);
    // 
    //   Put the mission area in
    //
    missionArea = new JTextArea("MISSION:");
    missionArea.setPreferredSize(new Dimension(250, 90));
    missionArea.setBackground(ColorBack);
    missionArea.setForeground(Color.yellow);
    missionArea.setFont(fb);
    missionArea.setBorder(BorderFactory.createEtchedBorder());
    controlArea.add(missionArea);
    //
    //   Put the influence area in
    //
    JPanel influencePane = new JPanel(new GridLayout(1,5));
    influencePane.setPreferredSize(new Dimension(255, 90));
    influencePane.setBackground(ColorBack);
    influencePane.setFont(sb);
    //
    slider = new JSlider[5];
    for (i=0; i<5; i++) {
        slider[i] = new JSlider(JSlider.VERTICAL);
        slider[i].setName(sliderNames[i]);
        slider[i].setFont(sb);
        slider[i].setPreferredSize(new Dimension(50,90));
        slider[i].setBackground(ColorBack);
        slider[i].setMajorTickSpacing(20);
        slider[i].setMinorTickSpacing(5);
        slider[i].setPaintTicks(true);
        slider[i].setValue(slidervals[i]);
        //slider[i].setPaintLabels(true);
        Border loweredetched = BorderFactory.createEtchedBorder(EtchedBorder.LOWERED);
        compound = BorderFactory.createTitledBorder(loweredetched, borderTitles[i],
                   TitledBorder.LEFT, TitledBorder.ABOVE_TOP, sb, Color.red);
        slider[i].setBorder(compound);

        slider[i].addChangeListener (new ChangeListener() {
          public void stateChanged(ChangeEvent e) {
            int islider = 0;
            JSlider source = (JSlider)e.getSource();
            if (source.getValueIsAdjusting() != true) {
              String name = source.getName();
              value = source.getValue();
              for (i=0; i<5; i++) {
                if (name.equals(sliderNames[i]))
                  islider = i;
              }
              //
              //   Check for Nuclear or Conventional COAs
              //
              if (islider == S_Nuclear) {
                if (value > 75) {
                  try {
                     readCOAs(NuclearCOAs);
                  }
                  catch(Exception rte) { }
                } else {
                  try {
                     readCOAs(DefaultCOAs);
                  }
                  catch(Exception rte) { }                    
                }
              }
              //
              //   Determine Score
              //
              findFlag = 1;
              findCOAValues(findFlag);
            }
          }
        });
      
        influencePane.add(slider[i]);        
    }
    //
    controlArea.add(influencePane);
    //
    //   Put the history log area in
    //
    String logText = "-10Feb04: Risk Lowered by Gen. Howitzer\n" +
        "   (No explanation given)\n";
    logText = logText + "-06Mar04: rEA Raised by Gen. Bumpers\n" +
        "   (More assets available)\n";
    logText = logText + "-22Mar04: # Pairs Raised by Adm. Bird\n" +
        "   (More targets in kill list)";
    JTextArea loggerArea = new JTextArea(logText);
    JScrollPane loggerPane = new JScrollPane(loggerArea);    
    loggerPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
    loggerPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
    loggerPane.setPreferredSize(new Dimension(250, 90));
    loggerPane.setBorder(BorderFactory.createEtchedBorder());
    controlArea.add(loggerPane);
    //
    String scoreText = " ";
    scoreArea = new JTextArea(scoreText);
    scoreArea.setForeground(Color.blue);
    JScrollPane scorePane = new JScrollPane(scoreArea);
    scorePane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
    scorePane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
    scorePane.setPreferredSize(new Dimension(120, 90));
    scorePane.setBorder(BorderFactory.createEtchedBorder());
    controlArea.add(scorePane);
    //
    //   Put the control area into the frame
    //
    getContentPane().add(controlArea);
    controlArea.setSize(910,95);
    controlArea.setLocation(0,0);
    //
    bne.initGAppFrame(false);
    bne.setSize(910, 600);
    bne.setLocation(0, 95);
    bne.show();
    bne.hide();
    //
    jp_Graph.setSize(1000, 600);
    jp_Graph.add(bne.getContentPane());
/*
**   Read the pre-defined COAs
*/
    try {
      COAGraph = readCOAs(DefaultCOAs);
    } 
    catch(Exception e) {
      e.printStackTrace();
    }
    /*
    try {
      //bne.loadStoryFromKB("IW_DCI", "IW_DCI");
      bne.loadStory(COAGraph, "CND", "CND");
    }
    catch(Exception e) { e.printStackTrace(); }
    /*
    _box = new PanAndScanPanel(100, 100, jp_Graph,
               new Color(206, 255, 238), new Color(0xFE, 0x86, 0x3B));
    */
    findCOAValues(0);
    /*
    db.SubGraphToStory(COAGraph, "DETEST", "DETEST",
                       TopNodes[bestCOA], TopNodes[bestCOA]-2);
    */
  }
  
  public DrawableNode getDrawableNodeById(String targetID)
  {
    for (NodeIterator itr=COAGraph.nodesIterator(); itr.hasNext(); ) {
      DrawableNode node = (DrawableNode) itr.next();
      if (targetID.equals(node.getProperty("id"))) {
        System.out.println("  Found Node ID " + node.getProperty("id"));
        return (node);
      }     
    }
    //   
    return (null);
  }

  public DrawableGraph readCOAs(String filename) throws Exception
  {
      int             fromNode = 0;
      double          dBranch = -1.0;
      String          chlevel;
      Graph           Network  = new Graph();
      Edge            A_Edge[] = new Edge[100];
      Node            A_Node[] = new Node[100];
      ConvertToDG     GtoDG = null;
      DrawableGraph   COAGraph = null;
      
      FileInputStream iInStream = new FileInputStream(filename);
      BufferedReader br = new BufferedReader(
                          new InputStreamReader(iInStream));
      missionTitle = br.readLine();
      //
      //   Read the mission description
      //
      String sTemp = br.readLine();
      int count = 0;
      do {
          if((sTemp.length() != 0) || (sTemp.charAt(0) != '#')) {
            if(sTemp.charAt(0) != '[') {
              missionLine[count] = sTemp;
              count++;
            } else {
                break;
            }
          }
          sTemp = br.readLine();
      } while(sTemp != null);
      nLines = count + 1;
      String missionText = "            MISSION: " + missionTitle;
      for (n=1; n<nLines; n++) {
        missionText = missionText + "\n" + missionLine[n-1];
      }
      missionArea.setText(missionText);
      //
      //   Read the # of COAs
      //
      sTemp = br.readLine();
      nCOA = Integer.valueOf(sTemp).intValue();
      //
      Network.setLabel("COA");
      Network.setProperty("directed", "1");
      Network.setProperty("layoutSaved", "true");
      Network.setProperty("Levels", "7");
      Network.setId(0);
      int T_Nodes = 0;
      //
      int BranchNo = 0;
      for (int n=0; n<nCOA; n++) {
        sTemp = br.readLine();
        //
        //   Read the worth
        //
        sTemp = br.readLine();
        double w = Double.valueOf(sTemp).doubleValue();
        COAworths[n] = w;
        //
        //   Read the title
        //
        COATitle[n] = br.readLine();
        //
        //   Read the Weapon/Target filenames
        //
        COAWeaponFN[n] = br.readLine();
        COATargetFN[n] = br.readLine();
        //
        //   Read the number of threads (branches) for this COA
        //   and build the top node.
        //
        sTemp = br.readLine();
        COAnThreads[n] = Integer.valueOf(sTemp).intValue();
        dBranch = dBranch + 1.0 + (double)(COAnThreads[n]-1)/2.0;
        TopNodes[n] = T_Nodes;
        A_Node[T_Nodes] = new Node();
        A_Node[T_Nodes].setId(T_Nodes+1);
        A_Node[T_Nodes].setLabel(COATitle[n]);
        A_Node[T_Nodes].setProperty("id", String.valueOf(T_Nodes+1));
        A_Node[T_Nodes].setProperty("selected", "false");
        A_Node[T_Nodes].setProperty("inEdit", "false");
        A_Node[T_Nodes].setProperty("sliderToggleVisible", "false");
        A_Node[T_Nodes].setProperty("manual", "false");
        A_Node[T_Nodes].setProperty("sliderVisible", "false");
        A_Node[T_Nodes].setProperty("scoreBubble", "true");
        A_Node[T_Nodes].setProperty("score", "0.0");
        A_Node[T_Nodes].setProperty("level", "5");
        A_Node[T_Nodes].setProperty("branch", String.valueOf(dBranch));
        A_Node[T_Nodes].setProperty("Belief", "0.0");
        A_Node[T_Nodes].setProperty("Disbelief", "0.0");
        A_Node[T_Nodes].setProperty("label", COATitle[n]);
        A_Node[T_Nodes].setProperty("Phrase", " ");
        A_Node[T_Nodes].setProperty("Threshold", "0.75");
        A_Node[T_Nodes].setProperty("Cutoff", "0.75");
        A_Node[T_Nodes].setProperty("y", "-88");
        A_Node[T_Nodes].setProperty("x", "34.5");
        A_Node[T_Nodes].setProperty("w", "225");
        A_Node[T_Nodes].setProperty("h", "144");
        int topNode = T_Nodes;
        T_Nodes++;
        //
        int EdgeCount = 0;
        int xInt = 10;
        //
        //   Build each thread (branch) for this COA
        //
        for (int i=0; i<COAnThreads[n]; i++) {
          //
          //   Get the number of nodes on this branch
          //
          sTemp = br.readLine();
          nNodes = Integer.valueOf(sTemp).intValue();
          COAnNodes[n][i] = nNodes;
          dBranch = BranchNo;
          fromNode = topNode;
          //
          //   Build each node of this branch
          //
          for (int j=0; j<nNodes; j++) {
             sTemp = br.readLine();
             int nodeID = T_Nodes+1;
             int j1 = 4-j;
             A_Node[T_Nodes] = new Node();
             A_Node[T_Nodes].setId(nodeID);
             A_Node[T_Nodes].setLabel(sTemp);
             A_Node[T_Nodes].setProperty("id", String.valueOf(T_Nodes+1));
             A_Node[T_Nodes].setProperty("selected", "false");
             A_Node[T_Nodes].setProperty("inEdit", "false");
             A_Node[T_Nodes].setProperty("sliderToggleVisible", "false");
             A_Node[T_Nodes].setProperty("manual", "false");
             A_Node[T_Nodes].setProperty("sliderVisible", "false");
             A_Node[T_Nodes].setProperty("sliderValue", "12");
             A_Node[T_Nodes].setProperty("connectorHubsVisible", "false");
             A_Node[T_Nodes].setProperty("SouthConnectorHubSelected", "false");
             A_Node[T_Nodes].setProperty("creatingEdge", "false");
             A_Node[T_Nodes].setProperty("scoreBubble", "false");
             A_Node[T_Nodes].setProperty("score", "0.0");
             A_Node[T_Nodes].setProperty("level", String.valueOf(j1));
             A_Node[T_Nodes].setProperty("branch", String.valueOf(dBranch));
             A_Node[T_Nodes].setProperty("Belief", "0.0");
             A_Node[T_Nodes].setProperty("Disbelief", "0.0");
             A_Node[T_Nodes].setProperty("Phrase", " ");
             A_Node[T_Nodes].setProperty("Threshold", "0.75");
             A_Node[T_Nodes].setProperty("Cutoff", "0.75");
             A_Node[T_Nodes].setProperty("y", "10");
             A_Node[T_Nodes].setProperty("x", String.valueOf(xInt));
             A_Node[T_Nodes].setProperty("w", "225");
             A_Node[T_Nodes].setProperty("h", "144");
             xInt += 10;
             //
             //   Define the edge between this node the the one above
             //
             A_Edge[T_Nodes-1] = new Edge(A_Node[fromNode], A_Node[T_Nodes]);
             A_Edge[T_Nodes-1].setProperty("sliderValue", "0");
             A_Edge[T_Nodes-1].setProperty("selected", "false");
             A_Edge[T_Nodes-1].setProperty("sliderVisible", "false");
             A_Edge[T_Nodes-1].setProperty("edgeBubble", "false");
             //A_Edge[T_Nodes-1].setProperty(DrawableEdge.PROPERTY_FAUX, "false");
             A_Edge[T_Nodes-1].setProperty("Link_Label", " ");
             A_Edge[T_Nodes-1].setProperty("B_Weight", "0.0");
             A_Edge[T_Nodes-1].setProperty("D_Weight", "0.0");
             //
             fromNode = T_Nodes;
             EdgeCount++;
             T_Nodes++;
           }
           BranchNo++;
        }
        TopNodes[nCOA] = T_Nodes;
      }
      //
      //   Done. Close input
      //
      iInStream.close();
      // 
      //   Build the directed graph
      //
      Network.add(A_Node);
      Network.add(A_Edge);
      //
      //   Convert it to a Drawable graph
      //
      try {
        COAGraph = GtoDG.parseGraph(Network);
        i = 0;
        for (NodeIterator itr=COAGraph.nodesIterator(); itr.hasNext(); ) {
          DrawableNode node = (DrawableNode) itr.next();
          if (node instanceof DrawableNode) {
            DN[i] = node;
            i++;
          }
        }
        //
        //   Display graph
        //
        try {
          //bne.loadStoryFromKB("IW_DCI", "IW_DCI");
          bne.loadStory(COAGraph);
        }
        catch(Exception e) { e.printStackTrace(); }
        //
        return COAGraph;       
      }
      catch(Exception ioe) {
        ioe.printStackTrace(); 
        return null;
      }
  }
  
    public void findCOAValues(int k)
  {
    int vmax = 0, vmin = 0, smax = 0, smin = 0;
    double score = 0.0;
    double realScore = 0.0;
    double We = 0.0, Ws = 0.0;
    double value = 0.0;
    double sum = 0.0;
    double minvalue = 9999.0, minscore = 9999.0;
    double maxvalue = -9999.0, maxscore = -9999.0;
    String msg = "";
    
    DecimalFormat df = new DecimalFormat("#0.0");

    value = 0.0;
    score = 0.0;
    for (int n=0; n<nCOA; n++) {
      //
      //   Calculate WORTH (0.0 - 1.0)
      //
      value = COAworths[n];
      COAValue[n] = value;
      Ws = slider[S_Similarity].getValue()/100.0;
      /*
      if (value < minvalue) {
          minvalue = value;
          vmin = n;
      }
      if (value > maxvalue) {
          maxvalue = value;
          vmax = n;
      }
      */
      //   Calculate SCORE (0.0 - 1.0)
      //     
      score = Optimize(n);
      COAScore[n] = Math.abs(score);
      double st = (double)((int)(COAScore[n]/100.0)*100);
      COAScore[n] = (COAScore[n] - st)/100.0;
      We = slider[S_Achieved].getValue()/100.0;
    }
    //
    String logText = "         Scores:";
    for (int n=0; n<nCOA; n++) {
      realScore = ((Ws/(Ws+We))*COAScore[n] + (We/(Ws+We))*COAValue[n])*100.0;
      if (realScore < minscore) {
        minscore = realScore;
        smin = n;
      }
      if (realScore > maxscore) {
        maxscore = realScore;
        smax = n;
      }
      logText = logText + "\nCOA[" + n + "] = " +
                df.format(realScore);
      DN[TopNodes[n]].setProperty("score", df.format(realScore));
    }
    scoreArea.setText(logText);
    bestCOA = smax;
    //
    if (k > 0) {
      msg = "                     COA #" + bestCOA + "\n" + ROpairs[bestCOA][0]; 
      for (i=0; i<nRO; i++) {       
        msg = msg + "\n" + ROpairs[bestCOA][i+1];
      }
      msg = msg + "\n Similarity Factor of COA to Order: " + 
            df.format(COAValue[bestCOA]*100.0) + "%\n" +
            " Probability of Effects Achieved:   " +
            df.format(COAScore[bestCOA]*100.0) + "%\n" +
            " Overall Score of Course-of-Action: " +
            df.format(realScore) + "%";
/*
      JOptionPane.showMessageDialog(jp_Graph, msg, "Score Summary",
                                    JOptionPane.PLAIN_MESSAGE);
*/
      final JDialog  scoreDialog = new JDialog(this, "Score Summary", true);
      scoreDialog.setLocation(0, 200);
      scoreDialog.setSize(910,475);
      scoreDialog.getContentPane().setLayout(new BorderLayout());
        JPanel  grfPanel = new JPanel();
        grfPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
        JTextArea summaryText = new JTextArea(msg);
        summaryText.setPreferredSize(new Dimension(250, 400));
        summaryText.setFont(eb);
        grfPanel.add(summaryText);
        //scoreDialog.getContentPane().add(summaryText);
        //
        JLabel graphLabel = new JLabel(new ImageIcon("images/OptimizeGraph.gif"));
        graphLabel.setSize(new Dimension(650,410));
        //JPanel graphPane = new JPanel();
        //graphPane.setLayout(new FlowLayout(FlowLayout.LEFT));  
        //graphPane.setBackground(Color.white);    
        //JScrollPane scrollableGraph = new JScrollPane(graphPane);
        //scrollableGraph.setPreferredSize(new Dimension(640, 410));
        //scrollableGraph.add(graphLabel);
        grfPanel.add(graphLabel);
        //
        JPanel dismissPanel = new JPanel();
        dismissPanel.setSize(new Dimension(900, 20));
        dismissPanel.setLayout(new BoxLayout(dismissPanel, BoxLayout.X_AXIS));
        dismissPanel.setBorder(BorderFactory.createEmptyBorder(5, 0, 5, 0));
        //
        JButton dismissBtn = new JButton("Dismiss");
        dismissBtn.addActionListener( new ActionListener() {
            public void actionPerformed(ActionEvent e)
            { scoreDialog.hide(); }
        } );
        dismissPanel.add(Box.createHorizontalGlue());
        dismissPanel.add(dismissBtn);
        dismissPanel.add(Box.createHorizontalGlue());
        scoreDialog.getContentPane().add(grfPanel, BorderLayout.CENTER);
        scoreDialog.getContentPane().add(dismissPanel, BorderLayout.PAGE_END);
      scoreDialog.show();
    }
  }
  
  private double Optimize(int n)
  {
    int i = 0;
    String cmd;
 
    try {
      Runtime rt = Runtime.getRuntime();
      nRO = (int)(slider[S_Pairs].getValue()/10.0);
      cmd = "COAOptimize.exe " + COAWeaponFN[n] + " " + COATargetFN[n] +
            " " + (slider[S_rEA].getValue()/10.0) +
            " " + nRO;
      //System.out.println("Optimer command :" + cmd);
      Process proc = rt.exec(cmd);
      //Process proc = rt.exec("COAOptimize.exe ActiveOptions.dat ActiveTargets.dat");
      InputStreamReader  isr = new InputStreamReader(proc.getInputStream());
      BufferedReader br = new BufferedReader(isr);

      String line;
      nOptions = 0;
      while ((line = br.readLine()) != null) {
        i = nOptions%(nRO+2);
        ROpairs[n][i] = line;
        nOptions++;
      }
      //
    }
    catch(IOException ioe) { System.out.println(ioe); }
    //
    //System.out.println("Looking for node " + String.valueOf(TopNodes[n]));
    //DrawableNode node = getDrawableNodeById(String.valueOf(TopNodes[0]));
    //if (node != null) node.setProperty("score", "97.1");
    //
    //optimizer.optimize();
    //
    return (Double.valueOf(ROpairs[n][nRO+1]).doubleValue());
  }
  
   private void initFrame() {

      setTitle("COA Analyzer");

      jp_Graph = new javax.swing.JPanel();
      JScrollPane _scroll = new JScrollPane();
      
      getContentPane().setLayout(null);
      
      addWindowListener(new java.awt.event.WindowAdapter() {
          public void windowClosing(java.awt.event.WindowEvent evt) {
              exitForm(evt);
          }
      });
      
      jp_Graph.setLayout(null);
      //jp_Graph.add(_scroll);
      
      jp_Graph.setMaximumSize(new java.awt.Dimension(800, 600));
      getContentPane().add(jp_Graph);
      jp_Graph.setBounds(0, 120, 388, 388);
      
      pack();
  }
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
  private void initComponents() {//GEN-BEGIN:initComponents
      jp_Graph = new javax.swing.JPanel();
      
      getContentPane().setLayout(null);
      
      addWindowListener(new java.awt.event.WindowAdapter() {
          public void windowClosing(java.awt.event.WindowEvent evt) {
              exitForm(evt);
          }
      });
      
      jp_Graph.setLayout(null);
      
      jp_Graph.setMaximumSize(new java.awt.Dimension(900, 800));
      getContentPane().add(jp_Graph);
      jp_Graph.setBounds(5, 100, 388, 272);
      
      pack();
  }//GEN-END:initComponents

    /** Exit the Application */
    private void exitForm(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_exitForm
        System.exit(0);
    }//GEN-LAST:event_exitForm

    /**
    * @param args the command line arguments
    */
    public static void main(String args[]) {
        COAAnalyzer coa = new COAAnalyzer();
        coa.setSize(910,720);
        coa.show();
    }

    private int             S_Similarity = 0;
    private int             S_Achieved   = 1;
    private int             S_rEA        = 2;
    private int             S_Pairs      = 3;
    private int             S_Nuclear    = 4;
    private int             value, i, j, n, nCOA, nLines, nOptions;
    private int             nNodes        = 0;
    private int             bestCOA       = 1;
    private int             nRDE          = 3;
    private int             nRO           = 3;
    private int             findFlag      = 0;
    private int             labelwidth[]  = { 375, 300, 245 };
    private int             TopNodes[]    = new int[20];
    private int             ScoreNode[]   = new int[20];
    private int             COAnThreads[] = new int[20];
    private int             COAnNodes[][] = new int[20][20];
    private double          COAworths[]   = new double[20];
    private double          COAValue[]    = new double[20];
    private double          COAScore[]    = new double[20];
    private String          DefaultCOAs   = "Conv-COAs.ini";
    private String          NuclearCOAs   = "Nuke-COAs.ini";
    private String[][]      ROpairs       = new String[20][20];
    private String          missionTitle  = "Global Strike";
    private String []       missionLine   = new String[20];
    private String []       COATitle      = new String[20];
    private String          COAWeaponFN[] = new String[20];
    private String          COATargetFN[] = new String[20];
    //private JFrame          _frame;
    //private JSlider         slider1, slider2, slider3, slider4, slider5;
    private JSlider         slider[];
    //private JLabel          wlabel[]    = new JLabel[20];
    private JTextArea       scoreArea;
    private JTextArea       missionArea;
    private Border          compound;
    private Color           coacolor      = new Color(222, 231, 231);
    //private ImageIcon       greenled = new ImageIcon("/tmp/tiny_green_led.gif");
    //private ImageIcon       goldled  = new ImageIcon("/tmp/tiny_yellow_led.gif");  
    //private ImageIcon       redled   = new ImageIcon("/tmp/tiny_red_led.gif");
    private Font            fb = new Font("TimesRoman", Font.BOLD, 12);
    private Font            sb = new Font("Helvetica", Font.BOLD, 8);
    private Font            eb = new Font("Elite", Font.BOLD, 9);
    //
    private DrawableGraph   COAGraph = null;
    private BNE_View        bne = new BNE_View();
    //private HMV             bne = new HMV();  
    private DrawableNode    DN[] = new DrawableNode[100];
    private COAOptimizer    optimizer;
    private AccessKBDlg     db = new AccessKBDlg();

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JPanel jp_Graph;
    // End of variables declaration//GEN-END:variables

}
