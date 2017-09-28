package com.appliedminds.martinix;

import com.appliedminds.martini.*;
import com.appliedminds.martini.io.*;
import com.appliedminds.martini.layout.*;
import com.appliedminds.martinix.greenpill.*;
import com.appliedminds.martinix.fader.*;
import com.appliedminds.hmv.*;
import royere.cwi.structure.*;
import com.appliedminds.jam.*;
import BNE_View.*;

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

public class SampleGraph {

  private   int             value, i, j, n, nCOA, nLines, nOptions;
  private   int             nNodes;
  private   int             bestCOA = 1;
  private   int             nRDE = 3;
  private   int             labelwidth[]  = { 375, 300, 245 };
  private   int             COAnThreads[] = new int[20];
  private   int             COAnNodes[][] = new int[20][20];
  private   double          COAworths[][] = new double[5][20];
  private   double          COAValue[]    = new double[20];
  private   String          COAWeaponFN[] = new String[20];
  private   String          COATargetFN[] = new String[20];
  private   JFrame          _frame;
  private   JSlider         slider1, slider2, slider3, slider4, slider5;
  private   JSlider         slider[];
  private   JLabel          wlabel[];
  private   Border          compound;
  private   GraphPanel      _gpanel;
  //private   JPanel          _gpanel;
  private   JScrollPane     _scrollPane;
  private   PanAndScanPanel _box;
  private   Font            fb = new Font("TimesRoman", Font.BOLD, 12);
  private   Font            sb = new Font("Helvetica", Font.BOLD, 8);
  private   Color           ColorBack;
  private   String          missionTitle = "Global Strike";
  private   String[]        missionLine = new String[20];
  private   String[]        COATitle = new String[20];
  private   String[]        ROpairs = new String[20];
  private   String borderTitles[] = { "Response", "Risk", "Political",
                                           "Success", "Assets" };
  private   String sliderNames[]  = { "sliderA", "sliderB", "sliderC",
                                           "sliderD", "sliderE" };
  private   ImageIcon       greenled = new ImageIcon("tiny_green_led.gif");
  private   ImageIcon       goldled  = new ImageIcon("tiny_yellow_led.gif");  
  private   ImageIcon       redled   = new ImageIcon("tiny_red_led.gif");
  private   Graph           Network  = new Graph();
  private   Node            A_Node[] = new Node[100];
  private   Edge            A_Edge[] = new Edge[100];
  private   ConvertToDG     GtoDG = null;
  private   DrawableGraph   COAGraph = null;
  //private   LayoutLoader    loader = new GLayoutLoader();
  private   BNE_View        bneview = new BNE_View();

  protected   DrawableGraph createTestGraph()
  {
    DrawableGraph dg = new DrawableGraph();

    DrawableNode node1 = dg.createNode();
    node1.setProperty(GreenPillUIPrefs.getNodeLabelPropertyName(), "node1");
    node1.setProperty(GreenPillUIPrefs.getSelectedPropertyName(), "false");
    node1.setProperty(GreenPillUIPrefs.getDistanceFromRootPropertyName(), "0");
    node1.setProperty(GreenPillUIPrefs.getRootNodePropertyName(), "true");

    DrawableNode node2 = dg.createNode();
    node2.setProperty(GreenPillUIPrefs.getNodeLabelPropertyName(), "node2sdfksdfds");
    node2.setProperty(GreenPillUIPrefs.getSelectedPropertyName(), "false");
    node2.setProperty(GreenPillUIPrefs.getDistanceFromRootPropertyName(), "1");
    node2.setProperty(GreenPillUIPrefs.getRootNodePropertyName(), "false");

    DrawableNode node3 = dg.createNode();
    node3.setProperty(GreenPillUIPrefs.getNodeLabelPropertyName(), "node 3");
    node3.setProperty(GreenPillUIPrefs.getSelectedPropertyName(), "false");
    node3.setProperty(GreenPillUIPrefs.getDistanceFromRootPropertyName(), "1");
    node3.setProperty(GreenPillUIPrefs.getRootNodePropertyName(), "false");

    DrawableEdge edge = dg.createEdge(node1, node2);
    CollapsedEdge ce = CollapsedEdge.getCollapsedEdge(edge);
    ce.addSubEdge(CollapsedEdge.HEAD_DIRECTION, "lalala", true);

    DrawableEdge edge2 = dg.createEdge(node1, node3);
    ce = CollapsedEdge.getCollapsedEdge(edge);
    ce.addSubEdge(CollapsedEdge.TAIL_DIRECTION, "blah", true);

    return dg;
  }
  
  public   void readCOAs() throws Exception
  {
      String chlevel;
      
      FileInputStream iInStream = new FileInputStream("ISPAN.ini");
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
      //
      //   Read the # of COAs
      //
      sTemp = br.readLine();
      nCOA = Integer.valueOf(sTemp).intValue();
      //
      for (n=0; n<nCOA; n++) {
        sTemp = br.readLine();
        //
        //   Read the title
        //
        COATitle[n] = br.readLine();
        wlabel[n] = new JLabel(goldled);
        //
        //   Read the worths
        //
        for (i=0; i<5; i++) {
          sTemp = br.readLine();
          double w = Double.valueOf(sTemp).doubleValue();
          COAworths[i][n] = w;
        }
        //
        COAWeaponFN[n] = br.readLine();
        COATargetFN[n] = br.readLine();
        sTemp = br.readLine();
        COAnThreads[n] = Integer.valueOf(sTemp).intValue();
        //
        Network.setLabel("COA");
        Network.setProperty("directed", "1");
        Network.setProperty("layoutSaved", "true");
        Network.setProperty("Levels", "6");
        Network.setId(0);
        //
        int T_Nodes = 0;
        A_Node[T_Nodes] = new Node();
        A_Node[T_Nodes].setId(T_Nodes+1);
        A_Node[T_Nodes].setLabel(COATitle[n]);
        A_Node[T_Nodes].setProperty("selected", "false");
        A_Node[T_Nodes].setProperty("inEdit", "false");
        A_Node[T_Nodes].setProperty("sliderToggleVisible", "false");
        A_Node[T_Nodes].setProperty("manual", "false");
        A_Node[T_Nodes].setProperty("sliderVisible", "false");
        A_Node[T_Nodes].setProperty("level", "0");
        A_Node[T_Nodes].setProperty("y", "-88");
        A_Node[T_Nodes].setProperty("x", "34.5");
        A_Node[T_Nodes].setProperty("w", "225");
        A_Node[T_Nodes].setProperty("h", "144");
        System.out.println("Node " + T_Nodes + " generated with label " + COATitle[n]);

        int EdgeCount = 0;
        //
        for (i=0; i<COAnThreads[n]; i++) {
           sTemp = br.readLine();
           nNodes = Integer.valueOf(sTemp).intValue();
           COAnNodes[n][i] = nNodes;
           int fromNode = 0;
             for (j=0; j<nNodes; j++) {
                sTemp = br.readLine();
                T_Nodes++;
                int nodeID = T_Nodes+1;
                int j1 = j+1;
                A_Node[T_Nodes] = new Node();
                A_Node[T_Nodes].setId(nodeID);
                A_Node[T_Nodes].setLabel(sTemp);
                A_Node[T_Nodes].setProperty("selected", "false");
                A_Node[T_Nodes].setProperty("inEdit", "false");
                A_Node[T_Nodes].setProperty("sliderToggleVisible", "false");
                A_Node[T_Nodes].setProperty("manual", "false");
                A_Node[T_Nodes].setProperty("sliderVisible", "false");
                A_Node[T_Nodes].setProperty("sliderValue", "78");
                A_Node[T_Nodes].setProperty("connectorHubsVisible", "false");
                A_Node[T_Nodes].setProperty("label", sTemp);
                A_Node[T_Nodes].setProperty("SouthConnectorHubSelected", "false");
                A_Node[T_Nodes].setProperty("creatingEdge", "false");
                A_Node[T_Nodes].setProperty("level", String.valueOf(j1));
                A_Node[T_Nodes].setProperty("y", "-88");
                A_Node[T_Nodes].setProperty("x", "34.5");
                A_Node[T_Nodes].setProperty("w", "225");
                A_Node[T_Nodes].setProperty("h", "144");
                /*
                A_Edge[T_Nodes-1] = new Edge(A_Node[fromNode], A_Node[T_Nodes]);
                A_Edge[T_Nodes-1].setProperty("sliderValue", "3");
                A_Edge[T_Nodes-1].setProperty("selected", "false");
                A_Edge[T_Nodes-1].setProperty("sliderVisible", "false");
                fromNode = T_Nodes;
                 **/
             }
        }
        A_Edge[0] = new Edge(A_Node[0], A_Node[1]);
        A_Edge[0].setProperty("sliderValue", "3");
        A_Edge[0].setProperty("selected", "false");
        A_Edge[0].setProperty("sliderVisible", "false");
        A_Edge[1] = new Edge(A_Node[1], A_Node[2]);
        A_Edge[1].setProperty("sliderValue", "3");
        A_Edge[1].setProperty("selected", "false");
        A_Edge[1].setProperty("sliderVisible", "false");
        A_Edge[2] = new Edge(A_Node[2], A_Node[3]);
        A_Edge[2].setProperty("sliderValue", "3");
        A_Edge[2].setProperty("selected", "false");
        A_Edge[2].setProperty("sliderVisible", "false");
        A_Edge[3] = new Edge(A_Node[3], A_Node[4]);
        A_Edge[3].setProperty("sliderValue", "3");
        A_Edge[3].setProperty("selected", "false");
        A_Edge[3].setProperty("sliderVisible", "false");
        A_Edge[4] = new Edge(A_Node[0], A_Node[5]);
        A_Edge[4].setProperty("sliderValue", "3");
        A_Edge[4].setProperty("selected", "false");
        A_Edge[4].setProperty("sliderVisible", "false");
        A_Edge[5] = new Edge(A_Node[5], A_Node[6]);
        A_Edge[5].setProperty("sliderValue", "3");
        A_Edge[5].setProperty("selected", "false");
        A_Edge[5].setProperty("sliderVisible", "false");
        A_Edge[6] = new Edge(A_Node[6], A_Node[7]);
        A_Edge[6].setProperty("sliderValue", "3");
        A_Edge[6].setProperty("selected", "false");
        A_Edge[6].setProperty("sliderVisible", "false");
        A_Edge[7] = new Edge(A_Node[7], A_Node[8]);
        A_Edge[7].setProperty("sliderValue", "3");
        A_Edge[7].setProperty("selected", "false");
        A_Edge[7].setProperty("sliderVisible", "false");
        A_Edge[8] = new Edge(A_Node[8], A_Node[9]);
        A_Edge[8].setProperty("sliderValue", "3");
        A_Edge[8].setProperty("selected", "false");
        A_Edge[8].setProperty("sliderVisible", "false");
        A_Edge[9] = new Edge(A_Node[0], A_Node[10]);
        A_Edge[9].setProperty("sliderValue", "3");
        A_Edge[9].setProperty("selected", "false");
        A_Edge[9].setProperty("sliderVisible", "false");
        A_Edge[10] = new Edge(A_Node[10], A_Node[11]);
        A_Edge[10].setProperty("sliderValue", "3");
        A_Edge[10].setProperty("selected", "false");
        A_Edge[10].setProperty("sliderVisible", "false");
        A_Edge[11] = new Edge(A_Node[11], A_Node[12]);
        A_Edge[11].setProperty("sliderValue", "3");
        A_Edge[11].setProperty("selected", "false");
        A_Edge[11].setProperty("sliderVisible", "false");
      }
      //
      //   Done. Close input
      //
      iInStream.close();
      //     
      Network.add(A_Node);
      Network.add(A_Edge);
      try {
        COAGraph = GtoDG.parseGraph(Network);
        
      }
      catch(Exception ioe) {ioe.printStackTrace(); }
  }
  
  public   void findCOAValues(int k)
  {
    int imax = 0, imin = 0;
    double sum = 0.0;
    double minsum = 9999.0;
    double maxsum = -9999.0;
    
    DecimalFormat df = new DecimalFormat("#0.00");

    for (n=0; n<nCOA; n++) {
      sum = 0.0;
      for (i=0; i<5; i++) {
        sum = sum + (COAworths[i][n] * (slider[i].getValue()/100.0))/5.0;
      }
      //
      COAValue[n] = sum;
      String chsum = "Worth is " + df.format(sum);
      wlabel[n].setText(chsum);
      wlabel[n].setIcon(goldled);
      if (sum < minsum) {
          minsum = sum;
          imin = n;
      }
      if (sum > maxsum) {
          maxsum = sum;
          imax = n;
      }
    }
    bestCOA = imin + 1;
    wlabel[imin].setIcon(greenled);
    wlabel[imax].setIcon(redled);
  }
  
  public   void Optimize(int n)
  {
    int i;
    String cmd;
 
    try {
      Runtime rt = Runtime.getRuntime();
      cmd = "COAOptimize.exe " + COAWeaponFN[n] + " " + COATargetFN[n];
      Process proc = rt.exec("COAOptimize.exe ActiveOptions.dat ActiveTargets.dat");
      InputStreamReader  isr = new InputStreamReader(proc.getInputStream());
      BufferedReader br = new BufferedReader(isr);

      String line;
      nOptions = 0;
      while ((line = br.readLine()) != null) {
        i = nOptions%(nRDE+2);
        ROpairs[i] = line;
        //System.out.println(line);
        nOptions++;
      }
    }
    catch(IOException ioe) { System.out.println(ioe); }

  }

  public void SampleGraph()
  {
    FaderUI2      _graphUI;
    FaderUIPrefs2 _graphUIPrefs;
    HMVSelectionManager _selectionManager;
    
    wlabel = new JLabel[20];
/*
**   Define the mechanism for getting out
*/
    WindowListener l = new WindowAdapter() {
        public void windowClosing(WindowEvent e) {System.exit(0);}
      };
/*
    _box = new PanAndScanPanel(100,
                               100,
                               _gpanel,
                               new Color(206, 255, 238),
                               new Color(0xFE, 0x86, 0x3B));
 **/
/*
    _scrollPane.addComponentListener
      (new ComponentListener() {
          public void componentMoved(ComponentEvent e) { }
          public void componentHidden(ComponentEvent e) { }
          public void componentShown(ComponentEvent e) {
            positionBox();
          }
          public void componentResized(ComponentEvent e) {
            positionBox();
          }
        });
*/
/*
**   Read the pre-defined COAs
*/
    try {
      readCOAs();
    } 
    catch(Exception e) {
      e.printStackTrace();
    }
/*
**   Set some preferences
*/   
    ColorBack = Color.lightGray;
/*
**   Define a frame to put everything into
*/    
    _frame = new JFrame("COA Viewer");
    _frame.addWindowListener(l);
    _frame.setSize(800,700);
    _frame.getContentPane().setBackground(ColorBack);
    _frame.getContentPane().setLayout(new FlowLayout());
/*
**   Define the control panel
*/   
    JPanel controlArea = new JPanel(new GridLayout(1,3));
    controlArea.setPreferredSize(new Dimension(780, 90));
    controlArea.setBackground(ColorBack);   
    // 
    //   Put the mission area in
    //
    String missionText = "            MISSION: " + missionTitle;
    for (n=1; n<nLines; n++) {
      missionText = missionText + "\n" + missionLine[n-1];
    }
    //
    JTextArea missionPane = new JTextArea(missionText);
    missionPane.setPreferredSize(new Dimension(260, 90));
    missionPane.setBackground(ColorBack);
    missionPane.setForeground(Color.yellow);
    missionPane.setFont(fb);
    missionPane.setBorder(BorderFactory.createEtchedBorder());
    controlArea.add(missionPane);
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
                    //System.err.println("Slider " + islider + " value = " + value);
                    findCOAValues(0);
                    Optimize(islider);
                    String msg = "                     COA #" + bestCOA + "\n" +
                                 ROpairs[0] + "\n" + ROpairs[1] + "\n" +
                                 ROpairs[2] + "\n" + ROpairs[3] + "\n" +
                                 ROpairs[4];
                    JOptionPane.showMessageDialog(_frame, msg, "Option/Target Pairs",
                    JOptionPane.PLAIN_MESSAGE);
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
    String logText = "- 10Oct03 : Risk Lowered by Gen. Howitzer\n" +
        "   (No explanation given)";
    JTextArea loggerPane = new JTextArea(logText);
    loggerPane.setPreferredSize(new Dimension(260, 90));
    loggerPane.setBorder(BorderFactory.createEtchedBorder());
    controlArea.add(loggerPane);
    //
    //   Put the control area into the frame
    //
    _frame.getContentPane().add(controlArea);
/*
**   Define the COA selection area
*/
    findCOAValues(0);
    
    _graphUIPrefs = new FaderUIPrefs2();
    //
    //   Set preferred colors
    //
    _graphUIPrefs.setDefaultColor(new Color(0, 0, 255));
    _graphUIPrefs.setSupportingColor(new Color(0, 255, 0));
    _graphUIPrefs.setRefutingColor(new Color(255, 0, 0));
    //
    //   Use curved edges
    //
    _graphUIPrefs.setCurvedLines(true);
    //
    //   Set a UI
    //
    _graphUI = new FaderUI2(_graphUIPrefs); // new GreenPillUI()
    _selectionManager = new FaderSelectionManager();
    //
    //   Define the network graph area
    //
    _gpanel = new GraphPanel(_graphUI);
    bneview.loadStory(COAGraph);
    _gpanel.setDrawableGraph(COAGraph); //g);
    GraphLayout layout = new DefaultGraphLayout();
    _gpanel.setLayout(layout);
    _gpanel.setScale(5.0);
    _gpanel.setBackground(Color.red);
    _gpanel.setBorder(BorderFactory.createLineBorder(Color.green));
    _gpanel.setPreferredSize(new Dimension(1500,1550));
    
    _gpanel.addDrawableGraphMouseEventListener
      ("NODE",
       new DrawableGraphMouseEventListener() {
         public boolean handleDrawableGraphMouseEvent
           (DrawableGraphMouseEvent e)
         {
           System.out.println("Got node event: " + e.toString());
           return(false);
         }
       });

    _gpanel.addDrawableGraphMouseEventListener
      ("NODE_TEXT",
       new DrawableGraphMouseEventListener() {
         public boolean handleDrawableGraphMouseEvent
           (DrawableGraphMouseEvent e)
         {
           System.out.println("Got node-text event: " + e.toString());
           return(false);
         }
       });

    _gpanel.addDrawableGraphMouseEventListener
      ("EDGE",
       new DrawableGraphMouseEventListener() {
         public boolean handleDrawableGraphMouseEvent
           (DrawableGraphMouseEvent e)
         {
           System.out.println("Got edge event: " + e.toString());
           return(false);
         }
       });

    _gpanel.addDrawableGraphMouseEventListener
      ("EDGE_BUBBLE",
       new DrawableGraphMouseEventListener() {
         public boolean handleDrawableGraphMouseEvent
           (DrawableGraphMouseEvent e)
         {
           System.out.println("Got edge bubble event: " + e.toString());
           return(false);
         }
       });

    _gpanel.addDrawableGraphMouseEventListener
      (GraphPanel.OFFGRAPH_DECORATION_ID,
       new DrawableGraphMouseEventListener() {
         public boolean handleDrawableGraphMouseEvent
           (DrawableGraphMouseEvent e)
         {
           System.out.println("Got off-graph event: " + e.toString());
           return(false);
         }
       });

    //
    //   Get image of COAs
    //
    ImageIcon coaimage = new ImageIcon("COA-Selection.gif");
    i = coaimage.getIconWidth();
    //
    //   Define a panel to hold the worths and the COAs
    //
    Color coacolor = new Color(222, 231, 231);
    JPanel coaArea = new JPanel();
    coaArea.setPreferredSize(new Dimension(i, 580));
    coaArea.setBackground(coacolor);
    //
    //   Define a panel to hold the worths
    //
    JPanel worthArea = new JPanel(/*new FlowLayout()*/);
    worthArea.setPreferredSize(new Dimension(i, 20));
    worthArea.setBackground(coacolor);
    for (n=0; n<nCOA; n++) {
      wlabel[n].setPreferredSize(new Dimension(labelwidth[n], 15));
      wlabel[n].setHorizontalAlignment(SwingConstants.CENTER);
      //wlabel[n].setBorder(BorderFactory.createEtchedBorder());
      worthArea.add(wlabel[n]);
    }
    coaArea.add(worthArea);
    //
    //   Define an area to hold the image
    //
    JLabel coapanel = new JLabel(coaimage);
    coaArea.add(coapanel);
    coaArea.addMouseListener (new MouseListener() {
            public void mousePressed(MouseEvent e) { }
            public void mouseClicked(MouseEvent e) { 
                String msg = "                     COA #" + bestCOA + "\n" +
                             ROpairs[0] + "\n" + ROpairs[1] + "\n" +
                             ROpairs[2] + "\n" + ROpairs[3] + "\n" +
                             ROpairs[4];
                JOptionPane.showMessageDialog(_frame, msg, "Option/Target Pairs",
                JOptionPane.PLAIN_MESSAGE);
            }
            public void mouseReleased(MouseEvent e) { }
            public void mouseEntered(MouseEvent e) { }
            public void mouseExited(MouseEvent e) { }
            });
    //
    //   Make them scrollable in case they are too big
    //
        
    bneview.initGAppFrame(false);
    bneview.setSize(1024, 768);
    bneview.show();
    bneview.hide();
    _frame.add(bneview.getContentPane());
//  bne.setServerLocal("158.114.52.140", "3306");
//  bne.loadStoryFromKB("IW_DCI", "IW_DCI");

    //_scrollPane = new JScrollPane(bneview.getContentPane()/*_gpanel coaArea*/);     //<----------------
    //_scrollPane.setPreferredSize(new Dimension(780,560));
    //_scrollPane.setViewportBorder(BorderFactory.createLineBorder(Color.orange));
    //_scrollPane.getViewport().setBackground(Color.white);
    //
    //   Put the COA selection area into the frame
    //
    //coaArea.add(bneview.getContentPane());
    //_frame.getContentPane().add(bneview.getContentPane());
    //_frame.getContentPane().add(_scrollPane);
    
    //JRadioButton button = new JRadioButton("small");
    //button.addActionListener(new ZoomListener(_gpanel));
    //_frame.getContentPane().add(button);
/*
    ViewportPanAndScanAdapter ada =
      new ViewportPanAndScanAdapter(_gpanel,
                                    _scrollPane.getViewport(),
                                    _box);
    //_frame.getLayeredPane().add(_box, JLayeredPane.PALETTE_LAYER);   
 */   
    //_frame.pack();
    _frame.setVisible(true);

    Optimize(bestCOA);
  }

  /*
   * Try to keep the box in the lower left-hand corner.
   */
  private   void positionBox() {
    Insets insets = _frame.getContentPane().getInsets();

    _box.setBounds
      (insets.left + 5,
       (int) (_scrollPane.getViewportBorderBounds().getHeight() -
              _box.getHeight()),
       _box.getWidth(),
       _box.getHeight());
  }

    class ZoomListener implements ActionListener {
    private GraphPanel _panel;

    public ZoomListener(GraphPanel p) {
      _panel = p;
    }

    public void actionPerformed(ActionEvent e) {
      if (((JRadioButton)e.getSource()).isSelected())
        _panel.setScale(1.0);
      else
        _panel.setScale(5.0);
      _frame.getContentPane().repaint();
    }
  }

  /*
   * A dummy transparent panel.
   */
  private   class TransP extends JPanel {
    private   final Color COLOR  = new Color(206,255,238);

    public TransP() {
      super();
      setOpaque(false);
    }

    public void paintComponent(Graphics g) {
      Graphics2D g2 = (Graphics2D) g;
      Dimension d = getSize();
      Rectangle2D rect =
        new Rectangle2D.Double(0,0,d.getWidth(), d.getHeight());
      g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER,
                                                 0.4f));
      g2.setColor(COLOR);
      g2.fill(rect);
    }
  }

  public static void main(String args[])
  {
      SampleGraph sg = new SampleGraph();
      sg.show();
  }
} // end class SampleGraph
