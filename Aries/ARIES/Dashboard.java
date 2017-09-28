import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.BorderFactory;
import java.lang.System.*;
import java.util.Properties;

public class Dashboard extends JPanel
{
  // Flash icons and labels:
  private JLabel  goldText = new JLabel();
  private JLabel  greenText = new JLabel();
  private JLabel  purpleText = new JLabel();
  private JLabel  redText = new JLabel();
  private JLabel  yellowText = new JLabel();

  private JLabel  goldComment = new JLabel();
  private JLabel  greenComment = new JLabel();
  private JLabel  purpleComment = new JLabel();
  private JLabel  redComment = new JLabel();
  private JLabel  yellowComment = new JLabel();

  // Status data windows:
  private ImageIcon  greenDiamondIcon = new ImageIcon("images/greenDiamond.gif");
  private ImageIcon  redDiamondIcon = new ImageIcon("images/redDiamond.gif");
  private ImageIcon  yellowDiamondIcon = new ImageIcon("images/yellowDiamond.gif");

  private JLabel  strikeIcon = new JLabel(greenDiamondIcon);
  private JLabel  defIcon = new JLabel(greenDiamondIcon);
  private JLabel  opIcon = new JLabel(greenDiamondIcon);
  private JLabel  ctlIcon = new JLabel(greenDiamondIcon);
  private JLabel  isrIcon = new JLabel(greenDiamondIcon);
  private JLabel  socomIcon = new JLabel(greenDiamondIcon);

  private JLabel  strikeData = createDataLabel();
  private JLabel  mslDefData = createDataLabel();
  private JLabel  opData = createDataLabel();
  private JLabel  spCtlData = createDataLabel();
  private JLabel  isrData = createDataLabel();
  private JLabel  socomData = createDataLabel();

  // Optimizer Metrics data windows:
  private JLabel  probData = createDataLabel();
  private JLabel  ratioData = createDataLabel();
  private JLabel  ofData = createDataLabel();
  private JLabel  simData = createDataLabel();
  private JLabel  scoreData = createDataLabel();

  // Panels:
  private JLabel  flashPanel = new JLabel(new ImageIcon("images/NKorea.jpg"));
  private JLabel  timeline = new JLabel(new ImageIcon("images/Timeline2.jpg"));
  private JLabel  amiPanel = new JLabel(new ImageIcon("images/BNEScreen_1.jpg"));

  // Colors:
  private Color      frameColor = new Color(200, 198, 162);
  private Color      panelColor = new Color(184, 181, 120);
  private Color      edgeColor = new Color(169, 165, 90);
  private Color      shadowColor = new Color(98, 88, 48);

  //----------------------------------------------------------------------------

  public Dashboard()
  {
    BorderLayout  lo = new BorderLayout();
    JPanel        controlPanel = createControlPanel();
    JPanel        appsPanel = createAppsPanel();
    JPanel        flash2Panel = createFlash2Panel();

strikeIcon.setIcon(redDiamondIcon);
isrIcon.setIcon(yellowDiamondIcon);

    lo.setVgap(4);
    setLayout(lo);
    setBorder(BorderFactory.createEmptyBorder(12, 7, 8, 7)); // t, l, b, r
    setBackground(frameColor);

    add(controlPanel, BorderLayout.PAGE_START);
    add(appsPanel, BorderLayout.CENTER);
    add(flash2Panel, BorderLayout.PAGE_END);
  }

  //----------------------------------------------------------------------------

  protected JPanel createControlPanel()
  {
    Border  paneBorder = BorderFactory.createCompoundBorder
      (BorderFactory.createLineBorder(edgeColor),
       BorderFactory.createEmptyBorder(5, 15, 5, 15));

    JPanel  mainPanel = new JPanel();
    JPanel  leftMainPanel = new JPanel();
    JPanel  rightMainPanel = new JPanel();


    // Control panel:

    JPanel   ctlPanel = new JPanel();
    JButton  planBtn = createControlButton("Planning", "launchPlanning.sh");
    JButton  cuerBtn = createControlButton("Memos", "mozilla AQFlash.sh");
    JButton  bneBtn = createControlButton("Evidence", "VisualKB.sh");

    ctlPanel.setLayout(new BoxLayout(ctlPanel, BoxLayout.Y_AXIS));
    ctlPanel.setBorder(paneBorder);
    ctlPanel.setBackground(panelColor);

    ctlPanel.add(Box.createRigidArea(new Dimension(0, 5)));
    ctlPanel.add(Box.createVerticalGlue());
    ctlPanel.add(planBtn);
    ctlPanel.add(Box.createVerticalGlue());
    ctlPanel.add(cuerBtn);
    ctlPanel.add(Box.createVerticalGlue());
    ctlPanel.add(bneBtn);
    ctlPanel.add(Box.createVerticalGlue());
    ctlPanel.add(Box.createRigidArea(new Dimension(0, 5)));


    // Status panel:

    JPanel        statusPanel = new JPanel();
    BorderLayout  statLO = new BorderLayout();
    statLO.setVgap(3);
    statusPanel.setLayout(statLO);
    statusPanel.setBorder(paneBorder);
    statusPanel.setBackground(panelColor);

    JLabel  statusLabel = new JLabel("STATUS");
    statusLabel.setHorizontalAlignment(SwingConstants.CENTER);

    JPanel  flagsPanel = new JPanel();
    flagsPanel.setLayout(new GridLayout(6, 1));


    JPanel  strikePanel =
      createIconLabelBlankPanel(strikeIcon, "Global Strike", strikeData);
    JPanel  mslDefPanel =
      createIconLabelBlankPanel(defIcon, "Missile Defense", mslDefData);
    JPanel  opPanel =
      createIconLabelBlankPanel(opIcon, "Information Operation", opData);
    JPanel  spCtlPanel =
      createIconLabelBlankPanel(ctlIcon, "Space Control", spCtlData);
    JPanel  isrPanel =
      createIconLabelBlankPanel(isrIcon, "ISR", isrData);
    JPanel  socomPanel =
      createIconLabelBlankPanel(socomIcon, "SOCOM", socomData);

    flagsPanel.add(strikePanel);
    flagsPanel.add(mslDefPanel);
    flagsPanel.add(opPanel);
    flagsPanel.add(spCtlPanel);
    flagsPanel.add(isrPanel);
    flagsPanel.add(socomPanel);

    statusPanel.add(statusLabel, BorderLayout.PAGE_START);
    statusPanel.add(flagsPanel, BorderLayout.CENTER);


    // Charts panel:

    JPanel  chartsPanel = new JPanel();

    chartsPanel.setLayout(new GridLayout(0, 2));
    chartsPanel.setBorder(paneBorder);
    chartsPanel.setBackground(panelColor);

    String[]   defStrs = {"Fighter", "PAC3", "Aegis", "Thaad"};
    JPanel     defPanel = new JPanel();
    JPanel     defSubPanel = new JPanel();
    JLabel     defTitle = new JLabel("DEF. SUMMARY");
    ImageIcon  defIcon = new ImageIcon("images/defChart.jpg");
    JLabel     defChart = new JLabel(defIcon);
    JComboBox  defMenu = new JComboBox(defStrs);
    Dimension  defMenuSize = new Dimension(defIcon.getIconWidth(), 25);

    defPanel.setLayout(new BorderLayout());
    defPanel.setBackground(panelColor);

    defSubPanel.setLayout(new BoxLayout(defSubPanel, BoxLayout.Y_AXIS));
    defSubPanel.setBackground(panelColor);

    defTitle.setHorizontalAlignment(SwingConstants.CENTER);
    defMenu.setPreferredSize(defMenuSize);
    defMenu.setMinimumSize(defMenuSize);
    defMenu.setMaximumSize(defMenuSize);
    defMenu.setAlignmentX(Component.CENTER_ALIGNMENT);
    defMenu.setBackground(panelColor);
    defChart.setAlignmentX(Component.CENTER_ALIGNMENT);
    defChart.setBorder(BorderFactory.createLineBorder(Color.BLACK));

    defSubPanel.add(Box.createRigidArea(new Dimension(0, 5)));
    defSubPanel.add(Box.createVerticalGlue());
    defSubPanel.add(defMenu);
    defSubPanel.add(Box.createRigidArea(new Dimension(0, 1)));
    defSubPanel.add(defChart);
    defSubPanel.add(Box.createVerticalGlue());

    defPanel.add(defTitle, BorderLayout.PAGE_START);
    defPanel.add(defSubPanel, BorderLayout.CENTER);

    String[]   offStrs = {"Fighter", "Bomber", "Aegis", "SLBM"};
    JPanel     offPanel = new JPanel();
    JPanel     offSubPanel = new JPanel();
    JLabel     offTitle = new JLabel("OFF. SUMMARY");
    ImageIcon  offIcon = new ImageIcon("images/offChart.jpg");
    JLabel     offChart = new JLabel(offIcon);
    JComboBox  offMenu = new JComboBox(offStrs);
    Dimension  offMenuSize = new Dimension(offIcon.getIconWidth(), 25);

    offPanel.setLayout(new BorderLayout());
    offPanel.setBackground(panelColor);

    offSubPanel.setLayout(new BoxLayout(offSubPanel, BoxLayout.Y_AXIS));
    offSubPanel.setBackground(panelColor);

    offTitle.setHorizontalAlignment(SwingConstants.CENTER);
    offMenu.setPreferredSize(offMenuSize);
    offMenu.setMinimumSize(offMenuSize);
    offMenu.setMaximumSize(offMenuSize);
    offMenu.setAlignmentX(Component.CENTER_ALIGNMENT);
    offMenu.setBackground(panelColor);
    offChart.setAlignmentX(Component.CENTER_ALIGNMENT);
    offChart.setBorder(BorderFactory.createLineBorder(Color.BLACK));

    offSubPanel.add(Box.createRigidArea(new Dimension(0, 5)));
    offSubPanel.add(Box.createVerticalGlue());
    offSubPanel.add(offMenu, BorderLayout.CENTER);
    offSubPanel.add(Box.createRigidArea(new Dimension(0, 1)));
    offSubPanel.add(offChart, BorderLayout.PAGE_END);
    offSubPanel.add(Box.createVerticalGlue());

    offPanel.add(offTitle, BorderLayout.PAGE_START);
    offPanel.add(offSubPanel, BorderLayout.CENTER);

    chartsPanel.add(defPanel);
    chartsPanel.add(offPanel);


    // Metrics panel:

    JPanel        metricsPanel = new JPanel();
    BorderLayout  metLO = new BorderLayout();
    metLO.setVgap(8);
    metricsPanel.setLayout(metLO);
    metricsPanel.setBorder(paneBorder);
    metricsPanel.setBackground(panelColor);

    JLabel  metricsLabel = new JLabel("OPTIMIZER METRICS");
    metricsLabel.setHorizontalAlignment(SwingConstants.CENTER);

    JPanel  dataPanel = new JPanel();
    dataPanel.setLayout(new BoxLayout(dataPanel, BoxLayout.Y_AXIS));
    dataPanel.setBorder(BorderFactory.createEmptyBorder(5, 0, 0, 0));
    dataPanel.setBackground(panelColor);

    JPanel  scorePanel = new JPanel();
    scorePanel.setLayout(new BoxLayout(scorePanel, BoxLayout.X_AXIS));
    scorePanel.setBackground(panelColor);
    scorePanel.setAlignmentX(0);
    scorePanel.add(new JLabel("COA Score  "));
    scorePanel.add(scoreData);
    scorePanel.add(new JLabel("  %"));

    JPanel  probPanel = new JPanel();
    probPanel.setLayout(new BoxLayout(probPanel, BoxLayout.X_AXIS));
    probPanel.setBackground(panelColor);
    probPanel.setAlignmentX(0);
    probPanel.add(new JLabel("Probability of Success  "));
    probPanel.add(probData);
    probPanel.add(new JLabel("  %"));

    JPanel  ratioPanel = new JPanel();
    ratioPanel.setLayout(new BoxLayout(ratioPanel, BoxLayout.X_AXIS));
    ratioPanel.setBackground(panelColor);
    ratioPanel.setAlignmentX(0);
    ratioPanel.add(new JLabel("Effects Ratio  "));
    ratioPanel.add(ratioData);
    ratioPanel.add(new JLabel("  of  "));
    ratioPanel.add(ofData);

    JPanel  simPanel = new JPanel();
    simPanel.setLayout(new BoxLayout(simPanel, BoxLayout.X_AXIS));
    simPanel.setBackground(panelColor);
    simPanel.setAlignmentX(0);
    simPanel.add(new JLabel("COA Similarity  "));
    simPanel.add(simData);
    simPanel.add(new JLabel("  %"));

    dataPanel.add(scorePanel);
    dataPanel.add(Box.createRigidArea(new Dimension(0, 2)));
    dataPanel.add(probPanel);
    dataPanel.add(Box.createRigidArea(new Dimension(0, 2)));
    dataPanel.add(ratioPanel);
    dataPanel.add(Box.createRigidArea(new Dimension(0, 2)));
    dataPanel.add(simPanel);

    // NOTE -- set values this way:
    scoreData.setText("83");
    probData.setText("95");
    ratioData.setText("7");
    ofData.setText("7");
    simData.setText("87");

    metricsPanel.add(metricsLabel, BorderLayout.PAGE_START);
    metricsPanel.add(dataPanel, BorderLayout.CENTER);


    // Map panel:

    JPanel  mapPanel = new JPanel();
    mapPanel.setLayout(new BorderLayout());
    mapPanel.setBorder(paneBorder);
    mapPanel.setBackground(panelColor);

    ImageIcon  mapIcon = new ImageIcon("images/navmap3.jpg");
    JButton    mapBtn = new JButton("Event Injector");

    mapBtn.setBackground(panelColor);
    mapPanel.add(new JLabel(mapIcon), BorderLayout.CENTER);
    //mapPanel.add(mapBtn, BorderLayout.PAGE_END);


    // Main panel:

    mainPanel.setLayout(new BorderLayout());
    mainPanel.setBorder(new ShadedBorder(frameColor, shadowColor, 7));
    mainPanel.setBackground(panelColor);

    leftMainPanel.setLayout(new BorderLayout());
    leftMainPanel.setBackground(panelColor);

    rightMainPanel.setLayout(new BorderLayout());
    rightMainPanel.setBackground(panelColor);

    leftMainPanel.add(ctlPanel, BorderLayout.LINE_START);
    leftMainPanel.add(statusPanel, BorderLayout.CENTER);
    leftMainPanel.add(chartsPanel, BorderLayout.LINE_END);

    rightMainPanel.add(metricsPanel, BorderLayout.LINE_START);
    rightMainPanel.add(mapPanel, BorderLayout.CENTER);

    mainPanel.add(leftMainPanel, BorderLayout.LINE_START);
    mainPanel.add(rightMainPanel, BorderLayout.CENTER);

    return mainPanel;
  }

  //----------------------------------------------------------------------------

  JButton  createControlButton(String title, String command)
  {
    ActionListener  btnListener = new ActionListener()
    { 
      public void actionPerformed(ActionEvent e)
      { 
        try
        {
          Runtime  rt = Runtime.getRuntime();
          String   cmd = "launchPlanning.sh";
          Process  proc = rt.exec(e.getActionCommand());
        }
        catch(Exception rte) { };
      }
    };

    Dimension  btnSize = new Dimension(100, 30);
    JButton    btn = new JButton(title);

    btn.setHorizontalAlignment(SwingConstants.CENTER);
    btn.setBackground(panelColor);
    btn.setPreferredSize(btnSize);
    btn.setMinimumSize(btnSize);
    btn.setMaximumSize(btnSize);
    btn.setActionCommand(command);
    btn.addActionListener(btnListener);

    return btn;
  }

  //----------------------------------------------------------------------------

  JPanel  createIconLabelBlankPanel(JLabel icon, String str, JLabel label)
  {
    JPanel  pnl = new JPanel();
    pnl.setLayout(new BoxLayout(pnl, BoxLayout.X_AXIS));
    pnl.setBackground(panelColor);
    pnl.setAlignmentX(0);

    pnl.add(icon);
    pnl.add(new JLabel(str + "  "));
    pnl.add(label);

    return pnl;
  }

  //----------------------------------------------------------------------------

  protected JPanel createAppsPanel()
  {
    JPanel  mainPanel = new JPanel();

//    mainPanel.setLayout(new GridLayout(1, 2, 15, 0)); // r, c, hg, vg
    mainPanel.setLayout(new GridLayout(1, 2, 8, 0)); // r, c, hg, vg
    mainPanel.setBackground(frameColor);

    flashPanel.setBorder(new ShadedBorder(frameColor, shadowColor, 7));
    flashPanel.addMouseListener( new MouseListener() {
	public void mouseClicked( MouseEvent e) {
	   try {
	     Runtime rt = Runtime.getRuntime();
	     String cmd = "mozilla AQFlash.swf";
             Process proc = rt.exec(cmd);
	   }
	   catch(Exception rte) { };
	 }
	public void mousePressed( MouseEvent e) { }
	public void mouseReleased( MouseEvent e) { }
	public void mouseEntered( MouseEvent e) { }
	public void mouseExited( MouseEvent e) { }
    } );

    amiPanel.setBorder(new ShadedBorder(frameColor, shadowColor, 7));
    amiPanel.setBackground(panelColor);
    amiPanel.addMouseListener( new MouseListener() {
	public void mouseClicked( MouseEvent e) {
	   try {
	     Runtime rt = Runtime.getRuntime();
	     String cmd = "VisualKB.sh";
             Process proc = rt.exec(cmd);
	   }
	   catch(Exception rte) { };
	}
	public void mousePressed( MouseEvent e) { }
	public void mouseReleased( MouseEvent e) { }
	public void mouseEntered( MouseEvent e) { }
	public void mouseExited( MouseEvent e) { }
    } );

    mainPanel.add(flashPanel);
    mainPanel.add(amiPanel);

    return mainPanel;
  }

  //----------------------------------------------------------------------------

  protected JPanel createTextPanel()
  {
    JLabel  goldBar = new JLabel(new ImageIcon("images/goldOval.gif"));
    JLabel  greenBar = new JLabel(new ImageIcon("images/greenOval.gif"));
    JLabel  purpleBar = new JLabel(new ImageIcon("images/purpleOval.gif"));
    JLabel  redBar = new JLabel(new ImageIcon("images/redOval.gif"));
    JLabel  yellowBar = new JLabel(new ImageIcon("images/yellowOval.gif"));

    JPanel  mainPanel = new JPanel();
    JPanel  outerPanel = new JPanel();
    JPanel  iconPanel = new JPanel();
    JPanel  textPanel = new JPanel();
    JPanel  greenPanel = new JPanel();
    JPanel  goldPanel = new JPanel();
    JPanel  yellowPanel = new JPanel();
    JPanel  redPanel = new JPanel();
    JPanel  purplePanel = new JPanel();

    Font    textFont = new Font("Serif", Font.PLAIN, 12);
    Font    commentFont = new Font("Serif", Font.ITALIC, 12);
    Color   designColor = new Color(135, 135, 135);


    mainPanel.setLayout(new BorderLayout());
    mainPanel.setBorder(new ShadedBorder(frameColor, shadowColor, 7));
    mainPanel.setBackground(designColor);

    outerPanel.setLayout(new BorderLayout());
    outerPanel.setBorder(BorderFactory.createEmptyBorder(6, 6, 6, 6));
    outerPanel.setBackground(designColor);

    iconPanel.setLayout(new GridLayout(5, 1, 0, 1));
    iconPanel.setBorder(BorderFactory.createEmptyBorder(3, 0, 3, 0));
    iconPanel.setBackground(designColor);

    textPanel.setLayout(new GridLayout(5, 1, 0, 1));
    textPanel.setBorder(BorderFactory.createEmptyBorder(3, 2, 3, 0));
    textPanel.setBackground(Color.WHITE);

    iconPanel.add(greenBar);
    iconPanel.add(goldBar);
    iconPanel.add(yellowBar);
    iconPanel.add(redBar);
    iconPanel.add(purpleBar);

    greenPanel.setBackground(Color.WHITE);
    goldPanel.setBackground(Color.WHITE);
    yellowPanel.setBackground(Color.WHITE);
    redPanel.setBackground(Color.WHITE);
    purplePanel.setBackground(Color.WHITE);

    greenPanel.setLayout(new FlowLayout(FlowLayout.LEFT, 2, 0));
    goldPanel.setLayout(new FlowLayout(FlowLayout.LEFT, 2, 0));
    yellowPanel.setLayout(new FlowLayout(FlowLayout.LEFT, 2, 0));
    redPanel.setLayout(new FlowLayout(FlowLayout.LEFT, 2, 0));
    purplePanel.setLayout(new FlowLayout(FlowLayout.LEFT, 2, 0));

    greenText.setFont(textFont);
    goldText.setFont(textFont);
    yellowText.setFont(textFont);
    redText.setFont(textFont);
    purpleText.setFont(textFont);
    greenComment.setFont(commentFont);
    goldComment.setFont(commentFont);
    yellowComment.setFont(commentFont);
    redComment.setFont(commentFont);
    purpleComment.setFont(commentFont);

    greenComment.setForeground(Color.BLUE);
    goldComment.setForeground(Color.BLUE);
    yellowComment.setForeground(Color.BLUE);
    redComment.setForeground(Color.BLUE);
    purpleComment.setForeground(Color.BLUE);

    greenText.setText("Utilize Japanese ISR...");
    greenComment.setText(" Intelligence Pending...");

    redText.setText("Special Ops Mission...");
    redComment.setText(" Seal Team Inserted to Neutralize Missile Launch Site...");

    yellowText.setText("Integrate Missile Defense for potential launch...");
    yellowComment.setText(" Aegis Standard Missile in Position...");

    goldText.setText("Increase Surveillance and Reconnaisance...");
    goldComment.setText(" SIA and Japanese Multi-Spectral Assets in place...");

    purpleText.setText("Conventional Global Strike...");
    purpleComment.setText(" F117A to Eliminate TEL near DMZ...");

    greenPanel.add(greenText);
    greenPanel.add(greenComment);
    goldPanel.add(goldText);
    goldPanel.add(goldComment);
    yellowPanel.add(yellowText);
    yellowPanel.add(yellowComment);
    redPanel.add(redText);
    redPanel.add(redComment);
    purplePanel.add(purpleText);
    purplePanel.add(purpleComment);

    textPanel.add(greenPanel);
    textPanel.add(goldPanel);
    textPanel.add(yellowPanel);
    textPanel.add(redPanel);
    textPanel.add(purplePanel);

    outerPanel.add(iconPanel, BorderLayout.LINE_START);
    outerPanel.add(textPanel, BorderLayout.CENTER);

    mainPanel.add(outerPanel, BorderLayout.CENTER);

    return mainPanel;
  }

  //----------------------------------------------------------------------------

  protected JPanel createFlash2Panel()
  {
    BorderLayout  fLO = new BorderLayout();
    JPanel        mainPanel = new JPanel();

    fLO.setVgap(4);
    mainPanel.setLayout(fLO);
    mainPanel.setBackground(frameColor);

    JPanel  textPanel = createTextPanel();

    timeline.setBorder(new ShadedBorder(frameColor, shadowColor, 7));

    mainPanel.add(textPanel, BorderLayout.CENTER);
    mainPanel.add(timeline, BorderLayout.PAGE_END);

    return mainPanel;
  }

  //----------------------------------------------------------------------------

  private JLabel createDataLabel()
  {
    Dimension  dataSize = new Dimension(30, 18);

    JLabel  lbl = new JLabel();
    lbl.setPreferredSize(dataSize);
    lbl.setMinimumSize(dataSize);
    lbl.setMaximumSize(dataSize);
    lbl.setBorder(BorderFactory.createLoweredBevelBorder());
    lbl.setOpaque(true);
    lbl.setBackground(Color.WHITE);

    return lbl;
  }

  //----------------------------------------------------------------------------

  public static void main (String args[])
  {
    JFrame frame = new JFrame ("Dashboard");
    JPanel pane = new Dashboard();

    frame.addWindowListener(new WindowAdapter()
    {
      public void windowClosing(WindowEvent e)
      {
        System.exit(0);
      }
    });

    frame.getContentPane().add (pane, BorderLayout.CENTER);
    frame.pack();
    frame.show();
  }
}

