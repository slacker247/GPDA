package com.appliedminds.hmv;

import com.appliedminds.core.config.*;
import com.appliedminds.core.widgets.BumpPanel;
import com.appliedminds.core.widgets.DockManager;
import com.appliedminds.hmv.plaf.HMVLookAndFeel;
import com.appliedminds.martini.*;
import com.appliedminds.martinix.ViewportPanAndScanAdapter;
import com.appliedminds.martinix.ui.FontCache;
import com.appliedminds.martinix.ui.TextScreenData;
import com.appliedminds.martinix.fader.*;
import com.appliedminds.martinix.gapp.*;
import com.appliedminds.jam.AccessKBDlg;
import com.appliedminds.martini.io.LayoutLoader;

import java.net.URLDecoder;
import java.awt.*;
import java.awt.event.*;
import java.awt.geom.*;
import java.awt.font.FontRenderContext;
import java.awt.font.TextLayout;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Date;
import java.util.logging.ConsoleHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.border.*;
import javax.swing.plaf.metal.*;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.JTextComponent;



/**
 * <b>The Belief Network Editor</b>.
 *
 * @author daepark@apmindsf.com, will@apmindsf.com, darin@apmindsf.com
 */
public class HMV
  extends GAppFrame
  implements Configurable,
             GraphChangeListener
{
  public static final String LOG_NAME = "appliedminds.HMV";

  protected static final String PROP_SIMDATA = "simdata";
  protected static final String HOME_PROP = "hmv.home";

  // node collapse animation styles
  protected static final int DECELERATE = 10;
  protected static final int BOUNCE = 12;

  /* The color for the grip-bar in the panner window */
  private static final Color GB_COLOR = new Color(0xB2, 0xB2, 0xB2);

  private static final String SPACER_ICON_PATH = ("com/appliedminds/hmv/resources/spacer.png");
  private static final String HMV_CONFIG  = "hmv.ser.txt";
  private static File _hmvHome;

  static {
    //
    // Register support handlers
    //

    // Handler for type "document"
    SupportHandlerManager.registerSupportHandler("document",
                                                 DocumentSupportHandler.class);

    // Handler for type "graph"
    SupportHandlerManager.registerSupportHandler("graph",
                                                 GraphSupportHandler.class);

    try {
      _hmvHome = findHome();
    }
    catch (IOException e) {
      System.err.println("Could not locate HMV home directory. " +
                         "Check the system property " + HOME_PROP +
                         " and try again. Error: " + e.toString());
      System.exit(0);
    }
  }

  protected JMenuItem _runSim;
  protected JMenuItem _sfSim; // step forward
  protected JMenuItem _sbSim; // step backward
  protected JMenuItem _pauseSim;
  protected JMenuItem _contSim;
  protected JMenuItem _stopSim;
  protected JMenuItem _clearSims;
  protected RunSimulationCommand _simCommand;
  protected JLabel _timeCodeLabel;
  protected JCheckBoxMenuItem _panAndScanItem;
  protected boolean _useAnimation = true;

  private JMenuItem _newNodeItem;
  private JCheckBoxMenuItem _animateItem;

  private HMVSelectionManager _selectionManager;

  /**
   * system property describing the location of
   * hmv.ser.txt
   */
  private FaderUI2 _graphUI;

  FaderUIPrefs2 _graphUIPrefs;

  // default to real time slider mode
  private boolean _realtimeSliderMode = true;

  //
  // Configuration stuff
  //

  // current configuration
  private HMVConfigBean2 _configBean;

  // configuration to store original configuration in case of
  // cancellation
  private HMVConfigBean2 _tmpConfigBean;

  private boolean _changedConfiguration = false;

  private ConfigPanel _controlPanel;

  private JFrame _prefWindow;

  private FontCache _defaultFontCache = null;

  private Logger _logger;

  private HMVSelectTool2 _hmvTool2;

  // for slider values
  private DrawableNode _editingNode;
  private JTextField _textField;

  // for node labels
  private DrawableGraphElement _slidingElement;
  private JTextArea _textArea;

  // contains the pan and scan panel
  private DockManager _dockMgr = new DockManager(this);
  private BumpPanel _panner;
  private JWindow _pannerWindow = null;

  // updates panner position depending on the visibility of the scroll
  // bars.
  private ChangeListener _myViewportListener = null;


  /**
   * Main
   */
  public static final void main(String[] args)
  {
    Level logLevel = Level.FINEST;

    //
    // Configure logging to the console
    //
    Logger.getLogger("").setLevel(logLevel);
    Handler[] handlers = Logger.getLogger("").getHandlers();
    for(int i=0; i < handlers.length; ++i) {
      Logger.getLogger("").removeHandler(handlers[i]);
    }
    Handler console = new ConsoleHandler();
    console.setLevel(logLevel);
    Logger.getLogger("").addHandler(console);

    final GAppFrame hmv = new HMV();
    hmv.initGAppFrame(false);

    // show the frame
    hmv.pack();
    hmv.setBounds(100, 100, 700, 700);

    // on startup, start with a new empty graph.
    hmv.addComponentListener(new ComponentAdapter() {
        private boolean __startUp = true;
        public void componentShown(ComponentEvent e) {
          if (__startUp) {
            hmv.doClickFileNew();
            __startUp = false;
          }
        }
      });

    hmv.setVisible(true);
  }


  /**
   * Constructor
   */
  public HMV()
  {
    //super(".:BNE:.", false, true, false, false,
          //true, false, false, false, false, false);
    super(".:BNE:.",
          false,      // sidebar
          true,      // menubar
          true,      // toolbar
          true,      // statusbar
          true,      // selectTool
          true,      // textTool
          true,      // zoomTool
          true,      // handTool
          true,      // nodeTool
          true       // edgeTool
          );
    init();
  }

  private void init()
  {
    _logger = Logger.getLogger(getLogName());
    updateLookAndFeel();
    _graphUIPrefs = new FaderUIPrefs2();
    // use curved edges
    _graphUIPrefs.setCurvedLines(false);
    
    _graphUI = new FaderUI2(_graphUIPrefs);

    _selectionManager = new FaderSelectionManager();
    setIsScalable(false);
  }


  /**
   * Get the FaderUIPrefs used by the FaderUI.
   */
  protected FaderUIPrefs2 getFaderUIPrefs() {
    return(_graphUIPrefs);
  }


  /**
   * Get the Logger object's name used by HMV.
   */
  protected String getLogName() {
    return(LOG_NAME);
  }

  /**
   * Get the HMV logger.
   *
   * @see #getLogName
   */
  protected Logger getLogger() {
    return (_logger);
  }


  /**
   * Erase the graph and redraw it.
   */
  private void redraw() {
    getGraphPanel().redraw();
  }


  //////////////////////////////////////
  // begin GraphChangeListener interface
  //
  public void graphChanged()
  {
    setDocumentState(DOC_MODIFIED);
    if (_panner != null) {
      ((PanAndScanPanel)_panner.getContents()).setDirty(true);
      _panner.repaint();
    }
  }
  // end GraphChangeListener interface
  //////////////////////////////////////


  ///////////////////////////////
  // begin Configurable interface
  //

  public ConfigBean getConfigBean() {
    // copy the original in case of cancellation
    _tmpConfigBean = new HMVConfigBean2(_configBean);

    return (_configBean);
  }

  public void setConfigBean(ConfigBean bean) {
    if (_tmpConfigBean == null) {
      // copy the original in case of cancellation
      _tmpConfigBean = new HMVConfigBean2(_configBean);
    }

    _configBean = (HMVConfigBean2) bean;
  }

  public void okConfig() {
    // update current configuration
    _controlPanel.updateBeans();

    // close the control panel
    _prefWindow.setVisible(false);

    // apply new configuration
    applyConfiguration(_configBean);

    // save current configuration
    saveConfigBean(_configBean);
  }

  public void applyConfig() {
    // update current configuration
    _controlPanel.updateBeans();

    // try out the new configuration
    applyConfiguration(_configBean);
  }

  public void cancelConfig() {
    // close the control panel
    _prefWindow.setVisible(false);

    // revert back to our original configuration
    _configBean = _tmpConfigBean;
    if (_changedConfiguration) {
      // reset to original configuration
      applyConfiguration(_configBean);
    }
  }

  //
  // end Configurable interface
  /////////////////////////////


  /**
   * GAppFrame framework method. Do nothing.
   */
  protected void setDrawableGraphHook()
  {
    // make sure the text edit widgets are invisible
    hideTextArea(_textArea);
    _textField.setVisible(false);

    _hmvTool2.setState(_hmvTool2.STATE_IDLE, null);

    _newNodeItem.setEnabled(true);

    // load any simulation files
    boolean hasSimData = false;

    for (NodeIterator itr=getGraphPanel().getDrawableGraph().nodesIterator();
         itr.hasNext();)
    {
      DrawableNode node = itr.next();
      Object simFileProp = node.getPropertyObj(HMV.PROP_SIMDATA);
      if (simFileProp instanceof String) {
        String simFileName = (String) simFileProp;
        try {
          simFileName = URLDecoder.decode(simFileName, "ISO-8859-1");
        }
        catch (UnsupportedEncodingException e) {
          e.printStackTrace(); // continue with the ascii file name
        }
        try {
          SimulationDataSet dataSet =
            SimulationDataSet.createSimulationDataSet(new File(simFileName));
          node.setProperty(HMV.PROP_SIMDATA, dataSet);
          hasSimData = true;
        }
        catch (InvalidSimFileException e) {
          System.err.println(e.getMessage()); // print message and keep going
          hasSimData = false;
        }
        catch (IOException e) {
          System.err.println(e.getMessage()); // print message and keep going
          hasSimData = false;
        }
      }
    }

    if (hasSimData) {
      prepareSimulation();  // reset the simulation timeline
    }
    else {
      _runSim.setEnabled(false);
      _clearSims.setEnabled(false);
      if (_timeCodeLabel != null) {
        _timeCodeLabel.setVisible(false);
      }
    }
  }


  /**
   * HMV only accepts acyclic directed graphs.
   */
  public boolean isGraphValid(DrawableGraph graph) {
    boolean isCyclic = GraphUtil.isCyclic(graph);

    if (isCyclic) {
      // TODO: popup warning message
      _logger.warning("HMV does not support cyclic graphs");
    }

    return (!isCyclic);
  }


  public boolean getRealTimeSliderMode() {
    return (_realtimeSliderMode);
  }


  public void setRealTimeSliderMode(boolean r) {
    _realtimeSliderMode = r;
  }


  /**
   * @return the HMVSelectionManager implementation that should be used to
   * keep track of selected nodes and edges
   */
  public HMVSelectionManager getSelectionManager() {
    return _selectionManager;
  }


  /**
   * GAppFrame framework method. HMV does not use a transformer.
   */
  protected GTransformer getTransformer() {
    return(null);
  }


  /**
   * Add HMV specific GUI components to the app.
   */
  protected void initGUIHook()
  {
    JMenu simMenu = initSimulationMenu();
    getJMenuBar().add(simMenu);

    JMenu settingMenu = new JMenu("Settings");

    _animateItem = new JCheckBoxMenuItem("Use Animation", _useAnimation);
    _animateItem.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          _useAnimation = _animateItem.getState();
        }
      });
    settingMenu.add(_animateItem);

    JMenuItem menuItem = new JMenuItem("Preferences...");
    menuItem.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          showConfigPanel();
        }
      });

    settingMenu.add(menuItem);

    getJMenuBar().add(settingMenu);

    // disable some menu items
    getActions().getFitInWindowAction().setEnabled(false);

    // use hierarchical for default layout
    //    useLayout(GActions.HIERARCHICAL_LAYOUT);
  }


  private JMenu initSimulationMenu()
  {
    JMenu simMenu = new JMenu("Simulation");

    _runSim = new JMenuItem("Start");
    _simCommand = new RunSimulationCommand(_hmvTool2);
    _runSim.addActionListener(new HMVCommandAction(_simCommand));
    _runSim.setEnabled(false);
    simMenu.add(_runSim);

    _sfSim = new JMenuItem("Step Forward");
    _sfSim.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          _simCommand.stepForward();
        }
      });
    _sfSim.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F,
                                                 ActionEvent.CTRL_MASK));
    _sfSim.setEnabled(false);
    simMenu.add(_sfSim);

    _sbSim = new JMenuItem("Step Back");
    _sbSim.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          _simCommand.stepBackward();
        }
      });
    _sbSim.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_B,
                                                 ActionEvent.CTRL_MASK));
    _sbSim.setEnabled(false);
    simMenu.add(_sbSim);

    _pauseSim = new JMenuItem("Pause");
    _pauseSim.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          _simCommand.pauseAnimation();
        }
      });
    _pauseSim.setEnabled(false);
    simMenu.add(_pauseSim);

    _contSim = new JMenuItem("Continue");
    _contSim.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          _simCommand.continueAnimation();
        }
      });
    _contSim.setEnabled(false);
    simMenu.add(_contSim);

    _stopSim = new JMenuItem("Stop");
    _stopSim.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          _simCommand.haltAnimation();
        }
      });
    _stopSim.setEnabled(false);
    simMenu.add(_stopSim);

    _clearSims = new JMenuItem("Clear All Simulation Data");
    _clearSims.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          clearAllSimData();
        }
      });
    _clearSims.setEnabled(false);
    simMenu.add(_clearSims);

    return (simMenu);
  }

private String g_Story = "";
private String g_Domain = "";
  protected void addToFileMenuHook(JMenu filemenu)
  {
    _newNodeItem = new JMenuItem("Create new node...");
    _newNodeItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N,
                                                       ActionEvent.CTRL_MASK |
                                                       ActionEvent.SHIFT_MASK));
    NewNodeCommand cmd = new NewNodeCommand(_hmvTool2, null);
    _newNodeItem.addActionListener(new HMVCommandAction(cmd));
    _newNodeItem.setEnabled(false);
    filemenu.insert(_newNodeItem, 3);

    //if(debug) System.out.println("In BNE_View.addToFileMenuHook():");
        javax.swing.JMenuItem jM_SaveToKB = new javax.swing.JMenuItem();
        jM_SaveToKB.setText("Save To KB");
        jM_SaveToKB.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                AccessKBDlg accessKB = new AccessKBDlg();
                accessKB.DrawableGraphToStory(getGraphPanel().getDrawableGraph(), g_Story, g_Domain);
            }
        });

        filemenu.add(jM_SaveToKB);

        javax.swing.JMenuItem jM_OpenFromKB = new javax.swing.JMenuItem();
        jM_OpenFromKB.setText("Open From KB");
        jM_OpenFromKB.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                com.appliedminds.jam.AccessKBDlg KBAccess = new com.appliedminds.jam.AccessKBDlg();

                KBAccess.setModal(true);
                KBAccess.showKB();

                if(KBAccess.OK) {
                    loadStoryFromKB(KBAccess.StoryName, KBAccess.Domain);
                }
            }
        });

        filemenu.add(jM_OpenFromKB);
    }
    
    /** Loads a Belief network from the KB
     */
    public void loadStoryFromKB(String Story, String Domain)
    {
        com.appliedminds.jam.AccessKBDlg KBAccess = new com.appliedminds.jam.AccessKBDlg();

        g_Story = KBAccess.StoryName = Story;
        g_Domain = KBAccess.Domain = Domain;

        com.appliedminds.martini.io.LayoutLoader loader = new GLayoutLoader();
        DrawableGraph graph = KBAccess.StoryToGMLFile();

        //Design the layout of the graph
        Rectangle2D rect[] = new Rectangle2D.Float[100];
        NodeIterator temp = graph.nodesIterator();
        long y = 20;
        int count = 0;
        long x[] = new long[10];
        for(int i = 0; i < 10; i++)
            x[i] = 20;
        String Levels = graph.getProperty("Levels");
        String Level[] = new String[10];
        for(int i = 0; i < 10; i++)
            Level[i] = "";
        int nextL = 0;
        
        if(Levels != null)
            for(int i = 0; i < Levels.length(); i++)
            {
                if(',' == Levels.charAt(i))
                {
                    nextL++;
                    continue;
                }
                Level[nextL] += Levels.charAt(i);
            }
        while(temp.hasNext())
        {
            com.appliedminds.martini.DrawableNode n = temp.next();
            for(int i = 0; i <= nextL; i++)
            {
                if(Level[i].equals(n.getProperty("level")))
                {
                    System.out.println("Level: " + Level[i]);
                    y = (i + 1)*100;
                    rect[count] = new Rectangle2D.Float();
                    rect[count].setFrame(x[i], y, 225, 144);
                    loader.loadNodeBounds(n, rect[count]);
                    x[i] += 300;
                    count++;
                }
            }
        }

        // we cannot call gapp's setDrawableGraph() method
        // from here or else we would get into an infinite
        // event loop
        getGraphPanel().stopAnimation();
        getGraphPanel().stopElementEdit();
        getGraphPanel().setDrawableGraph(graph);
        setDrawableGraphHook();

        // sets graph elements' coordinates
        loadLayout(loader);

        fitGraphInWindow();
        setDocumentState(GAppFrame.DOC_UNMODIFIED);
        fileOpenHook();
        getGraphPanel().repaint();
    //    return (_fileOpenKBAction);
    }

    
    public void loadStory(DrawableGraph graph)
    {
        LayoutLoader loader = new GLayoutLoader();

        //Design the layout of the graph
        Rectangle2D rect[] = new Rectangle2D.Float[100];
        NodeIterator temp = graph.nodesIterator();
        long y = 20;
        int count = 0;
        long x[] = new long[10];
        
        for(int i = 0; i < 10; i++)
            x[i] = 20;
        String Levels = graph.getProperty("Levels");
        String Level[] = new String[10];
        for(int i = 0; i < 10; i++)
            Level[i] = "";
        Level[0] = "0";
        Level[1] = "1";
        Level[2] = "2";
        Level[3] = "3";
        Level[4] = "4";
        Level[5] = "5";
        Level[6] = "6";
        /*
        int nextL = 0;
        for(int i = 0; i < Levels.length(); i++)
        {
            if(',' == Levels.charAt(i))
            {
                nextL++;
                continue;
            }
            Level[nextL] += Levels.charAt(i);
        }
         **/
        int xx = 0;
        while(temp.hasNext())
        {
            com.appliedminds.martini.DrawableNode n = temp.next();
            for(int i=0; i<Integer.valueOf(Levels).intValue(); i++)
            {
                if(Level[i].equals(n.getProperty("level")))
                {
                    y = (i + 1)*100;
                    xx = (int)(20.0 + (Double.valueOf(n.getProperty("branch")).doubleValue()*200.0));
                    rect[count] = new Rectangle2D.Float();
                    rect[count].setFrame(xx, y, 225, 144);
                    loader.loadNodeBounds(n, rect[count]);                    
                    //x[i] = xx; //+= 300;
                    count++;
                }
            }
        }

        // we cannot call gapp's setDrawableGraph() method
        // from here or else we would get into an infinite
        // event loop
        getGraphPanel().stopAnimation();
        getGraphPanel().stopElementEdit();
        getGraphPanel().setDrawableGraph(graph);
        setDrawableGraphHook();

        // sets graph elements' coordinates
        loadLayout(loader);

        //fitGraphInWindow();
        setDocumentState(GAppFrame.DOC_UNMODIFIED);
        fileOpenHook();
        getGraphPanel().repaint();
    //    return (_fileOpenKBAction);
    }    


  protected void addToViewMenuHook(JMenu viewMenu)
  {
    _panAndScanItem = new JCheckBoxMenuItem("Show Navigator Tool");
    _panAndScanItem.setState(true);
    _panAndScanItem.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          if (_panner != null) {
            if (_dockMgr.isDocked(_panner)) {
              positionPannerWindow();
              _pannerWindow.setVisible(false);//_panAndScanItem.getState());
            }
          }
        }
      });
    viewMenu.insert(_panAndScanItem, 0);
  }


  protected GraphPanel initGraphPanel()
  {
    GraphPanel p = new GraphPanel(_graphUI);
    p.setBackground(Color.white);
    p.setLayout(new SlackerLayout()); // give the app full control of layout
    initTextFields(p);
    initConfigBean();
    if (_configBean != null)
    {
      _configBean.applyConfiguration(this);
    }

    return(p);
  }

  private void initTextFields(GraphPanel p)
  {
    _textField = new JTextField(3);
    _textField.setDocument(new DigitOnlyDocument(3));
    _textField.setHorizontalAlignment(SwingConstants.RIGHT);
    _textField.addActionListener(new ActionListener()
      {
        public void actionPerformed(ActionEvent e)
        {
          endSliderValueEdit();
        }
      });
    _textField.addFocusListener(new FocusAdapter()
      {
        public void focusLost(FocusEvent e)
        {
          if (e.getComponent().isVisible() && !e.getComponent().hasFocus()) {
            e.getComponent().requestFocusInWindow();
          }
        }
      });

    int fontSize = (int)
      p.mapWorldToViewport(_graphUIPrefs.getSliderValueFontSize());
    if (_defaultFontCache == null) {
      _defaultFontCache = new FontCache(_graphUIPrefs.getFontResourcePath());
    }
    _textField.setFont(_defaultFontCache.getFontBySize(fontSize));
    p.add(_textField);
    _textField.setVisible(false);


    // text area for node edits
    _textArea = new JTextArea(new AMDocument());

    fontSize = (int)
      p.mapWorldToViewport(_graphUIPrefs.getNodeDrawProps(false, 0).getFontSize());
    _textArea.setFont(_defaultFontCache.getFontBySize(fontSize));
    _textArea.setLineWrap(true);
    _textArea.setWrapStyleWord(true);
    _textArea.addFocusListener(new FocusAdapter()
      {
        public void focusLost(FocusEvent e)
        {
          if ((_editingNode != null) &&
              FaderUtil.isInEdit(_editingNode) &&
              e.getComponent().isVisible() &&
              !e.getComponent().hasFocus()) {
            e.getComponent().requestFocusInWindow();
          }
        }
      });

    _textArea.addComponentListener(new ComponentAdapter()
      {
        public void componentResized(ComponentEvent e)
        {
          if (_editingNode != null) {
            FaderUtil.setNodeLabel(_editingNode, _textArea.getText());
            getGraphPanel().repaint();

            DrawProps nodeProps = _graphUIPrefs.getNodeDrawProps(false, false);
            int maxWidth = nodeProps.getMaxLabelWidth();

            // todo: refactor to a method
            HitTester hit = null;
            for (Iterator itr=getGraphPanel().getHitTesters(_editingNode);
                 itr.hasNext();)
            {
              hit = (HitTester) itr.next();
              if (hit.getDecorationId().equals(FaderUI2.DECORATION_NODE_TEXT)) {
                break;
              }
            }

            if (hit != null)
            {
              TextScreenData data = (TextScreenData) hit.getContext();
              Rectangle2D bounds = data.getBounds();
              int textHeight = (int) Math.round(bounds.getHeight());
              if ((textHeight != _textArea.getHeight()) &&
                  FaderUtil.isInEdit(_editingNode))
              {
                updateTextAreaPosition(_editingNode, _textArea,
                                       data.getBounds(), maxWidth);
              }
            }
          }
        }
      });

    p.add(_textArea);
    _textArea.setVisible(true);
    hideTextArea(_textArea);
  }


  protected GFilterTable getNodeFilterTable() {
    return(null);
  }


  protected GFilterTable getEdgeFilterTable() {
    return(null);
  }


  protected void addDrawableGraphMouseEventListeners() {
    //
    // SLIDER_KNOB
    //
    getGraphPanel().addDrawableGraphMouseEventListener
      (FaderUI2.DECORATION_SLIDER_KNOB,
       new DrawableGraphMouseEventListener() {
         public boolean handleDrawableGraphMouseEvent(DrawableGraphMouseEvent e)
         {
           return(_hmvTool2.handleSliderKnobMouseEvent(e));
         }
       });
    //
    // EDGE_BUBBLE
    //
    getGraphPanel().addDrawableGraphMouseEventListener
      (FaderUI2.DECORATION_EDGE_BUBBLE,
       new DrawableGraphMouseEventListener() {
         public boolean handleDrawableGraphMouseEvent(DrawableGraphMouseEvent e)
         {
           return(_hmvTool2.handleEdgeBubbleMouseEvent(e));
         }
       });
    //
    // NODE
    //
    getGraphPanel().addDrawableGraphMouseEventListener
      (FaderUI2.DECORATION_NODE,
       new DrawableGraphMouseEventListener() {
         public boolean handleDrawableGraphMouseEvent(DrawableGraphMouseEvent e)
         {
           return(_hmvTool2.handleNodeMouseEvent(e));
         }
       });
    //
    // NODE_TEXT
    //
    getGraphPanel().addDrawableGraphMouseEventListener
      (FaderUI2.DECORATION_NODE_TEXT,
       new DrawableGraphMouseEventListener() {
         public boolean handleDrawableGraphMouseEvent(DrawableGraphMouseEvent e)
         {
           return (_hmvTool2.handleNodeTextMouseEvent(e));
         }
       });
    //
    // NODE_ICON
    //
    getGraphPanel().addDrawableGraphMouseEventListener
      (FaderUI2.DECORATION_NODE_ICON,
       new DrawableGraphMouseEventListener() {
          public boolean handleDrawableGraphMouseEvent(DrawableGraphMouseEvent e)
         {
           return (_hmvTool2.handleNodeIconMouseEvent(e));
         }
       });
    //
    // EDGE
    //
    getGraphPanel().addDrawableGraphMouseEventListener
      (FaderUI2.DECORATION_EDGE,
       new DrawableGraphMouseEventListener() {
         public boolean handleDrawableGraphMouseEvent(DrawableGraphMouseEvent e)
         {
           return (_hmvTool2.handleEdgeMouseEvent(e));
         }
       });
    getGraphPanel().addDrawableGraphMouseEventListener
      (FaderUI2.DECORATION_SLIDER_VALUE,
       new DrawableGraphMouseEventListener() {
         public boolean handleDrawableGraphMouseEvent(DrawableGraphMouseEvent e)
         {
           return (_hmvTool2.handleSliderValueMouseEvent(e));
         }
       });
    getGraphPanel().addDrawableGraphMouseEventListener
      (FaderUI2.DECORATION_SLIDER_TOGGLE,
       new DrawableGraphMouseEventListener() {
         public boolean handleDrawableGraphMouseEvent(DrawableGraphMouseEvent e)
         {
           return (_hmvTool2.handleSliderToggleMouseEvent(e));
         }
       });

    //
    // OFFGRAPH_DECORATION_ID (whitespace hit)
    //
    getGraphPanel().addDrawableGraphMouseEventListener
      (GraphPanel.OFFGRAPH_DECORATION_ID,
       new DrawableGraphMouseEventListener() {
         public boolean handleDrawableGraphMouseEvent(DrawableGraphMouseEvent e)
         {
           return (_hmvTool2.handleHitTestMissedMouseEvent(e));
         }
       });
    //
    // register Multiple hit test listeners
    //
    getGraphPanel().addDrawableGraphMultipleSelectionEventListener
      (new DrawableGraphMultipleSelectionEventListener() {
          public void handleMultipleHitTestSuccess(DrawableGraphMultipleSelectionEvent evt)
          {
            HMVTool tool = null;
            try {
              tool = (HMVTool) getCurrentTool();
            } catch (ClassCastException shoot) {}

            if (tool != null) {
              tool.handleMultipleHitTestSuccess(evt);
            }
          }

          public void handleMultipleHitTestMissedGraph(Rectangle2D rectangle)
          {
            HMVTool tool = null;
            try {
              tool = (HMVTool) getCurrentTool();
            } catch (ClassCastException shoot) {}

            if (tool != null) {
              tool.handleMultipleHitTestMissedGraph(rectangle);
            }
          }
        });
  }

  protected Component getExtraStatusBarWidget() {
    return(null);
  }

  protected boolean mousePressedHook(MouseEvent e)
  {
    HMVTool tool = null;
    try {
      tool = (HMVTool) getCurrentTool();
    } catch (ClassCastException shoot) {}

    if (tool != null) {
      return (tool.mousePressedHook(e));
    }
    return(false);
  }

  protected void endSliderValueEdit()
  {
    if (_textField.isVisible())   // user typed in the slider value
    {
      int max, min;
      ArrayList path = new ArrayList();

      DrawableNode sourceNode;
      boolean isNode; // is the sliding element a node or an edge

      if (_slidingElement instanceof DrawableNode)
      {
        max = HMVNodeProperties.MAX_CERTAINTY_PERCENT;
        min = HMVNodeProperties.MIN_CERTAINTY_PERCENT;
        sourceNode = (DrawableNode) _slidingElement;
        isNode = true;
      }
      else {
        max = HMVEdgeProperties.MAX_INFLUENCE;
        min = HMVEdgeProperties.MIN_INFLUENCE;
        sourceNode = ((DrawableEdge)_slidingElement).getHead();
        ((DrawableEdge)_slidingElement).getTail().setHasChanged(true);
        isNode = false;
      }

      int val = FaderUtil.getSliderValue(_slidingElement);
      if (_textField.getText().trim().length() != 0)
      {
        val = (int)Float.parseFloat(_textField.getText());
      }
      if (val > max) {
        val = max;
      }
      else if (val < min) {
        val = min;
      }

      if (val != FaderUtil.getSliderValue(_slidingElement))
      {
        // we will not recalculate if the sliding element is a node,
        // other wise its new value will be overriden by any incoming
        // influencing node(s).
        HMVMath.initCalculationPath2(getGraphPanel().getDrawableGraph(),
                                     sourceNode,
                                     path,
                                     !isNode);
        FaderUtil.setSliderValue(_slidingElement, val);
        HMVMath.recalculateProbabilityUsingPath2(sourceNode,
                                                 path,
                                                 getGraphPanel());
        setDocumentState(DOC_MODIFIED);
      }

      _textField.setVisible(false);
      getGraphPanel().paintScreenBuffer();
    }
  }


  protected void endNodeLabelEdit()
  {
    if (_textArea.isVisible() && FaderUtil.isInEdit(_editingNode))
    {
      if ((_textArea.getText() != null) &&
          (_textArea.getText().trim().length() > 0)) // disallow blank label
      {
        String newLabel = _textArea.getText().trim();
        if (!newLabel.equals(FaderUtil.getNodeLabel(_editingNode))) {
          FaderUtil.setNodeLabel(_editingNode, newLabel);
          setDocumentState(DOC_MODIFIED);
        }
      }
      else
      {
        FaderUtil.setNodeLabel(_editingNode, _oldLabel);
      }
      hideTextArea(_textArea);
      FaderUtil.setInEdit(_editingNode, false);
      getGraphPanel().requestFocusInWindow();
      getGraphPanel().repaint();
    }
  }


  protected boolean mouseReleasedHook(MouseEvent e) {
    return(false);
  }


  protected void fileOpenHook()
  {
    _simCommand.haltAnimation();

    if (_panner != null) {
      _dockMgr.unregister(_panner);
      _pannerWindow.getContentPane().remove(_panner);
      _panner = null;
    }

    initPanner();
    _pannerWindow.setVisible(false);//_panAndScanItem.getState());
  }


  protected void fileNewHook()
  {
    _simCommand.haltAnimation();

    if (_panner != null) {
      _dockMgr.unregister(_panner);
      _pannerWindow.getContentPane().remove(_panner);
      _panner = null;
    }

    initPanner();
    _pannerWindow.setVisible(false);//_panAndScanItem.getState());

    clearAllSimData();
  }


  /**
   * The one and only HMV tool.
   */
  protected GTool initSelectTool(Cursor cur)
  {
    _hmvTool2 = new HMVSelectTool2(Cursor.getDefaultCursor(), this);
    _hmvTool2.addGraphChangeListener(this);
    return ((GTool)_hmvTool2);
  }


  protected GTool initTextTool(Cursor cur) {
    return null;
  }


  protected GTool initHandTool(Cursor open, Cursor closed) {
    return null;
  }


  protected GTool initZoomTool(Cursor in,
                               Cursor out,
                               Cursor marquee)
  {
    return null;
  }


  protected GTool initNodeTool(Cursor cur) {
    return null;
  }


  protected GTool initEdgeTool(Cursor cur) {
    return null;
  }


  protected GTool initPanTool(Cursor xy,
                              Cursor x,
                              Cursor y,
                              Cursor n,
                              Cursor w,
                              Cursor s,
                              Cursor r,
                              Cursor nw,
                              Cursor ne,
                              Cursor sw,
                              Cursor se)
  {
    return(new GToolAdapter(xy));
  }


  protected void prepareSimulation()
  {
    _runSim.setEnabled(true);
    _clearSims.setEnabled(true);
    _simCommand.updateTimeline(getGraphPanel().getDrawableGraph().nodesIterator());
    if (_timeCodeLabel == null) {
      initTimeCodeLabel(_simCommand.getFirstFrameDate());
    }
    _timeCodeLabel.setVisible(true);
    updateTimeCode(1, _simCommand.getFirstFrameDate());

    repaint();
  }


  JTextField getTextField()
  {
    return _textField;
  }

  JTextArea getTextArea()
  {
    return _textArea;
  }


  /**
   * Initialize the panner.
   */
  private void initPanner()
  {
    if (_panner == null) {

      //
      // Adjust the width of the panner based on the width of the
      // graph.  Height is set at 100.
      //
      GraphPanel graphPanel = getGraphPanel();

      Dimension d = getGraphPanel().getPreferredSize();

      double w = 100.0;
      double h = d.height * 100.0 / d.width;

      PanAndScanPanel nav =
        new PanAndScanPanel((int)w,
                            (int)h,
                            graphPanel,
                            new Color(0xB2, 0xB2, 0xB2),
                            new Color(0xFE, 0x86, 0x3B));

      // Keep panner in sync with the GraphPanel
      ViewportPanAndScanAdapter ada =
        new ViewportPanAndScanAdapter(graphPanel,
                                      getGraphScrollPane().getViewport(),
                                      nav);

      _panner = new BumpPanel(GB_COLOR, nav);

      // Setup the docking behaviors
      _dockMgr.register
        (
         _panner,
         new DockManager.DockCallback() {
           public void performDock(JComponent c, Rectangle bounds) {
             _pannerWindow.getContentPane().add(c);
             positionPannerWindow();
             _pannerWindow.setVisible(false);//_panAndScanItem.getState());
           }
         },
         new DockManager.UndockCallback() {
           public void performUndock(JComponent c) {
             _pannerWindow.getContentPane().remove(c);
             _pannerWindow.setVisible(false);
           }
         });

      _panner.setDockManager(_dockMgr);

      if (_pannerWindow == null) {
        _pannerWindow = new JWindow(this);
        addComponentListener(new ComponentAdapter() {
            public void componentMoved(ComponentEvent e) {
              positionPannerWindow();
            }
          });
      }
      _pannerWindow.getContentPane().add(_panner);
      positionPannerWindow();

      if (_myViewportListener == null) {
        final JScrollBar vsb = getGraphScrollPane().getVerticalScrollBar();
        final JScrollBar hsb = getGraphScrollPane().getHorizontalScrollBar();

        _myViewportListener = new ChangeListener() {
            private boolean __vsbVisible = vsb.isVisible();
            private boolean __hsbVisible = hsb.isVisible();

            public void stateChanged(ChangeEvent evt) {
              if (!(__vsbVisible == vsb.isVisible() ||
                    __hsbVisible == hsb.isVisible()))
              {
                positionPannerWindow();
                __vsbVisible = vsb.isVisible();
                __hsbVisible = hsb.isVisible();
              }
            }
          };

        getGraphScrollPane().getViewport().addChangeListener(_myViewportListener);
      }

      // get the panner to update
      Point p = new Point(1, 1);
      getGraphScrollPane().getViewport().setViewPosition(p);
      p.setLocation(0, 0);
      getGraphScrollPane().getViewport().setViewPosition(p);
    }
  }


  /*
   * Set/reset the position of the panner box.
   */
  private void positionPannerWindow() {
    if (_panner != null)
    {
      Insets insets = getContentPane().getInsets();

      Rectangle panelBounds = new Rectangle
        (
         (int) (getGraphScrollPane().getViewportBorderBounds().getWidth() - insets.right - 5 - _panner.getWidth()),
         (int) (getGraphScrollPane().getViewportBorderBounds().getHeight() - insets.bottom - 5 - _panner.getHeight()),
         _panner.getWidth(),
         _panner.getHeight()
         );

      panelBounds = SwingUtilities.convertRectangle(getContentPane(),
                                                    panelBounds,
                                                    this);

      panelBounds.setLocation(panelBounds.x + getLocation().x,
                              panelBounds.y + getLocation().y);

      _pannerWindow.setBounds(panelBounds);
    }
  }

  // todo: move to preference
  private Color _timeCodeColor = Color.red;
  protected void initTimeCodeLabel(Date date)
  {
    if (_timeCodeLabel == null) {
      Insets insets = getContentPane().getInsets();
      int x = insets.left + 5;
      int y = (int) getGraphScrollPane().getViewportBorderBounds().getHeight() -
        (insets.bottom + 5);

      _timeCodeLabel = new JLabel("Frame 1: " + date.toString());
      _timeCodeLabel.setBackground(getGraphPanel().getBackground());
      _timeCodeLabel.setForeground(_timeCodeColor);
      _timeCodeLabel.setSize(_timeCodeLabel.getPreferredSize());
      _timeCodeLabel.setLocation(x, (y - _timeCodeLabel.getHeight()));
      getGraphPanel().add(_timeCodeLabel);
      _timeCodeLabel.setVisible(false);
    }
  }

  protected void updateTimeCode(int frame, Date date)
  {
    if (_timeCodeLabel != null) {
      _timeCodeLabel.setText("Frame " + frame + ": " + date.toString());
      _timeCodeLabel.setSize(_timeCodeLabel.getPreferredSize());

      Insets insets = getContentPane().getInsets();
      int x = insets.left + 5;
      int y = (int) getGraphScrollPane().getViewportBorderBounds().getHeight() -
        (insets.bottom + 5);

      // scroll pane corrections
      Point p = getGraphScrollPane().getViewport().getViewPosition();
      x += p.x;
      y += p.y;
      _timeCodeLabel.setLocation(x, (y - _timeCodeLabel.getHeight()));
    }
  }


  /**
   * Component listener events for the scroll pane. Override if you
   * need a hook into a scrollpane component event
   */
  protected void scrollPaneComponentShown(ComponentEvent e) {
    positionPannerWindow();
  }


  /**
   * Component listener events for the scroll pane. Override if you
   * need a hook into a scrollpane component event
   */
  protected void scrollPaneComponentResized(ComponentEvent e) {
    positionPannerWindow();
  }


  /**
   * Helper method to constructor - initializes the look and feel for the
   * app
   */
  private void updateLookAndFeel() {
    HMVLookAndFeel laf = new HMVLookAndFeel();
    laf.installLookAndFeel(this);
  }


  /**
   * Show the HMV configuration panel.
   */
  public void showConfigPanel() {
    _changedConfiguration = false;

    // init config panel with current configuration

    if (_controlPanel == null) {
      _controlPanel = new ConfigPanel(this);
    }

    _controlPanel.loadBeans(getConfigBean());

    if (_prefWindow == null) {
      _prefWindow = new JFrame(".:BNE Configuration Panel:.");
      _prefWindow.getContentPane().add(_controlPanel);
    }

    _prefWindow.pack();
    _prefWindow.setVisible(true);
  }


  /**
   * Apply the given configuration to the current graph.
   */
  private void applyConfiguration(HMVConfigBean2 configuration) {
    DrawableGraphContext ctx = getGraphPanel().getDrawableGraphContext();

    // apply configuration to the graph panel.
    configuration.applyConfiguration(this);

    getGraphPanel().redraw();

    _changedConfiguration = true;
  }


  /**
   * Initialize HMV configuration either from a saved configuration
   * (~/.hmv) or use the default.
   */
  private void initConfigBean() {
    _configBean = null;

    try {
      File f = new File(_hmvHome, HMV_CONFIG);

      if (f.exists()) {
        _logger.info("Loading HMV configurations");
        _configBean = (HMVConfigBean2) ConfigBeanLoader.loadBeanFile(f);
      }
      else {
        _logger.info("Using default HMV configurations");
        _configBean = new HMVConfigBean2();
      }
    }
    catch (Exception e) {
      _logger.log(Level.WARNING, "Could not load hmv configuration", e);

      // use default configurations
      _logger.info("using default HMV configurations");
      _configBean = new HMVConfigBean2();
    }
  }


  /**
   * Save current configuration (~/.hmv).
   */
  private void saveConfigBean(ConfigBean bean) {
    if (_hmvHome != null && _hmvHome.exists()) {
      FileOutputStream out = null;
      try {
        out = new FileOutputStream(new File(_hmvHome, HMV_CONFIG));
        TextBeans.save(bean, out);
      }
      catch (IOException e) {
        _logger.log(Level.WARNING, "Could not save HMV configuration", e);
      }
      finally {
        try {
          out.flush();
          out.close();
        }
        catch (IOException ignore) { }
      }
    }
  }


  /**
   * Create a new File object that will reside in the HMV home
   * directory.  May return null if the hmv home directory has not
   * been set or does not exist.
   */
  protected static File createNewHomeFile(String name) {
    if (_hmvHome != null && _hmvHome.exists()) {
      return (new File(_hmvHome, name));
    }
    return (null);
  }


  // *** this is FaderUI-specific ***
  private void updateTextAreaPosition(DrawableNode node,
                                      JTextArea jta,
                                      Rectangle2D textRect,
                                      int maxWidth)
  {
    FaderUI2.NodeSize nodeSize = (FaderUI2.NodeSize)
      getGraphPanel().getDrawableGraphContext().getContextData(node,
                                                               "nodeSize");
    Size nodeSizeVP = getGraphPanel().mapWorldToViewport(nodeSize);
    Rectangle2D nodeBounds =
      getGraphPanel().getDrawableGraphContext().getBounds(node);
    Rectangle2D nodeBoundsVP =
      getGraphPanel().mapWorldToViewport(nodeBounds);

    int x = (int)
      Math.round(nodeBoundsVP.getX() +
                 ((nodeSizeVP.getWidth() - maxWidth) / 2));

    jta.setLocation(x, (int)textRect.getBounds().getLocation().getY());
    jta.setSize(maxWidth, (int)textRect.getBounds().getSize().getHeight());
  }


  private String _oldLabel;
  protected void editNodeLabel(DrawableNode node, Rectangle2D textRect)
  {
    // save this for later
    _editingNode = node;
    _oldLabel = FaderUtil.getNodeLabel(_editingNode);

    // set up text editor
    _textArea.setText(FaderUtil.getNodeLabel(node));
    _textArea.selectAll();
    _textArea.requestFocusInWindow();

    DrawProps nodeProps = _graphUIPrefs.getNodeDrawProps(false, false);
    int maxWidth = nodeProps.getMaxLabelWidth();
    updateTextAreaPosition(_editingNode, _textArea, textRect, maxWidth);

    FaderUtil.setInEdit(_editingNode, true);

    getGraphPanel().repaint();
  }


  protected void editSliderValue(DrawableGraphElement el, Rectangle2D textRect)
  {
    _slidingElement = el;

    if (_slidingElement instanceof DrawableNode) {
      ((HMV.DigitOnlyDocument)_textField.getDocument()).setAllowSign(false);
      ((HMV.DigitOnlyDocument)_textField.getDocument()).setMaxLength(3);
    }
    else {
      ((HMV.DigitOnlyDocument)_textField.getDocument()).setAllowSign(true);
      ((HMV.DigitOnlyDocument)_textField.getDocument()).setMaxLength(2);
    }

    int val = FaderUtil.getSliderValue(_slidingElement, 0);
    _textField.setText(String.valueOf(val));
    _textField.selectAll();
    _textField.setLocation((int)textRect.getX(), (int)textRect.getY());

    _textField.setVisible(true);
    _textField.requestFocusInWindow();
  }


  /**
   * Locate or create the hmv home directory (~/.hmv)
   */
  private static File findHome() throws IOException {
    String home = System.getProperty(HOME_PROP);

    if (home == null) {
      // get the default hmv home (~/.hmv)
      String user  = System.getProperty("user.home");

      home = user + File.separator + ".hmv";
    }

    File hmvHome  = new File(home);
    if (!hmvHome.exists()) {
      hmvHome.mkdirs();
    }

    return (hmvHome);
  }


  /**
   * Override to use our own messages.
   */
  protected String getExitQuestion()
  {
    return ("Would you like to save this belief network?");
  }

  protected String getModifiedSaveQuestion()
  {
    return ("Would you like to save the changes you've made " +
            "to this belief network?");
  }


  /**
   * The Slacker Layout lets the parent decide where to place the
   * children components.
   */
  private class SlackerLayout implements LayoutManager
  {
    public void addLayoutComponent(Component comp, Object constraints) {}
    public void addLayoutComponent(String name, Component comp) {}

    public void layoutContainer(Container parent) {
      _textField.setSize(_textField.getPreferredSize());
      _textArea.setSize(_textArea.getPreferredSize());
    }

    public Dimension minimumLayoutSize(Container parent)
    {
      Rectangle2D rect = getGraphPanel().getOverallBounds();
      return (new Dimension((int)rect.getX(), (int)rect.getY()));
    }

    public Dimension preferredLayoutSize(Container parent)
    {
      Rectangle2D rect = getGraphPanel().getOverallBounds();
      return (new Dimension((int)rect.getX(), (int)rect.getY()));
    }

    public void removeLayoutComponent(Component comp) {}
  }



  class DigitOnlyDocument extends DefaultStyledDocument
  {
    private int _maxSize;
    private boolean _allowSign = false;

    public DigitOnlyDocument(int maxSize) {
      _maxSize = maxSize;
    }

    public void setAllowSign(boolean b) {
      _allowSign = b;
    }

    public void setMaxLength(int m) {
      _maxSize = m;
    }

    public void insertString(int offset, String str, AttributeSet a)
      throws BadLocationException
    {
      int max = _allowSign ? _maxSize + 1 : _maxSize;
      String t = getText(0, getLength());

      if ((getLength() + str.length()) > max) {
          return;
      }

      if (getLength() >= max) {
        if (_allowSign) {
          if ((offset == 0) && (str.length() == 1) &&
              ((str.charAt(0) == '-') || (str.charAt(0) == '+')) &&
              (t.indexOf('+') == -1) && (t.indexOf('-') == -1))
          {
            super.insertString(offset, str, a);
            return;
          }
          else {
            return;
          }
        }
        else {
          return;
        }
      }
      else {
        if (_allowSign) {
          if ((offset == 0) && (str.length() == 1) &&
              ((str.charAt(0) == '-') || (str.charAt(0) == '+')) &&
              (t.indexOf('+') == -1) && (t.indexOf('-') == -1))
          {
            super.insertString(offset, str, a);
            return;
          }
          else if (getLength() >= max) {
            return;
          }
        }
      }

      for (int i=0; i<str.length(); i++) {
        char c = str.charAt(i);
        if ((i==0) && (c != '-') && (c != '+') && !Character.isDigit(c)) {
          return;
        }

        if ((i>0) && !Character.isDigit(c)) {
          return; // only can inser numbers
        }
      }
      super.insertString(offset, str, a);
    }

  } // end inner default class DigitOnlyDocument


  class AMDocument extends DefaultStyledDocument
  {
    public void insertString(int offset, String str, AttributeSet a)
      throws BadLocationException
    {
      if (getGraphPanel().getDrawableGraph() == null) {
        return;
      }

      if ((str.length() == 1) && (str.charAt(0) == '\n'))
      {
        endNodeLabelEdit();
        _hmvTool2.setState(_hmvTool2.STATE_SELECTED_NODE,
                           new Object[] {_editingNode});
      }
      else
      {
        super.insertString(offset, str, a);
      }
    }

  } // end innder default class AMDocument


  /**
   * Temporary workaround until we figure out JTextArea's focus issue.
   * If we set it to invisible then HMV's focus gets confused. But this
   * doesn't happen with JTextField...
   */
  private void hideTextArea(JTextArea jta)
  {
    jta.setLocation(-200, -200);
  }


  /**
   * Clear all simulation input data for the current graph.
   */
  private void clearAllSimData()
  {
    if ((getGraphPanel() == null) ||
        (getGraphPanel().getDrawableGraph() == null)) {
      return;
    }

    for (NodeIterator itr=getGraphPanel().getDrawableGraph().nodesIterator();
         itr.hasNext();)
    {
      DrawableNode node = itr.next();
      node.removeProperty(PROP_SIMDATA);
    }
    _simCommand.haltAnimation();
    _runSim.setEnabled(false);
    _clearSims.setEnabled(false);

    if (_timeCodeLabel != null) {
      _timeCodeLabel.setVisible(false);
    }
  }

} // end class HMV
