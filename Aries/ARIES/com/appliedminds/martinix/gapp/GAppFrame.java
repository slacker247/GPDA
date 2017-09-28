/* -*- Mode: Java; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*-*/
package com.appliedminds.martinix.gapp;

import com.appliedminds.core.util.IconAndCursorLoader;
import com.appliedminds.martini.*;
import com.appliedminds.martini.io.*;
import com.appliedminds.martini.layout.*;
import com.appliedminds.martinix.*;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.*;
import java.awt.geom.Rectangle2D;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.EventListenerList;


/**
 * Provides an application shell that can view and manipulate a graph.
 * Subclasses must implement the various framework methods.
 *
 *
 *
 * @author mathias@apmindsf.com
 * @author daepark@apmindsf.com
 */
public abstract class GAppFrame
  extends JFrame
  implements MarqueeListener,
             ComponentListener,
             TextListener,
             MarqueeContainer
{

  //
  // Tool names.
  //
  private static final String TOOL_NAME_SELECT       = "select tool";
  private static final String TOOL_NAME_NODE         = "node tool";
  private static final String TOOL_NAME_EDGE         = "edge tool";
  private static final String TOOL_NAME_TEXT         = "text tool";
  private static final String TOOL_NAME_ZOOM_IN      = "zoom in tool";
  private static final String TOOL_NAME_ZOOM_OUT     = "zoom out tool";
  private static final String TOOL_NAME_ZOOM_MARQUEE = "zoom marquee tool";
  private static final String TOOL_NAME_HAND         = "hand tool";


  private static final Insets ZERO_INSETS = new Insets(0, 0, 0, 0);

  //
  // is this app run as an applet?
  //
  private boolean _isApplet = false;

  // is the frame already initialized?
  private boolean _initialized = false;

  // should the program exit when the frame window is closed
  private boolean _exitWhenClosed = true;

  //
  // Gui components. Menubar, toolbar and statusbar can be disabled.
  //
  private JMenuBar    _menubar     = null;
  private JToolBar    _toolbar     = null;
  private JPanel      _statusBar   = null;
  private JComponent  _mainContentPane = null;
  private JMenu _fileMenu = null;
  private JMenu _layoutMenu = null;
  private int _fileMenuExitIndex;

  private boolean _useMenubar;
  private boolean _useToolbar;
  private boolean _useStatusbar;

  //
  // the graph panel
  //
  private GraphPanel         _graphPanel = null;

  //
  // The sidebar containg various application tools.
  // Sidebar use can be disabled.
  //
  private GSidebar            _sidebar    = null;
  private boolean _useSidebar;


  //
  // the current layout
  //
  private GraphLayout        _layout     = null;



  //
  // map of available layouts for easy access
  //
  private HashMap            _layouts    = null;


  //
  // the current tool being used
  //
  private GTool            _currentTool       = null;
  private GTool            _lastTool          = null;



  //
  // all the available tools for easy access
  //
  private GTool           _selectTool = null;
  private GTool           _textTool   = null;
  private GTool           _handTool   = null;
  private GTool           _zoomTool   = null;
  private GTool           _nodeTool   = null;
  private GTool           _edgeTool   = null;
  private GTool           _panTool    = null;

  private boolean _useSelectTool;
  private boolean _useTextTool;
  private boolean _useZoomTool;
  private boolean _useHandTool;
  private boolean _useNodeTool;
  private boolean _useEdgeTool;

  //
  // keep these components in sync with each other
  //
  private JCheckBoxMenuItem _sidebarMenuItem    = null;
  private JButton           _closeSidebarButton = null;
  private JButton           _openSidebarButton  = null;


  //
  // handlers for the action components in this application
  //
  private GActions _actions = null;


  //
  // knows how to load cursors and images
  //
  private IconAndCursorLoader _iconAndCursorLoader = null;


  //
  // handles all our marquee needs
  //
  private MarqueePane _marqueePane = null;


  //
  // holds GraphPanel and MarqueePane
  //
  private GLayeredPane _gLayeredPane = null;


  //
  // should the graph fit in the current window
  //
  private boolean _fitInWindow = false;


  //
  // event listeners
  //
  private EventListenerList _listenerList;


  /**
   * Is the app scalable or not.
   */
  private boolean _scalable;


  // keep track of the document modification state for "save as" warnings.
  protected static final int DOC_NONE       = -1;
  protected static final int DOC_NEW        = 0;
  protected static final int DOC_UNMODIFIED = 1;
  protected static final int DOC_MODIFIED   = 2;
  private int _docState = DOC_NONE;


  /**
   * Initialize the app.  Initialize all your members in the
   * constructor.  Once the app is constructed, you must call
   * initGAppFrame to instantiate all the GUI elements.
   *
   * @param title the frame title.
   */
  public GAppFrame(String title) {
    super(title);
    _useSidebar = true;
    _useMenubar = true;
    _useToolbar = true;
    _useStatusbar = true;
    _useSelectTool = true;
    _useTextTool = true;
    _useZoomTool = true;
    _useHandTool = true;
    _useNodeTool = true;
    _useEdgeTool = true;
    _scalable = true;
  }


  /**
   * Initialize the app.  Initialize all your members in the
   * constructor.  Once the app is constructed, you must call
   * initGAppFrame to instantiate all the GUI elements.
   *
   * @param title the frame title.
   * @param sidebar if you want to use the default sidebar.
   * @param menubar if you want to use the default menubar.
   * @param toolbar if you want to use the default toolbar.
   * @param statusbar if you want to use the default statusbar.
   * @param selectTool if you want to use the select tool.
   * @param textTool if you want to use the text tool.
   * @param zoomTool if you want to use the zoom tool.
   * @param handTool if you want to use the hand tool.
   * @param nodeTool if you want to use the node tool.
   * @param edgeTool if you want to use the edge tool.
   */
   public GAppFrame(String title,
                    boolean sidebar,
                    boolean menubar,
                    boolean toolbar,
                    boolean statusbar,
                    boolean selectTool,
                    boolean textTool,
                    boolean zoomTool,
                    boolean handTool,
                    boolean nodeTool,
                    boolean edgeTool)
  {
    super(title);
    _useSidebar = sidebar;
    _useMenubar = menubar;
    _useToolbar = toolbar;
    _useStatusbar = statusbar;

    _useSelectTool = selectTool;
    _useTextTool = textTool;
    _useZoomTool = zoomTool;
    _useHandTool = handTool;
    _useNodeTool = nodeTool;
    _useEdgeTool = edgeTool;
    _scalable = true;
  }

  /**
   * This must be called after construction and before doing anything
   * else.
   *
   * @param isApplet whether or not this is used inside an Applet
   */
  public final void initGAppFrame(boolean isApplet) {
    init(isApplet);
  }


  /**
   * Get all the actions.
   */
  public GActions getActions() {
    return(_actions);
  }


  /**
   * Is this app used in an Applet context?
   */
  public boolean isApplet() {
    return (_isApplet);
  }


  /**
   * Please do not call setDrawableGraph on the returned object.  Use
   * the version in this class (GAppFrame) instead.
   *
   * @return the main GraphPanel
   */
  public GraphPanel getGraphPanel() {
    return (_graphPanel);
  }


  /**
   * Show or hide the marquee pane by adjusting its visibility.
   *
   * @param b the visibility value for the marquee pane.
   */
  public void setMarqueePaneVisibility(boolean b) {
    _marqueePane.setVisible(b);
  }


  /**
   * Check if "fit in window" is in effect.
   */
  public boolean getFitInWindow() {
    return _fitInWindow;
  }


  /**
   * Set the "fit in window" flag.
   */
  public void setFitInWindow(boolean f) {
    _fitInWindow = f;
  }



  /**
   * @s true if this app can scale the rendered graph.
   */
  public void setIsScalable(boolean s)
  {
    _scalable = s;
  }


  /**
   * Those who want to be notified whenever a new graph is loaded
   * in this application should register here.
   *
   * @param l a GraphUpdateListener to add.
   */
  public void addGAppFrameListener(GAppFrameListener l) {
    _listenerList.add(GAppFrameListener.class, l);
  }


  /**
   * Those who want to be notified whenever a new graph is loaded
   * in this application should register here.
   *
   * @param l a Listener to remove.
   */
  public void removeGAppFrameListenerListener(GAppFrameListener l) {
    _listenerList.remove(GAppFrameListener.class, l);
  }


  /**
   * @return the state of the current document in the application. This
   * can be one of DOC_NONE, DOC_NEW, DOC_UNMODIFIED, or DOC_MODIFIED.
   */
  public int getDocumentState()
  {
    return _docState;
  }


  public void setDocumentState(int state)
  {
    if ((state < DOC_NONE) || (state > DOC_MODIFIED))
    {
      throw (new RuntimeException("Trying to set invalid document state"));
    }

    _docState = state;
  }


  ////////////////////////////////////
  // begin ComponentListener interface

  /**
   * Ensure the graph panel is at least as big as the viewable size.
   */
  public void componentResized(ComponentEvent e) {
    fitGraphPanelInViewport();
  }

  public void componentMoved(ComponentEvent e) { }

  public void componentShown(ComponentEvent e) { }

  public void componentHidden(ComponentEvent e) { }

  // end ComponentListener interface
  //////////////////////////////////


  ///////////////////////////////
  // begin TextListener interface (Adopted for our own use!).
  public void textValueChanged(TextEvent e)
  {
    String fileName = (String) e.getSource();
    File file = new File(fileName);

    if (!_initialized) {
      initGAppFrame(false);
    }

    if (!_initialized) {
      _exitWhenClosed = false;
      setBounds(100, 100, 700, 700);
      // remove the file/exit menu item
      _fileMenu.remove(_fileMenuExitIndex);
      _initialized = true;
    }

    setVisible(true);

    if (file.exists()) {
      loadFile(fileName);
      file.delete();
    }
    else {
      JOptionPane.showMessageDialog(this, "File does not exist: " + fileName,
                                    "Error", JOptionPane.ERROR_MESSAGE);
    }
  }
  // end TextListener interface
  ///////////////////////////////


  /**
   * Activate the tool and its appropriate cursor in the GraphPanel.
   */
  public void useSelectTool() {
    useTool(_selectTool);
  }


  /**
   * Activate the tool and its appropriate cursor in the GraphPanel.
   */
  public void useTextTool() {
    useTool(_textTool);
  }


  /**
   * Activate the tool and its appropriate cursor in the GraphPanel.
   */
  public void useZoomTool() {
    useTool(_zoomTool);
  }


  /**
   * Activate the tool and its appropriate cursor in the GraphPanel.
   */
  public void useHandTool() {
    useTool(_handTool);
  }


  /**
   * Get the panning tool.
   */
  public GTool getPanTool() {
    return(_panTool);
  }


  /**
   * Activate the tool and its appropriate cursor in the GraphPanel.
   */
  public void useNodeTool() {
    useTool(_nodeTool);
  }


  /**
   * Activate the tool and its appropriate cursor in the GraphPanel.
   */
  public void useEdgeTool() {
    useTool(_edgeTool);
  }


  /**
   * Use/update the layout of the GraphPanel where layout can be any of
   * the layout type strings defined in GActions.
   *
   * @param layout GActions.CIRCULAR_LAYOUT, GActions.HIERARCHICAL_LAYOUT,
   *               GActions.RANDOM_LAYOUT, GActions.ORTHOGONAL_LAYOUT,
   *               GActions.SYMMETRIC_LAYOUT, or GActions.TREE_LAYOUT
   */
  public void useLayout(String layout) {
    Action action = _actions.getLayoutAction(layout, null);

    if (!(action == null || _layoutMenu == null)) {
      for (int i=0; i<_layoutMenu.getItemCount(); i++) {
        JMenuItem menuItem = (JMenuItem)_layoutMenu.getItem(i);
        if (menuItem != null && menuItem.getAction() == action) {
          // programatically perform a click on the menu item which
          // will sync all the layout groups (menu items, combobox, etc.).
          menuItem.doClick();
        }
      }
    }
  }


  /**
   * This will not sync all the layout groups (menu items, combobox, etc.)
   */
  protected void setLayout(String layout)
  {
    GraphLayout l = (GraphLayout)_layouts.get(layout);
    if (l != null && l != _layout) {
      setLayout(l);
    }
    else {
      Logger.getLogger(getLogName()).log(Level.WARNING, layout + " layout is not implemented");
    }
  }


  /**
   * This will not sync all the layout groups (menu items, combobox, etc.)
   */
  protected void setLayout(GraphLayout layout) {
    _graphPanel.setLayout(layout);
    _layout = layout;

    if (getFitInWindow()) {
      fitGraphInWindow();
    }
    else {
      getGraphPanel().revalidate();
      getGraphPanel().paintImmediately();
    }
  }


  /**
   * Scale the graph panel so that it will fit into the current window.
   */
  public void fitGraphInWindow()
  {
    // calculate a scale that will fit the graph into the current viewport
    if (_graphPanel.getDrawableGraph() == null) {
      return;
    }
    _graphPanel.setScale(getMinimumScale());

    Dimension extSize = getGraphScrollPane().getViewport().getExtentSize();
    _graphPanel.updateGraphBounds(extSize.width, extSize.height, true);

    getGraphScrollPane().getViewport().setViewPosition(new Point(1, 1));

    _mainContentPane.paintImmediately(getGraphScrollPane().getBounds());

    _fitInWindow = true;
  }



  /**
   * @return the minimum scale factor that will fit the graph in the
   *  current window.
   */
  public double getMinimumScale()
  {
    if (!_scalable) {
      return 1.0;
    }

    Rectangle2D graphBounds =
      _graphPanel.getDrawableGraphContext().getOverallBounds(0);
    Rectangle2D vpBounds = _graphPanel.mapWorldToViewport(graphBounds);

    double xratio = getGraphScrollPane().getWidth() / vpBounds.getWidth();
    double yratio = getGraphScrollPane().getHeight() / vpBounds.getHeight();
    double ratio = (xratio > yratio) ? yratio : xratio;

    double minScale = (_graphPanel.getScale()*ratio) - 0.002;
    if (minScale > 1.0) {
      minScale = 1.0;
    }

    return (minScale);
  }


  /**
   * Open the sidebar panel.
   */
  public void openSidebar() {
    if (isUsingSidebar()) {
      MySplitPane p = (MySplitPane) _mainContentPane;
      p.showRightComponent(true);

      // keep sidebar components in sync
      _sidebarMenuItem.setState(true);
      _toolbar.remove(_openSidebarButton);
      _toolbar.add(_closeSidebarButton, BorderLayout.EAST);
      _closeSidebarButton.invalidate();
      _toolbar.revalidate();
      _toolbar.repaint();
    }
  }


  /**
   * Close the sidebar panel.
   */
  public void closeSidebar() {
    if (isUsingSidebar()) {
      MySplitPane p = (MySplitPane) _mainContentPane;
      p.showRightComponent(false);

      // keep sidebar components in sync
      _sidebarMenuItem.setState(false);
      _toolbar.remove(_closeSidebarButton);
      _toolbar.add(_openSidebarButton, BorderLayout.EAST);
      _openSidebarButton.invalidate();
      _toolbar.revalidate();
      _toolbar.repaint();
    }
  }



  /**
   * The main GraphPanel in this app is inside a JScrollPane.
   *
   * @return the scroll pane containing the main GraphPanel.
   */
  public JScrollPane getGraphScrollPane() {
    if (isUsingSidebar()) {
      MySplitPane p = (MySplitPane) _mainContentPane;
      return((JScrollPane) p.getLeftComponent());
    }
    else {
      return((JScrollPane) _mainContentPane);
    }
  }


  /**
   * Return the name of the logger to use.  By default this just returns
   * the root logger, "".  Override if necessary.
   *
   * @return the name of the logger to send log messages to.
   */
  protected String getLogName() {
    return("");
  }


  /**
   * Framework for loading precomputed graph context data (in most cases,
   * layout data) of the graph. Use with caution.
   *
   * @param loader a LayoutLoader whose graph element bounds have been
   * already loaded.
   */
  protected final void loadLayout(LayoutLoader loader)
  {
    GraphPanel panel = getGraphPanel();
    DrawableGraph graph = panel.getDrawableGraph();

    DrawableGraphContext currentContext = panel.getDrawableGraphContext();

    NodeIterator itr = graph.nodesIterator();
    double x = 0.0, y = 0.0;

    while (itr.hasNext()) {
      DrawableNode node = itr.next();

      Rectangle2D oldBounds = currentContext.getBounds(node);
      Rectangle2D newBounds = loader.getBounds(node);

      boolean invalid = false;

      if (newBounds == null) {
        // arbitrary layout
        newBounds = new Rectangle2D.Double(x, y, 0, 0);
        x += 100;
        y += 100;
        invalid = true;
      }

      if (oldBounds == null) {
        currentContext.setBounds(node, newBounds);
      }
      else {
        oldBounds.setRect(newBounds);
      }

      // still need to invalidate to make sure the nodes will get
      // real sizes
      if (invalid) {
        currentContext.invalidate(node);
      }
    }

    panel.updateWorldBounds();
    fitGraphPanelInViewport();
  }


  /**
   * Instead of calling setDrawableGraph on the GraphPanel directly
   * by calling getGraphPanel().setDrawableGraph(), we <b>URGE</b>
   * clients to use this method as it allows this frame to stay
   * in sync with the GraphPanel.
   *
   * @param g the new drawable graph.
   */
  protected final void setDrawableGraph(DrawableGraph g) {
    //
    // Quell any activity on the graph panel.
    //
    getGraphPanel().stopAnimation();
    getGraphPanel().stopElementEdit();

    if (isGraphValid(g)) {
      getGraphPanel().setDrawableGraph(g);

      // paint graph immediately so that we can fit the graph in the window
      getGraphPanel().paintImmediately();
      fitGraphPanelInViewport();
      getContentPane().repaint();

      notifyGAppFrameListeners();
      setDrawableGraphHook();
    }
  }


  /**
   * Called after setDrawableGraph executes.
   */
  protected abstract void setDrawableGraphHook();


  /**
   * Called after a file is opened.
   */
  protected abstract void fileOpenHook();


  /**
   * Called after a new file action.
   */
  protected abstract void fileNewHook();


  /**
   * Called just before exit occurs.  Override if you want to do any
   * clean up.  Exit always occurs immediately after this method
   * returns.  The default implementation does nothing.
   *
   * @return TRUE then GAppFrame exits, otherwise just setVisible to false.
   */
  protected boolean exitHook() {
    return (_exitWhenClosed);
  }


  /**
   * Messages displayed to the user when exiting. Applications can
   * override this to display app-specific messages.
   */
  protected String getExitQuestion()
  {
    return ("Do you want to save your changes before exiting?");
  }

  protected String getModifiedSaveQuestion()
  {
    return ("Do you want to save your changes before exiting?");
  }


  /**
   * Is the graph valid to the application?
   */
  public abstract boolean isGraphValid(DrawableGraph g);


  /**
   * Get the current tool.
   */
  protected final GTool getCurrentTool() {
    return(_currentTool);
  }


  /**
   * access to the IconAndCursorLoader
   */
  public IconAndCursorLoader getIconAndCursorLoader() {
    return (_iconAndCursorLoader);
  }


  /**
   * Make sure the graph panel is the same size as our scrollpane's
   * viewport, and redraw the graph.
   */
  public void fitGraphPanelInViewport() {
    JViewport vp = getGraphScrollPane().getViewport();
    _graphPanel.paintImmediately();
    _graphPanel.updateGraphBounds(vp.getWidth(), vp.getHeight(), false);
    _graphPanel.redraw();
  }


  /**
   * Fire off a graph updated event.
   *
   * This method should be private.  If you find you have to call it
   * from some other object, modify the code so that we are calling it
   * from here.  We are incharge of the GraphPanel and hence the
   * Graph.
   */
  private void notifyGAppFrameListeners() {
    // Guaranteed to return a non-null array
    Object[] listeners = _listenerList.getListenerList();
    // Process the listeners last to first, notifying
    // those that are interested in this event
    for (int i = listeners.length-2; i>=0; i-=2) {
      if (listeners[i]==GAppFrameListener.class) {
        ((GAppFrameListener)listeners[i+1]).gAppFrameGraphUpdated(getGraphPanel());
      }
    }
  }



  /**
   * Return a transformer here.
   */
  protected abstract GTransformer getTransformer();


  /**
   * Initialize the application.
   *
   * @param isApplet whether or not this app is within an Applet context
   */
  private void init(boolean isApplet) {
    _isApplet = isApplet;

    //
    // init members
    //
    _actions = new GActions(this, getTransformer());
    _iconAndCursorLoader = new IconAndCursorLoader(this);
    _listenerList = new EventListenerList();

    initTools();
    initLayouts();
    _layout = null;

    //
    // init gui components (menu, toolbar, graph panel, sidebar, etc.)
    //
    initGUI();

    if (_isApplet == true) {
      // we can't system exit on an applet
      // for now just hide the frame
      addWindowListener(new WindowAdapter() {
          public void windowClosing(WindowEvent e) {
            _actions.exitProgram();
          }
        });

      //
      // disable non-applet functionality in our app
      // if this is used in an Applet context
      //
      _actions.getFileOpenAction().setEnabled(false);
      _actions.getFileSaveAction().setEnabled(false);
    }
    else {
      addWindowListener(new WindowAdapter() {
          public void windowClosing(WindowEvent e) {
            _actions.exitProgram();
          }
        });
    }

    // activate default tool
    useSelectTool();
    closeSidebar();

    // we are our own component listener
    addComponentListener(this);

  }


  /**
   * Quick and dirty way to load a GML file.
   */
  public void loadFile(String fname)
  {
    try {
      LayoutLoader loader = new GLayoutLoader();
      DrawableGraph g = _actions.loadGraphFromFile(new File(fname), loader);
      getGraphPanel().setDrawableGraph(g);
      setDrawableGraphHook();
      loadLayout(loader);
      fitGraphInWindow();
      getGraphPanel().repaint();
    }
    catch (Exception e) {
      Logger.getLogger(getLogName()).log(Level.WARNING, "Unable to load graph from file '" + fname + "'", e);
    }
  }


  /**
   * Initialize all the tools
   */
  private void initTools()  {

    // Prepare the cursors for the tools
    String BASE = "com/appliedminds/martinix/gapp/resources/";

    Cursor selectCursor =
      _iconAndCursorLoader.loadCursor(BASE + "select_cursor.png",
                                      new Point(1, 1), TOOL_NAME_SELECT);

    Cursor nodeCursor   =
      _iconAndCursorLoader.loadCursor(BASE + "node_cursor.png",
                                      new Point(1, 1), TOOL_NAME_NODE);

    Cursor edgeCursor   =
      _iconAndCursorLoader.loadCursor(BASE + "edge_cursor.png",
                                      new Point(1, 1), TOOL_NAME_EDGE);

    Cursor textCursor   =
      _iconAndCursorLoader.loadCursor(BASE + "text_cursor.png",
                                      new Point(8, 8), TOOL_NAME_TEXT);

    Cursor zoomInCursor =
      _iconAndCursorLoader.loadCursor(BASE + "zoom_in.png",
                                      new Point(5, 5), TOOL_NAME_ZOOM_IN);

    Cursor zoomOutCursor =
      _iconAndCursorLoader.loadCursor(BASE + "zoom_out.png",
                                      new Point(5, 5), TOOL_NAME_ZOOM_OUT);

    Cursor zoomMarqueeCursor =
      _iconAndCursorLoader.loadCursor(BASE + "zoom_marquee.gif",
                                      new Point(5, 5), TOOL_NAME_ZOOM_MARQUEE);

    Cursor handOpenCursor =
      _iconAndCursorLoader.loadCursor(BASE + "hand_open.png",
                                      new Point(8, 8), TOOL_NAME_HAND);

    Cursor handClosedCursor =
      _iconAndCursorLoader.loadCursor(BASE + "hand_closed.png",
                                      new Point(8, 8), TOOL_NAME_HAND);

    Cursor panXYCursor =
      _iconAndCursorLoader.loadCursor(BASE + "pan_xy.gif",
                                      new Point(13, 13), "PAN_XY");

    Cursor panXCursor =
      _iconAndCursorLoader.loadCursor(BASE + "pan_x.gif",
                                      new Point(13, 13), "PAN_X");

    Cursor panYCursor =
      _iconAndCursorLoader.loadCursor(BASE + "pan_y.gif",
                                      new Point(13, 13), "PAN_Y");

    Cursor panUpCursor =
      _iconAndCursorLoader.loadCursor(BASE + "pan_up.gif",
                                      new Point(13, 13), "PAN_UP");

    Cursor panLeftCursor =
      _iconAndCursorLoader.loadCursor(BASE + "pan_left.gif",
                                      new Point(13, 13), "PAN_LEFT");

    Cursor panDownCursor =
      _iconAndCursorLoader.loadCursor(BASE + "pan_down.gif",
                                      new Point(13, 13), "PAN_DOWN");

    Cursor panRightCursor =
      _iconAndCursorLoader.loadCursor(BASE + "pan_right.gif",
                                      new Point(13, 13), "PAN_RIGHT");

    Cursor panUpLeftCursor =
      _iconAndCursorLoader.loadCursor(BASE + "pan_up_left.gif",
                                      new Point(13, 13), "PAN_UP_LEFT");

    Cursor panUpRightCursor =
      _iconAndCursorLoader.loadCursor(BASE + "pan_up_right.gif",
                                      new Point(13, 13), "PAN_UP_RIGHT");

    Cursor panDownLeftCursor =
      _iconAndCursorLoader.loadCursor(BASE + "pan_down_left.gif",
                                      new Point(13, 13), "PAN_DOWN_LEFT");

    Cursor panDownRightCursor =
      _iconAndCursorLoader.loadCursor(BASE + "pan_down_right.gif",
                                      new Point(13, 13), "PAN_DOWN_RIGHT");

    // Create the tools
    _selectTool = initSelectTool(selectCursor);
    _textTool   = initTextTool(textCursor);
    _handTool   = initHandTool(handOpenCursor, handClosedCursor);
    _zoomTool   = initZoomTool(zoomInCursor, zoomOutCursor, zoomMarqueeCursor);
    _nodeTool   = initNodeTool(nodeCursor);
    _edgeTool   = initEdgeTool(edgeCursor);

    _panTool    = initPanTool(panXYCursor,
                              panXCursor,
                              panYCursor,
                              panUpCursor,
                              panLeftCursor,
                              panDownCursor,
                              panRightCursor,
                              panUpLeftCursor,
                              panUpRightCursor,
                              panDownLeftCursor,
                              panDownRightCursor);
  }




  /** Return the select tool here */
  protected abstract GTool initSelectTool(Cursor cur);

  /** Return the text tool here */
  protected abstract GTool initTextTool(Cursor cur);

  /** Return the hand tool here */
  protected abstract GTool initHandTool(Cursor open, Cursor closed);

  /** Return the select zoom here */
  protected abstract GTool initZoomTool(Cursor in,
                                        Cursor out,
                                        Cursor marquee);

  /** Return the node tool here */
  protected abstract GTool initNodeTool(Cursor cur);

  /** Return the edge tool here */
  protected abstract GTool initEdgeTool(Cursor cur);

  /** Return the pan tool here */
  protected abstract GTool initPanTool(Cursor xy,
                                       Cursor x,
                                       Cursor y,
                                       Cursor n,
                                       Cursor w,
                                       Cursor s,
                                       Cursor r,
                                       Cursor nw,
                                       Cursor ne,
                                       Cursor sw,
                                       Cursor se);



  /**
   * Initialize all the available layouts.
   */
  private void initLayouts() {
    _layouts = new HashMap();

    //
    // after implementing a graph layout, add it to this map
    //
    //_layouts.put(GActions.CIRCULAR_LAYOUT, new YourCircularLayout());
    //_layouts.put(GActions.HIERARCHICAL_LAYOUT, new YourHierarchicalLayout());
    //_layouts.put(GActions.ORTHOGONAL_LAYOUT, new YourOrthogonalLayout());
    //_layouts.put(GActions.RANDOM_LAYOUT, new YourRandomLayout());
    //_layouts.put(GActions.SYMMETRIC_LAYOUT, new YourSymmetricLayout());
    //_layouts.put(GActions.TREE_LAYOUT, new YourTreeLayout());
  }


  /**
   * Add the different gui components
   */
  private void initGUI() {
    if (_useMenubar) {
      initMenuBar();
    }


    if (_useToolbar) {
      initToolBar();
    }

    if (_useStatusbar) {
      initStatusBar();
    }

    initSplitPane();

    if (_useMenubar) {
      super.setJMenuBar(_menubar);
    }

    if (_useToolbar) {
      super.getContentPane().add(_toolbar, BorderLayout.NORTH);
    }

    super.getContentPane().add(_mainContentPane, BorderLayout.CENTER);

    if (_useStatusbar) {
      super.getContentPane().add(_statusBar, BorderLayout.SOUTH);
    }

    initGUIHook();
  }


  /**
   * Called when initGUI is complete.
   */
  protected abstract void initGUIHook();



  /**
   * If you want to add app-specific file menu stuff add it here.
   */
  protected abstract void addToFileMenuHook(JMenu filemenu);


  /**
   * If you want to add app-specific view menu stuff override this
   */
  protected void addToViewMenuHook(JMenu filemenu) {}


  /**
   * Programmatically perform a "click" on the File->New menu item.
   */
  public void doClickFileNew() {
    JMenuItem item = _fileMenu.getItem(0);   // this is the new file action command
    item.doClick();
  }

  /**
   * the menu bar
   */
  private void initMenuBar() {

    _menubar = new JMenuBar();

    //
    // file menu
    //
    JPopupMenu.setDefaultLightWeightPopupEnabled(true);
    _fileMenu = new JMenu("File");
    _fileMenu.add(_actions.getFileNewAction());  // if changing this item's position, update doClickFileNew method.
    _fileMenu.add(_actions.getFileOpenAction());
    _fileMenu.add(_actions.getFileSaveAction());
    _fileMenu.addSeparator();
    addToFileMenuHook(_fileMenu);
    //_fileMenu.add(_actions.getFileCloseAction());
    _fileMenu.add(_actions.getFileExitAction());
    _fileMenuExitIndex = 5; // index to the exit menu item

    //
    // tool menu
    //
    JMenu toolMenu = new JMenu("Tools");
    JRadioButtonMenuItem rbMenuItem = null;
    ButtonGroup toolMenuGroup = new ButtonGroup();

    if (_useSelectTool) {
      rbMenuItem =
        new JRadioButtonMenuItem(_actions.getSelectToolAction(toolMenuGroup));
      rbMenuItem.setSelected(true);
      toolMenuGroup.add(rbMenuItem);
      toolMenu.add(rbMenuItem);
    }

    if (_useTextTool) {
      rbMenuItem =
        new JRadioButtonMenuItem(_actions.getTextToolAction(toolMenuGroup));
      toolMenuGroup.add(rbMenuItem);
      toolMenu.add(rbMenuItem);
    }

    if (_useZoomTool) {
      rbMenuItem =
        new JRadioButtonMenuItem(_actions.getZoomToolAction(toolMenuGroup));
      toolMenuGroup.add(rbMenuItem);
      toolMenu.add(rbMenuItem);
    }

    if (_useHandTool) {
      rbMenuItem =
        new JRadioButtonMenuItem(_actions.getHandToolAction(toolMenuGroup));
      toolMenuGroup.add(rbMenuItem);
      toolMenu.add(rbMenuItem);
    }

    if (_useNodeTool) {
      rbMenuItem =
        new JRadioButtonMenuItem(_actions.getNodeToolAction(toolMenuGroup));
      toolMenuGroup.add(rbMenuItem);
      toolMenu.add(rbMenuItem);
    }

    if (_useEdgeTool) {
      rbMenuItem =
        new JRadioButtonMenuItem(_actions.getEdgeToolAction(toolMenuGroup));
      toolMenuGroup.add(rbMenuItem);
      toolMenu.add(rbMenuItem);
    }


    //
    // view menu
    //
    JMenu viewMenu = new JMenu("View");
    if (isUsingSidebar()) {
      _sidebarMenuItem =
        new JCheckBoxMenuItem(_actions.getCloseSidebarAction());
      _sidebarMenuItem.setState(true);
      _sidebarMenuItem.setIcon(null);
      _sidebarMenuItem.addItemListener(new ItemListener() {
          public void itemStateChanged(ItemEvent e) {
            JCheckBoxMenuItem cb = (JCheckBoxMenuItem)e.getSource();
            if (cb.getState() == true) {
              cb.setAction(_actions.getOpenSidebarAction());
            }
            else {
              cb.setAction(_actions.getCloseSidebarAction());
            }
            cb.setIcon(null);
          }
        });
      viewMenu.add(_sidebarMenuItem);
      viewMenu.addSeparator();
    }
    addToViewMenuHook(viewMenu);
    viewMenu.add(_actions.getViewRefreshAction());


    //
    // layout menu
    //
    _layoutMenu = new JMenu("Layout");
    _layoutMenu.add(_actions.getFitInWindowAction());
    _layoutMenu.addSeparator();
    ButtonGroup layoutMenuGroup = new ButtonGroup();

    rbMenuItem =
      new JRadioButtonMenuItem(_actions.getLayoutAction(GActions.CIRCULAR_LAYOUT,
                                                        layoutMenuGroup));
    rbMenuItem.setSelected(true);
    _layoutMenu.add(rbMenuItem);
    layoutMenuGroup.add(rbMenuItem);

    rbMenuItem =
      new JRadioButtonMenuItem(_actions.getLayoutAction(GActions.HIERARCHICAL_LAYOUT,
                                                        layoutMenuGroup));
    _layoutMenu.add(rbMenuItem);
    layoutMenuGroup.add(rbMenuItem);

    rbMenuItem =
      new JRadioButtonMenuItem(_actions.getLayoutAction(GActions.ORTHOGONAL_LAYOUT,
                                                        layoutMenuGroup));
    _layoutMenu.add(rbMenuItem);
    layoutMenuGroup.add(rbMenuItem);

    rbMenuItem =
      new JRadioButtonMenuItem(_actions.getLayoutAction(GActions.RANDOM_LAYOUT,
                                                        layoutMenuGroup));
    _layoutMenu.add(rbMenuItem);
    layoutMenuGroup.add(rbMenuItem);

    rbMenuItem =
      new JRadioButtonMenuItem(_actions.getLayoutAction(GActions.SYMMETRIC_LAYOUT,
                                                        layoutMenuGroup));
    rbMenuItem.setSelected(true);
    _layoutMenu.add(rbMenuItem);
    layoutMenuGroup.add(rbMenuItem);

    rbMenuItem =
      new JRadioButtonMenuItem(_actions.getLayoutAction(GActions.TREE_LAYOUT,
                                                        layoutMenuGroup));
    _layoutMenu.add(rbMenuItem);
    layoutMenuGroup.add(rbMenuItem);

    //
    // add all menus to the menu bar
    //
    _menubar.add(_fileMenu);
    _menubar.add(viewMenu);
    if (toolMenu.getItemCount() > 1) {
      _menubar.add(toolMenu);
    }
    //    _menubar.add(_layoutMenu);
  }


  /**
   * the tool bar
   */
  private void initToolBar() {

    _toolbar = new JToolBar();
    _toolbar.setFloatable(false);

    Box box = new Box(BoxLayout.X_AXIS);

    ButtonGroup toolRadioGroup = new ButtonGroup();

    // select tool
    ImageIcon defaultIcon = null;
    ImageIcon selectedIcon = null;

    if (_useSelectTool) {
      defaultIcon = _iconAndCursorLoader.loadImageIcon("com/appliedminds/martinix/gapp/resources/select_tool_up.png");
      selectedIcon = _iconAndCursorLoader.loadImageIcon("com/appliedminds/martinix/gapp/resources/select_tool_down.png");
      JRadioButton selectToolButton = new ToolRadioButton(defaultIcon,
                                                          selectedIcon,
                                                          toolRadioGroup,
                                                          true);
      selectToolButton.setAction(_actions.getSelectToolAction(toolRadioGroup));
      selectToolButton.setText("");
      box.add(selectToolButton);
    }

    if (_useTextTool) {
      // text tool
      defaultIcon = _iconAndCursorLoader.loadImageIcon("com/appliedminds/martinix/gapp/resources/text_tool_up.png");
      selectedIcon = _iconAndCursorLoader.loadImageIcon("com/appliedminds/martinix/gapp/resources/text_tool_down.png");
      JRadioButton textToolButton = new ToolRadioButton(defaultIcon,
                                                        selectedIcon,
                                                        toolRadioGroup,
                                                        false);
      textToolButton.setAction(_actions.getTextToolAction(toolRadioGroup));
      textToolButton.setText("");
      box.add(textToolButton);
    }

    if (_useZoomTool) {
      // zoom tool
      defaultIcon = _iconAndCursorLoader.loadImageIcon("com/appliedminds/martinix/gapp/resources/zoom_tool_up.png");
      selectedIcon = _iconAndCursorLoader.loadImageIcon("com/appliedminds/martinix/gapp/resources/zoom_tool_down.png");
      JRadioButton zoomToolButton = new ToolRadioButton(defaultIcon,
                                                        selectedIcon,
                                                        toolRadioGroup,
                                                        false);
      zoomToolButton.setAction(_actions.getZoomToolAction(toolRadioGroup));
      zoomToolButton.setText("");
      box.add(zoomToolButton);
    }

    if (_useHandTool) {
      // hand tool
      defaultIcon = _iconAndCursorLoader.loadImageIcon("com/appliedminds/martinix/gapp/resources/hand_tool_up.png");
      selectedIcon = _iconAndCursorLoader.loadImageIcon("com/appliedminds/martinix/gapp/resources/hand_tool_down.png");
      JRadioButton handToolButton = new ToolRadioButton(defaultIcon,
                                                        selectedIcon,
                                                        toolRadioGroup,
                                                        false);
      handToolButton.setAction(_actions.getHandToolAction(toolRadioGroup));
      handToolButton.setText("");
      box.add(handToolButton);
    }

    if (_useNodeTool) {
      // node tool
      defaultIcon = _iconAndCursorLoader.loadImageIcon("com/appliedminds/martinix/gapp/resources/node_tool_up.png");
      selectedIcon = _iconAndCursorLoader.loadImageIcon("com/appliedminds/martinix/gapp/resources/node_tool_down.png");
      JRadioButton nodeToolButton = new ToolRadioButton(defaultIcon,
                                                        selectedIcon,
                                                        toolRadioGroup,
                                                        false);
      nodeToolButton.setAction(_actions.getNodeToolAction(toolRadioGroup));
      nodeToolButton.setText("");
      box.add(nodeToolButton);
    }

    if (_useEdgeTool) {
      // edge tool
      defaultIcon = _iconAndCursorLoader.loadImageIcon("com/appliedminds/martinix/gapp/resources/edge_tool_up.png");
      selectedIcon = _iconAndCursorLoader.loadImageIcon("com/appliedminds/martinix/gapp/resources/edge_tool_down.png");
      JRadioButton edgeToolButton = new ToolRadioButton(defaultIcon,
                                                        selectedIcon,
                                                        toolRadioGroup,
                                                        false);
      edgeToolButton.setAction(_actions.getEdgeToolAction(toolRadioGroup));
      edgeToolButton.setText("");
      box.add(edgeToolButton);
    }

    // sidebar open/close buttons
    if (isUsingSidebar()) {
      _closeSidebarButton = new JButton(_actions.getCloseSidebarAction());
      _closeSidebarButton.setBorder(null);
      _closeSidebarButton.setText("");
      _openSidebarButton = new JButton(_actions.getOpenSidebarAction());
      _openSidebarButton.setBorder(null);
      _openSidebarButton.setText("");
    }


    _toolbar.setLayout(new BorderLayout());
    box.add(Box.createRigidArea(new Dimension(80, 0)));
    _toolbar.add(box, BorderLayout.WEST);

    if (isUsingSidebar()) {
      _toolbar.add(_closeSidebarButton, BorderLayout.EAST);
    }
  }



  /**
   * Create and return a GraphPanel here.  Remember to set
   * the background.
   */
  protected abstract GraphPanel initGraphPanel();



  /**
   * The split pane containing the GraphPanel and the sidebar
   */
  private void initSplitPane()
  {
    _graphPanel = initGraphPanel();
    _graphPanel.setLayout(_layout);
    //    _graphPanel.setBackground(Color.white);

    addDrawableGraphMouseEventListeners();


    // capture our own mouse and key events in the GraphPanel
    //    _graphPanel.addKeyListener(new ToolShortcutKeyAdapter());
    _graphPanel.addKeyListener(new GraphPanelKeyAdapter());
    _graphPanel.addMouseListener(new GraphPanelMouseAdapter());
    _graphPanel.addMouseMotionListener(new GraphPanelMouseMotionAdapter());

    // set up the marquee pane
    _marqueePane = new MarqueePane(this);
    _marqueePane.addMarqueeListener(this);

    _gLayeredPane = new GLayeredPane(_graphPanel, _marqueePane);

    JScrollPane scrollPane = new JScrollPane(_gLayeredPane);

    scrollPane.setPreferredSize(this.getPreferredSize());
    scrollPane.getViewport().setBackground(_graphPanel.getBackground());
    scrollPane.addComponentListener(new ComponentListener() {
        public void componentMoved(ComponentEvent e) {
          scrollPaneComponentMoved(e);
        }
        public void componentHidden(ComponentEvent e) {
          scrollPaneComponentHidden(e);
        }
        public void componentShown(ComponentEvent e) {
          scrollPaneComponentShown(e);
        }
        public void componentResized(ComponentEvent e) {
          scrollPaneComponentResized(e);
        }
      });


    _graphPanel.addComponentListener(_gLayeredPane);

    Insets spInsets = scrollPane.getInsets();
    _graphPanel.setLocation(spInsets.left, spInsets.top);

    // set up the sidebar
    if (isUsingSidebar()) {
      _sidebar = new GSidebar(this, getNodeFilterTable(), getEdgeFilterTable());
      //
      // the sidebar is a GAppFrameListener
      //
      addGAppFrameListener(_sidebar);
      MySplitPane p = new MySplitPane(scrollPane, _sidebar, true);
      p.setBackground(Color.yellow);
      _mainContentPane = p;
    }
    else {
      _mainContentPane = scrollPane;
    }

    _gLayeredPane.setBackground(Color.blue);
  }


  /**
   * Component listener events for the scroll pane. Override if you
   * need a hook into a scrollpane component event
   */
  protected void scrollPaneComponentMoved(ComponentEvent e) { }


  /**
   * Component listener events for the scroll pane. Override if you
   * need a hook into a scrollpane component event
   */
  protected void scrollPaneComponentHidden(ComponentEvent e) { }


  /**
   * Component listener events for the scroll pane. Override if you
   * need a hook into a scrollpane component event
   */
  protected void scrollPaneComponentShown(ComponentEvent e) { }


  /**
   * Component listener events for the scroll pane. Override if you
   * need a hook into a scrollpane component event
   */
  protected void scrollPaneComponentResized(ComponentEvent e) { }


  /**
   * Return a filter table here if you have enabled the sidebar in the
   * constructor.  If the sidebar is not rendered then this method
   * will neve be called.
   */
  protected abstract GFilterTable getNodeFilterTable();


  /**
   * Return a filter table here if you have enabled the sidebar in the
   * constructor.  If the sidebar is not rendered, then this method
   * will never be called.
   */
  protected abstract GFilterTable getEdgeFilterTable();



  /**
   * At this point the graph panel has been initialized and subclasses
   * should add listeners to hook up the tools.
   */
  protected abstract void addDrawableGraphMouseEventListeners();



  public void marqueeSelected(Rectangle marqueeBounds) {
    if ((getGraphPanel() != null) && (_currentTool != null)) {
      _currentTool.handleMarqueeSelection(marqueeBounds);
    }
  }


  /**
   * The status bar.
   */
  private void initStatusBar() {
    // set up status bar panel
    GridBagLayout gridBag = new GridBagLayout();
    GridBagConstraints c = new GridBagConstraints();
    _statusBar = new JPanel();
    _statusBar.setLayout(gridBag);

    // layout combo box
    LayoutComboBox cb = new LayoutComboBox();
    c.weightx = 1.0;
    c.anchor = GridBagConstraints.WEST;
    gridBag.setConstraints(cb, c);
    _statusBar.add(cb);

    // SE widget
    Component w = getExtraStatusBarWidget();
    if (w != null) {
      c.anchor = GridBagConstraints.EAST;
      gridBag.setConstraints(w, c);
      _statusBar.add(w);
    }
  }


  /**
   * Subclasses can return a widget here to place in the lower right
   * hand part of the app.  Subclasses could also return null.
   */
  protected abstract Component getExtraStatusBarWidget();


  /**
   * Use the specified tool and set the appropriate cursor in the GraphPanel.
   */
  protected void useTool(GTool newTool) {
    useTool(newTool, null);
  }


  protected void useLastTool() {
    if (_lastTool != null) {
      useTool(_lastTool);
    }
  }


  protected final void useTool(GTool newTool, Object[] args) {
    if (newTool != _currentTool) {
      if (_currentTool != null) {
        _currentTool.deactivate();
      }


      _lastTool = _currentTool;

      _currentTool = newTool;

      if (_currentTool != null) {
        _currentTool.activate(args);

        if (_graphPanel != null) {
          // use the appropriate cursor for the select tool and
          // current tool state
          setCursor(_currentTool.getCursor());

          // give the keyboard focus to the graph panel
          if (_graphPanel.hasFocus() == false) {
            _graphPanel.requestFocus();
          }
        }
      }
    }
  }


  /**
   * Set the cursor.  This method should when the default cursor (as
   * set by the current tool) needs to be modified.  For example, when
   * you want to change the cursor in response to some event.
   *
   * @param c a cursor.
   */
  public void setCursor(Cursor c) {
    _gLayeredPane.setCursor(c);
    _graphPanel.setCursor(c);
  }



  /**
   * Called when the mouse is clicked on the display.  Normally
   * the click is given to the current tool.
   *
   * @return true to prevent passing the mouse event on to
   * the current tool.
   */
  protected abstract boolean mousePressedHook(MouseEvent e);


  /**
   * Called when the mouse is released on the display.  Normally
   * the release event is passed to the current tool.
   *
   * @return true to prevent passing the mouse event on to
   * the current tool.
   */
  protected abstract boolean mouseReleasedHook(MouseEvent e);



  /**
   * Determine if the sidebar was created.  This reflects the boolean
   * argument passed into the constructor.
   *
   * @return true if the sidebar has been rendered, false if not.
   */
  protected final boolean isUsingSidebar() {
    return(_useSidebar);
  }



  /**
   * @return returns true if the document used by the app is new or
   * has been modified.
   */
  protected boolean isDocumentModified()
  {
    if ((getDocumentState() == DOC_UNMODIFIED) ||
        (getDocumentState() == GAppFrame.DOC_NONE))
    {
      return false;
    }
    else
    {
      return true;
    }
  }


  /**
   * A custom button for our tools
   */
  class ToolRadioButton extends JRadioButton {

    public ToolRadioButton(Icon defaultIcon,
                           Icon selectedIcon,
                           ButtonGroup group,
                           boolean isSelected)
    {
      super(defaultIcon);
      this.setSelectedIcon(selectedIcon);

      // add to the logical button group
      if (group != null) {
        group.add(this);
      }

      this.setSelected(isSelected);
    }

    public Insets getInsets(Insets insets) {
      return (new Insets(0, 0, 0, 0));
    }

  } // end class ToolButton




  /**
   * A combo box containg the list of available graph layouts
   * This combo box will be in sync with the layout menu.
   */
  class LayoutComboBox extends JComboBox {

    public LayoutComboBox() {
      super();
      //this.addItem(GActions.CIRCULAR_LAYOUT);
      this.addItem(GActions.HIERARCHICAL_LAYOUT);
      //this.addItem(GActions.ORTHOGONAL_LAYOUT);
      //this.addItem(GActions.RANDOM_LAYOUT);
      this.addItem(GActions.SYMMETRIC_LAYOUT);
      //this.addItem(GActions.TREE_LAYOUT);

      //
      // register this combo box as a group to be synchronized with
      // other groups maintaining the same logical list of layouts
      //
      _actions.getLayoutAction(GActions.HIERARCHICAL_LAYOUT, this);

      this.addItemListener(new ItemListener() {
          public void itemStateChanged(ItemEvent e) {
            if (e.getStateChange() == ItemEvent.SELECTED) {
              JComboBox cb = (JComboBox)e.getSource();
              String layout = (String)cb.getSelectedItem();
              Action action = _actions.getLayoutAction(layout, null);
              if (action != null) {
                // allow normal propagation of the selection change event
                // within the combo box which will fire off the
                // Action set here
                cb.setAction(action);
              }
            }
          }
        });

    }

  } // end class LayoutComboBox




  /**
   * an image button
   */
  private class ImageButton extends JButton {

    public ImageButton(ImageIcon upImage,
                       ImageIcon dnImage) {
      super(upImage);
      super.setPressedIcon(dnImage);
      super.setMargin(new Insets(0, 0, 0, 0));
      super.setBorderPainted(false);
    }

    public ImageButton(ImageIcon upImage,
                       ImageIcon dnImage,
                       ImageIcon offImage)
    {
      super(upImage);
      super.setPressedIcon(dnImage);
      super.setDisabledIcon(offImage);
      super.setMargin(new Insets(0, 0, 0, 0));
      super.setBorderPainted(false);
    }

    public Insets getInsets() {
      return (new Insets(0, 0, 0, 0));
    }

  } // end class ImageButton



  /**
   * A horizontal split-pane containing a scrollable GraphPanel on the left
   * and a sidebar tool panel on the right.
   */
  private class MySplitPane extends JSplitPane implements ComponentListener
  {
    // remember the last divider location
    private int __lastDividerLocationOffset = -1;

    /**
     * Create a horizontal split pane with the specified components
     *
     * @param left      the left component
     * @param right     the right component
     * @param showRight whether or not to show the right component
     */
    public MySplitPane(Component   left,
                       Component   right,
                       boolean     showRight)
    {
      super(JSplitPane.HORIZONTAL_SPLIT,
            left,
            right);
      this.showRightComponent(showRight);
      this.getRightComponent().addComponentListener(this);
      this.addComponentListener(this);
    }


    /**
     * Toggle the visibility of the right component.
     *
     * @param show TRUE show, FALSE hide
     */
    public void showRightComponent(boolean show) {
      Component right = this.getRightComponent();
      right.setVisible(show);
      right.invalidate();

      if (show == true) {
        if (__lastDividerLocationOffset == -1) {
          __lastDividerLocationOffset = this.getDefaultDividerLocationOffset();
        }

        this.setDividerLocation(this.getWidth() - __lastDividerLocationOffset);
      }
      else {
        // remember last divider location
        __lastDividerLocationOffset = (this.getWidth() -
                                       this.getDividerLocation());
      }

      this.revalidate();
      this.repaint();

    }


    public Insets getInsets() {
      return ZERO_INSETS;
    }


    public Insets getInsets(Insets i) {
      return ZERO_INSETS;
    }


    //
    // begin ComponentListener interface
    //

    public void componentResized(ComponentEvent e) {
      Component component = e.getComponent();
      if (component == this.getRightComponent()) {
        __lastDividerLocationOffset = (this.getWidth() - this.getDividerLocation());
      }
      else if (component == this) {
        this.setDividerLocation(this.getWidth() - __lastDividerLocationOffset);
        fitGraphPanelInViewport();
      }
    }

    public void componentMoved(ComponentEvent e) { }

    public void componentShown(ComponentEvent e) { }

    public void componentHidden(ComponentEvent e) { }

    //
    // end ComponentListener interface
    //

    /**
     * @return the default divider location
     */
    private int getDefaultDividerLocationOffset() {
      final int DEFAULT_TOOLPANEL_WIDTH = 250;

      return (DEFAULT_TOOLPANEL_WIDTH);
    }

  } // end class MySplitPane



  /**
   * GraphPanel mouse listener. Delegates all events
   * to the current tool.
   */
  class GraphPanelMouseAdapter extends MouseAdapter {


    public void mouseClicked(MouseEvent e) {
      if (e.getClickCount() > 1) {
        mouseDoubleClicked(e);
      }
      else {
        mouseSingleClicked(e);
      }
    }

    public void mouseSingleClicked(MouseEvent e) {
      if (_currentTool != null) {
        _currentTool.mouseClicked(e);
      }
    }

    public void mouseDoubleClicked(MouseEvent e) {
      if (_currentTool != null) {
        _currentTool.mouseDoubleClicked(e);
      }
    }

    public void mousePressed(MouseEvent e) {
      if (!mousePressedHook(e)) {
        if (_currentTool != null) {
          _currentTool.mousePressed(e);
        }
      }
    }

    public void mouseReleased(MouseEvent e) {
      if (! mouseReleasedHook(e)) {
        if (_currentTool != null) {
          _currentTool.mouseReleased(e);
        }
      }
    }

    public void mouseEntered(MouseEvent e) {
      // request focus on our graph panel
      _graphPanel.requestFocus();

      if (_currentTool != null) {
        _currentTool.mouseEntered(e);
      }
    }

    public void mouseExited(MouseEvent e) {
      if (_currentTool != null) {
        _currentTool.mouseExited(e);
      }
    }

  } // end class GraphPanelMouseAdapter


  /**
   * GraphPanel mouse motion listener. Delegates all events
   * to the current tool.
   */
  class GraphPanelMouseMotionAdapter extends MouseMotionAdapter {

    public void mouseDragged(MouseEvent e) {
      if (_currentTool != null) {
        _currentTool.mouseDragged(e);
      }
    }

    public void mouseMoved(MouseEvent e) {
      if (_currentTool != null) {
        _currentTool.mouseMoved(e);
      }
    }

  } // end class GraphPanelMouseMotionAdapter


  /**
   * GraphPanel key listener. Delegates all events
   * to the current tool except for shortcut keys:
   *
   * ALT +
   *   z - zoom tool
   *   h - hand tool
   *   n - node tool
   *   j - edge tool
   *   v - select tool
   *   t - text tool
   */
  class GraphPanelKeyAdapter extends KeyAdapter {

    private JButton __dummyButton = new JButton();
    private boolean __shortcutKeyPressed = false;

    public void keyPressed(KeyEvent e) {
      // z - highlights the zoom tool
      if (e.isAltDown() && (KeyEvent.VK_Z == e.getKeyCode() ||
                            KeyEvent.VK_J == e.getKeyCode() ||
                            KeyEvent.VK_N == e.getKeyCode() ||
                            KeyEvent.VK_V == e.getKeyCode() ||
                            KeyEvent.VK_H == e.getKeyCode() ||
                            KeyEvent.VK_T == e.getKeyCode()))
      {
        __shortcutKeyPressed = true;
      }

      else if (_currentTool != null) {
        _currentTool.keyPressed(e);
      }
    }

    public void keyReleased(KeyEvent e)
    {
      if (__shortcutKeyPressed == true) {
        // z - highlights the zoom tool
        if (KeyEvent.VK_Z == e.getKeyCode()) {
          if (_zoomTool != _currentTool) {
            this.fireDummyAction(_actions.getZoomToolAction(null));
          }
        }

        // j - highlights the join edges tool
        else if (KeyEvent.VK_J == e.getKeyCode()) {
          if (_edgeTool != _currentTool) {
            this.fireDummyAction(_actions.getEdgeToolAction(null));
          }
        }

        // n - create node
        else if (KeyEvent.VK_N == e.getKeyCode()) {
          if (_nodeTool != _currentTool) {
            this.fireDummyAction(_actions.getNodeToolAction(null));
          }
        }

        // v - move/select tool
        else if (KeyEvent.VK_V == e.getKeyCode()) {
          if (_selectTool != _currentTool) {
            this.fireDummyAction(_actions.getSelectToolAction(null));
          }
        }

        // h - hand tool
        else if (KeyEvent.VK_H == e.getKeyCode()) {
          if (_handTool != _currentTool) {
            this.fireDummyAction(_actions.getHandToolAction(null));
          }
        }

        // t - text tool
        else if (KeyEvent.VK_T == e.getKeyCode()) {
          if (_textTool != _currentTool) {
            this.fireDummyAction(_actions.getTextToolAction(null));
          }
        }

        __shortcutKeyPressed = false;
      }

      else if (_currentTool != null) {
        _currentTool.keyReleased(e);
      }
    }

    public void keyTyped(KeyEvent e) {
      if (__shortcutKeyPressed == false) {
        if (_currentTool != null) {
          _currentTool.keyTyped(e);
        }
      }
    }

    /**
     * Programmaticallly perform a "click" on a dummy button with the
     * specified action. This does the same thing as if the user had
     * pressed and released the button.
     */
    private void fireDummyAction(Action action) {
      System.err.println("!!!! GAppFrame.GraphPanelKeyAdapter.fireDummyAction()");
      __dummyButton.setAction(action);
      __dummyButton.doClick();
    }

  } // end class GraphPanelKeyAdapter


  public Component getMarqueeLayer() {
    return (_gLayeredPane);
  }


  private class GLayeredPane extends JLayeredPane
    implements Scrollable,
               ComponentListener
  {
    private GraphPanel __graphPanel;
    private MarqueePane __marqueePane;

    public GLayeredPane(GraphPanel gp, MarqueePane mp) {
      __graphPanel = gp;
      __marqueePane = mp;
      this.setLayout(new OverlayLayout(this));
      this.add(__graphPanel, JLayeredPane.DEFAULT_LAYER);
      this.add(__marqueePane, JLayeredPane.DRAG_LAYER);

      // hook marquee up to the graph panel
      //
      // ??? should marquee pane listen to its own mouse events ???
      // will says: sure, unless the marquee pane is invisible,
      // in which case it won't get any events at all.
      // if we hook it up to the GraphPanel like this we can contain
      // all the marquee management code in the MarqueePane class.
      // what a concept!
      __marqueePane.setEventSource(__graphPanel);
    }


    public void setCursor(Cursor c) {
      __graphPanel.setCursor(c);
      __marqueePane.setCursor(c);
    }


    /**
     * We need to override this method and call GraphPanel's paintComponent()
     * explicitly because we need to trigger GraphPanel to figure out its
     * own size at some point. It's the old chicken-and-egg problem. There
     * should be no noticible performance degradation since GraphPanel manages
     * its own redraws already.
     **/
    public void paintComponent(Graphics g) {
      super.paintComponent(g);
      __graphPanel.paintComponent(g);
    }

    //////////////////////////////////////////////////
    // Scrollable Interface Methods
    //

    /**
     * Part of the Scrollable interface. We delegate them all to graph
     * panel
     */
    public int getScrollableUnitIncrement(Rectangle visibleRect,
                                          int orientation,
                                          int direction)
    {
      return __graphPanel.getScrollableUnitIncrement(visibleRect,
                                                     orientation, direction);
    }
    public int getScrollableBlockIncrement(Rectangle visibleRect,
                                           int orientation,
                                           int direction)
    {
      return __graphPanel.getScrollableBlockIncrement(visibleRect,
                                                      orientation, direction);
    }

    /** Part of the Scrollable interface */
    public Dimension getPreferredScrollableViewportSize() {
      return __graphPanel.getPreferredScrollableViewportSize();
    }

    /** Part of the Scrollable interface */
    public boolean getScrollableTracksViewportWidth() {
      return __graphPanel.getScrollableTracksViewportWidth();
    }

    /** Part of the Scrollable interface */
    public boolean getScrollableTracksViewportHeight() {
      return __graphPanel.getScrollableTracksViewportHeight();
    }
    // end Scrollable Interface Methods
    //
    //////////////////////////////////////////////////


    //////////////////////////////////////////////////
    // ComponentListener API
    public void componentHidden(ComponentEvent e) {}
    public void componentMoved(ComponentEvent e) {}
    public void componentShown(ComponentEvent e) {}

    public void componentResized(ComponentEvent e) {
      this.setSize(e.getComponent().getSize());
      this.revalidate();
    }
    // EndComponentListener API
    //////////////////////////////////////////////////

  } // end private class GLayeredPane


} // end class GAppFrame
