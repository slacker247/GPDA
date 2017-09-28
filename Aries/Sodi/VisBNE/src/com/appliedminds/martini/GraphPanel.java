package com.appliedminds.martini;

import com.appliedminds.martini.animation.GraphAnimationPane;
import com.appliedminds.martini.layout.DefaultGraphLayout;

import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Collection;
import java.util.EventListener;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import javax.swing.JComponent;
import javax.swing.JLayeredPane;
import javax.swing.JPanel;
import javax.swing.OverlayLayout;
import javax.swing.RepaintManager;
import javax.swing.Scrollable;
import javax.swing.SwingConstants;
import javax.swing.event.EventListenerList;

/**
 * This provides a panel on which a DrawableGraph can be displayed and
 * manipulated.  In addition, this provides:
 *
 * <ul>
 * <li>Scaling (aka, zooming).
 *
 * <li>A mechanism for controlling the look and feel of the
 * DrawableGraph by specifying a GraphUI instance.
 *
 * <li>A mechanism for controlling the layout algorithm.
 *
 * <li>a framework for registering for and revieving events from the
 * DrawableGraph components.
 *
 * <li>Element dragging.
 *
 * <li>Element editing.
 * </ul>
 *
 * <p>
 * And, since this is a JPanel, this provides everything that a JPanel
 * does too.
 *
 * <p> The GraphPanel is <b>modal</b>.  The three modes of the panel
 * are <b>idle</b>, <b>dragging</b>, and <b>editing</b>.  Certain
 * behaviors of the panel are effected by the current mode, so watch
 * out.  Also, the modes mutually exclusive-- only one can be active
 * at a time.
 *
 *
 *
 * <p><b>Look and Feel</b>
 * <blockquote>
 *
 * Look and feel is controlled by a GraphUI instance.  The GraphUI
 * instance must be specified at construction time.  The GraphUI
 * instance is responsible for actually drawing the graph and also for
 * dealing with hit testing (ie, whether or not an event is generated
 * when a user clicks on a node).
 *
 * </blockquote>
 *
 *
 *
 * <p><b>Layout</b>
 * <blockquote>
 *
 * Layout functions are delegated to a GraphLayout instance that must
 * be set via the setLayout() method in this class.  Users of the
 * GraphPanel must set the layout object before attempting to draw the
 * graph.
 *
 * </blockquote>
 *
 *
 *
 * <p><b>Mouse Events</b>
 * <blockquote>
 *
 * <p>The GraphPanel listens for MouseEvents that are handed down by
 * the awt.  These events are then dispatched to the graph to see if
 * any graph object was hit.
 *
 * <P>If a graph object was hit then the MouseEvent is wrapped in a
 * DrawableGraphMouseEvent and handed to any listeners.
 * DrawableGraphMouseEvents are distinguished by "decoration id", if a
 * listener exists that is interested in the particular decoration id
 * of the event, then it is notified.  Decoration id's are defined and
 * created by GraphUI instances.
 *
 * <p>If no graph element was hit (ie, the user hit "whitespace"), and
 * the MouseEvent was of the MOUSE_PRESSED or MOUSE_RELEASED variety,
 * then we wrap the MouseEvent in a DrawableGraphMouseEvent and hand
 * it off to any listeners who are listeneing for the
 * OFFGRAPH_DECORATION_ID decoration id (defined here in the
 * GraphPanel).  This event has a context of this object, and a
 * graph-element of null.
 *
 *
 * </blockquote>
 *
 *
 *
 * <p><b>Keyboard Events</b>
 * <blockquote>
 *
 * <p>Clients interested in keyboard events should use the
 * addKeyListener() method provided by JPanel.
 *
 * </blockquote>
 *
 * <p><b>Graph Multiple Selection Events</b>
 * <blockquote>
 *
 * <p>Being a MarqueeListener, the GraphPanel may dispatch a
 * &quot;multiple hit test&quot; to the graph to see if any graph
 * object is contained within the bounds of a marquee rectangle. If
 * the hit test succeeds, then we wrap all the graph objects contained
 * within the marquee rectangle in a
 * DrawableGraphMultipleSelectionEvent and hand it off to any
 * DrawableGraphMultipleSelectionEventListeners registered with the
 * GraphPanel. The listeners will also be notified when the marquee
 * rectangle &quot;hit test misses&quot; the entire graph.
 *
 * <p>The chain of events are as follows:
 *   <ol>
 *     <li>GraphPanel receives a marqueeSelected event.
 *     <li>GraphPanel delegates hit testing to the underlying graph
 *         for the marquee bounds.
 *     <li>GraphPanel emits a DrawableGraphMultipleSelectionEvent
 *         to all the registered DrawableGraphMultipleSelectionEventListeners
 *         if there is at least one successful hit. If hit test misses,
 *         it emits a handleMultipleHitTestMissedGraph to the registered
 *         DrawableGraphMultipleSelectionEventListeners.
 *   </ol>
 *
 * </blockquote>
 *
 *
 *
 *
 * <p><b>Interactive Editing of Graph Elements</b>
 * <blockquote>
 *
 * <p>This is implemented as a joint effort between this class and a
 * GraphUI.  Clients who want to perform an edit on a graph element
 * must call the <code>startElementEdit</code> method.  The panel then
 * checks to make sure that the GraphUI will support the edit by
 * calling <code>GraphUI.setupEditSession()</code>.  Once we are in
 * the "edit state", we pass all relevant events on to the GraphUI.
 * Editing can be completed by the GraphUI (via a return value) or by
 * some external client (via <code>stopElementEdit</code>).
 *
 * </blockquote>
 *
 *
 *
 *
 * <p><b>Sample creation of a GraphPanel:</b>
 * <pre>
 *   ...
 *   GraphUI myui = new myui();
 *   GraphPanel panel = new GraphPanel(myui);
 *   GraphLayout layout = new mycrazylayout();
 *   panel.setLayout(layout);
 *   DrawableGraph graph = GMLInput.parseGML(in);
 *   panel.setDrawableGraph(graph);
 *   ...
 * </pre>
 *
 *
 * @author will@apmindsf.com
 * @author daepark@apmindsf.com
 * @author mathias@apmindsf.com
 */
public class GraphPanel extends JPanel
  implements Scrollable,
             HitTestListener,
             MouseListener,
             MouseMotionListener,
             MarqueeListener,
             BufferedComponent,
             GraphAnimationPane
{
  /**
   * The decoration ID that corresponds to the area of the GraphPanel
   * that does not contain any graph elements.  Objects interested in
   * getting click events that occur in the whitespace area should
   * register with this id.
   */
  public static final String OFFGRAPH_DECORATION_ID        = "GP_WHITESPACE";



  /**
   * The ID for registered
   * DrawableGraphMultipleSelectionEventListeners. This ID is
   * used internally for mapping unique ids to a collection of
   * identical DrawableGraphEventListeners.
   *
   * @see #marqueeSelected
   */
  private static final String MULTIPLE_HIT_LISTENER_ID    = "MULTIPLE_HIT";

  private static final int MARGIN = 10;
  private static final int PAD = 5;


  /*
   * This is the maximum increment each click on a scrollbar arrow will
   * add to/subtract from the current scrollbar positions.
   */
  private static int MAX_UNIT_INCREMENT = 5;

  private DrawableGraph _graph = null;
  private GraphUI _graphUI = null;
  private HitTesterMap _hitMap;
  private DrawableGraphContext _context;
  private GraphLayout _layout = new DefaultGraphLayout();
  private Viewport _viewport = null;


  // this buffer is needed to avoid graph redraw (slow) during scroll
  // operations
  //private BufferedImage _bufferedImage = null;
  private BufferedImageManager _bufferedImageManager = null;

  private boolean _needsRedraw = true;
  private boolean _needsLayout = true;

  // map of decorationId => ArrayList of listeners.
  // map of decorationId => EventListenerList.
  private HashMap _graphEventListeners;

  private boolean _hitTestingEnabled;

  private boolean _animating = false;

  // States
  private PanelState _currentState;
  private PanelState _idleState = new IdleState();
  private PanelState _dragState = new DragState();
  private PanelState _textEditState = new ElementEditState();


  /**
   * Create an empty GraphPanel with hit testing enabled.
   *
   * @ui the GraphUI this panel will use
   */
  public GraphPanel(GraphUI ui)
  {
    // turn double buffer off, since we'll manage our own buffer
    super(false);

    _currentState = _idleState;

    _hitTestingEnabled = true;
    _hitMap = new HitTesterMap();
    _context = new DrawableGraphContext();
    _graphEventListeners = new HashMap();
    _viewport = new Viewport();
    _graphUI = ui;
    addMouseListener(this);
    addMouseMotionListener(this);

    _bufferedImageManager = new BufferedImageManager();

    ui.setGraphPanel(this);
  }


  /**
   * Get the GraphUI object that was set in the constructor.
   *
   * @return the GraphUI in use by this panel.
   */
  public GraphUI getGraphUI() {
    return (_graphUI);
  }


  /**
   * Get a reference to our DrawableGraphContext.
   *
   * @return the DrawableGraphContext.
   */
  public DrawableGraphContext getDrawableGraphContext() {
    return(_context);
  }


  /**
   * Set the current graph.
   *
   * @param g the new graph.
   */
  public void setDrawableGraph(DrawableGraph g)
  {
    _graph = g;
    _context = new DrawableGraphContext();
    if (_hitTestingEnabled) {
      _graph.addHitTestListener(this);
    }
    _graph.setGraphUI(_graphUI);
    _needsLayout = true;
    setNeedsRedraw(true);
  }


  /**
   * Enable or disable hit testing on the graph panel.
   *
   * @param f the new value for the 'hit-testing-enabled' flag.
   */
  public void setHitTestingEnabled(boolean f) {
    if (f != _hitTestingEnabled) {
      if (_graph != null) {
        if (f) {
          _graph.addHitTestListener(this);
        }
        else {
          _graph.removeHitTestListener(this);
        }
      }
      _hitTestingEnabled = f;
    }
  }


  /**
   * Get a reference to the graph being used by this panel.
   *
   * @return the current graph.
   */
  public DrawableGraph getDrawableGraph() {
    return (_graph);
  }


  /**
   * @return true if GraphPanel is in drag state (some one has called
   * startDrag().
   */
  public boolean getInDrag() {
    // FIX: Is the method really needed?
    return(_currentState.getInDrag());
  }


  /**
   * Set the GraphLayout implementation to use for this widget.
   *
   * @param layout the GraphLayout to use.
   */
  public void setLayout(GraphLayout layout) {
    _layout = layout;
    _needsLayout = true;
    setNeedsRedraw(true);
  }



  /**
   * Register a handler for a graph mouse event.
   *
   * @param decorationId the decoration id you are interested in.
   * @param l the handler that will be called when the event of the
   * specified ID occurs.
   */
  public void addDrawableGraphMouseEventListener
    (String decorationId,
     DrawableGraphMouseEventListener l)
  {
    EventListenerList listenerList =
      (EventListenerList)_graphEventListeners.get(decorationId);
    if (listenerList == null) {
      listenerList = new EventListenerList();
      _graphEventListeners.put(decorationId, listenerList);
    }
    listenerList.add(DrawableGraphMouseEventListener.class, l);
  }


  public void addDrawableGraphMultipleSelectionEventListener
    (DrawableGraphMultipleSelectionEventListener l)
  {
    EventListenerList listenerList =
      (EventListenerList)_graphEventListeners.get(MULTIPLE_HIT_LISTENER_ID);
    if (listenerList == null) {
      listenerList = new EventListenerList();
      _graphEventListeners.put(MULTIPLE_HIT_LISTENER_ID,
                               listenerList);
    }
    listenerList.add(DrawableGraphMultipleSelectionEventListener.class, l);
  }


  /**
   * Erase and redraw the current graph.
   */
  public void redraw() {
    setNeedsRedraw(true);
    repaint();
  }


  /** Part of the BufferedComponent interface */
  public JComponent getBufferedComponent() {
    return(this);
  }

  /** Part of the BufferedComponent interface */
  public BufferedImage getBufferedComponentImage() {
    return(_bufferedImageManager.getBufferedImage());
  }


  /**
   * Layout the graph now.  Layout of the graph usually occurs
   * automatically whenever it is needed.  This method is provided in
   * case there is ever a need to layout the graph on demand.
   */
  public void layoutGraph() {
    Graphics2D g = (Graphics2D) getGraphics();
    layoutGraph(g);
  }

  /**
   * Override JPanel's paintComponent() method to draw our own content.
   *
   * @see javax.swing.JComponent#paintComponent(java.awt.Graphics)
   */
  public void paintComponent(Graphics graphics)
  {
    //    super.paintComponent(graphics);

    if (_graph == null) {
      return;
    }

    if (_needsLayout) {
      layoutGraph();
    }

    if (_needsRedraw)
    {
      // create a new buffered image (resized, etc.)
      Rectangle2D b = _context.getOverallBounds(MARGIN);
      int w = (int) _viewport.mapWorldToViewport(b.getWidth());
      int h = (int) _viewport.mapWorldToViewport(b.getHeight());

      if (_bufferedImageManager.getBufferedImage() != null) {
         w = Math.max(w, _bufferedImageManager.getBufferedImage().getWidth());
         h = Math.max(h, _bufferedImageManager.getBufferedImage().getHeight());
      }

      _bufferedImageManager.allocateBuffer(w, h);

      // set our own size
      setPreferredSize(new Dimension(w, h));
      setSize(w, h);
    }

    Rectangle2D clipRect = null;
    if (!_animating) {
      clipRect = _bufferedImageManager.updateBuffer(_needsRedraw);
    }
    _needsRedraw = false;

    if (clipRect != null) {
      graphics.setClip((int)clipRect.getX(),
                       (int)clipRect.getY(),
                       (int)clipRect.getWidth(),
                       (int)clipRect.getHeight());
    }

    //    long start = System.currentTimeMillis();
    graphics.drawImage(_bufferedImageManager.getBufferedImage(),
                       0,
                       0,
                       GraphPanel.this);

    //    long end = System.currentTimeMillis();
    //    System.err.println("blast time: " + (end - start));
    revalidate();
  }


  /**
   * Convenient method to call paintImmediately with our current
   * bounds.
   */
  public void paintImmediately()
  {
    //long now = System.currentTimeMillis();
    Rectangle visRect = getVisibleRect();
    paintImmediately(visRect);
    //long elapsed = System.currentTimeMillis() - now;
    //System.err.println("paintImmediately() within " + visRect +
    //                             " took " + elapsed + " msecs");
  }

  public void paintScreenBuffer()
  {
    Graphics graphics = null;
    try {
      graphics = getGraphics();
      paintComponent(graphics);
    }
    finally {
      graphics.dispose();
    }
  }


  /**
   * This method should be called to ensure the graph's bounds are at least
   * the width and height given.
   *
   * <p>If clip is TRUE, force the buffer size to be the specified
   * width and height. If clip is FALSE, then only adjust buffer size
   * if width and height is greater than current buffer size.
   */
  public void updateGraphBounds(int width, int height, boolean clip) {
    _bufferedImageManager.updateBufferSize(width, height, clip);
  }


  /**
   * One element may be in edit mode at a time.  This enters the
   * "element edit" state.
   *
   * @param e the element.
   * @param loc the user start location (mouse click), can be null.
   * @param ctx some context object.
   */
  public void startElementEdit(DrawableGraphElement e,
                               HashSet set, Point loc, Object ctx)
  {
    startElementEdit(e, set, loc, ctx, true);
  }


  /**
   * One element may be in edit mode at a time.  This enters the
   * "element edit" state, optionally using drawing optimizations.
   *
   * @param e the element.
   * @param loc the user start location (mouse click), can be null.
   * @param ctx some context object.
   * @param optimize if you set this to true then we will use a
   * drawing optimization whereby only the element being edited will
   * be refreshed during the edit.  If you set this to false, then the
   * entire graph will be rendered as usual.  The default is "true".
   */
  public void startElementEdit(DrawableGraphElement e,
                               HashSet set,
                               Point loc,
                               Object ctx,
                               boolean optimize)
  {
    _currentState.startElementEdit(e, set, loc, ctx, optimize);
  }


  /**
   * Exit the "element edit" state.
   */
  public void stopElementEdit() {
    _currentState.stopElementEdit();
  }



  /**
   * Prepare for a node-drag operation by placing the listed DrawableNodes
   * into drag state.
   *
   * Enters the "drag" state.
   *
   * @param nodeList a list of nodes to be placed in drag-state.
   */
  public void startDrag(Collection dragNodes)
  {
    if (dragNodes == null) {
      throw(new MartiniError("GraphPanel.startDrag() requires non-null arg"));
    }

    _currentState.startDrag(dragNodes);
  }


  /**
   * Update the positions of nodes in the drag state, and refresh the
   * panel.  Only works if we are already in the "drag" state.
   *
   * @param dx x differential to add to the x coordinates of all dragged nodes
   * @param dy y differential to add to the y coordiantes of all dragged nodes
   */
  public void dragTo(int dx, int dy)
  {
    _currentState.dragTo(dx, dy);
  }


  /**
   * Replace nodes in drag state in the default layer (or make them
   * visible again), and clean up our lists.
   *
   * Leaves the "drag" state.
   */
  public void finishDrag()
  {
    _currentState.finishDrag();
  }


  /**
   * Scale the graph by some amount.  This is like zooming.  Values
   * greater that 1.0 make the graph larger, values between 1.0 and
   * 0.0 make the graph smaller.
   *
   * @param s the new scale value.
   */
  public void setScale(double s) {
    if (Math.abs(s - _viewport.getScale()) < 0.01)
      return;

    _viewport.setScale(s);
    setNeedsRedraw(true);
  }


  /**
   * Get the current scale factor.
   *
   * @return the current scaling factor.
   * @see #setScale()
   */
  public double getScale() {
    return _viewport.getScale();
  }


  /**
   * Update the overall bounds (in WORLD coordinates) of the graph.
   */
  public void updateWorldBounds() {
    _viewport.setWorldBounds(_context.getOverallBounds(MARGIN));
  }


  public void setWorldBounds(Rectangle2D bounds)
  {
    _viewport.setWorldBounds
      (new Rectangle2D.Double((bounds.getX() - MARGIN),
                              (bounds.getY() + MARGIN),
                              (bounds.getWidth() + 2*MARGIN),
                              (bounds.getHeight() + 2*MARGIN)));
  }


  /**
   * This returns the size and position of a DrawableNode's bounding
   * rectangle in screen (viewport) coordinates.
   *
   * @return the bounds of the node in viewport coordinates.
   */
  public Rectangle2D getViewportBounds(DrawableNode node) {
    return _viewport.mapWorldToViewport(_context.getBounds(node));
  }


  /**
   * Return the current bounds, including the margin.
   */
  public Rectangle2D getOverallBounds()
  {
    return (_context.getOverallBounds(MARGIN));
  }


  /**
   * Map a point from this graph panels viewport coordinates to the
   * graph panles world coordinates.  The world coordinates are those
   * that the DrawableGraph lives in.
   *
   * @param pt a point in viewport (screen) coordinates.
   * @return a point in world coordinates.
   */
  public Point2D mapViewportToWorld(Point2D pt) {
    return(_viewport.mapViewportToWorld(pt));
  }


  /**
   * Map a length from this graph panel's viewport coordinates to the
   * graph panle's world coordinates.  The world coordinates are those
   * that the DrawableGraph lives in.
   *
   * @param len a length in viewport (screen) coordinates.
   * @return a length in world coordinates.
   */
  public double mapViewportToWorld(double len) {
    return (_viewport.mapViewportToWorld(len));
  }


  /**
   * Map a rectangle from this graph panels viewport coordinates to the
   * graph panles world coordinates.  The world coordinates are those
   * that the DrawableGraph lives in.
   *
   * @param rect a rectangle in viewport (screen) coordinates.
   * @return a rectangle in world coordinates.
   */
  public Rectangle2D mapViewportToWorld(Rectangle2D rect) {
    return (_viewport.mapViewportToWorld(rect));
  }


  /**
   * Map a point from world coordinates into this graph panels
   * viewport coordinates.  The world coordinates are those that the
   * DrawableGraph lives in, the viewport coordinates are screen
   * coordinates.
   *
   * @param pt a point in world coordinates.
   * @return a point in viewport (screen) coordinates.
   */
  public Point2D mapWorldToViewport(Point2D pt) {
    return(_viewport.mapWorldToViewport(pt));
  }


  /**
   * Map a size value from the world to the viewport world.  Only scaling
   * effects this transform.
   *
   * @param size the size of something in the world coordinates.
   * @return the size of the same thing in the viewport coordinates.
   */
  public Size mapWorldToViewport(Size size) {
    return (_viewport.mapWorldToViewport(size));
  }


  /**
   * Map a size value from the viewport to the world.  Only scaling
   * effects this transform.
   *
   * @param size the size of something in the viewport coordinates.
   * @return the size of the same thing in the world coordinates.
   */
  public Size mapViewportToWorld(Size size) {
    return (_viewport.mapViewportToWorld(size));
  }


  /**
   * Map a rectangle from world coordinates into this graph panels
   * viewport coordinates.  The world coordinates are those that the
   * DrawableGraph lives in, the viewport coordinates are screen
   * coordinates.
   *
   * @param rect a rectangle in world coordinates.
   * @return a rectangle in viewport (screen) coordinates.
   */
  public Rectangle2D mapWorldToViewport(Rectangle2D rect) {
    return (_viewport.mapWorldToViewport(rect));
  }

  public double mapWorldToViewport(double length) {
    return (_viewport.mapWorldToViewport(length));
  }


  /**
   * Disable the HitTester instance for a DrawableGraphElement. It may
   * be reenabled later.
   *
   * @param e the graph element whose HitTester we want to disable.
   */
  public void disableHitTester(DrawableGraphElement e) {
    _hitMap.disableHitTester(e);
  }


  /**
   * Enable a previously disabled HitTester for a graph element.
   *
   * @param e the graph element whose HitTesters we want to reenable.
   */
  public void enableHitTester(DrawableGraphElement e) {
    _hitMap.enableHitTester(e);
  }


  /**
   * @param the DrawableGraphElement whose HitTesters we want.
   * @return an Iterator over the list of all HitTesters associated with
   *  a graph element.
   */
  public Iterator getHitTesters(DrawableGraphElement e)
  {
    return _hitMap.listHitTesters(e);
  }


  //////////////////////////////////////////////////
  // begin HitTestListener Interface Methods
  //

  /**
   * Part of the HitTestListener interface, this will fire a
   * DrawableGraphEvent.
   *
   * @return TRUE if the hit is consumed by any of the potential
   * listeners.
   */
  public boolean hitTestSuccess(String decorationId,
                                DrawableGraphElement el,
                                Object context,
                                MouseEvent evt)
  {
    boolean consumed = false;

    EventListenerList listenerList =
      (EventListenerList)_graphEventListeners.get(decorationId);

    if (listenerList != null) {
      // Guaranteed to return a non-null array
      Object[] listeners = listenerList.getListenerList();
      for (int i = listeners.length - 2; i >= 0; i -= 2) {
        if (listeners[i] == DrawableGraphMouseEventListener.class) {
          consumed = consumed ||
            ((DrawableGraphMouseEventListener)listeners[i+1]).
            handleDrawableGraphMouseEvent(new DrawableGraphMouseEvent
                                          (decorationId, el, context, evt));
        }
      }
    }

    return (consumed);
  }


  /** Part of the HitTestListener interface */
  public void hitTestMissedGraph(MouseEvent evt) {
    EventListenerList listenerList =
      (EventListenerList)_graphEventListeners.get(OFFGRAPH_DECORATION_ID);

    if (listenerList != null) {
      // Guaranteed to return a non-null array
      Object[] listeners = listenerList.getListenerList();
      for (int i = listeners.length - 2; i >= 0; i -= 2) {
        if (listeners[i] == DrawableGraphMouseEventListener.class) {
          ((DrawableGraphMouseEventListener)listeners[i+1]).
            handleDrawableGraphMouseEvent(new DrawableGraphMouseEvent
                                          (OFFGRAPH_DECORATION_ID, null, this, evt));
        }

      }
    }
  }

  /** Part of the HitTestListener interface */
  public void multipleHitTestSuccess(Object[] hitElements,
                                     Rectangle2D rect)
  {
    EventListenerList listenerList =
      (EventListenerList)_graphEventListeners.get(MULTIPLE_HIT_LISTENER_ID);

    if (listenerList != null) {
      // Guaranteed to return a non-null array
      Object[] listeners = listenerList.getListenerList();
      for (int i = listeners.length - 2; i >= 0; i -= 2) {
        if (listeners[i] == DrawableGraphMultipleSelectionEventListener.class) {
          DrawableGraphMultipleSelectionEventListener listener = (DrawableGraphMultipleSelectionEventListener)listeners[i+1];
          DrawableGraphMultipleSelectionEvent evt =
            new DrawableGraphMultipleSelectionEvent(hitElements, rect);
          listener.handleMultipleHitTestSuccess(evt);
        }
      }
    }
  }


  /** Part of the HitTestListener interface */
  public void multipleHitTestMissedGraph(Rectangle2D rect) {
    EventListenerList listenerList =
      (EventListenerList)_graphEventListeners.get(MULTIPLE_HIT_LISTENER_ID);

    if (listenerList != null) {
      // Guaranteed to return a non-null array
      Object[] listeners = listenerList.getListenerList();
      for (int i = listeners.length - 2; i >= 0; i -= 2) {
        if (listeners[i] == DrawableGraphMultipleSelectionEventListener.class) {
          DrawableGraphMultipleSelectionEventListener listener = (DrawableGraphMultipleSelectionEventListener)listeners[i+1];
          listener.handleMultipleHitTestMissedGraph(rect);
        }
      }
    }
  }

  //
  // end HitTestListener Interface Methods
  //////////////////////////////////////////////////



  //////////////////////////////////////////////////
  // Scrollable Interface Methods
  //

  /** Part of the Scrollable interface */
  public int getScrollableUnitIncrement(Rectangle visibleRect,
                                        int orientation,
                                        int direction)
  {
    int currentPosition = 0;
    if (orientation == SwingConstants.HORIZONTAL)
      currentPosition = visibleRect.x;
    else
      currentPosition = visibleRect.y;

    if (direction < 0) {
      int newPosition = currentPosition -
        (currentPosition / MAX_UNIT_INCREMENT) * MAX_UNIT_INCREMENT;
      return (newPosition == 0) ? MAX_UNIT_INCREMENT : newPosition;
    } else {
      return ((currentPosition / MAX_UNIT_INCREMENT) + 1) * MAX_UNIT_INCREMENT -
        currentPosition;
    }
  }

  /** Part of the Scrollable interface */
  public int getScrollableBlockIncrement(Rectangle visibleRect,
                                         int orientation,
                                         int direction)
  {
    if (orientation == SwingConstants.HORIZONTAL)
      return visibleRect.width - MAX_UNIT_INCREMENT;
    else
      return visibleRect.height - MAX_UNIT_INCREMENT;
  }

  /** Part of the Scrollable interface */
  public Dimension getPreferredScrollableViewportSize() {
    return getPreferredSize();
  }

  /** Part of the Scrollable interface */
  public boolean getScrollableTracksViewportWidth() {
    return false;
  }

  /** Part of the Scrollable interface */
  public boolean getScrollableTracksViewportHeight() {
    return false;
  }

  // end Scrollable Interface Methods
  //
  //////////////////////////////////////////////////


  //////////////////////////////////////////////////
  // begin MouseListener Interface Methods
  //

  /** Part of the MouseListener interface. */
  public void mouseClicked(MouseEvent e) {
    dispatchEvent(e);
  }

  /** Part of the MouseListener interface. */
  public void mouseReleased(MouseEvent e) {
    dispatchEvent(e);
  }

  /** Part of the MouseListener interface. */
  public void mousePressed(MouseEvent e) {
    dispatchEvent(e);
  }

  /** Part of the MouseListener interface. */
  public void mouseEntered(MouseEvent e) { }


  /** Part of the MouseListener interface. */
  public void mouseExited(MouseEvent e) { }

  //
  // end MouseListener Interface Methods
  //////////////////////////////////////////////////


  //////////////////////////////////////////////////
  // begin MouseMotionListener Interface Methods
  //

  /** Part of the MouseMotionListener interface. */
  public void mouseMoved(MouseEvent e) {
    dispatchEvent(e);
  }

  /** Part of the MouseMotionListener interface. */
  public void mouseDragged(MouseEvent e) {
    dispatchEvent(e);
  }

  //
  // end MouseMotionListener Interface Methods
  //////////////////////////////////////////////////


  //////////////////////////////////////////////////
  // begin MarqueeListener Interface Methods


  /** Part of the MarqueeListener interface */
  public void marqueeSelected(Rectangle rect) {
    //          System.err.println("GraphPanel: marqueeSelection event received: " + rect);
    // construct a fake awt Event and perform hit test?
    if (_graph == null) {
      return;
    }

    if (_hitTestingEnabled) {
      _graph.performHitTesting(_hitMap,
                               _viewport,
                               _viewport.mapViewportToWorld(rect));
    }
  }

  // end MarqueeListener Interface Methods
  //////////////////////////////////////////////////


  //////////////////////////////////////////////////
  // begin GraphAnimationPane Interface Methods

  /**
   * This should be called before call to getAnimationGraphics
   *
   * @param element
   */
  public void startAnimation(DrawableGraphElement element) {
    _bufferedImageManager.setForegroundElement(element);
    _animating = true;
  }


  /**
   * This should be called once an animation is finished.
   */
  public void stopAnimation() {
    if (_animating) {
      _animating = false;
      _bufferedImageManager.clearForegroundElements();
    }
  }

  public Graphics2D getAnimationGraphics() {
    return (_bufferedImageManager.getAnimationGraphics());
  }

  public void paintAnimationImmediately(Rectangle2D bounds) {
    if (bounds != null) {
      paintImmediately(bounds.getBounds());
    }
    else {
      paintImmediately();
    }
  }

  //
  // end GraphAnimationPane Interface Methods
  //////////////////////////////////////////////////





  /*
   * Send a mouse event to the graph for hit testing.  If any of the
   * objects in the graph respond, then they will be calling the
   * hitTestSuccess() method in this class.
   */
  private void dispatchEvent(MouseEvent e)
  {
    _currentState.dispatchEvent(e);
  }


  /*
   * Set the needs-redraw flag by setting the 'needs-repaint' flag
   * in the context.
   */
  private void setNeedsRedraw(boolean b) {
    if (b) {
      // reset the 'needs repaint' flag in all the elements
      _context.setNeedsRepaint();
    }
    _needsRedraw = b;
  }


  /*
   * Helper method to layout the graph.
   *
   * Side effects:
   *   Resets the _needsLayout flag.
   *   Updates the viewport world bounds.
   */
  private void layoutGraph(Graphics2D g)
  {
    if (_layout == null)
    {
      _needsLayout = false;
      return; // do nothing
    }

    _layout.performLayout(g,
                          _graph,
                          _viewport.getScale(),
                          _context);
    _needsLayout = false;
    _viewport.setWorldBounds(_context.getOverallBounds(MARGIN));
  }



  /**
   * Set up the static and redraw nodes/edges lists. We include static
   * nodes attach to moving edges in the redraw list.
   */
  private void setMovingNodes(HashSet movingNodes, boolean outGoingOnly)
  {
    HashSet elements = new HashSet();

    for (Iterator itr=movingNodes.iterator(); itr.hasNext();) {
      elements.add(itr.next());
    }

    for (EdgeIterator itr=_graph.edgesIterator(); itr.hasNext();)
    {
      DrawableEdge edge = itr.next();

      if (outGoingOnly)
      {
        if (movingNodes.contains(edge.getTail()))
        {
          elements.add(edge);
        }
      }
      else
      {
        if (movingNodes.contains(edge.getHead()) ||
            movingNodes.contains(edge.getTail()))
        {
          elements.add(edge);
          if (!elements.contains(edge.getHead())) {
            elements.add(edge.getHead());
          }
          if (!elements.contains(edge.getTail())) {
            elements.add(edge.getTail());
          }
        }
      }
    }

    _bufferedImageManager.setForegroundElements(elements);
  }



  // ------------------------------------------------------------



  /**
   * Takes care of buffer mixing.
   *
   * This is the class responsible for actually rendering the
   * graph.
   *
   */
  private class BufferedImageManager {

    private static final int DIRTY_RECT_PADDING = 15;

    /**
     * The main image buffer used by GraphPanel which corresponds to
     * what's drawn on screen.
     */
    private BufferedImage __bufferedImage = null;

    /**
     * An image buffer that only contains static elements that are not
     * in the foreground (i.e., elements that are NOT being dragged).
     *
     * @see #setForegroundElements
     */
    private BufferedImage __backBuffer = null;

    /**
     * Graphics context of our main image buffer.
     */
    private Graphics2D __g2d = null;

    /**
     * Set of elements in the foreground (i.e., element that are being
     * dragged) such that G = elementsToRedraw U staticElements.
     *
     * @see #setForegroundElements
     */
    //    private HashSet __elementsToRedraw = new HashSet();
    private HashSet __nodesToRedraw = new HashSet();
    private HashSet __edgesToRedraw = new HashSet();

    /*
     * Set of elements that are not in the foreground (i.e., element
     * that are NOT being dragged) such that
     * G = elementsToRedraw U staticElements.
     *
     * @see #setForegroundElements
     */
    //    private HashSet __staticElements = new HashSet();
    private HashSet __staticNodes = new HashSet();
    private HashSet __staticEdges = new HashSet();

    private Rectangle2D __lastDirtyRect = new Rectangle2D.Double();

    /**
     * Stores the width/height of the maing buffered image into
     * "return value" <b>rv</b> and returns <b>rv</b>.  If rv is null
     * a new Dimension object is allocated.  This version of getSize()
     * is useful if the caller wants to avoid allocating a new
     * Dimension object on the heap.
     *
     * @param rv the return value, modified to the component's size
     * @return rv
     */
    public Dimension getBufferSize(Dimension rv)
    {
      if (__bufferedImage == null) {
        return (null);
      }

      if (rv == null) {
        return (new Dimension(__bufferedImage.getWidth(),
                              __bufferedImage.getHeight()));
      }
      else {
        rv.setSize(__bufferedImage.getWidth(), __bufferedImage.getHeight());
        return (rv);
      }
    }

    /**
     * If clip is TRUE, force the buffer size to be the specified
     * width and height. If clip is FALSE, then only adjust buffer
     * size if width and height is greater than current buffer size.
     */
    public synchronized void updateBufferSize(int width,
                                              int height,
                                              boolean clip)
    {
      if (__bufferedImage == null) {
        //        System.err.println(">>>>> __bufferedImage is null");
        return;
      }

      boolean needsUpdate = false;

      if (!clip) {
        if ((width - (2*PAD)) > __bufferedImage.getWidth()) {
          needsUpdate = true;
        }

        if ((height - (2*PAD)) > __bufferedImage.getHeight()) {
          needsUpdate = true;
        }
      }

      if (needsUpdate || clip) {
        BufferedImage newbuf =
          new BufferedImage(width, height,
                            BufferedImage.TYPE_USHORT_565_RGB);

        __g2d = (Graphics2D) newbuf.getGraphics();
        __g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                               RenderingHints.VALUE_ANTIALIAS_ON);
        __g2d.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING,
                               RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
        __g2d.setRenderingHint(RenderingHints.KEY_RENDERING,
                               RenderingHints.VALUE_RENDER_QUALITY);
        __g2d.setBackground(getBackground());

        __g2d.clearRect(0, 0, width, height);

        __g2d.drawImage(__bufferedImage, 0, 0, GraphPanel.this);
        __bufferedImage = newbuf;
        System.gc();
        setSize(width, height);
        setPreferredSize(new Dimension(width, height));
        revalidate();
      }
    }


    /**
     * Copies the content of the main image buffer to the static
     * image buffer.
     */
    public synchronized void copyToBackBuffer()
    {
      Graphics2D g2d = newBackBuffer();
      g2d.drawImage(__bufferedImage, 0, 0, GraphPanel.this);
    }


    /**
     * Blast the buffer containing our static elements to our main image
     * buffer.
     */
    public void blitBackBuffer()
    {
      if (__backBuffer != null) {
        __g2d.drawImage(__backBuffer, 0, 0, GraphPanel.this);
      }
    }


    /**
     * Just add a single element to the foreground.
     */
    public void setForegroundElement(DrawableGraphElement e)
    {
      if (e instanceof DrawableNode) {
        __nodesToRedraw.add(e);
      }
      else if (e instanceof DrawableEdge) {
        __edgesToRedraw.add(e);
      }
      setForegroundElements();
    }


    /**
     * Add a set of elements to the foreground.
     *
     * @param elements a list of DrawableGraphElements.
     */
    public void setForegroundElements(HashSet elements)
    {
      for(Iterator i = elements.iterator(); i.hasNext(); ) {
        DrawableGraphElement e = (DrawableGraphElement)i.next();
        if (e instanceof DrawableNode) {
          __nodesToRedraw.add(e);
        }
        else if (e instanceof DrawableEdge) {
          __edgesToRedraw.add(e);
        }
      }
      setForegroundElements();
    }


    /**
     * Clean up and rebuild the main image buffer quickly.
     */
    public void clearForegroundElements()
    {
      __g2d.drawImage(__backBuffer, 0, 0, GraphPanel.this);

      for (Iterator itr=__edgesToRedraw.iterator(); itr.hasNext();) {
        DrawableEdge edge = (DrawableEdge) itr.next();
        //
        // Render graph elements.
        //
        edge.draw(__g2d, _viewport, _hitMap, _context, false, false);
      }

      for (Iterator itr=__nodesToRedraw.iterator(); itr.hasNext();) {
        DrawableNode node = (DrawableNode) itr.next();
        //
        // Render graph elements.
        //
        node.draw(__g2d, _viewport, _hitMap, _context, false, false);
      }

      paintScreenBuffer();
      clearElementsToRedraw();
      clearStaticElements();
      System.gc();
    }

    /**
     * Get the main image buffer which will be eventually blitted on
     * to the screen.
     */
    public BufferedImage getBufferedImage() {
      return __bufferedImage;
    }


    /**
     * Allocate our main image buffer in memory.
     */
    public synchronized Graphics2D allocateBuffer(int width, int height)
    {
      if (__bufferedImage == null || __g2d == null ||
          !(__bufferedImage.getWidth() == width &&
            __bufferedImage.getHeight() == height))
      {
        __bufferedImage = null;
        System.gc();
        __bufferedImage = new BufferedImage(width, height,
                                            BufferedImage.TYPE_USHORT_565_RGB);

        __g2d = (Graphics2D) __bufferedImage.getGraphics();
        __g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                               RenderingHints.VALUE_ANTIALIAS_ON);
        __g2d.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING,
                               RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
        __g2d.setRenderingHint(RenderingHints.KEY_RENDERING,
                               RenderingHints.VALUE_RENDER_QUALITY);
        __g2d.setBackground(getBackground());
      }

      __g2d.clearRect(0, 0, width, height);

      return (__g2d);
    }


    /**
     * Either:
     *
     *     Draw the entire graph (when there is nothing in our
     *     "elements-to-redraw" list.
     *
     * Or:
     *
     *     Superimpose the "static" elements with the "elements-to-redraw"
     *
     *
     *
     * @param newBuffer ???
     */
    public synchronized Rectangle2D updateBuffer(boolean newBuffer)
    {
      if (__g2d != null) {
        if (__nodesToRedraw.size() == 0 && __edgesToRedraw.size() == 0) {
          __g2d.setClip(null);
          //
          // Render the entire graph
          //
          _graph.draw(__g2d, _viewport, _hitMap,
                      _context, newBuffer, !newBuffer);
        }
        else {
          // caculate the dirty rectangle
          Rectangle2D dirtyRect = _viewport.mapWorldToViewport
            (_context.getOverallBounds(__nodesToRedraw.iterator(),
                                       DIRTY_RECT_PADDING));

          if (dirtyRect != null) {
            if (__lastDirtyRect != null) {
              Rectangle2D tmp = new Rectangle2D.Double();
              tmp.setRect(dirtyRect);
              dirtyRect.add(__lastDirtyRect);
              __lastDirtyRect = tmp;
            }
            __g2d.setClip(dirtyRect);
          }

          blitBackBuffer();

          for(Iterator itr=__edgesToRedraw.iterator(); itr.hasNext();) {
            DrawableEdge edge = (DrawableEdge) itr.next();
            _context.setNeedsRepaint(edge, true);
            //
            // Render inidividual graph elements.
            //
            edge.draw(__g2d, _viewport, _hitMap, _context, newBuffer, false);
          }

          for(Iterator itr=__nodesToRedraw.iterator(); itr.hasNext();) {
            DrawableNode node = (DrawableNode) itr.next();
            _context.setNeedsRepaint(node, true);
            //
            // Render inidividual graph elements.
            //
            node.draw(__g2d, _viewport, _hitMap, _context, newBuffer, false);
          }

          return dirtyRect;
        }
      }
      return null;
    }

    public Graphics2D getAnimationGraphics() {
      //
      // Fix: for now just return the primary buffer graphics
      //
      blitBackBuffer();

      return (__g2d);
    }

    /**
     * Allocate a new buffer that will contain our static elements.
     */
    private synchronized Graphics2D newBackBuffer()
    {
      Graphics2D g2d = null;
      if (__backBuffer == null ||
          !(__backBuffer.getWidth() == __bufferedImage.getWidth() &&
            __backBuffer.getHeight() == __bufferedImage.getHeight()))
      {
        //        System.err.println("newBackBuffer: NEW");
        __backBuffer = new BufferedImage(__bufferedImage.getWidth(),
                                         __bufferedImage.getHeight(),
                                         BufferedImage.TYPE_USHORT_565_RGB);
        g2d = (Graphics2D) __backBuffer.getGraphics();
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                             RenderingHints.VALUE_ANTIALIAS_ON);
        g2d.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING,
                             RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
        g2d.setRenderingHint(RenderingHints.KEY_RENDERING,
                             RenderingHints.VALUE_RENDER_QUALITY);
      }
      else {
        //        System.err.println("newBackBuffer: OLD");
        g2d = (Graphics2D)__backBuffer.getGraphics();
        g2d.clearRect(0, 0, __backBuffer.getWidth(), __backBuffer.getHeight());
      }

      return g2d;
    }


    /**
     * Only call this if __nodesToRedraw and __edgesToRedraw are already setup.
     */
    private synchronized void setForegroundElements() {

      for (NodeIterator itr=_graph.nodesIterator(); itr.hasNext();) {
        DrawableNode node = itr.next();
        if (!__nodesToRedraw.contains(node)) {
          __staticNodes.add(node);
        }
      }

      for (EdgeIterator itr=_graph.edgesIterator(); itr.hasNext();) {
        DrawableEdge edge = itr.next();
        if (!__edgesToRedraw.contains(edge)) {
          __staticEdges.add(edge);
        }
      }

      Graphics2D g2d = newBackBuffer();
      g2d.setBackground(getBackground());
      g2d.clearRect(0, 0, __backBuffer.getWidth(), __backBuffer.getHeight());

      for (Iterator itr=__staticEdges.iterator(); itr.hasNext();) {
        DrawableEdge edge = (DrawableEdge) itr.next();
        _context.setNeedsRepaint(edge, true);
        //
        // Render the individual "static" elements.
        //
        edge.draw(g2d, _viewport, _hitMap, _context, true, false);
      }

      for (Iterator itr=__staticNodes.iterator(); itr.hasNext();) {
        DrawableNode node = (DrawableNode) itr.next();
        _context.setNeedsRepaint(node, true);
        //
        // Render the individual "static" elements.
        //
        node.draw(g2d, _viewport, _hitMap, _context, true, false);
      }

      // initialized our dirty rectangle that contains the foreground
      // elements.
      __lastDirtyRect =  _viewport.mapWorldToViewport
        (_context.getOverallBounds(__nodesToRedraw.iterator(),
                                   DIRTY_RECT_PADDING));
    }

    private void clearElementsToRedraw() {
      __nodesToRedraw.clear();
      __edgesToRedraw.clear();
    }

    private void clearStaticElements() {
      __staticNodes.clear();
      __staticEdges.clear();
    }

  } // end private class BufferedImageManager



  // ---------------------------------------------------
  // State stuff.
  // ---------------------------------------------------


  /**
   * The super class of all states.
   */
  private abstract class PanelState {

    /*
     * This is the normal event dispatching code.
     */
    public void dispatchEvent(MouseEvent e) {
      if (_graph == null) {
        return;
      }
      if (_hitTestingEnabled) {
        Point2D pt = _viewport.mapViewportToWorld(e.getPoint());
        Rectangle2D rect = new Rectangle2D.Double(pt.getX()-1, pt.getY()+1, 3, 3);
        _graph.performHitTesting(_hitMap, _viewport, rect, e);
      }
    }


    /*
     * @param e the element to edit
     * @param loc the point where the mouse clicked
     * @param ctx the context object from the UI.
     */
    public void startElementEdit(DrawableGraphElement e,
                                 HashSet eset,
                                 Point loc,
                                 Object ctx,
                                 boolean optimize) { }
    public void stopElementEdit() { }


    public void startDrag(Collection dragNodes) { }
    public void dragTo(int dx, int dy) { }
    public void finishDrag() { }
    public boolean getInDrag() {
      return(false);
    }

  }// end class "PanelState"



  /**
   * The IDLE state.  This is the normal state when nothing state
   * related is happening.
   */
  private class IdleState extends PanelState {

    public void startElementEdit(DrawableGraphElement e,
                                 HashSet set,
                                 Point loc,
                                 Object ctx,
                                 boolean optimize)
    {
      _currentState = _textEditState;
      ((ElementEditState) _currentState).init(e, set, loc, ctx, optimize);
    }

    public void startDrag(Collection dragNodes) {
      _currentState = _dragState;
      ((DragState) _currentState).init(dragNodes);
    }

  }// end class "IdleState"



  /**
   * The state for editing of nodes or edges.
   */
  private class ElementEditState extends PanelState {

    private KeyListener __listener;
    private DrawableGraphElement __element;
    private boolean __optimize;

    public void init(DrawableGraphElement e,
                     HashSet affectedElements,
                     Point loc,
                     Object ctx,
                     boolean optimize)
    {
      if (_graphUI.setupEditSession(e, loc, ctx)) {

        __element = e;
        __optimize = optimize;

        __listener =
          new KeyListener() {
            public void keyPressed(KeyEvent e) {

              byte flags = _graphUI.keyPressed(e);
              if ((flags & GraphUI.FLAG_CONTINUE_EDIT) != 0) {
                if ((flags & GraphUI.FLAG_NEEDS_REPAINT) != 0) {
                  paintImmediately();
                }
              }
              else {
                stopElementEdit();
              }
            }
            public void keyReleased(KeyEvent e) { }
            public void keyTyped(KeyEvent e) { }
          };

        addKeyListener(__listener);

        if (__optimize) {
          //_bufferedImageManager.setForegroundElement(e);
          if (affectedElements != null) {
            setMovingNodes(affectedElements, true);
          }
          //_bufferedImageManager.updateBuffer(false);
        }
        //paintImmediately();
        paintScreenBuffer();
      }
      else {
        // Edit mode has been denied by the UI
        _currentState = _idleState;
      }
    }


    public void stopElementEdit() {
      removeKeyListener(__listener);
      __listener = null;

      _graphUI.teardownEditSession();

      _context.invalidate(__element);

      if (__optimize) {
        _bufferedImageManager.clearForegroundElements();
      }
      paintImmediately();

      _currentState = _idleState;
    }


    public void dispatchEvent(MouseEvent e) {

      byte flags = _graphUI.mouseEvent(e);

      if ((flags & GraphUI.FLAG_CONTINUE_EDIT) != 0) {
        if ((flags & GraphUI.FLAG_NEEDS_REPAINT) != 0) {
          paintScreenBuffer();
        }
      }
      else {
        stopElementEdit();
        _currentState.dispatchEvent(e);
      }
    }

  }// end class "ElementEditState"




  /**
   * The state for dragging nodes or edges.
   */
  private class DragState extends PanelState {

    private HashSet __draggedNodes = new HashSet();


    public void init(Collection dragNodes) {
      //
      // Build a list of graph elements we need to redraw. this will at
      // least include the nodes in dragNodes. it will also include all
      // edges attached to dragNodes, and all nodes attached to these
      // edges.
      //
      for (Iterator itr=dragNodes.iterator(); itr.hasNext();) {
        __draggedNodes.add(itr.next());
      }

      setMovingNodes(__draggedNodes, false);
      //_bufferedImageManager.updateBuffer(false);
      paintScreenBuffer();
    }

    public void dragTo(int dx, int dy) {
      double dxw = _viewport.mapViewportToWorld(dx);
      double dyw = -1 * _viewport.mapViewportToWorld(dy);

      // update the coordinates of nodes in _draggedNodes
      for (Iterator itr=__draggedNodes.iterator(); itr.hasNext();) {
        DrawableNode node = (DrawableNode) itr.next();
        Rectangle2D bounds = _context.getBounds(node);
        bounds.setRect(bounds.getX() + dxw, bounds.getY() + dyw,
                       bounds.getWidth(), bounds.getHeight());
        _context.setBounds(node, bounds);
      }

      paintScreenBuffer();
    }


    public void finishDrag() {
      __draggedNodes.clear();
      _bufferedImageManager.clearForegroundElements();
      repaint();
      _currentState = _idleState;
    }


    public boolean getInDrag() {
      return(true);
    }

  }// end class "DragState"


} // end class GraphPanel
