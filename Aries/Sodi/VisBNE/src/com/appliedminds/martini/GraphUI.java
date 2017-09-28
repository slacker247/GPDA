package com.appliedminds.martini;

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.util.ArrayList;
import java.util.Iterator;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;


/**
 * A GraphUI implementation is responsible for the low-level drawing
 * functions.
 *
 * <p>Specifically, this object has these responsibilities:
 * <ul>
 *
 * <li>Draws to the Graphics2D object and creates HitTester objects.
 *
 * <li>Knows about a set of UI decoration id's that are attached to
 * specific events that are fired in response of user interation.
 * These decoration ids should be clearly listed and defined in the
 * class comments for the particular GraphUI implementation.
 *
 * <li>Can calculate the preferred sizes of nodes (and therefore
 * effects layout).
 *
 * <li>Handles the editing on graph elements (one at a time) if
 * required.
 *
 * <li>Must run as fast as possible.
 *
 * </ul>
 *
 * <P>In general, most of the methods in DrawableEdge and DrawableNode
 * actually delegate to an instance of GraphUI.  Hit testing must be
 * carefully configured by a GraphUI instance, it is not automatic.
 *
 *
 *
 * @author will@apmindsf.com
 * @author daepark@apmindsf.com
 * @author mathias@apmindsf.com
 */
public interface GraphUI {


  /** 
   * This flag could be present in the return value of mouseEvent() or
   * keyPressed() and if set, indicates that the edit session should
   * continue.  If this flag is cleared, then the edit should stop.
   *
   * @see #mouseEvent()
   * @see #keyPressed()
   */
  public static final byte FLAG_CONTINUE_EDIT = 0x1;

  /**
   * This flag could be present in the return value of mouseEvent() or
   * keyPressed() and if set, indicates that the element currently
   * being editid is in need of a repaint.  If this flag is cleared,
   * then the edit should stop.
   *
   * @see #mouseEvent()
   * @see #keyPressed()
   */
  public static final byte FLAG_NEEDS_REPAINT = 0x2;



  /**
   * Get the minmum size required by an node.  This is used as a
   * recommendation for the layout algorithm.
   *
   * @param node the node whose minimum size we want.
   */
  Size getMinimumSize(Graphics2D g, DrawableNode node, double scale);


  /**
   * Get the maximum size required by a node.  This is used as a 
   * recommendation for the layout algorithm.
   *
   * @param node the node whose maximum size we want.
   */
  Size getMaximumSize(Graphics2D g, DrawableNode node, double scale);


  /**
   * Get the preferred size required by a node.  This is used as a
   * reccommendation for the layout algorithm.
   *
   * @param node the node whose perferred size we want.
   */
  Size getPreferredSize(Graphics2D g, DrawableNode node, double scale);


  /**
   * Set the GraphPanel this GraphUI works with.
   *
   * @param graphPanel the GraphPanel this GraphUI is attached to.
   */
  void setGraphPanel(GraphPanel graphPanel);


  /**
   * Draw an edge onto the screen.
   *
   * @param graphics the graphics context.
   * @param edge the edge we want to draw.
   * @param vp the viewport used to map from world coordinates to
   * the screen coordinates.
   * @param hit the HitTesterMap for storing HitTester objects.
   * @param newBuffer a rendering hint to the UI indicating that the
   * graphics context is from a freshly created image buffer.
   * @param erase a rendering hint to the UI indicating that the UI *
   * should first try erasing the previously drawn self first before *
   * drawing.
   */
  void draw(Graphics2D graphics, 
            DrawableEdge edge, 
            Viewport vp, 
            HitTesterMap hit,
            DrawableGraphContext ctx,
            boolean newBuffer,
            boolean erase);


  /**
   * Draw a node onto the screen.
   *
   * @param graphics the graphics context.
   * @param node the node we want to draw.
   * @param vp the viewport used to map from world coordinates to
   * the screen coordinates.
   * @param hit the HitTesterMap for storing HitTester objects.
   * @param newBuffer a rendering hint to the UI indicating that the
   * graphics context is from a freshly created image buffer.
   * @param erase a rendering hint to the UI indicating that the UI *
   * should first try erasing the previously drawn self first before *
   * drawing.
   */
  void draw(Graphics2D graphics, 
            DrawableNode node, 
            Viewport vp, 
            HitTesterMap hit,
            DrawableGraphContext ctx,
            boolean newBuffer,
            boolean erase);


  /**
   * Validate a given graph. Create or update properties expected by
   * the GraphUI.
   *
   * @param g the DrawableGraph to validate.
   * @return true if the graph is validated, or false if we're unable to
   *   use the graph.
   */
  boolean validate(DrawableGraph g);


  /**
   * Called to request an edit session with the UI.  The UI can refuse
   * the session by returing false.  If the UI returns true, this
   * method should cause the UI to enter or set up some sort of "edit
   * session" which can be disposed of by calling
   * teardownEditSession().  While in the edit session, the caller can
   * make calls to keyPressed() and mouseEvent() as necessary.
   *
   * @param el the element to be edited.
   * @param loc the mouse point where the user clicked to begin 
   * the edit operation.  May be null.
   * @param ctx some context object associated with the element.  Eg, 
   * a TextScreenData object from the UI.
   *
   * @return true if the UI wants to enter into an edit session.
   *
   *
   * @see #keyPressed()
   * @see mouseEvent()
   * @see #teardownEditSession()
   */
  public boolean setupEditSession(DrawableGraphElement el, 
                                  Point loc,
                                  Object ctx);


  /** 
   * During interactive editing operations (an "edit session"), this
   * method is used to communicate keypress events to the UI.  This
   * method should only be called if a previous call to
   * setupEditSession() returned true.  This returns edit state
   * information back to the caller.
   *
   * @param e the key event from the awt.
   *
   * @return some collection of the edit FLAGS (defined in this
   * interface) or'd together.  
   *
   * @see @teardownEditSession()
   */
  public byte keyPressed(KeyEvent e);


  /** 
   * During interactive editing operations ("an edit session"), this
   * method is used to communicate mouse events to the UI.  This
   * method should only be called if a previous call to
   * setupEditSession() returned true.  This returns edit state
   * information back to the caller.
   *
   * @param e the mouse event from the awt.
   *
   * @return some collection of the edit FLAGS (defined in this
   * interface) or'd together.  
   *
   * @see @teardownEditSession()
   */
  public byte mouseEvent(MouseEvent e);


  /**
   * Tell the UI that the edit session has come to an end.  No more
   * calls to keyPressed() will be made unless a call to
   * setupEditSession() occurs first.
   *
   * @see #setupEditSession()
   */
  public void teardownEditSession();


} // end interface GraphUI
