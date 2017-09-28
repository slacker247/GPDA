package com.appliedminds.martini;


import java.awt.Shape;
import java.awt.geom.Rectangle2D;
import java.awt.geom.PathIterator;


/**
 * Associate the details of a drawn shape with a decoration
 * identifier.  Becuase this class knows about how a particular UI
 * element is drawn, it is the thing that performs the ultimate
 * fine-grained hit testing.  GraphUI implementations should create
 * these objects inside the draw() methods and store them in
 * DrawableNodes and DrawableEdges.  The DrawableNodes and
 * DrawableEdges will then use them for hit-testing.
 *
 *
 *
 * @author mathias@apmindsf.com
 * @author will@apmindsf.com
 */
public class HitTester {


  private Object _context;
  private Shape _shape;
  private String _decorationId;
  private boolean _isOverFlag;
  private Object[] _args;


  /**
   * Create a new hit tester.
   *
   * @param shape the screen shape.
   * @param decorationId the ui decoration identifier.
   * @param context the context of this hit test. It can be the actual
   * DrawableGraphElement, martinix.ui.TextScreenData, or any
   * pertinent information that needs to be passed on to the the hit
   * listener.
   */
  public HitTester(Shape shape, String decorationId, Object context) {
    _context = context;
    _shape = shape;
    _decorationId = decorationId;
    _isOverFlag = false;
  }


  /**
   * Perform a fine-grained hit test. Tests if the drawn shape
   * associated with this HitTester intersects the given
   * rectangle. Same as <code>hitTest(rect, false)<code>.
   *
   * @param rect the area to test whether or not the drawn shape
   * intersects it.
   * @return true if the hit succeeded.
   * @see #hitTest(Rectangle2D, boolean)
   */
  public boolean hitTest(Rectangle2D rect) {
    return (_shape.intersects(rect));
  }


  /**
   * Get the UI decoration id.
   * @return the identifier set in the constructor.
   */
  public String getDecorationId() {
    return(_decorationId);
  }


  /**
   * Get the context object associated with this hit test.
   *
   * @return the context object as set in the constructor.
   */
  public Object getContext() {
    return(_context);
  }


  public Shape getShape() {
    return (_shape);
  }


  /**
   * Keep track of a "mouse is over this object" flag to aid in event
   * dispatch.  This is used in hit testing.
   *
   * @return true if the "mouse is over this flag" is set.
   */
  public boolean isOver() {
    return(_isOverFlag);
  }


  /**
   * Set the "mouse is over this object" flag.  Used in hit testing.
   */
  public void setIsOver(boolean b) {
    _isOverFlag = b;
  }


  /**
   * Does this hit context have any additional arguments or information that
   * needs to be passed on to the hit listeners.
   *
   * @see #getArgs
   */
  public boolean hasArgs() {
    return (_args != null);
  }


  /**
   * Get any additional arguments or infromation that needs to be
   * passed on to the hit listeners. May return null.
   *
   * @see #hasArgs
   */
  public Object[] getArgs() {
    return (_args);
  }

} // end class "HitTester"


