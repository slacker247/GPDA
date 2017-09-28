package com.appliedminds.martini;

import java.util.HashMap;
import java.util.HashSet;
import java.awt.geom.Rectangle2D;
import java.util.Iterator;



/**
 * This collects data about the graph that relates to how it is drawn
 * on the screen.  This is in a different object so that you can share
 * a DrawableGraph object and then display it on different "screens".
 *
 *
 *
 * @author mathias@apmindsf.com
 * @author dae@apmindsf.com
 */
public class DrawableGraphContext {


  private HashMap _repaintMap;
  private HashMap _boundsMap;
  private HashSet _invalidSet;
  private HashMap _contextMap;


  /**
   * Create a new, empty graph context.
   */
  public DrawableGraphContext() {
    _repaintMap = new HashMap();
    _boundsMap = new HashMap();
    _invalidSet = new HashSet();
    _contextMap = new HashMap();
  }



  /**
   * Put arbitrary GUI data into a hash map.  This is provided for the
   * use of the GraphUI.  No other code should use this feature.
   *
   * @param el the drawable element (each element gets its own hash table).
   * @param key the key for the map.
   * @param obj the object to store.
   */
  public void putContextData(DrawableGraphElement el, String key, Object obj)
  {
    HashMap map = (HashMap) _contextMap.get(el);
    if (map == null) {
      map = new HashMap();
      _contextMap.put(el, map);
    }
    map.put(key, obj);
  }


  /**
   * Retrieve arbitrary GUI data from the elements hash map.  This is
   * provided for the use of the GraphUI.  No other code should use
   * this feature.
   *
   * @param el the drawable element (each element gets its own hash table).
   * @param key the key for the map.
   * @return obj the object stored in the map or null.
   */
  public Object getContextData(DrawableGraphElement el, String key) {
    HashMap map = (HashMap) _contextMap.get(el);
    if (map == null) {
      return(null);
    }
    return(map.get(key));
  }


  /**
   * Clear the hash map for the given elements.  This is provided for
   * the use of the GraphUI.  No other code should use this feature.
   *
   * @param el the drawable element whos map you want to clear.
   */
  public void clearContextData(DrawableGraphElement el) {
    _contextMap.remove(el);
  }


  /**
   * Invalidate the size information for the given graph element.
   * This should result a recalculation of the elements size as soon
   * as possible.
   *
   * @param e the element to invalidate.
   */
  public void invalidate(DrawableGraphElement e) {
    _invalidSet.add(e);
    clearContextData(e);
  }


  /**
   * Check if the element size data is valid.  Most client code will
   * not have to worry about this.  This is used by the graph
   * structure to determine if it needs to recalcualate the size of an
   * element.
   *
   * @param e the element to check.
   *
   * @see #invlaidate(DrawableGraphElement)
   */
  public boolean isValid(DrawableGraphElement e) {
//     if (e instanceof DrawableEdge) {
//       return (true); // hack since we do not need to calculate edge size
//     }
    return(_invalidSet.isEmpty() || (!_invalidSet.contains(e)));
  }


  /**
   * Check if the given graph element needs to be repainted.  This
   * should be used by the GraphUI to decide whether to paint the
   * element or not.
   *
   * @param el the element to check.
   */
  public boolean getNeedsRepaint(DrawableGraphElement el) {
    Boolean val = (Boolean) _repaintMap.get(el);
    return(Boolean.TRUE.equals(val));
  }


  /**
   * Set the 'needs repaint' rendering hint for the given graph element.
   *
   * @param el the element.
   * @param b the new value for the 'needs repaint' flag.
   */
  public void setNeedsRepaint(DrawableGraphElement el, boolean b) {
    setNeedsRepaint(el, b ? Boolean.TRUE : Boolean.FALSE);
  }


  /**
   * Set the 'needs repaint' flag to TRUE for all elements.
   */
  public void setNeedsRepaint() {
    for(Iterator i = _repaintMap.keySet().iterator(); i.hasNext(); ) {
      _repaintMap.put(i.next(), Boolean.TRUE);
    }
  }


  /**
   * Get the current bounds (position and extent) of an element.
   *
   * @param el an element.
   * @return the current bounds for the element in world coordinates.
   */
  public Rectangle2D getBounds(DrawableGraphElement el) {
    Rectangle2D bounds = (Rectangle2D) _boundsMap.get(el);
    return(bounds);
  }


  /**
   * Set the position and dimension of an element in world
   * coordinates.  We presume that this will be called during layout.
   * Immediately after this, calls to isValid will return true for the
   * given element.
   *
   * @param el the element
   * @param bounds the bounds in world coordinates.
   */
  public void setBounds(DrawableGraphElement el, Rectangle2D bounds) {
    _boundsMap.put(el, bounds);
    setNeedsRepaint(el, Boolean.TRUE);
    validate(el);
  }



  /**
   * Get the overall bounding rectangle of the set of graph elements.
   * This method currently does not cache the bounds, so this call can
   * be slow if the graph is big. The user may want to cache the
   * returned value.
   *
   * @param padding a padding value to add to the bounds.
   * @return the smallest rectangle that encloses all graph elements,
   * or a minimal bounds (0, 0, 1, 1) if there are no bounds data
   * available.
   */
  public Rectangle2D getOverallBounds(int padding)
  {
    double top, left, bottom, right;

    Iterator it = _boundsMap.keySet().iterator();
    if (!it.hasNext()) {
      return (new Rectangle2D.Double(0, 0, 1, 1));
    }

    return (getOverallBounds(it, padding));
  }



  /**
   *
   * @param itr of DrawableGraphElements
   * @param padding the padding value to add to the bounds.
   */
  public Rectangle2D getOverallBounds(Iterator itr, int padding)
  {
    double top, left, bottom, right;


    // initialize to the first node

    Rectangle2D firstBounds = null;
    while(itr.hasNext()) {
      firstBounds = (Rectangle2D) getBounds((DrawableGraphElement)itr.next());
      if (firstBounds != null) {
        break;
      }
    }

    if (firstBounds == null) {
      // Uh oh, no nodes -- (possibly, just edges)
      return(null);
    }

    left = firstBounds.getX();
    right = left + firstBounds.getWidth();
    top = firstBounds.getY();
    bottom = top - firstBounds.getHeight();

    while (itr.hasNext()) {
      Rectangle2D bounds = getBounds((DrawableGraphElement)itr.next());
      if (bounds == null) {
        continue;
      }

      double nodeRight = bounds.getX() + bounds.getWidth();
      double nodeBottom = bounds.getY() - bounds.getHeight();

      if (bounds.getX() < left)
        left = bounds.getX();

      if (nodeRight > right)
        right = nodeRight;

      if (bounds.getY() > top)
        top = bounds.getY();

      if (nodeBottom < bottom)
        bottom = nodeBottom;
    }

    left -= padding;
    top += padding;
    right += padding;
    bottom -= padding;

    return (new Rectangle2D.Double(left, top, (right - left), (top - bottom)));
  }


  /*
   * Set the needs-repaint flag for an element.
   */
  private void setNeedsRepaint(DrawableGraphElement el, Boolean b) {
    _repaintMap.put(el, b);
  }


  /*
   * Undo the effects of invalidate()
   */
  private void validate(DrawableGraphElement e) {
    if ((!_invalidSet.isEmpty()) && (_invalidSet.contains(e))) {
      _invalidSet.remove(e);
    }
  }

} // end class DrawableGraphContext
