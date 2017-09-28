package com.appliedminds.martini;

import java.awt.Graphics2D;
import java.awt.Graphics;
import java.awt.Shape;
import java.awt.event.MouseEvent;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.HashMap;
import java.util.Properties;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.beans.PropertyChangeEvent;
import javax.swing.event.EventListenerList;

/**
 * This is an abstract object that is the parent of graph elements
 * that can be drawn on the screen.  This takes care of some common
 * tasks for a drawable graph:
 *
 * <ul>
 * <li>Managing properties
 * <li>managing hit testing.
 * </ul>
 *
 * <p>Subclasses must implement the framework method, getGraph().
 *
 * <p><b>Except for hit testing</b>, all coordinate values and
 * dimensions here are in the world coordinate space.
 *
 * <p>This implements the low level hit testing algorithm, and uses a
 * list of HitTester associations passed in a HitTesterMap.
 *
 *
 * @see com.appliedminds.martini.DrawableNode
 * @see com.appliedminds.martini.DrawableEdge
 *
 * @author mathias@apmindsf.com
 */
public abstract class DrawableGraphElement {

  private boolean _visible = true;
  private HashMap _props;
  private boolean _hasChanged;


  /**
   * Create a new DrawableGraphElement.
   */
  public DrawableGraphElement()
  {
    _props = new HashMap();
    _hasChanged = false;
  }


  /**
   * @param v should this element be visible or not. this is a hint for
   * the specific GraphUI implementation used.
   */
  public void setVisible(boolean v) {
    _visible = v;
  }


  /**
   * @return true if this element's visibility hint is true.
   */
  public boolean isVisible() {
    return _visible;
  }


  /**
   * Return true if a property has changed since the last draw.
   */
  public boolean hasChanged() {
    return(_hasChanged);
  }


  public void setHasChanged(boolean hasChanged) {
    _hasChanged = hasChanged;
  }

  /**
   * Subclasses should call this method after it has drawn the element
   * accounting for the property changes.
   */
  public void clearChanges() {
    _hasChanged = false;
  }


  /**
   * Draw myself by delegating to a GraphUI instance held in the
   * DrawableGraph.
   *
   * @param g a Graphics Context.
   * @param hit the hit test map (modified).
   * @param ctx the drawing context.
   * @param newBuffer a rendering hint passed on to the UI indicating
   * that the graphics context is from a freshly created image buffer.
   * @param erase a rendering hint passed on to the UI indicating that
   * the UI should first try erasing the previously drawn self first
   * before drawing.
   */
  public abstract void draw(Graphics2D g,
                            Viewport vp,
                            HitTesterMap hit,
                            DrawableGraphContext ctx,
                            boolean newBuffer,
                            boolean erase);


  /**
   * Framework method that must be implemented by subclasses.
   *
   * @return the "parent" graph object.
   */
  public abstract DrawableGraph getGraph();



  /**
   * Get an arbitrary string property from the node.
   *
   * @param key to a property we want.
   * @return the value for the given key.
   */
  public String getProperty(String key)
  {
    Object res = _props.get(key);
    if (res == null) {
      return null;
    }

    if (res instanceof String) {
      return ((String) res);
    }
    else {
      return res.toString();
    }
  }


  /**
   * Get an arbitrary property value object from the node.
   *
   * @param key to a property we want.
   * @return the value for the given key.
   */
  public Object getPropertyObj(Object key)
  {
    return (_props.get(key));
  }


  /**
   * Get the set of property keys.
   *
   * @return the set of property keys.
   */
  public Iterator getPropertyKeys()
  {
    //return(_props.keySet().iterator());
    return(_props.keySet().iterator());
  }


  /**
   * Set an arbitrary string property on the node.  Could modify
   * the "hasChanged" state of this object.
   *
   * @param key the key to a property value we want to set on this node.
   * @param val the value for the key we want to set on this node.
   *
   * @throws MartiniError if key is null.
   */
  public void setProperty(Object key, Object val)
  {
    if (key == null) {
      throw(new MartiniError("Null value for property key"));
    }
    Object old = _props.get(key);
    if ((val != null) && (!val.equals(old)))
    {
      _props.put(key, val);
      _hasChanged = true;
    }
  }


  /**
   * Remove a property from this element.  Could modify the "hasChanged"
   * state of this object.
   *
   * @param key the key of the property to remove.
   */
  public void removeProperty(String key)
  {
    if ((key != null) && _props.containsKey(key))
    {
      _props.remove(key);
      _hasChanged = true;
    }
  }


  /**
   * Do a fine grained hit test on this node and also notify the
   * graph if we were hit, or if any object we have drawn on the screen
   * was entered or exited.
   *
   * @param map the HitTester/Element associative map.
   * @param rect the rectangle in VIEWPORT coordinates.
   * @param evt the raw awt event.
   *
   * @return a HitTestResul object.
   */
  public HitTestResult hitTest(HitTesterMap map,
                               Rectangle2D rect,
                               MouseEvent evt)
  {
    HitTester lastHit = null;
    for(Iterator i = map.listHitTesters(this); i.hasNext(); ) {
      HitTester ht = (HitTester) i.next();
      if (ht.hitTest(rect)) {
        if (!ht.isOver()) {
          ht.setIsOver(true);
          if (getGraph().notifyMouseEntered(ht.getDecorationId(), this, evt, ht.getContext())) {
            return(HitTestResult.hitTestConsumed(ht));
          }
        }
        if (getGraph().notifyHitTestSuccess(ht.getDecorationId(), this, evt, ht.getContext())) {
          return(HitTestResult.hitTestConsumed(ht));
        }
        lastHit = ht;
      }
      else {
        if (ht.isOver()) {
          ht.setIsOver(false);
          if (getGraph().notifyMouseExited(ht.getDecorationId(), this, evt, ht.getContext())) {
            return(HitTestResult.hitTestConsumed(ht));
          }
        }
      }
    }

    if (lastHit != null) {
      return(HitTestResult.hitTestHitNotConsumed(lastHit));
    }
    else {
      return(HitTestResult.hitTestMissed());
    }
  }


  /**
   * Perform a multiple hit test on all drawn graph objects associated
   * with this graph element.
   *
   * @param map the HitTester/Element associative map.
   * @param rect hit test succeeds for each HitTester/Element
   * association if and only if the drawn graph object shape is within
   * this rectangle (in VIEWPORT coordinates).
   * @return a list of HitTestResults for each HitTester/Element association
   * that pass this hit test.
   */
  public HitTestResult[] hitTest(HitTesterMap map,
                                 Rectangle2D rect)
  {
    List results = new ArrayList();
    for(Iterator i = map.listHitTesters(this); i.hasNext(); ) {
      HitTester ht = (HitTester) i.next();
      if (ht.hitTest(rect)) {
        results.add(HitTestResult.hitTestConsumed(ht));
      }
    }

    HitTestResult[] array = new HitTestResult[results.size()];
    int i = 0;
    Iterator itr = results.iterator();
    while (itr.hasNext()) {
      array[i++] = (HitTestResult)itr.next();
    }

    return (array);
  }


} // end class DrawableGraphElement
