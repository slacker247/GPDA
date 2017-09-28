package com.appliedminds.martini;


import java.awt.event.MouseEvent;



/**
 * This type of event is emitted by the GraphPanel when some kind of
 * mouse stimulus is applied to the displayed graph.  The event
 * particulars are determined by the current GraphUI instance that is
 * operating with the DrawableGraph.
 *
 * <p>Each GraphUI instance defines a set of decoration identifiers
 * that it uses to tag its events.  When registering for events from
 * the GraphPanel you can specifiy the decoration id that you are
 * interested in.  A GraphUI is supposed to list its decoration id's
 * in its class comments.
 *
 * <p>In addition to the decoration id, a context object is passed
 * around with this event.  The context object is of type "object" and
 * can be anything.  Its actualy type is determined by the GraphUI.
 * The type of the context should also be clearly stated in the class
 * comments of the GraphUI instance.
 *
 * <p>The third piece of data that is packaged inside these events, is
 * the original AWT MouseEvent.  This is the events that triggered the
 * DrawableGraphMouseEvent.
 *
 *
 *
 * @author mathias@apmindsf.com
 */
public class DrawableGraphMouseEvent {


  private MouseEvent _event;
  private DrawableGraphElement _element;
  private Object _context;
  private String _decoration;


  /**
   * Create a new DrawableGraphMouseEvent.
   *
   * @param decorationId the constant value decoration identifier (as
   * specified by a GraphUI).
   * @param el the graph element (node or edge or null).
   * @param context some context object (could be null).
   * @param awtEvent the raw awt event that is the cause of this
   * event.
   */
  public DrawableGraphMouseEvent(String decorationId,
                                 DrawableGraphElement el,
                                 Object context,
                                 MouseEvent awtEvent)
  { 
    _decoration = decorationId;
    _element = el;
    _context = context;
    _event = awtEvent;
  }


  /**
   * Get the context object associated with this event.  The type of
   * this object is determined by the particular GraphUI instance in
   * use and may be null.
   *
   * @return the context object.
   */
  public Object getContext() {
    return _context;
  }


  /**
   * Set the context object associated with this event.
   */
  public void setContext(Object c) {
    _context = c;
  }


  /**
   * Get the graph element (node or edge) associated with this event.
   * This could be null.
   *
   * @return the graph element.
   */
  public DrawableGraphElement getGraphElement() {
    return _element;
  }


  /**
   * Get the AWT MouseEvent that this event is based on.  
   *
   * @return the MouseEvent that triggered this event, if there is
   * one.
   */
  public MouseEvent getMouseEvent() {
    return _event;
  }
  

  /** 
   * Generate a debugging string.
   */
  public String toString() {
    String what;
    switch(_event.getID()) {
    case MouseEvent.MOUSE_CLICKED: 
      what = "MOUSE_CLICKED";
      break;
    case MouseEvent.MOUSE_DRAGGED:
      what = "MOUSE_DRAGGED";
      break;
    case MouseEvent.MOUSE_ENTERED:
      what = "MOUSE_ENTERED";
      break;
    case MouseEvent.MOUSE_EXITED:
      what = "MOUSE_EXITED";
      break;
    case MouseEvent.MOUSE_MOVED:
      what = "MOUSE_MOVED";
      break;
    case MouseEvent.MOUSE_PRESSED:
      what = "MOUSE_PRESSED";
      break;
    case MouseEvent.MOUSE_RELEASED:
      what = "MOUSE_RELEASED";
      break;
    default:
      what = "?UNKNOWN?";
    }
    
    return("DrawableGraphMouseEvent{ " + 
           _decoration + " | " + 
           what + " | " + _event.getX() + "," + _event.getY() + " }");    
  }

} // end class DrawableGraphMouseEvent
