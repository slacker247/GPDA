package com.appliedminds.martini;


import java.awt.geom.Rectangle2D;


/**
 * This type of event is emitted by the GraphPanel when the panel
 * receives a marquee event and the parts of the underlying graph
 * intersects the marquee rectangle.
 *
 * <p>This event object contains all the graph elements that intersect
 * the marquee bounding rectangle along with the rectangle itself.
 *
 * <p>Those who wish to be notified of this event should register with
 * the GraphPanel and implement a
 * DrawableGraphMultipleSelectionEventListener
 *
 * @see DrawableGraphMultipleSelectionEventListener
 *
 * @author daepark@apmindsf.com
 */
public class DrawableGraphMultipleSelectionEvent {

	private Object[] _contexts;
	private Rectangle2D _rectangle;


	/**
	 * @param contexts the collection of graph elements that intersect
	 * the specified rectangle bounds (in VIEWPORT coordinates).
	 * @param rectangle the rectangle in which the hit testing was performed.
	 */
	public DrawableGraphMultipleSelectionEvent(Object[] contexts,
																						 Rectangle2D rectangle) 
	{
		_contexts = contexts;
		_rectangle = rectangle;
	}

	
	/**
   * Get the graph elements that intersect that hit test succeeded the
   * specified rectangle.
   *
   * @return the graph elements that intersect that hit test succeeded the
	 * specified rectangle.
   */
	public Object[] getContexts() {
		return (_contexts);
	}


	/**
	 * Get the rectangle in which the hit testing was performed.
	 *
	 * @return the rectangle in which the hit testing was performed.
	 */
	public Rectangle2D getRectangle() {
		return (_rectangle);
	}

} // end class DrawableGraphMultipleSelectionEvent
