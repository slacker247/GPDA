package com.appliedminds.martini;


import java.awt.geom.Rectangle2D;


/**
 * Those who wish to repsond to DrawableGraphMultipleSelectionEvents
 * from the GraphPanel should implement this interface.
 *
 * <p>These events are usually fired off by the GraphPanel in
 * response to a marqueeSelected event since the GraphaPanel is a
 * MarqueeListener.
 *
 * @see DrawableGraphMultipleSelectionEvent
 *
 * @author daepark@apmindsf.com
 */
public interface DrawableGraphMultipleSelectionEventListener 
	extends DrawableGraphEventListener 
{

	/**
	 * Succeeded in hit testing for a marquee rectangle bounds.
	 *
	 * @param evt a DrawableGraphMultipleSelectionEvent describing the
	 * graph elements that were hit.
	 */
	public void handleMultipleHitTestSuccess(DrawableGraphMultipleSelectionEvent evt);


	/**
	 * Hit test missed for the specified marquee rectangle.
	 *
	 * @param rectangle the marquee rectangle responsible for the hit
	 * test miss.
	 */
	public void handleMultipleHitTestMissedGraph(Rectangle2D rectangle);

} // end class DrawableGraphMultipleSelectionEventListener

