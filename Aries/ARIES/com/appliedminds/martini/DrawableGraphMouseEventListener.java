package com.appliedminds.martini;


/**
 * Objects wishing to respond to DrawableGraphMouseEvents from the
 * GraphPanel should implement this interface.
 *
 * <p>These are gui/graph related events that are fired off by the
 * graph UI elements usually in response to user mouse activity.
 *
 * <P>Objects interested in the DrawableGraphMousEvents would usually
 * register with a GraphPanel.
 *
 * <p>GraphUI implementors interested in generating these events need
 * to create HitTester objects and store them in the nodes and edges.
 *
 *
 * @see com.appliedminds.martini.GraphPanel
 * @see com.appliedminds.martini.HitTester
 *
 *
 * @author mathias@apmindsf.com
 */
public interface DrawableGraphMouseEventListener 
	extends DrawableGraphEventListener 
{

  /** 
   * @return TRUE if you want to stop the propogation of this event in
   * the UI.  FALSE if you wish the event to go to the next eligable
   * UI component.
	 * @param e the DrawableGraphMouseEvent to be handled.
   */
	public boolean handleDrawableGraphMouseEvent(DrawableGraphMouseEvent e);

} // end interface DrawableGraphMouseEventListener
