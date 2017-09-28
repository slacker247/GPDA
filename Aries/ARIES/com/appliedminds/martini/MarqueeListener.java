package com.appliedminds.martini;

import java.awt.Rectangle;



/**
 * A simple interface that gets called when a marquee event occurs in the
 * MarqueePane. An implementing class may be added to the marquee pane to
 * receive marquee events.
 *
 * @author will@apmindsf.com
 */
public interface MarqueeListener {

	/**
	 * @param the marquee selected by the user.
	 */
	void marqueeSelected(Rectangle marqueeBounds);

} // end interface MarqueeListener
