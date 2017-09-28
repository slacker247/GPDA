package com.appliedminds.martini.animation;

import com.appliedminds.martini.DrawableGraphElement;

import java.util.EventListener;


/**
 * <b>GraphAnimationListener</b> is notified of graph animation events
 * from a GraphAnimation. Those who want to be notified of
 * GraphElementAnimation events should implement this interface and
 * register with a GraphAnimation via its
 * <code>addGraphAnimationListener</code> method. This class is very
 * much like the AnimationListener interface but is specific to the
 * GraphElementAnimation object.
 * 
 * @author daepark@apmindsf.com
 */
public interface GraphAnimationListener extends EventListener {

	/**
	 * The GraphElementAnimation is being started.
	 */
	public void graphElementAnimationStarted(GraphElementAnimation animation);


	/**
	 * The GraphElementAnimation is being stopped.
	 */
	public void graphElementAnimationStopped(GraphElementAnimation animation);


	/**
	 * A new AnimationFrame for a DrawableGraphElement is available.
	 */
	public void newGraphElementAnimationFrame(DrawableGraphElement graphElement, 
																						AnimationFrame animationFrame);

} // end inteface GraphAnimationListener
