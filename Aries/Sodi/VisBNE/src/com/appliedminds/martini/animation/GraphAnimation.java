package com.appliedminds.martini.animation;

import com.appliedminds.martini.DrawableGraphElement;

import java.util.HashMap;
import javax.swing.Timer;
import java.awt.Graphics2D;


/**
 * <b>GraphAnimation</b> is a specific composite animation of
 * GraphElementAnimation objects. You can only add
 * GraphElementAnimation objects to a GraphAnimation. Currently, a
 * GraphAnimation can only animate one GraphElementAnimation at a time.
 *
 * <p> A GraphAnimation is constructed with a GraphAnimationPane as it
 * will attempt to draw any new frames from the collection of
 * GraphElementAnimation objects as soon as they are available. When
 * you add a GraphElementAnimation , the GraphAnimation will
 * automatically listen to the animation events of the
 * GraphElementAnimation. When a GraphElementAnimation notfies of a
 * new AnimationFrame, it will ask the GraphAnimationPane for an
 * animation graphics context and render the new AnimationFrame onto
 * it and then ask the GraphAnimationPane to paint the result
 * immediately via the
 * <code>GraphAnimationPane.paintAnimationImmediately</code> method.
 *
 * @see GraphAnimationPane
 * @author daepark@apmindsf.com
 */
public class GraphAnimation 
	extends CompositeAnimation 
{

	private GraphAnimationPane _animationPane = null;
	private DrawableGraphElement _readyToAnimate = null;


	/**
	 * Create a new GraphAnimation.
	 *
	 * @param animationPane a GraphAnimationPane used to draw the AnimationFrame
	 * whenever there is a new one.
	 */
	public GraphAnimation(GraphAnimationPane animationPane) {
		super();
		_animationPane = animationPane;
	}


	/**
	 * Add a GraphAnimationListener who wants to be notified of 
	 * GraphElementAnimation events.
	 */
	public void addGraphAnimationListener(GraphAnimationListener l) {
		eventListeners.add(GraphAnimationListener.class, l);
	}


	/**
	 * Remove a GraphAnimationListener who wants to be notified of 
	 * GraphElementAnimation events.
	 */
	public void removeGraphAnimationListener(GraphAnimationListener l) {	
		eventListeners.remove(GraphAnimationListener.class, l);
	}


	/**
	 * Add a GraphElementAnimation to this composite.
	 *
	 * <p><b>!!!DO NOI use addAnimation!!!</b>
	 */
	public void addGraphElementAnimation(GraphElementAnimation animation) {
		super.addAnimation(animation.getDrawableGraphElement(), animation);
	}


	/**
	 * Remove a GraphElementAnimation to this composite.
	 *
	 * <p><b>!!!DO NOI use removeAnimation!!!</b>
	 */
	public void removeGraphElementAnimation(GraphElementAnimation animation) {
		super.removeAnimation(animation.getDrawableGraphElement());
	}


	/**
	 * @see addGraphElementAnimation
	 */
	public final void addAnimation(Object animationKey, Animation animation) {
		throw (new RuntimeException("please use addGraphElementAnimation instead"));
	}


	/**
	 * @see removeGraphElementAnimation
	 */
	public final void removeAnimation(Object animationKey) {
		throw (new RuntimeException("please use removeGraphElementAnimation instead"));
	}

	
	/////////////////////////////////////
	// begin AnimationListener interface

	public void animationStarted(Animation animation) {
		// Ready animation pane to start animating the graph element
		GraphElementAnimation ga = (GraphElementAnimation)animation;
		DrawableGraphElement element = ga.getDrawableGraphElement();
		_animationPane.startAnimation(element);
		_readyToAnimate = element;

		// Notify listeners the animation has started
		fireGraphElementAnimationStarted(ga);
	}

	public void animationStopped(Animation animation) {
		// Reset animation pane back to its normal state.
		GraphElementAnimation ga = (GraphElementAnimation)animation;
		_animationPane.stopAnimation();

		_readyToAnimate = null;

		// Notify listseners the animation has stopped.
		fireGraphElementAnimationStopped(ga);
	}

	public void newAnimationFrame(AnimationFrame animationFrame) {
		GraphElementAnimation ga = 
			(GraphElementAnimation)animationFrame.getAnimation();

		// Ensure that the element is ready to be animated in the animation pane.
		ensureReadyToAnimate(ga.getDrawableGraphElement());

		// Draw the animation frame.
		Graphics2D graphics = _animationPane.getAnimationGraphics();
		animationFrame.draw(graphics);
		_animationPane.paintAnimationImmediately(animationFrame.getBounds());

		// Notify listeners there is a new animation frame.
		fireNewGraphElementAnimationFrame(ga.getDrawableGraphElement(), 
																			animationFrame);
	}

	// end AnimationListener interface
	/////////////////////////////////////


	/**
	 * Notify GraphAnimationListeners
	 */
	protected void fireGraphElementAnimationStarted
		(GraphElementAnimation animation) 
	{
		// Guaranteed to return a non-null array
		Object[] listeners = eventListeners.getListenerList();
		// Process the listeners last to first, notifying
		// those that are interested in this event
		for (int i = listeners.length-2; i>=0; i-=2) {
			if (listeners[i]==GraphAnimationListener.class) {
				((GraphAnimationListener)listeners[i+1]).
					graphElementAnimationStarted(animation);
			}
		}
	}


	/**
	 * Notify GraphAnimationListener
	 */
	protected void fireGraphElementAnimationStopped
		(GraphElementAnimation animation) 
	{
		// Guaranteed to return a non-null array
		Object[] listeners = eventListeners.getListenerList();
		// Process the listeners last to first, notifying
		// those that are interested in this event
		for (int i = listeners.length-2; i>=0; i-=2) {
			if (listeners[i]==GraphAnimationListener.class) {
				((GraphAnimationListener)listeners[i+1]).
					graphElementAnimationStopped(animation);
			}
		}
	}


	/**
	 * Notify GraphAnimationListener
	 */
	protected void fireNewGraphElementAnimationFrame
		(DrawableGraphElement element, 
		 AnimationFrame animationFrame)
	{
		// Guaranteed to return a non-null array
		Object[] listeners = eventListeners.getListenerList();
		// Process the listeners last to first, notifying
		// those that are interested in this event
		for (int i = listeners.length-2; i>=0; i-=2) {
			if (listeners[i]==GraphAnimationListener.class) {
				((GraphAnimationListener)listeners[i+1]).
					newGraphElementAnimationFrame(element, animationFrame);
			}
		}
	}


	/**
	 * Ensure that the element is ready to be animated by the animation pane.
	 */
	private void ensureReadyToAnimate(DrawableGraphElement element) {
		if (_readyToAnimate != null && _readyToAnimate != element) {
			_animationPane.stopAnimation();
			_animationPane.startAnimation(element);
		}
	}

} // end class GraphAnimation
