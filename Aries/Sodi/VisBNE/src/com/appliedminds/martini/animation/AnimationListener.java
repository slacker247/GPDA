package com.appliedminds.martini.animation;

import java.util.EventListener;


/**
 * <b>AnimationListener</b> is notified of animation events when
 * registered with with an Animation via the
 * <code>addAnimationListener</code> method. The AnimationListener
 * will be notfied when the animation is started, stopped and when a
 * new frame (the next frame) is generated (via the
 * AnimationScheduler).
 *
 * @author daepark@apmindsf.com
 */
public interface AnimationListener extends EventListener {

	/**
	 * Called when an animation is started.
	 *
	 * @param animation the animation being started.
	 */
	public void animationStarted(Animation animation);
	

	/**
	 * Called when an animation is started.
	 *
	 * @param animation the animation being stopped.
	 */
	public void animationStopped(Animation animation);


	/**
	 * Called when a new AnimationFrame is available from an
	 * animation.
	 */
	public void newAnimationFrame(AnimationFrame animationFrame);

} // end interface AnimationListener
