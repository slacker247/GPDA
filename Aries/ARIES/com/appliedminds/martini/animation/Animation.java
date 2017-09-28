/**
 * Copyright (C) 2001-@year@ by University of Maryland, College Park,
 * MD 20742, USA All rights reserved.
 */
package com.appliedminds.martini.animation;

import javax.swing.event.EventListenerList;


/**
 * <b>Animation</b> and its subclasses provide the central programming
 * interface to the animation system. Most animations consist of a
 * source property, a destination property and a AnimationTarget
 * object. The animation interpolates between its source and
 * destination property over time, applying the intermediate values to
 * its target. For a concrete look at this behavior see
 * ColorAnimation.
 *
 * <p> Each Animation has a Alpha object that it uses to determine
 * when it should start, when it should finish, and how it should
 * interpolate between its source and destination values.
 *
 * <p>When creating most animations three decisions need to be made.
 * <ol>
 * <li>What object should this animation apply to? This determines the AnimationTarget of the animation.
 * <li>What property on the target do i want to change. This will determine the Animation subclass
 * that you use, and its source and destination values. (if the property is a color then use ColorAnimation)
 * <li>How should this property be set over time, this determines the parameters used
 * to set up the animations Alpha.
 * </ol>
 *
 * <p> Once an animation has been constructed the method
 * <code>play</code> must be called so that the animation gets
 * scheduled with the AnimationScheduler. Once the scheduler
 * determines (by using the animations Alpha and NextFrameCondition)
 * that the next frame of the animation should be animated it calls
 * <code>animateFrameForTime</code> on the animation. This framework
 * method calls the concrete Animation object's <code>animate</code>
 * method, which should performs all animation activities and return
 * an AnimationFrame. Then finally the
 * <code>animateFrameForTime</code>method schedules its next frame if
 * it wishes to continue animating.
 *
 * <p>This example code demonstrates how to animate the fill color of
 * a rectangle from color red to color yellow.
 *
 * <p>
 * <code>
 * <pre>
 * Rectangle aRect = new Rectangle(0, 0, 100, 100);
 *
 * // Create a new Alpha that will run from the current time for 1.5 seconds.
 * // This alpha will change linearly; 
 * // see the Alpha class to learn how to create
 * // slow in slow out animation effects.
 * Alpha alpha = new Alpha(1, 1500);
 *
 * // Create the ColorAnimation with its source and destination values. Here
 * // we choose to animate the target from color red to color yellow.
 * ColorAnimation aColorAnimation = 
 *   new ColorAnimation(Color.red, Color.yellow);
 *
 * // Set the target of the animation.
 * aColorAnimation.setFillPaintTarget(new FillPaintTarget(aRect));
 *
 * // Set the alpha value for the animation. 
 * // This animation will start immediately,
 * // and run for 1.5 seconds.
 * aColorAnimation.setAlpha(alpha);
 *
 * // Start the animation by registering it with the AnimationScheduler.
 * aColorAnimation.play();
 * </pre>
 * </code>
 *
 * <p>
 * @see Alpha
 * @author Jesse Grosjean
 * @author daepark@apmindsf.com
 */
public abstract class Animation {

	protected static final int ANIMATION_RATE_BY_NEXT_FRAME = 0;
	protected static final int ANIMATION_RATE_BY_ELAPSED_TIME = 1;
	protected static final int ANIMATION_RATE_BY_ELAPSED_FRAMES = 2;

	protected EventListenerList _listenerList = new EventListenerList();

	private Alpha           _alpha;
	private int             _animationRateMode   = ANIMATION_RATE_BY_NEXT_FRAME;
	private long            _animationRateValue              = 0;
	private boolean         _isStopped                       = true;
	private boolean         _hasSeenFirstFrameOfPlaySequence = false;
	private AnimationFrame  _lastAnimationFrame              = null;
	private AnimationTarget _animationTarget                 = null;


	/**
	 * Create an animation with a default animation rate mode.
	 */
	public Animation()  { }


	/**
	 * Construct a new ZAnimation.
	 *
	 * @param alpha the alpha parameter determines the animations start
	 * and finish time, and generates any alpha values needed by the
	 * animation to interpolate between values.
	 */
	public Animation(Alpha alpha) {
		_alpha = alpha;
	}


	/**
	 * Return the alpha object used to determine the animations start
	 * and finish time, and to generate any alpha values that it needs
	 * when interpolating between values.
	 *
	 * @return the animations alpha object, the default value is null.
	 */
	public Alpha getAlpha() {
		return (_alpha);
	}


	/**
	 * Set the alpha object used to determine the animations start and
	 * finish time, and to generate alpha values that it needs when
	 * interpolating between values.
	 *
	 * @param aAlpha the alpha
	 */
	public void setAlpha(Alpha alpha) {
		_alpha = alpha;
	}


	public void setAnimationTarget(AnimationTarget animationTarget) {
		_animationTarget = animationTarget; 
	}

	public AnimationTarget getAnimationTarget() {
		return (_animationTarget);
	}


	/**
	 * Add an AnimationListener that want to be notified of animation
	 * activities.
	 */
	public void addAnimationListener(AnimationListener l) {
		_listenerList.add(AnimationListener.class, l);
	}


	/**
	 * Remove an AnimationListener that want to be notified of animation
	 * activities.
	 */
	public void removeAnimationListener(AnimationListener l) {
		_listenerList.remove(AnimationListener.class, l);
	}


	/**
	 * Make it so that that this animation will be animated on every
	 * successive frame when the AnimationScheduler runs. This is the
	 * default.
	 */
	public void setAnimationRateByNextFrame() {
		_animationRateMode  = ANIMATION_RATE_BY_NEXT_FRAME;
		_animationRateValue = 0;
	}


	/** 
	 * This method makes the animation schedule its frames by elapsed
	 * time. For example this code would make the animation animate one
	 * frame per seconds. 
	 *
	 * <p><code>animation.animationRateByElapsedTime(1000)</code>
	 *
	 * @param elapsedTime the amount of time to wait before animating
	 * the next frame of the animation, specified in milliseconds.
	 */
	public void setAnimationRateByElapsedTime(long elapsedTime) {
		 _animationRateMode = ANIMATION_RATE_BY_ELAPSED_TIME;
		 _animationRateValue = elapsedTime; 
	}


	/**
	 * This method makes the animation schedule its frames by elapsed
	 * frames. For example this code would make the animation animate on
	 * every 5th frame. 
	 *
	 * <p><code>animation.animationRateByElapsedFrames(5)</code>
	 *
	 * @param aFramesCount the number of frames to wait until the next
	 * frame of this animation is animated.
	 */
	public void setAnimationRateByElapsedFrames(long framesCount) {
		_animationRateMode  = ANIMATION_RATE_BY_ELAPSED_FRAMES;
		_animationRateValue = framesCount;
	}


	/**
	 * Schedule this animation with the AnimationScheduler. Once an
	 * animation is constructed it is necessary to call this method to
	 * so that the animation is scheduled.
	 */
	public void play() {
		if (isStopped()) {
			_isStopped = false;
			_hasSeenFirstFrameOfPlaySequence = false;
			scheduleNextFrame();
		}
	}


	/**
	 * Temporarily or permanently stop the animation. This will stop any
	 * animation frames that have been scheduled with the
	 * AnimationScheduler from playing. The animation can be restarted
	 * with <code>play</code> method.
	 */
	public void stop() {
		if (!isStopped()) {
			_isStopped = true;
			fireAnimationStopped();
		}
	}


	/**
	 * Return true if the animation has been stopped. It can be
	 * restarted with the <code>play</code> method.
	 */
	public boolean isStopped() {
		return (_isStopped);
	}

	
	/**
	 * Frame work method for retrieving a frame of this animation at the
	 * specified time.  
	 */
	public abstract AnimationFrame animate(long time);


	/**
	 * Get the last AnimationFrame of this animation.
	 */
	public AnimationFrame getAnimationFrame() { 
		return (_lastAnimationFrame);
	}

	
	/**
	 * Schedule the next frame of the animation. This is normaly called
	 * as the last statement in <code>animateFrameForTime</code>. It
	 * creates a new NextFrameCondition and schedules this animation
	 * with that condition on the AnimationScheduler.
	 *
	 * @see #scheduleNextFrame(Animation, NextFrameCondition)
	 */
	protected void scheduleNextFrame() {
		NextFrameCondition condition = 
			createNextFrameCondition(_alpha, 
															 _animationRateMode,
															 _animationRateValue);
		scheduleNextFrame(this, condition);
	}


	/**
	 * Schedule the next frame of the animation with the AnimationScheduler.
	 */
	protected static void scheduleNextFrame(Animation animation,
																					NextFrameCondition condition) 
	{
		AnimationScheduler.getInstance().scheduleAnimation(animation, condition);
	}
	

	/**
	 * Create a NextFrameCondition given the alpha value and
	 * the animation mode.
	 */
	protected static NextFrameCondition createNextFrameCondition
		(Alpha alpha, 
		 int animationRateMode, 
		 long animationRateValue)
	{
		NextFrameCondition condition = null;
		
		switch (animationRateMode) {
		case ANIMATION_RATE_BY_ELAPSED_TIME:
			condition = new NextFrameOnElapsedTime(alpha, animationRateValue);
			break;
		case ANIMATION_RATE_BY_ELAPSED_FRAMES:
			condition = new NextFrameOnElapsedFrames(alpha, animationRateValue);
			break;
		default:
			condition = new NextFrameOnElapsedTime(alpha, 0);
			break;
		}

		return (condition);
	}


	/**
	 * Animate one frame of this animation at the specified time, and
	 * schedule a new frame to be animated if the alpha object that is
	 * scheduling this animation has not yet finished. This method is
	 * called by the AnimationScheduler.
	 *
	 * <p> Animation subclasses must implement the <code>animate</code>
	 * method and return an AnimationFrame. This AnimatinFrame will be
	 * set to be the last AnimationFrame created by this animation.
	 */
	protected final void animateFrameForTime(long time) {
		if (!_hasSeenFirstFrameOfPlaySequence) {
			fireAnimationStarted();
			_hasSeenFirstFrameOfPlaySequence = true;
		}
		
		if (_alpha != null && _alpha.isFinished(time)) {
			stop();
			return; // don't schedule any more frames.
		}
		
		scheduleNextFrame();

		_lastAnimationFrame = animate(time);
		fireNewAnimationFrame(_lastAnimationFrame);
	}


	/**
	 * Notify AnimationListeners
	 */
	protected void fireAnimationStopped() {
		// Guaranteed to return a non-null array
		Object[] listeners = _listenerList.getListenerList();
		// Process the listeners last to first, notifying
		// those that are interested in this event
		for (int i = listeners.length-2; i>=0; i-=2) {
			if (listeners[i]==AnimationListener.class) {
				((AnimationListener)listeners[i+1]).animationStopped(this);
			}
		}
	}
	
	
	/**
	 * Notify AnimationListeners
	 */
	protected void fireNewAnimationFrame(AnimationFrame animationFrame) {
		// Guaranteed to return a non-null array
		Object[] listeners = _listenerList.getListenerList();
		// Process the listeners last to first, notifying
		// those that are interested in this event
		for (int i = listeners.length-2; i>=0; i-=2) {
			if (listeners[i]==AnimationListener.class) {
				if (animationFrame != null) {
					((AnimationListener)listeners[i+1]).newAnimationFrame(animationFrame);
				}
			}
		}
	}


	/**
	 * Notify AnimationListeners
	 */
	protected void fireAnimationStarted() {
		// Guaranteed to return a non-null array
		Object[] listeners = _listenerList.getListenerList();
		// Process the listeners last to first, notifying
		// those that are interested in this event
		for (int i = listeners.length-2; i>=0; i-=2) {
			if (listeners[i]==AnimationListener.class) {
				((AnimationListener)listeners[i+1]).animationStarted(this);
			}
		}
	}

}  // end clas Animation
