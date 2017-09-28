package com.appliedminds.martini.animation;

import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Collections;
import java.util.List;
import java.util.ArrayList;
import javax.swing.event.EventListenerList;


/**
 * <b>CompositeAnimation</b> is a collection of one or more
 * animations.  When a composite animation is played it will have the
 * effect of scheduling all the animations it knows about with the
 * scheduler. Also, when stopped, it will stop all animations in its
 * collection with the scheduler.
 *
 * <p>Those who listen to the animation events of a CompositeAnimation
 * via the AnimationListener interface, will be notified of starts and
 * stops of the collection as a whole. In other words, a composite
 * animation is considered started when the first animation in its
 * collection is started. Similarily, it is considered stopped when
 * all the animations in its collection are stopped. Also, when an
 * animation frame is avialable from an animation in the collection,
 * the composite animation will create a CompositeAnimationFrame from
 * the newest animation frames from all the animations in its
 * collection and notify its listener of a new animation frame on the
 * composite animation frame.
 *
 * <p>
 * <code>
 * <pre>
 * ColorAnimation a1 = new ColorAnimation(...);
 * StrokeAnimation a2 = new StrokeAnimation(...);
 * CompositeAnimation animation = new CompositeAnimation();
 * animation.add("color highlight animation", a1);
 * animation.add("thinning line animation", a2);
 * animation.play();
 * </pre>
 * </code>
 *
 * @see CompositeAnimationFrame
 * @author daepark@apmindsf.com
 */
public class CompositeAnimation 
	extends Animation 
	implements AnimationListener 
{
	protected EventListenerList eventListeners = new EventListenerList();

	private Map _animationMap = Collections.synchronizedMap(new HashMap());
	private CompositeAnimationFrame _compositeAnimationFrame = null;
	private boolean _startNotified = false;
	private boolean _stopNotified = false;

	private boolean _compositePlaying = true;

	/**
	 * Initialize.
	 */
	public CompositeAnimation() {
		super();
	}

	
	/**
	 * Add an animation to the collection. If this composite animation is
	 * being played, then the animation being added will be immediately
	 * played as well.
	 *
	 * @param animationKey a unique key to the animation being added.
	 * @param animation the animation to add.
	 */
	public void addAnimation(Object animationKey, Animation animation) {
		if (animation != null) {
			synchronized (_animationMap) {
				_animationMap.put(animationKey, animation);
			}
			animation.addAnimationListener(this);

			if (_compositePlaying) {
				// immediately play this animation as well if this is playing
				animation.play();
			}
		}
	}


	/**
	 * Remove an animation from the collection. If the animation
	 * is being played, stop the animation.
	 *
	 * @param animationKey a unique key to the animation being added.
	 */
	public void removeAnimation(Object animationKey) {
		Animation animation = null;
		synchronized (_animationMap) {
			animation = (Animation)_animationMap.remove(animationKey);
		}

		if (animation != null) {
			animation.removeAnimationListener(this);
		}

		if (!animation.isStopped()) {
			animation.stop();
		}
	}

	
	/**
	 * Get an animation from the collection.
	 *
	 * @param animationKey the unique key used to add the animation to
	 * the collection.
	 * @see #addAnimation
	 */
	public Animation getAnimation(Object animationKey) {
		return ((Animation)_animationMap.get(animationKey));
	}


	/**
	 * Get all animations that were added to the collection.
	 */
	public Iterator getAnimations() {
		synchronized(_animationMap) {
			List list = new ArrayList(_animationMap.values());
			return (list.iterator());
		}
	}


	/**
	 * Override Animation.addAnimationListener. Note that a composite
	 * animation is considered stopped if all the animation in the
	 * collection is stopped.
	 */
	public void addAnimationListener(AnimationListener l) {
		eventListeners.add(AnimationListener.class, l);
	}

	
	/**
	 * Override Animation.removeAnimationListener. Note that a composite
	 * animation is considered stopped if all the animation in the
	 * collection is stopped.
	 */
	public void removeAnimationListener(AnimationListener l) {
		eventListeners.remove(AnimationListener.class, l);
	}


	/**
	 * Override to play all animations this knows about
	 */
	public void play() {
		_compositePlaying = true;

		if (isAllStopped()) {
			Iterator itr = getAnimations();
			while (itr.hasNext()) {
				Animation animation = (Animation)itr.next();
				
				if (animation != null) {
					animation.play();
				}			
			}
		}
	}


	/**
	 * Override to stop all animations this knows about
	 */
	public void stop() {
		_compositePlaying = false;

		Iterator itr = getAnimations();
		while (itr.hasNext()) {
			Animation animation = (Animation)itr.next();

			if (animation != null) {
				animation.stop();
			}			
		}
	}

	public boolean isStopped() {
		return (isAllStopped());
	}


	/**
	 * Override to call all animations' animate method
	 * and creating a composite animation frame.
	 */
	public AnimationFrame animate(long time) {
		CompositeAnimationFrame caf = new CompositeAnimationFrame(this);
		
		Iterator itr = getAnimations();
		while (itr.hasNext()) {
			AnimationFrame af = ((Animation)itr.next()).animate(time);
			if (af != null) {
				caf.addAnimationFrame(af);
			}
		}
		return (caf);
	}


	public AnimationFrame getAnimationFrame() { 
		return (_compositeAnimationFrame);
	}


	/////////////////////////////////////
	// begin AnimationListener interface

	public void animationStarted(Animation animation) {
		//		System.err.println("CompositeAnimation.animationStarted");
		if (!_startNotified) {
			fireAnimationStarted();
			_startNotified = true;
			_stopNotified = false;
		}
	}

	public void animationStopped(Animation animation) {
		//		System.err.println("CompositeAnimation.animationStopped");
		if (isAllStopped() && !_stopNotified) {
			fireAnimationStopped();
			_startNotified = false;
			_stopNotified = true;
		}
	}

	public void newAnimationFrame(AnimationFrame animationFrame) {
		//		System.err.println("CompositeAnimation.newAnimationFrame");
		updateCompositeAnimationFrame();
	}

	// end AnimationListener interface
	/////////////////////////////////////


	private boolean isAllStopped() {
		Iterator itr = getAnimations();
		while (itr.hasNext()) {
			Animation animation = (Animation)itr.next();
			if (!animation.isStopped()) {
				return (false);
			}
		}
		return (true);
	}

	/**
	 * Compose our animation frame from all the most recent animation from
	 * all our animation in the collection.
	 */
	private void updateCompositeAnimationFrame() {
		// update composite animation frame from all animations in
		// the collection

		//
		// get a clean composite animation frame
		if (_compositeAnimationFrame == null) {
			_compositeAnimationFrame = new CompositeAnimationFrame(this);
		}
		else {
			_compositeAnimationFrame.clear();
		}
		
		// Foreach animation in collection:
		//   getAnimationFrame()
		//   compositeAnimation.addAnimationFrame(animationFrame);
		Iterator itr = getAnimations();
		while (itr.hasNext()) {
			Animation animation = (Animation)itr.next();
			if (animation != null) {
				AnimationFrame af = animation.getAnimationFrame();
				if (af != null) {
					_compositeAnimationFrame.addAnimationFrame(af);
				}
			}
		}
		

		// notify listeners we have an updated composite animation frame
		fireNewAnimationFrame(_compositeAnimationFrame);
	}


	/**
	 * Notify AnimationListeners
	 */
	protected void fireAnimationStopped() {
		// Guaranteed to return a non-null array
		Object[] listeners = eventListeners.getListenerList();
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
		Object[] listeners = eventListeners.getListenerList();
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
		Object[] listeners = eventListeners.getListenerList();
		// Process the listeners last to first, notifying
		// those that are interested in this event
		for (int i = listeners.length-2; i>=0; i-=2) {
			if (listeners[i]==AnimationListener.class) {
				((AnimationListener)listeners[i+1]).animationStarted(this);
			}
		}
	}

} // end class CompositeAnimation
