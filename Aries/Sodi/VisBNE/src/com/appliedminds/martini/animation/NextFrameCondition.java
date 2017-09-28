/**
 * Copyright (C) 2001-@year@ by University of Maryland, College Park,
 * MD 20742, USA All rights reserved.
 */
package com.appliedminds.martini.animation;


/**
 * <b>NextFrameCondition</b> is used by Animations to communicate to
 * the AnimationScheduler when they want their next frame to be
 * animated. Only when this condition is met, as determined by
 * <code>isReadyToAnimate</code>, will the animation scheduler animate
 * the next frame of the animation.  
 *
 * <p> This class uses a alpha object to determine when the next frame
 * should be animated. Its concrete subclasses
 * NextFrameOnElapsedFrames and NextFrameOnElapsedTime use the alpha
 * object together with the current frame number or current time to
 * determine when the next frame should be animated.  
 *
 * <p> For normal use of the animation system you will not need to use
 * NextFrameCondition's directly. Animation will do everything behind
 * the scenes for you why you press <code>Animation.play</code>.
 *
 * @author Jesse Grosjean
 * @author daepark@apmindsf.com
 */
public abstract class NextFrameCondition implements Comparable {

	private Alpha fAlpha;

	/**
	 * Create a new NextFrameCondition.
	 *
	 * @param aAlpha the alpha that will be used to determine if the
	 * next frame from that associated animation should be animated yet.
	 */
	public NextFrameCondition(Alpha aAlpha) {
		super();
		fAlpha = aAlpha;
	}

	/**
	 * Return true if this condition has been met, and the next frame of
	 * its associated animation is ready to animate. This implementation
	 * asks the alpha object if it is started for the current time
	 * provided by the AnimationScheduler.
	 */
	public boolean isReadyToAnimate() {
		if (fAlpha != null) {
			return fAlpha.isStarted(AnimationScheduler.getInstance().getCurrentTime());
		}
		return true;
	}

	/**
	 * This is used to compare two frame conditions. The frame condition
	 * whos associated animation should be animated first is the greater
	 * of the two. The Alpha associated with each frame condition is
	 * used to determine this.
	 */
	public int compareTo(Object o) {
		long aTime = AnimationScheduler.getInstance().getCurrentTime();
		NextFrameCondition aFrameCondition = (NextFrameCondition) o;

		Alpha thisAlpha = fAlpha;
		Alpha thatAlpha = aFrameCondition.fAlpha;

		// Conditions with a null alpha always come first.
		if (thisAlpha == null && thatAlpha != null) return 1;
		if (thisAlpha != null && thatAlpha == null) return -1;
		if (thisAlpha == null && thatAlpha == null) return 0;

		boolean isThisStarted = thisAlpha.isStarted(aTime);
		boolean isThatStarted = thatAlpha.isStarted(aTime);

		if (isThisStarted && !isThatStarted) {          // if this one is started and that one isn't
			return 1;                                   // then this is greater then that.

		} else if (!isThisStarted && isThatStarted) {   // if this one is not started and that one is
			return -1;                                  // started then that one is greater the this.

		} else if (!isThatStarted && !isThatStarted) {  // if neither has been started then compare by alpha start time.
			long thisStart = fAlpha.getTriggerTime() + fAlpha.getPhaseDelayDuration();
			long thatStart = aFrameCondition.fAlpha.getTriggerTime() + aFrameCondition.fAlpha.getPhaseDelayDuration();

			if (thisStart < thatStart) {                // if this start time is less then that start time then this
				return 1;                               // is greater.

			} else if (thisStart > thatStart) {
				return -1;                              // if this start time is greater then that start time then
				// that is greater.
			}
		}

		// they are equal.
		return 0;
	}


	/**
	 * avoids instanceof operations
	 */
	public abstract boolean isTimeCondition();


	/**
	 * avoids instanceof operations
	 */
	public abstract boolean isFrameCondition();

} // end class NextFrameCondition
