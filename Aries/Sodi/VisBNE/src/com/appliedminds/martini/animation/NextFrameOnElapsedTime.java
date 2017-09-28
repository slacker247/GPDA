/**
 * Copyright (C) 2001-@year@ by University of Maryland, College Park,
 * MD 20742, USA All rights reserved.
 */
package com.appliedminds.martini.animation;


/**
 * <b>NextFrameOnElapsedTime</b> allows an animation to schedule its
 * frame rate based on time.  This condition is how an animation that
 * wants to be animated every 500 milliseconds would communicate that
 * request to the AnimationScheduler.  
 *
 * <p> This class is used internally to support
 * <code>Animation.animationRateByElapsedTime</code>, it should not
 * normally be used directly.
 *
 * @author Jesse Grosjean
 * @author daepark@apmindsf.com
 */
public class NextFrameOnElapsedTime extends NextFrameCondition {

	private long fTriggerTime;

	public NextFrameOnElapsedTime(Alpha aAlpha, long aTimeDurration) {
		super(aAlpha);
		fTriggerTime = 
			aTimeDurration + AnimationScheduler.getInstance().getCurrentTime();
	}

	public boolean isReadyToAnimate() {
		return super.isReadyToAnimate() &&
			AnimationScheduler.getInstance().getCurrentTime() >= fTriggerTime;
	}

	public int compareTo(Object o) {
		int result = super.compareTo(o);
		if (result == 0) {
			NextFrameOnElapsedTime condition = (NextFrameOnElapsedTime) o;

			if (fTriggerTime > condition.fTriggerTime) {
				result = 1;
			} else if (fTriggerTime < condition.fTriggerTime) {
				result = -1;
			}
		}

		return result;
	}


	public boolean isTimeCondition() { return (true); }

	public boolean isFrameCondition() { return (false); }

} // end class NextFrameOnElapsedTime
