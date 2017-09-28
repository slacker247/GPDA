/**
 * Copyright (C) 2001-@year@ by University of Maryland, College Park,
 * MD 20742, USA All rights reserved.
 */
package com.appliedminds.martini.animation;


/**
 * <b>NextFrameOnElapsedFrames</b> allows an animation to schedule its
 * animation rate by frame count instead of by time. That condition is
 * how an animation that wants to be animated on every third frame
 * would communicate that request to the AnimationScheduler.
 *
 * <p> This class will is used internally to support
 * <code>Animation.animationRateByElapsedFrames</code>, you should not
 * normally need to use it directly.
 *
 * @author Jesse Grosjean
 * @author daepark@apmindsf.com
 */
public class NextFrameOnElapsedFrames extends NextFrameCondition {

	private long fTriggerFrame;

	public NextFrameOnElapsedFrames(Alpha aAlpha, long aFrameCount) {
		super(aAlpha);
		fTriggerFrame = 
			aFrameCount + AnimationScheduler.getInstance().getCurrentFrame();
	}

	public boolean isReadyToAnimate() {
		return super.isReadyToAnimate() &&
			AnimationScheduler.getInstance().getCurrentFrame() >= fTriggerFrame;
	}

	public int compareTo(Object o) {
		int result = super.compareTo(o);
		if (result == 0) {
			NextFrameOnElapsedFrames condition = (NextFrameOnElapsedFrames) o;

			if (fTriggerFrame > condition.fTriggerFrame) {
				result = -1;        // that frame needs to be scheduled first
				// so we get put in the queue after it.

			} else if (fTriggerFrame < condition.fTriggerFrame) {
				result = 1;         // this frame needs to be schedule first
				// so put it earlier in the queue.
			}
		}

		return result;
	}

	public boolean isTimeCondition() { return (true); }

	public boolean isFrameCondition() { return (false); }

} // end class NextFrameOnElapsedFrames
