/**
 * Copyright (C) 2001-@year@ by University of Maryland, College Park,
 * MD 20742, USA All rights reserved.
 */
package com.appliedminds.martini.animation;

import java.awt.BasicStroke;
import java.awt.Stroke;


/**
 * <b>StrokeAnimation</b> animates an object from a source stroke to a
 * destination stroke over time. An Alpha class is used to specify the
 * start time, duration, and acceleration time for this animation.
 * 
 * <p>This code demonstrates how to animate the stroke of a rectangle
 * from width 0 to width 5.
 
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
 * // Create the TransformAnimation with its source 
 * // and destination values. Here
 * // we choose to animate the target from a stroke with 
 * // width 0 to a stroke with width 5.
 * StrokeAnimation aStrokeAnimation = 
 *   new StrokeAnimation(new BasicStroke(0), new BasicStroke(5));
 *
 * // Set the target of the animation.
 * aStrokeAnimation.setStrokeTarget(new StrokeTarget(aRect));
 *
 * // Set the alpha value for the animation. 
 * // This animation will start immediately,
 * // and run for 1.5 seconds.
 * aStrokeAnimation.setAlpha(alpha);
 *
 * // Start the animation by registering it with the AnimationScheduler.
 * aStrokeAnimation.play();
 * </pre>
 * </code>
 * <p>
 * @see Alpha
 * @author Jesse Grosjean
 * @author daepark@apmindsf.com
 */
public class StrokeAnimation extends Animation {

	private BasicStroke fSource;
	private BasicStroke fDestination;
	
	private StrokeTarget _strokeTarget;
	private AnimationFrame _myAnimationFrame;

	/**
	 * Construct a new StrokeAnimation.
	 *
	 * @param aSource      the source stroke that the animation will start at.
	 * @param aDestination the destination stroke that the animation will end at.
	 */
	public StrokeAnimation(BasicStroke aSource, BasicStroke aDestination) {
		super();
		setSourceStroke(aSource);
		setDestinationStroke(aDestination);
	}

	/**
	 * Return the source stroke, the stroke that the animation will start at.
	 */
	public BasicStroke getSourceStroke() {
		return fSource;
	}

	/**
	 * Set the source stroke, the stroke that the animation will start at.
	 */
	public void setSourceStroke(BasicStroke aStroke) {
		fSource = aStroke;
	}

	/**
	 * Return the destination stroke, the stroke that the animation will end at.
	 */
	public BasicStroke getDestinationStroke() {
		return fDestination;
	}

	/**
	 * Set the destination stroke, the stroke that the animation will end at.
	 */
	public void setDestinationStroke(BasicStroke aStroke) {
		fDestination = aStroke;
	}

	/**
	 * Animate one frame of this animation for the given time. The time
	 * paramater is used to generate an alpha value, which is then used
	 * to create a new stroke interpolated between the source and
	 * destination strokes. The stroke target is then set to this
	 * interpolated value.
	 */
	public AnimationFrame animate(long time) {
		if (_myAnimationFrame == null) {
			_myAnimationFrame = new AnimationFrame(this);
		}
		else {
			_myAnimationFrame.clear();
		}

		AnimationTarget target = getAnimationTarget();
		if (target != null) {
			float ratio = getAlpha().value(time);
			float lineWidth = fSource.getLineWidth() + (ratio * (fDestination.getLineWidth() - fSource.getLineWidth()));
			target.setTargetProp(new BasicStroke(lineWidth));

			target.addToAnimationFrame(_myAnimationFrame);
		}

		return (_myAnimationFrame);
	}


} // end class StrokeAnimation
