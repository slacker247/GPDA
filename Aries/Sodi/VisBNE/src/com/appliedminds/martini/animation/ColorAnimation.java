/**
 * Copyright (C) 2001-@year@ by University of Maryland, College Park,
 * MD 20742, USA All rights reserved.
 */
package com.appliedminds.martini.animation;

import java.awt.Color;

/**
 * <b>ColorAnimation</b> animates an object from a source color to a
 * destination color over time. An Alpha class is used to specify the
 * start time, duration, and acceleration time for this animation.
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
public class ColorAnimation extends Animation {

	private Color fSource;
	private Color fDestination;
	private AnimationFrame _myAnimationFrame;


	/**
	 * Construct a new ColorAnimation.
	 *
	 * @param aSource      the source color that the animation will start at.
	 * @param aDestination the destination color that the animation will end at.
	 */
	public ColorAnimation(Color aSource, Color aDestination) {
		super();
		fSource = aSource;
		fDestination = aDestination;
	}

	/**
	 * Return the source color, the color that the animation will start at.
	 */
	public Color getSourceColor() {
		return fSource;
	}

	/**
	 * Set the source color, the color that the animation will start at.
	 */
	public void setSourceColor(Color aSource) {
		fSource = aSource;
	}

	/**
	 * Return the destination color, the color that the animation will end at.
	 */
	public Color getDestinationColor() {
		return fDestination;
	}

	/**
	 * Set the destination color, the color that the animation will end at.
	 */
	public void setDestinationColor(Color aDestination) {
		fDestination = aDestination;
	}

	/**
	 * Animate one frame of this animation for the given time. The time
	 * parameter is used to generate an alpha value, which is then used
	 * to create a color interpolated between the source and destination
	 * colors. The color is then applied to both the pen paint and fill
	 * paint targets. If the animation should only apply to one of these
	 * targets the other can safely be left as null.
	 */
	public AnimationFrame animate(long aTime) {
		if (_myAnimationFrame == null) {
			_myAnimationFrame = new AnimationFrame(this);
		}
		else {
			_myAnimationFrame.clear();
		}

		AnimationTarget target = getAnimationTarget();
		if (target != null) {
			float ratio = getAlpha().value(aTime);
			float red = (float) (fSource.getRed() + (ratio * (fDestination.getRed() - fSource.getRed())));
			float green = (float) (fSource.getGreen() + (ratio * (fDestination.getGreen() - fSource.getGreen())));
			float blue = (float) (fSource.getBlue() + (ratio * (fDestination.getBlue() - fSource.getBlue())));
			float alpha = (float) (fSource.getAlpha() + (ratio * (fDestination.getAlpha() - fSource.getAlpha())));
			
			Color interpolation = new Color(red/255, green/255, blue/255, alpha/255);
			target.setTargetProp(interpolation);
			
			target.addToAnimationFrame(_myAnimationFrame);
		}
		
		return (_myAnimationFrame);
	}

} // end class ColorAnimation
