package com.appliedminds.martini.animation;

import java.util.Iterator;

import java.awt.Stroke;
import java.awt.Paint;


/**
 * <b>CompositeAnimationFrame</b> is a collection of one or more
 * AnimationFrames. Currently, only addition is supported.
 * 
 * @author daepark@apmindsf.com
 */
public class CompositeAnimationFrame extends AnimationFrame {

	/**
	 * Initialize.
	 *
	 * @param animation the Animation this frame belongs to.
	 */
	public CompositeAnimationFrame(Animation animation) {
		super(animation);
	}


	/**
	 * Add an animation frame to the collection.
	 *
	 * @param animationFrame the animation frame to add to the collection.
	 */ 
	public void addAnimationFrame(AnimationFrame animationFrame) {
		Iterator itr = animationFrame.getDrawables();
		while (itr.hasNext()) {
			Drawable drawable = (Drawable)itr.next();
			Drawable.Props props = getDrawableProps(drawable);

			if (props == null) {
				props = new Drawable.Props();
				addDrawable(drawable, props);
			}


			mergeDrawableProps(props, animationFrame.getDrawableProps(drawable));
		}
	}


	/**
	 * Merge the two Drawable.Props together.
	 *
	 * @param meregedProps this will contain the merged outcome.
	 * @param propsToMerge this contains the props to merge.
	 */
	private void mergeDrawableProps(Drawable.Props mergedProps,
																	Drawable.Props propsToMerge)
	{
		Stroke newStroke = propsToMerge.getStroke();
		Stroke oldStroke = mergedProps.getStroke();
		
		Paint newPenPaint = propsToMerge.getPenPaint();
		Paint oldPenPaint = mergedProps.getPenPaint();
		
		Paint newFillPaint = propsToMerge.getFillPaint();
		Paint oldFillPaint = mergedProps.getFillPaint();

		Paint newFontPaint = propsToMerge.getFontPaint();
		Paint oldFontPaint = mergedProps.getFontPaint();

		Transform newTransform = propsToMerge.getTransform();
		Transform oldTransform = mergedProps.getTransform();

		if (newStroke != null) {
			mergedProps.setStroke(newStroke);
		}
		
		if (newPenPaint != null) {
			mergedProps.setPenPaint(newPenPaint);
		}
		
		if (newFillPaint != null) {
			mergedProps.setFillPaint(newFillPaint);
		}

		if (newFontPaint != null) {
			mergedProps.setFontPaint(newFontPaint);
		}

		if (newTransform != null) {
			mergedProps.setTransform(newTransform);
		}

	}

} // end interface AnimationFrame

