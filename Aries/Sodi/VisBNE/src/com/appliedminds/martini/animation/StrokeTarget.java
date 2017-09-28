package com.appliedminds.martini.animation;

import java.awt.Stroke;


/**
 * <b>StrokeTarget</b> is an AnimationTarget who's stroke property is
 * changed during the course of an animation.
 *
 * @author daepark@apmindsf.com
 */
public class StrokeTarget extends AnimationTarget {

	private Stroke _stroke = null;


	public StrokeTarget(Drawable drawable) {
		super(drawable);
	}
	

	public void setTargetProp(Object property) {
		_stroke = (Stroke)property;
	}


	public Object getTargetProp() {
		return (_stroke);
	}


	public void addToAnimationFrame(AnimationFrame animationFrame) { 
		Drawable drawable = getTarget();
		Drawable.Props props = animationFrame.getDrawableProps(drawable);
		if (props == null) {
			props = new Drawable.Props();
			animationFrame.addDrawable(drawable, props);
		}

		props.setStroke(_stroke);
	}

} // end class StrokeTarget
