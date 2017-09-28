package com.appliedminds.martini.animation;

import java.awt.Paint;


/**
 * <b>PenPaintTarget</b> is an AnimationTarget who's pen paint or the
 * paint to draw the outline of the target object is changed during
 * the course of an animation.
 *
 * @author daepark@apmindsf.com
 */
public class PenPaintTarget extends AnimationTarget {
	
	private Paint     _paint;
	

	public PenPaintTarget(Drawable drawable) {
		super(drawable);
	}
	

	public void setTargetProp(Object property) {
		_paint = (Paint)property;
	}


	public Object getTargetProp() {
		return (_paint);
	}


	public void addToAnimationFrame(AnimationFrame animationFrame) { 
		Drawable drawable = getTarget();
		Drawable.Props props = animationFrame.getDrawableProps(drawable);
		if (props == null) {
			props = new Drawable.Props();
			animationFrame.addDrawable(drawable, props);
		}
		
		props.setPenPaint(_paint);
	}

} // end class PenPaintTarget
