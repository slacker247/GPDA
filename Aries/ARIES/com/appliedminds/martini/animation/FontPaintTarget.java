package com.appliedminds.martini.animation;

import java.awt.Paint;


/**
 * <b>FontPaintTarget</b> is an AnimationTarget who's font paint
 * property is changed during the course of an animation.
 *
 * @author daepark@apmindsf.com
 */
public class FontPaintTarget extends AnimationTarget {
	
	private Paint     _paint;
	

	public FontPaintTarget(Drawable drawable) {
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
		
		props.setFontPaint(_paint);
	}

} // end class FontPaintTarget
