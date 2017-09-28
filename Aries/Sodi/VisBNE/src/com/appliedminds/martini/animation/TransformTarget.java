package com.appliedminds.martini.animation;


/**
 * <b>TransformTarget</b> is an AnimationTarget who's transform property is
 * changed during the course of an animation.
 *
 * @author daepark@apmindsf.com
 */
public class TransformTarget extends AnimationTarget {

	private Transform _transform = null;

	public TransformTarget(Drawable drawable) {
		super(drawable);
	}
	

	public void setTargetProp(Object property) {
		_transform = (Transform)property;
	}


	public Object getTargetProp() {
		return (_transform);
	}


	public void addToAnimationFrame(AnimationFrame animationFrame) { 
		Drawable drawable = getTarget();
		Drawable.Props props = animationFrame.getDrawableProps(drawable);
		if (props == null) {
			props = new Drawable.Props();
			animationFrame.addDrawable(drawable, props);
		}
		
		props.setTransform(_transform);
	}

} // end class TransformTarget
