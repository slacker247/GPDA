package com.appliedminds.martini.animation;


/**
 * <b>AnimationTarget</b> is the Drawable object and its associated
 * drawing properties. Each animation applies to an AnimationTarget
 * object. That is, this is the object whose property will change
 * during the course of an animation.
 *
 * @author daepark@apmindsf.com
 */
public abstract class AnimationTarget {

  private Drawable _drawable;

  public AnimationTarget(Drawable drawable) {
    _drawable = drawable;
  }

  /**
   * Set the drawable object being animated.
   */
  public void setTarget(Drawable target) {
    _drawable = target;
  }

  /**
   * Get the drawable object being animated.
   */
  public Drawable getTarget() {
    return (_drawable);
  }


  public abstract void setTargetProp(Object property);

  public abstract Object getTargetProp();

  public abstract void addToAnimationFrame(AnimationFrame animationFrame);

} // end class AnimationTarget
