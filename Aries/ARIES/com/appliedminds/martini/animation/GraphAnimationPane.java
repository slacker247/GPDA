package com.appliedminds.martini.animation;

import com.appliedminds.martini.DrawableGraphElement;
import java.awt.Graphics2D;
import java.awt.geom.Rectangle2D;


/**
 * <b>GraphAnimationPane</b> is responsible for providing the
 * <code>java.awt.Graphics2D</code> context for rendering a
 * GraphAnimation onto screen. A GraphAnimationPane knows hows to
 * allocate a <code>java.awt.Graphics2D</code> context specific for
 * drawing animation frames. Also, it should render the drawn
 * animation frames as quickly as possible when its
 * <code>paintAnimationImmediately</code> is called.
 *
 * @author daepark@apmindsf.com
 */
public interface GraphAnimationPane {

	/**
	 * This should be called before any calls to the
	 * <code>getAnimationGraphics</code>. This will ready the animation pane
	 * so that all subsequent calls to getAnimationGraphics is in context
	 * of animating the specified graph element(s).
	 *
	 * @param element the graph element we are about to animate.
	 */
	public void startAnimation(DrawableGraphElement element);

	
	/**
	 * This should be called after an animation is finished.
	 */
	public void stopAnimation();


	/**
	 * Allocate an animation graphics context. This should be done as
	 * fast as possible. Note that <code>startAnimation</code> must be
	 * called to get a valid graphics context for an animation.
	 */
	public Graphics2D getAnimationGraphics();


	/**
	 * Render the drawn animation frame as quickly as possible to screen.
	 *
	 * @param bounds the area the GraphAnimationPane should try to paint as
	 * possible. If null, the GraphAnimationPane should try to paint the whole
	 * animation graphics context.
	 */
	public void paintAnimationImmediately(Rectangle2D bounds);

} // end interface GraphAnimationPane
