package com.appliedminds.martini.animation;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Collections;
import java.util.List;
import java.util.ArrayList;
import java.awt.Graphics2D;
import java.awt.Paint;
import java.awt.Stroke;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Rectangle2D;


/**
 * <b>AnimatinFrame</b> represents one frame during the course of an
 * animation.  An AnimationFrame is composed of Drawable objects and
 * their corresponding Drawable.Props properties. An AnimationFrame
 * knows how to render it self onto a <code>java.awt.Graphics2D</code>
 * context when required. Actually, it calls each individual Drawable
 * object and have them render themselves onto the graphics context.
 *
 * @author daepark@apmindsf.com
 */
public class AnimationFrame {

	private HashMap _drawablePropsMap;
	private Animation _animation;


	/**
	 * Initialize.
	 *
	 * @param animation the Animation this frame belongs to.
	 */
	public AnimationFrame(Animation animation) {
		_drawablePropsMap = new HashMap();
		_animation = animation;
	}

	public Animation getAnimation() {
		return (_animation);
	}

	public void clear() {
		_drawablePropsMap.clear();
	}


	public void addDrawable(Drawable drawable, Drawable.Props props) {
		_drawablePropsMap.put(drawable, props);
	}
	
	public Drawable.Props getDrawableProps(Drawable drawable) {
		return ((Drawable.Props)_drawablePropsMap.get(drawable));
	}


	private Rectangle2D getTransformedBounds(Drawable drawable) {
		Rectangle2D bounds = drawable.getBounds();
		Drawable.Props props = getDrawableProps(drawable);
		if (props != null) {
			Transform transform = props.getTransform();
			if (transform != null) {
				double[] matrix = new double[6];
				transform.getMatrix(matrix);
				AffineTransform af = new AffineTransform(matrix);
				Shape transformedShape = af.createTransformedShape(bounds);
				bounds = transformedShape.getBounds2D();
			}
		}
		return (bounds);
	}

	public Rectangle2D getBounds() {
		Iterator itr = getDrawables();

		Rectangle2D bounds = null;
		Drawable drawable = null;
		if (itr.hasNext()) {
			drawable = (Drawable)itr.next();
			bounds = getTransformedBounds(drawable);
		}
		
		while (itr.hasNext()) {
			drawable = (Drawable)itr.next();
			bounds.add(getTransformedBounds(drawable));
		}
		
		return (bounds);
	}
	
	protected Iterator getDrawables() {
		return (_drawablePropsMap.keySet().iterator());
	}


	public void draw(Graphics2D g) {
		List drawables = new ArrayList(_drawablePropsMap.keySet());

		//
		// Sort the drawables so that the drawables are drawn in order.
		//
		Collections.sort(drawables);

		Iterator itr = drawables.iterator();
		while (itr.hasNext()) {
			Drawable drawable = (Drawable)itr.next();
			Drawable.Props dp = (Drawable.Props)_drawablePropsMap.get(drawable);
			drawable.draw(g, dp);
		}
	}


	public String debug() {
		StringBuffer buf = new StringBuffer();
		Iterator itr = getDrawables();
		while (itr.hasNext()) {
			Drawable drawable = (Drawable)itr.next();
			Drawable.Props dp = (Drawable.Props)_drawablePropsMap.get(drawable);
			buf.append(drawable).append("     ").append(dp != null ? dp.debug() : "null").append("\n");
		}

		return (buf.toString());
	}

} // end interface AnimationFrame

