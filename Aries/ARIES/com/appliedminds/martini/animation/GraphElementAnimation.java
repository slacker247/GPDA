package com.appliedminds.martini.animation;

import com.appliedminds.martini.DrawableGraphElement;


/**
 * <b>GraphElementAnimation</b> is a composite animation coupled with a 
 * <code>com.appliedminds.martini.DrawableGraphElement</code>.
 *
 * @author daepark@apmindsf.com
 */
public class GraphElementAnimation extends CompositeAnimation {

	private DrawableGraphElement _element;


	/**
	 * Associate an animation to a graph element.
	 *
	 * @param element the graph element this animation is associated with.
	 */
	public GraphElementAnimation(DrawableGraphElement element) {
		super();
		_element = element;
	}


	/**
	 * Get the graph element associated with this animation.
	 */
	public DrawableGraphElement getDrawableGraphElement() {
		return (_element);
	}

} // end class GraphElementAnimation
