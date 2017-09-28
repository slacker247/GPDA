package com.appliedminds.hmv;

import com.appliedminds.core.config.ConfigBean;
import com.appliedminds.martini.DrawableGraphElement;


/**
 * The basis for all ConfigBean objects that will contain properties
 * for a DrawableGraphElement object.
 *
 * @author daepark@apmindsf.com
 */
public abstract class GraphElementProperties implements ConfigBean {

  private DrawableGraphElement _element;


  /**
   * Default constructor.
   */
  public GraphElementProperties(DrawableGraphElement element) {
    _element = element;
  }


  /**
   * Get the graph element which this property is based on.
   */
  protected DrawableGraphElement getGraphElement() {
    return (_element);
  }


  /**
   * Framework method for replicating itself.
   */
  protected abstract GraphElementProperties replicate();


  /**
   * Framework method for applying this property to the element.
   */
  protected abstract void applyConfiguration();

} // end class "GraphElementProperties"
