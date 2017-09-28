package com.appliedminds.hmv;

import java.util.EventListener;


/**
 * <b>GraphChangeListeer</b> is an interface to a handler for graph
 * change events.
 *
 * @author daepark@apmindsf.com
 */
public interface GraphChangeListener extends EventListener {

  /**
   * Signifies that there has been a change to a graph (i.e., new
   * node, new edge, node moved, etc.
   */
  public void graphChanged();

} // end interface "GraphChangeListener"

