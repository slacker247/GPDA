package com.appliedminds.martini.io;

import com.appliedminds.martini.*;
import java.awt.geom.Rectangle2D;


/**
 * GMLInput will use a LayoutLoader to load a saved layout.
 *
 * @author daepark@apmindsf.com
 */
public interface LayoutLoader {

  /**
   * Load the overall bounds of the whole graph.
   *
   * @param graph the bounds is for this graph.
   * @param bounds the bounds of the graph.
   */
  public void loadGraphBounds(DrawableGraph graph, Rectangle2D bounds);


  /**
   * Load the bounds of a node.
   *
   * @param node the bounds is for this node.
   * @param bounds the bounds of the node.
   */
  public void loadNodeBounds(DrawableNode node, Rectangle2D bounds);


  /**
   * Load the bounds of an edge.
   *
   * @param edge the bounds is for this edge.
   * @param bounds the bounds of the node.
   */
  public void loadEdgeBounds(DrawableEdge edge, Rectangle2D bounds);


  /**
   * Get the saved bounds of the whole graph.
   */
  public Rectangle2D getGraphBounds();


  /**
   * Get the saved bounds of the node.
   */
  public Rectangle2D getBounds(DrawableNode node);


  /**
   * Get the saved bounds of the edge.
   */
  public Rectangle2D getBounds(DrawableEdge edge);

} // end interface "LayoutLoader"
