package com.appliedminds.martini;

import java.awt.geom.Rectangle2D;
import java.awt.Graphics2D;

/**
 * A GraphLayout implementation is responsible for setting the bounds
 * of a DrawableGraphs nodes and edges.  This is the object that calls
 * the setBounds() method in DrawableEdge and DrawableNode.
 *
 * <P>A GraphLayout is supposed to use the getPreferredSize() method
 * in DrawableNode as a hint for how large the node wants to be,
 * however, the GraphLayout us ultimately responsible for making the
 * decision about the size of a node.  If the GraphLayout chooses to
 * ignore a nodes desired size, then the node may not render
 * properly/optimally on the screen.
 *
 * <p>A GraphLayout's behavior may depend on the GraphUI since the
 * GraphUI is used to determine the sizes of nodes and edges.
 * 
 *
 * @author will@apmindsf.com
 * @author daepark@apmindsf.com
 */
public interface GraphLayout {

  /**
   * This method takes a DrawableGraph and lays it out according to the
   * constraints of the bounds.
   *
   * @param g a Graphics2D context
   * @param graph the DrawableGraph we want to lay out.
   * @param scale the scalefactor between the world coordinates and the
   * viewport coordinates.
   * @param ctx a DrawableGraphContext object for keeping track of bounds.
   */
  void performLayout(Graphics2D g, 
                     DrawableGraph graph, 
                     double scale,
                     DrawableGraphContext ctx);

} // end interface GraphLayout
