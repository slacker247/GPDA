package com.appliedminds.martini;

import java.awt.Graphics2D;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.Properties;
import java.util.ArrayList;
import java.awt.event.MouseEvent;
import java.util.Iterator;


/**
 * An edge in a DrawableGraph.  The main function of a DrawableEdge is
 * to retain any properties of the original graph while offering an
 * ability to draw itself.
 *
 * <p>An edge has direction: the run from the tail to the head.
 *
 * <p><b>Except for hit testing</b>, all coordinate values and
 * dimensions here are in the world coordinate space.
 *
 * <P>A concrete GraphUI instance must back the DrawableGraph in order
 * for this object to function.  The GraphUI is responsible for
 * configuring hit testing, determining bounds, and drawing.
 *
 * <p>The bounds of a Node are kept in a DrawableGraphContext object.
 * That context object is passed in as needed.
 *
 *
 * @author daepark@apmindsf.com
 * @author will@apmindsf.com
 */
public class DrawableEdge extends DrawableGraphElement {

  public static final String PROPERTY_FAUX = "faux";

  private DrawableGraph _graph;
  private DrawableNode _head;
  private DrawableNode _tail;


  /**
   * Create a new DrawableEdge.
   *
   * @param g the graph that this edge is a part of.
   * @param head a DrawableNode connected to this edge. It is the head node
   * if the graph is directed.
   * @param tail a DrawableNode connected to this edge. It is the tail node
   * if the graph is directed.
   */
  DrawableEdge(DrawableGraph g, DrawableNode head, DrawableNode tail)
  {
    _graph = g;
    _head = head;
    _tail = tail;
  }


  // Framework method.
  public DrawableGraph getGraph() {
    return(_graph);
  }


  /**
   * Get the node attached to the head of this edge.
   *
   * @return the head node.
   */
  public DrawableNode getHead() {
    return _head;
  }


  /**
   * Get the node attached to the tail of this edge.
   *
   * @return the tail node.
   */
  public DrawableNode getTail() {
    return _tail;
  }


  /**
   * Set the head node to the given node.
   *
   * @param node the new head node.
   */
  public void setHead(DrawableNode node) {
    _head = node;
  }


  /**
   * Set the tail node to the given node.
   *
   * @param node the new tail node.
   */
  public void setTail(DrawableNode node) {
    _tail = node;
  }


  /**
   * Draw the edge.  This is accomplished with the help of some
   * GraphUI implementation as defined in the DrawableGraph.
   *
   * @param g a Graphics context object.
   * @param graphUI the GraphUI object to use to draw this edge.
   * @param newBuffer a rendering hint passed on to the UI indicating
   * that the graphics context is from a freshly created image buffer.
   * @param erase a rendering hint passed on to the UI indicating that
   * the UI should first try erasing the previously drawn self first
   * before drawing.
   */
  public void draw(Graphics2D g,
                   Viewport vp,
                   HitTesterMap hit,
                   DrawableGraphContext ctx,
                   boolean newBuffer,
                   boolean erase)
  {
    if (!ctx.isValid(this)) {
      //      throw(new MartiniError("Edge marked invalid!  Now what?"));
      ctx.setNeedsRepaint(this, true);
    }
    _graph.getGraphUI().draw(g, this, vp, hit, ctx, newBuffer, erase);
    clearChanges();
    ctx.setNeedsRepaint(this, false);
  }

} // end class DrawableEdge
