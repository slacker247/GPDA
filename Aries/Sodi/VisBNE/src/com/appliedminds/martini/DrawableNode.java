package com.appliedminds.martini;

import java.awt.Graphics2D;
import java.awt.Graphics;
import java.awt.event.MouseEvent;
import java.awt.geom.Rectangle2D;
import java.util.List;
import java.util.ArrayList;
import java.util.Set;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Properties;


/**
 * An node in a DrawableGraph.  The main function of a DrawableNode is
 * to retain any properties of the original graph while offering an
 * ability to draw itself.
 *
 * <p>In contrast to a DrawableEdge, the DrawableNode has some notion
 * of its minimum, maximum, and preferred sizes.  These values are to
 * be used as hints to the layout algorithm.
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
public class DrawableNode extends DrawableGraphElement {

  /** Canonical string for the "collapsed" property */
  public static final String PROPERTY_COLLAPSED = "collapsed";

  /** Canonical values for the collapsed property */
  public static final String COLLAPSED_TARGET = "target";
  public static final String COLLAPSED_INVISIBLE = "invisible";
  public static final String COLLAPSED_VISIBLE = "visible";

  private DrawableGraph _graph;
  private ArrayList _hitTesters;

  private EdgeList _outgoingEdges;
  private EdgeList _incomingEdges;
  private NodeList _headNodes;
  private NodeList _tailNodes;

  /**
   * Create a new DrawableNode in a graph.
   *
   * @param g the DrawableGraph containing this node.
   */
  DrawableNode(DrawableGraph g) {
    _graph = g;
    _outgoingEdges = new EdgeList();
    _incomingEdges = new EdgeList();
    _headNodes = new NodeList();
    _tailNodes = new NodeList();
  }


  /**
   * Generally you will want to avoid this method.  The recommended
   * way to make a node is via the createNode method on DrawableGraph.
   */
  public DrawableNode() { }


  // Framework method
  public DrawableGraph getGraph() {
    return(_graph);
  }


  /**
   * Draw myself by delegating to a GraphUI instance held in the
   * DrawableGraph.
   *
   * @param g a Graphics Context.
   * @param hit the hit test map (modified).
   * @param ctx the drawing context.
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
      Rectangle2D oldbounds = ctx.getBounds(this);
      Size sz = getPreferredSize(g, vp.getScale());
      Rectangle2D newbounds = new Rectangle2D.Double(oldbounds.getX(),
                                                     oldbounds.getY(),
                                                     sz.getWidth(),
                                                     sz.getHeight());
      ctx.setBounds(this, newbounds);
    }
    _graph.getGraphUI().draw(g, this, vp, hit, ctx, newBuffer, erase);
    clearChanges();
    ctx.setNeedsRepaint(this, false);
  }


  /**
   * Get a list of all nodes connected to this node.  Every node that
   * is directly connected to this node along any type of edge is
   * returned.
   *
   * <P>This does not use an efficient algorithm and could be an
   * expensive operation in terms of time.
   *
   * @return a list of <b>unique</b> nodes (DrawableNode objects)
   * connected to this node.
   */
  public NodeIterator getConnectedNodes()
  {
    NodeList connectedNodes = new NodeList();

    NodeIterator itr = getTailNodes();
    while (itr.hasNext()) {
      connectedNodes.add(itr.next());
    }
    itr = getHeadNodes();
    while (itr.hasNext()) {
      connectedNodes.add(itr.next());
    }

    return (connectedNodes.iterator());
  }


  /**
   * Get all outbound edges, that is the edges that consider this node
   * as the tail.
   */
  public EdgeIterator getOutgoingEdges() {
    return (_outgoingEdges.iterator());
  }


  /**
   * Get all inbound edges, that is the edges that consider this node
   * as the head.
   */
  public EdgeIterator getIncomingEdges() {
    return (_incomingEdges.iterator());
  }


  /**
   * Get all the nodes that are connected to this node that
   * regard this node as the tail.
   */
  public NodeIterator getHeadNodes() {
    return (_headNodes.iterator());
  }


  /**
   * Get all the nodes that are connected to this node that
   * regard this node as the head.
   */
  public NodeIterator getTailNodes() {
    return (_tailNodes.iterator());
  }


  /**
   * @return a NodeIterator containing all of the descendants of the node
   */
  public NodeIterator getAllDescendants() {
    NodeList desc = new NodeList();
    getDescendants(this, desc);
    return desc.iterator();
  }


  /**
   * Collapses a node by setting the "collapse" property to appropriate values
   * on all descendants of this node. The value of the property will be
   * one of the following static variables:
   * <ul>
   * <li>COLLAPSED_TARGET - only the original target node will have this value
   * <li>COLLAPSED_INVISIBLE - if node should be invisible; a node should be 
   * invisible
   * only if all its ancestors are either the target node, or invisible
   * <li>COLLAPSED_VISIBLE - if the node is "collapsed" under its ancestor, but
   * should still be visible
   * </ul>
   *
   * @return a NodeList consisting of descendants of the target node which
   *         are hidden.
   */
  public NodeList collapseNode()
  {
    NodeList hiddenList = new NodeList();

    setProperty(PROPERTY_COLLAPSED, COLLAPSED_TARGET);
    collapseDescendants(this, hiddenList);

    return hiddenList;
  }


  /**
   * Reverse of collapseNode - restores a node to its uncollapsed state by
   * setting the "collapsed" property of all descendants to null
   *
   * @return a NodeList consisting of hidden descendants of the target node 
   * which have been expanded.
   */
  public NodeList expandNode()
  {
    removeProperty(PROPERTY_COLLAPSED);
    NodeList desc = new NodeList();
    getDescendants(this, desc);

    NodeList res = new NodeList();

    for (NodeIterator i = desc.iterator(); i.hasNext(); )
    {
      DrawableNode n = i.next();
      String value = n.getProperty(PROPERTY_COLLAPSED);
      if (value != null && value.equals(COLLAPSED_INVISIBLE))
        res.add(n);
      n.removeProperty(PROPERTY_COLLAPSED);
      n.setHasChanged(true);
      setEdgesDirty(n);
    }

    return res;
  }


  /**
   * @return true if this node is currently collapsed
   */
  public boolean isCollapsed() {
    String state = getProperty(PROPERTY_COLLAPSED);
    if (state == null)
      return false;
    else
      return state.equals(COLLAPSED_TARGET);
  }


  /**
   * @return the collapsed ancestor of this node, or null if no ancestor is
   * currently collapsed
   */
  public DrawableNode getCollapsedAncestor() {
    return findCollapsedAncestor(this);
  }


  /**
   * Get the suggested minimum size of this node.  This is a hint for
   * the layout manager.
   *
   * @return the suggested minimum size in world coordinates.
   */
  public Size getMinimumSize(Graphics2D g, double scale) {
    return (_graph.getGraphUI().getMinimumSize(g, this, scale));
  }


  /**
   * Get the suggested maximum size of this node.  This is a hint for
   * the layout manager.
   *
   * @return the suggested maximum size in world coordinates.
   */
  public Size getMaximumSize(Graphics2D g, double scale) {
    return (_graph.getGraphUI().getMaximumSize(g, this, scale));
  }


  /**
   * Get the suggested preferred size of this node.  This is a hint for
   * the layout manager.
   *
   * @return the suggested preferred size in world coordinates.
   */
  public Size getPreferredSize(Graphics2D g, double scale) {
    return (_graph.getGraphUI().getPreferredSize(g, this, scale));
  }


  void addIncomingEdge(DrawableEdge edge) {
    _incomingEdges.add(edge);
  }

  void addOutgoingEdge(DrawableEdge edge) {
    _outgoingEdges.add(edge);
  }

  void removeIncomingEdge(DrawableEdge edge) {
    _incomingEdges.remove(edge);
  }

  void removeOutgoingEdge(DrawableEdge edge) {
    _outgoingEdges.remove(edge);
  }

  void addTailNode(DrawableNode node) {
    _tailNodes.add(node);
  }

  void addHeadNode(DrawableNode node) {
    _headNodes.add(node);
  }

  void removeHeadNode(DrawableNode node) {
    _headNodes.remove(node);
  }

  void removeTailNode(DrawableNode node) {
    _tailNodes.remove(node);
  }


  /**
   * Recursively sets the "collapse" property on all descendants of the target
   * node, adding each descendant to the given NodeList
   *
   * @param node the target node
   * @param hiddenList nodes that are hidden by the collapse operation will
   * be added to this list
   * @return descendant nodes which have been marked as "hidden". these are
   *  the ones descendants of a collapsed node.
   */
  private void collapseDescendants(DrawableNode node, NodeList hiddenList)
  {
    for (NodeIterator i = node.getTailNodes(); i.hasNext(); )
    {
      DrawableNode nextNode = i.next();
      String nextNodeName = nextNode.getProperty("label");
      boolean isInvisible = true;
      for (NodeIterator itr = nextNode.getHeadNodes(); itr.hasNext(); ) {
        DrawableNode n = itr.next();
        String nName = n.getProperty("label");
        String val = n.getProperty(PROPERTY_COLLAPSED);
        if (val == null ||
            (!val.equals(COLLAPSED_INVISIBLE) && 
             !val.equals(COLLAPSED_TARGET))) {
          isInvisible = false;
          break;
        }
      }
      if (isInvisible)
      {
        nextNode.setProperty(PROPERTY_COLLAPSED, COLLAPSED_INVISIBLE);
        hiddenList.add(nextNode);
      }
      else
      {
        nextNode.setProperty(PROPERTY_COLLAPSED, COLLAPSED_VISIBLE);
      }

      setEdgesDirty(nextNode);
      collapseDescendants(nextNode, hiddenList);
    }
  }


  /**
   * Recursively adds all descendants of the given node to the given list
   *
   * @param node the "ancestor" node
   * @param list the NodeList that the descendants should be added to
   */
  private void getDescendants(DrawableNode node, NodeList list) {
    for (NodeIterator i = node.getTailNodes(); i.hasNext(); ) {
      DrawableNode n = i.next();
      getDescendants(n, list);
      list.add(n);
    }
  }


  /**
   * Recursively find the collapsed ancestor above the given node
   *
   * @param node the target node
   * @return the collapsed ancestor, or null if one does not exist
   */
  private DrawableNode findCollapsedAncestor(DrawableNode node) {
    for (NodeIterator i = node.getHeadNodes(); i.hasNext(); ) {
      DrawableNode n = i.next();
      if (n.isCollapsed())
        return n;
    }
    // no collapsed ancestor in immediate parentage - go up a level
    for (NodeIterator i = node.getHeadNodes(); i.hasNext(); ) {
      DrawableNode collapsed = findCollapsedAncestor(i.next());
      if (collapsed != null)
        return collapsed;
    }
    return null;
  }


  /**
   * Sets the "hasChanged" flag on all edges attached to the given node to
   * true
   *
   * @param node the target node
   */
  private void setEdgesDirty(DrawableNode node) {
    for (EdgeIterator i = node.getIncomingEdges(); i.hasNext(); ) 
      i.next().setHasChanged(true);
    for (EdgeIterator i = node.getOutgoingEdges(); i.hasNext(); )
      i.next().setHasChanged(true);
  }


} // end class DrawableNode
