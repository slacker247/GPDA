package com.appliedminds.martini;


import java.awt.Graphics2D;
import java.awt.Graphics;
import java.awt.event.MouseEvent;
import java.awt.event.MouseEvent;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.Set;



/**
 * The GraphPanel analog of a standard Graph datastructure.  This
 * graph is drawable so it has a method on it that can cuase it to
 * draw itself.  In fact, it doesn't really draw itself, it delegates
 * to some concrete GraphUI object to do that.
 *
 * <p>The main responsibilities of the DrawableGraph are:
 *
 * <ul>
 *
 * <li>To keep track of its nodes and edges.
 *
 * <li>To hold on to a reference of a GraphUI implementor.
 *
 * <li>To perform coarse-grained hit testing, and to delegate
 * fine-grained hit testing to nodes and edges.
 *
 * <li>To hold graph properties.
 *
 * </ul>
 *
 * <p>Objects that are interested in hit test results on this graph
 * can register as HitTestListeners.  The hit-test interface is
 * provided so that this object can work properly inside of some
 * higher-level component, like a GraphPanel.  Client code that is
 * using a GraphPanel to display the graph will not normally use this
 * interface-- instead the client code will register for specific
 * DrawableGraphEvents in the (for example) GraphPanel.
 *
 *
 * @author will@apmindsf.com
 * @author daepark@apmindsf.com
 * @author mathias@apmindsf.com
 */
public class DrawableGraph {


  private GraphUI _graphUI;
  private ArrayList _hitTestListeners;
  private Properties _props;

  private NodeList _nodeList;
  private EdgeList _edgeList;

  /**
   * (Not sure how we're creating graph yet. This will be rewritten later.)
   */
  public DrawableGraph() {
    _hitTestListeners = new ArrayList();
    _props = new Properties();

    _nodeList = new NodeList();
    _edgeList = new EdgeList();
  }


  /**
   * Create a new node in this graph.
   *
   * @return the newly created DrawableNode.
   */
  public DrawableNode createNode() {
    DrawableNode node = new DrawableNode(this);

    // set default to show score bubble
    node.setProperty("scoreBubble", "false");
    node.setProperty("score", "0");
    
    _nodeList.add(node);

    return node;
  }


  /**
   * Create an edge in this graph.
   *
   * @param head a DrawableNode we want to connect the new edge to. This
   *    will be the head node if the graph is directed.
   * @param tail a DrawableNode we want to connect the new edge to. This
   *    will be the tail node if the graph is directed.
   * @return the newly created DrawableEdge.
   */
  public DrawableEdge createEdge(DrawableNode head, DrawableNode tail)
  {
    if (!_nodeList.contains(head))
      throw (new RuntimeException("head node does not exist in the graph"));

    if (!_nodeList.contains(tail))
      throw (new RuntimeException("tail node does not exist in the graph"));

    DrawableEdge edge = new DrawableEdge(this, head, tail);
    
    // set default to show edge bubble
    edge.setProperty("edgeBubble", "true");
    
    _edgeList.add(edge);

    // update head and tail edge lists
    head.addIncomingEdge(edge);
    tail.addOutgoingEdge(edge);

    // update node lists
    head.addTailNode(tail);
    tail.addHeadNode(head);

    return edge;
  }


  /**
   * Remove a node from the graph. All connecting edges will also be
   * removed.
   *
   * @param node the DrawableNode to remove from this graph.
   */
  public void remove(DrawableNode node) {
    // Remove outgoing edges.
    EdgeIterator edges = node.getOutgoingEdges();
    while (edges.hasNext()) {
      remove(edges.next());
    }

    // Remove incoming edges.
    edges = node.getIncomingEdges();
    while (edges.hasNext()) {
      remove(edges.next());
    }

    // finally remove node
    _nodeList.remove(node);
  }


  /**
   * Remove an edge from the graph.
   *
   * @param edge the edge we want to remove from the graph.
   */
  public void remove(DrawableEdge edge) {
    // the connected nodes
    DrawableNode head = edge.getHead();
    DrawableNode tail = edge.getTail();

    // remove tail from tail node list of head node
    head.removeTailNode(tail);
    head.removeIncomingEdge(edge);

    // remove head from head node list of tail node
    tail.removeHeadNode(head);
    tail.removeOutgoingEdge(edge);

    // finally remove edge
    _edgeList.remove(edge);
  }


  /**
   * Set the GraphUI.  This should invalidate (as far as drawing goes)
   * the current graph.
   *
   * @param graphUI the GraphUI implementation to use with this graph.
   */
  void setGraphUI(GraphUI graphUI) {
    _graphUI = graphUI;
  }


  /**
   * Get the current GraphUI that this graph is using.
   *
   * @return the GraphUI implementation this graph uses.
   */
  GraphUI getGraphUI() {
    return _graphUI;
  }


  /**
   * Get the collection of nodes.
   *
   * @return an Iterator over the nodes in this graph.
   */
  public NodeIterator nodesIterator() {
    return (_nodeList.iterator());
  }


  /**
   * Get the collection of edges.
   *
   * @return an Iterator over the edges in this graph.
   */
  public EdgeIterator edgesIterator() {
    return (_edgeList.iterator());
  }


  /**
   * Draw this graph in the given Graphics Context.
   *
   * @param g the Graphics Context to use for drawing this graph.
   * @param vp the viewport for translating into viewport coords.
   * @param hit the hit tester map (modified).
   * @param ctx the context info for the current graph.
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
    drawEdges(g, vp, hit, ctx, newBuffer, erase);
    drawNodes(g, vp, hit, ctx, newBuffer, erase);
  }


  /**
   * Add a HitTestListener to this object.  Registered listeners will
   * be notified when a hit test is successful.
   *
   * @param l a listener.
   */
  public void addHitTestListener(HitTestListener l) {
    _hitTestListeners.add(l);
  }


  public void removeHitTestListener(HitTestListener l) {
    _hitTestListeners.remove(l);
  }


  /**
   * Notify all registered listeners that a hit test has succeeded.
   *
   * @param decorationId the UI decoration identifier.
   * @param obj the object that was hit.
   * @param evt the raw awt event.
   * @param context some context object passed up from the UI.
   */
  public boolean notifyHitTestSuccess(String decorationId,
                                      DrawableGraphElement obj,
                                      MouseEvent evt,
                                      Object context)
  {
    boolean consumed = false;
    for(Iterator i = _hitTestListeners.iterator(); i.hasNext(); ) {
      consumed = consumed ||
        ((HitTestListener) i.next()).hitTestSuccess(decorationId, obj, context, evt);
    }
    return(consumed);
  }



  /**
   * Notify all registered listeners that the mouse has entered a
   * graph objects space.
   *
   * @param decorationId the UI decoration identifier.
   * @param obj the object that was hit.
   * @param evt the raw awt event.
   * @param context some context object passed up from the UI.
   */
  public boolean notifyMouseEntered(String decorationId,
                                    DrawableGraphElement obj,
                                    MouseEvent evt,
                                    Object context)
  {
    MouseEvent mevt = createMouseEvent(evt, MouseEvent.MOUSE_ENTERED);
    boolean consumed = false;
    for(Iterator i = _hitTestListeners.iterator(); i.hasNext(); ) {
      HitTestListener htl = (HitTestListener) i.next();
      consumed = consumed ||
        htl.hitTestSuccess(decorationId, obj, context, mevt);
    }
    return(consumed);
  }


  /**
   * Notify all registered listeners that the mouse has exited a
   * graph objects space.
   *
   * @param decorationId the UI decoration identifier.
   * @param obj the object that was hit.
   * @param evt the raw awt event.
   * @param context some context object passed up from the UI.
   */
  public boolean notifyMouseExited(String decorationId,
                                   DrawableGraphElement obj,
                                   MouseEvent evt,
                                   Object context)
  {
    MouseEvent mevt = createMouseEvent(evt, MouseEvent.MOUSE_EXITED);
    boolean consumed = false;
    for(Iterator i = _hitTestListeners.iterator(); i.hasNext(); ) {
      consumed = consumed ||
        ((HitTestListener) i.next()).hitTestSuccess(decorationId, obj, context, mevt);
    }
    return(consumed);
  }


  /**
   * Notify all registered listeners that a hit test has missed the
   * graph.
   *
   * @param evt the raw awt event.
   */
  public void notifyHitMissedGraph(MouseEvent evt) {
    for(Iterator i = _hitTestListeners.iterator(); i.hasNext(); ) {
      ((HitTestListener) i.next()).hitTestMissedGraph(evt);
    }
  }


  /**
   * Notify all registered listeners that multiple elements in the
   * graph objects space have been &quot;hit&quot;
   *
   * @param hitElements the list of graph objects that
   */
  public void notifyMultipleHitTestSuccess(Object[] hitElements,
                                           Rectangle2D rect)
  {
    Iterator itr = _hitTestListeners.iterator();
    while (itr.hasNext()) {
      ((HitTestListener)itr.next()).multipleHitTestSuccess(hitElements, rect);
    }
  }


  public void notifyMultipleHitTestMissedGraph(Rectangle2D rect) {
    Iterator itr = _hitTestListeners.iterator();
    while (itr.hasNext()) {
      ((HitTestListener)itr.next()).multipleHitTestMissedGraph(rect);
    }
  }


  /**
   * Get the list of graph elements that intersect the specified rectangle.
   *
   * @param map a HitTesterMap object
   * @param rect the rectangle in VIEWPORT coordinates
   * @return a list of DrawableGraphElements that intersect the
   * specified rectangle.
   */
  public Set getIntersectingElements(HitTesterMap map,
                                     Rectangle2D rect)
  {
    Set hitElements = new HashSet();

    //
    // hit test nodes
    //
    NodeIterator itr = nodesIterator();
    while (itr.hasNext()) {
      DrawableNode node = itr.next();
      HitTestResult[] nodeHitTestResults= node.hitTest(map, rect);
      for (int i = 0; i < nodeHitTestResults.length; i++) {
        hitElements.add(nodeHitTestResults[i].getHitTester().getContext());

      }
    }


    //
    // hit test edges
    //
    EdgeIterator itr2 = edgesIterator();
    while (itr2.hasNext()) {
      DrawableEdge edge = itr2.next();
      HitTestResult[] edgeHitTestResults = edge.hitTest(map, rect);
      for (int i = 0; i < edgeHitTestResults.length; i++) {
        hitElements.add(edgeHitTestResults[i].getHitTester().getContext());
      }
    }

    return (hitElements);
  }


  /**
   * Perform multiple hit testing on the graph. Hit test all graph
   * elements within the specified rectangle. Objects registered with
   * the HitTestListeners will be notified of success or miss.
   *
   * @param vp the current viewport.
   * @param rect the bounds considered to be a "hit" by the * user in
   * WORLD coordinates.
   */
  public void performHitTesting(HitTesterMap map,
                                Viewport vp,
                                Rectangle2D rect)
  {
    //
    // Do fine grained hit testing on nodes, then edges using VIEWPORT
    // coordinates.
    //
    Rectangle2D vpRect = vp.mapWorldToViewport(rect);

    Set hitElements = getIntersectingElements(map, vpRect);


    //
    // notify multiple hit listeners
    //
    if (hitElements.size() > 0) {
      notifyMultipleHitTestSuccess(hitElements.toArray(), vpRect);
    }
    else {
      notifyMultipleHitTestMissedGraph(vpRect);
    }
  }


  /**
   * Perform hit testing on the graph.  Objects registered with the as
   * HitTestListeners will be notified of success.
   *
   * @param vp the current viewport.
   * @param rect the area "hit" by the user in WORLD coordinates.
   * @param event the original awt input event.
   */
  public void performHitTesting(HitTesterMap map,
                                Viewport vp,
                                Rectangle2D rect,
                                MouseEvent event)
  {
    // FIX: Should do corse hit testing first using world coordinates.

    boolean graphObjectHit = false;

    //
    // Do fine grained hit testing on nodes, then edges using VIEWPORT
    // coordinates.
    //
    Point2D wpt = new Point2D.Double(rect.getX(), rect.getY());
    Point2D vpt = vp.mapWorldToViewport(wpt);
    Rectangle2D vpRect = new Rectangle2D.Double(vpt.getX(),
                                                vpt.getY(),
                                                rect.getWidth(),
                                                rect.getHeight());

    boolean consumed = false;
    for(NodeIterator i = nodesIterator(); i.hasNext();) {
      DrawableNode n = i.next();
      HitTestResult res = n.hitTest(map, vpRect, event);
      if (res.wasHit()) {

        if (!graphObjectHit) {
          graphObjectHit = true;
        }

        if (res.wasConsumed()) {
          consumed = true;
          break;
        }
      }
    }

    // Contiue with the edges only if one of the nodes didn't take it.
    if (!consumed) {

      for(EdgeIterator i = edgesIterator(); i.hasNext(); ) {
        DrawableEdge e = i.next();
        HitTestResult res = e.hitTest(map, vpRect, event);
        if (res.wasHit()) {

          if (!graphObjectHit) {
            graphObjectHit = true;
          }

          if (res.wasConsumed()) {
            break;
          }
        }
      }
    }


    //
    // If user clicked outside the graph, that is interesting.  Note
    // that this could be done during/right after the coarse testing
    // (which is not implemented).
    //
    if (!graphObjectHit) {
      int id = event.getID();
      if ((id == MouseEvent.MOUSE_CLICKED) ||
          (id == MouseEvent.MOUSE_RELEASED) ||
          (id == MouseEvent.MOUSE_PRESSED))

      {
        notifyHitMissedGraph(event);
      }
    }

  }


  /**
   * Get an arbitrary string property from the graph.
   *
   * @param key to a property we want.
   * @return the value for the given key.
   */
  public String getProperty(String key) {
    return (_props.getProperty(key));
  }


  /**
   * Set an arbitrary string property on the graph.
   *
   * @param key the key to a property value we want to set on this graph.
   * @param val the value for the key we want to set on this graph.
   */
  public void setProperty(String key, String val) {
    _props.setProperty(key, val);
  }



  /**
   * @return an Iterator for all the property keys this graph knows about.
   */
  public Iterator getPropertyKeys() {
    return (_props.keySet().iterator());
  }


  /**
   * Remove a property (key and value) from the graph.
   *
   * @param key the key of the property to remove.
   */
  public void removeProperty(String key) {
    if (key != null) {
      _props.remove(key);
    }
  }



  /*
   * Create a mouse event based on the given mouse event and type.
   */
  private MouseEvent createMouseEvent(MouseEvent orig, int type)
  {
    return(new MouseEvent(orig.getComponent(),
                          type,
                          orig.getWhen(),
                          orig.getModifiers(),
                          orig.getX(),
                          orig.getY(),
                          orig.getClickCount(),
                          orig.isPopupTrigger()));
  }


  /**
   * Draw an edge in the given Graphics Context.
   *
   * @param g the Graphics Context to use for drawing the edge.
   */
  private void drawEdges(Graphics2D g,
                         Viewport vp,
                         HitTesterMap hit,
                         DrawableGraphContext ctx,
                         boolean newBuffer,
                         boolean erase)
  {
    for (EdgeIterator it=edgesIterator(); it.hasNext();) {
      DrawableEdge edge = it.next();
      edge.draw(g, vp, hit, ctx, newBuffer, erase);
    }
  }


  /**
   * Draw a node in the given Graphics Context.
   *
   * @param g the Graphics Context to use for drawing the node.
   */
  private void drawNodes(Graphics2D g,
                         Viewport vp,
                         HitTesterMap hit,
                         DrawableGraphContext ctx,
                         boolean newBuffer,
                         boolean erase)
  {
    for (NodeIterator it=nodesIterator(); it.hasNext();) {
      DrawableNode node = it.next();
      node.draw(g, vp, hit, ctx, newBuffer, erase);
    }
  }


} // end class DrawableGraph
