package com.appliedminds.hmv;

import com.appliedminds.martini.*;
import com.appliedminds.martinix.fader.*;
import com.appliedminds.martinix.ui.TextScreenData;

import java.awt.Cursor;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.*;
import java.awt.geom.Rectangle2D;
import java.io.*;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.*;
import javax.swing.*;


/**
 * The select tool for the HMV.
 *
 * <b>Note that this has been replaced by the non-modal
 * tool, HMVSelectTool2<b>. Its legacy is for source reference
 * only.
 *
 * @author daepark@apmindsf.com
 */
public class HMVSelectTool
  extends HMVTool
{
  /** property indicating that an element is selected */
  private static final String PROPERTY_SELECTED = "selected";

  /** precomputed natural log of 2 */
  private static final double LOG_2_INVERSE = 1 / Math.log(2);

  private boolean _isAdjusting;

  // the node whose slider is being adjusted
  private DrawableNode _sliderNode;

  // the edge whose slider is being adjusted
  private DrawableEdge _sliderEdge;

  // the graph element whose slider value is being edited
  private DrawableGraphElement _slidingElement;


  private Set _selection;   // a list of currently selected graph elements

  // for moving nodes
  private Point _lastPoint;
  private Set   _selectedNodes;
  private boolean _prepDrag = false;

  // marqueeing flag
  private boolean _marqueeing = false;

  private JPopupMenu _nodePopupMenu = null;
  private JPopupMenu _edgePopupMenu = null;
  private DeleteAction _deleteAction = null;
  private NodePropertiesAction _nodePropertiesAction = null;
  private GraphElementPropertiesDialog _nodePropertiesDialog = null;

  private JPopupMenu _nodeIconPopupMenu = null;
  private SupportIconAddAction _nodeIconAddAction = null;
  private SupportIconDeleteAction _nodeIconDeleteAction = null;
  private SupportIconPropertiesAction _nodeIconPropertiesAction = null;
  private SupportIconPropertiesDialog _nodeIconPropertiesDialog = null;

  //
  // This is the path through the graph that tells us which nodes will
  // be affected if the probability of the _sliderNode changes.
  //
  private List _cpath;


  public HMVSelectTool(Cursor cursor, HMV app) {
    super(cursor, app);
    _isAdjusting = false;
    _lastPoint = null;
    _sliderNode = null;
    _cpath = new ArrayList();
    _selectedNodes = new HashSet();
    _selection = new HashSet();

    _deleteAction = new DeleteAction();
  }


  // low level events we care about
  public void mousePressed(MouseEvent e) {
    _lastPoint = e.getPoint();
  }


  public void mouseDragged(MouseEvent e) {
    if (_isAdjusting) {
      recalculateProbabilityUsingPath2(_sliderNode, _cpath, getGraphPanel());
      getGraphPanel().paintImmediately();
    }

    if (_prepDrag) {
      getGraphPanel().startDrag(_selectedNodes);
      _prepDrag = false; // stage 2
    }

    if (!getGraphPanel().getInDrag()) {
      return;
    }
    else {
      // first compute how much we moved
      Point curPoint = e.getPoint();
      int dx = (curPoint.x - _lastPoint.x);
      int dy = (curPoint.y - _lastPoint.y);

      getGraphPanel().dragTo(dx, dy);
      _lastPoint = curPoint;
    }
  }


  public void mouseReleased(MouseEvent e) {
    if (_isAdjusting) {
      recalculateProbabilityUsingPath2(_sliderNode, _cpath, getGraphPanel());

      if (!_app.getRealTimeSliderMode()) {
        // reshow all sliders
        showAllSliders();
      }

      getGraphPanel().stopElementEdit();
      _isAdjusting = false;
      _sliderNode = null;
    }

    _lastPoint = null;
    if (getGraphPanel().getInDrag()) {
      getGraphPanel().finishDrag();
      _prepDrag = false;
    }
  }


  /**
   * Handles key stroke events
   */
  public void keyReleased(KeyEvent e)
  {
    if (!getApp().getTextField().isVisible() &&
        (e.getKeyCode() == KeyEvent.VK_DELETE ||
         e.getKeyCode() == KeyEvent.VK_BACK_SPACE))
    {
      _deleteAction.actionPerformed(new ActionEvent(e.getSource(), e.getID(),
                                                    e.toString()));
    }
  }


  /**
   * Default impl returns false, override if you want this event.
   */
  public boolean handleNodeMouseEvent(DrawableGraphMouseEvent e) {
    MouseEvent me = e.getMouseEvent();
    DrawableNode node = (DrawableNode)e.getGraphElement();

    if (mousePressed(e) || me.isPopupTrigger()) {

      deactivateMarqueePane();

      // prepare for drag
      _prepDrag = true;

      if (SwingUtilities.isLeftMouseButton(me)) {
        if (me.isControlDown() && !me.isShiftDown()) {
          toggleNodeSelection(node);
        }
        else {
          if (!me.isShiftDown()) {
            clearSelection();
          }
          selectNode(node);
        }

        refreshGraph();
      }
      //
      // if right click, show node popup
      //
      else if (me.isPopupTrigger()) {
        showNodePopupMenu((DrawableNode)e.getGraphElement(), me);
      }
    }

    return (true);
  }


  public boolean handleNodeTextMouseEvent(DrawableGraphMouseEvent e)
  {
    MouseEvent me = e.getMouseEvent();
    DrawableNode n = (DrawableNode) e.getGraphElement();

    if (SwingUtilities.isLeftMouseButton(me) &&
        (me.getClickCount() == 1) &&
        (me.getID() == MouseEvent.MOUSE_CLICKED))
    {
      TextScreenData nodeLabelData = (TextScreenData) e.getContext();
      Rectangle2D textRect = nodeLabelData.getBounds();
      getApp().editNodeLabel(n, textRect);
      return (true);
    }
    else
    {
      return (false);
    }
  }


  public boolean handleEdgeMouseEvent(DrawableGraphMouseEvent e) {
    MouseEvent me = e.getMouseEvent();

    if (mousePressed(e) || me.isPopupTrigger()) {
      DrawableEdge edge = (DrawableEdge)e.getGraphElement();

      deactivateMarqueePane();

      if (updateEdgeSelection(edge, e))
        refreshGraph();

      //
      // if right click, show node popup
      //
      if (me.isPopupTrigger()) {
        showEdgePopupMenu((DrawableEdge)e.getGraphElement(), me);
      }
    }

    return (true);
  }


  /**
   * Default impl returns false, override if you want this event.
   */
  public boolean handleHitTestMissedMouseEvent(DrawableGraphMouseEvent e) {
    MouseEvent me = e.getMouseEvent();

    if (mousePressed(e)) {
      if (SwingUtilities.isLeftMouseButton(me)) {
        if (!me.isShiftDown()) {
          clearSelection();
          refreshGraph();
        }

        activateMarqueePane();
      }
    }

    return (true);
  }

  public void handleMultipleHitTestSuccess(DrawableGraphMultipleSelectionEvent e)
  {
    if (_marqueeing) {
      Object[] objs = e.getContexts();
      for (int i = 0; i < objs.length; i++) {
        if (objs[i] instanceof DrawableGraphElement) {
          selectGraphElement((DrawableGraphElement)objs[i]);
        }
      }
      refreshGraph();
      deactivateMarqueePane();
    }
  }

  public void handleMultipleHitTestMissedGraph(Rectangle2D rectangle)
  {
    deactivateMarqueePane();
  }


  public void handleMarqueeSelection(Rectangle marqueeBounds) {
    _app.getGraphPanel().marqueeSelected(marqueeBounds);
  }


  /**
   * Open edit mode.
   */
  public boolean handleSliderKnobMouseEvent(DrawableGraphMouseEvent e)
  {
    boolean handled = false;

    if (mousePressed(e)) {

      deactivateMarqueePane();

      if (!_isAdjusting) {
        GraphPanel p = getGraphPanel();
        DrawableGraphElement elt = e.getGraphElement();

        if (elt instanceof DrawableNode) {
          _sliderNode = (DrawableNode)elt;

          FaderUtil.setManual(_sliderNode, true);

          initCalculationPath2(p.getDrawableGraph(), _sliderNode,
                              _cpath, false);
          p.startElementEdit(_sliderNode,
                             null,
                             e.getMouseEvent().getPoint(),
                             e.getContext(),
                             false);

          _isAdjusting = true;

          if (!_app.getRealTimeSliderMode()) {
            // erase all sliders except for the manual one
            hideAutomaticSliders(_sliderNode);
            getGraphPanel().paintImmediately();
          }

          handled = true;
        }
        else if (elt instanceof DrawableEdge) {
          _sliderEdge = (DrawableEdge)elt;

          FaderUtil.setManual(_sliderEdge, true);

          _sliderNode = null;

          initCalculationPath2(p.getDrawableGraph(), _sliderEdge.getHead(),
                              _cpath, true);

          p.startElementEdit(_sliderEdge,
                             null,
                             e.getMouseEvent().getPoint(),
                             e.getContext(),
                             false);

          _isAdjusting = true;

          if (!_app.getRealTimeSliderMode()) {
            // erase all sliders except for the manual one
            hideAutomaticSliders(null);
            getGraphPanel().paintImmediately();
          }

          handled = true;
        }
      }
    }

    return (handled);
  }


  /**
   * show/hide edge slider
   */
  public boolean handleEdgeBubbleMouseEvent(DrawableGraphMouseEvent e)
  {
    boolean handled = false;

    if (mousePressed(e)) {
      deactivateMarqueePane();

      MouseEvent me = e.getMouseEvent();
      //
      // if left click, show/hide edge slider
      //
      if (SwingUtilities.isLeftMouseButton(me)) {
        DrawableEdge edge = (DrawableEdge)e.getGraphElement();
        updateEdgeSelection(edge, e);
        FaderUtil.setSliderVisible(edge, !FaderUtil.isSliderVisible(edge));
        refreshGraph();

        handled = true;
      }
    }

    return (handled);
  }


  public boolean handleNodeIconMouseEvent(DrawableGraphMouseEvent e)
  {
    MouseEvent me = e.getMouseEvent();

    if (mousePressed(e) || me.isPopupTrigger()) {
      if (SwingUtilities.isLeftMouseButton(me)) {
        //
        // Delegate to various support handlers.
        //
        FaderIconHitContext hitContext =
          (FaderIconHitContext)e.getContext();

        SupportHandler handler =
          SupportHandlerManager.findSupportHandler(hitContext.getType());

        Point pt = me.getPoint();
        SwingUtilities.convertPointToScreen(pt, getGraphPanel());

        if (handler != null) {
          handler.handleURL(e.getGraphElement(), hitContext.getURL(), pt);
        }
        else {
          System.err.println("No support handler found for type: " +
                             hitContext.getType());
        }
      }
      else if (me.isPopupTrigger()) {
        FaderIconHitContext hitContext =
          (FaderIconHitContext)e.getContext();

        showNodeIconPopupMenu((DrawableNode)e.getGraphElement(),
                              hitContext, me);
      }
    }
    return (true);
  }


  /**
   * Let the user edit the value in a textfield widget if it is a
   * mouse click.
   */
  public boolean handleSliderValueEvent(DrawableGraphMouseEvent e)
  {
    MouseEvent me = e.getMouseEvent();
    if (SwingUtilities.isLeftMouseButton(me) &&
        (me.getClickCount() == 1) &&
        (me.getID() == MouseEvent.MOUSE_CLICKED))
    {
      _slidingElement = e.getGraphElement();
      int val = FaderUtil.getSliderValue(_slidingElement, 0);

      Rectangle2D bounds = (Rectangle2D) e.getContext();
      JTextField tf = getApp().getTextField();
      tf.setText(String.valueOf(val));
      tf.selectAll();
      tf.setLocation((int)bounds.getX(), (int)bounds.getY());
      tf.setVisible(true);

      if (_slidingElement instanceof DrawableNode) {
        ((HMV.DigitOnlyDocument)tf.getDocument()).setAllowSign(false);
        ((HMV.DigitOnlyDocument)tf.getDocument()).setMaxLength(3);
      }
      else {
        ((HMV.DigitOnlyDocument)tf.getDocument()).setAllowSign(true);
        ((HMV.DigitOnlyDocument)tf.getDocument()).setMaxLength(2);
      }
      tf.requestFocus();
      return true;
    }

    return (false);
  }


  private void processSliderValueTextField()
  {
    JTextField tf = getApp().getTextField();
    if (tf.isVisible())   // user typed in the slider value
    {
      int max, min;

      if (_slidingElement instanceof DrawableNode) {
        max = HMVNodeProperties.MAX_CERTAINTY_PERCENT;
        min = HMVNodeProperties.MIN_CERTAINTY_PERCENT;
      }
      else {
        max = HMVEdgeProperties.MAX_INFLUENCE;
        min = HMVEdgeProperties.MIN_INFLUENCE;
      }

      int val = FaderUtil.getSliderValue(_slidingElement);
      if (tf.getText().trim().length() != 0)
      {
        val = (int)Float.parseFloat(tf.getText());
      }
      if (val > max) {
        val = max;
      }
      else if (val < min) {
        val = min;
      }
      FaderUtil.setSliderValue(_slidingElement, val);
      tf.setVisible(false);
      getGraphPanel().repaint();
    }
  }


  public void actionPerformed(ActionEvent e) {
    processSliderValueTextField();
  }


  public boolean mousePressedHook(MouseEvent e)
  {
    mousePressed(e);  // this saves the press point. maybe we should rename it
    return (true);
  }


  /**
   * Set all node sliders to visible.
   */
  private void showAllSliders() {
    NodeIterator itr = getGraphPanel().getDrawableGraph().nodesIterator();
    while (itr.hasNext()) {
      DrawableNode node = itr.next();
      if (!FaderUtil.isSliderVisible(node)) {
        FaderUtil.setSliderVisible(node, true);
      }
    }
  }

  //
  // begin HMVCommonTool interface
  //
  public void showNodeProperties(DrawableNode node) {
    if (_nodePropertiesAction == null) {
      _nodePropertiesAction = new NodePropertiesAction();
    }

    _nodePropertiesAction.showNodeProperties(node);
  }

  public void showNodePopupMenu(DrawableNode node, MouseEvent e) {
    if (e.isPopupTrigger()) {
      if (_nodePopupMenu == null) {
        _nodePopupMenu = new JPopupMenu();

        if (_nodeIconAddAction == null) {
          _nodeIconAddAction = new SupportIconAddAction();
        }

        if (_nodePropertiesAction == null) {
          _nodePropertiesAction = new NodePropertiesAction();
        }

        _nodePopupMenu.add(_deleteAction);
        _nodePopupMenu.addSeparator();
        _nodePopupMenu.add(_nodeIconAddAction);
        _nodePopupMenu.addSeparator();
        _nodePopupMenu.add(_nodePropertiesAction);
      }

      // make this node the only selected element, unless it's already selected
      if (!elementIsSelected(node)) {
        clearSelection();
        selectNode(node);
      }
      _nodeIconAddAction.setCurrentNode(node);
      _nodePropertiesAction.setCurrentNode(node);

      _nodePopupMenu.show(e.getComponent(), e.getX(), e.getY());
    }
  }

  public boolean isNodePopupMenuVisible() {
    return (_nodePopupMenu != null && _nodePopupMenu.isVisible());
  }

  public void showEdgePopupMenu(DrawableEdge edge, MouseEvent e) {
    if (e.isPopupTrigger()) {
      if (_edgePopupMenu == null) {
        _edgePopupMenu = new JPopupMenu();

        _edgePopupMenu.add(_deleteAction);
      }

      // make this edge the only selected element, unless it's already selected
      if (!elementIsSelected(edge)) {
        clearSelection();
        selectEdge(edge);
      }

      _edgePopupMenu.show(e.getComponent(), e.getX(), e.getY());
    }
  }

  public boolean isEdgePopupMenuVisible() {
    return (_edgePopupMenu != null && _edgePopupMenu.isVisible());
  }

  //
  // end HMVCommonTool interface
  //


  /**
   * Called by edge mouse event handlers - updates the selection status of
   * the edge
   *
   * @param edge the currently active DrawableEdge
   * @param event the DrawableGraphMouseEvent currently being handled
   * @return true if the current selection was changed (callers may want to
   * refresh the graph in this case)
   */
  private boolean updateEdgeSelection(DrawableEdge edge,
                                      DrawableGraphMouseEvent event) {
    boolean selectionChanged = false;
    MouseEvent me = event.getMouseEvent();
    if (SwingUtilities.isLeftMouseButton(me)) {
      if (me.isControlDown() && !me.isShiftDown()) {
        toggleEdgeSelection(edge);
      }
      else {
        if (!me.isShiftDown()) {
          clearSelection();
        }
        selectEdge(edge);
      }
      selectionChanged = true;
    }
    return selectionChanged;
  }


  /**
   * Hide automatic sliders by setting the slider visibility property
   * on "automatic" nodes to false.
   */
  private void hideAutomaticSliders(DrawableNode sliderNode) {
    NodeIterator itr = getGraphPanel().getDrawableGraph().nodesIterator();
    while (itr.hasNext()) {
      DrawableNode node = itr.next();

      String val = node.getProperty("sliderVisible");
      boolean visible = FaderUtil.isSliderVisible(node);

      if (visible && (node != sliderNode)) {
        FaderUtil.setSliderVisible(node, false);
      }
      else if (!visible && (node == sliderNode)) {
        FaderUtil.setSliderVisible(node, true);
      }
    }
  }




  /**
   * Show context popup menu for the node icons.
   */
  private void showNodeIconPopupMenu(DrawableNode node,
                                     FaderIconHitContext context,
                                     MouseEvent e)
  {
    if (e.isPopupTrigger()) {
      if (_nodeIconPopupMenu == null) {
        _nodeIconPopupMenu = new JPopupMenu();

        if (_nodeIconDeleteAction == null) {
          _nodeIconDeleteAction = new SupportIconDeleteAction(getGraphPanel());
        }

        if (_nodeIconPropertiesAction == null) {
          _nodeIconPropertiesAction = new SupportIconPropertiesAction();
        }

        _nodeIconPopupMenu.add(_nodeIconDeleteAction);
        _nodeIconPopupMenu.addSeparator();
        _nodeIconPopupMenu.add(_nodeIconPropertiesAction);
      }

      _nodeIconDeleteAction.setSupportIconToDelete(node, context);
      _nodeIconPropertiesAction.setNodeIconProperties(node, context);

      _nodeIconPopupMenu.show(e.getComponent(), e.getX(), e.getY());
    }
  }


  /**
   * Use the pre-computed path to recalulate the probability of
   * affected nodes.
   *
   * @param sliderNode the node whose slider is being adjusted.
   * @param path the path
   */
  protected static final void recalculateProbabilityUsingPath2
    (DrawableNode mnode, List path, GraphPanel graphPanel)
  {
    DrawableGraphContext ctx = graphPanel.getDrawableGraphContext();

    if (mnode != null) {
      setNeedsRepaintOnOutgoingEdges(mnode, ctx);
    }

    for(Iterator i = path.iterator(); i.hasNext(); ) {
      DrawableNode anode = (DrawableNode)i.next();

      int oldValue  = FaderUtil.getSliderValue(anode);

      // There is always a supporting and refuting edge list.
      List supporting = (List)i.next();
      List refuting = (List)i.next();

      double newValue = unbias(supporting, refuting);

      //      System.err.println("unbiased value: " + newValue);

      int newIntValue = (int)Math.round(newValue * 100.0);

      if (oldValue != newIntValue) {
        FaderUtil.setSliderValue(anode, newIntValue);
        setNeedsRepaintOnOutgoingEdges(anode, ctx);
      }
    }
  }


  private static void setNeedsRepaintOnOutgoingEdges(DrawableNode node,
                                                     DrawableGraphContext ctx)
  {
    // need to update the outgoing edges of this
    // node who's slider value is changing
    EdgeIterator edges = node.getOutgoingEdges();
    while (edges.hasNext()) {
      DrawableEdge edge = edges.next();
      ctx.setNeedsRepaint(edge, true);
    }
  }


  /**
   * Normalize (unbias) node calculation
   *
   * <Pre>
   * Public Calculate(Arraylist Sw, Arraylist, Sv, Arraylist Rw, Arraylist Rv)
   * {
   * Float Ssum=0;
   * Float A=0;
   * For(Int I=0; I <Sw.Size(); I++){
   *   Ssum+=Sw[I]*Sv[I];
   *   A+=Sw[I];
   * }
   * Float Rsum=0;
   * Float B=0;
   * For(Int I=0; I <Rw.Size(); I++){
   *  Rsum+=Rw[I]*Rv[I];
   *  B+=Rw[I];
   * }
   *
   * Float T=(Ssum-Rsum)/(A);
   *
   * Return Math.max(T, 0);
   * }
   * </Pre>
   *
   * @see Kurt Bollacker
   */
  private static double unbias(List supporting, List refuting) {
    double Ssum = 0;
    double A = 0;

    for (Iterator itr=supporting.iterator(); itr.hasNext();) {
      DrawableEdge edge = (DrawableEdge)itr.next();
      double tailValue = FaderUtil.getSliderValue(edge.getTail()) * 0.01;
      int edgeValue = FaderUtil.getSliderValue(edge);
      Ssum += (edgeValue * tailValue);
      A += edgeValue;
    }

    double Rsum = 0;
    double B = 0;
    for (Iterator itr=refuting.iterator(); itr.hasNext();) {
      DrawableEdge edge = (DrawableEdge)itr.next();
      double tailValue = FaderUtil.getSliderValue(edge.getTail()) * 0.01;
      int edgeValue = FaderUtil.getSliderValue(edge);
      Rsum += (edgeValue * tailValue);
      B += edgeValue;
    }

    if (A == 0) {
      return (0);
    }

    double T = (Ssum - Rsum) / A;
    return (Math.max(T, 0));

//     System.err.println("Ssum = " + Ssum + ", A = " + A + ", Rsum = " + Rsum + ", B = " + B + ", T = " + T;
  }


  /*
   * Construct the path that is a list of nodes and all of its
   * incoming edges broken into records. Each record starts with a
   * single node followed by all of its supporting edges (i.e., edges
   * that regard the node as its head and is &quot;positive&quot;),
   * followed by all of its refuting edges (i.e., edges that regard
   * the node as its head and is &quot;negative&quot;). If a node is
   * in the list then it will always have a pair of supporting and
   * refuting edge list.
   *
   * This allows us to do the hard graph sorting and traversal stuff
   * up front so that we can recalculate slider values quickly later.
   *
   * @param sliderNode the node whose slider is being adjusted
   * @param path will be filled with the calculation path.
   */
  protected static final void initCalculationPath2(DrawableGraph graph,
                                                   DrawableNode sliderNode,
                                                   List path,
                                                   boolean recalculate)
  {
    path.clear();

    //
    // Collect only the nodes that are underneath the slider node.
    // Performing a BFS from the slider node should take care of this.
    //
    final Set childNodes = new HashSet();
    if (sliderNode != null) {
      GraphUtil.bfs(graph, sliderNode, new GraphUtil.GraphVisitor() {
          public void visit(DrawableGraphElement e) {
            childNodes.add(e);
          }
        });
    }
    else {
      // We are recalculating everything (for example, deleting a node
      // or edge).
      for (NodeIterator itr=graph.nodesIterator(); itr.hasNext();) {
        childNodes.add(itr.next());
      }
    }


    //    System.err.println("childNodes.size() : " + childNodes.size());


    // Do a topological sort on the graph to get
    // the ordering on what should be calculated first
    //
    // We only need to do a topologial sort
    // from the manual node and NOT on the entire graph
    //
    Iterator sorted = GraphUtil.topologicalSort(graph);

    List sortedList = new ArrayList();
    boolean haveSeenSliderNode = false;

    if (sliderNode != null) {
      while (sorted.hasNext()) {
        DrawableNode current = (DrawableNode)sorted.next();
        if (current == sliderNode) {
          haveSeenSliderNode = (haveSeenSliderNode || true);
        }

        if (haveSeenSliderNode) {
          sortedList.add(current);
        }
      }
    }
    else {
      while (sorted.hasNext()) {
        sortedList.add(sorted.next());
      }
    }

    //    System.err.println("sortedList.size() : " + sortedList.size());

    //
    // Now calculate all nodes from the manual node
    //
    sorted = sortedList.iterator();
    while (sorted.hasNext()) {
      DrawableNode current = (DrawableNode)sorted.next();
      // don't adjust the manual node
      if (!recalculate && current == sliderNode) {
        continue;
      }
      else if ((recalculate && current == sliderNode) ||
               current != sliderNode)
      {
        if (childNodes.contains(current)) {
          // set it to automatic
          FaderUtil.setManual(current, false);

          EdgeIterator itr = current.getIncomingEdges();

          // only update slider value if the node has parents
          if (itr.hasNext()) {

            List supporting = new ArrayList();
            List refuting = new ArrayList();

            int edgeCount = 0;
            path.add(current);

            while (itr.hasNext()) {

              DrawableEdge edge = (DrawableEdge)itr.next();
              if (FaderUtil.isEdgePositive(edge)) {
                supporting.add(edge);
                //                System.err.println("adding supporting edge: " + edge.getTail().getProperty("id") + " to " + edge.getHead().getProperty("id"));
              }
              else {
                refuting.add(edge);
                //                System.err.println("adding refuting edge: " + edge.getTail().getProperty("id") + " to " + edge.getHead().getProperty("id"));
              }
              ++edgeCount;
            }

            path.add(supporting);
            path.add(refuting);
          }
        }
      }
    }
  }

  private boolean toggleNodeSelection(DrawableNode node) {
    if (toggleGraphElement(node)) {
      _selectedNodes.add(node);
      return (true);
    }
    else {
      _selectedNodes.remove(node);
      return (false);
    }
  }


  private void toggleEdgeSelection(DrawableEdge edge) {
    toggleGraphElement(edge);
  }


  /**
   * @return true if element is selected
   */
  private boolean toggleGraphElement(DrawableGraphElement element) {
    String selected = element.getProperty(PROPERTY_SELECTED);

    if ("true".equals(selected)) {
      element.setProperty(PROPERTY_SELECTED, "false");
      _selection.remove(element);
      return (false);
    }
    else {
      selectGraphElement(element);
      return (true);
    }
  }


  /**
   * @return true if the given element is currently selected
   */
  private boolean elementIsSelected(DrawableGraphElement element) {
    String selected =
      element.getProperty(PROPERTY_SELECTED);
    return ("true".equals(selected));
  }



  /**
   * Select a node.
   */
  private void selectNode(DrawableNode node) {
    selectGraphElement(node);
  }


  /**
   * Select an edge.
   */
  private void selectEdge(DrawableEdge edge) {
    selectGraphElement(edge);
  }


  /**
   * Select a graph element (node or edge).
   */
  private void selectGraphElement(DrawableGraphElement element) {
    element.setProperty("selected", "true");
    _selection.add(element);
    if (element instanceof DrawableNode) {
      _selectedNodes.add(element);
    }
  }


  private void refreshGraph() {
    getGraphPanel().repaint();
  }


  /**
   * @return an Iterator containing the currently selected items
   */
  private Iterator getSelected() {
    return _selection.iterator();
  }


  /**
   * Clear all current selected graph elements.
   */
  private void clearSelection() {
    for (Iterator itr=_selection.iterator(); itr.hasNext();) {
      ((DrawableGraphElement) itr.next()).
        setProperty("selected", "false");
    }
    _selection.clear();
    _selectedNodes.clear();
  }


  private void deactivateMarqueePane() {
    _app.setMarqueePaneVisibility(false);
    _marqueeing = false;
  }


  private void activateMarqueePane() {
    _app.setCursor(getCursor());
    _app.setMarqueePaneVisibility(true);
    _marqueeing = true;
  }


  /**
   * Action object that knows how to delete nodes and edges.
   */
  private class DeleteAction extends AbstractAction {

    private Set __nodesToDelete;
    private Set __edgesToDelete;

    public DeleteAction() {
      super("Delete");
      __nodesToDelete = new HashSet();
      __edgesToDelete = new HashSet();
    }

    /**
     * delete nodes and edges in the "to delete" list.
     */
    public void actionPerformed(ActionEvent e) {
      DrawableGraph graph = getGraphPanel().getDrawableGraph();
      initElementsToDelete();

      if (__nodesToDelete.size() == 0 && __edgesToDelete.size() == 0) {
        return;
      }

      // first "erase" it from the visual first
      Iterator itr = __nodesToDelete.iterator();
      while (itr.hasNext()) {
        DrawableNode node = (DrawableNode)itr.next();
        if (node == null)
          System.err.println("node is null");

        // remove the edges connected to this node as well
        EdgeIterator edges = node.getIncomingEdges();
        while (edges.hasNext()) {
          DrawableEdge edge = edges.next();
          __edgesToDelete.add(edge);
        }
        edges = node.getOutgoingEdges();
        while (edges.hasNext()) {
          DrawableEdge edge = edges.next();
          __edgesToDelete.add(edge);
        }

        // remove node from graph
        graph.remove(node);
      }

      itr = __edgesToDelete.iterator();
      while (itr.hasNext()) {
        DrawableEdge edge = (DrawableEdge)itr.next();
        System.err.println("deleting " + edge.toString());

        // remove edge from graph
        graph.remove(edge);
      }

      getGraphPanel().redraw();

      // recalculate probabilites
      initCalculationPath2(graph, null, _cpath, true);
      recalculateProbabilityUsingPath2(null, _cpath, getGraphPanel());

      getGraphPanel().paintImmediately();

      __nodesToDelete.clear();
      __edgesToDelete.clear();
    }

    /**
     * Creates the list of nodes and edges to delete, based on the currently
     * selected elements
     */
    private void initElementsToDelete() {
      __nodesToDelete.clear();
      __edgesToDelete.clear();

      for (Iterator i = getSelected(); i.hasNext(); )
      {
        Object o = i.next();
        if (o instanceof DrawableNode)
          __nodesToDelete.add(o);
        else if (o instanceof DrawableEdge)
          __edgesToDelete.add(o);
      }
    }

  } // end class DeleteAction


  /**
   * Action to add support documents to a node.
   */
  private class SupportIconAddAction extends AbstractAction {

    private DrawableNode __currentNode = null;

    private DialogHandler __handler = new DialogHandlerAdapter() {
        public void clickedOk() {
          String newType = _nodeIconPropertiesDialog.getType();
          URL newURL = _nodeIconPropertiesDialog.getURL();

          if (! (__currentNode == null || newType == null || newURL == null)) {
            TypeInfoAdapter typeInfo =
              TypeInfoAdapter.getTypeInfoAdapter(__currentNode);

            typeInfo.add(newType, true, newURL);
            getGraphPanel().repaint();  // repaint to add icon
          }

          // Remove ourself from the dialogs listener list.
          // This will be added whenever there is an action event.
          _nodeIconPropertiesDialog.removeDialogHandler(this);
        }

        public void clickedCancel() {
          // Remove ourself from the dialogs listener list.
          // This will be added whenever there is an action event.
          _nodeIconPropertiesDialog.removeDialogHandler(this);
        }
      };

    public SupportIconAddAction() {
      super("Add Supporting Document");
    }

    public void actionPerformed(ActionEvent e) {
      if (_nodeIconPropertiesDialog == null) {
        _nodeIconPropertiesDialog = new SupportIconPropertiesDialog(getApp());
      }

      _nodeIconPropertiesDialog.addDialogHandler(__handler);
      _nodeIconPropertiesDialog.update(null, null);
      _nodeIconPropertiesDialog.setVisible(true);
    }

    public void setCurrentNode(DrawableNode node) {
      __currentNode = node;
    }

  } // end class "SupportIconAddAction"


  /**
   * Action object to view/edit node properties (including labels,
   * slider value, etc.).
   */
  private class NodePropertiesAction extends AbstractAction {

    private DrawableNode __currentNode = null;

    private DialogHandler __handler = new DialogHandlerAdapter() {
        public void clickedOk() {
          NodePropertiesAction.this.updateGraphPanel();
          // Remove ourself from the dialogs listener list.
          // This will be added whenever there is an action event.
          _nodePropertiesDialog.removeDialogHandler(this);
          _nodePropertiesDialog.setVisible(false);
        }

        public void clickedApply() {
          NodePropertiesAction.this.updateGraphPanel();
        }

        public void clickedCancel() {
          // Undo any changes
          // This should have been taken care by the NodeProperties dialog
          // We just need to recalculate the probabilities

          NodePropertiesAction.this.updateGraphPanel();

          // Remove ourself from the dialogs listener list.
          // This will be added whenever there is an action event.
          _nodePropertiesDialog.removeDialogHandler(this);
          _nodePropertiesDialog.setVisible(false);
        }
      };

    public NodePropertiesAction() {
      super("Node Properties");
    }

    public void actionPerformed(ActionEvent e) {
      this.showNodeProperties();
    }

    public void showNodeProperties() {
      if (__currentNode != null) {
        if (_nodePropertiesDialog == null) {
          _nodePropertiesDialog = new GraphElementPropertiesDialog(getApp());
        }


        HMVNodeProperties nodeProps = new HMVNodeProperties(__currentNode);
        _nodePropertiesDialog.setGraphElementProperties(nodeProps);
        _nodePropertiesDialog.addDialogHandler(__handler);
        _nodePropertiesDialog.setVisible(true);
      }
    }

    public void showNodeProperties(DrawableNode node) {
      this.setCurrentNode(node);
      this.showNodeProperties();
    }

    public void setCurrentNode(DrawableNode node) {
      __currentNode = node;
    }

    private void updateGraphPanel() {
      // Recalculate probabilities
      if (_nodePropertiesDialog.propertiesChanged()) {
        initCalculationPath2(getGraphPanel().getDrawableGraph(),
                             __currentNode,
                             _cpath,
                             false);
        recalculateProbabilityUsingPath2(__currentNode,
                                         _cpath,
                                         getGraphPanel());

        // Repaint graph
        getGraphPanel().repaint();
      }
    }

  } // end class "NodePropertiesAction"


  /**
   * Action object that knows how to show the properties of a support
   * icon.
   */
  private class SupportIconPropertiesAction extends AbstractAction {

    private DrawableNode __currentNode = null;
    private FaderIconHitContext __currentContext = null;
    private DialogHandler __updateHandler = new DialogHandlerAdapter() {
        public void clickedOk() {
          String newType = _nodeIconPropertiesDialog.getType();
          URL newURL = _nodeIconPropertiesDialog.getURL();

          if (newType != null && newURL != null) {
            TypeInfoAdapter typeInfo =
              TypeInfoAdapter.getTypeInfoAdapter(__currentNode);
            String oldType = typeInfo.getType(__currentContext.getIndex());
            URL oldURL = typeInfo.getTypeURL(__currentContext.getIndex());

            if (!newType.equals(oldType)) {
              typeInfo.setType(__currentContext.getIndex(), newType);
              getGraphPanel().repaint();  // repaint to change icons
            }

            if (!newURL.equals(oldURL)) {
              typeInfo.setTypeURL(__currentContext.getIndex(), newURL);
            }
          }

          // Remove ourself from the dialogs listener list.
          // This will be added whenever there is an action event.
          _nodeIconPropertiesDialog.removeDialogHandler(this);
        }

        public void clickedCancel() {
          // Remove ourself from the dialogs listener list.
          // This will be added whenever there is an action event.
          _nodeIconPropertiesDialog.removeDialogHandler(this);}
      };

    public SupportIconPropertiesAction() {
      super("Supporting Document Properties");
    }

    public void setNodeIconProperties(DrawableNode node,
                                      FaderIconHitContext context)
    {
      __currentNode = node;
      __currentContext = context;
    }

    public void actionPerformed(ActionEvent e) {
      if (!(__currentNode == null || __currentContext == null)) {
        if (_nodeIconPropertiesDialog == null) {
          _nodeIconPropertiesDialog = new SupportIconPropertiesDialog(getApp());
        }

        _nodeIconPropertiesDialog.addDialogHandler(__updateHandler);
        _nodeIconPropertiesDialog.update(__currentContext.getType(),
                                         __currentContext.getURL());

        _nodeIconPropertiesDialog.setVisible(true);
      }
    }

  } // end class "SupportIconPropertiesAction"

} // end class "HMVSelectTool"
