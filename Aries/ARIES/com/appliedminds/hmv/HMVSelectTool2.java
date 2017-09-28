package com.appliedminds.hmv;

import com.appliedminds.martini.*;
import com.appliedminds.martinix.ui.*;
import com.appliedminds.martinix.fader.*;
import com.appliedminds.martinix.motion.Motion;

import java.awt.Cursor;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.event.*;
import java.awt.geom.Rectangle2D;
import java.awt.geom.Point2D;
import java.awt.geom.Ellipse2D;
import java.io.*;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.*;
import javax.swing.*;
import javax.swing.event.*;
import java.util.logging.Logger;

/**
 * The select tool for the HMV.
 *
 * @author daepark@apmindsf.com
 */
public class HMVSelectTool2 extends HMVTool
{
  public final SelectToolState STATE_IDLE =
    new SelectToolStateIdle();
  public final SelectToolState STATE_HOVER_NODE =
    new SelectToolStateHoverNode();
  public final SelectToolState STATE_SELECTED_NODE =
    new SelectToolStateSelectedNode();
  public final SelectToolState STATE_EDIT_NODE =
    new SelectToolStateEditNode();
  public final SelectToolState STATE_DRAG_NODE =
    new SelectToolStateDragNode();
  public final SelectToolState STATE_CREATE_EDGE =
    new SelectToolStateCreateEdge();
  public final SelectToolState STATE_SELECTED_EDGE =
    new SelectToolStateSelectedEdge();
  public final SelectToolState STATE_NODE_SLIDER_EDIT =
    new SelectToolStateNodeSliderEdit();
  public final SelectToolState STATE_EDGE_SLIDER_EDIT =
    new SelectToolStateEdgeSliderEdit();

  private static final String ORIG_X = "origX";
  private static final String ORIG_Y = "origY";


  private SelectToolState _currentState;

  private ArrayList _cpath = new ArrayList();

  // to avoid excess instantiations
  private Object[] _objArray = new Object[2]; // current max number of args

  // list of GraphChangeListeners
  private EventListenerList _listenerList = new EventListenerList();

  private HMVSelectionManager _selectionManager;


  public HMVSelectTool2(Cursor cursor, HMV app) {
    super(cursor, app);
    _selectionManager = app.getSelectionManager();
    setState(STATE_IDLE, null);
  }

  public void addGraphChangeListener(GraphChangeListener l) {
    _listenerList.add(GraphChangeListener.class, l);
  }

  public void removeGraphChangeListener(GraphChangeListener l) {
    _listenerList.remove(GraphChangeListener.class, l);
  }


  /**
   * Notify registered GraphChangeListener objects that the graph has
   * changed in response to this tool (i.e, new node, new edge, node
   * moved, etc.).
   */
  protected void fireGraphChanged() {
    // Guaranteed to return a non-null array
    Object[] listeners = _listenerList.getListenerList();
    // Process the listeners last to first, notifying
    // those that are interested in this event
    for (int i = listeners.length-2; i>=0; i-=2) {
      if (listeners[i] == GraphChangeListener.class) {
        // Lazily create the event:
        ((GraphChangeListener)listeners[i+1]).graphChanged();
      }
    }
  }

  /**
   ** WARNING: We should do our best NOT to mix the AWT mouse event
   ** and DrawableGraphMouseEvent handlers. The following two methods,
   ** mouseDragged and mouseReleased, are exceptions for moving nodes.
   **/
  public void mouseDragged(MouseEvent e) {
    _currentState.mouseDragged(e);
  }

  public void mouseReleased(MouseEvent e) {
    _currentState.mouseReleased(e);
  }

  protected void setState(SelectToolState state, Object[] args) {
    _currentState = state;
    _currentState.init(args);
  }

  protected void addSelectedNode(DrawableNode node) {
    _selectionManager.setSelected(node, true);
  }

  protected void clearSelectedNodes() {
    _selectionManager.clearAllSelections();
  }

  protected boolean elementIsSelectable(DrawableGraphElement element) {
    return (element.getProperty(DrawableEdge.PROPERTY_FAUX) == null);
  }


  /**
   * Handle mouse event on a node.
   */
  public boolean handleNodeMouseEvent(DrawableGraphMouseEvent e) {
    return (_currentState.handleNodeMouseEvent(e));
  }

  /**
   * Handle mouse event on a node text.
   */
  public boolean handleNodeTextMouseEvent(DrawableGraphMouseEvent e) {
    return (_currentState.handleNodeTextMouseEvent(e));
  }

  /**
   * handles mouse events on the slider value label
   */
  public boolean handleSliderValueMouseEvent(DrawableGraphMouseEvent e) {
    if (isMouseClicked(e))
    {
      selectAndHighlight(e.getGraphElement());
      _objArray[0] = e.getGraphElement();

      if (e.getGraphElement() instanceof DrawableNode) {
        setState(STATE_SELECTED_NODE, _objArray);
      }
      else {
        if (elementIsSelectable(e.getGraphElement()))
          setState(STATE_SELECTED_EDGE, _objArray);
        else
          return true;
      }
    }
    return (_currentState.handleSliderValueMouseEvent(e));
  }

  public boolean handleSliderToggleMouseEvent(DrawableGraphMouseEvent e) {
    return (_currentState.handleSliderToggleMouseEvent(e));
  }

  public boolean handleEdgeMouseEvent(DrawableGraphMouseEvent e) {
    return (_currentState.handleEdgeMouseEvent(e));
  }

  public boolean handleEdgeBubbleMouseEvent(DrawableGraphMouseEvent e) {
    return (_currentState.handleEdgeBubbleMouseEvent(e));
  }

  public boolean handleSliderKnobMouseEvent(DrawableGraphMouseEvent e) {
    return (_currentState.handleSliderKnobMouseEvent(e));
  }

  public boolean handleHitTestMissedMouseEvent(DrawableGraphMouseEvent e) {
    return (_currentState.handleHitTestMissedMouseEvent(e));
  }

  public void keyReleased(KeyEvent e) {
    _currentState.keyReleased(e);
  }



  /**
   * dummy SelectToolState
   */
  abstract class SelectToolState
  {
    private JPopupMenu __idlePopupMenu = null;
    private JPopupMenu __elementPopupMenu = null;
    private NewNodeCommand __newNodeCmd = null;
    private DeleteSelectionCommand __deleteCmd = null;
    private JMenuItem __simItem = null;
    /*
    private JMenuItem __decelTestItem = null;
    private JMenuItem __bounceTestItem = null;
    */

    public void init(Object[] args) {}

    public void mouseDragged(MouseEvent e) { }

    public void mouseReleased(MouseEvent e) { }

    public boolean handleNodeMouseEvent(DrawableGraphMouseEvent e) {
      return (false);
    }

    public boolean handleNodeTextMouseEvent(DrawableGraphMouseEvent e) {
      return (this.handleNodeMouseEvent(e));
    }

    public boolean handleHitTestMissedMouseEvent(DrawableGraphMouseEvent e) {
      return (false);
    }

    public boolean handleSliderKnobMouseEvent(DrawableGraphMouseEvent e) {
      return (false);
    }

    public boolean handleSliderValueMouseEvent(DrawableGraphMouseEvent e) {
      return (false);
    }

    public boolean handleSliderToggleMouseEvent(DrawableGraphMouseEvent e) {
      return (false);
    }

    public boolean handleEdgeBubbleMouseEvent(DrawableGraphMouseEvent e) {
      return (false);
    }

    public boolean handleEdgeMouseEvent(DrawableGraphMouseEvent e) {
      return (false);
    }

    public void keyReleased(KeyEvent e) {
      // default behavior does nothing
    }

    protected void showIdlePopupMenu(MouseEvent me)
    {
      if (__idlePopupMenu == null) {
        __idlePopupMenu = new JPopupMenu();
        JMenuItem menuItem = new JMenuItem("Create new node...");
        __newNodeCmd = new NewNodeCommand(HMVSelectTool2.this,
                                   me.getPoint());
        menuItem.addActionListener(new HMVCommandAction(__newNodeCmd));
        __idlePopupMenu.add(menuItem);
      }
      else {
        __newNodeCmd.setPosition(me.getPoint());
      }

      __idlePopupMenu.show(me.getComponent(), me.getX(), me.getY());
    }

    protected void showNodePopupMenu(DrawableGraphMouseEvent e) {
      showElementPopupMenu(e, true);
    }

    protected void showEdgePopupMenu(DrawableGraphMouseEvent e) {
      showElementPopupMenu(e, false);
    }

    protected void showElementPopupMenu(DrawableGraphMouseEvent e,
                                        boolean isNode)
    {
      // disable the popup menu during animation
      if (getApp()._simCommand.isAnimating()) {
        return;
      }

      MouseEvent me = e.getMouseEvent();
      if (__elementPopupMenu == null)
      {
        __elementPopupMenu = new JPopupMenu();

        JMenuItem deleteItem = new JMenuItem("Delete");
        __deleteCmd =
          new DeleteSelectionCommand(HMVSelectTool2.this,
                                     _selectionManager.getSelectedNodes(),
                                     _selectionManager.getSelectedEdges());
        deleteItem.addActionListener(new HMVCommandAction(__deleteCmd));
        __elementPopupMenu.add(deleteItem);

        __simItem = new JMenuItem("Set sim file...");
        SetSimFileCommand simCmd =  new SetSimFileCommand(HMVSelectTool2.this);
        __simItem.addActionListener(new HMVCommandAction(simCmd));
        __elementPopupMenu.add(__simItem);

        // Adds the ability to directly edit the properties of a given element.
        JMenuItem PropsItem = new JMenuItem("Properties");
        PropsItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) 
            {
                VisualKB.gui.ElementProps ep = new VisualKB.gui.ElementProps();
                ep.setElement((DrawableGraphElement)_selectionManager.getSelectedElements().next()); //need to figure out how to pass the element.
                ep.show();
            }
        });
        __elementPopupMenu.add(PropsItem);
        
        /*
        __decelTestItem = new JMenuItem("Decelerate Test");
        __decelTestItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e)
            {
              decelTest();
            }
          });
        __elementPopupMenu.add(__decelTestItem);

        __bounceTestItem = new JMenuItem("Bounce Test");
        __bounceTestItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e)
            {
              bounceTest();
            }
          });
        __elementPopupMenu.add(__bounceTestItem);
        */
      }
      else {
        __deleteCmd.setNodesToDelete(_selectionManager.getSelectedNodes());
        __deleteCmd.setEdgesToDelete(_selectionManager.getSelectedEdges());
      }

      if (isNode)
      {
        __simItem.setVisible(true);
        /*
        __decelTestItem.setVisible(true);
        __bounceTestItem.setVisible(true);
        */
      }
      else
      {
        __simItem.setVisible(false);
        /*
        __decelTestItem.setVisible(false);
        __bounceTestItem.setVisible(false);
        */
      }

      __elementPopupMenu.show(me.getComponent(), me.getX(), me.getY());
    }

  } // end class "SelectToolState"


  /**
   * STATE_IDLE
   */
  private class SelectToolStateIdle extends SelectToolState {

    public void init(Object[] args) {
    }

    public boolean handleNodeMouseEvent(final DrawableGraphMouseEvent e) {
      MouseEvent me = e.getMouseEvent();
      DrawableNode node = (DrawableNode)e.getGraphElement();
      _objArray[0] = node;

      if (isMousePressed(e) && !_selectionManager.isSelected(node)) {
        // go directly to selected node
        setState(STATE_SELECTED_NODE, _objArray);
        _currentState.handleNodeMouseEvent(e);
      }
      else if (isMouseEntered(e) && !_selectionManager.isSelected(node)) {
        setState(STATE_HOVER_NODE, _objArray);
        _currentState.handleNodeMouseEvent(e);
      }

      return (true);
    }

    public boolean handleEdgeMouseEvent(DrawableGraphMouseEvent e) {
      if (isMousePressed(e))
      {
        if (elementIsSelectable(e.getGraphElement())) {
          _objArray[0] = e.getGraphElement();
          setState(STATE_SELECTED_EDGE, _objArray);
          return (_currentState.handleEdgeMouseEvent(e));
        }
        else
          return true;
      }
      return (false);
    }

    public boolean handleEdgeBubbleMouseEvent(DrawableGraphMouseEvent e) {
      if (isMousePressed(e))
      {
        if (elementIsSelectable(e.getGraphElement())) {
          _objArray[0] = e.getGraphElement();
          setState(STATE_SELECTED_EDGE, _objArray);
          return (_currentState.handleEdgeMouseEvent(e));
        }
        else
          return true;
      }
      return (false);
    }

    public boolean handleSliderKnobMouseEvent(DrawableGraphMouseEvent e) {
      if (isMousePressed(e))
      {
        DrawableGraphElement el = e.getGraphElement();
        _objArray[0] = el;
        if (el instanceof DrawableNode)
        {
          setState(STATE_SELECTED_NODE, _objArray);
        }
        else {
          if (elementIsSelectable(e.getGraphElement())) {
            setState(STATE_SELECTED_EDGE, _objArray);
            return (_currentState.handleEdgeMouseEvent(e));
          }
          else
            return true;
        }
        return (_currentState.handleSliderKnobMouseEvent(e));
      }
      return (false);
    }

    public boolean handleHitTestMissedMouseEvent(DrawableGraphMouseEvent e) {
      if (isMousePressed(e) || e.getMouseEvent().isPopupTrigger())
      {
        _selectionManager.clearAllSelections();
        refreshGraph();

        if (e.getMouseEvent().isPopupTrigger())
          showIdlePopupMenu(e.getMouseEvent());
      }
      return (true);
    }

    public void keyReleased(KeyEvent e) {
      if (!_app.getTextField().isVisible() &&
          (e.getKeyCode() == KeyEvent.VK_DELETE ||
           e.getKeyCode() == KeyEvent.VK_BACK_SPACE))
      {
        if (_selectionManager.selectedNodesCount() > 0 ||
            _selectionManager.selectedEdgesCount() > 0) {
          DeleteSelectionCommand cmd =
            new DeleteSelectionCommand(HMVSelectTool2.this,
                                       _selectionManager.getSelectedNodes(),
                                       _selectionManager.getSelectedEdges());
          cmd.execute();
        }
      }
    }
  } // end class "SelectToolStateIdle"


  /**
   * STATE_HOVER_NODE
   */
  private class SelectToolStateHoverNode extends SelectToolState {

    private DrawableNode __currentNode;

    /**
     * @param args args[0] is a DrawableNode
     */
    public void init(Object[] args) {
      __currentNode = (DrawableNode)args[0];

    }

    public boolean handleNodeMouseEvent(DrawableGraphMouseEvent e) {
      MouseEvent me = e.getMouseEvent();
      Object context = e.getContext();

      if (isMouseEntered(e)) {
        // show edge connector hubs
        FaderUtil.setConnectorHubsVisible(__currentNode, true);

        // show node slider toggle arrow
        FaderUtil.setSliderToggleVisible(__currentNode, true);

        refreshGraph();
        return (true);
      }
      else if (isMousePressed(e)) {
        if (context instanceof EdgeConnectorHub)
        {
          _objArray[0] = e.getGraphElement();
          _objArray[1] = context;
          setState(STATE_CREATE_EDGE, _objArray);
          return (_currentState.handleNodeMouseEvent(e));
        }
        else if (context instanceof DrawableNode ||
                 context instanceof TextScreenData)
        {
          _objArray[0] = e.getGraphElement();
          setState(STATE_SELECTED_NODE, _objArray);
          return (_currentState.handleNodeMouseEvent(e));
        }
      }
      else if (isMouseExited(e)) {
        if (!(context instanceof TextScreenData)) {
          // hide connector hubs
          FaderUtil.setConnectorHubsVisible(__currentNode, false);

          // hide slider toggle
          FaderUtil.setSliderToggleVisible(__currentNode, false);

          refreshGraph();

          setState(STATE_IDLE, null);
          return (true);
        }
      }

      return (false);
    }

  } // end class "SelectToolStateHoverNode"


  /**
   * STATE_SELECTED_NODE
   */
  private class SelectToolStateSelectedNode extends SelectToolState {

    private DrawableNode __currentNode;
    private Point __lastPoint;

    /**
     * @param args args[0] is a DrawableNode
     */
    public void init(Object[] args) {
      __currentNode = (DrawableNode)args[0];
    }

    public void mouseDragged(MouseEvent e) {
      _objArray[0] = __lastPoint;
      _objArray[1] = __currentNode;
      setState(STATE_DRAG_NODE, _objArray);
      _currentState.mouseDragged(e);
    }

    public boolean handleNodeMouseEvent(DrawableGraphMouseEvent e)
    {
      Object context = e.getContext();
      if (isMousePressed(e)) {
        MouseEvent me = e.getMouseEvent();
        __lastPoint = me.getPoint();

        if (me.isShiftDown())
          _selectionManager.setSelected(__currentNode, true);
        else if (me.isControlDown())
          _selectionManager.toggleSelection(__currentNode);
        else {
          _selectionManager.clearAllSelections();
          _selectionManager.setSelected(__currentNode, true);
        }

        if (me.isPopupTrigger())
          showNodePopupMenu(e);
        else if (context instanceof EdgeConnectorHub)
        {
          // we're creating an edge
          _objArray[0] = e.getGraphElement();
          _objArray[1] = context;
          setState(STATE_CREATE_EDGE, _objArray);
          return (_currentState.handleNodeMouseEvent(e));
        }

        _app.endSliderValueEdit(); // end any slider edit
        refreshGraph();
      }

      else if (isMouseEntered(e)) {
        if (e.getGraphElement() != __currentNode) {
          _objArray[0] = e.getGraphElement();
          setState(STATE_HOVER_NODE, _objArray);
          _currentState.handleNodeMouseEvent(e);
        }
      }

      else if (isMouseReleased(e)) {
        // for Windows: isPopupTrigger only works in mouseReleased
        if (e.getMouseEvent().isPopupTrigger())
          showNodePopupMenu(e);
      }

      else if (isMouseClicked(e)) {
        if (e.getMouseEvent().getClickCount() == 2) {
          if (__currentNode.isCollapsed())
          {
            NodeList descendants = __currentNode.expandNode();
            removeFauxEdges(__currentNode);
            expandAnimation(__currentNode, descendants);
          }
          else
          {
            NodeList descendants = __currentNode.collapseNode();
            createFauxEdges(__currentNode, descendants);
            collapseAnimation(__currentNode, descendants);
          }
          _app.endSliderValueEdit(); // end any slider edit
          refreshGraph();
          fireGraphChanged();
        }
      }

      return (true);
    }

    public boolean handleEdgeMouseEvent(DrawableGraphMouseEvent e) {
      if (isMousePressed(e))
      {
        if (elementIsSelectable(e.getGraphElement())) {
          _objArray[0] = e.getGraphElement();
          setState(STATE_SELECTED_EDGE, _objArray);
          return (_currentState.handleEdgeMouseEvent(e));
        }
        else
          return true;
      }
      return (false);
    }

    public boolean handleNodeTextMouseEvent(DrawableGraphMouseEvent e)
    {
      MouseEvent me = e.getMouseEvent();

      if (isMousePressed(e)) {
        __lastPoint = me.getPoint();
        _app.endSliderValueEdit();
      }

      if (SwingUtilities.isLeftMouseButton(me) &&
          (me.getClickCount() == 1) &&
          (me.getID() == MouseEvent.MOUSE_CLICKED))
      {
        TextScreenData nodeLabelData = (TextScreenData) e.getContext();
        _objArray[0] = e.getGraphElement();
        _objArray[1] = nodeLabelData.getBounds();
        setState(STATE_EDIT_NODE, _objArray);
      }
      return (true);
    }

    public boolean handleHitTestMissedMouseEvent(DrawableGraphMouseEvent e)
    {
      _app.endSliderValueEdit();
      setState(STATE_IDLE, null);
      return (_currentState.handleHitTestMissedMouseEvent(e));
    }

    public boolean handleSliderKnobMouseEvent(DrawableGraphMouseEvent e)
    {
      if (isMousePressed(e))
      {
        DrawableGraphElement el = e.getGraphElement();
        if (el instanceof DrawableNode)
        {
          __currentNode = (DrawableNode) e.getGraphElement();
          selectAndHighlight(__currentNode);
          _objArray[0] = __currentNode;
          setState(STATE_NODE_SLIDER_EDIT, _objArray);
          return (_currentState.handleSliderKnobMouseEvent(e));
        }
        else {
          // goto edge slider edit state
          _objArray[0] = el;
          setState(STATE_EDGE_SLIDER_EDIT, _objArray);
          return (_currentState.handleSliderKnobMouseEvent(e));
        }
      }
      else if (isMouseReleased(e)) {
        getGraphPanel().stopElementEdit();
      }

      return (false);
    }

    public boolean handleSliderValueMouseEvent(DrawableGraphMouseEvent e)
    {
      MouseEvent me = e.getMouseEvent();
      if (SwingUtilities.isLeftMouseButton(me) &&
          (me.getClickCount() == 1) &&
          (me.getID() == MouseEvent.MOUSE_CLICKED))
      {
        _app.editSliderValue(e.getGraphElement(),
                             (Rectangle2D) e.getContext());
        return (true);
      }
      else
      {
        return (false);
      }
    }

    public boolean handleSliderToggleMouseEvent(DrawableGraphMouseEvent e)
    {
      MouseEvent me = e.getMouseEvent();
      if (isMouseClicked(e)) {
        boolean visible = FaderUtil.isSliderVisible(__currentNode);
        FaderUtil.setSliderVisible(__currentNode, !visible);
        _app.endSliderValueEdit(); // end any slider value edit
        refreshGraph();

        return (true);
      }
      else {
        return (false);
      }
    }

    public boolean handleEdgeBubbleMouseEvent(DrawableGraphMouseEvent e) {
      if (isMousePressed(e))
      {
        if (elementIsSelectable(e.getGraphElement())) {
          _objArray[0] = e.getGraphElement();
          setState(STATE_SELECTED_EDGE, _objArray);
          return (_currentState.handleEdgeMouseEvent(e));
        }
        else
          return true;
      }
      return (false);
    }

    public void keyReleased(KeyEvent e) {
      if (!_app.getTextField().isVisible() &&
          (e.getKeyCode() == KeyEvent.VK_DELETE ||
           e.getKeyCode() == KeyEvent.VK_BACK_SPACE))
      {
        if (_selectionManager.selectedNodesCount() > 0 ||
            _selectionManager.selectedEdgesCount() > 0) {
          DeleteSelectionCommand cmd =
            new DeleteSelectionCommand(HMVSelectTool2.this,
                                       _selectionManager.getSelectedNodes(),
                                       _selectionManager.getSelectedEdges());
          cmd.execute();
          setState(STATE_IDLE, null);
        }
      }
    }

    /**
     * Creates "faux" edges to replace edges between visible descendants of
     * a collapsed node and hidden descendants. The faux edge goes between the
     * visible descendant and the collapsed node
     *
     * @param collapsedNode the target node
     * @param hidden the nodes that were hidden by the collapse operation
     */
    private void createFauxEdges(DrawableNode collapsedNode, NodeList hidden) {
      DrawableGraph graph = _app.getGraphPanel().getDrawableGraph();
      for (NodeIterator i = hidden.iterator(); i.hasNext(); ) {
        DrawableNode n = i.next();
        for (EdgeIterator ei = n.getIncomingEdges(); ei.hasNext(); ) {
          DrawableEdge e = ei.next();
          if (!hidden.contains(e.getTail())) {
            // the tail is visible but the head is not - make a faux edge
            DrawableEdge newEdge = graph.createEdge(collapsedNode, e.getTail());
            newEdge.setProperty(DrawableEdge.PROPERTY_FAUX, "true");
          }
        }
      }
    }

    /**
     * Removes any faux edges attached to the given node
     *
     * @param node the target node
     */
    private void removeFauxEdges(DrawableNode node) {
      DrawableGraph graph = _app.getGraphPanel().getDrawableGraph();
      for (EdgeIterator ei = node.getIncomingEdges(); ei.hasNext(); ) {
        DrawableEdge e = ei.next();
        if (e.getProperty(DrawableEdge.PROPERTY_FAUX) != null)
          graph.remove(e);
      }
    }

  } // end class "SelectToolStateSelectedNode"


  private class SelectToolStateSelectedEdge extends SelectToolState
  {
    private DrawableEdge __edge;

    public void init(Object[] args) {
      __edge = (DrawableEdge) args[0];
    }

    public boolean handleNodeMouseEvent(DrawableGraphMouseEvent e)
    {
      if (isMousePressed(e)) {
        DrawableGraphElement el = e.getGraphElement();
        _objArray[0] = el;
        setState(STATE_SELECTED_NODE, _objArray);
        return (_currentState.handleNodeMouseEvent(e));
      }

      return false;
    }

    public boolean handleEdgeMouseEvent(DrawableGraphMouseEvent e)
    {
      if (isMousePressed(e)) {
        __edge = (DrawableEdge) e.getGraphElement();
        if (!elementIsSelectable(__edge))
          return true;
        MouseEvent me = e.getMouseEvent();

        if (me.isShiftDown())
          _selectionManager.setSelected(__edge, true);
        else if (me.isControlDown())
          _selectionManager.toggleSelection(__edge);
        else {
          _selectionManager.clearAllSelections();
          _selectionManager.setSelected(__edge, true);
        }

        if (e.getMouseEvent().isPopupTrigger())
          showEdgePopupMenu(e);

        _app.endSliderValueEdit(); // end any slider edit
        refreshGraph();

        return (true);
      }

      else if (isMouseReleased(e)) {
        // for Windows: isPopupTrigger only works in mouseReleased
        if (e.getMouseEvent().isPopupTrigger())
          showEdgePopupMenu(e);
      }

      return (false);
    }

    public boolean handleEdgeBubbleMouseEvent(DrawableGraphMouseEvent e)
    {
      if (isMousePressed(e)) {
        __edge = (DrawableEdge) e.getGraphElement();
        MouseEvent me = e.getMouseEvent();
        FaderUtil.setSliderVisible(__edge,
                                   !FaderUtil.isSliderVisible(__edge));

        if (me.isShiftDown())
          _selectionManager.setSelected(__edge, true);
        else if (me.isControlDown())
          _selectionManager.toggleSelection(__edge);
        else {
          _selectionManager.clearAllSelections();
          _selectionManager.setSelected(__edge, true);
        }

        _app.endSliderValueEdit();
        refreshGraph();
        return (true);
      }
      return (false);
    }

    public boolean handleSliderKnobMouseEvent(DrawableGraphMouseEvent e)
    {
      if (isMousePressed(e))
      {
        DrawableGraphElement el = e.getGraphElement();
        _objArray[0] = el;
        if (el instanceof DrawableNode)
        {
          setState(STATE_SELECTED_NODE, _objArray);
        }
        else {
          setState(STATE_EDGE_SLIDER_EDIT, _objArray);
        }
        return (_currentState.handleSliderKnobMouseEvent(e));
      }
      return (false);
    }

    public boolean handleSliderValueMouseEvent(DrawableGraphMouseEvent e)
    {
      MouseEvent me = e.getMouseEvent();
      if (SwingUtilities.isLeftMouseButton(me) &&
          (me.getClickCount() == 1) &&
          (me.getID() == MouseEvent.MOUSE_CLICKED))
      {
        _app.editSliderValue(e.getGraphElement(),
                             (Rectangle2D) e.getContext());
        return (true);
      }
      else
      {
        return (false);
      }
    }

    public boolean handleHitTestMissedMouseEvent(DrawableGraphMouseEvent e)
    {
      _app.endSliderValueEdit();
      setState(STATE_IDLE, null);
      return (_currentState.handleHitTestMissedMouseEvent(e));
    }

    public void keyReleased(KeyEvent e) {
      if (!getApp().getTextField().isVisible() &&
          (e.getKeyCode() == KeyEvent.VK_DELETE ||
           e.getKeyCode() == KeyEvent.VK_BACK_SPACE))
      {
        if (_selectionManager.selectedNodesCount() > 0 ||
            _selectionManager.selectedEdgesCount() > 0) {
          DeleteSelectionCommand cmd =
            new DeleteSelectionCommand(HMVSelectTool2.this,
                                       _selectionManager.getSelectedNodes(),
                                       _selectionManager.getSelectedEdges());
          cmd.execute();
          setState(STATE_IDLE, null);
        }
      }
    }

  } // end class SelectToolStateSelectedEdge


  /**
   * STATE_CREATE_EDGE
   */
  private class SelectToolStateCreateEdge extends SelectToolState
  {
    private final String SUPPORTS             = "supports";
    private final String REFUTES              = "refutes";
    private final int    DEFAULT_SLIDER_VALUE = 0;

    private final Color rubberBandColor = new Color(0x35, 0x7C, 0xE8);
    private final float dash1[] = {5.0f};
    private final Stroke rubberBandStroke =
      new BasicStroke(2.0f,
                      BasicStroke.CAP_BUTT,
                      BasicStroke.JOIN_MITER,
                      10.0f,
                      dash1,
                      0.0f);

    private DrawableNode __sourceNode;
    private DrawableNode __targetNode;
    private EdgeConnectorHub __sourceHub;
    private Point2D __toPt = null;
    private Point2D __fromPt = null;


    /**
     * @param args args[0] is a DrawableNode
     *             args[1] is a EdgeConnectorHub (to be created)
     */
    public void init(Object[] args)
    {
      __sourceNode = (DrawableNode)args[0];
      __sourceHub = (EdgeConnectorHub)args[1];
      __toPt = null;
      __fromPt = __sourceHub.getPoint();
      __targetNode = null;
    }

    public boolean handleNodeMouseEvent(DrawableGraphMouseEvent e)
    {
      DrawableGraphElement elt = e.getGraphElement();
      Object context = e.getContext();
      MouseEvent me = e.getMouseEvent();
      if (isMousePressed(e)) {
        FaderUtil.setCreatingEdge(__sourceNode, true);
        FaderUtil.setEdgeConnectorHubSelected(__sourceNode,
                                              __sourceHub.getConnection(),
                                              true);
        refreshGraph();
      }

      if (elt != __sourceNode) {
        if (!(context instanceof TextScreenData)) {
          if (isMouseEntered(e)) {
            __targetNode = (DrawableNode)elt;
            FaderUtil.setCreatingEdge(__targetNode, true);
            refreshGraph();

            __fromPt = null;
            __toPt = null;
          }
          else if (isMouseExited(e)) {
            if (__targetNode != null) {
              FaderUtil.setCreatingEdge(__targetNode, false);
              refreshGraph();
              __targetNode = null;
            }
            __toPt = null;
            __fromPt = null;
          }
        }

        if (isMouseDragged(e)) {
          if (__fromPt != null) {
            if (__toPt != null) {
              // erase the last marquee line
              rubberBand(__fromPt, __toPt);
            }

            __toPt = new Point2D.Double(me.getX(), me.getY());
            rubberBand(__fromPt, __toPt);
          }
          else {
            __fromPt = __sourceHub.getPoint();
          }
        }

        else if (isMouseReleased(e)) {
          if (__targetNode != null) {
            if (GraphUtil.willCreateCyclicGraph(getGraphPanel().getDrawableGraph(),
                                                __sourceNode, __targetNode))
            {
              // don't let them create cyclic graphs
              //
              Logger.getLogger(_app.getLogName()).warning("Can not create cyclic graphs");
              JOptionPane.showMessageDialog(getApp(), "Can not create cyclic graphs");
            }
            else if (findEdge(__targetNode, __sourceNode) != null) {
              // no multiple edges allowed
              //
              Logger.getLogger(_app.getLogName()).warning("Can not create duplicate edges");
              JOptionPane.showMessageDialog(getApp(), "Can not create duplicate edges");
            }
            else {
              DrawableEdge newEdge = createEdge(__targetNode, __sourceNode);

              // set up edge properties
              FaderUtil.setSliderValue(newEdge, DEFAULT_SLIDER_VALUE);
              FaderUtil.setSliderVisible(newEdge, false);

              // recalculate propbabilities from head node
              HMVMath.initCalculationPath2(getGraphPanel().getDrawableGraph(),
                                           __sourceNode,
                                           _cpath,
                                           true);
              HMVMath.recalculateProbabilityUsingPath2(null, _cpath,
                                                       getGraphPanel());

              DrawableGraphContext ctx =
                getGraphPanel().getDrawableGraphContext();
              ctx.setNeedsRepaint(__sourceNode, true);
              ctx.setNeedsRepaint(newEdge, true);
              ctx.setNeedsRepaint(__targetNode, true);
              getGraphPanel().repaint();

              fireGraphChanged();
            }
            this.handleHitTestMissedMouseEvent(e);
          }
        }
      }
      return (true);
    }

    public void mouseDragged(MouseEvent e) {
      if (__targetNode == null) {
        if (__fromPt != null) {
          if (__toPt != null) {
            // erase the last marquee line
            rubberBand(__fromPt, __toPt);
          }

          __toPt = new Point2D.Double(e.getX(), e.getY());
          rubberBand(__fromPt, __toPt);
        }
        else {
          __fromPt = __sourceHub.getPoint();
        }
      }
    }

    public boolean handleHitTestMissedMouseEvent(DrawableGraphMouseEvent e) {
      if (isMouseReleased(e)) {
        if (__targetNode != null) {
          clearCreatingEdgeNode(__targetNode);
          __targetNode = null;
        }

        Point2D fromPt = __sourceHub.getPoint();
        if (__toPt != null) {
          rubberBand(fromPt, __toPt);
        }

        clearCreatingEdgeNode(__sourceNode);
        FaderUtil.setEdgeConnectorHubSelected(__sourceNode,
                                              __sourceHub.getConnection(),
                                              false);
        refreshGraph();
        setState(STATE_IDLE, null);
        return (_currentState.handleHitTestMissedMouseEvent(e));
      }
      return (true);
    }

    private void clearCreatingEdgeNode(DrawableNode node) {
      FaderUtil.setCreatingEdge(node, false);

      if (!FaderUtil.isSelected(node)) {
        FaderUtil.setConnectorHubsVisible(node, false);
        FaderUtil.setSliderToggleVisible(node, false);
      }
    }

    private void rubberBand(Point2D from, Point2D to) {
      Graphics2D g = null;
      try {
        g = getGraphics();

        g.setColor(rubberBandColor);
        g.setStroke(rubberBandStroke);

        g.setXORMode(getGraphPanel().getBackground());
        g.drawLine((int)from.getX(), (int)from.getY(),
                   (int)to.getX(), (int)to.getY());

        // connector hub draw props
        double d =
          getGraphPanel().mapWorldToViewport(_app.getFaderUIPrefs().getConnectorHubDiameter());
        double r = d / 2;
        DrawProps drawProps = _app.getFaderUIPrefs().getEdgeConnectorHubDrawProps(true, false, false);
        Ellipse2D hub = new Ellipse2D.Double(to.getX() - r,
                                             to.getY() - r,
                                             d, d);
        g.setStroke(new BasicStroke((float)drawProps.getStrokeWidth()));
        g.setColor(drawProps.getFillColor().getColor());
        g.fill(hub);
        g.setColor(drawProps.getStrokeColor().getColor());
        g.draw(hub);
      }
      finally {
        g.dispose();
      }
    }

    /**
     * Create an edge in the exising graph. Set TMV specific properties
     * and handle incorporating the new edge into any existing edge.
     *
     * @param the target node (head node)
     * @param the source node (tail node)
     */
    private DrawableEdge createEdge(DrawableNode toNode,
                                    DrawableNode fromNode)
    {
      DrawableGraph graph = getGraphPanel().getDrawableGraph();
      return (graph.createEdge(toNode, fromNode));
    }


    /**
     * Find an edge between the two given nodes, if any. This assume
     * a "collapsed" graph, where there can only be at most one edge
     * between any two nodes.
     *
     * @param node1
     * @param node2
     * @return an edge between node1 and node2 if any, or null if none.
     */
    private DrawableEdge findEdge(DrawableNode node1, DrawableNode node2)
    {
      DrawableGraph graph = getGraphPanel().getDrawableGraph();

      // using edges for now, but might be more efficient to iterate
      // through nodes, (depending on the graph).
      for (EdgeIterator it=graph.edgesIterator(); it.hasNext();) {
        DrawableEdge edge = it.next();
        DrawableNode head = edge.getHead();
        DrawableNode tail = edge.getTail();
        if (((node1 == head) && (node2 == tail)) ||
            ((node2 == head) && (node1 == tail))) {
          return (edge);
        }
      }

      return (null);
    }

  } // end class "SelectToolStateCreateEdge"


  /**
   * STATE_EDIT_NODE
   */
  private class SelectToolStateEditNode extends SelectToolState {

    private DrawableNode __currentNode;
    private Rectangle2D __textRect;

    /**
     * @param args args[0] is a DrawableNode
     *             args[1] is a TextScreenData
     */
    public void init(Object[] args) {
      __currentNode = (DrawableNode) args[0];
      __textRect = (Rectangle2D) args[1];
      _app.editNodeLabel(__currentNode, __textRect);
    }

    public boolean handleNodeMouseEvent(DrawableGraphMouseEvent e) {
      if (isMousePressed(e))
      {
        _app.endNodeLabelEdit();
        _objArray[0] = e.getGraphElement();
        setState(STATE_SELECTED_NODE, _objArray);
        return (_currentState.handleNodeMouseEvent(e));
      }
      return (true);
    }

    public boolean handleHitTestMissedMouseEvent(DrawableGraphMouseEvent e) {
      _app.endNodeLabelEdit();
      setState(STATE_IDLE, null);
      return (_currentState.handleHitTestMissedMouseEvent(e));
    }

  } // end class "SelectToolStateEditNode"


  /**
   * STATE_DRAG_EDGE
   */
  private class SelectToolStateDragNode extends SelectToolState {

    private DrawableNode __draggedNode; // the primary node that's being dragged
    private Point __lastPoint;

    /**
     * @param args args[0] is a Point (starting drag position)
     *             args[1] is a DrawableNode (the primary node being dragged)
     */
    public void init(Object[] args) {
      __lastPoint = (Point)args[0];
      __draggedNode = (DrawableNode)args[1];
      Collection c = new HashSet();
      for (Iterator i = _selectionManager.getSelectedNodes(); i.hasNext(); )
        c.add(i.next());

      getGraphPanel().startDrag(c);
    }

    public void mouseDragged(MouseEvent e) {
      if (getGraphPanel().getInDrag()) {
        // first compute how much we moved
        Point curPoint = e.getPoint();
        int dx = (curPoint.x - __lastPoint.x);
        int dy = (curPoint.y - __lastPoint.y);

        getGraphPanel().dragTo(dx, dy);
        __lastPoint = curPoint;

        fireGraphChanged();
      }
    }

    public void mouseReleased(MouseEvent e) {
      __lastPoint = null;
      if (getGraphPanel().getInDrag()) {
        getGraphPanel().finishDrag();
      }
      _objArray[0] = __draggedNode;
      setState(STATE_SELECTED_NODE, _objArray);

      fireGraphChanged();
    }

  } // end class "SelectToolStateDragNode"


  private class SelectToolStateNodeSliderEdit extends SelectToolState
  {
    private DrawableNode __currentNode;

    public void init(Object[] args) {
      __currentNode = (DrawableNode) args[0];
    }

    public boolean handleSliderKnobMouseEvent(DrawableGraphMouseEvent e)
    {
      if (isMousePressed(e))
      {
        HMVMath.initCalculationPath2(getGraphPanel().getDrawableGraph(),
                                     __currentNode,
                                     _cpath,
                                     false);
        getGraphPanel().startElementEdit(__currentNode,
                                         getSubGraph(__currentNode),
                                         e.getMouseEvent().getPoint(),
                                         e.getContext(),
                                         true);

        if (!_app.getRealTimeSliderMode()) {
          hideAutomaticSliders(__currentNode);
        }
        return (true);
      }

      return false;
    }

    public void mouseDragged(MouseEvent e)
    {
      HMVMath.recalculateProbabilityUsingPath2(__currentNode,
                                               _cpath, getGraphPanel());
      getGraphPanel().paintScreenBuffer();
    }

    public void mouseReleased(MouseEvent e)
    {
      getGraphPanel().stopElementEdit();
      HMVMath.recalculateProbabilityUsingPath2(__currentNode,
                                               _cpath, getGraphPanel());
      _objArray[0] = __currentNode;
      setState(STATE_SELECTED_NODE, _objArray);
    }

  } // end class SelectToolStateNodeSliderEdit


  private class SelectToolStateEdgeSliderEdit extends SelectToolState
  {
    private DrawableEdge __currentEdge;
    private int __lastNodeWeight;

    public void init(Object[] args)
    {
      __currentEdge = (DrawableEdge) args[0];
    }

    public boolean handleSliderKnobMouseEvent(DrawableGraphMouseEvent e)
    {
      if (isMousePressed(e)) {
        HMVMath.initCalculationPath2(getGraphPanel().getDrawableGraph(),
                                     __currentEdge.getTail(),
                                     _cpath, true);
        getGraphPanel().startElementEdit(__currentEdge,
                                         getSubGraph(__currentEdge.getTail()),
                                         e.getMouseEvent().getPoint(),
                                         e.getContext(),
                                         true);

        if (!_app.getRealTimeSliderMode())
        {
          // erase all sliders except for the manual one
          hideAutomaticSliders(null);
        }

        __lastNodeWeight =
          FaderUtil.getNodeWeight(__currentEdge.getTail(),
                         _app.getFaderUIPrefs().getDefaultEdgeSliderValue());

        return true;
      }

      return false;
    }


    public void mouseDragged(MouseEvent e)
    {
      int newWeight =
        FaderUtil.getNodeWeight(__currentEdge.getTail(),
                  _app.getFaderUIPrefs().getDefaultEdgeSliderValue());

      // detected a sign change in the source node's weight
      if ((newWeight * __lastNodeWeight) <= 0)
      {
        __currentEdge.getTail().setHasChanged(true);
      }

      HMVMath.recalculateProbabilityUsingPath2(__currentEdge.getTail(),
                                               _cpath, getGraphPanel());
      __lastNodeWeight = newWeight;
      getGraphPanel().paintScreenBuffer();
    }

    public void mouseReleased(MouseEvent e)
    {
      getGraphPanel().stopElementEdit();
      HMVMath.recalculateProbabilityUsingPath2(__currentEdge.getTail(),
                                               _cpath, getGraphPanel());
      if (elementIsSelectable(__currentEdge)) {
        _objArray[0] = __currentEdge;
        refreshGraph();
        setState(STATE_SELECTED_EDGE, _objArray);
      }
      // else do nothing
    }

  } // end class SelectToolStateEdgeSliderEdit


  // common convenient methods
  protected void refreshGraph() {
    if (getGraphPanel() != null) {
      getGraphPanel().paintImmediately();
    }
  }


  private void selectAndHighlight(DrawableGraphElement el)
  {
    _selectionManager.clearAllSelections();
    if (el instanceof DrawableNode) {
      _selectionManager.setSelected((DrawableNode) el, true);
    }
    else {
      _selectionManager.setSelected((DrawableEdge) el, true);
    }
    _app.endSliderValueEdit();
    refreshGraph();
  }


  /**
   * Hide automatic sliders by setting the slider visibility property
   * on "automatic" nodes to false.
   */
  private void hideAutomaticSliders(DrawableNode sliderNode)
  {
    NodeIterator itr = getGraphPanel().getDrawableGraph().nodesIterator();
    while (itr.hasNext())
    {
      DrawableNode node = itr.next();
      boolean visible = FaderUtil.isSliderVisible(node);

      if (visible && (node != sliderNode)) {
        FaderUtil.setSliderVisible(node, false);
      }
      else if (!visible && (node == sliderNode)) {
        FaderUtil.setSliderVisible(node, true);
      }
    }
  }


  private HashSet getSubGraph(DrawableNode n)
  {
    HMVGraphVisitor v = new HMVGraphVisitor();
    GraphUtil.bfs(_app.getGraphPanel().getDrawableGraph(), n, v);
    return (v.__subGraph);
  }

  private class HMVGraphVisitor implements GraphUtil.GraphVisitor
  {
    HashSet __subGraph = new HashSet();

    public void visit(DrawableGraphElement e)
    {
      __subGraph.add(e);
    }
  }



  /**
   * Constants used by the animation math.
   */
  private static final double DECEL_ENDT = 10.0;
  private static final double BOUNCE_ENDT = 20.0;
  private static final double HALF_BOUNCE_ENDT = BOUNCE_ENDT / 2.0;
  private static final double DAMPING = 0.9;
  private static final int    BOUNCES = 3;
  private static final double INIT_POS = 0.0;


  /**
   * Animate a group of nodes when they're expanded. We use the "bounce"
   * algorithm when we expand.
   */
  private void expandAnimation(DrawableNode target, NodeList descendants)
  {
    if (!getApp()._useAnimation || (descendants.size() == 0)) {
      return;
    }

    GraphPanel panel = getApp().getGraphPanel();
    DrawableGraphContext ctx = panel.getDrawableGraphContext();

    Rectangle2D targetBounds = ctx.getBounds(target);

    // get the saved end positions (where to move the nodes to)
    HashMap endPositionMap = getSavedPositionList(targetBounds, descendants);

    double startx = targetBounds.getX();
    double starty = targetBounds.getY();

    // set the starting position so we won't get any rogue node on the screen
    for (NodeIterator itr=descendants.iterator(); itr.hasNext();)
    {
      DrawableNode node = itr.next();
      Rectangle2D bounds = ctx.getBounds(node);
      bounds.setRect(startx, starty, bounds.getWidth(), bounds.getHeight());
    }

    // here we go
    panel.startDrag(endPositionMap.keySet());

    for (double t=0.0; t<=BOUNCE_ENDT; t++)
    {
      for (NodeIterator itr=descendants.iterator(); itr.hasNext();)
      {
        DrawableNode node = itr.next();
        Point2D endPoint = (Point2D) endPositionMap.get(node);
        Rectangle2D curBounds = ctx.getBounds(node);

        Point2D p;
        double endx = endPoint.getX();
        double endy = endPoint.getY();

        p = Motion.bounce(t, HALF_BOUNCE_ENDT, DAMPING, BOUNCES, INIT_POS,
                          startx, starty, endx, endy);

        curBounds.setRect(p.getX(), p.getY(),
                          curBounds.getWidth(), curBounds.getHeight());
      }
      panel.paintScreenBuffer();
    }

    panel.finishDrag();
  }


  /**
   * Animate the descendants from their current positions toward the
   * target node. We use deceleration when we collapse.
   */
  private void collapseAnimation(DrawableNode target, NodeList descendants)
  {
    if (!getApp()._useAnimation || (descendants.size() == 0)) {
      return;
    }

    // save the starting positions
    HashMap startPositionMap = toPositionList(descendants);

    // make the descendants visible during animation
    setGroupProperty(descendants,
                     DrawableNode.PROPERTY_COLLAPSED,
                     DrawableNode.COLLAPSED_VISIBLE);

    GraphPanel panel = getApp().getGraphPanel();
    DrawableGraphContext ctx = panel.getDrawableGraphContext();

    Rectangle2D targetBounds = ctx.getBounds(target);
    double endx = targetBounds.getX();
    double endy = targetBounds.getY();

    // here we go
    panel.startDrag(startPositionMap.keySet());

    for (double t=0.0; t<=DECEL_ENDT; t++)
    {
      for (NodeIterator itr=descendants.iterator(); itr.hasNext();)
      {
        DrawableNode node = itr.next();
        Point2D startPoint = (Point2D) startPositionMap.get(node);
        Rectangle2D curBounds = ctx.getBounds(node);

        Point2D p = Motion.decelerate(t, DECEL_ENDT,
                                      startPoint.getX(), startPoint.getY(),
                                      endx, endy);
        curBounds.setRect(p.getX(), p.getY(),
                          curBounds.getWidth(), curBounds.getHeight());
      }
      panel.paintScreenBuffer();
    }

    // hide the descendants again
    setGroupProperty(descendants,
                     DrawableNode.PROPERTY_COLLAPSED,
                     DrawableNode.COLLAPSED_INVISIBLE);

    // save their relative starting positions for expansion
    for (NodeIterator itr=descendants.iterator(); itr.hasNext();)
    {
      DrawableNode node = itr.next();
      Point2D pos = (Point2D) startPositionMap.get(node);
      node.setProperty(ORIG_X, String.valueOf(pos.getX()-endx));
      node.setProperty(ORIG_Y, String.valueOf(pos.getY()-endy));
    }

    panel.finishDrag();
  }


  /**
   * Take a list of nodes and return a HashMap of their positions (as
   * Point2Ds) keyed to the nodes.
   *
   * @param nodes the list of nodes to construct the position HashMap from.
   * @return a HashMap containing the nodes as keys and their positions
   *  as values (as Point2Ds)
   */
  private HashMap toPositionList(NodeList nodes)
  {
    GraphPanel panel = getApp().getGraphPanel();
    DrawableGraphContext ctx = panel.getDrawableGraphContext();

    HashMap res = new HashMap();
    for (NodeIterator itr=nodes.iterator(); itr.hasNext();)
    {
      DrawableNode node = itr.next();
      Rectangle2D origBounds = ctx.getBounds(node);
      Point2D pos = new Point.Double(origBounds.getX(), origBounds.getY());
      res.put(node, pos);
    }

    return res;
  }


  /**
   * Retrieve the saved coordinates from the list of nodes, and return
   * them in a HashMap, keyed to the nodes. targetBounds contains the
   * bounds of the target node which the saved positions are relative to.
   */
  private HashMap getSavedPositionList(Rectangle2D targetBounds,
                                       NodeList descendants)
  {
    HashMap res = new HashMap();
    double targetX = targetBounds.getX();
    double targetY = targetBounds.getY();

    for (NodeIterator itr=descendants.iterator(); itr.hasNext();)
    {
      DrawableNode node = itr.next();
      double origX = Double.parseDouble(node.getProperty(ORIG_X));
      double origY = Double.parseDouble(node.getProperty(ORIG_Y));
      res.put(node, new Point2D.Double(origX+targetX, origY+targetY));
    }

    return res;
  }


  /**
   * Set a property on a group of nodes.
   *
   * @nodes a list of nodes to modify
   * @param key property key
   * @param val property value
   */
  private void setGroupProperty(NodeList nodes, String key, String val)
  {
    for (NodeIterator itr=nodes.iterator(); itr.hasNext();)
    {
      DrawableNode node = itr.next();
      node.setProperty(key, val);
    }
  }

} // end class "HMVSelectTool2"
