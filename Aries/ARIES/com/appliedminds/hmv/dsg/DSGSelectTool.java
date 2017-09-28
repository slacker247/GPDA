package com.appliedminds.hmv.dsg;

import com.appliedminds.hmv.*;
import com.appliedminds.martini.*;
import com.appliedminds.martinix.fader.*;

import java.awt.*;
import java.awt.event.*;
import java.awt.geom.*;
import java.util.*;
import java.net.URL;
import javax.swing.*;


/**
 * The select tool for the DSGViewer.
 *
 * @author daepark@apmindsf.com
 */
public class DSGSelectTool
  extends DSGTool
  implements DSGCommonTool
{
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
  private EdgePropertiesAction _edgePropertiesAction = null;
  private GraphElementPropertiesDialog _edgePropertiesDialog = null;

  private JPopupMenu _edgeIconPopupMenu = null;
  private SupportIconAddAction _edgeIconAddAction = null;
  private SupportIconDeleteAction _edgeIconDeleteAction = null;
  private SupportIconPropertiesAction _edgeIconPropertiesAction = null;
  private SupportIconPropertiesDialog _edgeIconPropertiesDialog = null;

  public DSGSelectTool(Cursor c, DSGViewer app) {
    super(c, app);
    _lastPoint = null;
    _selectedNodes = new HashSet();
    _selection = new HashSet();
  }

  // low level events we care about
  public void mousePressed(MouseEvent e) {
    _lastPoint = e.getPoint();
  }

  public void mouseDragged(MouseEvent e) {
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
    _lastPoint = null;
    if (getGraphPanel().getInDrag()) {
      getGraphPanel().finishDrag();
      _prepDrag = false;
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

  public boolean handleNodeTextMouseEvent(DrawableGraphMouseEvent e) {
    return (handleNodeMouseEvent(e));
  }

  public boolean handleEdgeMouseEvent(DrawableGraphMouseEvent e) {
    MouseEvent me = e.getMouseEvent();

    if (mousePressed(e) || me.isPopupTrigger()) {

      deactivateMarqueePane();

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
    getGraphPanel().marqueeSelected(marqueeBounds);
  }

  public boolean handleEdgeIconMouseEvent(DrawableGraphMouseEvent e) {
    MouseEvent me = e.getMouseEvent();

    if (mousePressed(e) || me.isPopupTrigger()) {
      if (SwingUtilities.isLeftMouseButton(me)) {
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

        showEdgeIconPopupMenu((DrawableEdge)e.getGraphElement(),
                              hitContext, me);
      }
    }
    return (true);
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

        if (_deleteAction == null) {
          _deleteAction = new DeleteAction();
        }

        if (_nodePropertiesAction == null) {
          _nodePropertiesAction = new NodePropertiesAction();
        }

        _nodePopupMenu.add(_deleteAction);
        _nodePopupMenu.addSeparator();
        _nodePopupMenu.add(_nodePropertiesAction);
      }

      _deleteAction.setNodesToDelete(new DrawableNode[] { node } );
      _nodePropertiesAction.setCurrentNode(node);

      _nodePopupMenu.show(e.getComponent(), e.getX(), e.getY());
    }
  }

  public boolean isNodePopupMenuVisible() {
    return (_nodePopupMenu != null && _nodePopupMenu.isVisible());
  }

  public void showEdgeProperties(DrawableEdge edge) {
    if (_edgePropertiesAction == null) {
      _edgePropertiesAction = new EdgePropertiesAction();
    }

    _edgePropertiesAction.showEdgeProperties(edge);
  }

  public void showEdgePopupMenu(DrawableEdge edge, MouseEvent e) {
    if (e.isPopupTrigger()) {
      if (_edgePopupMenu == null) {
        _edgePopupMenu = new JPopupMenu();

        if (_deleteAction == null) {
          _deleteAction = new DeleteAction();
        }

        if (_edgeIconAddAction == null) {
          _edgeIconAddAction = new SupportIconAddAction();
        }

        if (_edgePropertiesAction == null) {
          _edgePropertiesAction = new EdgePropertiesAction();
        }

        _edgePopupMenu.add(_deleteAction);
        _edgePopupMenu.addSeparator();
        _edgePopupMenu.add(_edgeIconAddAction);
        _edgePopupMenu.addSeparator();
        _edgePopupMenu.add(_edgePropertiesAction);
      }

      _deleteAction.setEdgesToDelete(new DrawableEdge[] { edge });
      _edgeIconAddAction.setCurrentEdge(edge);
      _edgePropertiesAction.setCurrentEdge(edge);

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
   * Show context popup menu for the edge icons.
   */
  private void showEdgeIconPopupMenu(DrawableEdge edge,
                                     FaderIconHitContext context,
                                     MouseEvent e)
  {
    if (e.isPopupTrigger()) {
      if (_edgeIconPopupMenu == null) {
        _edgeIconPopupMenu = new JPopupMenu();

        if (_edgeIconDeleteAction == null) {
          _edgeIconDeleteAction = new SupportIconDeleteAction(getGraphPanel());
        }

        if (_edgeIconPropertiesAction == null) {
          _edgeIconPropertiesAction = new SupportIconPropertiesAction();
        }

        _edgeIconPopupMenu.add(_edgeIconDeleteAction);
        _edgeIconPopupMenu.addSeparator();
        _edgeIconPopupMenu.add(_edgeIconPropertiesAction);
      }

      _edgeIconDeleteAction.setSupportIconToDelete(edge, context);
      _edgeIconPropertiesAction.setEdgeIconProperties(edge, context);

      _edgeIconPopupMenu.show(e.getComponent(), e.getX(), e.getY());
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
    String selected =
      element.getProperty("selected");

    if ("true".equals(selected)) {
      element.setProperty("selected", "false");
      _selection.remove(element);
      return (false);
    }
    else {
      selectGraphElement(element);
      return (true);
    }
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
      this.showNodeProperties(__currentNode);
    }

    public void showNodeProperties(DrawableNode node) {
      if (_nodePropertiesDialog == null) {
        _nodePropertiesDialog = new GraphElementPropertiesDialog(getApp());
      }

      DSGNodeProperties nodeProps = new DSGNodeProperties(node);
      _nodePropertiesDialog.setGraphElementProperties(nodeProps);
      _nodePropertiesDialog.addDialogHandler(__handler);
      _nodePropertiesDialog.setVisible(true);
    }

    public void setCurrentNode(DrawableNode node) {
      __currentNode = node;
    }

    private void updateGraphPanel() {
      if (_nodePropertiesDialog.propertiesChanged()) {
        // Repaint graph
        getGraphPanel().repaint();
      }
    }

  } // end class "NodePropertiesAction"


  /**
   * Action to add support documents to a edge.
   */
  private class SupportIconAddAction extends AbstractAction {

    private DrawableEdge __currentEdge = null;

    private DialogHandler __handler = new DialogHandlerAdapter() {
        public void clickedOk() {
          String newType = _edgeIconPropertiesDialog.getType();
          URL newURL = _edgeIconPropertiesDialog.getURL();

          if (! (__currentEdge == null || newType == null || newURL == null)) {
            TypeInfoAdapter typeInfo =
              TypeInfoAdapter.getTypeInfoAdapter(__currentEdge);

            typeInfo.add(newType, true, newURL);
            getGraphPanel().repaint();  // repaint to add icon
          }

          // Remove ourself from the dialogs listener list.
          // This will be added whenever there is an action event.
          _edgeIconPropertiesDialog.removeDialogHandler(this);
        }

        public void clickedCancel() {
          // Remove ourself from the dialogs listener list.
          // This will be added whenever there is an action event.
          _edgeIconPropertiesDialog.removeDialogHandler(this);
        }
      };

    public SupportIconAddAction() {
      super("Add Supporting Document");
    }

    public void actionPerformed(ActionEvent e) {
      if (_edgeIconPropertiesDialog == null) {
        _edgeIconPropertiesDialog = new SupportIconPropertiesDialog(getApp());
      }

      _edgeIconPropertiesDialog.addDialogHandler(__handler);
      _edgeIconPropertiesDialog.update(null, null);
      _edgeIconPropertiesDialog.setVisible(true);
    }

    public void setCurrentEdge(DrawableEdge edge) {
      __currentEdge = edge;
    }

  } // end class "SupportIconAddAction"


  /**
   * Action object to view/edit edge properties.
   */
  private class EdgePropertiesAction extends AbstractAction {

    private DrawableEdge __currentEdge = null;

    private DialogHandler __handler = new DialogHandlerAdapter() {
        public void clickedOk() {
          EdgePropertiesAction.this.updateGraphPanel();
          // Remove ourself from the dialogs listener list.
          // This will be added whenever there is an action event.
          _edgePropertiesDialog.removeDialogHandler(this);
          _edgePropertiesDialog.setVisible(false);
        }

        public void clickedApply() {
          EdgePropertiesAction.this.updateGraphPanel();
        }

        public void clickedCancel() {
          // Undo any changes
          // This should have been taken care by the EdgeProperties dialog
          // We just need to recalculate the probabilities

          EdgePropertiesAction.this.updateGraphPanel();

          // Remove ourself from the dialogs listener list.
          // This will be added whenever there is an action event.
          _edgePropertiesDialog.removeDialogHandler(this);
          _edgePropertiesDialog.setVisible(false);
        }
      };

    public EdgePropertiesAction() {
      super("Edge Properties");
    }

    public void actionPerformed(ActionEvent e) {
      this.showEdgeProperties(__currentEdge);
    }

    public void showEdgeProperties(DrawableEdge edge) {
      if (_edgePropertiesDialog == null) {
        _edgePropertiesDialog = new GraphElementPropertiesDialog(getApp());
      }

      DSGEdgeProperties edgeProps = new DSGEdgeProperties(edge);
      _edgePropertiesDialog.setGraphElementProperties(edgeProps);
      _edgePropertiesDialog.addDialogHandler(__handler);
      _edgePropertiesDialog.setVisible(true);
    }

    public void setCurrentEdge(DrawableEdge edge) {
      __currentEdge = edge;
    }

    private void updateGraphPanel() {
      if (_edgePropertiesDialog.propertiesChanged()) {
        // Repaint graph
        getGraphPanel().repaint();
      }
    }

  } // end class "EdgePropertiesAction"


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
     * set the nodes to delete
     */
    public void setNodesToDelete(DrawableNode[] nodes) {
      __nodesToDelete.clear();
      if (nodes != null) {
        for (int i = 0; i < nodes.length; i++) {
          __nodesToDelete.add(nodes[i]);
        }
      }
    }

    /**
     * set the edges to delete
     */
    public void setEdgesToDelete(DrawableEdge[] edges) {
      __edgesToDelete.clear();
      if (edges != null) {
        for (int i = 0; i < edges.length; i++) {
          __edgesToDelete.add(edges[i]);
        }
      }
    }

    /**
     * delete nodes and edges in the "to delete" list.
     */
    public void actionPerformed(ActionEvent e) {
      GraphPanel panel = getGraphPanel();
      DrawableGraph graph = panel.getDrawableGraph();

      if (__nodesToDelete.size() == 0 && __edgesToDelete.size() == 0) {
        return;
      }

      // first "erase" it from the visual first
      Iterator itr = __nodesToDelete.iterator();
      while (itr.hasNext()) {
        DrawableNode node = (DrawableNode)itr.next();

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

        // remove edge from graph
        graph.remove(edge);
      }

      panel.redraw();

      __nodesToDelete.clear();
      __edgesToDelete.clear();
    }

  } // end class "DeleteAction"


  /**
   * Action object that knows how to show the properties of a support
   * icon.
   */
  private class SupportIconPropertiesAction extends AbstractAction {

    private DrawableEdge __currentEdge = null;
    private FaderIconHitContext __currentContext = null;
    private DialogHandler __updateHandler = new DialogHandlerAdapter() {
        public void clickedOk() {
          String newType = _edgeIconPropertiesDialog.getType();
          URL newURL = _edgeIconPropertiesDialog.getURL();

          if (newType != null && newURL != null) {
            TypeInfoAdapter typeInfo =
              TypeInfoAdapter.getTypeInfoAdapter(__currentEdge);
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
          _edgeIconPropertiesDialog.removeDialogHandler(this);
        }

        public void clickedCancel() {
          // Remove ourself from the dialogs listener list.
          // This will be added whenever there is an action event.
          _edgeIconPropertiesDialog.removeDialogHandler(this);}
      };

    public SupportIconPropertiesAction() {
      super("Supporting Document Properties");
    }

    public void setEdgeIconProperties(DrawableEdge edge,
                                      FaderIconHitContext context)
    {
      __currentEdge = edge;
      __currentContext = context;
    }

    public void actionPerformed(ActionEvent e) {
      if (!(__currentEdge == null || __currentContext == null)) {
        if (_edgeIconPropertiesDialog == null) {
          _edgeIconPropertiesDialog = new SupportIconPropertiesDialog(getApp());
        }

        _edgeIconPropertiesDialog.addDialogHandler(__updateHandler);
        _edgeIconPropertiesDialog.update(__currentContext.getType(),
                                         __currentContext.getURL());

        _edgeIconPropertiesDialog.setVisible(true);
      }
    }

  } // end class "SupportIconPropertiesAction"

} // end class "DSGSelectTool"


