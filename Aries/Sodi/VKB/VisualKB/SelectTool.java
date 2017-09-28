/*
 * SelectTool.java
 *
 * Created on June 27, 2003, 11:49 AM
 */

package VisualKB;

import com.appliedminds.martinix.gapp.*;
import com.appliedminds.martini.*;
import com.appliedminds.martinix.ui.*;
import com.appliedminds.martinix.fader.*;

import java.awt.Cursor;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;
import java.awt.event.KeyEvent;
import javax.swing.*;
import com.appliedminds.hmv.*;
import java.awt.Point;
import java.awt.Color;
import java.awt.Stroke;
import java.awt.geom.Point2D;
import java.awt.Cursor;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.BasicStroke;
import java.awt.Graphics2D;
import java.awt.event.*;
import java.awt.geom.Rectangle2D;
import java.awt.geom.Ellipse2D;
import java.io.*;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.*;
import javax.swing.*;
import javax.swing.event.*;
import java.util.logging.Logger;
/**
 *
 * @author  s824685
 */
public class SelectTool implements GTool {
    
    private Cursor zCursor = new Cursor(0);
    private int MouseY = 0;
    private GraphPanel gp = null;
    private boolean debug = false;
    private Point lastPoint = new Point();
    private int DragType = 9;
    
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
    private Graphics2D Gpoint2D = null;
    private FaderUIPrefs2 _graphUIPrefs = null;
    private DrawableGraphElement el = null;
    private MouseEvent me = null;
    private Object context = null;
    private HMVSelectionManager _selectionManager;

    /** Creates a new instance of SelectTool */
    public SelectTool() {
    }
    
    /** Creates a new instance of SelectTool */
    public SelectTool(Cursor c) {
        zCursor = c;
    }
    
    public void setGraphPanel(GraphPanel panel)
    {
        gp = panel;
    }
    
    public void setPos(Point pos)
    {
        lastPoint = pos;
    }
    
    public void setSelectionManager(HMVSelectionManager sm)
    {
        _selectionManager = sm;
    }
    
    public void setDragType(int type)
    {
        DragType = type;
    }

    public void nodeEvent(DrawableGraphElement dge, Object inObj)
    {
        el = dge;
        context = inObj;
    }
    
    public void setNewEdgeNode(DrawableNode sourceNode, EdgeConnectorHub sourceHub, Graphics2D g, FaderUIPrefs2 fuip)
    {
        _graphUIPrefs = fuip;
          Gpoint2D = g;
          __sourceNode = sourceNode;
          __sourceHub = sourceHub;
          __toPt = null;
          __fromPt = __sourceHub.getPoint();
          __targetNode = null;
    }
    
    /** Notification that the tool has been activated.
     * Perferm any intialization steps before it's actually used.
     *
     * @args any arguments this tool need to be activated.
     */
    public void activate(Object[] args) {
        if(debug) System.out.println("SelectTool.activate():");
    }
    
    /** Notification that the tool has been deactivated.
     * Do any clean up before another tool is activated.
     */
    public void deactivate() {
        if(debug) System.out.println("SelectTool.deactivate():");
    }
    
    /** @return the Cursor that will represent this tool when this
     * tool is activated.
     */
    public Cursor getCursor() {
        if(debug) System.out.println("SelectTool.getCursor():");
        return(zCursor);
    }
    
    /** Handle a Marquee Selection event.
     */
    public void handleMarqueeSelection(Rectangle marqueeBounds) {
        if(debug) System.out.println("SelectTool.handleMarqueeSelection():");
    }
    
    /** Handle a key pressed event.
     */
    public void keyPressed(KeyEvent e) {
        if(debug) System.out.println("SelectTool.keyPressed():");
    }
    
    /** Handle a key released event.
     */
    public void keyReleased(KeyEvent e) {
        if(debug) System.out.println("SelectTool.keyReleased():");
    }
    
    /** Handle a key typed event.
     */
    public void keyTyped(KeyEvent e) {
        if(debug) System.out.println("SelectTool.keyTyped():");
    }
    
    /** Handle a single mouse click event
     */
    public void mouseClicked(MouseEvent e) {
        if(debug) System.out.println("SelectTool.mouseClicked():");
    }
    
    /** Handle a double mouse click event
     */
    public void mouseDoubleClicked(MouseEvent e) {
        if(debug) System.out.println("SelectTool.mouseDoubleClicked():");
    }
    
    /** Handle a mouse dragged event.
     */
    public void mouseDragged(MouseEvent e) {
//        Collection c = new HashSet();
//        for (Iterator i = _selectionManager.getSelectedNodes(); i.hasNext(); )
//           c.add(i.next());
//        gp.startDrag(c);
        System.out.println("SelectTool.mouseDragged():");
        if (gp.getInDrag() && DragType == 0) {
            System.out.println("SelectTool.mouseDragged() gp.getInDrag():");
            // first compute how much we moved
            Point curPoint = e.getPoint();
            int dx = (curPoint.x - lastPoint.x);
            int dy = (curPoint.y - lastPoint.y);

            gp.dragTo(dx, dy);
            lastPoint = curPoint;
        }else if(DragType == 1)
        {
            System.out.println("SelectTool.mouseDragged() DragType 1:");

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
            gp.redraw();
        }
    }
    
    /** Handle a mouse entered event.
     */
    public void mouseEntered(MouseEvent e) {
        if(debug) System.out.println("SelectTool.mouseEntered():");
    }
    
    /** Handle a mouse exit event.
     */
    public void mouseExited(MouseEvent e) {
        if(debug) System.out.println("SelectTool.mouseExited():");
    }
    
    /** Handle a mouse moved event.
     */
    public void mouseMoved(MouseEvent e) {
        if(debug) System.out.println("SelectTool.mouseMoved():");
    }
    
    /** Handle a mouse pressed event.
     */
    public void mousePressed(MouseEvent e) {
        if(debug) System.out.println("SelectTool.mousePressed():");
    }
    
    /** Handle a mouse released event.
     */
    public void mouseReleased(MouseEvent e) {
        if(debug) System.out.println("SelectTool.mouseReleased():");
    }
    
/*///////////////////////////////////////////////////////////////////////////////
    /**
    * dummy SelectToolState
    *\/
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
        *\/

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

//******************************************************************************
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
          }
          else {
            __deleteCmd.setNodesToDelete(_selectionManager.getSelectedNodes());
            __deleteCmd.setEdgesToDelete(_selectionManager.getSelectedEdges());
          }

          if (isNode)
          {
            __simItem.setVisible(true);
          }
          else
          {
            __simItem.setVisible(false);
          }

          __elementPopupMenu.show(me.getComponent(), me.getX(), me.getY());
        }
//******************************************************************************
    } // end class "SelectToolState"


    /**
    * STATE_IDLE
    *\/
    public class SelectToolStateIdle extends SelectToolState {

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
    *\/
    public class SelectToolStateHoverNode extends SelectToolState {

        private DrawableNode __currentNode;

        /**
         * @param args args[0] is a DrawableNode
         *\/
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
    *\/
    public class SelectToolStateSelectedNode extends SelectToolState {

        private DrawableNode __currentNode;
        private Point __lastPoint;

        /**
         * @param args args[0] is a DrawableNode
         *\/
        public void init(Object[] args) {
          __currentNode = (DrawableNode)args[0];
        }

        public void mouseDragged(MouseEvent e) {
          _objArray[0] = __lastPoint;
          _objArray[1] = __currentNode;
          setState(STATE_DRAG_NODE, _objArray);
          _currentState.mouseDragged(e);
        }

//******************************************************************************
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
//******************************************************************************

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
         *\/
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
         *\/
        private void removeFauxEdges(DrawableNode node) {
          DrawableGraph graph = _app.getGraphPanel().getDrawableGraph();
          for (EdgeIterator ei = node.getIncomingEdges(); ei.hasNext(); ) {
            DrawableEdge e = ei.next();
            if (e.getProperty(DrawableEdge.PROPERTY_FAUX) != null)
              graph.remove(e);
          }
        }

    } // end class "SelectToolStateSelectedNode"


    public class SelectToolStateSelectedEdge extends SelectToolState
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

//******************************************************************************
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
//******************************************************************************

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

//******************************************************************************
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
//******************************************************************************

    } // end class SelectToolStateSelectedEdge


*///******************************************************************************
    /**
    * STATE_CREATE_EDGE
    *\/
    public class SelectToolStateCreateEdge extends SelectToolState
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
         *\/
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
//            refreshGraph();
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
*/
        private void rubberBand(Point2D from, Point2D to) {
          Graphics2D g = null;
          try {
            g = Gpoint2D;

            g.setColor(rubberBandColor);
            g.setStroke(rubberBandStroke);

            g.setXORMode(gp.getBackground());
            g.drawLine((int)from.getX(), (int)from.getY(),
                       (int)to.getX(), (int)to.getY());

            // connector hub draw props
            double d =
              gp.mapWorldToViewport(_graphUIPrefs.getConnectorHubDiameter());
            double r = d / 2;
            DrawProps drawProps = _graphUIPrefs.getEdgeConnectorHubDrawProps(true, false, false);
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
         *\/
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
         *\/
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

    } // end class "SelectToolStateCreateEdge
/******************************************************************************
    
    /**
    * STATE_DRAG_EDGE
    *\/
    public class SelectToolStateDragNode extends SelectToolState {

        private DrawableNode __draggedNode; // the primary node that's being dragged
        private Point __lastPoint;

        /**
         * @param args args[0] is a Point (starting drag position)
         *             args[1] is a DrawableNode (the primary node being dragged)
         *\/
        public void init(Object[] args) {
          __lastPoint = (Point)args[0];
          __draggedNode = (DrawableNode)args[1];
          Collection c = new HashSet();
          for (Iterator i = _selectionManager.getSelectedNodes(); i.hasNext(); )
            c.add(i.next());

          getGraphPanel().startDrag(c);
        }

//******************************************************************************
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
//******************************************************************************

    } // end class "SelectToolStateDragNode"
*/}
