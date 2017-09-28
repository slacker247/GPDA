/*
 * BNE_View.java
 *
 * Created on June 25, 2003, 11:11 AM
 */

package VisualKB;

import com.appliedminds.hmv.*;
import com.appliedminds.martinix.gapp.*;
import com.appliedminds.martinix.ui.*;
import com.appliedminds.martini.*;
import com.appliedminds.martinix.fader.*;
import com.appliedminds.jam.AccessKBDlg;
import com.appliedminds.martini.io.LayoutLoader;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.geom.*;
import java.util.*;
/**
 *
 *   This class is designed to create an instance of the graphical view and 
 * functionality of the Applied Minds Belief Network code.  There is a need
 * to redesign the functionality to mimic the behavior of our belief network.
 *
 * @author  s824685
 */
public class BNE_View extends GAppFrame{
    
    private FaderUI2 _graphUI;
    public FaderUIPrefs2 _graphUIPrefs;
    private HMVSelectionManager _selectionManager;
    private boolean debug = false;
    private String g_Story = "";
    private String g_Domain = "";
    public String g_IP = "";
    public String g_Port = "";

    /** Creates a new instance of BNE_View */
    public BNE_View() {
        super(".:BNE:.",
                    false,    // sidebar
                    true,     // menubar
                    true,     // toolbar
                    false,    // statusbar
                    true,     // selectTool
                    true,     // textTool
                    true,     // zoomTool
                    false,     // handTool
                    true,     // nodeTool
                    true      // edgeTool
                    );
        _graphUIPrefs = new FaderUIPrefs2();

        // colors
        _graphUIPrefs.setDefaultColor(new Color(0, 0, 255));
        _graphUIPrefs.setSupportingColor(new Color(0, 255, 0));
        _graphUIPrefs.setRefutingColor(new Color(255, 0, 0));

        // use curved edges
        _graphUIPrefs.setCurvedLines(true);

        _graphUI = new FaderUI2(_graphUIPrefs);
        _selectionManager = new FaderSelectionManager();
    }
    
    /** At this point the graph panel has been initialized and subclasses
     * should add listeners to hook up the tools.
     */
    protected void addDrawableGraphMouseEventListeners() {
        if(debug) System.out.println("In BNE_View.addDrawableGraphMouseEventListeners():");
    //
    // SLIDER_KNOB
    //
    getGraphPanel().addDrawableGraphMouseEventListener
      (FaderUI2.DECORATION_SLIDER_KNOB,
       new DrawableGraphMouseEventListener() {
         public boolean handleDrawableGraphMouseEvent(DrawableGraphMouseEvent e)
         {
            if(debug) System.out.println("In BNE_View.handleDrawableGraphMouseEven() SLIDER_KNOB:");
             return false;
         }
       });
    //
    // NODE
    //
    getGraphPanel().addDrawableGraphMouseEventListener
    (FaderUI2.DECORATION_NODE,
    new DrawableGraphMouseEventListener() {
     public boolean handleDrawableGraphMouseEvent(DrawableGraphMouseEvent e)
     {
        if(debug) System.out.println("In BNE_View.handleDrawableGraphMouseEven() NODE with node selection:");
        DrawableGraphElement el = e.getGraphElement();

      MouseEvent me = e.getMouseEvent();
      Object context = e.getContext();
//      ((SelectTool) getCurrentTool()).setDragType(0);
//      ((SelectTool) getCurrentTool()).setPos(me.getPoint());
//      ((SelectTool) getCurrentTool()).nodeEvent(el, context);

      if (me.getID() == MouseEvent.MOUSE_ENTERED && getCurrentTool() instanceof SelectTool) {
        // show edge connector hubs
        FaderUtil.setConnectorHubsVisible((DrawableNode) el, true);
        getGraphPanel().paintImmediately();
      }
      else if (me.getID() == MouseEvent.MOUSE_PRESSED && getCurrentTool() instanceof SelectTool) {
        if (context instanceof EdgeConnectorHub)
        {
            if(debug) System.out.println("In BNE_View.handleDrawableGraphMouseEven() NODE with EdgeConnectorHub selection:");
            getGraphPanel().finishDrag();
            ((SelectTool) getCurrentTool()).setDragType(1);
            ((SelectTool) getCurrentTool()).setNewEdgeNode((DrawableNode) el, ((EdgeConnectorHub) context), (Graphics2D) getGraphics(), _graphUIPrefs);            
        }
        else if (context instanceof DrawableNode ||
                 context instanceof TextScreenData)
        {
            if(debug) System.out.println("In BNE_View.handleDrawableGraphMouseEven() NODE with TextScreenData or DrawableNode selection:");
            _selectionManager.clearAllSelections();
            _selectionManager.setSelected((DrawableNode) el, true);
            // Don't show node slider toggle arrow
            FaderUtil.setSliderToggleVisible((DrawableNode) el, false);
            getGraphPanel().paintImmediately();
        }
      }
      else if (me.getID() == MouseEvent.MOUSE_EXITED && getCurrentTool() instanceof SelectTool) {
        if (!(context instanceof TextScreenData)) {
          if(debug) System.out.println("In BNE_View.handleDrawableGraphMouseEven() NODE with TextScreenData selection:");
          // hide connector hubs
          FaderUtil.setConnectorHubsVisible((DrawableNode) el, false);
          getGraphPanel().paintImmediately();
        }
      }
      else if(me.getID() == MouseEvent.MOUSE_DRAGGED && getCurrentTool() instanceof SelectTool)
      {
          if(debug) System.out.println("In BNE_View.handleDrawableGraphMouseEven() NODE with Mouse Dragged:");
          Collection c = new HashSet();
          for (Iterator i = _selectionManager.getSelectedNodes(); i.hasNext(); )
             c.add(i.next());
          getGraphPanel().startDrag(c);
          ((SelectTool) getCurrentTool()).setPos(me.getPoint());
      }
      else if(me.getID() == MouseEvent.MOUSE_RELEASED && getCurrentTool() instanceof SelectTool)
      {
            ((SelectTool) getCurrentTool()).setDragType(0);
            getGraphPanel().finishDrag();
      }

         return false;
     }
    });
    //
    // EDGE_BUBBLE
    //
    getGraphPanel().addDrawableGraphMouseEventListener
      (FaderUI2.DECORATION_EDGE_BUBBLE,
       new DrawableGraphMouseEventListener() {
         public boolean handleDrawableGraphMouseEvent(DrawableGraphMouseEvent e)
         {
            if(debug) System.out.println("In BNE_View.handleDrawableGraphMouseEven() EDGE_BUBBLE:");
             return false;
         }
       });
    //
    // NODE_TEXT
    //
    getGraphPanel().addDrawableGraphMouseEventListener
      (FaderUI2.DECORATION_NODE_TEXT,
       new DrawableGraphMouseEventListener() {
         public boolean handleDrawableGraphMouseEvent(DrawableGraphMouseEvent e)
         {
            if(debug) System.out.println("In BNE_View.handleDrawableGraphMouseEven() NODE_TEXT:");
             return false;
         }
       });
    //
    // NODE_ICON
    //
    getGraphPanel().addDrawableGraphMouseEventListener
      (FaderUI2.DECORATION_NODE_ICON,
       new DrawableGraphMouseEventListener() {
          public boolean handleDrawableGraphMouseEvent(DrawableGraphMouseEvent e)
         {
            if(debug) System.out.println("In BNE_View.handleDrawableGraphMouseEven() NODE_ICON:");
             return false;
         }
       });
    //
    // EDGE
    //
    getGraphPanel().addDrawableGraphMouseEventListener
      (FaderUI2.DECORATION_EDGE,
       new DrawableGraphMouseEventListener() {
         public boolean handleDrawableGraphMouseEvent(DrawableGraphMouseEvent e)
         {
            if(debug) System.out.println("In BNE_View.handleDrawableGraphMouseEven() EDGE:");
            if (e.getMouseEvent().getID() == MouseEvent.MOUSE_PRESSED)
            {
                getGraphPanel().finishDrag();
                _selectionManager.clearAllSelections();
                _selectionManager.setSelected((DrawableEdge) e.getGraphElement(), true);
                getGraphPanel().redraw();
            }
            return false;
         }
       });
    //
    // SLIDER_VALUE
    //
    getGraphPanel().addDrawableGraphMouseEventListener
      (FaderUI2.DECORATION_SLIDER_VALUE,
       new DrawableGraphMouseEventListener() {
         public boolean handleDrawableGraphMouseEvent(DrawableGraphMouseEvent e)
         {
            if(debug) System.out.println("In BNE_View.handleDrawableGraphMouseEven() SLIDER_VALUE:");
             return false;
         }
       });
    //
    // SLIDER_TOGGLE
    //
    getGraphPanel().addDrawableGraphMouseEventListener
      (FaderUI2.DECORATION_SLIDER_TOGGLE,
       new DrawableGraphMouseEventListener() {
         public boolean handleDrawableGraphMouseEvent(DrawableGraphMouseEvent e)
         {
            if(debug) System.out.println("In BNE_View.handleDrawableGraphMouseEven() SLIDER_TOGGLE:");
             return false;
         }
       });

    //
    // OFFGRAPH_DECORATION_ID (whitespace hit)
    //
    getGraphPanel().addDrawableGraphMouseEventListener
      (GraphPanel.OFFGRAPH_DECORATION_ID,
       new DrawableGraphMouseEventListener() {
         public boolean handleDrawableGraphMouseEvent(DrawableGraphMouseEvent e)
         {
            if(debug) System.out.println("In BNE_View.handleDrawableGraphMouseEven() OFFGRAPH_DECORATION_ID (whitespace hit):");
            _selectionManager.clearAllSelections();
            
            getGraphPanel().finishDrag();
            getGraphPanel().redraw();
             return false;
         }
       });
    //
    // register Multiple hit test listeners
    //
    getGraphPanel().addDrawableGraphMultipleSelectionEventListener
      (new DrawableGraphMultipleSelectionEventListener() {
          public void handleMultipleHitTestSuccess(DrawableGraphMultipleSelectionEvent evt)
          {
            if(debug) System.out.println("In BNE_View.handleDrawableGraphMouseEven() register Multiple hit test listeners 1:");
          }

          public void handleMultipleHitTestMissedGraph(Rectangle2D rectangle)
          {
            if(debug) System.out.println("In BNE_View.handleDrawableGraphMouseEven() register Multiple hit test listeners 2:");
          }
        });
    }
////////////////////////////////////////////////////end of Mouse Listner
    
    /** If you want to add app-specific file menu stuff add it here.
     */
    protected void addToFileMenuHook(JMenu filemenu) {
        if(debug) System.out.println("In BNE_View.addToFileMenuHook():");
        javax.swing.JMenuItem jM_SaveToKB = new javax.swing.JMenuItem();
        jM_SaveToKB.setText("Save To KB");
        jM_SaveToKB.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                AccessKBDlg accessKB = new AccessKBDlg();
                accessKB.DrawableGraphToStory(getGraphPanel().getDrawableGraph(), g_Story, g_Domain);
            }
        });

        filemenu.add(jM_SaveToKB);

        javax.swing.JMenuItem jM_OpenFromKB = new javax.swing.JMenuItem();
        jM_OpenFromKB.setText("Open From KB");
        jM_OpenFromKB.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                AccessKBDlg KBAccess = new com.appliedminds.jam.AccessKBDlg();

                KBAccess.setModal(true);
                KBAccess.showKB();

                if(KBAccess.OK) {
                    loadStoryFromKB(KBAccess.StoryName, KBAccess.Domain);
                }
            }
        });

        filemenu.add(jM_OpenFromKB);
    }
    
    public void setServerLocal(String IP, String Port)
    {
        g_IP = IP;
        g_Port = Port;
    }
    
    /** Loads a Belief network from the KB
     */
    public void loadStoryFromKB(String Story, String Domain)
    {
        AccessKBDlg KBAccess = new com.appliedminds.jam.AccessKBDlg();
//        KBAccess.test.configuration.setHost(g_IP);
//        KBAccess.test.configuration.setPort(g_Port);

        g_Story = KBAccess.StoryName = Story;
        g_Domain = KBAccess.Domain = Domain;

        LayoutLoader loader = new GLayoutLoader();
        DrawableGraph graph = KBAccess.StoryToGMLFile();

        //Design the layout of the graph
        Rectangle2D rect[] = new Rectangle2D.Float[100];
        NodeIterator temp = graph.nodesIterator();
        long y = 20;
        int count = 0;
        long x[] = new long[10];
        for(int i = 0; i < 10; i++)
            x[i] = 20;
        String Levels = graph.getProperty("Levels");
        String Level[] = new String[10];
        for(int i = 0; i < 10; i++)
            Level[i] = "";
        int nextL = 0;
        for(int i = 0; i < Levels.length(); i++)
        {
            if(',' == Levels.charAt(i))
            {
                nextL++;
                continue;
            }
            Level[nextL] += Levels.charAt(i);
        }
        while(temp.hasNext())
        {
            com.appliedminds.martini.DrawableNode n = temp.next();
            for(int i = 0; i <= nextL; i++)
            {
                if(Level[i].equals(n.getProperty("level")))
                {
                    System.out.println("Level: " + Level[i]);
                    y = (i + 1)*100;
                    rect[count] = new Rectangle2D.Float();
                    rect[count].setFrame(x[i], y, 225, 144);
                    loader.loadNodeBounds(n, rect[count]);
                    x[i] += 300;
                    count++;
                }
            }
        }

        // we cannot call gapp's setDrawableGraph() method
        // from here or else we would get into an infinite
        // event loop
        getGraphPanel().stopAnimation();
        getGraphPanel().stopElementEdit();
        getGraphPanel().setDrawableGraph(graph);
        setDrawableGraphHook();

        // sets graph elements' coordinates
        loadLayout(loader);

        fitGraphInWindow();
        setDocumentState(GAppFrame.DOC_UNMODIFIED);
        fileOpenHook();
        getGraphPanel().repaint();
    //    return (_fileOpenKBAction);
    }
    
    /** Called after a new file action.
     */
    protected void fileNewHook() {
        if(debug) System.out.println("In BNE_View.fileNewHook():");
    }
    
    /** Called after a file is opened.
     */
    protected void fileOpenHook() {
        if(debug) System.out.println("In BNE_View.fileOpenHook():");
    }
    
    /** Return a filter table here if you have enabled the sidebar in the
     * constructor.  If the sidebar is not rendered, then this method
     * will never be called.
     */
    protected GFilterTable getEdgeFilterTable() {
        if(debug) System.out.println("In BNE_View.getEdgeFilterTable():");
        return null;
    }
    
    /** Subclasses can return a widget here to place in the lower right
     * hand part of the app.  Subclasses could also return null.
     */
    protected Component getExtraStatusBarWidget() {
        if(debug) System.out.println("In BNE_View.getExtraStatusBarWidget():");
        return null;
    }
    
    /** Return a filter table here if you have enabled the sidebar in the
     * constructor.  If the sidebar is not rendered then this method
     * will neve be called.
     */
    protected GFilterTable getNodeFilterTable() {
        if(debug) System.out.println("In BNE_View.getNodeFilterTable():");
        return null;
    }
    
    /** Return a transformer here.
     */
    protected GTransformer getTransformer() {
        if(debug) System.out.println("In BNE_View.getTransformer():");
        return null;
    }
    
    /** Return the edge tool here  */
    protected GTool initEdgeTool(Cursor cur) {
        if(debug) System.out.println("In BNE_View.initEdgeTool():");
        return null;
    }
    
    /** Called when initGUI is complete.
     */
    protected void initGUIHook() {
        if(debug) System.out.println("In BNE_View.initGUIHook():");
    }
    
    /** Create and return a GraphPanel here.  Remember to set
     * the background.
     */
    protected GraphPanel initGraphPanel() {
        if(debug) System.out.println("In BNE_View.initGraphPanel():");
        GraphPanel p = new GraphPanel(_graphUI);
        p.setBackground(Color.white);
        p.setLayout(new SlackerLayout()); // give the app full control of layout
        return(p);
    }
    
    /** Return the hand tool here  */
    protected GTool initHandTool(Cursor open, Cursor closed) {
        if(debug) System.out.println("In BNE_View.initHandTool():");
        return null;
    }
    
    /** Return the node tool here  */
    protected GTool initNodeTool(Cursor cur) {
        if(debug) System.out.println("In BNE_View.initNodeTool():");
        return null;
    }
    
    /** Return the pan tool here  */
    protected GTool initPanTool(Cursor xy, Cursor x, Cursor y, Cursor n, Cursor w, Cursor s, Cursor r, Cursor nw, Cursor ne, Cursor sw, Cursor se) {
        if(debug) System.out.println("In BNE_View.initPanTool():");
        return null;
    }
    
    /** Return the select tool here  */
    protected GTool initSelectTool(Cursor cur) {
        if(debug) System.out.println("In BNE_View.initSelectTool():");
        return (new SelectTool(cur));
    }
    
    /** Return the text tool here  */
    protected GTool initTextTool(Cursor cur) {
        if(debug) System.out.println("In BNE_View.initTextTool():");
        return null;
    }
    
    /** Return the select zoom here  */
    protected GTool initZoomTool(Cursor in, Cursor out, Cursor marquee) {
        if(debug) System.out.println("In BNE_View.initZoomTool():");
        return (new ZoomTool(marquee));
    }
    
    /** Is the graph valid to the application?
     */
    public boolean isGraphValid(DrawableGraph g) {
        if(debug) System.out.println("In BNE_View.isGraphValid():");
        return false;
    }
    
    /** Called when the mouse is clicked on the display.  Normally
     * the click is given to the current tool.
     *
     * @return true to prevent passing the mouse event on to
     * the current tool.
     */
    protected boolean mousePressedHook(MouseEvent e) {
        if(debug) System.out.println("In BNE_View.mousePressedHook():");
        return false;
    }
    
    /** Called when the mouse is released on the display.  Normally
     * the release event is passed to the current tool.
     *
     * @return true to prevent passing the mouse event on to
     * the current tool.
     */
    protected boolean mouseReleasedHook(MouseEvent e) {
        if(debug) System.out.println("In BNE_View.mouseReleasedHook():");
        return false;
    }
    
    /** Called after setDrawableGraph executes.
     */
    protected void setDrawableGraphHook() {
        if(debug) System.out.println("In BNE_View.setDrawableGraphHook():");

        useZoomTool();
        ZoomTool zTool = null;
        zTool = (ZoomTool) getCurrentTool();

        if (zTool != null)
        {
            //Any initialization code from this class that needs to be used with
            // the tool.
            zTool.setGraphPanel(getGraphPanel());
            zTool.setScaleValue(getMinimumScale());
        }

        useSelectTool();
        SelectTool sTool = null;
        sTool = (SelectTool) getCurrentTool();

        if (sTool != null)
        {
            //Any initialization code from this class that needs to be used with
            // the tool.
            sTool.setGraphPanel(getGraphPanel());
            sTool.setSelectionManager(_selectionManager);
        }
    }
    
  /**
   * The Slacker Layout lets the parent decide where to place the
   * children components.
   */
  private class SlackerLayout implements LayoutManager
  {
    public void addLayoutComponent(Component comp, Object constraints) {}
    public void addLayoutComponent(String name, Component comp) {}

    public void layoutContainer(Container parent) {
//      _textField.setSize(_textField.getPreferredSize());
//      _textArea.setSize(_textArea.getPreferredSize());
    }

    public Dimension minimumLayoutSize(Container parent)
    {
      Rectangle2D rect = getGraphPanel().getOverallBounds();
      return (new Dimension((int)rect.getX(), (int)rect.getY()));
    }

    public Dimension preferredLayoutSize(Container parent)
    {
      java.awt.geom.Rectangle2D rect = getGraphPanel().getOverallBounds();
      return (new Dimension((int)rect.getX(), (int)rect.getY()));
    }

    public void removeLayoutComponent(Component comp) {}
  }
    
    /**
     * @param args the command line arguments
     */
    public static void main(String args[])
    {
        final GAppFrame BNEView = new BNE_View();
        BNEView.initGAppFrame(false);
        BNEView.setSize(800, 800);
        BNEView.show();
    }
}
