package com.appliedminds.hmv.dsg;

import com.appliedminds.hmv.MyDialog;
import com.appliedminds.hmv.DialogHandler;
import com.appliedminds.martini.*;
import com.appliedminds.martinix.fader.*;
import com.appliedminds.martinix.gapp.*;
import java.awt.*;
import java.awt.geom.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import java.util.*;


/**
 * The default supporting graph viewer using the
 * martinix.fader.FaderUI as its GraphUI.
 *
 * <p>This can be used as a dialog box, as it comes with an ok and a
 * cancel button. Those who wish to get a handle on these dialog
 * buttons can register via the addDialogHandler and
 * removeDialogHandler methods.
 *
 * @author daepark@apmindsf.com
 */
public class DSGViewer extends GAppFrame {

  public static final String LOG_NAME = "appliedminds.HMV";

  private GraphUI _graphUI;
  private FaderUIPrefs2 _graphUIPrefs;
  private MyDialog _myDialog; // place holder to route DialogHandler events

  private DSGCommonTool _commonTool;

  public DSGViewer() {
    super(".:Supporting Graph Viewer:.", false, false, true, false,
          true, false, false, false, true, true);
    _graphUIPrefs = new FaderUIPrefs2();
    _graphUIPrefs.setSliders(false);
    _graphUI = new FaderUI(_graphUIPrefs);
    initGAppFrame(false);
    super.getContentPane().add(getDialogButtons(), BorderLayout.SOUTH);
    _myDialog = new MyDialog(this, false);
  }


  public void setMyGraph(DrawableGraph graph) {
    setDrawableGraph(graph);
  }

  protected void setDrawableGraphHook() { }


  public boolean isGraphValid(DrawableGraph graph) { return (true); }


  protected GTransformer getTransformer() { return(null); }


  protected void initGUIHook() { }


  protected void addToFileMenuHook(JMenu filemenu) { }


  protected GraphPanel initGraphPanel() {
    GraphPanel p = new GraphPanel(_graphUI);
    p.setBackground(Color.white);
    return (p);
  }


  protected GFilterTable getNodeFilterTable() { return(null); }


  protected GFilterTable getEdgeFilterTable() { return(null); }


  protected void addDrawableGraphMouseEventListeners() {
    //
    // NODE
    //
    getGraphPanel().addDrawableGraphMouseEventListener
      ("NODE",
       new DrawableGraphMouseEventListener() {
         public boolean handleDrawableGraphMouseEvent(DrawableGraphMouseEvent e)
         {
           DSGTool tool = null;
           try {
             tool = (DSGTool) getCurrentTool();
           } catch (ClassCastException oops) {}

           if (tool != null) {
             return(tool.handleNodeMouseEvent(e));
           }
           else {
             return(false);
           }
         }
       });
    //
    // NODE_TEXT
    //
    getGraphPanel().addDrawableGraphMouseEventListener
      ("NODE_TEXT",
       new DrawableGraphMouseEventListener() {
         public boolean handleDrawableGraphMouseEvent(DrawableGraphMouseEvent e)
         {
           DSGTool tool = null;
           try {
             tool = (DSGTool) getCurrentTool();
           } catch (ClassCastException shoot) {}

           if (tool != null) {
             return (tool.handleNodeTextMouseEvent(e));
           }
           return (false);
         }
       });
    //
    // EDGE
    //
    getGraphPanel().addDrawableGraphMouseEventListener
      ("EDGE",
       new DrawableGraphMouseEventListener() {
         public boolean handleDrawableGraphMouseEvent(DrawableGraphMouseEvent e)
         {
           DSGTool tool = null;
           try {
             tool = (DSGTool) getCurrentTool();
           } catch (ClassCastException shoot) {}

           if (tool != null) {
             return (tool.handleEdgeMouseEvent(e));
           }
           return (false);
         }
       });
    //
    // EDGE_ICON
    //
    getGraphPanel().addDrawableGraphMouseEventListener
      ("EDGE_ICON",
       new DrawableGraphMouseEventListener() {
         public boolean handleDrawableGraphMouseEvent(DrawableGraphMouseEvent e)
         {
           DSGTool tool = null;
           try {
             tool = (DSGTool)getCurrentTool();
           } catch (ClassCastException darn) {}

           if (tool != null) {
             return (tool.handleEdgeIconMouseEvent(e));
           }
           else {
             return (false);
           }
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
           DSGTool tool = null;
           try {
             tool = (DSGTool) getCurrentTool();
           } catch (ClassCastException shoot) {}

           if (tool != null) {
             return (tool.handleHitTestMissedMouseEvent(e));
           }
           return (false);
         }
       });
    //
    // register Multiple hit test listeners
    //
    getGraphPanel().addDrawableGraphMultipleSelectionEventListener
      (new DrawableGraphMultipleSelectionEventListener() {
          public void handleMultipleHitTestSuccess(DrawableGraphMultipleSelectionEvent evt)
          {
            DSGTool tool = null;
            try {
              tool = (DSGTool) getCurrentTool();
            } catch (ClassCastException shoot) {}

            if (tool != null) {
              tool.handleMultipleHitTestSuccess(evt);
            }
          }

          public void handleMultipleHitTestMissedGraph(Rectangle2D rectangle)
          {
            DSGTool tool = null;
            try {
              tool = (DSGTool) getCurrentTool();
            } catch (ClassCastException shoot) {}

            if (tool != null) {
              tool.handleMultipleHitTestMissedGraph(rectangle);
            }
          }
        });
  }


  protected Component getExtraStatusBarWidget() { return (null); }


  protected Component getDialogButtons() {
    JPanel panel = new JPanel();

    JButton ok = new JButton("Ok");
    JButton cancel = new JButton("Cancel");

    ok.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          _myDialog.fireClickedOk();
        }
      });

    cancel.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          _myDialog.fireClickedCancel();
        }
      });

    panel.add(ok);
    panel.add(cancel);

    return (panel);
  }


  protected boolean mousePressedHook(MouseEvent e) { return(false); }


  protected boolean mouseReleasedHook(MouseEvent e) { return(false); }


  protected void fileOpenHook() { }


  protected void fileNewHook() { }


  protected GTool initSelectTool(Cursor cur) {
    _commonTool = new DSGSelectTool(cur, this);
    return ((GTool)_commonTool);
  }


  protected GTool initTextTool(Cursor cur) {
    return (new GToolAdapter(cur));
  }


  protected GTool initHandTool(Cursor open, Cursor closed) {
    return (new GToolAdapter(open));
  }


  protected GTool initZoomTool(Cursor in,
                               Cursor out,
                               Cursor marquee)
  {
    return (new GToolAdapter(in));
  }


  protected GTool initNodeTool(Cursor cur) {
    return (new DSGNodeTool(cur, this, _commonTool));
  }


  protected GTool initEdgeTool(Cursor cur) {
    return (new DSGEdgeTool(cur, this, _commonTool));
  }


  protected GTool initPanTool(Cursor xy,
                              Cursor x,
                              Cursor y,
                              Cursor n,
                              Cursor w,
                              Cursor s,
                              Cursor r,
                              Cursor nw,
                              Cursor ne,
                              Cursor sw,
                              Cursor se)
  {
    return(new GToolAdapter(xy));
  }


  protected boolean exitHook() {
    return (false);
  }


  /**
   * Get the FaderUIPrefs used by the FaderUI.
   */
  protected FaderUIPrefs2 getFaderUIPrefs() {
    return(_graphUIPrefs);
  }


  /**
   * Add a handler for the dialog buttons ("ok" and "cancel");
   */
  public void addDialogHandler(DialogHandler handler) {
    _myDialog.addDialogHandler(handler);
  }


  /**
   * Remove a handler for the dialog buttons ("ok" and "cancel");
   */
  public void removeDialogHandler(DialogHandler handler) {
    _myDialog.removeDialogHandler(handler);
  }


} // end class "DSGViewer"

