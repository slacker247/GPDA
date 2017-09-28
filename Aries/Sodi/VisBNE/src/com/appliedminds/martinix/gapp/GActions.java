/* -*- Mode: Java; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*-*/
package com.appliedminds.martinix.gapp;


import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.geom.Rectangle2D;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.FileOutputStream;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import javax.swing.AbstractAction;
import javax.swing.AbstractButton;
import javax.swing.Action;
import javax.swing.ButtonGroup;
import javax.swing.ButtonModel;
import javax.swing.ComboBoxModel;
import javax.swing.Icon;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.JRadioButton;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.filechooser.FileFilter;

import com.appliedminds.core.util.IconAndCursorLoader;
import com.appliedminds.martini.DrawableGraph;
import com.appliedminds.martini.DrawableGraphContext;
import com.appliedminds.martini.GraphPanel;
import com.appliedminds.martini.GraphUI;
import com.appliedminds.martini.io.GMLInput;
import com.appliedminds.martini.io.GMLOutput;
import com.appliedminds.martini.io.LayoutLoader;
import com.appliedminds.martini.io.MalformedGMLException;
import com.appliedminds.martinix.*;
import com.appliedminds.martini.NodeIterator;

/**
 * The default set of actions used by the GAppFrame.
 *
 * @author daepark@apmindsf.com
 */
public class GActions {

  /** layout type/description for circular layout */
  public static final String CIRCULAR_LAYOUT     = "circular";

  /** layout type/description for hierarchical layout */
  public static final String HIERARCHICAL_LAYOUT = "hierarchical";

  /** layout type/description for random layout */
  public static final String RANDOM_LAYOUT       = "random";

  /** layout type/description for orthogonal layout */
  public static final String ORTHOGONAL_LAYOUT   = "orthogonal";

  /** layout type/description for symmetric layout */
  public static final String SYMMETRIC_LAYOUT    = "symmetric";

  /** layout type/description for tree layout */
  public static final String TREE_LAYOUT         = "tree";

  private GAppFrame _app;

  private JFileChooser _fileChooser;

  //
  // file menu actions
  //
  private Action _fileNewAction;
  private Action _fileOpenAction;
  private Action _fileSaveAction;
  private Action _fileQueryTopicGraphAction;
  private Action _fileCloseAction;
  private Action _fileExitAction;

  //
  // view menu actions
  //
  private Action _openSidebarAction;
  private Action _closeSidebarAction;
  private Action _viewRefreshAction;

  //
  // tool bar/menu actions
  //
  private Action _selectToolAction;
  private Action _textToolAction;
  private Action _zoomToolAction;
  private Action _handToolAction;
  private Action _nodeToolAction;
  private Action _edgeToolAction;

  //
  // layout menu/list actions
  //
  private Action _fitInWindowAction;
  private Action _circularLayoutAction;
  private Action _hierarchicalLayoutAction;
  private Action _orthogonalLayoutAction;
  private Action _randomLayoutAction;
  private Action _symmetricLayoutAction;
  private Action _treeLayoutAction;

  //
  // a set of radio ButtonGroups associated with
  // the tool Action objects
  //
  private Set _toolButtonGroups;

  //
  // A combination of ComboBoxModel and ButtonGroup objects.
  // The available layouts will be displayed as a combo box
  // or as a radio group in the TMV application.
  //
  private Set _layoutGroups;

  private GTransformer _tranny;


  /**
   * Initialize all TMVActions in the context of the TMV application
   *
   * @param app the app context of TMVActions
   * @param tranny the transformer, can be null.
   */
  public GActions(GAppFrame app, GTransformer tranny) {
    _app = app;
    _toolButtonGroups = new HashSet();
    _layoutGroups = new HashSet();
    _tranny = tranny;
  }


  public Action getFileNewAction() {
    if (_fileNewAction == null) {
      _fileNewAction = new AbstractAction("New") {
          public void actionPerformed(ActionEvent e) {
            // check if we need to save the current graph
            if (_app.isDocumentModified() &&
                confirmDialog(_app.getModifiedSaveQuestion())) {
              _fileSaveAction.actionPerformed(null);
            }

            _app.setDrawableGraph(new DrawableGraph());
            _app.setDocumentState(GAppFrame.DOC_UNMODIFIED);
            _app.fileNewHook();
          }
        };
    }

    return (_fileNewAction);
  }


  /**
   * Open file. Display a file chooser open dialog.
   */
  public Action getFileOpenAction() {
    if (_fileOpenAction == null) {
      _fileOpenAction = new AbstractAction("Open") {
          public void actionPerformed(ActionEvent e)
          {
            // check if we need to save the current graph
            if (_app.isDocumentModified() &&
                confirmDialog(_app.getModifiedSaveQuestion())) {
              _fileSaveAction.actionPerformed(null);
            }

            File file = null;
            JFileChooser fc = getFileChooser();

            while (true) {

              int retVal = fc.showOpenDialog(_app);

              if (retVal == JFileChooser.APPROVE_OPTION) {
                file = fc.getSelectedFile();

                if (file.isFile() && file.exists()) {

                  // load it up!
                  try {
                    LayoutLoader loader = new GLayoutLoader();
                    DrawableGraph graph = loadGraphFromFile(file, loader);
                    // we cannot call gapp's setDrawableGraph() method
                    // from here or else we would get into an infinite
                    // event loop
                    _app.getGraphPanel().stopAnimation();
                    _app.getGraphPanel().stopElementEdit();
                    _app.getGraphPanel().setDrawableGraph(graph);
                    _app.setDrawableGraphHook();

                    // sets graph elements' coordinates
                    _app.loadLayout(loader);

                    _app.fitGraphInWindow();
                    _app.setDocumentState(GAppFrame.DOC_UNMODIFIED);
                    _app.fileOpenHook();
                    _app.getGraphPanel().repaint();
                  }
                  catch (FileNotFoundException e1) {
                    e1.printStackTrace();
                  }
                  catch (IOException e2) {
                    e2.printStackTrace();
                  }
                  catch (MalformedGMLException e3) {
                    e3.printStackTrace();
                  }
                }
                else {
                  JOptionPane.showMessageDialog
                    (null, "Please select a valid graph file");
                  continue;
                }
              }
              break;
            }
          }
        };
    }
    return (_fileOpenAction);
  }

  /**
   * Load a graph from the specified GML file. Also load the layout
   * positions of the graph into the specified graph context object,
   * if present.
   *
   * @param file the GML graph file, most likely written by GMLOutput.
   * @param layoutLoader a LayoutLoader object if you want tol load a
   * saved layout, otherwise null.
   * @see GMLInput#parseGML
   */
  protected DrawableGraph loadGraphFromFile(File file,
                                            LayoutLoader layoutLoader)
    throws IOException, MalformedGMLException, FileNotFoundException
  {
    BufferedInputStream in = null;
    DrawableGraph g = null;
    try {
      in = new BufferedInputStream(new FileInputStream(file));
      g = GMLInput.parseGML(in, layoutLoader);
    }
    catch(IOException e) {
      throw(e);
    }
    finally {
      try {
        in.close();
      }
      catch(Exception ignore) { }
    }

    if (_tranny != null) {
      _tranny.transform(g);
    }

    if (g == null) {
      throw (new MalformedGMLException("Unable to parse input GML file " +
                                       file.toString()));
    }

    GraphPanel graphPanel = _app.getGraphPanel();
    if (graphPanel == null) {
      throw (new RuntimeException("no graph panel in app"));
    }

    GraphUI graphUI = graphPanel.getGraphUI();
    if (graphUI == null) {
      throw (new RuntimeException("no graph UI in app"));
    }

    if (graphUI.validate(g) == false) {
      throw (new MalformedGMLException("Invalid GML input file for the current GraphUI"));
    }

    return g;
  }


  /**
   * Save file. Display a file chooser save dialog.
   */
  public Action getFileSaveAction() {
    if (_fileSaveAction == null) {
      _fileSaveAction = new AbstractAction("Save") {
          public void actionPerformed(ActionEvent e) {
            File file = null;
            JFileChooser fc = getFileChooser();
            while (true) {
              if (_app.getGraphPanel().getDrawableGraph() == null) {
                return;  // nothing to save
              }
              int retVal = fc.showSaveDialog(_app);

              if (retVal == JFileChooser.APPROVE_OPTION) {
                file = fc.getSelectedFile();
                String path = file.getAbsolutePath();

                String extension = getExtension(file);
                if (extension == null) {
                  // no exension has been given
                  // apply the default gml extension
                  file = new File(path + ".gml");
                }

                if (file.exists())
                {
                  String question = "File already exists. Overwrite?";
                  if (!confirmDialog(question))
                  {
                    continue;
                  }
                }

                if (file != null) {
                  DrawableGraph g = _app.getGraphPanel().getDrawableGraph();
                  DrawableGraphContext ctx =
                    _app.getGraphPanel().getDrawableGraphContext();
                  FileOutputStream out = null;
                  try {
                    out = new FileOutputStream(file);
                    if (_tranny != null) {
                      _tranny.transform(g);
                    }
                    GMLOutput.writeGML(g, out, GMLOutput.ROYERE_HACK, ctx);
                    if (_tranny != null) {
                      _tranny.transform(g);
                    }
                    _app.setDocumentState(GAppFrame.DOC_UNMODIFIED);
                  }
                  catch(IOException err) {
                    JOptionPane.showMessageDialog(null,
                                                  err.getMessage(),
                                                  "Save Error",
                                                  JOptionPane.WARNING_MESSAGE);
                    continue;
                  }
                  finally {
                    try {
                      if (out != null) {
                        out.close();
                      }
                    }
                    catch(Exception ignore) { }
                  }
                }
              }

              break;
            }

          }
        };
    }
    return (_fileSaveAction);
  }



  /**
   * Close. Exit for now.
   */
  public Action getFileCloseAction() {
    if (_fileCloseAction == null) {
      _fileCloseAction = new AbstractAction("Close") {
          public void actionPerformed(ActionEvent e) {
            exitProgram();
          }
        };
    }
    return (_fileCloseAction);
  }


  /**
   * Exit. Display confirm dialog.
   */
  public Action getFileExitAction() {
    if (_fileExitAction == null) {
      _fileExitAction = new AbstractAction("Exit") {
          public void actionPerformed(ActionEvent e) {
            exitProgram();
          }
        };
    }
    return (_fileExitAction);
  }


  /**
   * @return the Action object responsible for closing the sidebar
   * in the application.
   */
  public Action getCloseSidebarAction() {
    if (_closeSidebarAction == null) {
      _closeSidebarAction = new SidebarAction(false);
    }
    return (_closeSidebarAction);
  }


  /**
   * @return the Action object responsible for opening the sidebar
   * in the application.
   */
  public Action getOpenSidebarAction() {
    if (_openSidebarAction == null) {
      _openSidebarAction = new SidebarAction(true);
    }
    return (_openSidebarAction);
  }


  /**
   * @return the Action object responsible for refreshing the GraphPanel.
   */
  public Action getViewRefreshAction() {
    if (_viewRefreshAction == null) {
      _viewRefreshAction = new AbstractAction("Refresh") {
          public void actionPerformed(ActionEvent e) {
            _app.fitGraphPanelInViewport();
          }
        };
    }
    return (_viewRefreshAction);
  }


  /**
   * @return the handler for selecting the select tool
   *
   * @param group A group maintaining the list of tools that
   *            want to be synchronized with each other. If
   *            one group selects one tool, all other registered
   *            groups will select the same tool.
   *            Null if don't want to be in sync.
   */
  public Action getSelectToolAction(ButtonGroup group) {
    addToolButtonGroup(group);
    if (_selectToolAction == null) {
      _selectToolAction = new AbstractAction("Select") {
          public void actionPerformed(ActionEvent e) {
            // notify all radio button groups to select the radio
            // representing this action
            syncToolButtonGroups(e.getSource());

            _app.useSelectTool();
          }
        };
    }
    return (_selectToolAction);
  }


  /**
   * @return the handler for selecting the text tool
   *
   * @param group A group maintaining the list of tools that
   *            want to be synchronized with each other. If
   *            one group selects one tool, all other registered
   *            groups will select the same tool.
   *            Null if don't want to be in sync.
   */
  public Action getTextToolAction(ButtonGroup group) {
    addToolButtonGroup(group);
    if (_textToolAction == null) {
      _textToolAction = new AbstractAction("Text") {
          public void actionPerformed(ActionEvent e) {
            // notify all radio button groups to select the radio
            // representing this action
            syncToolButtonGroups(e.getSource());

            _app.useTextTool();
          }
        };
    }
    return (_textToolAction);
  }


  /**
   * @return the handler for selecting the zoom tool
   *
   * @param group A group maintaining the list of tools that
   *            want to be synchronized with each other. If
   *            one group selects one tool, all other registered
   *            groups will select the same tool.
   *            Null if don't want to be in sync.
   */
  public Action getZoomToolAction(ButtonGroup group) {
    addToolButtonGroup(group);
    if (_zoomToolAction == null) {
      _zoomToolAction = new AbstractAction("Zoom") {
          public void actionPerformed(ActionEvent e)
          {
            // notify all radio button groups to select the radio
            // representing this action
            syncToolButtonGroups(e.getSource());

            _app.useZoomTool();
          }
        };
    }
    return (_zoomToolAction);
  }


  /**
   * @return the handler for selecting the hand tool
   *
   * @param group A group maintaining the list of tools that
   *            want to be synchronized with each other. If
   *            one group selects one tool, all other registered
   *            groups will select the same tool.
   *            Null if don't want to be in sync.
   */
  public Action getHandToolAction(ButtonGroup group) {
    addToolButtonGroup(group);
    if (_handToolAction == null) {
      _handToolAction = new AbstractAction("Hand") {
          public void actionPerformed(ActionEvent e) {
            // notify all radio button groups to select the radio
            // representing this action
            syncToolButtonGroups(e.getSource());

            _app.useHandTool();
          }
        };
    }
    return (_handToolAction);
  }


  /**
   * @return the handler for selecting the node tool
   *
   * @param group A group maintaining the list of tools that
   *            want to be synchronized with each other. If
   *            one group selects one tool, all other registered
   *            groups will select the same tool.
   *            Null if don't want to be in sync.
   */
  public Action getNodeToolAction(ButtonGroup group) {
    addToolButtonGroup(group);
    if (_nodeToolAction == null) {
      _nodeToolAction = new AbstractAction("Node") {
          public void actionPerformed(ActionEvent e) {
            // notify all radio button groups to select the radio
            // representing this action
            syncToolButtonGroups(e.getSource());

            _app.useNodeTool();
          }
        };
    }
    return (_nodeToolAction);
  }


  /**
   * @return the handler for selecting the edge tool
   *
   * @param group A group maintaining the list of tools that
   *            want to be synchronized with each other. If
   *            one group selects one tool, all other registered
   *            groups will select the same tool.
   *            Null if don't want to be in sync.
   */
  public Action getEdgeToolAction(ButtonGroup group) {
    addToolButtonGroup(group);
    if (_edgeToolAction == null) {
      _edgeToolAction = new AbstractAction("Edge") {
          public void actionPerformed(ActionEvent e) {
            // notify all radio button groups to select the radio
            // representing this action
            syncToolButtonGroups(e.getSource());

            _app.useEdgeTool();
          }
        };
    }
    return (_edgeToolAction);
  }



  /**
   * @return the fit in window action
   *
   */
  public Action getFitInWindowAction() {
    if (_fitInWindowAction == null)
    {
      _fitInWindowAction = new AbstractAction("Fit in Window") {
          public void actionPerformed(ActionEvent e) {
            _app.fitGraphInWindow();
          }
        };
    }
    return (_fitInWindowAction);
  }



  /**
   * @return the proper layout Action object to handle a graph
   * layout request for the given layout type. Null if unrecognized
   * layout type.
   *
   * @param layout CIRCULAR_LAYOUT, HIERARCHICAL_LAYOUT, ORTHOGONAL_LAYOUT,
   *               RANDOM_LAYOUT, SYMMETRIC_LAYOUT, or TREE_LAYOUT.
   * @param group A group maintaining the list of layouts that
   *         want to be synchronized with each other. Currently
   *         only JComboBox and ButtongGroup objects are supported.
   *         Null if don't want to be in sync.
   */
  public Action getLayoutAction(String layout, Object group) {
    if (group != null) {
      _layoutGroups.add(group);
    }

    Action action = null;
    if (HIERARCHICAL_LAYOUT.equals(layout)) {
      action = getHierarchicalLayoutAction();
    }
    else if (SYMMETRIC_LAYOUT.equals(layout)) {
      action = getSymmetricLayoutAction();
    }
    else if (CIRCULAR_LAYOUT.equals(layout)) {
      action = getCircularLayoutAction();
    }
    else if (ORTHOGONAL_LAYOUT.equals(layout)) {
      action = getOrthogonalLayoutAction();
    }
    else if (RANDOM_LAYOUT.equals(layout)) {
      action = getRandomLayoutAction();
    }
    else if (TREE_LAYOUT.equals(layout)) {
      action = getTreeLayoutAction();
    }
    return (action);
  }



  /**
   * Circular layout action.
   */
  private Action getCircularLayoutAction() {
    if (_circularLayoutAction == null) {
      _circularLayoutAction = new AbstractAction(CIRCULAR_LAYOUT) {
          public void actionPerformed(ActionEvent e) {
            // notify all groups to select the layout
            // representing this action
            syncLayoutGroups(e.getSource());

            processLayoutAction(CIRCULAR_LAYOUT);
          }
        };
    }
    return (_circularLayoutAction);
  }


  /**
   * Hierarchical layout action.
   */
  private Action getHierarchicalLayoutAction() {
    if (_hierarchicalLayoutAction == null) {
      _hierarchicalLayoutAction = new AbstractAction(HIERARCHICAL_LAYOUT) {
          public void actionPerformed(ActionEvent e) {
            // notify all groups to select the layout
            // representing this action
            syncLayoutGroups(e.getSource());

            processLayoutAction(HIERARCHICAL_LAYOUT);
          }
        };
    }
    return (_hierarchicalLayoutAction);
  }


  /**
   * Orthogonal layout action.
   */
  private Action getOrthogonalLayoutAction() {
    if (_orthogonalLayoutAction == null) {
      _orthogonalLayoutAction = new AbstractAction(ORTHOGONAL_LAYOUT) {
          public void actionPerformed(ActionEvent e) {
            // notify all groups to select the layout
            // representing this action
            syncLayoutGroups(e.getSource());

            processLayoutAction(ORTHOGONAL_LAYOUT);
          }
        };
    }
    return (_orthogonalLayoutAction);
  }


  /**
   * Random layout action.
   */
  private Action getRandomLayoutAction() {
    if (_randomLayoutAction == null) {
      _randomLayoutAction = new AbstractAction(RANDOM_LAYOUT) {
          public void actionPerformed(ActionEvent e) {
            // notify all groups to select the layout
            // representing this action
            syncLayoutGroups(e.getSource());

            processLayoutAction(RANDOM_LAYOUT);
          }
        };
    }
    return (_randomLayoutAction);
  }


  /**
   * Symmetric layout action.
   */
  private Action getSymmetricLayoutAction() {
    if (_symmetricLayoutAction == null) {
      _symmetricLayoutAction = new AbstractAction(SYMMETRIC_LAYOUT) {
          public void actionPerformed(ActionEvent e) {
            // notify all groups to select the layout
            // representing this action
            syncLayoutGroups(e.getSource());

            processLayoutAction(SYMMETRIC_LAYOUT);
          }
        };
    }
    return (_symmetricLayoutAction);
  }


  /**
   * Tree layout action.
   */
  private Action getTreeLayoutAction() {
    if (_treeLayoutAction == null) {
      _treeLayoutAction = new AbstractAction(TREE_LAYOUT) {
          public void actionPerformed(ActionEvent e) {
            // notify all groups to select the layout
            // representing this action
            syncLayoutGroups(e.getSource());

            processLayoutAction(TREE_LAYOUT);
          }
        };
    }
    return (_treeLayoutAction);
  }

  /**
   * Tell the app to apply the specified layout.
   */
  private void processLayoutAction(String layout) {
    _app.setLayout(layout);
  }


  /**
   * Synchronize all the registered layout groups maintainng a set
   * of the possible graph layout styles, so that all
   * the groups will point to the same layout.
   */
  private void syncLayoutGroups(Object source) {
    JComboBox comboBox = null;
    AbstractButton button = null;
    Action action = null;

    if (source instanceof JComboBox) {
      comboBox = (JComboBox)source;
      action = comboBox.getAction();
    }

    else if (source instanceof AbstractButton) {
      button = (AbstractButton)source;
      action = button.getAction();
    }

    if (action != null) {
      Iterator itr = _layoutGroups.iterator();
      while (itr.hasNext()) {
        Object g = itr.next();
        if (g instanceof JComboBox && g != comboBox) {
          // FIX: this may re-trigger a layout Action by the
          // LayoutComboBox. Temporary fix is
          // to keep track of the current layout, an don't process
          // if current layout and the requested layout is the same.
          ((JComboBox)g).setSelectedItem(action.getValue(Action.NAME));
        }
        else if (g instanceof ButtonGroup) {
          Enumeration enum = ((ButtonGroup)g).getElements();
          while (enum.hasMoreElements()) {
            AbstractButton b = (AbstractButton)enum.nextElement();
            if (action == b.getAction() && b != button) {
              ((ButtonGroup)g).setSelected(b.getModel(), true);
              //              b.repaint();
            }
          }
        }
      }
    }
  }


  /**
   * Synchronize access to the synchronized set.
   */
  private void addToolButtonGroup(ButtonGroup group) {
    if (group != null) {
      _toolButtonGroups.add(group);
    }
  }


  /**
   * Synchronize all the ButtonGroup objects maintaining a set
   * of tool radio buttons (i.e., as menu items or tool bar buttons),
   * so that all the groups will point to the same tool.
   */
  private void syncToolButtonGroups(Object source) {
    if (source instanceof AbstractButton) {
      AbstractButton button = (AbstractButton)source;
      Action action = button.getAction();
      if (action != null) {
        Iterator itr = _toolButtonGroups.iterator();
        while (itr.hasNext()) {
          ButtonGroup bg = (ButtonGroup)itr.next();
          Enumeration enum = bg.getElements();
          while (enum.hasMoreElements()) {
            AbstractButton b = (AbstractButton)enum.nextElement();
            if (action == b.getAction() && button != b) {
              bg.setSelected(b.getModel(), true);
              b.repaint();
            }
          }
        }

      }
    }
  }


  /**
   * Lazy initalization of the file chooser shared by open/save handlers.
   */
  private JFileChooser getFileChooser() {
    if (_fileChooser == null) {
      _fileChooser = new JFileChooser(System.getProperty("user.dir"));
      _fileChooser.setFileFilter(new GMLGraphFilter());
      _fileChooser.setAcceptAllFileFilterUsed(false);
    }
    return (_fileChooser);
  }



  private static String getExtension(File f) {
    String ext = null;
    String s = f.getName();
    int i = s.lastIndexOf('.');

    if (i > 0 && i < s.length() - 1) {
      ext = s.substring(i+1).toLowerCase();
    }
    return ext;
  }


  /**
   * Check if the current document needs to be saved, before exiting.
   */
  protected void exitProgram()
  {
    if (_app.isDocumentModified())
    {
      String question = _app.getExitQuestion();

      if (_app.getDocumentState() == _app.DOC_MODIFIED)
      {
        question = _app.getModifiedSaveQuestion();
      }

      if (confirmDialog(question)) {
        _fileSaveAction.actionPerformed(null);
      }

      _app.setDocumentState(GAppFrame.DOC_NONE);
    }

    if ((_app.isApplet() == true) || (!_app.exitHook())) {
      _app.setVisible(false);
    }
    else {
      System.exit(0);
    }
  }



  /**
   * Ask the user a question using a popup dialog box.
   *
   * @param question the question to display in the dialog box.
   * @return true if user selected yes.
   */
  private boolean confirmDialog(String question)
  {
    int confirm = JOptionPane.showConfirmDialog(null,
                                                question,
                                                "Warning",
                                                JOptionPane.YES_NO_OPTION,
                                                JOptionPane.WARNING_MESSAGE);

    return (confirm == JOptionPane.YES_OPTION);
  }



  /**
   * A filter for GML graph files to be used with a JFileChooser
   */
  private class GMLGraphFilter extends FileFilter {

    /**
     * Accept all directories and all gml files.
     */
    public boolean accept(File f) {
      if (f.isDirectory()) {
        return (true);
      }

      String extension = getExtension(f);
      if (extension != null) {
        if (extension.equals("gml")) {
          return (true);
        }
      }
      return (false);
    }

    /**
     * @return the description of this filter
     */
    public String getDescription() {
      return ("GML Graph files (*.gml)");
    }

  } // end class GMLGraphFilter


  /**
   * Open/close sidebar handler
   */
  private class SidebarAction extends AbstractAction {

    private boolean __open;

    public SidebarAction(boolean open) {
      super("Sidebar");
      __open = open;
      IconAndCursorLoader iconLoader = _app.getIconAndCursorLoader();
      if (iconLoader != null) {
        Icon icon = null;
        if (__open == true) {
          icon = iconLoader.loadImageIcon(
                                          "com/appliedminds/martinix/gapp/resources/open_sidebar.png");
        }
        else {
          icon = iconLoader.loadImageIcon(
                                          "com/appliedminds/martinix/gapp/resources/close_sidebar.png");
        }
        this.putValue(AbstractAction.SMALL_ICON, icon);
      }
    }

    public void actionPerformed(ActionEvent e) {
      if (__open == true) {
        // open the sidebar
        _app.openSidebar();
      }
      else {
        // close the sidebar
        _app.closeSidebar();
      }
    }

  }  // end class SidebarAction


} // end class TMVActions
