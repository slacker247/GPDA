/**
 * Title:        Browser<p>
 * Description:  This allows you to browse a library of knowledge bases.<p>
 * Copyright:    Copyright (c) Warren Shen<p>
 * Company:      Stanford Medical Informatics<p>
 * @author Warren Shen
 * @version 1.0
 */
package browser.ui;

import browser.Browser;
import java.awt.*;
import browser.util.*;
import java.awt.event.*;
import javax.swing.JPanel;
import java.util.*;
import java.io.*;
import edu.stanford.smi.protege.util.*;
import javax.swing.*;
import edu.stanford.smi.protege.resource.*;
import browser.model.*;
import browser.event.*;
import browser.action.*;
import att.grappa.*;
import dfki.protege.ontoviz_tab.GraphPanel;

public class DependenciesLattice extends AbstractLibraryView implements GrappaConstants, GrappaListener {
  private String _inputFile, _dotFile;

  private static final String NEED_TO_UPDATE = "Library has changed - update the graph first.";

  private String _message = "";
  private boolean _isUpdated;

  private ImageComponent _image;
  private Hashtable _openedProjects;
  private GraphPanel _graphPanel;
  private Graph _graph = null;

  public void initialize() {
    _isUpdated = false;
    _openedProjects = new Hashtable(_library.size() * 2);
    setLayout(new BorderLayout());
    _graphPanel = new GraphPanel();
    LabeledComponent c = new LabeledComponent("Dependencies", _graphPanel);
    add(c, BorderLayout.CENTER);
    JPanel panel = new JPanel();
    panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
    panel.add(ComponentFactory.createButton(new UpdateDependenciesAction(this)));
    panel.add(Box.createHorizontalGlue());
    c.setHeaderComponent(panel);
  }

  public static void main(String[] args) {
    JFrame frame = new JFrame();
    frame.getContentPane().add(new DependenciesLattice());
    frame.pack();
    frame.show();

  }

  private void dot(String inputFileName, String fileName, String option, boolean wait) throws Exception {
    String commandPath = Browser.getProperties().getProperty(Browser.DOT_COMMAND_PROPERTY, "dot");
    String[] command =
        new String[] {commandPath, option, inputFileName, "-o", fileName};
    try {
      Process process = Runtime.getRuntime().exec(command);
      if (wait) {
        process.waitFor();
      }
    } catch(Exception e) {
      Warning.showWarning("Could not generate graph", "Warning: Could not invoke command '" + commandPath + "' when generating dependencies graph");
    }
  }

  private void export() {
    try {
      File file;
      file = File.createTempFile("states", ".dot-input");
      _inputFile = file.getPath();
      _dotFile = FileUtilities.getDirectory(file.getPath()) + "\\states.dot";
      export(FileUtilities.createWriter(file));
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  private void export(Writer writer) {
    try {
      PrintWriter pw;
      pw = new PrintWriter(writer);
      pw.println("digraph test {\n");
      exportDependencies(pw);
      pw.println("}\n");

      pw.flush();
      pw.close();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  private void exportDependencies(PrintWriter pw) {
    int i;
    Hashtable pairs = new Hashtable();
    Hashtable nodes = new Hashtable();
    ArrayList queue = new ArrayList();
    Pair pair;
    LibraryItem item;

    if(_library.size() == 0) {
      return;
    }
    for(i = 0; i < _library.size(); i++) {
      item = _library.getIthKBSummary(i);
      if(!item.isIncluded() && item.isActive()) {
        queue.add(new Pair(item, item, true));
      }
    }
    while(!queue.isEmpty()) {
      pair = (Pair)queue.remove(0);
      handleOnePair(pw, pair, pairs, nodes);
      enqueueDependencies(queue, pair.getItem());
    }
  }

  private void handleOnePair(PrintWriter pw, Pair pair, Hashtable pairs, Hashtable nodes) {
    LibraryItem item = pair.getItem();
    if(!nodes.contains(item.getIdentifier())) {
      drawNode(pw, item, pair.itemInLibrary());
      nodes.put(item.getIdentifier(), item);
    }
    if(!pairs.containsKey(pair.getKey())) {
      drawPair(pw, pair);
      pairs.put(pair.getKey(), pair);
    }
  }

  private void drawNode(PrintWriter pw, LibraryItem item, boolean isInLibrary) {
    String color;
    String line;
    if(isInLibrary && !item.isIncluded()) {
      color = "limegreen";
    } else {
      color = "lightslategray";
    }
    line = "\"" + item.getName() + "\"";
    line += "[style = filled, color = ";
    line += color + ", ";
    line += "_id = \"" + item.getIdentifier() + "\", ";
    line += "_name = \"" + item.getName() + "\"]\n";
    pw.print(line);
  }

  private void enqueueDependencies(ArrayList queue, LibraryItem item) {
    LibraryItem dependent;
    String dependentIdentifier;
    Collection dependencies = item.getDirectDependenciesIds();
    Iterator iter = dependencies.iterator();
    Pair pair;
    while(iter.hasNext()) {
      dependentIdentifier = (String)iter.next();
      if(_library.isInLibrary(dependentIdentifier)) {
        pair = new Pair(_library.getLibraryItem(dependentIdentifier), item, true);
        queue.add(pair);
      } else {
        dependent = (LibraryItem)CollectionUtilities.getSoleItem(item.createNewLibraryItems(
          CollectionUtilities.createCollection(dependentIdentifier), true, false));
        _library.addLibraryItem(dependent, true, false);
        pair = new Pair(dependent, item, false);
        queue.add(pair);
      }
    }
  }

  private void drawPair(PrintWriter pw, Pair pair) {
    if(pair.getReferencer() == pair.getItem()) {
      return;
    }
    pw.println("\"" + pair.getItem().getName() + "\" -> " +
      "\"" + pair.getReferencer().getName() + "\";\n");
  }

  private String extractName(String filePath) {
    return FileUtilities.getBaseName(FileUtilities.getName(filePath));
  }

  public void updateDependencies() {
    WaitCursor waitCursor = new WaitCursor(BrowserManager.getBrowserManager().getRootPane());
    try {
      export();
      dot(_inputFile, _dotFile, "", true);
      FileInputStream input = new FileInputStream(_dotFile);
      Parser graphParser = new Parser(input, System.err);
      graphParser.parse();
      _graph = graphParser.getGraph();
      _graphPanel.setGraph(_graph);
      _graphPanel.addGrappaListener(this);
     setUpdated(true);
     repaint();
    } catch (Exception e) {
      e.printStackTrace();
    } finally {
      waitCursor.hide();
    }
  }

  private void setUpdated(boolean isUpdated) {
    _isUpdated = isUpdated;
    if(isUpdated) {
      _message = "";
    } else {
      _message = this.NEED_TO_UPDATE;
    }
    repaint();
  }

  public void paint(Graphics g) {
    super.paint(g);
    g.drawString(_message, 7, 3 * g.getFontMetrics().getAscent());
  }

  public void selectionChanged(LibraryEvent event) {
    if(_graph == null) return;
    if(!_isUpdated) {
     _message = this.NEED_TO_UPDATE;
     repaint();
     return;
    }

    Collection col = _library.getSelectedKBSummaries();
    if(col == null) return;
    Attribute attrib;
    Enumeration enum;
    Element elem;
    String id, itemId;
    Iterator iter = col.iterator();
    ArrayList list = new ArrayList();
    _graphPanel.deselectAll();
    while(iter.hasNext()) {
      enum = _graph.nodeElements();
      itemId = ((LibraryItem)iter.next()).getIdentifier();
      while(enum.hasMoreElements()) {
        elem = ((Element)enum.nextElement());
        attrib = elem.getAttribute("_id");
        if(attrib != null) {
          id = attrib.getValue().toString();
          if(id.equals(itemId)) {
            list.add(elem.getName());
          }
        }
      }
    }
    _graphPanel.selectMultiple(list);
  }

  public void libraryModified(LibraryEvent event) {
    setUpdated(false);
    repaint();
  }
  public void librarySaved(LibraryEvent event) {}
  public void libraryClosed(LibraryEvent event) {}

  public String getConfigurationString() {
    return "";
  }
  public void loadConfigurationString()
  {}

  public void grappaClicked(Subgraph subg, Element elem, GrappaPoint pt, int modifiers, GrappaPanel panel) {
  }

  public void grappaPressed(Subgraph subg, Element elem, GrappaPoint pt,
      int modifiers, GrappaPanel panel) {
    if(elem == null || elem.getAttribute("_id") == null) {
      return;
    }
    if(_isUpdated) {
      String identifier = elem.getAttribute("_id").getValue().toString();
      LibraryItem item = _library.getLibraryItem(identifier);
      if(!item.isIncluded()) {
        _library.setSelectedKBSummary(_library.getLibraryItem(identifier));
      }
    } else {
      _message = this.NEED_TO_UPDATE;
      repaint();
    }
  }

  public void grappaReleased(Subgraph subg, Element elem, GrappaPoint pt, int modifiers, Element pressedElem, GrappaPoint pressedPt, int pressedModifiers, GrappaBox outline, GrappaPanel panel) {
  }

  public void grappaDragged(Subgraph subg, GrappaPoint currentPt, int currentModifiers, Element pressedElem, GrappaPoint pressedPt, int pressedModifiers, GrappaBox outline, GrappaPanel panel) {
  }

  public String grappaTip(Subgraph subg, Element elem, GrappaPoint pt, int modifiers, GrappaPanel panel) {
    if(elem != null && elem.getAttribute("_name") != null) {
      return elem.getAttribute("_name").getValue().toString();
    } else {
      return "";
    }
  }


  public String getName() {
    return "Dependencies Lattice";
  }

  public AbstractValidatableComponent getConfigurationPanel(){return null;}

  private class Pair {
    private LibraryItem _item, _referencer;
    private boolean _itemInLibrary;
    public Pair(LibraryItem item, LibraryItem referencer, boolean itemInLibrary) {
      _item = item;
      _referencer = referencer;
      _itemInLibrary = itemInLibrary;
    }

    public LibraryItem getItem() {
      return _item;
    }

    public LibraryItem getReferencer() {
      return _referencer;
    }

    public boolean itemInLibrary() {
      return _itemInLibrary;
    }

    public String getKey() {
      return _item.getIdentifier()+_referencer.getIdentifier();
    }
  }
}