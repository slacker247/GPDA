/**
 * Title:        <p>
 * Description:  <p>
 * Copyright:    Copyright (c) <p>
 * Company:      <p>
 * @author
 * @version 1.0
 */
package browser.model;

import browser.util.*;
import browser.ui.*;
import browser.Browser;
import javax.swing.plaf.basic.*;
import browser.ui.BrowserManager;
import java.util.*;
import java.io.*;
import java.lang.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;
import dfki.protege.ontoviz_tab.*;
import javax.swing.*;
import att.grappa.*;
import java.awt.BorderLayout;
import java.awt.event.*;
import java.awt.Graphics;
import java.awt.Dimension;

public class KBSummary extends LibraryItem {
  private JComponent _thumbnail;
  private Collection _dependencies = null;
  private Collection _directDependencies = null;
  private Hashtable _attributes = new Hashtable();
  private ArrayList _attributeNames = new ArrayList();
  private KnowledgeBaseFactory _factory = null;
  private StatusBox _statusBox;
  private Project _p;
  private static int _levelsForThumbnail = 3;

  private static final String INCLUDED = "(INCLUDED)";

  public KBSummary() {
    super();
  }

  private void loadFromProject(String projectFilePath, boolean isIncluded) {
    if(projectFilePath == null) {
      return;
    }
    Collection errors = new ArrayList();
    Project p = Project.loadProjectFromFile(projectFilePath, errors);
    if (errors.size() == 0) {
      init(p, isIncluded);
    } else {
      Iterator i = errors.iterator();
      System.out.println("ERRORS while loading "+projectFilePath+":");
      int num = 1;
      while (i.hasNext()) {
        System.out.println(num++ + ". Error: " + i.next());
      }
      _name = _filePath = "Could not load: " + projectFilePath;
    }
    _p = p;
  }

  public void init(Project p, boolean isIncluded) {
    _isIncluded = isIncluded;
    _isActive = !isIncluded;
    _dependencies = new ArrayList();
    _directDependencies = new ArrayList();
    if(p == null) {
      _filePath = "Could not load";
      return;
    }
    _factory = p.getKnowledgeBaseFactory();
    _filePath = p.getProjectFilePath();
    _name = FileUtilities.getName(_filePath);
    initDependencies(p);
    initAttributes(p);
    initThumbnail(p);
  }

  public KnowledgeBaseFactory getKnowledgeBaseFactory() {
    return _factory;
  }

  public Collection createNewLibraryItems(Collection identifiers, boolean isIncluded, boolean isActive) {
    Iterator iter = identifiers.iterator();
    String identifier;
    KBSummary newItem;
    ArrayList results = new ArrayList();
    while(iter.hasNext()) {
      identifier = (String)iter.next();
      newItem = (KBSummary) new KBSummary();
      newItem.loadFromProject(identifier, isIncluded);
      newItem.setActive(isActive);
      newItem.setIncluded(isIncluded);
      results.add(newItem);
    }
    return results;
  }

  public Collection getUserLibraryItems() {
    Iterator iter;
    ArrayList items = new ArrayList();
    String path;
    KBSummary item;
    iter = BrowserManager.launchFileChooser("Protege", ".pprj").iterator();
    while(iter.hasNext()) {
      path = (String)iter.next();
      item = new KBSummary();
      item.loadFromProject(path, false);
      items.add(item);
    }
    return items;
  }

  public String getTypeName() {
    return "Protege";
  }

  public String getIdentifier() {
    return _filePath;
  }

  public ArrayList getSummaryAttributeNames() {
    ArrayList list = new ArrayList();
    for(int i = 0; i < SummaryConstants.DEFAULT_SUMMARY_ATTRIBUTE_NAMES.length; i++) {
      list.add(SummaryConstants.DEFAULT_SUMMARY_ATTRIBUTE_NAMES[i]);
    }
    return list;
  }

  public ArrayList getAttributeNames() {
    return _attributeNames;
  }

  public Collection getAttributeValues(Object attributeName) {
    return (Collection)_attributes.get(attributeName);
  }

  public Collection getDirectDependenciesIds() {
    return _directDependencies;
  }

  public Collection getAllDependenciesIds() {
    return _dependencies;
  }

  public String getNameFromIdentifier(String id) {
    if(id == null) {
      System.out.println("id is null");
      return "";
    }
    return FileUtilities.getName(id);
  }

  public JComponent getThumbnail() {
    return this._thumbnail;
  }

  public Project getProject() {
    return _p;
  }

  private void initDependencies(Project p) {
    String dir = FileUtilities.getDirectory(p.getProjectFilePath()) + "\\";
    _dependencies = formatDependencies(p.getIncludedProjects(), dir);
    _directDependencies = formatDependencies(p.getDirectIncludedProjects(), dir);
  }

  private ArrayList formatDependencies(Collection projects, String dir) {
    ArrayList dependencies = new ArrayList();
    if(projects == null) return dependencies;
    Iterator iter = projects.iterator();
    String dependent;
    while(iter.hasNext()) {
      dependent = (String)iter.next();
      if(FileUtilities.getDirectory(dependent) == null) {
        dependencies.add(dir.concat(dependent));
      } else {
        dependencies.add(dependent);
      }
    }
    return dependencies;
  }

  private void initThumbnail(Project p) {
    Collection c = this.getAttributeValues(SummaryConstants.THUMBNAIL_FILE);
    String name;
    if(c != null && !c.isEmpty()) {
      name = (String)CollectionUtilities.getFirstItem(c);
      name = p.getProjectDirectory() + "\\" + name;
      browser.ui.ImageComponent _image = new browser.ui.ImageComponent();
      _image.setImage(name);
      _thumbnail = _image;
    } else {
      _thumbnail = new Thumbnail(p);
    }
  }

  private void initAttributes(Project p) {
    Instance instance = p.getKnowledgeBase().getInstance(SummaryConstants.SUMMARY_INSTANCE_NAME);
    if(instance == null) {
      return;
    }
    Iterator iter = instance.getOwnSlots().iterator();
    Slot slot;
    Collection slotValues;

    while(iter.hasNext()) {
      slot = (Slot)iter.next();
      slotValues = instance.getOwnSlotValues(slot);
      _attributes.put(slot.getName(), slotValues);
      _attributeNames.add(slot.getName());
    }
  }

  private Collection querySlotNames(String slotName) {
    KnowledgeBase kb = _p.getKnowledgeBase();
    Collection col = kb.getFrameNameMatches("*" + slotName, 99999);
    Frame f;
    Iterator iter = col.iterator();
    ArrayList list = new ArrayList();
    while(iter.hasNext()) {
      f = (Frame)iter.next();
      if(f instanceof Slot) {
        if(!f.isIncluded()) {
          list.add(f.getBrowserText());
        }
      }
    }
    return list;
  }

  private Collection queryClassNames(String clsName) {
    KnowledgeBase kb = _p.getKnowledgeBase();
    Collection col = kb.getClsNameMatches("*" + clsName, 999999999);
    Cls c;
    Iterator iter = col.iterator();
    ArrayList list = new ArrayList();
    while(iter.hasNext()) {
      c = (Cls)iter.next();
      if(!c.isIncluded()) {
        list.add(c.getBrowserText());
      }
    }
    return list;
  }

  private Collection queryAttributeNames(String attributeName) {
    String name, attrib;
    int i;
    attrib = attributeName.trim();
    ArrayList list = new ArrayList();
    Enumeration keys = _attributes.keys();
    while(keys.hasMoreElements()) {
      name = ((String)keys.nextElement());
      for(i = 0; i <= name.length() - attrib.length(); i++) {
        if(name.toLowerCase().regionMatches(false, i, attrib.toLowerCase(), 0, attrib.length())) {
          Iterator iter = ((Collection)_attributes.get(name)).iterator();
          while(iter.hasNext()) {
            list.add(name + "  (value = \"" + iter.next() + "\")");
          }
        }
      }
    }
    return list;
  }


  public Collection getQueryTypeNames() {
    ArrayList names = new ArrayList();
    names.add("Classes");
    names.add("Annotation");
    names.add("Slots");
    return names;
  }

  public Collection query(String query, String queryTypeName, boolean showIncluded) {
    Collection result = new ArrayList();

    if(queryTypeName.equals("Slots")) { result = querySlotNames(query); }
    if(queryTypeName.equals("Classes")) { result = queryClassNames(query); }
    if(queryTypeName.equals("Annotation")) { result = queryAttributeNames(query); }

    if(showIncluded) {
      return result;
    } else {
      Iterator iter = result.iterator();
      String s;
      ArrayList list = new ArrayList(result.size());
      while(iter.hasNext()) {
        s = (String)iter.next();
        if(!s.endsWith(INCLUDED)) {
          list.add(s);
        }
      }
      return list;
    }
  }

  private class Thumbnail extends JPanel {
    private GraphPanel _graphPanel;
    private JTextField _textField;
    private JButton _button;
    private String _message;
    private Project _p;
    private int _levels;

    public Thumbnail(Project p) {
      setLayout(new BorderLayout());
      _message = "";
      _p = p;
      _levels = KBSummary._levelsForThumbnail;
      _graphPanel = new GraphPanel();
      _graphPanel.setGraph(generateThumbnail(_p));
      add(_graphPanel, BorderLayout.CENTER);
      add(getUpdateUI(), BorderLayout.NORTH);
    }

    private JPanel getUpdateUI() {
      JLabel label = ComponentFactory.createLabel();
      label.setText("Number of Levels");
      _textField = new JTextField(String.valueOf(KBSummary._levelsForThumbnail), KBSummary._levelsForThumbnail);
      _textField.setMaximumSize(_textField.getPreferredSize());
      _textField.setAction(parseTextFieldAction());
      _button = ComponentFactory.createButton(parseTextFieldAction());

      JPanel panel = ComponentFactory.createPanel();
      panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
      panel.add(Box.createHorizontalGlue());
      panel.add(label);
      panel.add(Box.createRigidArea(new Dimension(10, 0)));
      panel.add(_textField);
      panel.add(Box.createRigidArea(new Dimension(10, 0)));
      panel.add(_button);
      return panel;
    }

    private AbstractAction parseTextFieldAction() {
      return new AbstractAction("Update") {
        public void actionPerformed(ActionEvent e) {
          try {
            int field = Integer.parseInt(_textField.getText());
            KBSummary._levelsForThumbnail = field;
            _levels = field;
            _graphPanel.setGraph(generateThumbnail(_p));
            _message = "";
            repaint();
          } catch (Exception expression) {
            _message = "Invalid entry: " + _textField.getText() + "is not a number";
            repaint();
          }
        }
      };
    }

    private Graph generateThumbnail(Project p) {
      Graph newGraph = new att.grappa.Graph("hi");
      WaitCursor waitCursor = new WaitCursor(BrowserManager.getBrowserManager().getRootPane());
      try {
        String dotFileName = File.createTempFile(p.getName(), ".dot").getPath();
        String inputFileName = dotFileName+"-input";
        PrintWriter pw;
        pw = new PrintWriter(new FileWriter(inputFileName));
        exportGraph(pw, KBSummary._levelsForThumbnail, p);
        pw.flush();
        pw.close();
        dot(inputFileName, dotFileName, "", true);
        FileInputStream input = new FileInputStream(dotFileName);
        Parser graphParser = new Parser(input, System.err);
        graphParser.parse();
        newGraph = graphParser.getGraph();
      } catch (Exception e) {
        e.printStackTrace();
      } finally {
        waitCursor.hide();
      }
      return newGraph;
    }

    private void exportGraph(PrintWriter pw, int levels, Project p) {
      pw.print("digraph test {\n");
      pw.print("fontsize = 18;\n");

      KnowledgeBase kb = p.getKnowledgeBase();
      Cls thing = kb.getCls(":THING");
      ArrayList queue = new ArrayList(thing.getDirectSubclasses());
      int i;
      Hashtable ht = new Hashtable();

      for(i = 0; i < queue.size(); i++) {
        if(!((Cls)queue.get(i)).getName().equals(":SYSTEM-CLASS")) {
          expandNode(pw, (Cls)queue.get(i), levels-1, ht);
        }
      }

      pw.print("}\n");
    }

    private void expandNode(PrintWriter pw, Cls node, int levelsLeft, Hashtable arcs) {
      String color;
      if(node.getName().equals(":SYSTEM-CLASS") || node.isSystem()) return;
      if(node.isIncluded()) {
        color = "lightslategray";
      } else {
        color = "mediumseagreen";
      }
      pw.print("\""+ node.getName() + "\" [style = filled, color = " + color + "];");
      if(levelsLeft == 0) return;
      ArrayList queue = new ArrayList(node.getDirectSubclasses());
      int i;
      String subclass;
      for(i = 0; i < queue.size(); i++) {
        subclass = ((Cls)queue.get(i)).getBrowserText();
        if(!arcs.containsKey(node.getBrowserText() + "->" + subclass)) {
          pw.print("\""+ node.getBrowserText() + "\"-> \"" + subclass + "\";\n");
          arcs.put(node.getBrowserText() + "->" + subclass, "value");
        }
        expandNode(pw, (Cls)queue.get(i), levelsLeft - 1, arcs);
      }
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
        Warning.showWarning("Could not generate graph", "Warning: Could not invoke command '" + commandPath + "' when generating graphviz thumbnail");
        System.err.println("Warning: Could not invoke command '" + commandPath + "' when generating graphviz thumbnail");
      }
    }

    public void paint(Graphics g) {
      if(_levels != KBSummary._levelsForThumbnail) {
        _levels = KBSummary._levelsForThumbnail;
        _textField.setText(String.valueOf(_levels));
        _graphPanel.setGraph(generateThumbnail(_p));
      }
      super.paint(g);
      g.drawString(_message, 7, 3 * g.getFontMetrics().getAscent());
    }
  }
}