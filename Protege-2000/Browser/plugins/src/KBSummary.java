/**
 * Title:        <p>
 * Description:  <p>
 * Copyright:    Copyright (c) <p>
 * Company:      <p>
 * @author
 * @version 1.0
 */

import browser.model.*;
import browser.Browser;
import browser.ui.BrowserManager;
import java.util.*;
import java.io.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;
import dfki.protege.ontoviz_tab.*;
import javax.swing.*;
import att.grappa.*;
import java.awt.BorderLayout;

public class KBSummary extends LibraryItem {
  private JComponent _thumbnail;
  private Collection _dependencies = null;
  private Hashtable _attributes = new Hashtable();
  private Hashtable _classes = new Hashtable();
  private Hashtable _instances = new Hashtable();
  private KnowledgeBaseFactory _factory = null;
  //private Project _p;

  private static final String INCLUDED = "(INCLUDED)";

  /*private KBSummary() {
    this(null, true);
  }*/

  private void loadProject(String projectFilePath, boolean isHidden) {
    if(projectFilePath == null) {
      init(null, true);
      return;
    }
    System.out.println("creating new kbsummary: " + projectFilePath);
    Collection errors = new ArrayList();
    Project p = Project.loadProjectFromFile(projectFilePath, errors);
    //_p = p;
    if (errors.size() == 0) {
      init(p, isHidden);
    } else {
      Iterator i = errors.iterator();
      System.out.println("ERRORS while loading "+projectFilePath+":");
      int num = 1;
      while (i.hasNext()) {
      	System.out.println(num++ + ". Error: " + i.next());
      }
    }
    p.dispose();
    p = null;
  }

  private void init(Project p, boolean isHidden) {

    _isHidden = isHidden;
    _isModified = false;
    _isActive = !isHidden;
    if(p == null) {
      _filePath = null;
      return;
    }
    _factory = p.getKnowledgeBaseFactory();
    Iterator iter;
    String dependent;
    _dependencies = new ArrayList();
    iter = p.getDirectIncludedProjects().iterator();
    String dir = FileUtilities.getDirectory(p.getProjectFilePath()) + "\\";
    while(iter.hasNext()) {
      dependent = (String)iter.next();
      if(FileUtilities.getDirectory(dependent) == null) {
        _dependencies.add(dir.concat(dependent));
      } else {
        _dependencies.add(dependent);
      }
    }
    _filePath = p.getProjectFilePath();
    initAttributes(p);
    initThumbnail(p);
    extractFrames(p);
  }

  private void extractFrames(Project p) {
    KnowledgeBase kb = p.getKnowledgeBase();
    Iterator iter = kb.getClses().iterator();
    Cls c;
    String name;
    while(iter.hasNext()) {
      c = ((Cls)iter.next());
      if(!c.isIncluded()) {
        name = c.getBrowserText();
        _classes.put(name, name);
      } else {
        name = c.getBrowserText() + " " + INCLUDED;
        _classes.put(c.getBrowserText(), name);
      }
    }
    iter = kb.getInstances().iterator();
    while(iter.hasNext()) {
      name = ((Instance)iter.next()).getBrowserText(); // need to take out instances that are classes
      _instances.put(name, name);
    }
  }

  private void extractClasses(String projectFilePath) {
    String dir = FileUtilities.getAbsoluteDirectory(projectFilePath);
    String name = FileUtilities.getBaseName(projectFilePath);
    String pontName = dir + "\\" + name + ".pont";
    // use this to get the .pont file.
  }

  public KnowledgeBaseFactory getKnowledgeBaseFactory() {
    return _factory;
  }

  public Collection createNewLibraryItems(Collection identifiers, boolean isHidden, boolean isActive) {
    Iterator iter = identifiers.iterator();
    String identifier;
    KBSummary newItem;
    ArrayList results = new ArrayList();
    while(iter.hasNext()) {
      identifier = (String)iter.next();
      newItem = new KBSummary();
      newItem.loadProject(identifier, isHidden);
      newItem.setActive(isActive);
      results.add(newItem);
    }
    return results;
  }

  public Collection getUserLibraryItems() {
    Iterator iter;
    ArrayList items = new ArrayList();
    String path;
    KBSummary kb;
    iter = BrowserManager.launchFileChooser("Protege", ".pprj").iterator();
    while(iter.hasNext()) {
      path = (String)iter.next();
      kb = new KBSummary();
      kb.loadProject(path, false);
      items.add(kb);
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
    ArrayList list = new ArrayList();
    for(int i = 0; i < SummaryConstants.DEFAULT_ATTRIBUTE_NAMES.length; i++) {
      list.add(SummaryConstants.DEFAULT_ATTRIBUTE_NAMES[i]);
    }
    return list;
  }

  public Collection getAttributeValues(Object attributeName) {
    return (Collection)_attributes.get(attributeName);
  }

  public String getName() {
    return FileUtilities.getName(_filePath);
  }

  public Collection getDependencies() {
    return _dependencies;
  }

  public JComponent getThumbnail() {
    return this._thumbnail;
  }

  private String commandPath = Browser.getProperties().getProperty(Browser.DOT_COMMAND_PROPERTY, "dot");

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
      Graph g;
      g = generateThumbnail(p);
      GraphPanel _graphPanel = new GraphPanel();
      _graphPanel.setGraph(g);
      _thumbnail = _graphPanel;
    }
  }

  private Graph generateThumbnail(Project p) {
    Graph newGraph = new att.grappa.Graph("hi");
    try {
      String dotFileName = File.createTempFile(p.getName(), ".dot").getPath();
      //dotFileName = p.getProjectDirectory() + "\\" + p.getName() + ".dot";
      String inputFileName = dotFileName+"-input";
      PrintWriter pw;
      pw = new PrintWriter(new FileWriter(inputFileName));
      exportGraph(pw, 3, p);
      pw.flush();
      pw.close();
      dot(inputFileName, dotFileName, "", true);
      FileInputStream input = new FileInputStream(dotFileName);
      Parser graphParser = new Parser(input, System.err);
      graphParser.parse();
      newGraph = graphParser.getGraph();

    } catch (Exception e) {
      e.printStackTrace();
    }
    return newGraph;
  }

  private void exportGraph(PrintWriter pw, int levels, Project p) {
    pw.print("digraph test {\n");
    //pw.print("size = \"6, 4\";\n");
    pw.print("fontsize = 18;\n");
    pw.print("\"Top three levels\" [shape = box, style = filled, color = beige];\n");

    KnowledgeBase kb = p.getKnowledgeBase();
    Cls thing = kb.getCls(":THING");
    ArrayList queue = new ArrayList(thing.getDirectSubclasses());
    int i;

    for(i = 0; i < queue.size(); i++) {
      if(!((Cls)queue.get(i)).getName().equals(":SYSTEM-CLASS")) {
        expandNode(pw, (Cls)queue.get(i), levels-1);
      }
    //System.out.println("Done Expanding, " + queue.size() + " " + i);
    }

    pw.print("}\n");
  }

  private void expandNode(PrintWriter pw, Cls node, int levelsLeft) {
    if(node.getName().equals(":SYSTEM-CLASS") || node.isSystem()) return;
    pw.print("\""+ node.getName() + "\" [style = filled, color = mediumseagreen];");
    if(levelsLeft == 0) return;
    ArrayList queue = new ArrayList(node.getDirectSubclasses());
    int i;
    for(i = 0; i < queue.size(); i++) {
      pw.print("\""+ node.getName() + "\"-> \"" + ((Cls)queue.get(i)).getName() + "\";\n");
      expandNode(pw, (Cls)queue.get(i), levelsLeft - 1);
    }
  }

  private void dot(String inputFileName, String fileName, String option, boolean wait) throws Exception {
    String[] command =
      new String[] {commandPath, option, inputFileName, "-o", fileName};
    Process process = Runtime.getRuntime().exec(command);

    if (wait) {
      process.waitFor();
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
      //System.out.println(slot.getName());
      _attributes.put(slot.getName(), slotValues);
    }
  }

  private Collection getInstanceNames(String instanceName) {
    ArrayList list = new ArrayList();
    if(_instances.containsKey(instanceName)) {
      list.add(instanceName);
    }
    return list;
  }
  private Collection getClassNames(String clsName) {
    ArrayList list = new ArrayList();
    if(_classes.containsKey(clsName)) {
      list.add(_classes.get(clsName));
    }
    return list;
  }
  private Collection getAttributeNames(String attributeName) {
    //System.out.println("searching for " + attributeName + " in " + this.getName());
    ArrayList list = new ArrayList();
    if(_attributes.containsKey(attributeName)) {
      Iterator iter = ((Collection)_attributes.get(attributeName)).iterator();
      while(iter.hasNext()) {
        list.add(attributeName + "  (value = \"" + iter.next() + "\")");
      }
    }
    //System.out.println("  found " + list.size());
    return list;
  }


  public Collection getQueryTypeNames() {
    ArrayList names = new ArrayList();
    names.add("Classes");
    names.add("Attributes");
    return names;
  }

  public Collection query(String query, String queryTypeName, boolean showIncluded) {
    Collection result;
    if(queryTypeName.equals("Classes")) {
      result = getClassNames(query);
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
    if(queryTypeName.equals("Attributes")) {
      return getAttributeNames(query);
    }
    return new ArrayList();
  }

}
