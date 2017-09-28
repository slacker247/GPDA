/**
 * Title:        Browser<p>
 * Description:  This allows you to browse a library of knowledge bases.<p>
 * Copyright:    Copyright (c) Warren Shen<p>
 * Company:      Stanford Medical Informatics<p>
 * @author Warren Shen
 * @version 1.0
 */
package browser.model;

import edu.stanford.smi.protege.model.*;
import browser.ui.BrowserManager;
import java.util.Collection;
import javax.swing.JComponent;
import java.util.*;
import javax.swing.*;

public class InstanceSummary extends LibraryItem {
  private String _projectName;
  private ArrayList _dependencies;
  private Hashtable _slots;
  private ArrayList _slotNames;

  public InstanceSummary() {
    _name = "no name";
    _filePath = "no name";
    _projectName = "no name";
    _dependencies = new ArrayList();
  }

  public InstanceSummary(Instance i) {
    _name = i.getBrowserText();
    _filePath = i.getProject().getProjectFilePath();
    _projectName = i.getProject().getName();
    _identifier = createIdentifier(i);

    _slots = new Hashtable();
    Iterator iter = i.getOwnSlots().iterator();
    while(iter.hasNext()) {
      _slots.put(((Slot)iter).getBrowserText(), i.getOwnSlotValues((Slot)iter));
    }

    extractDependencies(i);
  }

  public InstanceSummary(String instanceName, String projectName, String fileName) {
    init(instanceName, projectName, fileName, null);
  }

  public InstanceSummary(String instanceName, String projectName, String fileName,
                          Hashtable slots) {
    init(instanceName, projectName, fileName, slots);
  }

  private void init(String instanceName, String projectName,
                          String projectFileName, Hashtable slots) {
    _name = instanceName;
    _projectName = projectName;
    _filePath = projectFileName;
    _slotNames = new ArrayList();
    if(slots == null) {
      _slots = new Hashtable();
    } else {
      _slots = slots;
      _slotNames.add(_slots.keys());
    }
  }

  private void extractDependencies(Instance i) {
    _dependencies = new ArrayList();
    //Iterator iter = cls.getDirectSubclasses().iterator();
    //while(iter.hasNext()) {
      //_dependencies.add(((Cls)iter.next()).getBrowserText());
   // }
  }

  public Collection getAttributeValues(Object attributeName) {
    return (Collection)_slots.get(attributeName);
  }

  public Collection createNewLibraryItems(Collection identifiers, boolean isHidden, boolean isActive) {
    Iterator iter = identifiers.iterator();
    String identifier;
    ArrayList results = new ArrayList();
    while(iter.hasNext()) {
      identifier = (String)iter.next();
      InstanceSummary insSum = new InstanceSummary(extractInstanceName(identifier),
        extractProjectName(identifier), extractProjectFileName(identifier));
      insSum.setActive(isActive);
      insSum.setHidden(isHidden);
      results.add(insSum);
    }
    return results;
  }

  public Collection getUserLibraryItems() {
    Iterator iter;
    Collection temp;
    ArrayList items = new ArrayList();
    ArrayList errors = new ArrayList();
    Project p = null;
    iter = BrowserManager.launchFileChooser("Protege", ".pprj").iterator();
    while(iter.hasNext()) {
      p = Project.loadProjectFromFile((String)iter.next(), errors);
      temp = getInstanceSummariesFromProject(p);
      items.addAll(temp);

    }
    if(p != null) {
      p.dispose();
    }
    return items;
  }

  private Collection getInstanceSummariesFromProject(Project p) {
    Instance i;
    ArrayList summaries = new ArrayList();
    String className;
    Iterator instances = p.getKnowledgeBase().getInstances().iterator();
    while(instances.hasNext()) {
      i = (Instance)instances.next();
      className = i.getClass().getName();
      if(!className.equals("edu.stanford.smi.protege.model.DefaultSlot") &&
         !className.equals("edu.stanford.smi.protege.model.DefaultCls") &&
         !className.equals("edu.stanford.smi.protege.model.DefaultFacet")) {
        summaries.add(new InstanceSummary(i));
      }
    }

    return summaries;
  }

  private String createIdentifier(Instance i) {
    String instanceName, projectName, projectFileName;
    instanceName = i.getBrowserText();
    projectName = i.getProject().getProjectName();
    projectFileName = i.getProject().getProjectFilePath();
    return createIdentifier(instanceName, projectName, projectFileName);
  }

  private String createIdentifier(String instance, String project, String file) {
    return instance + "%" + project + "%" + file;
  }

  private String createIdentifier() {
    return createIdentifier(_name, _projectName, _filePath);
  }

  private String extractProjectFileName(String identifier) {
    return getIthToken(identifier, "%", 2);
  }

  private String extractProjectName(String identifier) {
    return getIthToken(identifier, "%", 1);
  }

  private String extractInstanceName(String identifier) {
    return getIthToken(identifier, "%", 0);
  }

  private String getIthToken(String string, String delim, int i) {
    StringTokenizer st = new StringTokenizer(string, delim);
    String name = "no name";
    for(int j = 0 ; j <= i; j++) {
      name = st.nextToken();
    }
    return name;
  }

  public String getTypeName() {
    return "Instance";
  }

  public String getIdentifier() {
    return createIdentifier();
  }

  public ArrayList getSummaryAttributeNames() {
    return new ArrayList();
  }

  public ArrayList getAttributeNames() {
    return _slotNames;
  }

  public Collection getDependencies() {
    return _dependencies;
  }

  public JComponent getThumbnail() {
    return new JPanel();
  }

  public Collection getQueryTypeNames() {
    return new ArrayList();
  }

  public Collection query(String query, String queryTypeName, boolean showIncluded9) {
    return new ArrayList();
  }
}