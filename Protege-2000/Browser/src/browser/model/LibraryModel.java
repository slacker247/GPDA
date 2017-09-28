/**
 * Title:        Browser<p>
 * Description:  This allows you to browse a library of knowledge bases.<p>
 * Copyright:    Copyright (c) Warren Shen<p>
 * Company:      Stanford Medical Informatics<p>
 * @author Warren Shen
 * @version 1.0
 */
package browser.model;

import browser.util.*;
import browser.event.*;
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.protege.model.*;
import java.util.*;
import java.util.Collections.*;

public class LibraryModel {
  private Hashtable _libraryHT;
  private ArrayList _library;
  private Collection _selectedKBSummaries;
  private int[] _selectedIndices;
  private ListenerCollection _listeners = new ListenerList(new LibraryEventDispatcher());
  private Collection _queryTypes;
  private int _currentItemType;
  private ArrayList _itemTypes;
  private String _fileName;

  public LibraryModel() {
    init();
  }

  private void init() {
    _libraryHT = new Hashtable();
    _library = new ArrayList();
    _selectedKBSummaries = new ArrayList();
    _queryTypes = new ArrayList();
    _currentItemType = 0;
    _itemTypes = new ArrayList();
    _fileName = "";
  }

  public LibraryModel(Collection listeners) {
    init();
    if(listeners != null) {
      Iterator iter = listeners.iterator();
      while(iter.hasNext()) {
        _listeners.add(this, (LibraryListener)iter.next());
      }
    }
  }

  public void addLibraryItem(LibraryItem item) {
    addLibraryItem(item, item.isIncluded(), item.isActive());
  }

  public void addLibraryItems(Collection items) {
    Iterator iter = items.iterator();
    while(iter.hasNext()) {
      addLibraryItem((LibraryItem)iter.next());
    }
  }

  public void addLibraryItem(LibraryItem item, boolean isIncluded, boolean isActive) {
    if(item == null || item.getIdentifier() == null) {
      System.out.println("Cannot add null project.");
      return;
    }
    if(isInLibrary(item.getIdentifier()) && item.isIncluded() == isIncluded &&
      item.isActive() == isActive) {
        return;
    }
    if(!isInLibrary(item.getIdentifier())) {
      _libraryHT.put(item.getIdentifier(), item);
      _library.add(item);
      addQueryTypes(item);
    }
    item = (LibraryItem)_libraryHT.get(item.getIdentifier());
    item.setIncluded(isIncluded);
    item.setActive(isActive);
    postLibraryEvent(LibraryEvent.LIBRARY_MODIFIED);
  }

  public void addLibraryItems(Collection identifiers, int type, boolean isIncluded, boolean isActive) {
    addLibraryItems(identifiers, (LibraryItem)_itemTypes.get(type), isIncluded, isActive);
  }

  public void addLibraryItems(Collection identifiers, LibraryItem type, boolean isIncluded, boolean isActive) {
    ArrayList ids = new ArrayList();
    Iterator iter = identifiers.iterator();
    String identifier;
    boolean modified = false;
    LibraryItem item;
    while(iter.hasNext()) {
      identifier = (String)iter.next();
      if(isInLibrary(identifier)) {
        item = getLibraryItem(identifier);
        if(item.isActive() != isActive || item.isIncluded() != isIncluded) {
          getLibraryItem(identifier).setIncluded(isIncluded);
          getLibraryItem(identifier).setActive(isActive);
          modified = true;
        }
      } else {
        ids.add(identifier);
      }
    }
    if(modified) postLibraryEvent(LibraryEvent.LIBRARY_MODIFIED);
    Collection items;
    items = type.createNewLibraryItems(ids, isIncluded, isActive);
    addLibraryItems(items);
  }

  public void getUserLibraryItems() {
    if(_itemTypes.size() == 0) {
      Warning.showWarning("No Library Items", "Warning: No library item types found.  Check the plugins" +
                          "directory to make sure the jar files are there.");
      System.err.println("Warning: No library item types found.  Check the plugins");
      System.err.println("         directory to make sure the jar files are there.");
      return;
    }

    WaitCursor waitCursor = new WaitCursor(browser.ui.BrowserManager.getBrowserManager().getRootPane());
    try {
      Collection col = ((LibraryItem)_itemTypes.get(_currentItemType)).getUserLibraryItems();
      addLibraryItems(col);
    } finally {
      waitCursor.hide();
    }
  }

  public void addLibraryListener(LibraryListener listener) {
    _listeners.add(this, listener);
  }

  public void removeItem(Object kbsummary) {
    _library.remove(kbsummary);
    _libraryHT.remove(((LibraryItem)kbsummary).getIdentifier());
    postLibraryEvent(LibraryEvent.LIBRARY_MODIFIED);
  }

  public void removeItem(int index) {
    LibraryItem kb = (LibraryItem)_library.get(index);
    _library.remove(index);
    _libraryHT.remove(kb.getIdentifier());
    postLibraryEvent(LibraryEvent.LIBRARY_MODIFIED);
  }

  public void removeSelectedItems() {
    int i;
    Iterator iter = _selectedKBSummaries.iterator();
    LibraryItem item;
    while(iter.hasNext()) {
      item = (LibraryItem)iter.next();
      _library.remove(item);
      _libraryHT.remove(item.getIdentifier());
    }
    _selectedKBSummaries = new ArrayList();
    setSelectedKBSummary(null);
    postLibraryEvent(LibraryEvent.LIBRARY_MODIFIED);
  }

  public void setSelectedKBSummary(int i) {
    _selectedKBSummaries = new ArrayList();
    _selectedIndices = new int[0];
    if(i >= 0 && i < _library.size()) {
      _selectedKBSummaries.add((LibraryItem) _library.get(i));
      _selectedIndices = new int[1];
      _selectedIndices[0] = i;
      postLibraryEvent(LibraryEvent.SELECTION_CHANGED, _selectedKBSummaries);
    }
  }

  public void setSelectedKBSummary(LibraryItem kbSummary) {
    _selectedKBSummaries = new ArrayList();
    _selectedIndices = new int[0];
    if(kbSummary != null) {
      _selectedKBSummaries.add(kbSummary);
      int index = _library.indexOf(kbSummary);
      _selectedIndices = new int[1];
      _selectedIndices[0] = index;
    }
    postLibraryEvent(LibraryEvent.SELECTION_CHANGED, _selectedKBSummaries);
  }

  public void setSelectedKBSummaries(int[] indices) {
    int i;
    _selectedKBSummaries.clear();
    _selectedKBSummaries = new ArrayList();
    _selectedIndices = new int[indices.length];
    for(i = 0; i < indices.length; i++) {
      _selectedKBSummaries.add(_library.get(indices[i]));
      _selectedIndices[i] = indices[i];
    }
    postLibraryEvent(LibraryEvent.SELECTION_CHANGED, _selectedKBSummaries);
  }

  public void setSelectedKBSummaries(Collection selected) {
    _selectedKBSummaries = selected;
    _selectedIndices = new int[selected.size()];
    int i = 0;
    Iterator iter = selected.iterator();
    while(iter.hasNext()) {
      _selectedIndices[i++] = _library.indexOf(iter.next());
    }
    postLibraryEvent(LibraryEvent.SELECTION_CHANGED, selected);
  }

  public void setActiveKBSummary(int i, boolean isActive) {
    if(i >= 0 && i < _library.size()) {
      ((LibraryItem)_library.get(i)).setActive(isActive);
      postLibraryEvent(LibraryEvent.LIBRARY_MODIFIED, CollectionUtilities.createCollection(_library.get(i)));
    }
  }

  public void setActiveKBSummary(LibraryItem summary, boolean isActive) {
    summary.setActive(isActive);
    postLibraryEvent(LibraryEvent.LIBRARY_MODIFIED, CollectionUtilities.createCollection(summary));
  }

  public void moveKnowledgeBaseSummary(int fromIndex, int toIndex) {
    if(fromIndex >= 0 && fromIndex < _library.size() && toIndex >= 0 && toIndex < _library.size()) {
      LibraryItem temp = (LibraryItem)_library.get(fromIndex);
      _library.remove(fromIndex);
      _library.add(toIndex, temp);
      postLibraryEvent(LibraryEvent.LIBRARY_MODIFIED, CollectionUtilities.createCollection(temp));
    }
  }

  public void sortLibrary(Comparator comparator) {
    java.util.Collections.sort(_library, comparator);
    postLibraryEvent(LibraryEvent.LIBRARY_MODIFIED);
  }

  public void postLibraryEvent(int type) {
    postLibraryEvent(type, null);
  }

  public void postLibraryEvent(int type, Object arg1) {
    _listeners.postEvent(this, type, null);
  }

  public LibraryItem getLibraryItem(String identifier) {
    return (LibraryItem)_libraryHT.get(identifier);
  }

  public LibraryItem getIthKBSummary(int i) {
    return (LibraryItem)_library.get(i);
  }

  public int size() {
    return _library.size();
  }

  public boolean isInLibrary(String identifier) {
    LibraryItem kb = (LibraryItem)_libraryHT.get(identifier);
    if(kb == null) {
      return false;
    } else {
      return true;
    }
  }

  public boolean isIncluded(String identifier) {
    if(isInLibrary(identifier)) {
      return(getLibraryItem(identifier).isIncluded());
    }
    return true;
  }

  public Collection getSelectedKBSummaries() {
    return _selectedKBSummaries;
  }

  public int[] getSelectedIndices() {
    return _selectedIndices;
  }

  public Collection getActiveKBSummaries() {
    Collection c = new ArrayList();
    int i;
    for(i = 0; i < _library.size(); i++) {
      if(((LibraryItem)_library.get(i)).isActive()) {
        c.add(_library.get(i));
      }
    }
    return c;
  }

  public Collection getAllItems() {
    return _library;
  }

  public Collection getAllNonIncludedItems() {
    Iterator iter = _library.iterator();
    LibraryItem item;
    ArrayList list = new ArrayList();
    while(iter.hasNext()) {
      item = (LibraryItem) iter.next();
      if(!item.isIncluded()) {
        list.add(item);
      }
    }
    return list;
  }

  public Collection getQueryTypeNames() {
    return _queryTypes;
  }

  private void addQueryTypes(LibraryItem item) {
    Iterator iter = item.getQueryTypeNames().iterator();
    Object type;
    while(iter.hasNext()) {
      type = iter.next();
      if(!_queryTypes.contains(type)) {
        _queryTypes.add(type);
      }
    }
    postLibraryEvent(LibraryEvent.LIBRARY_MODIFIED, CollectionUtilities.createCollection(item));
  }

  private static void displayErrors(Collection errors) {
    Iterator i = errors.iterator();
    System.out.println("Printing ERRORS:");
    int num = 1;
    while (i.hasNext()) {
      System.out.println(num++ + ". Error: " + i.next());
    }
  }

  public void addItemType(LibraryItem type) {
    if(type == null) return;
    if(getItemTypeIndex(type.getTypeName()) == -1) {
      _itemTypes.add(type);
    }
    this.postLibraryEvent(LibraryEvent.LIBRARY_MODIFIED);
  }

  public void setItemType(String type) {
    int i;
    i = getItemTypeIndex(type);
    if(i >= 0) {
      _currentItemType = i;
    }
  }

  public String getCurrentItemType() {
    if(_itemTypes.size() == 0) {
      return null;
    }
    return ((LibraryItem)_itemTypes.get(_currentItemType)).getTypeName();
  }

  public LibraryItem getItemType(String typeName) {
    for(int i = 0; i < _itemTypes.size(); i++) {
      if(((LibraryItem)_itemTypes.get(i)).getTypeName().equals(typeName)) {
        return (LibraryItem)_itemTypes.get(i);
      }
    }
    return null;
  }

  private int getItemTypeIndex(String type) {
    int i;
    for(i = 0; i < _itemTypes.size(); i++) {
      if(type.equals(((LibraryItem)_itemTypes.get(i)).getTypeName())) {
        return i;
      }
    }
    return -1;
  }

  public LibraryItem getSampleItemType(String typeName) {
    return (LibraryItem)_itemTypes.get(getItemTypeIndex(typeName));
  }

  public Collection getItemTypes() {
    return _itemTypes;
  }

  public String getFileName() {
    return _fileName;
  }

  public void setFileName(String name) {
    _fileName = name;
  }
}