
/**
 * Title:        Browser<p>
 * Description:  This allows you to browse a library of knowledge bases.<p>
 * Copyright:    Copyright (c) Warren Shen<p>
 * Company:      Stanford Medical Informatics<p>
 * @author Warren Shen
 * @version 1.0
 */
package browser.ui;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.table.*;
import browser.model.*;
import browser.ui.BrowserView;
import java.util.*;
import browser.event.*;
import javax.swing.event.*;
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.protege.widget.*;
import edu.stanford.smi.protege.ui.*;
import browser.action.*;

public class LibraryTable extends AbstractLibraryView implements SelectionListener {
  private SelectableTable _table, _includedTable;
  private LaunchPromptAction _promptAction;
  private LaunchProtegeAction _protegeAction;
  private AddIncludedItemAction _addIncludedAction;
  private boolean _hideInactive;
  private AbstractValidatableComponent _configPanel;
  private ArrayList _columns, _attributes;
  private LabeledComponent _c, _incTableComp;
  private boolean _hasChecks;
  private Hashtable _includedItemFromName, _includedIdFromName;
  private ArrayList _items;

  public  static final int HIDE_INACTIVE = 100;
  public  static final int SHOW_INACTIVE = 101;
  private static final String NO_VALUE = "No Value";
  private static final String ITEM_FILE_NAME = "File";
  private static final String IS_ACTIVE = "Active";

  public void initialize() {
    _hideInactive = true;
    _hasChecks = true;
    _columns = new ArrayList();
    _attributes = new ArrayList();
    extractAllAttributes();

    _c = new LabeledComponent("Library Overview", initTable());
    _incTableComp = new LabeledComponent("Included Items", initIncludedTable());
    JSplitPane splitPane = ComponentFactory.createTopBottomSplitPane();
//    splitPane.add(_c, JSplitPane.TOP);
    splitPane.setTopComponent(_c);
    splitPane.setBottomComponent(_incTableComp);
//    splitPane.add(_incTableComp, JSplitPane.BOTTOM);

    setLayout(new BorderLayout());
    add(splitPane, BorderLayout.CENTER);
    addHeaderButtons();

    splitPane.setDividerLocation(BrowserView.DIVIDER_LOCATION);
    this.setVisible(true);
  }

  public void setHasChecks(boolean checks) {
    _hasChecks = checks;
    _c.setCenterComponent(initTable());
  }

  private JComponent initTable() {
    Action action = createAction();
    _table = ComponentFactory.createSelectableTable(action);
    _table.setModel(createTableModel());

    if(_hasChecks) {
      ComponentUtilities.addColumn(_table, new CheckBoxRenderer());
    }
    ComponentUtilities.addColumn(_table, new DefaultTableCellRenderer());
    for(int i = 0; i < _columns.size(); i++) {
      ComponentUtilities.addColumn(_table, new DefaultTableCellRenderer());
    }
    _table.addMouseListener(new ClickListener());
    _table.addSelectionListener(this);
    JComponent p = ComponentFactory.createScrollPane(_table);
    return p;
  }

  private JComponent initIncludedTable() {
    SelectionListener sl;
    Action action = createAction();
    _includedTable = (SelectableTable)ComponentFactory.createTable(action);
    _includedTable.setModel(createIncludedTableModel());
    ComponentUtilities.addColumn(_includedTable, new DefaultTableCellRenderer());
    _includedTable.setCellSelectionEnabled(false);
    _includedTable.setRowSelectionAllowed(true);
    sl = new SelectionListener() {
      public void selectionChanged(SelectionEvent e) {
        _addIncludedAction.setEnabled(canAddIncludedItems());
      }
    };
    _includedTable.addSelectionListener(sl);
    JComponent p2 = ComponentFactory.createScrollPane(_includedTable);
    if(_addIncludedAction != null) _addIncludedAction.setEnabled(canAddIncludedItems());
    return p2;
  }

  private void addHeaderButtons() {
    _protegeAction = new LaunchProtegeAction(_library);
    _protegeAction.setEnabled(false);
    _c.addHeaderButton(_protegeAction);
    _promptAction = new LaunchPromptAction(BrowserManager.getBrowserManager());
    _promptAction.setEnabled(false);
    _c.addHeaderButton(_promptAction);
    _c.addHeaderButton(new AddFileAction(BrowserManager.getBrowserManager()));
    _c.addHeaderButton(new RemoveItemAction(BrowserManager.getBrowserManager()));
    _addIncludedAction = new AddIncludedItemAction(this);
    _incTableComp.addHeaderButton(_addIncludedAction);
    _addIncludedAction.setEnabled(canAddIncludedItems());
  }

  public void setMode(int mode) {
    if(mode == this.SHOW_INACTIVE) {
      _hideInactive = false;
    } else {
      _hideInactive = true;
    }
    _table.setModel(createTableModel());
  }


  /**
   * This is invoked by the table, and changes the library.
   */

  public void selectionChanged(SelectionEvent e) {
    int i;
    int []rows = _table.getSelectedRows();
    ArrayList selected = new ArrayList();
    for(i = 0; i < rows.length; i++) {
      selected.add(_items.get(rows[i]));
    }
    _library.setSelectedKBSummaries(selected);
  }

  /**
   * This is invoked by the LibraryEventDispatcher when the library
   * is selected by someone else.
   */
  public void selectionChanged(LibraryEvent e) {
    Collection col = _library.getSelectedKBSummaries();

    /* enable or enable prompt and protege buttons */
    _promptAction.setEnabled(col.size() == 2 &&
                             col.toArray()[0].getClass().getName().equals("browser.model.KBSummary") &&
                             col.toArray()[1].getClass().getName().equals("browser.model.KBSummary"));
    _protegeAction.setEnabled(col.size() == 1 &&
                             col.toArray()[0].getClass().getName().equals("browser.model.KBSummary"));

    /* Update selection in table.  Since the the LibraryTable listens to its own table,
     * remove the LibraryTable as a listener before updating its table.
     */
    _table.getSelectionModel().removeListSelectionListener(_table);
    _table.removeSelectionListener(this);
    _table.getSelectionModel().removeSelectionInterval(0, _table.getRowCount()-1);
    Iterator iter = col.iterator();
    int index;
    while(iter.hasNext()) {
      index = getRowFromID(((LibraryItem)iter.next()).getIdentifier());
      if(index >= 0)
         _table.getSelectionModel().addSelectionInterval(index, index);
    }
    /* Add this LibraryTable as a listener again after we made all our updates to the table */
    _table.addSelectionListener(this);
    _table.getSelectionModel().addListSelectionListener(_table);
    _table.repaint();

    /* Update the includedTable */
    if(_table.getSelectedRowCount() > 0) {
      LibraryItem item = (LibraryItem)_items.get(_table.getSelectedRows()[0]);
      _incTableComp.setHeaderLabel("Items included in: " + item.getName());
    } else {
      _incTableComp.setHeaderLabel("Included items");
    }
    _incTableComp.setCenterComponent(initIncludedTable());
  }

  /* Add the items in the includedTable */
  public void addSelectedIncludedItems() {
    int i;
    Collection c, selected;
    LibraryItem item;
    String name, id;
    Iterator iter = _includedTable.getSelection().iterator();
    selected = _library.getSelectedKBSummaries();
    while(iter.hasNext()) {
      name = (String)iter.next();
      id = (String)_includedIdFromName.get(name);
      c = CollectionUtilities.createCollection(id);
      item = ((LibraryItem)_includedItemFromName.get(name));
      _library.addLibraryItems(c, item, false, true);
    }
    _library.setSelectedKBSummaries(selected);
  }

  public void libraryClosed(LibraryEvent e) {
    _library = new LibraryModel();
    extractAllAttributes();
    _c.setCenterComponent(initTable());
    _incTableComp.setCenterComponent(initIncludedTable());
  }

  public void librarySaved(LibraryEvent e) {
  }

  public void libraryChanged(LibraryEvent event) {
    _library = BrowserManager.getCurrentLibrary();
    extractAllAttributes();
    _c.setCenterComponent(initTable());
    _incTableComp.setCenterComponent(initIncludedTable());
    repaint();
  }

  public void libraryModified(LibraryEvent e) {
    extractAllAttributes();
    _c.setCenterComponent(initTable());
    _incTableComp.setCenterComponent(initIncludedTable());
    repaint();
  }

  public void changeColumns(ArrayList columns) {
    _columns = columns;
    _c.setCenterComponent(initTable());
    repaint();
  }


  private void extractAllAttributes() {
    Iterator iter = _library.getAllNonIncludedItems().iterator();
    ArrayList attributes = new ArrayList();;
    LibraryItem item;

    if(_library.getAllItems().size() == 0) {
      _columns = new ArrayList();
      _attributes = new ArrayList();
    }
    while(iter.hasNext()) {
      item = (LibraryItem)iter.next();
      extractColumns(item.getSummaryAttributeNames());
      Iterator iter2 = extractAttributes(item.getAttributeNames()).iterator();
      Object o;
      while(iter2.hasNext()) {
        o = iter2.next();
        if(!attributes.contains(o)){
          attributes.add(o);
        }
      }
    }
    _attributes = attributes;
    takeOutNonExistentColumns();
  }

  private void extractColumns(Collection attributes) {
    Iterator iter;
    String attribute;
    if(attributes == null) return;
    iter = attributes.iterator();
    while(iter.hasNext()) {
      attribute = (String)iter.next();
      if(!_attributes.contains(attribute) &&
         !_columns.contains(attribute)) {
        _columns.add(attribute);
      }
    }
  }

  private Collection extractAttributes(Collection attributes) {
    ArrayList temp = new ArrayList();
    if(attributes == null) return temp;
    Iterator iter = attributes.iterator();
    String attribute;
    while(iter.hasNext()) {
      attribute = (String)iter.next();
      if(!temp.contains(attribute)) {
        temp.add(attribute);
      }
    }
    return temp;
  }

  private void takeOutNonExistentColumns() {
    Iterator iter = _columns.iterator();
    String attribute;
    ArrayList temp = new ArrayList();

    while(iter.hasNext()) {
      attribute = (String)iter.next();
      if(!_attributes.contains(attribute)) {
        temp.add(attribute);
      }
    }
    _columns.removeAll(temp);
  }

  public String getName() {
    return "Library Overview";
  }

  public AbstractValidatableComponent getConfigurationPanel(){
    return new ConfigurationPanel(_attributes, _columns, this);
  }

  private TableModel createTableModel() {
    DefaultTableModel model = new DefaultTableModel();
    try {
      addColumnsToModel(model);
      addRowsToModel(model, _columns);

      return model;
    } catch (Exception e) {
      e.printStackTrace();
      return _table.getModel();
    }
  }

  private void addColumnsToModel(DefaultTableModel model) {
    ArrayList columns;
    columns = _columns;
    if(_hasChecks) model.addColumn(this.IS_ACTIVE);
    model.addColumn(this.ITEM_FILE_NAME);
    for(int i = 0; i < columns.size(); i++) {
      model.addColumn(columns.get(i));
    }
  }

  private void addRowsToModel(DefaultTableModel model, ArrayList columns) {
    int i, j;
    Vector row;
    String temp;
    _items = new ArrayList();
    boolean hasValue;
    LibraryItem item;
    int index = 0;
    for(i = 0; i < _library.size(); i++) {
      row = new Vector();
      hasValue = false;
      item = _library.getIthKBSummary(i);
      if(!item.isIncluded()) {
        if(_hasChecks) row.add(new Boolean(item.isActive()));
        row.add(item.getName());
        for(j = 0; j < columns.size() && item != null; j++) {
          temp = handleMultipleValues(_library.getIthKBSummary(i).getAttributeValues(columns.get(j)));
          row.add(temp);
          if(!temp.equals(this.NO_VALUE)) { hasValue = true; }
        }
        model.addRow(row);
        _items.add(item);
      }
    }
  }

  private String handleMultipleValues(Collection values) {
    Object value = CollectionUtilities.getFirstItem(values);
    if(value == null) {
      return LibraryTable.NO_VALUE;
    }
    return CollectionUtilities.getFirstItem(values).toString();
  }

  private TableModel createIncludedTableModel() {
    _includedItemFromName = new Hashtable();
    _includedIdFromName = new Hashtable();
    DefaultTableModel model = new DefaultTableModel();
    model.addColumn("File");
    Iterator iter = _library.getSelectedKBSummaries().iterator();
    Iterator depIter;
    LibraryItem item;
    Collection dep;
    String id, name;
    while(iter.hasNext()) {
      item = (LibraryItem)iter.next();
      dep = item.getAllDependenciesIds();
      depIter = dep.iterator();
      while(depIter.hasNext()) {
        id = (String)depIter.next();
        name = item.getNameFromIdentifier(id);
        _includedIdFromName.put(name, id);
        _includedItemFromName.put(name, item);
        model.addRow(new Vector(CollectionUtilities.createCollection(name)));
      }
    }
    return model;
  }

  private Action createAction() {
    return new AbstractAction("hello there") {
      public void actionPerformed(ActionEvent event) {
      }
    };
  }

  private boolean canAddIncludedItems() {
    Collection col = _includedTable.getSelection();
    if(col == null) return false;
    Iterator iter = col.iterator();
    _includedTable.getSelection();
    String id;
    LibraryItem item;
    while(iter.hasNext()) {
      id = (String)_includedIdFromName.get(iter.next());
      if(!_library.isInLibrary(id)) {
        return true;
      } else {
        item = _library.getLibraryItem(id);
        if(item.isIncluded()) return true;
      }
    }
    return false;
  }

  public String getConfigurationString() {
    return "";
  }
  public void loadConfigurationString()
  {}

  private class ClickListener extends MouseAdapter {
    public void mousePressed(MouseEvent event) {
      Point p = event.getPoint();
      int col = _table.columnAtPoint(p);
      if (col == 0 && _hasChecks) {
        int row = _table.rowAtPoint(p);
        Boolean b = (Boolean) _table.getModel().getValueAt(row, 0);
        Boolean newValue = new Boolean(!b.booleanValue());
        _table.getModel().setValueAt(newValue, row, 0);
        ((LibraryItem)_items.get(_table.getSelectedRow())).setActive(newValue.booleanValue());
        _library.postLibraryEvent(LibraryEvent.LIBRARY_MODIFIED);
      }

    }
  }

  private class ConfigurationPanel extends OptionsPanel {
    LibraryTable _table;

    public ConfigurationPanel(ArrayList attributes, ArrayList summary, LibraryTable table) {

      _table = table;
      init(attributes, summary, "Attributes",
          "Select which attributes to show in the Library Overview table", true);
    }

    public void saveContents() {
      LibraryTable table;
      for(int i = 0; i < _listeners.size(); i++) {
        if(_listeners.get(i) instanceof LibraryTable) {
          table = (LibraryTable)_listeners.get(i);
          table.changeColumns(new ArrayList(getSelectedOptions()));
        }
      }
      _table.changeColumns(new ArrayList(getSelectedOptions()));
    }
  }

  private int getRowFromID(String id) {
    for(int i = 0; i < _items.size(); i++) {
      if(((LibraryItem)_items.get(i)).getIdentifier().equals(id)) return i;
    }
    return -1;
  }
}