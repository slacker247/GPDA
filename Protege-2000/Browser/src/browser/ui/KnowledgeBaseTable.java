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
import javax.swing.*;
import javax.swing.table.*;
import browser.model.*;
import browser.event.*;
import javax.swing.event.*;
import javax.swing.border.*;
import java.awt.event.*;
import edu.stanford.smi.protege.widget.*;
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.protege.ui.*;
import java.util.*;

public class KnowledgeBaseTable extends AbstractLibraryView {
  private SelectableTable _attributeTable;
  private LabeledComponent _labeledComponent;

  private static final String NO_VALUE = "No Value";

  public void initialize() {
    try {
      jbInit();
    }
    catch(Exception ex) {
      ex.printStackTrace();
    }
  }

  private void jbInit() throws Exception {
    this.setLayout(new BorderLayout());

    Action action = createAction();
    _attributeTable = (SelectableTable)ComponentFactory.createTable(action);
    _attributeTable.setModel(createTableModel(null));
    ComponentUtilities.addColumn(_attributeTable, new DefaultTableCellRenderer());
    ComponentUtilities.addColumn(_attributeTable, new DefaultTableCellRenderer());

    JComponent t = ComponentFactory.createScrollPane(_attributeTable);
    setLayout(new BorderLayout());

    String text = "No Item Selected";
    _labeledComponent = new LabeledComponent(text, t);

    add(_labeledComponent, BorderLayout.CENTER);

  }

  private TableModel createTableModel(LibraryItem kb) {
    String[] columnNames = new String[2];
    columnNames[0] = "Annotation";
    columnNames[1] = "Value";

    if(kb == null) {
      DefaultTableModel model = new DefaultTableModel(2, 2);
      model.setColumnIdentifiers(columnNames);
      model.setValueAt("No information to display.", 0, 0);
      model.setValueAt("Select an item to view its summary.", 1, 0);
      return model;
    }
    ArrayList attributeNames = kb.getAttributeNames();
    Collection attribute;

    if(attributeNames.size() == 0) {
      DefaultTableModel model = new DefaultTableModel(4, 2);
      model.setColumnIdentifiers(columnNames);
      model.setValueAt("No information to display.", 0, 0);
      model.setValueAt("\"" + kb.getName() + "\" has no annotation.", 1, 0);
      if(kb instanceof KBSummary) {
        model.setValueAt("See the tutorial to learn how", 2, 0);
        model.setValueAt("to annotate Protege projects.", 3, 0);
      }
      return model;
    }

    ArrayList rows, row;
    DefaultTableModel model = new DefaultTableModel(attributeNames.size(), 2);
    model.setColumnIdentifiers(columnNames);

    rows = new ArrayList(attributeNames.size());
    int i = 0;
    while(i < attributeNames.size() && i < model.getRowCount()) {
      row = new ArrayList();
      attribute = kb.getAttributeValues(attributeNames.get(i));
      if(attribute != null) {
        row.add(0, attributeNames.get(i));
        row.add(1, handleMultipleValues(attribute));
        rows.add(row);
      }
      i++;
    }
    Collections.sort(rows, new Comparator() {
      public int compare(Object first, Object second) {
        if(((ArrayList)first).get(1).equals(KnowledgeBaseTable.NO_VALUE)) {
          return 1;
        }
        if(((ArrayList)second).get(1).equals(KnowledgeBaseTable.NO_VALUE)) {
          return -1;
        }
        return 0;
      }
    });

    for(int j = 0; j < rows.size(); j++) {
      for(int k = 0; k < 2; k++) {
        model.setValueAt(((ArrayList)rows.get(j)).get(k), j, k);
      }
    }
    return model;
  }

  private String handleMultipleValues(Collection values) {
    if(values == null || values.size() == 0) {
      return KnowledgeBaseTable.NO_VALUE;
    }
    ArrayList list = new ArrayList(values);
    int i;
    String string = "";
    for(i = 0; i < list.size() - 1; i++) {
      string += (String)list.get(i) + ", ";
    }
    string += (String)(list.get(list.size() - 1).toString());
    return string;
  }

  private Action createAction() {
        return new AbstractAction("Show Slot Info") {
            public void actionPerformed(ActionEvent event) {

            }
        };
    }

  public AbstractValidatableComponent getConfigurationPanel(){return null;}

  public String getName() {
    return "Knowledge Base Table";
  }

  public void selectionChanged(LibraryEvent e) {
    if(_attributeTable == null) {
      return ;
    }
    if(_library.getSelectedKBSummaries() != null && _library.getSelectedKBSummaries().size() > 0) {
      LibraryItem kb = (LibraryItem) CollectionUtilities.getFirstItem(_library.getSelectedKBSummaries());
      _attributeTable.setModel(createTableModel(kb));
      _labeledComponent.setHeaderLabel(kb.getFilePath());
    } else {
      _attributeTable.setModel(createTableModel(null));
      _labeledComponent.setHeaderLabel("No Item Selected");
    }
  }

  public void libraryModified(LibraryEvent e) {
    selectionChanged(e);
  }

  public void libraryClosed(LibraryEvent e) {
    _attributeTable.setModel(createTableModel(null));
  }

  public void librarySaved(LibraryEvent e) {
  }

  public void libraryChanged(LibraryEvent event) {
    _library = BrowserManager.getCurrentLibrary();
    _attributeTable.setModel(createTableModel(null));
    _labeledComponent.setHeaderLabel("No Item Selected");
    repaint();
  }
  public String getConfigurationString() {
    return "";
  }

  public void loadConfigurationString(){}

}
