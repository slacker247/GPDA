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
import browser.util.*;
import browser.action.*;
import java.awt.event.*;
import java.util.*;
import browser.event.*;
import edu.stanford.smi.protege.util.*;

public class QueryPanel extends AbstractLibraryView implements SelectionListener {
  private SelectableTable _resultTable;
  private JPanel _queryPanel;
  private JComboBox _queryTypeComboBox;
  private JTextField _queryText;
  private LabeledComponent _queryType, _query;
  private JCheckBox _showIncluded;
  private ArrayList _libItems;
  private AbstractAction _protegeButton;
  private String _queryTypeName;

  private static final String NOT_PROTEGE = "not protege";

  public void initialize() {
    _queryTypeName = "";
    initResultTable();
    initQueryPanel();

    setLayout(new BorderLayout());
    LabeledComponent results = new LabeledComponent("Results", ComponentFactory.createScrollPane(_resultTable));
    _protegeButton = new LaunchProtegeFromQueryAction(this);
    results.addHeaderButton(_protegeButton);
    _protegeButton.setEnabled(false);
    add(results, BorderLayout.SOUTH);
    LabeledComponent query = new LabeledComponent("Query", _queryPanel);
    add(query, BorderLayout.CENTER);
  }

  private void initResultTable() {
    SelectableTable tempTable = new SelectableTable();
        tempTable.addMouseListener(new DoubleClickActionAdapter(createAction()));
        tempTable.setModel(createTableModel(null, null, null));
        tempTable.setShowGrid(false);
        tempTable.setIntercellSpacing(new Dimension(0, 0));
        tempTable.setColumnSelectionAllowed(false);
        tempTable.setAutoResizeMode(JTable.AUTO_RESIZE_LAST_COLUMN);
        tempTable.setAutoCreateColumnsFromModel(false);
        tempTable.setDefaultEditor(Object.class, null);
        tempTable.addSelectionListener(this);
    _resultTable = tempTable;
  }

  private TableModel createTableModel(ArrayList results, ArrayList types, ArrayList items) {
    DefaultTableModel model = new DefaultTableModel();
    model.addColumn("Found");
    model.addColumn("Type");
    model.addColumn("Item");
    if(results == null || items == null || types == null) {
      return model;
    }
    if(results.size() != items.size() || types.size() != results.size()) {
      System.out.println("Cannot display query results: arrays not same size");
    }

    int i;
    Vector row;
    for(i = 0; i < results.size(); i++) {
      row = new Vector(3);
      row.add(results.get(i));
      row.add(types.get(i));
      row.add(items.get(i));
      model.addRow(row);
    }
    if(results.size() == 0) {
      row = new Vector();
      row.add("No results found");
      model.addRow(row);
    }

    return model;
  }

  private void initQueryPanel() {
    _queryPanel = new JPanel();
    _queryPanel.setLayout(new BoxLayout(_queryPanel, BoxLayout.Y_AXIS));

    _queryText = ComponentFactory.createTextField();
    _queryText.addActionListener(new AbstractAction("Do Query") {
      public void actionPerformed(ActionEvent event) {
        performQuery();
      }
    });
    _query = new LabeledComponent("named", _queryText);
    _queryType = new LabeledComponent("Find", getQueryTypeComboBox(_queryTypeName));
    Box queryBox = Box.createHorizontalBox();
    queryBox.add(_queryType);
    queryBox.add(_query);

    JPanel buttonPanel = ComponentFactory.createPanel();
    buttonPanel.setLayout(new BorderLayout());
    JButton searchButton = ComponentFactory.createButton(createQueryAction());
    buttonPanel.add(searchButton, BorderLayout.EAST);

    _queryPanel.add(queryBox);
    _queryPanel.add(buttonPanel);
  }

  private JComboBox getQueryTypeComboBox(String selected) {
    String type;
    Action action;
    _queryTypeComboBox = ComponentFactory.createComboBox();
    _queryTypeComboBox.addItem(LibraryItem.FIND_ANY);
    Iterator iter = _library.getQueryTypeNames().iterator();
    while(iter.hasNext()) {
      type = (String)iter.next();
      _queryTypeComboBox.addItem(type);
      if(type.equals(selected)) _queryTypeComboBox.setSelectedItem(type);
    }
    action = new AbstractAction() {
      public void actionPerformed(ActionEvent e) {
        _queryTypeName = (String)_queryTypeComboBox.getSelectedItem();
      }
    };
    _queryTypeComboBox.setAction(action);
    return _queryTypeComboBox;
  }

  private void initQueryTypes() {
    _queryType.setCenterComponent(getQueryTypeComboBox(_queryTypeName));
  }

  private Collection getSelectedQueryTypes() {
    Collection queryTypes = new ArrayList();
    Object [] types = _queryTypeComboBox.getSelectedObjects();
    for(int i = 0; i < types.length; i++) {
      if(types[i].equals(LibraryItem.FIND_ANY)) {
        return _library.getQueryTypeNames();
      } else {
        queryTypes.add(types[i]);
      }
    }
    return queryTypes;
  }

  private void performQuery() {
    Collection queryTypes = getSelectedQueryTypes();
    String query = _queryText.getText();
    if(query == null || query.equals("")) {
      return;
    }

    int i;
    LibraryItem item;
    ArrayList results = new ArrayList();
    ArrayList types = new ArrayList();
    ArrayList libitems = new ArrayList();
    _libItems = new ArrayList();
    Collection tempresults;
    Iterator iter, iter2;
    Object tempType;

    for(i = 0; i < _library.size(); i++) {
      item = _library.getIthKBSummary(i);
      if(item.isActive()) {
        iter = queryTypes.iterator();
        while(iter.hasNext()) {
          tempType = iter.next();
          tempresults = item.query(query, (String)tempType,false);
          iter2 = tempresults.iterator();
          while(iter2.hasNext()) {
            results.add(iter2.next());
            types.add(tempType);
            _libItems.add(item);
            libitems.add(item.getName());
          }
        }
      }
    }
    this._resultTable.setModel(createTableModel(results, types, libitems));
    repaint();
  }

  public void launchProtege() {
    int row = _resultTable.getSelectedRow();
    if(row == -1) return;
    LibraryItem item = (LibraryItem)_libItems.get(row);
    if(item instanceof KBSummary) {
      BrowserManager.launchProtege(item.getFilePath());
    } else {
      Warning.showWarning("Not Protege", "The item you selected is not a Protege project.");
    }
  }

  private Action createQueryAction() {
    return new AbstractAction("Do Query") {
      public void actionPerformed(ActionEvent event) {
        performQuery();
      }
    };
  }

  private Action createAction() {
    return new AbstractAction("hello there") {
      public void actionPerformed(ActionEvent event) {

      }
    };
  }

  public String getName() {
    return "Query";
  }

  public AbstractValidatableComponent getConfigurationPanel(){return null;}
  public String getConfigurationString() {
    return "";
  }
  public void loadConfigurationString() {}

  public void selectionChanged(SelectionEvent event) {
    int row = _resultTable.getSelectedRow();
    if(row == -1) {
      _protegeButton.setEnabled(false);
      return;
    }
    if(_libItems.size() == 0) {
      _protegeButton.setEnabled(false);
    }
    LibraryItem item = (LibraryItem)_libItems.get(row);
    _protegeButton.setEnabled(item instanceof KBSummary);
  }

  public void selectionChanged(LibraryEvent event){}
  public void libraryModified(LibraryEvent event) {
    initQueryTypes();
  }
  public void librarySaved(LibraryEvent event){}
  public void libraryClosed(LibraryEvent event){}

}