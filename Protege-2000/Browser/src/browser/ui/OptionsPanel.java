/**
 * Title:        Browser<p>
 * Description:  This allows you to browse a library of knowledge bases.<p>
 * Copyright:    Copyright (c) Warren Shen<p>
 * Company:      Stanford Medical Informatics<p>
 * @author Warren Shen
 * @version 1.0
 */

package browser.ui;

import edu.stanford.smi.protege.util.*;
import javax.swing.*;
import java.util.*;
import java.awt.event.*;
import javax.swing.table.*;
import java.awt.*;

public class OptionsPanel extends AbstractValidatableComponent {
  private SelectableTable _table;
  private ArrayList _options;
  private Collection _selectedOptions;
  private String _optionColumnName;
  private boolean _allowMultiple;

  public OptionsPanel() {
    init(new ArrayList(), new ArrayList(), "Default", "Select an option", true);
  }

  public OptionsPanel(ArrayList options,
                      Collection initialSelections,
                      String optionColumnName, String label,
                      boolean allowMultiple) {
    init(options, initialSelections, optionColumnName, label, allowMultiple);
  }

  public void init(ArrayList options,
                      Collection initialSelections,
                      String optionColumnName, String label,
                      boolean allowMultiple) {
    _options = options;
    _allowMultiple = allowMultiple;
    _optionColumnName = optionColumnName;
    _selectedOptions = new ArrayList();
    if(initialSelections != null) {
      Iterator iter = initialSelections.iterator();
      while(iter.hasNext()) {
        _selectedOptions.add(iter.next());
      }
    }
    initTable();
    this.setLayout(new BorderLayout());
    JScrollPane pane = ComponentFactory.createScrollPane(_table);
    pane.setColumnHeaderView(_table.getTableHeader());
    pane.setBackground(_table.getBackground());
    LabeledComponent c = new LabeledComponent(label, pane);
    add(c);
  }

  public SelectableTable getTable() {
    return _table;
  }

  private void initTable() {
    _table = ComponentFactory.createSelectableTable(new AbstractAction("options") {
            public void actionPerformed(ActionEvent event) { }});
    _table.setModel(createTableModel());
    ComponentUtilities.addColumn(_table, new CheckBoxRenderer());
    ComponentUtilities.addColumn(_table, new DefaultTableCellRenderer());
    _table.addMouseListener(new ClickListener());
  }

  private TableModel createTableModel() {
    int i;
    Vector row;
    DefaultTableModel model = new DefaultTableModel();

    model.addColumn("");
    model.addColumn(_optionColumnName);

    for(i = 0; i < _options.size(); i++) {
      row = new Vector(2);
      row.add(new Boolean(_selectedOptions.contains((String)_options.get(i))));
      row.add(_options.get(i));
      model.addRow(row);
    }
    return model;
  }

  public void saveContents() {
  }

  public boolean validateContents() {
        return true;
  }

  public Collection getSelectedOptions() {
    return _selectedOptions;
  }

  private class ClickListener extends MouseAdapter {
    public void mousePressed(MouseEvent event) {
      Point p = event.getPoint();
      int col = _table.columnAtPoint(p);
      if (col == 0) {
        int row = _table.rowAtPoint(p);
        String selectedOption = (String)_options.get(row);
        if(_allowMultiple) {
          if(!_selectedOptions.contains(selectedOption)) {
            _selectedOptions.add(selectedOption);
          } else {
            _selectedOptions.remove(selectedOption);
          }
        } else {
          _selectedOptions = CollectionUtilities.createCollection(selectedOption);
        }
        _table.setModel(createTableModel());
      }
    }
  }
}