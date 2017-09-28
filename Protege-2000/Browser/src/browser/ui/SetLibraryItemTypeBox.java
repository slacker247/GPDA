
/**
 * Title:        Browser<p>
 * Description:  This allows you to browse a library of knowledge bases.<p>
 * Copyright:    Copyright (c) Warren Shen<p>
 * Company:      Stanford Medical Informatics<p>
 * @author Warren Shen
 * @version 1.0
 */
package browser.ui;

import java.awt.event.*;
import java.awt.*;
import javax.swing.*;
import browser.model.*;
import java.util.*;
import edu.stanford.smi.protege.util.*;

public class SetLibraryItemTypeBox extends JFrame {
  private JPanel _panel;
  private ArrayList _checkBoxes;
  private JButton _button;
  private LibraryModel _model;
  private String _typeName;

  public SetLibraryItemTypeBox(LibraryModel model) {
    _model = model;
    ArrayList list = new ArrayList();
    list.add("HI");
    //init();
    this.getContentPane().add(new OptionsPanel(list));
    this.setTitle("Pick Item Type");
    this.setResizable(false);
    this.pack();
    this.show();
  }

  private void init() {
    _panel = (JPanel)this.getContentPane();
    _checkBoxes = new ArrayList();
    _panel.setLayout(new FlowLayout());
    Iterator iter = _model.getItemTypes().iterator();
    while(iter.hasNext()) {
      addCheckBox(((LibraryItem)iter.next()).getTypeName(), _panel);
    }
    selectCheckBox(_model.getCurrentItemType());
    _button = new JButton("OK");
    _button.setAction(new AbstractAction("OK") {
      public void actionPerformed(ActionEvent e) {
        setSelection();
        closeBox();
      }
    });
    _panel.add(_button);
  }

  private void addCheckBox(String name, JPanel panel) {
    JCheckBox box;
    box = new JCheckBox();
    box.setName(name);
    box.setAction(createAction(name));
    _checkBoxes.add(box);
    panel.add(box);
  }

  private AbstractAction createAction(String name) {
    return new SelectCheckBoxAction(name, this);
  }

  private void selectCheckBox(String name) {
    JCheckBox box;
    int i;
    for(i = 0; i < _checkBoxes.size(); i++) {
      box = (JCheckBox)_checkBoxes.get(i);
      box.setSelected(box.getName().equals(name));
    }
    _typeName = name;
  }

  public void setSelection() {
    _model.setItemType(_typeName);
  }

  public void closeBox() {
    this.dispose();
  }

  private class SelectCheckBoxAction extends AbstractAction {
    String _name;
    SetLibraryItemTypeBox _box;
    public SelectCheckBoxAction(String name, SetLibraryItemTypeBox box) {
      super(name);
      _box = box;
      _name = name;
    }

    public void actionPerformed(ActionEvent e) {
      _box.selectCheckBox(_name);
    }
  }
}