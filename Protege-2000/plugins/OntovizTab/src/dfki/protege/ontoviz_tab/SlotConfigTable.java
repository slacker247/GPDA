package dfki.protege.ontoviz_tab;

import java.util.*;
import java.awt.*;
import javax.swing.*;
import javax.swing.table.*;
import javax.swing.border.*;
import edu.stanford.smi.protege.model.KnowledgeBase;
import edu.stanford.smi.protege.model.Slot;


class SlotConfigTable extends AbstractTableModel 
		      implements SlotConfigConstants {

  Vector itsRows; // of SlotConfig
  Hashtable itsSlotConfigs; // Slot -> SlotConfig
  // JTable itsView;
  JFrame itsFrame;
  JColorChooser itsColorChooser;


  SlotConfigTable(KnowledgeBase kb, SlotConfigTable oldTable) {
    itsRows = new Vector();
    itsSlotConfigs = new Hashtable();
    Collection slots = kb.getSlots();
    for (Iterator slotIterator = slots.iterator(); slotIterator.hasNext();) {
      Slot slot = (Slot)slotIterator.next();
      // instead of listeners, we use this simple hack:
      SlotConfig slotConfig = null;
      if (oldTable != null)   slotConfig = oldTable.getSlotConfig(slot);
      if (slotConfig == null) slotConfig = new SlotConfig(slot);
      itsSlotConfigs.put(slot, slotConfig);
      itsRows.add(slotConfig);
    }
    Collections.sort(itsRows, new Comparator() {
      public int compare(Object o1, Object o2) {
	SlotConfig sc1 = (SlotConfig)o1;
	SlotConfig sc2 = (SlotConfig)o2;
	if ((sc1.isSystemSlot() && sc2.isSystemSlot())
	    || (!sc1.isSystemSlot() && !sc2.isSystemSlot()))
	  return sc1.getSlotName().compareToIgnoreCase(sc2.getSlotName());
	else if (sc1.isSystemSlot())
	  return 1; // system slots come after normal ones
	else
	  return -1;
      }
    });

  }


  void setupView(JTable view, JFrame frame) {
    itsColorChooser = new JColorChooser();
    // itsView = view;
    itsFrame = frame;
    view.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
    // slot column
    TableColumn slotColumn = view.getColumn("slot");
    slotColumn.setPreferredWidth(200);
    // state column
    JComboBox comboBox = new JComboBox(state);
    /*
    comboBox.addItem(state[DEFAULT]);
    comboBox.addItem(state[HIDDEN]);
    comboBox.addItem(state[CONFIGURED]);
    */
    TableColumn stateColumn = view.getColumn("config");
    stateColumn.setCellEditor(new DefaultCellEditor(comboBox));
    DefaultTableCellRenderer stateColumnRenderer =
      new DefaultTableCellRenderer() {
	public Component getTableCellRendererComponent(
	    JTable table, Object value, 
	    boolean isSelected, boolean hasFocus, int row, int column) {
	  Component component = super.getTableCellRendererComponent(
	    table, value, isSelected, hasFocus, row, column);
	  if (column == 1) {
	    SlotConfig slotConfig = getSlotConfig(row);
	    if (slotConfig.is(CONFIGURED))
	      component.setForeground(slotConfig.getColor());
	    else if (slotConfig.is(HIDDEN))
	      component.setForeground(Color.gray);
	    else
	      component.setForeground(Color.black);
	  }
	  return component;
	}
      };
    stateColumn.setCellRenderer(stateColumnRenderer);
    stateColumn.setPreferredWidth(75);
  }

  SlotConfig getSlotConfig(int row) {
    return (SlotConfig)itsRows.elementAt(row);
  }

  SlotConfig getSlotConfig(Slot slot) {
    return (SlotConfig)itsSlotConfigs.get(slot);
  }

  int getState(Slot slot) {
    SlotConfig slotConfig = getSlotConfig(slot);
    if (slotConfig == null)
      return DEFAULT;
    else
      return slotConfig.getState();
  }

  boolean accept(Slot slot, boolean defaultHidden) {
    int state = getState(slot);
    return ((state == DEFAULT && !defaultHidden) || state == CONFIGURED);
  }

  Color getColor(Slot slot, Color defaultColor) {
    SlotConfig slotConfig = getSlotConfig(slot);
    if (slotConfig == null || !slotConfig.is(CONFIGURED))
      return defaultColor;
    else
      return slotConfig.getColor();
  }

  int getDirection(Slot slot) {
    SlotConfig slotConfig = getSlotConfig(slot);
    if (slotConfig == null || !slotConfig.is(CONFIGURED))
      return DIR_DEFAULT;
    else
      return slotConfig.getDirection();
  }

  void setDefault() {
    for (Iterator slotConfigIterator = itsRows.iterator(); 
	 slotConfigIterator.hasNext();) {
      SlotConfig slotConfig = (SlotConfig)slotConfigIterator.next();
      slotConfig.setState(DEFAULT);
    }
    fireTableDataChanged();
  }


  // implementation of AbstractTableModel

  public int getColumnCount() { return 2; }

  public int getRowCount() { return itsRows.size(); }

  public Object getValueAt(int rowNo, int col) {
    SlotConfig slotConfig = (SlotConfig)itsRows.elementAt(rowNo);
    if (col == 0)
      return slotConfig.getSlotName();
    else
      return slotConfig.getStateName();
  }

  public String getColumnName(int col) { 
    if (col == 0)
      return "slot";
    else
      return "config";
  }

  /*
  public Class getColumnClass(int c) {
    return getValueAt(0, c).getClass();
  }
  */

  public boolean isCellEditable(int row, int col) { return col > 0; }

  public void setValueAt(Object value, int row, int col) {
    // col must be > 0 !!
    SlotConfig slotConfig = (SlotConfig)itsRows.elementAt(row);
    if (value.equals(state[DEFAULT]))
      slotConfig.setState(DEFAULT);
    else if (value.equals(state[HIDDEN]))
      slotConfig.setState(HIDDEN);
    else {
      slotConfig.configure(itsFrame, itsColorChooser);
    }
  }



}


