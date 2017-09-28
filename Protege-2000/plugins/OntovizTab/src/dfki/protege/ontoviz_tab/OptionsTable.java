package dfki.protege.ontoviz_tab;

import java.util.*;
import javax.swing.*;
import javax.swing.table.*;
import javax.swing.border.*;
import edu.stanford.smi.protege.model.*;


class OptionsTable extends AbstractTableModel implements OptionsTableConstants {

  Vector itsRows; // of Vector


  OptionsTable() {
    itsRows = new Vector();
  }


  void setupView(JTable table) {
    table.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
    for (int i = 0; i <= itsLastColumn; i++) {
      int width;
      if (i == 0) width = frameColWidth; else width = colWidth;
      setupTableColumn(table, columnNames[i], width, tips[i]);
    }
  }


  void setupTableColumn(JTable table, String col, int width, String tip) {

    TableColumn column;

    try {
      column = table.getColumn(col);
    } catch (Exception e) {
      System.out.println("column " + col + " cannot be set up");
      System.out.println(e);
      return;
    }

    // doesn't work in JDK1.3 !!!
    TableCellRenderer headerRenderer = column.getHeaderRenderer();
    if (headerRenderer instanceof DefaultTableCellRenderer)
      ((DefaultTableCellRenderer)headerRenderer).setToolTipText(tip);

    /* for JDK1.3; but: border does not work !!!
    DefaultTableCellRenderer headerRenderer =
      new DefaultTableCellRenderer();
    headerRenderer.setBorder(UIManager.getBorder("TableHeader.cellBorder"));
    headerRenderer.setToolTipText(tip);
    column.setHeaderRenderer(headerRenderer);
    */

    column.setPreferredWidth(width);

  }


  // adding and deleting rows

  void addFrame(Frame frame) {
    Vector row = new Vector();
    row.addElement(frame);
    for (int i = 1; i <= itsLastColumn; i++)
      row.addElement(Boolean.FALSE); // extend this (default values)
    itsRows.addElement(row);
    int rowNo = itsRows.size();
    fireTableRowsInserted(rowNo, rowNo);
  }

  void addFrames(Collection frames) {
    for (Iterator frameIterator = frames.iterator();
	 frameIterator.hasNext();) {
      Frame frame = (Frame)frameIterator.next();
      addFrame(frame);
    }
  }

  void deleteRow(int rowNo) {
    itsRows.removeElementAt(rowNo); 
    fireTableRowsDeleted(rowNo, rowNo);
  }

  void deleteAll() {
    int l = itsRows.size();
    itsRows.removeAllElements();
    fireTableRowsDeleted(0, l);
  }

  void deleteRows(int[] rows) {
    Arrays.sort(rows);
    int l = rows.length;
    for (int i = l-1; i >= 0; i--)
      deleteRow(rows[i]);
  }


  // implementation of AbstractTableModel

  public int getColumnCount() { return columnNames.length; }

  public int getRowCount() { return itsRows.size(); }

  public Object getValueAt(int rowNo, int col) {
    Vector row = (Vector)itsRows.elementAt(rowNo);
    Object value = row.elementAt(col);
    if (value instanceof Frame)
      return ((Frame)value).getBrowserText();
    else
      return value;
  }

  public String getColumnName(int column) { return columnNames[column]; }

  public Class getColumnClass(int c) {
    return getValueAt(0, c).getClass();
  }

  public boolean isCellEditable(int row, int col) { return col > 0; }

  public void setValueAt(Object value, int rowNo, int col) {
    // col must be > 0 !!
    Vector row = (Vector)itsRows.elementAt(rowNo);
    row.setElementAt(value, col);
  }

  // methods used in ExportDot:

  boolean isEmpty() { return itsRows.size() == 0; }

  void setTrue(int row, int col) { setValueAt(Boolean.TRUE, row, col); }

  Frame getFrame(int rowNo) {
    Vector row = (Vector)itsRows.elementAt(rowNo);
    return (Frame)row.elementAt(FRAME);
  }

  boolean isSet(int row, int col) {
    // for col > 0
    return ((Boolean)getValueAt(row, col)).booleanValue();
  }



}


