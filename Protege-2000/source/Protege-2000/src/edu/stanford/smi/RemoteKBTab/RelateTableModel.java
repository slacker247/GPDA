/*
 * The contents of this file are subject to the Mozilla Public License
 * Version 1.1 (the "License");  you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * The Original Code is Protege-2000.
 *
 * The Initial Developer of the Original Code is Stanford University. Portions
 * created by Stanford University are Copyright (C) 2001.  All Rights Reserved.
 *
 * Protege-2000 was developed by Stanford Medical Informatics
 * (http://www.smi.stanford.edu) at the Stanford University School of Medicine
 * with support from the National Library of Medicine, the National Science
 * Foundation, and the Defense Advanced Research Projects Agency.  Current
 * information about Protege can be obtained at http://protege.stanford.edu
 *
 * Contributor(s):
 */

package edu.stanford.smi.RemoteKBTab;

import java.util.*;
import javax.swing.table.*;

/** Related TableModel is the default table model which is used in
    the relation table. */
public class RelateTableModel extends DefaultTableModel {
  Vector objDataVector;

  /** Constructor. */
  public RelateTableModel() {
     objDataVector = new Vector();
  }

  /** Get the table data from defaultTableModel. */
  public Vector getData() {
    return dataVector;
  }

  /** Get the column label from defaultTableModel. */
  public Vector getColLabel() {
    return columnIdentifiers;
  }

  /** Clear the Table. */
  public void clearAll() {
    synchronized(this) {
      // dataVector.clear();
      for(int i=this.getRowCount()-1;i>=0;i--)
        this.removeRow(i);
      objDataVector.clear();
    }
  }

  /**  This default implementation returns false for all cells. */
  public boolean isCellEditable(int row,   int column) {
     return false;
  }

  /** Call the default TableModel setValueAt directly. */
  public void setValueAt(Object aValue, int row, int column){
     if (row >= dataVector.size() || (column >=((Vector)dataVector.get(row)).size())){
      System.out.println("calling with an invalid row or column number");
      return;
     }
     super.setValueAt(aValue, row, column);
  }

  /** Get the Object correspond to specified row. This is for the
      RelationTable interface. */
  public Object getObjRow(int i) {
     return (Object) objDataVector.elementAt(i);
  }

  /** Add specified object at the end of objDataVector. This is for the
      RelationTable interface. */
  public void addObjRow(Object obj) {
     objDataVector.addElement(obj);
  }
}

