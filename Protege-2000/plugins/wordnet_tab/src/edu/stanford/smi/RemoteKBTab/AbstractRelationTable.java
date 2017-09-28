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

import javax.swing.*;
import java.util.*;
import java.awt.*;

/** AbstractRelationTable provides a default implementation of the
 *  RelationTable interface. Most RelationTable should probably inherit from
 *  this abstract class. */
public abstract class AbstractRelationTable implements RelationTable {
    protected JTable table;
    protected RelateTableModel tableModel;
    protected String[] labels;
    protected JScrollPane scrollpane;

    protected Vector concepts;
    protected Vector modelVector;

    public AbstractRelationTable() {
        this(new String[0], new Vector(), new Vector());
    }

    /** In this constructor, a table is set up with its table model. */
    public AbstractRelationTable(String[] labels,  Vector objVec,
                                 Vector strVec) {
        tableModel = new RelateTableModel();
        table = new JTable(tableModel);
        scrollpane = new JScrollPane(table);
        scrollpane.setPreferredSize(new Dimension(80,300));

        table.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
        concepts = objVec;
        modelVector = strVec;
        this.labels = labels;
    }

    /** Set the column names for the table. */
    public String[] getTableLabels() {
        return labels;
    }

    /** Deep copy vector. */
    protected void copyVec(Vector newvec,Vector oldvec) {
        if (newvec == null) {
            newvec = new Vector();
        }
	newvec.clear();
        if (oldvec == null) {
            return;
        }
	for (int i = 0; i<oldvec.size(); i++) {
	    newvec.addElement(oldvec.elementAt(i));
	}
    }

    public Vector getTableVecObj() {
        return (Vector) concepts;
    }

    public void setTableVecObj(Vector objVec) {
        copyVec(concepts, objVec);
    }

    /** Get the table String vector. */
    public Vector getModelVector() {
        return (Vector) modelVector;
    }

    /** Set table model vector */
    public void setModelVector(Vector strVec) {
        modelVector = new Vector();
        copyVec(modelVector, strVec);
    }

    /** Return the component which will be shown on the screen. The default
     *  one is a scrollpane. */
    public JComponent getComponent(){
        return scrollpane;
    }

    /** Return the table reference. */
    public JTable getTable() {
        return table;
    }

    /** Return the relate table model. */
    public RelateTableModel getTableModel() {
        return tableModel;
    }

    /**  matchnames is a Vector of Vectors which will be used to fill
     *   the tableModel.  These are all of the Strings that will appear in the
     *   table.  matchObjects are optional corresponding objects for each row,
     *   so matchObjects should have a size less than or equal to the size of
     *   matchnames. */
    public void setData(Vector matchnames, Vector matchObjects) {
        table.clearSelection();
        tableModel.clearAll();

        if (matchnames == null || matchnames.size() == 0) {
            return;
        }

	if (matchnames.size() > 0) {
	    for (int i = 0; i<matchnames.size(); i++) {
                tableModel.addRow((Vector)(matchnames.elementAt(i)));
	    }
	    for (int j = 0; j < matchObjects.size(); j++) {
                tableModel.addObjRow(matchObjects.elementAt(j));
            }
	}
    }
}
