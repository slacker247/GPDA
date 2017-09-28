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

/** Related ListModel is the default list model which is used in the relation
 *  display. */
public class RelatedListModel extends AbstractListModel {

    private Vector data = new Vector();

    public RelatedListModel() {
    }

    /** Must override this function from ListModel. Return the length of the
     *  list. */
    public int getSize() {
        return (data.size());
    }

    /** Must override this function from ListModel. Return the value at
     *  specified index. */
    public Object getElementAt(int index) {
        return (data.elementAt(index));
    }

    /** Inserts the specified string at the end of this list. */
    public int addRow(String string) {
        data.addElement(string);
        fireIntervalAdded(this, data.size()-1, data.size()-1);
	return (data.size()-1);
    }

    /**  Clear all the list content. */
    public void clearRows() {
        int len = data.size();
        data.clear();
        fireIntervalRemoved(this,0, len);
    }

    /** Return the content of the list as a vector. */
    public Vector getData() {
        return (Vector) data;
    }

    /** Update the list with the value of the string array */
    public void setData(String[] itsdata) {
        clearRows();
	for (int i=0; i<itsdata.length; i++) {
            addRow(itsdata[i]);
	}
    }

    /** Deep copy the content of the list to specified string array. */
    public void copyInto(String[] strData) {
        for (int i=0; i<data.size(); i++) {
            strData[i] = new String((String)data.elementAt(i));
        }
    }

    /** Update the list with the value of a vector. */
    public void setTypes(Vector vecdata) {
        clearRows();
        for (int i=0; i<vecdata.size(); i++) {
            addRow((String)vecdata.elementAt(i));
        }
    }
}
