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

package edu.stanford.smi.protegex.queries_tab;

import java.awt.*;
import javax.swing.*;
import javax.swing.border.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.event.*;
import java.io.*;
import edu.stanford.smi.protegex.queries_tab.toolbox.*;

public class QueryListModel extends AbstractListModel{
   private Vector data = new Vector();

    public QueryListModel() {
    }

    public QueryListModel(Vector v) {
        data = v;
    }

    public int addRow(Object obj) {
        data.addElement(obj);
        fireIntervalAdded(this, data.size() - 1, data.size() - 1);
        return (data.size() - 1);
    }

    public void deleteRow(int row) {
        InstancesQuery query = (InstancesQuery) data.elementAt(row);
        query.changed("DELETED");
        removeQuery(row);
        data.removeElementAt(row);
        fireIntervalRemoved(this, row, row);

    }

    public Object getElementAt(int index) {
        return getQueryAt(index);
    }

    public int getPosition(InstancesQuery query) {
        if (query == null)
            return -1;
        for (int i = 0; i < data.size(); i++) {
            InstancesQuery q = (InstancesQuery) data.elementAt(i);
            if (q.getName().equalsIgnoreCase(query.getName()))
                return i;
        }
        return -1;
    }

    public Collection getQueries() {
        return (Collection) data;
    }

    public InstancesQuery getQuery(InstancesQuery query) {
        for (int i = 0; i < data.size(); i++) {
            if (getQueryAt(i).getName().equalsIgnoreCase(query.getName()))
                return getQueryAt(i);
        }
        return null;
    }

    public InstancesQuery getQueryAt(int index) {
        InstancesQuery query = (InstancesQuery) data.elementAt(index);
        if (query == null)
            return null;
        else
            return query;

    }

    public InstancesQuery getQueryWithName(String name) {
        for (int i = 0; i < data.size(); i++) {
            InstancesQuery query = (InstancesQuery) data.elementAt(i);
            if (query.getName().equalsIgnoreCase(name))
                return query;
        }
        return null;
    }

    public int getSize() {
        return (data.size());
    }

    // this is used to try to remove the query once it is removed from the querylist.
    // One problem happened is what we should do if one of the query is now being viewed.
    private void removeQuery(int index) {
        InstancesQuery query = (InstancesQuery) data.elementAt(index);
        query = null;
    }
}
