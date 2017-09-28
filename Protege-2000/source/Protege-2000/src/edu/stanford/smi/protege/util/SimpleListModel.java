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

package edu.stanford.smi.protege.util;


import java.util.*;
import javax.swing.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class SimpleListModel extends AbstractListModel {
    private List _list;

    public SimpleListModel() {
        _list = new ArrayList();
    }

    public SimpleListModel(Collection values) {
        _list = new ArrayList(values);
    }

    public int addValue(Object o) {
        int index = _list.size();
        _list.add(index, o);
        fireIntervalAdded(this, index, index);
        return index;
    }

    public void addValue(Object o, int index) {
        _list.add(index, o);
        fireIntervalAdded(this, index, index);
    }

    public int addValues(Collection values) {
        int startIndex = _list.size();
        int endIndex = startIndex + values.size();
        _list.addAll(values);
        fireIntervalAdded(this, startIndex, endIndex);
        return startIndex;
    }

    public void clear() {
        if (!_list.isEmpty()) {
            int index = _list.size() - 1;
            _list.clear();
            fireIntervalRemoved(this, 0, index);
        }
    }

    public boolean contains(Object o) {
        return _list.contains(o);
    }

    public Object getElementAt(int i) {
        return _list.get(i);
    }

    public int getSize() {
        return _list.size();
    }

    public List getValues() {
        return Collections.unmodifiableList(_list);
    }

    public int indexOf(Object o) {
        return _list.indexOf(o);
    }

    public void moveValue(int start, int end) {
        if (start != end) {
            Object o = _list.remove(start);
            if (end > start) {
                --end;
            }
            _list.add(end, o);
            fireContentsChanged(this, start, end);
        }
    }

    public int removeValue(Object o) {
        int index = _list.indexOf(o);
        if (index == -1) {
            // Log.stack("not in list", this, "removeValue", o);
        } else {
            _list.remove(index);
            fireIntervalRemoved(this, index, index);
        }
        return index;
    }

    public int removeValues(Collection values) {
        int startIndex = -1;
        Iterator i = values.iterator();
        while (i.hasNext()) {
            Object o = i.next();
            if (startIndex == -1) {
                startIndex = indexOf(o);
            }
            removeValue(o);
        }
        return startIndex;
    }

    public void setValue(int index, Object o) {
        _list.set(index, o);
        fireContentsChanged(this, index, index);
    }

    public void setValues(Collection values) {
        clear();
        if (!values.isEmpty()) {
            _list = new ArrayList(values);
            fireIntervalAdded(this, 0, values.size() - 1);
        }
    }

    public String toString() {
        return "SimpleListModel";
    }
}
