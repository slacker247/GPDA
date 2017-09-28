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

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public final class BidirectionalMultiMap {
    private Map _values; // <Object, ExtendedList<Object>>
    private Map _keys;

    private final static int THRESHOLD = 10;

    // <Object, ExtendedList<Object>>
    public BidirectionalMultiMap(int startSize) {
        _values = new HashMap(startSize);
        _keys = new HashMap(startSize);
    }

    private void addBackwardReference(Object key, Object value) {
        getOrCreateKeyCollection(value, 1).add(key);
    }

    private void addBackwardReferences(Object key, Collection values) {
        Iterator i = values.iterator();
        while (i.hasNext()) {
            Object o = i.next();
            addBackwardReference(key, o);
        }
    }

    private void addForwardReference(Object key, Object value) {
        getOrCreateValueList(key, 1).add(value);
    }

    private void addForwardReference(Object key, Object value, int index) {
        getOrCreateValueList(key, 1).add(index, value);
    }

    private void addForwardReferences(Object key, Collection values) {
        getOrCreateValueList(key, values.size()).addAll(values);
    }

    public void addValue(Object key, Object value) {
        Assert.assertTrue("value not collection", !(value instanceof Collection));
        addForwardReference(key, value);
        addBackwardReference(key, value);
    }

    public void addValue(Object key, Object value, int index) {
        Assert.assertTrue("value not collection", !(value instanceof Collection));
        addForwardReference(key, value, index);
        addBackwardReference(key, value);
    }

    public void addValues(Object key, Collection values) {
        Collection c = getOrCreateValueList(key, values.size());
        c.addAll(values);
        // adding forward references
        Iterator i = values.iterator();
        while (i.hasNext()) {
            Object value = i.next();
            addBackwardReference(key, value);
        }
    }

    private ExtendedList checkCollection(ExtendedList c, int size) {
        ExtendedList newCollection;
        if (c == null) {
            if (size < THRESHOLD) {
                newCollection = new ExtendedArrayList(size);
            } else {
                newCollection = new HashList();
            }
        } else if (c instanceof HashList) {
            newCollection = c;
        } else if (c.size() + size > THRESHOLD) {
            newCollection = new HashList();
            newCollection.addAll(c);
            // Log.trace("converting to hashlist (" + ++nHashs + ") on " + c.size() + " + " + size, this, "checkCollection");
        } else {
            newCollection = c;
        }
        return newCollection;
    }

    private ExtendedList getKeyCollection(Object value) {
        return (ExtendedList) _keys.get(value);
    }

    public Collection getKeys() {
        return Collections.unmodifiableCollection(_values.keySet());
    }

    public Collection getKeys(Object value) {
        Collection keys;
        ExtendedList c = getKeyCollection(value);
        if (c == null) {
            Log.trace("unreferenced value", this, "getKeys", value);
            keys = Collections.EMPTY_LIST;
        } else {
            keys = c.getUnmodifiableList();
        }
        return keys;
    }

    private ExtendedList getOrCreateKeyCollection(Object value, int size) {
        ExtendedList currentCollection = getKeyCollection(value);
        ExtendedList newCollection = checkCollection(currentCollection, size);
        if (currentCollection != newCollection) {
            _keys.put(value, newCollection);
        }
        return newCollection;
    }

    private ExtendedList getOrCreateValueList(Object key, int size) {
        ExtendedList currentCollection = getValueList(key);
        ExtendedList newCollection = checkCollection(currentCollection, size);
        if (currentCollection != newCollection) {
            _values.put(key, newCollection);
        }
        return newCollection;
    }

    public Object getValue(Object key) {
        ExtendedList c = getValueList(key);
        return (c == null || c.isEmpty()) ? (Object) null : c.get(0);
    }

    public int getValueCount(Object key) {
        // Log.enter(this, "getValueCount");
        Collection c = getValueList(key);
        return (c == null) ? -1 : c.size();
    }

    // -------------------------------------------------------------------------
    private ExtendedList getValueList(Object key) {
        return (ExtendedList) _values.get(key);
    }

    public List getValues(Object key) {
        ExtendedList values = getValueList(key);
        return (values == null) ? (List) null : values.getUnmodifiableList();
    }

    private void moveForwardReference(Object key, int fromIndex, int toIndex) {
        ExtendedList c = getValueList(key);
        Object o = c.remove(fromIndex);
        if (fromIndex < toIndex) {
            --toIndex;
        }
        c.add(toIndex, o);
    }

    private void moveForwardReference(Object key, Object value, int toIndex) {
        ExtendedList c = getValueList(key);
        int fromIndex = c.indexOf(value);
        Object o = c.remove(fromIndex);
        if (fromIndex < toIndex) {
            --toIndex;
        }
        c.add(toIndex, o);
    }

    public void moveValue(Object key, int fromIndex, int toIndex) {
        moveForwardReference(key, fromIndex, toIndex);
    }

    public void moveValue(Object key, Object value, int toIndex) {
        moveForwardReference(key, value, toIndex);
    }

    private void removeBackwardReference(Object key, Object value) {
        Collection c = getKeyCollection(value);
        if (c == null) {
            // Log.stack("back ref collection doesn't exist", this, "removeBackwardReference", key, value);
            throw new RuntimeException("back ref collection doesn't exist");
        } else {
            // Log.trace("removing backward ref", this, "removeBackwardReference", key, value);
            c.remove(key);
        }
    }

    private Object removeForwardReference(Object key, int index) {
        return getValueList(key).remove(index);
    }

    private void removeForwardReference(Object key, Object value) {
        Collection c = getValueList(key);
        if (c == null) {
            Log.warning("forward ref collection doesn't exist", this, "removeForwardReference", key, value);
        } else {
            // Log.trace("removing forward ref", this, "removeForwardReference", key, value);
            c.remove(value);
        }
    }

    public void removeKey(Object key) {
        Collection c = getValueList(key);
        if (c != null) {
            if (!c.isEmpty()) {
                Iterator i = c.iterator();
                while (i.hasNext()) {
                    Object value = i.next();
                    removeBackwardReference(key, value);
                }
            }
            removeValueCollection(key);
        }
    }

    private void removeKeyCollection(Object value) {
        _keys.remove(value);
    }

    public void removeReferencesToValue(Object value) {
        Assert.assertTrue("not a collection", !(value instanceof Collection));
        Collection c = getKeyCollection(value);
        if (c != null) {
            Iterator i = c.iterator();
            while (i.hasNext()) {
                Object key = i.next();
                removeForwardReference(key, value);
            }
            removeKeyCollection(value);
        }
    }

    public Object removeValue(Object key, int index) {
        Object value = removeForwardReference(key, index);
        removeBackwardReference(key, value);
        return value;
    }

    public void removeValue(Object key, Object value) {
        removeForwardReference(key, value);
        removeBackwardReference(key, value);
    }

    private void removeValueCollection(Object key) {
        _values.remove(key);
    }

    public void replaceValue(Object key, int index, Object value) {
        // Log.enter(this, "replaceValue", key, fromValue, toValue);
        getValueList(key).set(index, value);
    }

    public void replaceValue(Object key, Object from, Object to) {
        // Log.enter(this, "replaceValue", key, fromValue, toValue);
        getValueList(key).replace(from, to);
    }

    /**
     *  a null value is interpreted as an empty collection use "removeValue" if
     *  you want to get rid of the key
     *
     * @param  key    The new Value value
     * @param  value  The new Value value
     */
    public void setValue(Object key, Object value) {
        Assert.assertTrue("value not collection", !(value instanceof Collection));
        removeKey(key);
        if (value == null) {
            getOrCreateValueList(key, 0);
        } else {
            addValue(key, value);
        }
    }

    public void setValues(Object key, Collection values) {
        removeKey(key);
        addValues(key, values);
    }

    public String toString() {
        return "BidirectionalMultiMap";
    }
}
