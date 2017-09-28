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

package edu.stanford.smi.protege.model.framedb;


import java.util.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;

/**
 * Description of Type
 *
 * @author Ray Fergerson <fergerson@smi.stanford.edu>
 */
public abstract class MultiMap {
    private Map _map;

    public MultiMap() {
        this(11);
    }

    public MultiMap(int initSize) {
        _map = new HashMap(initSize);
    }

    public void addValue(Object key, Object value) {
        Collection c = (Collection) _map.get(key);
        if (c == null) {
            c = createCollection();
            _map.put(key, c);
        }
        c.add(value);
    }

    public void addValues(Object key, Collection values) {
        Collection c = (Collection) _map.get(key);
        c.addAll(values);
    }

    public abstract Collection createCollection();

    public Collection getKeys() {
        return _map.keySet();
    }

    public Collection getValues(Object key) {
        return (Collection) _map.get(key);
    }

    public Collection removeKey(Object key) {
        return (Collection) _map.remove(key);
    }

    public void removeValue(Object key, Object value) {
        Collection c = (Collection) _map.get(key);
        if (c == null) {
            // Log.trace("key not found", this, "removeValue", key, value);
        } else {
            boolean succeeded = c.remove(value);
            if (!succeeded) {
                Log.trace("value not found", this, "removeValue", key, value);
            }
        }
    }

    public void removeValues(Object key, Collection values) {
        Collection c = (Collection) _map.get(key);
        c.removeAll(values);
    }
}
