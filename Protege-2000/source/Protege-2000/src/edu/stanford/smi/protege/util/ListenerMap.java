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
public class ListenerMap extends ListenerCollection {
    private Set _postingSources = new HashSet();
    private Map _listeners = new HashMap();

    // <Object, Collection<EventListener>
    public ListenerMap(EventDispatcher d) {
        super(d);
    }

    public Collection getListeners(Object o) {
        return (Collection) _listeners.get(o);
    }

    public Collection getSources() {
        return _listeners.keySet();
    }

    public boolean hasListeners(Object source) {
        Collection c = getListeners(source);
        return c != null && !c.isEmpty();
    }

    protected boolean isPosting(Object o) {
        return _postingSources.contains(o);
    }

    public void remove(Object source, EventListener listener) {
        super.remove(source, listener);
        Collection c = getListeners(source);
        if (c != null && c.isEmpty()) {
            _listeners.remove(source);
        }
    }

    public void removeAllListeners(Object source) {
        _listeners.remove(source);
    }

    protected void saveListeners(Object source, Collection c) {
        _listeners.put(source, c);
    }

    protected void setFinishPosting(Object o) {
        _postingSources.remove(o);
    }

    protected boolean setStartPosting(Object o) {
        return !_postingSources.add(o);
    }
}
