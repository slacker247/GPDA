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
public abstract class ListenerCollection {
    private boolean _isPostingEnabled = true;
    private EventDispatcher _dispatcher;

    public ListenerCollection(EventDispatcher dispatcher) {
        _dispatcher = dispatcher;
    }

    public void add(Object source, EventListener listener) {
        Collection c = getListeners(source);
        if (c == null) {
            c = newCollection();
            saveListeners(source, c);
        } else if (isPosting(source)) {
            c = newCollection(c);
            saveListeners(source, c);
        }
        if (c.contains(source)) {
            Log.warning("duplicate listener", this, "add", source, listener);
        }
        c.add(listener);
    }

    public abstract Collection getListeners(Object source);

    public abstract Collection getSources();

    public abstract boolean hasListeners(Object source);

    protected abstract boolean isPosting(Object source);

    public boolean isPostingEnabled() {
        return _isPostingEnabled;
    }

    private Collection newCollection() {
        return new LinkedList();
    }

    private Collection newCollection(Collection c) {
        return new LinkedList(c);
    }

    public void postEvent(Object source, int type) {
        postEvent(source, type, null, null, null);
    }

    public void postEvent(Object source, int type, Object arg1) {
        postEvent(source, type, arg1, null, null);
    }

    public void postEvent(Object source, int type, Object arg1, Object arg2) {
        postEvent(source, type, arg1, arg2, null);
    }

    public void postEvent(Object source, int type, Object arg1, Object arg2, Object arg3) {
        if (_isPostingEnabled) {
            // Log.trace("Posting", this, "postEvent", source, new Integer(type), arg1);
            Collection c = getListeners(source);
            if (c != null && !c.isEmpty()) {
                boolean wasPosting = setStartPosting(source);
                _dispatcher.postEvent(c, source, type, arg1, arg2, arg3);
                if (!wasPosting) {
                    setFinishPosting(source);
                }
            }
        }
    }

    public void remove(Object source, EventListener listener) {
        boolean wasAttached;
        Collection c = getListeners(source);
        if (c == null) {
            wasAttached = false;
        } else if (isPosting(source)) {
            c = newCollection(c);
            wasAttached = c.remove(listener);
            saveListeners(source, c);
        } else {
            wasAttached = c.remove(listener);
        }
        if (!wasAttached) {
            Log.warning("listener not attached", this, "remove", source, listener);
        }
    }

    public abstract void removeAllListeners(Object source);

    protected abstract void saveListeners(Object source, Collection listeners);

    protected abstract void setFinishPosting(Object source);

    public boolean setPostingEnabled(boolean postingEnabled) {
        boolean wasEnabled = _isPostingEnabled;
        _isPostingEnabled = postingEnabled;
        return wasEnabled;
    }

    protected abstract boolean setStartPosting(Object source);

    public String toString() {
        return getClass().getName();
    }
}
