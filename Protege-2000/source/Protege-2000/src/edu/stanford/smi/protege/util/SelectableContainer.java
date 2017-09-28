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


import java.awt.*;
import java.util.*;
import javax.swing.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class SelectableContainer extends JComponent implements Selectable, Disposable {
    private ListenerCollection _listeners = new ListenerList(new SelectionEventDispatcher());
    private Selectable _selectable;

    private SelectionListener _selectionListener = new SelectionListener() {
        public void selectionChanged(SelectionEvent event) {
            onSelectionChange();
            notifySelectionListeners();
        }
    };

    public SelectableContainer() {
        setLayout(new BorderLayout());
    }

    public SelectableContainer(Selectable s) {
        this();
        addSelectable(s);
    }

    private void addSelectable(Selectable selectable) {
        _selectable = selectable;
        if (_selectable != null) {
            _selectable.addSelectionListener(_selectionListener);
        }
    }

    public void addSelectionListener(SelectionListener listener) {
        // Log.enter(this, "addSelectionListener", listener);
        _listeners.add(this, listener);
    }

    public void clearSelection() {
        _selectable.clearSelection();
    }

    public void dispose() {
        removeSelectable();
    }

    public Selectable getSelectable() {
        return _selectable;
    }

    public Collection getSelection() {
        return _selectable.getSelection();
    }

    public void notifySelectionListeners() {
        _listeners.postEvent(this, SelectionEvent.SELECTION_CHANGED);
    }

    public void onSelectionChange() {
        // do nothing
    }

    private void removeSelectable() {
        if (_selectable != null) {
            _selectable.removeSelectionListener(_selectionListener);
        }
        _selectable = null;
    }

    public void removeSelectionListener(SelectionListener listener) {
        _listeners.remove(this, listener);
    }

    public boolean setNotificationsEnabled(boolean b) {
        return _listeners.setPostingEnabled(b);
    }

    public void setSelectable(Selectable s) {
        removeSelectable();
        addSelectable(s);
    }
}
