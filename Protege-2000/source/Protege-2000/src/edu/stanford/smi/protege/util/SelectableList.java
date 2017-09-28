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


import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import javax.swing.event.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class SelectableList extends JList implements Selectable {
    private ListenerCollection _listeners = new ListenerList(new SelectionEventDispatcher());

    // This ugly flag allows us to work around the problem caused by dragging causing
    // the list selection to change
    private boolean _isDragEvent;
    private boolean _isMultiSelectPressEvent;
    private boolean _isDeferringSelection;

    public SelectableList() {
        addListSelectionListener(
            new ListSelectionListener() {
                public void valueChanged(ListSelectionEvent event) {
                    if (!event.getValueIsAdjusting()) {
                        notifySelectionListeners();
                    }
                }
            }
        );
    }

    public void addSelectionListener(SelectionListener listener) {
        // Log.enter(this, "addSelectionListener", listener);
        _listeners.add(this, listener);
    }

    public Collection getSelection() {
        return ComponentUtilities.getSelection(this);
    }

    public void notifySelectionListeners() {
        _listeners.postEvent(this, SelectionEvent.SELECTION_CHANGED);
    }

    public void processMouseEvent(MouseEvent event) {
        // Log.enter(this, "processMouseEvent", new Integer(event.getID()));
        _isMultiSelectPressEvent = false;
        int index = locationToIndex(event.getPoint());
        int id = event.getID();
        if (id == MouseEvent.MOUSE_PRESSED) {
            if (index == -1) {
                clearSelection();
            } else if (isSelectedIndex(index)) {
                _isMultiSelectPressEvent = true;
                _isDeferringSelection = true;
            }
        }
        if (_isDeferringSelection && id == MouseEvent.MOUSE_RELEASED) {
            setSelectionInterval(index, index);
            _isDeferringSelection = false;
        }
        super.processMouseEvent(event);
        _isMultiSelectPressEvent = false;
    }

    public void processMouseMotionEvent(MouseEvent event) {
        boolean dragDropEnabled = ComponentUtilities.isDragAndDropEnabled(this);
        _isDragEvent = dragDropEnabled && (event.getID() == MouseEvent.MOUSE_DRAGGED);
        super.processMouseMotionEvent(event);
        _isDragEvent = false;
    }

    public void removeSelectionListener(SelectionListener listener) {
        // Log.enter(this, "removeSelectionListener", listener);
        _listeners.remove(this, listener);
    }

    public void setSelectionInterval(int r1, int r2) {
        if (!_isDragEvent && !_isMultiSelectPressEvent) {
            super.setSelectionInterval(r1, r2);
        } else {
            // Log.trace("skipped", this, "setSelectionInterval", new Integer(r1), new Integer(r2));
        }
    }

    public String toString() {
        return "SelectableList";
    }
}
