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


import java.awt.datatransfer.*;
import java.awt.dnd.*;
import java.util.*;
import javax.swing.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public abstract class ListDragSourceListener implements DragGestureListener, DragSourceListener {
    private int[] _indexes;
    private Collection _objects;

    public abstract void doCopy(JComponent c, int[] indexes, Collection objects);

    public abstract void doMove(JComponent c, int[] indexes, Collection objects);

    public void dragDropEnd(DragSourceDropEvent e) {
        if (e.getDropSuccess()) {
            JComponent c = (JComponent) e.getDragSourceContext().getComponent();
            int action = e.getDropAction();
            if (action == DnDConstants.ACTION_MOVE) {
                // Log.trace("move", this, "dragDropEnd");
                doMove(c, _indexes, _objects);
            } else if (action == DnDConstants.ACTION_COPY) {
                // Log.trace("copy", this, "dragDropEnd");
                doCopy(c, _indexes, _objects);
            } else {
                // Log.trace("action=" + action, this, "dragDropEnd");
                // do nothing
            }
        } else {
            // Log.trace("failed", this, "dragDropEnd");
        }
    }

    public void dragEnter(DragSourceDragEvent e) {
    }

    public void dragExit(DragSourceEvent e) {
    }

    public void dragGestureRecognized(DragGestureEvent e) {
        // Log.enter(this, "dragGestureRecognized", e);
        JList list = (JList) e.getComponent();
        if (ComponentUtilities.isDragAndDropEnabled(list)) {
            _indexes = list.getSelectedIndices();
            _objects = Arrays.asList(list.getSelectedValues());
            if (!_objects.isEmpty()) {
                Transferable t = new TransferableCollection(_objects);
                e.startDrag(DragSource.DefaultMoveDrop, t, this);
            }
        }
    }

    public void dragOver(DragSourceDragEvent e) {
    }

    public void dropActionChanged(DragSourceDragEvent e) {
    }
}
