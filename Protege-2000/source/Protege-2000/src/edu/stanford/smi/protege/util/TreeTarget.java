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
import java.awt.datatransfer.*;
import java.awt.dnd.*;
import java.util.*;
import javax.swing.*;
import javax.swing.tree.*;
import edu.stanford.smi.protege.model.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public abstract class TreeTarget implements DropTargetListener {
    private static DataFlavor _localTransferFlavor;
    private int _dropSelectionRow;
    private Object _dropSelectionArea;
    private boolean _allowsBetweenDrops;

    static {
        try {
            _localTransferFlavor = new DataFlavor(DataFlavor.javaJVMLocalObjectMimeType);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public TreeTarget(boolean allowsBetweenDrops) {
        this._allowsBetweenDrops = allowsBetweenDrops;
    }

    private void clearDropSelection(JTree tree) {
        tree.putClientProperty(DefaultRenderer.DROP_TARGET, null);
        tree.putClientProperty(DefaultRenderer.DROP_TARGET_AREA, null);
        _dropSelectionRow = -1;
        _dropSelectionArea = null;
        tree.repaint();
    }

    public abstract boolean doDrop(JTree tree, Object source, int row, Object area);

    protected boolean doDrop(JTree tree, Collection sources, int row, Object area) {
        boolean succeeded = true;
        Iterator i = sources.iterator();
        while (i.hasNext()) {
            Object source = i.next();
            succeeded = doDrop(tree, source, _dropSelectionRow, _dropSelectionArea);
        }
        return succeeded;
    }

    public void dragEnter(DropTargetDragEvent e) {
        // e.acceptDrag(DnDConstants.ACTION_MOVE);
        e.acceptDrag(e.getDropAction());
    }

    public void dragExit(DropTargetEvent e) {
        clearDropSelection(getTree(e));
    }

    public void dragOver(DropTargetDragEvent e) {
        setDropSelection(e);
        // e.acceptDrag(DnDConstants.ACTION_MOVE);
        e.acceptDrag(e.getDropAction());
    }

    public void drop(DropTargetDropEvent e) {
        boolean succeeded = false;
        JTree tree = getTree(e);
        if (e.isDataFlavorSupported(_localTransferFlavor)) {
            try {
                int action = e.getDropAction();
                e.acceptDrop(action);
                setDropSelection(getTree(e), e.getLocation());
                if (_dropSelectionRow != -1) {
                    Collection sources = (Collection) e.getTransferable().getTransferData(_localTransferFlavor);
                    succeeded = doDrop(tree, sources, _dropSelectionRow, _dropSelectionArea);
                } else {
                    Log.trace("no drop selection", this, "drop");
                }
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        } else {
            e.rejectDrop();
            Log.error("unsupported flavor", this, "drop", e);
        }
        e.dropComplete(succeeded);
        clearDropSelection(tree);
    }

    public void dropActionChanged(DropTargetDragEvent e) {
        // e.acceptDrag(DnDConstants.ACTION_MOVE);
        e.acceptDrag(e.getDropAction());
    }

    private Object eventToObject(DropTargetDropEvent e) {
        TreePath path = getTree(e).getPathForLocation(e.getLocation().x, e.getLocation().y);
        Object object;
        if (path == null) {
            object = null;
        } else {
            object = ((LazyTreeNode) path.getLastPathComponent()).getUserObject();
        }
        return object;
    }

    private JTree getTree(DropTargetDragEvent e) {
        return (JTree) e.getDropTargetContext().getComponent();
    }

    private JTree getTree(DropTargetEvent e) {
        return (JTree) e.getDropTargetContext().getComponent();
    }

    private void setDropSelection(DropTargetDragEvent e) {
        setDropSelection(getTree(e), e.getLocation());
    }

    private void setDropSelection(JTree tree, Point p) {
        _dropSelectionRow = tree.getRowForLocation(p.x, p.y);
        if (_dropSelectionRow == -1) {
            clearDropSelection(tree);
        } else {
            if (_allowsBetweenDrops) {
                Rectangle r = tree.getRowBounds(_dropSelectionRow);
                if (p.y < r.y + r.height / 4) {
                    if (_dropSelectionRow == 0) {
                        _dropSelectionArea = DefaultRenderer.DROP_TARGET_AREA_ABOVE;
                    } else {
                        --_dropSelectionRow;
                        _dropSelectionArea = DefaultRenderer.DROP_TARGET_AREA_BELOW;
                    }
                } else if (p.y < r.y + 3 * r.height / 4) {
                    _dropSelectionArea = DefaultRenderer.DROP_TARGET_AREA_ON;
                } else {
                    _dropSelectionArea = DefaultRenderer.DROP_TARGET_AREA_BELOW;
                }
            } else {
                _dropSelectionArea = DefaultRenderer.DROP_TARGET_AREA_ON;
            }

            TreePath path = tree.getPathForRow(_dropSelectionRow);
            LazyTreeNode node = (LazyTreeNode) path.getLastPathComponent();
            tree.putClientProperty(DefaultRenderer.DROP_TARGET, node);
            tree.putClientProperty(DefaultRenderer.DROP_TARGET_AREA, _dropSelectionArea);
            tree.repaint();
            Thread.yield();
        }
    }
}
