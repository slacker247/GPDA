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

package edu.stanford.smi.protege.ui;


import java.util.*;
import edu.stanford.smi.protege.event.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class ParentChildNode extends LazyTreeNode {
    private ClsListener _clsListener = new ClsAdapter() {
        public void directSubclassAdded(ClsEvent event) {
            childAdded(event.getSubclass());
            // Log.enter(ParentChildNode.this, "directSubclassAdded");
        }

        public void directSubclassRemoved(ClsEvent event) {
            childRemoved(event.getSubclass());
            // Log.enter(ParentChildNode.this, "directSubclassRemoved");
        }

        public void directSubclassMoved(ClsEvent event) {
            Cls subclass = event.getSubclass();
            childRemoved(subclass);
            int index = ((List) getCls().getDirectSubclasses()).indexOf(subclass);
            childAdded(subclass, index);
            // Log.enter(ParentChildNode.this, "directSubclassesMoved", subclass, new Integer(index));
        }

        public void directInstanceCreated(ClsEvent event) {
            notifyNodeChanged();
        }

        public void directInstanceDeleted(ClsEvent event) {
            notifyNodeChanged();
        }

        public void templateFacetValueChanged(ClsEvent event) {
            notifyNodeChanged();
        }

        public void directSuperclassAdded(ClsEvent event) {
            notifyNodeChanged();
        }
    };

    private FrameListener _frameListener = new FrameAdapter() {
        public void browserTextChanged(FrameEvent event) {
            notifyNodeChanged();
        }

        public void ownSlotValueChanged(FrameEvent event) {
            notifyNodeChanged();
        }

        public void visibilityChanged(FrameEvent event) {
            notifyNodeChanged();
        }
    };
    private KnowledgeBaseListener _knowledgeBaseListener;

    public ParentChildNode(LazyTreeNode parentNode, Cls parentCls) {
        super(parentNode, parentCls);
        parentCls.addClsListener(_clsListener);
        parentCls.addFrameListener(_frameListener);
        if (parentCls.isSlotMetaCls() || parentCls.isFacetMetaCls()) {
            _knowledgeBaseListener =
                new KnowledgeBaseAdapter() {
                    public void defaultSlotMetaClsChanged(KnowledgeBaseEvent event) {
                        super.defaultSlotMetaClsChanged(event);
                        notifyNodeChanged();
                    }

                    public void defaultFacetMetaClsChanged(KnowledgeBaseEvent event) {
                        notifyNodeChanged();
                    }
                }
            ;
            parentCls.getKnowledgeBase().addKnowledgeBaseListener(_knowledgeBaseListener);
        }
    }

    protected LazyTreeNode createNode(Object o) {
        return new ParentChildNode(this, (Cls) o);
    }

    protected void dispose() {
        super.dispose();
        getCls().removeClsListener(_clsListener);
        getCls().removeFrameListener(_frameListener);
        if (_knowledgeBaseListener != null) {
            getCls().getKnowledgeBase().removeKnowledgeBaseListener(_knowledgeBaseListener);
        }
    }

    protected int getChildObjectCount() {
        return (showHidden()) ? getCls().getDirectSubclassCount() : getCls().getVisibleDirectSubclassCount();
    }

    protected Collection getChildObjects() {
        return (showHidden()) ? getCls().getDirectSubclasses() : getCls().getVisibleDirectSubclasses();
    }

    protected Cls getCls() {
        return (Cls) getUserObject();
    }

    protected Comparator getComparator() {
        return new LazyTreeNodeFrameComparator();
    }

    protected void notifyNodeChanged() {
        notifyNodeChanged(this);
    }

    private boolean showHidden() {
        return getCls().getProject().getDisplayHiddenClasses();
    }

    public String toString() {
        return "ParentChildNode(" + getCls() + ")";
    }
}
