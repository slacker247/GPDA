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

class ReferenceNode extends LazyTreeNode {
    private Slot _slot;

    private ClsListener _clsListener =
        new ClsAdapter() {
            public void templateFacetValueChanged(ClsEvent event) {
                reload();
            }
        }
    ;
    private FrameListener _frameListener =
        new FrameAdapter() {
            public void nameChanged(FrameEvent event) {
                notifyNodeChanged();
            }

            public void ownSlotValueChanged(FrameEvent event) {
                reload();
            }

            public void visibilityChanged(FrameEvent event) {
                notifyNodeChanged();
            }
        }
    ;

    public ReferenceNode(LazyTreeNode parent, Frame frame, Slot slot) {
        super(parent, frame);
        _slot = slot;
        frame.addFrameListener(_frameListener);
        if (frame instanceof Cls) {
            ((Cls) frame).addClsListener(_clsListener);
        }
    }

    private void addOwnSlotReferences(Frame frame, Slot slot, Collection references) {
        if (frame.hasOwnSlot(slot)) {
            ValueType type = frame.getOwnSlotValueType(slot);
            if (type.equals(ValueType.INSTANCE) || type.equals(ValueType.CLS)) {
                references.addAll(frame.getOwnSlotValues(slot));
            }
        }
    }

    private void addReferences(Frame frame, Slot slot, Collection references) {
        if (frame instanceof Cls) {
            Cls cls = (Cls) frame;
            addTemplateSlotReferences(cls, slot, references);
        }
        addOwnSlotReferences(frame, slot, references);
    }

    private void addTemplateSlotReferences(Cls cls, Slot slot, Collection references) {
        KnowledgeBase kb = slot.getKnowledgeBase();
        if (cls.hasTemplateSlot(slot)) {
            ValueType type = cls.getTemplateSlotValueType(slot);
            if (type.equals(ValueType.INSTANCE)) {
                references.addAll(cls.getTemplateSlotAllowedClses(slot));
            } else if (type.equals(ValueType.CLS)) {
                references.addAll(cls.getTemplateSlotAllowedParents(slot));
            }
        }
    }

    public LazyTreeNode createNode(Object o) {
        return new ReferenceNode(this, (Frame) o, _slot);
    }

    public int getChildObjectCount() {
        return getChildObjects().size();
    }

    public Collection getChildObjects() {
        Collection references = new ArrayList();
        Frame frame = (Frame) getUserObject();
        if (_slot == null) {
            if (frame instanceof Cls) {
                Cls cls = (Cls) frame;
                Iterator i = cls.getTemplateSlots().iterator();
                while (i.hasNext()) {
                    Slot slot = (Slot) i.next();
                    addTemplateSlotReferences(cls, slot, references);
                }
            }
            Iterator j = frame.getOwnSlots().iterator();
            while (j.hasNext()) {
                Slot slot = (Slot) j.next();
                if (!slot.isSystem()) {
                    addOwnSlotReferences(frame, slot, references);
                }
            }
        } else {
            addReferences(frame, _slot, references);
        }
        return references;
    }

    public Comparator getComparator() {
        return new LazyTreeNodeFrameComparator();
    }

    protected void notifyNodeChanged() {
        notifyNodeChanged(this);
    }
}
