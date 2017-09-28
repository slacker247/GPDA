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
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;

class ClsClsReferenceNode extends LazyTreeNode {
    private Slot _slot;

    public ClsClsReferenceNode(LazyTreeNode parent, Cls cls, Slot slot) {
        super(parent, cls);
        _slot = slot;
    }

    private void addClsReferences(Cls cls, Slot slot, Collection references) {
        KnowledgeBase kb = slot.getKnowledgeBase();
        ValueType type = cls.getTemplateSlotValueType(slot);
        if (type.equals(ValueType.INSTANCE)) {
            references.addAll(cls.getTemplateSlotAllowedClses(slot));
        } else if (type.equals(ValueType.CLS)) {
            references.addAll(cls.getTemplateSlotAllowedParents(slot));
        }
    }

    public LazyTreeNode createNode(Object o) {
        return new ClsClsReferenceNode(this, (Cls) o, _slot);
    }

    public int getChildObjectCount() {
        return getChildObjects().size();
    }

    public Collection getChildObjects() {
        Collection childClses = new HashSet();
        Cls cls = (Cls) getUserObject();
        if (_slot == null) {
            Iterator i = cls.getTemplateSlots().iterator();
            while (i.hasNext()) {
                Slot slot = (Slot) i.next();
                addClsReferences(cls, slot, childClses);
            }
        } else {
            if (cls.hasTemplateSlot(_slot)) {
                addClsReferences(cls, _slot, childClses);
            }
        }
        return childClses;
    }

    public Comparator getComparator() {
        return new LazyTreeNodeFrameComparator();
    }
}
