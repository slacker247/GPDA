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

class ClsClsReferenceRoot extends LazyTreeRoot {
    private Slot _slot;

    public ClsClsReferenceRoot(KnowledgeBase kb, Slot slot) {
        super(getReferencingClses(kb, slot));
        _slot = slot;
    }

    public LazyTreeNode createNode(Object o) {
        return new ClsClsReferenceNode(this, (Cls) o, _slot);
    }

    protected Comparator getComparator() {
        return new LazyTreeNodeFrameComparator();
    }

    private static Collection getReferencingClses(KnowledgeBase kb, Slot slot) {
        Collection clses = new ArrayList();
        if (slot == null) {
            clses = kb.getClses();
            /*
             * Not sure this is desired
             * Iterator i = clses.iterator();
             * while (i.hasNext()) {
             * Cls cls = (Cls) i.next();
             * if (!hasInstanceSlot(cls)) {
             * i.remove();
             * }
             * }
             */
        } else {
            clses = slot.getTemplateSlotClses();
        }
        return clses;
    }

    private static boolean hasInstanceSlot(Cls cls) {
        boolean hasInstanceSlot = false;
        Iterator i = cls.getTemplateSlots().iterator();
        while (i.hasNext()) {
            Slot slot = (Slot) i.next();
            if (cls.getTemplateSlotValueType(slot).equals(ValueType.INSTANCE)) {
                hasInstanceSlot = true;
                break;
            }
        }
        return hasInstanceSlot;
    }
}
