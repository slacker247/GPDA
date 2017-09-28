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
public class SlotSubslotRoot extends LazyTreeRoot {
    private KnowledgeBase _knowledgeBase;

    private KnowledgeBaseListener _listener = new KnowledgeBaseAdapter() {
        public void slotCreated(KnowledgeBaseEvent event) {
            super.slotCreated(event);
            Slot slot = event.getSlot();
            // Log.enter(this, "slotCreated", slot);
            if (slot.getDirectSuperslots().isEmpty()) {
                List slots = (List) getUserObject();
                int index = Collections.binarySearch(slots, slot, new FrameComparator());
                if (index < 0) {
                    index = -(index + 1);
                }
                slots.add(index, slot);
                childAdded(slot, index);
            }
        }
        public void slotDeleted(KnowledgeBaseEvent event) {
            super.slotDeleted(event);
            Slot slot = event.getSlot();
            List slots = (List) getUserObject();
            boolean changed = slots.remove(slot);
            if (changed) {
                childRemoved(slot);
            }
        }
    };

    public SlotSubslotRoot(KnowledgeBase kb) {
        super(getSlots(kb));
        kb.addKnowledgeBaseListener(_listener);
        _knowledgeBase = kb;
    }

    public LazyTreeNode createNode(Object o) {
        return new SlotSubslotNode(this, (Slot) o);
    }

    public void dispose() {
        super.dispose();
        _knowledgeBase.removeKnowledgeBaseListener(_listener);
    }

    public Comparator getComparator() {
        return new LazyTreeNodeFrameComparator();
    }

    private static Collection getSlots(KnowledgeBase kb) {
        List results = new ArrayList(kb.getSlots());
        Iterator i = results.iterator();
        while (i.hasNext()) {
            Slot slot = (Slot) i.next();
            if (slot.getDirectSuperslotCount() > 0) {
                i.remove();
            }
        }
        Collections.sort(results, new FrameComparator());
        return results;
    }
}
