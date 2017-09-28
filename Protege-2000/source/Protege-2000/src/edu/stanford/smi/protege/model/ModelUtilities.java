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

package edu.stanford.smi.protege.model;

import java.util.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class ModelUtilities {

    public static void addOwnSlotValue(Frame frame, String slotName, Object value) {
        Assert.assertNotNull("value", value);
        frame.addOwnSlotValue(getSlot(frame, slotName), value);
    }

    // map from original frame to copied frame knowledge bases
    public static Map createValueMap(KnowledgeBase sourceKB, KnowledgeBase targetKB) {
        // Log.enter(ModelUtilities.class, "createValueMap", sourceKB, targetKB);
        Map valueMap = new HashMap();
        Iterator i = sourceKB.getFrames().iterator();
        while (i.hasNext()) {
            Frame sourceFrame = (Frame) i.next();
            if (sourceKB != targetKB || !isCopyable(sourceFrame)) {
                Frame targetFrame = targetKB.getFrame(sourceFrame.getName());
                valueMap.put(sourceFrame, targetFrame);
                // Log.trace("valueMap.put " + sourceFrame.getName() + " - " + targetFrame, ModelUtilities.class, "createValueMap");
            }
        }
        valueMap.put(sourceKB, targetKB);
        return valueMap;
    }

    private static Facet getFacet(Frame frame, String facetName) {
        Facet facet = frame.getKnowledgeBase().getFacet(facetName);
        if (facet == null) {
            Log.stack("missing facet", ModelUtilities.class, "getFacet", frame, facetName);
        }
        return facet;
    }

    public static Object getOwnSlotValue(Frame frame, String name) {
        Object value;
        Slot slot = getSlot(frame, name);
        if (slot == null) {
            value = null;
            Log.warning("unknown slot", ModelUtilities.class, "getOwnSlotValue", frame, name);
        } else {
            value = frame.getOwnSlotValue(slot);
        }
        return value;
    }

    public static Collection getOwnSlotValues(Frame frame, String name) {
        Collection values;
        Slot slot = getSlot(frame, name);
        if (slot == null) {
            values = Collections.EMPTY_LIST;
            Log.stack("unknown slot", ModelUtilities.class, "getOwnSlotValues", frame, name);
        } else {
            values = frame.getOwnSlotValues(slot);
        }
        return values;
    }

    public List getPath(Cls cls, List list) {
        list.add(0, cls);
        Cls superclass = (Cls) CollectionUtilities.getFirstItem(cls.getDirectSuperclasses());
        if (superclass != null) {
            getPath(superclass, list);
        }
        return list;
    }

    public static List getPathToRoot(Cls cls) {
        return getPathToRoot(cls, new LinkedList());
    }

    private static List getPathToRoot(Cls cls, LinkedList list) {
        list.add(0, cls);
        Cls superclass = (Cls) CollectionUtilities.getFirstItem(cls.getDirectSuperclasses());
        if (superclass != null) {
            getPathToRoot(superclass, list);
        }
        return list;
    }

    private static Slot getSlot(Frame frame, String slotName) {
        Assert.assertNotNull("frame", frame);
        Assert.assertNotNull("slot", slotName);
        Slot slot = frame.getKnowledgeBase().getSlot(slotName);
        if (slot == null) {
            Log.warning("missing slot", ModelUtilities.class, "getSlot", frame, slotName);
        }
        return slot;
    }

    public static Object getTemplateFacetValue(Cls cls, Slot slot, String facetName) {
        Object value;
        Facet facet = getFacet(cls, facetName);
        if (facet == null) {
            value = null;
        } else {
            value = cls.getTemplateFacetValue(slot, facet);
        }
        return value;
    }

    public static Collection getTemplateFacetValues(Cls cls, Slot slot, String facetName) {
        Collection values;
        Facet facet = getFacet(cls, facetName);
        if (facet == null) {
            values = Collections.EMPTY_LIST;
        } else {
            values = cls.getTemplateFacetValues(slot, facet);
        }
        return values;
    }

    private static boolean isCopyable(Frame frame) {
        // deep copy does not copy clses, slots, or facets within same kb
        return !(frame instanceof Cls ||
                frame instanceof Slot ||
                frame instanceof Facet);
    }

    public static void removeOwnSlotValue(Frame frame, String slotName, Object value) {
        frame.removeOwnSlotValue(getSlot(frame, slotName), value);
    }

    public static void setOwnSlotValue(Frame frame, String slotName, Object value) {
        frame.setOwnSlotValue(getSlot(frame, slotName), value);
    }

    public static void setOwnSlotValues(Frame frame, String slotName, Collection values) {
        frame.setOwnSlotValues(getSlot(frame, slotName), values);
    }

    public static void setTemplateFacetValue(Cls cls, Slot slot, String facetName, Object value) {
        cls.setTemplateFacetValue(slot, getFacet(cls, facetName), value);
    }

    public static void setTemplateFacetValues(Cls cls, Slot slot, String facetName, Collection values) {
        cls.setTemplateFacetValues(slot, getFacet(cls, facetName), values);
    }
}
