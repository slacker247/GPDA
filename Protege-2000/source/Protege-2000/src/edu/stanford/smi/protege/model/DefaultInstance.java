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
import edu.stanford.smi.protege.event.*;
import edu.stanford.smi.protege.util.*;

/**
 * Default implementation of Instance interface.  Forwards all method calls
 * to its DefaultKnowledgeBase.  This is the base class of all of the concrete
 * frame classes such as those for slot, class, etc.
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public abstract class DefaultInstance extends DefaultFrame implements Instance {

    DefaultInstance(KnowledgeBase kb, FrameID id) {
        super(kb, id);
    }

    public void addInstanceListener(InstanceListener listener) {
        getDefaultKnowledgeBase().addInstanceListener(this, listener);
    }

    public Frame deepCopy(KnowledgeBase targetKB, Map valueMap) {
        // Log.enter(this, "deepCopy", targetKB);
        KnowledgeBase sourceKB = getKnowledgeBase();
        if (targetKB == null) {
            targetKB = sourceKB;
        }
        if (valueMap == null) {
            valueMap = ModelUtilities.createValueMap(sourceKB, targetKB);
        }
        Instance copy = (Instance) valueMap.get(this);
        if (copy == null) {
            String name = null;
            Cls copyCls = (Cls) valueMap.get(getDirectType());
            Assert.assertNotNull("copy class", copyCls);
            copy = targetKB.createInstance(name, copyCls);
            valueMap.put(this, copy);
        }
        return super.deepCopy(targetKB, valueMap);
    }

    public String getBrowserText() {
        return getDefaultKnowledgeBase().getBrowserText(this);
    }

    public Cls getDirectType() {
        return getDefaultKnowledgeBase().getDirectType(this);
    }

    public Object getOwnFacetValue(Slot slot, Facet facet) {
        return getDirectType().getTemplateFacetValue(slot, facet);
    }

    public Collection getOwnFacetValues(Slot slot, Facet facet) {
        return getDirectType().getTemplateFacetValues(slot, facet);
    }

    public Collection getReachableSimpleInstances() {
        Collection roots = CollectionUtilities.createCollection(this);
        return getDefaultKnowledgeBase().getReachableSimpleInstances(roots);
    }

    public boolean hasDirectType(Cls cls) {
        return getDefaultKnowledgeBase().hasDirectType(this, cls);
    }

    public boolean hasType(Cls cls) {
        return getDefaultKnowledgeBase().hasType(this, cls);
    }

    public void removeInstanceListener(InstanceListener listener) {
        getDefaultKnowledgeBase().removeInstanceListener(this, listener);
    }

    public Instance setDirectType(Cls type) {
        return getDefaultKnowledgeBase().setDirectType(this, type);
    }

    public String toString() {
        StringBuffer buffer = new StringBuffer();
        buffer.append("Instance(");
        buffer.append(getName());
        buffer.append(" of ");
        buffer.append(getDirectType());
        buffer.append(")");
        return buffer.toString();
    }
}
