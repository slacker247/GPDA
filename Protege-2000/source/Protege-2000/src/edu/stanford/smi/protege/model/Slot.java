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

/**
 *  The description of a constraint on the value of a slot-binding.
 *
 * @author Ray Fergerson <fergerson@smi.stanford.edu>
 */
public interface Slot extends Instance {

    void addDirectSuperslot(Slot slot);

    void addSlotListener(SlotListener listener);

    Collection getAllowedClses();

    Collection getAllowedParents();

    Collection getAllowedValues();

    boolean getAllowsMultipleValues();

    Facet getAssociatedFacet();

    Collection getDefaultValues();

    int getDirectSubslotCount();

    Collection getDirectSubslots();

    int getDirectSuperslotCount();

    Collection getDirectSuperslots();

    Collection getDocumentation();

    Slot getInverseSlot();

    int getMaximumCardinality();

    Number getMaximumValue();

    int getMinimumCardinality();

    Number getMinimumValue();

    Collection getSubslots();

    Collection getSuperslots();

    Collection getTemplateSlotClses();

    Collection getValues();

    ValueType getValueType();

    boolean hasValueAtSomeFrame();

    void removeDirectSuperslot(Slot slot);

    void removeSlotListener(SlotListener listener);

    void setAllowedClses(Collection clses);

    void setAllowedParents(Collection parents);

    void setAllowedValues(Collection values);

    void setAllowsMultipleValues(boolean b);

    void setAssociatedFacet(Facet facet);

    void setDefaultValues(Collection values);

    void setDirectTypeOfSubslots(Cls cls);

    void setDocumentation(String doc);

    void setInverseSlot(Slot slot);

    void setMaximumCardinality(int max);

    void setMaximumValue(Number n);

    void setMinimumCardinality(int min);

    void setMinimumValue(Number n);

    void setValues(Collection values);

    void setValueType(ValueType type);
}
