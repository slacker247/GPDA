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

import edu.stanford.smi.protege.util.*;

/**
 * junit test case for DefaultCls
 *
 * @author Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class DefaultCls_Test extends DefaultTestCase {

    public DefaultCls_Test(String name) {
        super(name);
    }

    public static void main(String[] args) {
        run(DefaultCls_Test.class);
    }

    public void testHasDirectlyOverriddenTemplateFacet() {
        Cls standardSlot = getCls(Model.Cls.STANDARD_SLOT);
        Cls myMetaSlot = createSubCls(standardSlot);
        Slot slotWithFacet = createSingleValuedSlot(ValueType.STRING);
        Facet facet = createFacet();
        slotWithFacet.setAssociatedFacet(facet);
        myMetaSlot.addDirectTemplateSlot(slotWithFacet);
        Slot slot = (Slot) createInstance(myMetaSlot);
        slot.setOwnSlotValue(slotWithFacet, "foo");
        Cls cls = createCls();
        cls.addDirectTemplateSlot(slot);
        assertTrue("attached", !cls.hasDirectlyOverriddenTemplateFacet(slot, facet));
        cls.setTemplateFacetValue(slot, facet, "bar");
        assertTrue("overridden", cls.hasDirectlyOverriddenTemplateFacet(slot, facet));
        Cls subClass = createSubCls(cls);
        assertTrue("not overridden in subclass", !subClass.hasDirectlyOverriddenTemplateFacet(slot, facet));
        Cls subSubClass = createSubCls(subClass);
        assertTrue("not overridden in subsubclass", !subSubClass.hasDirectlyOverriddenTemplateFacet(slot, facet));
        subClass.setTemplateFacetValue(slot, facet, "baz");
        assertTrue("overridden in subclass", subClass.hasDirectlyOverriddenTemplateFacet(slot, facet));
        assertTrue("not overridden in subsubclass - 2", !subSubClass.hasDirectlyOverriddenTemplateFacet(slot, facet));
        subSubClass.setTemplateFacetValue(slot, facet, "bat");
        assertTrue("overridden in subsubclass", subSubClass.hasDirectlyOverriddenTemplateFacet(slot, facet));
    }

    public void testHasOverriddenTemplateFacet() {
        Facet facet = getFacet(Model.Facet.VALUE_TYPE);
        Cls cls = createCls();
        Slot slot = createMultiValuedSlot(ValueType.STRING);
        cls.addDirectTemplateSlot(slot);
        Cls subclass = createSubCls(cls);
        assertTrue("subclass not overridden", !subclass.hasOverriddenTemplateFacet(slot, facet));
        cls.setTemplateSlotValueType(slot, ValueType.INTEGER);
        assertTrue("class overridden", cls.hasOverriddenTemplateFacet(slot, facet));
        assertTrue("subclass overridden", subclass.hasOverriddenTemplateFacet(slot, facet));
    }
}
