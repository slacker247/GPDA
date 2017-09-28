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
import javax.swing.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.util.*;

/**
 * Renderer for cardinality facet in the template slots pane
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class CardinalityFacetRenderer extends DefaultRenderer {
    private Facet _minCardinalityFacet;
    private Facet _maxCardinalityFacet;

    public CardinalityFacetRenderer(KnowledgeBase kb) {
        _minCardinalityFacet = kb.getFacet(Model.Facet.MINIMUM_CARDINALITY);
        _maxCardinalityFacet = kb.getFacet(Model.Facet.MAXIMUM_CARDINALITY);
    }

    private static Integer getFacetValue(Cls cls, Slot slot, Facet facet) {
        return (Integer) CollectionUtilities.getFirstItem(cls.getTemplateFacetValues(slot, facet));
    }

    public void load(Object o) {
        FrameSlotCombination combination = (FrameSlotCombination) o;
        Cls cls = (Cls) combination.getFrame();
        Slot slot = combination.getSlot();
        StringBuffer buffer = new StringBuffer();
        Integer min = getFacetValue(cls, slot, _minCardinalityFacet);
        if (min != null) {
            buffer.append("required ");
        }
        Integer max = getFacetValue(cls, slot, _maxCardinalityFacet);
        if (max == null) {
            buffer.append("multiple");
        } else {
            int i = max.intValue();
            if (i == 0) {
                buffer.append("none");
            } else if (i == 1) {
                buffer.append("single");
            } else {
                buffer.append("multiple");
            }
        }

        setMainText(buffer.toString());
        setGrayedText(!cls.isEditable());
    }
}
