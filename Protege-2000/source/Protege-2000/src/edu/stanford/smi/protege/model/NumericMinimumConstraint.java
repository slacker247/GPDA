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
public class NumericMinimumConstraint extends AbstractFacetConstraint {

    public String getInvalidValuesText(Frame frame, Slot slot, Collection slotValues, Collection facetValues) {
        String result = null;
        Number n = (Number) CollectionUtilities.getFirstItem(facetValues);
        if (n != null) {
            double min = n.doubleValue();
            Iterator i = slotValues.iterator();
            while (i.hasNext()) {
                Object value = i.next();
                result = getInvalidValueText(min, value);
            }
        }
        return result;
    }

    private String getInvalidValueText(double min, Object value) {
        String result = null;
        if (value instanceof Number) {
            Number n = (Number) value;
            if (n.doubleValue() < min) {
                result = "The minimum value is " + min;
            }
        }
        return result;
    }

    public String getInvalidValueText(Frame frame, Slot slot, Object o, Collection facetValues) {
        String result = null;
        Number n = (Number) CollectionUtilities.getFirstItem(facetValues);
        if (n != null) {
            double min = n.doubleValue();
            result = getInvalidValueText(min, o);
        }
        return result;
    }
}
