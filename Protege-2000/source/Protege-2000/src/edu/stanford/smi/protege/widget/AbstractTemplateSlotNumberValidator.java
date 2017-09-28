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

package edu.stanford.smi.protege.widget;

import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Bill Grosso <grosso@smi.stanford.edu>
 */
public abstract class AbstractTemplateSlotNumberValidator implements NumberValidator {
    public final static int VALID_NUMBER = 0;
    public final static int NUMBER_OUT_OF_BOUNDS = 1;
    public final static int FORMAT_ERROR = 2;

    private Cls _cls;
    private Slot _slot;

    public AbstractTemplateSlotNumberValidator(Cls cls, Slot slot) {
        _cls = cls;
        _slot = slot;
    }

    private String getErrorRange(Number min, Number max) {
        String lowerBound = (null == min) ? "-infinity" : min.toString();
        String upperBound = (null == max) ? "+infinity" : max.toString();
        return "[" + lowerBound + ", " + upperBound + "]";
    }

    public boolean isValid(Object o) {
        return validateString(o.toString()) == null;
    }

    public String validateString(String stringToCheck) {
        Number number;
        try {
            number = convertToNumber(stringToCheck);
        } catch (NumberFormatException e) {
            return getErrorMessage();
        }
        Number max = _cls.getTemplateSlotMaximumValue(_slot);
        Number min = _cls.getTemplateSlotMinimumValue(_slot);
        if (null != max) {
            double maxAsDouble = max.doubleValue();
            if (maxAsDouble < number.doubleValue()) {
                return "Number too large. It must be in the range " + getErrorRange(min, max);
            }
        }
        if (null != min) {
            double minAsDouble = min.doubleValue();
            if (minAsDouble > number.doubleValue()) {
                return "Number too small. It must be in the range " + getErrorRange(min, max);
            }
        }
        return null;
    }
}
