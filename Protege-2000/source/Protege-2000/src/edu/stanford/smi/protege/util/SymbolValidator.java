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

package edu.stanford.smi.protege.util;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class SymbolValidator implements Validator {

    private static boolean containsAny(String s, String test) {
        boolean contains = false;
        int len = test.length();
        for (int i = 0; !contains && i < len; ++i) {
            contains = s.indexOf(test.charAt(i)) >= 0;
        }
        return contains;
    }

    public String getErrorMessage() {
        return "Contains invalid characters";
    }

    public boolean isValid(Object o) {
        boolean isValid = true;
        if (o instanceof String) {
            String s = (String) o;
            if (containsAny(s, " \t\n\r()")) {
                isValid = false;
            }
        }
        return isValid;
    }
}
