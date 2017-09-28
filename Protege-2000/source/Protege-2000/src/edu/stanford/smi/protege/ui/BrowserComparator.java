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

/** comparator for browser key text */
class BrowserComparator implements Comparator {

    public int compare(Object o1, Object o2) {
        String s1;
        String s2;
        if (o1 instanceof String) {
            s1 = (String) o1;
            s2 = ((Instance) o2).getBrowserText();
        } else {
            s1 = ((Instance) o1).getBrowserText();
            s2 = (String) o2;
        }
        return String.CASE_INSENSITIVE_ORDER.compare(s1, s2);
    }
}
