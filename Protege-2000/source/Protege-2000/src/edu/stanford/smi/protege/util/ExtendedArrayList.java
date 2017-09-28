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


import java.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class ExtendedArrayList extends ArrayList implements ExtendedList {
    private List _unmodifiableList;

    public ExtendedArrayList(int i) {
        super(i);
    }

    public Object getFirst() {
        return (isEmpty()) ? (Object) null : get(0);
    }

    public List getUnmodifiableList() {
        if (_unmodifiableList == null) {
            _unmodifiableList = Collections.unmodifiableList(this);
        }
        return _unmodifiableList;
    }

    public void replace(int index, Object to) {
        set(index, to);
    }

    public void replace(Object from, Object to) {
        int index = indexOf(from);
        set(index, to);
    }
}
