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
import edu.stanford.smi.protege.model.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class CollectionUtilities {
    public static final ArrayList EMPTY_ARRAY_LIST = new ArrayList();

    public static boolean containSameItems(Collection c1, Collection c2) {
        Set s1 = (c1 instanceof Set) ? (Set) c1 : new HashSet(c1);
        Set s2 = (c2 instanceof Set) ? (Set) c2 : new HashSet(c2);
        return s1.equals(s2);
    }

    /**
     * Returns true if there is any element that is common to both collections.
     */
    public static boolean containsAny(Collection c1, Collection c2) {
        // A better implementation here would be to use sets
        Collection smallCollection;
        Collection largeCollection;
        if (c1.size() < c2.size()) {
            smallCollection = c1;
            largeCollection = c2;
        } else {
            smallCollection = c2;
            largeCollection = c1;
        }
        boolean intersect = false;
        Iterator i = smallCollection.iterator();
        while (i.hasNext()) {
            if (largeCollection.contains(i.next())) {
                intersect = true;
                break;
            }
        }
        return intersect;
    }

    public static Collection createCollection(Object o) {
        return createList(o);
    }

    public static List createList(Object o) {
        List c;
        if (o == null) {
            c = Collections.EMPTY_LIST;
        } else {
            c = new ArrayList(1);
            c.add(o);
        }
        return c;
    }

    public static Map createSmallMap() {
        return new HashMap(11);
    }

    public static boolean equals(Collection c1, Collection c2) {
        boolean equals;
        if (c1 == null) {
            equals = (c2 == null);
        } else if (c2 == null) {
            equals = false;
        } else if (c1.size() == c2.size()) {
            equals = true;
            Iterator iterC1 = c1.iterator();
            Iterator iterC2 = c2.iterator();
            while (equals && iterC1.hasNext() && iterC2.hasNext()) {
                Object o1 = iterC1.next();
                Object o2 = iterC2.next();
                equals = o1.equals(o2);
            }
        } else {
            equals = false;
        }
        return equals;
    }

    /**
     * Returns the first item in the collection or null if the collection is empty
     */
    public static Object getFirstItem(Collection c) {
        Object o;
        if (c == null || c.isEmpty()) {
            o = null;
        } else if (c instanceof List) {
            o = ((List) c).get(0);
        } else {
            o = c.iterator().next();
        }
        return o;
    }

    /**
     * Returns the first item in the collection.  Throws an AssertionFailureError
     * if there is not exactly 1 item in the collection.  Use getFirstItem() to
     * get the first item of a collection if it is possible that there is either
     * 0 items or more than 1 item in the collection.
     */
    public static Object getSoleItem(Collection c) {
        Assert.assertEquals("size", c.size(), 1);
        return getFirstItem(c);
    }

    public static String toString(Collection collection) {
        StringBuffer buffer = new StringBuffer();
        if (collection != null) {
            boolean first = true;
            Iterator i = collection.iterator();
            while (i.hasNext()) {
                if (first) {
                    first = false;
                } else {
                    buffer.append(", ");
                }
                Object o = i.next();
                if (o instanceof Frame) {
                    o = ((Frame) o).getBrowserText();
                }
                buffer.append(o);
            }
        }
        return buffer.toString();
    }
}
