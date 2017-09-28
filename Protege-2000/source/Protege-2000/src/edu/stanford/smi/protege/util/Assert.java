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
 * A utility class for checking assertions.  Assertion failures throw
 * AssertionFailedError.
 *
 * Every method throws the error directly so that only one Assert class
 * method shows up on the stack trace.
 *
 * Modeled on the JUnit Assert class.
 *
 * @author    Ray Fergerson (fergerson@smi.stanford.edu)
 */
public class Assert {

    public static void assertEquals(String description, double x, double y, double delta) {
        if (Math.abs(x - y) > delta) {
            throw new AssertionFailedException(description, "assertEquals", new Double(x), new Double(y));
        }
    }

    public static void assertEquals(String description, int i, int j) {
        if (i != j) {
            throw new AssertionFailedException(description, "assertEquals", new Integer(i), new Integer(j));
        }
    }

    public static void assertEquals(String description, Object o1, Object o2) {
        if ((o1 == null) ? (o2 != null) : !o1.equals(o2)) {
            throw new AssertionFailedException(description, "assertEquals", o1, o2);
        }
    }

    public static void assertFalse(String description, boolean condition) {
        if (condition) {
            throw new AssertionFailedException(description, "assertFalse");
        }
    }

    public static void assertNotNull(String description, Object o) {
        if (o == null) {
            throw new AssertionFailedException(description, "assertNotNull", o);
        }
    }

    public static void assertNull(String description, Object o) {
        if (o != null) {
            throw new AssertionFailedException(description, "assertNull", o);
        }
    }

    public static void assertSame(String description, Object o1, Object o2) {
        if (o1 != o2) {
            throw new AssertionFailedException(description, "assertSame", o1, o2);
        }
    }

    public static void assertTrue(String description, boolean condition) {
        if (!condition) {
            throw new AssertionFailedException(description, "assertTrue");
        }
    }

    public static void fail(String description) {
        throw new AssertionFailedException(description, "fail");
    }
}
