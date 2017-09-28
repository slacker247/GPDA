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
 * JUnit test for Log
 *
 * @author Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class Log_Test extends DefaultTestCase {

    /**
     * Log_Test constructor comment.
     * @param name java.lang.String
     */
    public Log_Test(String name) {
        super(name);
    }

    public static void main(String[] args) {
        run(Log_Test.class);
    }

    public void setUp() {
        assertEquals(0, Log.getTraceCount());
    }

    public void testErrorCount() {
        Log.error("test error", this, "testErrorCount");
        assertEquals(1, Log.getErrorCount());
        Log.resetCounts();
        assertEquals(0, Log.getErrorCount());
    }

    public void testTraceCount() {
        Log.trace("test trace", this, "testTraceCount");
        Log.warning("test warning", this, "testWarningCount");
        Log.error("test error", this, "testWarningCount");
        assertEquals(3, Log.getTraceCount());
        Log.resetCounts();
        assertEquals(0, Log.getTraceCount());
    }

    public void testWarningCount() {
        Log.warning("test warning", this, "testWarningCount");
        Log.error("test error", this, "testWarningCount");
        assertEquals(2, Log.getWarningCount());
        Log.resetCounts();
        assertEquals(0, Log.getWarningCount());
    }
}
