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

package edu.stanford.smi.protege;

import junit.framework.*;
import edu.stanford.smi.protege.util.*;

/**
 * Automated test suite for all of Protege
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class _UnitTests extends DefaultTestSuite {

    public static void main(String[] args) {
        run(_UnitTests.class);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite("All Unit Tests");
        suite.addTest(edu.stanford.smi.protege._PackageTests.suite());
        suite.addTest(edu.stanford.smi.protege.action._PackageTests.suite());
        suite.addTest(edu.stanford.smi.protege.event._PackageTests.suite());
        suite.addTest(edu.stanford.smi.protege.model._PackageTests.suite());
        suite.addTest(edu.stanford.smi.protege.model.framedb._PackageTests.suite());
        suite.addTest(edu.stanford.smi.protege.resource._PackageTests.suite());
        suite.addTest(edu.stanford.smi.protege.storage.clips._PackageTests.suite());
        suite.addTest(edu.stanford.smi.protege.storage.jdbc._PackageTests.suite());
        suite.addTest(edu.stanford.smi.protege.ui._PackageTests.suite());
        suite.addTest(edu.stanford.smi.protege.util._PackageTests.suite());
        suite.addTest(edu.stanford.smi.protege.widget._PackageTests.suite());

        DefaultTestCase.setReloadingEnabled(true);
        return suite;
    }
}
