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
import javax.swing.tree.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class LazyTreeModel extends DefaultTreeModel {
    private static class EmptyRoot extends LazyTreeRoot {
        EmptyRoot() {
            super(Collections.EMPTY_LIST);
        }

        public LazyTreeNode createNode(Object o) {
            Assert.fail("no override");
            return null;
        }

        public Comparator getComparator() {
            return null;
        }
    }

    public LazyTreeModel() {
        this(null);
    }

    public LazyTreeModel(LazyTreeRoot root) {
        super(root == null ? new EmptyRoot() : root);
        getLazyTreeRoot().setModel(this);
    }

    public void dispose() {
        removeRoot();
    }

    private LazyTreeRoot getLazyTreeRoot() {
        return (LazyTreeRoot) getRoot();
    }

    private void removeRoot() {
        getLazyTreeRoot().dispose();
    }

    public void setRoot(LazyTreeRoot root) {
        if (root == null) {
            root = new EmptyRoot();
        }
        removeRoot();
        super.setRoot(root);
        root.setModel(this);
    }
}
