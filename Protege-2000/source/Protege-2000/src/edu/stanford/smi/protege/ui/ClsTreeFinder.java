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


import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.util.List;
import javax.swing.*;
import javax.swing.tree.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class ClsTreeFinder extends Finder {
    private JTree _tree;
    private KnowledgeBase _knowledgeBase;

    public ClsTreeFinder(KnowledgeBase kb, JTree tree, String description) {
        super(description);
        _knowledgeBase = kb;
        _tree = tree;
    }

    protected int getBestMatch(List matches, String text) {
        int result = Collections.binarySearch(matches, text, new BrowserComparator());
        if (result < 0) {
            int index = -(result + 1);
            if (index < matches.size()) {
                Instance instance = (Instance) matches.get(index);
                String browserText = instance.getBrowserText().toLowerCase();
                if (browserText.startsWith(text.toLowerCase())) {
                    result = index;
                }
            }
        }
        return result;
    }

    protected List getMatches(String text, int maxMatches) {
        Cls kbRoot = _knowledgeBase.getRootCls();
        ArrayList matches = new ArrayList(_knowledgeBase.getClsNameMatches(text, maxMatches));
        LazyTreeRoot root = (LazyTreeRoot) _tree.getModel().getRoot();
        Set rootNodes = new HashSet((Collection) root.getUserObject());
        if (rootNodes.size() != 1 || CollectionUtilities.getFirstItem(rootNodes) != kbRoot) {
            // Log.trace("removing bad matches", this, "getMatches");
            Iterator i = matches.iterator();
            while (i.hasNext()) {
                Cls cls = (Cls) i.next();
                boolean isValid = rootNodes.contains(cls);
                if (!isValid) {
                    Collection superClasses = cls.getSuperclasses();
                    isValid = superClasses.removeAll(rootNodes);
                }
                if (!isValid) {
                    i.remove();
                }
            }
        }
        Collections.sort(matches, new FrameComparator());
        return matches;
    }

    private void getPathToRoot(Cls cls, Collection clses) {
        Collection rootClses = (Collection) ((LazyTreeNode) _tree.getModel().getRoot()).getUserObject();
        clses.add(cls);
        Collection superclasses = cls.getDirectSuperclasses();
        Cls parent = (Cls) CollectionUtilities.getFirstItem(superclasses);
        if (parent == null) {
            Log.error("no parents", this, "getPathToRoot", cls);
        } else if (rootClses.contains(parent)) {
            clses.add(parent);
        } else {
            getPathToRoot(parent, clses);
        }
    }

    protected void select(Object o) {
        Cls cls = (Cls) o;

        WaitCursor cursor = new WaitCursor(this);
        try {
            ArrayList clses = new ArrayList();
            getPathToRoot(cls, clses);
            Collections.reverse(clses);
            ComponentUtilities.setSelectedObjectPath(_tree, clses);
        } finally {
            cursor.hide();
        }
    }

    public String toString() {
        return "ClsTreeFinder";
    }
}
