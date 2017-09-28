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

package edu.stanford.smi.RemoteKBTab.toolbox;

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.util.List;
import javax.swing.*;
import javax.swing.tree.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.protege.ui.*;

public class RemoteKBTabClsTreeFinder extends RemoteKBTabFinder {

    private JTree itsTree;
    private KnowledgeBase itsKnowledgeBase;

    public RemoteKBTabClsTreeFinder(KnowledgeBase kb, JTree tree, String description) {
        super(description);
        itsKnowledgeBase = kb;
        itsTree = tree;
    }

    protected int getBestMatch(List matches, String text) {
        return 0;
    }

    protected List getMatches(String text, int maxMatches) {
        Cls kbRoot = itsKnowledgeBase.getRootCls();
        ArrayList matches = new ArrayList(itsKnowledgeBase.getClsNameMatches(text, maxMatches));
        LazyTreeRoot root = (LazyTreeRoot) itsTree.getModel().getRoot();
        Collection rootNodes = (Collection) root.getUserObject();
        if (rootNodes.size() != 1 || CollectionUtilities.getFirstItem(rootNodes) != kbRoot) {
            Log.trace("removing bad matches", this, "getMatches");
            Iterator i = matches.iterator();
            while (i.hasNext()) {
                Cls cls = (Cls) i.next();
                boolean isValid = false;
                Iterator j = rootNodes.iterator();
                while (j.hasNext()) {
                    Cls rootCls = (Cls) j.next();
                    if (cls.hasSuperclass(rootCls)) {
                        isValid = true;
                        break;
                    }
                }
                if (!isValid) {
                    i.remove();
                }
            }
        }
        Collections.sort(matches, new FrameComparator());
        return matches;
    }

    protected void select(Object o) {
        Cls cls = (Cls) o;
        WaitCursor cursor = new WaitCursor(this);
        ArrayList clses = new ArrayList();
        getPathToRoot(cls, clses);
        Collections.reverse(clses);
        ComponentUtilities.setSelectedObjectPath(itsTree, clses);
        cursor.hide();
    }

    private void getPathToRoot(Cls cls, Collection clses) {
        Collection rootClses = (Collection) ((LazyTreeNode)itsTree.getModel().getRoot()).getUserObject();
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

    public String toString() {
        return "RemoteKBTabClsTreeFinder";
    }
}

