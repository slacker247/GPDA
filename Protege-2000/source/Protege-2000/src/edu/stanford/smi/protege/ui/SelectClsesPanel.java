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
import java.util.*;
import javax.swing.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class SelectClsesPanel extends JComponent {
    private JTree _tree;

    public SelectClsesPanel(KnowledgeBase kb) {
        this(CollectionUtilities.createCollection(kb.getRootCls()));
    }

    public SelectClsesPanel(KnowledgeBase kb, DefaultRenderer renderer) {
        this(CollectionUtilities.createCollection(kb.getRootCls()));
        _tree.setCellRenderer(renderer);
    }

    public SelectClsesPanel(Collection clses) {
        _tree = ComponentFactory.createSelectableTree(null, new ParentChildRoot(clses));
        _tree.setCellRenderer(FrameRenderer.createInstance());
        if (clses.size() == 1) {
            _tree.expandRow(0);
            _tree.setSelectionRow(0);
        }
        KnowledgeBase kb = ((Cls) CollectionUtilities.getFirstItem(clses)).getKnowledgeBase();
        setLayout(new BorderLayout());
        add(new JScrollPane(_tree), BorderLayout.CENTER);
        add(new ClsTreeFinder(kb, _tree, "Find Class"), BorderLayout.SOUTH);
        setPreferredSize(new Dimension(300, 300));
    }

    public Collection getSelection() {
        return ComponentUtilities.getSelection(_tree);
    }
}
