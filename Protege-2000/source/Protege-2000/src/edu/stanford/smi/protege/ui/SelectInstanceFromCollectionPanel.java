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
import javax.swing.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class SelectInstanceFromCollectionPanel extends JComponent {
    private JList _list;

    public SelectInstanceFromCollectionPanel(Collection c, int initialSelection) {
        setLayout(new BorderLayout());
        _list = ComponentFactory.createList(getCloseAction());
        c = removeHidden(c);
        _list.setListData(c.toArray());
        FrameRenderer renderer = FrameRenderer.createInstance();
        renderer.setDisplayTrailingIcons(false);
        _list.setCellRenderer(renderer);
        if (initialSelection >= 0) {
            setSelection(initialSelection);
        }
        JScrollPane pane = ComponentFactory.createScrollPane(_list);
        add(pane);
        setPreferredSize(new Dimension(300, 150));
    }

    private Action getCloseAction() {
        return
            new AbstractAction() {
                public void actionPerformed(ActionEvent event) {
                    JComponent c = SelectInstanceFromCollectionPanel.this;
                    ModalDialog dialog = (ModalDialog) SwingUtilities.getRoot(c);
                    dialog.attemptClose(ModalDialog.OPTION_OK);
                }
            }
        ;
    }

    public Instance getSelection() {
        return (Instance) _list.getSelectedValue();
    }

    private Collection removeHidden(Collection instances) {
        Collection result;
        Project p = ((Instance)(CollectionUtilities.getFirstItem(instances))).getProject();
        if (p.getDisplayHiddenClasses()) {
            result = instances;
        } else {
            result = new ArrayList();
            Iterator i = instances.iterator();
            while (i.hasNext()) {
                Instance instance = (Instance) i.next();
                if (instance.isVisible()) {
                    result.add(instance);
                }
            }
        }
        return result;
    }

    private void setSelection(final int index) {
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                _list.setSelectedIndex(index);
                _list.ensureIndexIsVisible(index);
            }
        });
    }
}
