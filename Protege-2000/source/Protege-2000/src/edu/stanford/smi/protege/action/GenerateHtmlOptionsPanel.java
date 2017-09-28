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

package edu.stanford.smi.protege.action;

import java.awt.*;
import java.util.*;
import javax.swing.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.model.Frame;
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protege.util.*;

/**
 * Description of Type
 *
 * @author Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class GenerateHtmlOptionsPanel extends JComponent {
    private KnowledgeBase kb;
    private FileField directoryComponent;
    private SelectableList rootClsesComponent;
    private JCheckBox _includeInstancesCheckBox;
    private static Collection savedRootNames = new ArrayList();

    public GenerateHtmlOptionsPanel(KnowledgeBase kb) {
        this.kb = kb;
        setLayout(new BorderLayout(10, 10));
        add(createDirectoryPane(), BorderLayout.NORTH);
        add(createRootClsesPane(), BorderLayout.CENTER);
        add(createIncludeInstancesPane(), BorderLayout.SOUTH);
        restoreRoots();
    }

    private Action createAddClsAction() {
        return new AddAction("Add root class") {
            public void onAdd() {
                Collection c = DisplayUtilities.pickClses(GenerateHtmlOptionsPanel.this, kb);
                if (!c.isEmpty()) {
                    ComponentUtilities.removeListValues(rootClsesComponent, kb.getRootClses());
                    ComponentUtilities.addListValues(rootClsesComponent, c);
                }
            }
        };
    }

    private JComponent createDirectoryPane() {
        directoryComponent = new FileField("Output Directory", null, null, "Output Directory");
        return directoryComponent;
    }

    private Action createRemoveClsAction() {
        return new RemoveAction("Remove selected classes", rootClsesComponent) {
            public void onRemove() {
                ComponentUtilities.removeListValues(rootClsesComponent, rootClsesComponent.getSelection());
            }
        };
    }

    private JComponent createRootClsesPane() {
        rootClsesComponent = ComponentFactory.createSelectableList(null);
        rootClsesComponent.setCellRenderer(FrameRenderer.createInstance());
        LabeledComponent c = new LabeledComponent("Root Classes", new JScrollPane(rootClsesComponent));
        c.addHeaderButton(createAddClsAction());
        c.addHeaderButton(createRemoveClsAction());
        return c;
    }

    private JComponent createIncludeInstancesPane() {
        _includeInstancesCheckBox = ComponentFactory.createCheckBox("Include Instances");
        return _includeInstancesCheckBox;
    }

    public String getOutputDirectory() {
        return directoryComponent.getPath();
    }

    public Collection getRootClses() {
        Collection c = ComponentUtilities.getListValues(rootClsesComponent);
        saveRoots(c);
        return c;
    }

    public boolean getIncludeInstances() {
        return _includeInstancesCheckBox.isSelected();
    }

    private void restoreRoots() {
        Collection roots = new ArrayList();
        if (savedRootNames != null) {
            Iterator i = savedRootNames.iterator();
            while (i.hasNext()) {
                String name = (String) i.next();
                Frame frame = kb.getFrame(name);
                if (frame instanceof Cls) {
                    roots.add(frame);
                }
            }
        }
        if (roots.isEmpty()) {
            roots.addAll(kb.getRootClses());
        }
        ComponentUtilities.setListValues(rootClsesComponent, roots);
    }

    /* save the names rather than the frames in order to allow the frames
     * to be garbage collected.
     */
    private void saveRoots(Collection clses) {
        savedRootNames.clear();
        Iterator i = clses.iterator();
        while (i.hasNext()) {
            Cls cls = (Cls) i.next();
            savedRootNames.add(cls.getName());
        }
    }
}
