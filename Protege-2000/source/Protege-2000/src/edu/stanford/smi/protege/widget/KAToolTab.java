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

package edu.stanford.smi.protege.widget;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protege.util.*;

/**
 * Insert the type's description here.
 * Creation date: (8/17/2000 2:57:37 PM)
 * @author:
 */
public class KAToolTab extends AbstractTabWidget {
    private static final String TOP_LEVEL_INSTANCE_PROPERTY = "KATool.TOP_LEVEL_INSTANCE";

    public boolean configure() {
        KAToolTabConfigurationPanel panel = new KAToolTabConfigurationPanel(this);
        ModalDialog.showDialog(this, panel, "Configure KA Tool", ModalDialog.MODE_OK_CANCEL);
        return true;
    }

    private JComponent createClsPanel() {
        Instance topLevelInstance = getTopLevelInstance();
        Assert.assertNotNull("top level instance", topLevelInstance);
        InstanceDisplay d = new InstanceDisplay(getProject());
        d.setInstance(topLevelInstance);
        return d;
    }

    private Action createSelectClsAction() {
        return new AbstractAction("Press to select top-level instance") {
            public void actionPerformed(ActionEvent event) {
                Instance instance = DisplayUtilities.pickInstance(KAToolTab.this, getKnowledgeBase());
                if (instance != null) {
                    setTopLevelInstance(instance);
                }
            }
        };
    }

    /**
     * Insert the method's description here.
     * Creation date: (8/17/2000 5:43:40 PM)
     */
    private JComponent createSetupPanel() {
        JPanel outerPanel = new JPanel(new FlowLayout());
        JPanel innerPanel = new JPanel(new BorderLayout());
        innerPanel.add(ComponentFactory.createButton(createSelectClsAction()));
        outerPanel.add(innerPanel);
        return outerPanel;

    }

    public Instance getTopLevelInstance() {
        Instance result = null;
        String name = getPropertyList().getString(TOP_LEVEL_INSTANCE_PROPERTY);
        if (name != null) {
            result = getKnowledgeBase().getInstance(name);
            if (result == null) {
                Log.warning("Unable to find top level instance " + name, this, "getTopLevelInstance");
            }
        }
        return result;
    }

    /**
     * initialize method comment.
     */
    public void initialize() {
        if (getLabel() == null) {
            setLabel("Knowledge Acquisition");
        }
        setupUI();
    }

    public void setTopLevelInstance(Instance instance) {
        getPropertyList().setString(TOP_LEVEL_INSTANCE_PROPERTY, instance.getName());
        setupUI();
    }

    private void setupUI() {
        removeAll();
        setLayout(new BorderLayout());
        Instance topLevelInstance = getTopLevelInstance();
        JComponent mainForm;
        if (topLevelInstance == null) {
            mainForm = createSetupPanel();
        } else {
            mainForm = createClsPanel();
        }
        add(mainForm);
        revalidate();
        repaint();
    }
}
