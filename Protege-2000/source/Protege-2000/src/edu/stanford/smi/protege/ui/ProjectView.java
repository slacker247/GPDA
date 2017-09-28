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
import java.beans.*;
import java.util.*;
import javax.swing.*;
import javax.swing.event.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.widget.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class ProjectView extends JComponent {
    private Project _project;
    private JComponent _tabbedPane;

    public ProjectView(Project project) {
        _project = project;
        setLayout(new BorderLayout());
        add(createTabbedPane());
    }

    private void addTab(JTabbedPane pane, WidgetDescriptor widgetDescriptor) {
        TabWidget widget = WidgetUtilities.createTabWidget(widgetDescriptor, _project);
        JComponent component = (JComponent) widget;
        Icon icon = widget.getIcon();
        String title = widget.getLabel();
        String help = widget.getShortDescription();
        pane.addTab(title, icon, component, help);
    }

    public boolean attemptClose() {
        boolean canClose = true;
        Iterator i = getTabs().iterator();
        while (canClose && i.hasNext()) {
            TabWidget tab = (TabWidget) i.next();
            canClose = tab.canClose();
        }
        if (canClose) {
            close();
        }
        return canClose;
    }

    public boolean attemptSave() {
        boolean canSave = true;
        Iterator i = getTabs().iterator();
        while (canSave && i.hasNext()) {
            TabWidget tab = (TabWidget) i.next();
            canSave = tab.canSave();
        }
        if (canSave) {
            save();
        }
        return canSave;
    }

    public void close() {
        Iterator i = getTabs().iterator();
        while (i.hasNext()) {
            TabWidget tab = (TabWidget) i.next();
            tab.close();
        }
    }

    private JComponent createTabbedPane() {
        JTabbedPane pane = ComponentFactory.createTabbedPane(true);
        Iterator i = _project.getTabWidgetDescriptors().iterator();
        while (i.hasNext()) {
            WidgetDescriptor d = (WidgetDescriptor) i.next();
            if (d.isVisible()) {
                String className = d.getWidgetClassName();
                addTab(pane, d);
            }
        }
        if (pane.getTabCount() > 0) {
            pane.setSelectedIndex(0);
        }
        _tabbedPane = pane;
        return pane;
    }

    public void dispose() {
    }

    public void finalize() {
        try {
            super.finalize();
            // System.out.println(getClass().getName() + " finalize");
        } catch (Throwable t) {
            t.printStackTrace();
        }
    }

    public Project getProject() {
        return _project;
    }

    public TabWidget getTab(String className) {
        TabWidget result = null;
        Component[] components = _tabbedPane.getComponents();
        for (int i = 0; i < components.length; ++i) {
            Component c = components[i];
            if (c.getClass().getName().equals(className)) {
                result = (TabWidget) c;
                break;
            }
        }
        return result;
    }

    private Collection getTabs() {
        return Arrays.asList(_tabbedPane.getComponents());
    }

    public void reload() {
        if (_tabbedPane != null) {
            ComponentUtilities.dispose(_tabbedPane);
        }
        removeAll();
        add(createTabbedPane());
        revalidate();
        repaint();
    }

    private void save() {
        Iterator i = getTabs().iterator();
        while (i.hasNext()) {
            TabWidget tab = (TabWidget) i.next();
            tab.save();
        }
    }

    public String toString() {
        return "ProjectView";
    }
}
