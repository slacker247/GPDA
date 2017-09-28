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
import javax.swing.border.*;
import edu.stanford.smi.protege.event.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.widget.*;
import edu.stanford.smi.protege.util.*;

/**
 *  A panel that holds a design time class form
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class FormDisplay extends JComponent implements Disposable {
    private final static String INHERITED = "<inherit>";
    private Project _project;
    private JComponent _mainPanel;
    private JComboBox _browserKeyBox;
    private JComboBox _widgetSelectionBox;

    /* We need to listen for form changes because the user can do layout operations elsewhere (on the FormsPanel)
     * and we need to update the display with the new form widget.
     */
    private ProjectListener _projectListener = new ProjectAdapter() {
        public void formChanged(ProjectEvent event) {
            Cls cls = event.getCls();
            FormWidget widget = getCurrentWidget();
            if (widget != null && cls == widget.getCls() && !haveFocus()) {
                setWidgetCls(cls);
            }
        }
    };

    private SwitchableItemListener _descriptorChoiceListener = new SwitchableItemListener() {
        public void changed(ItemEvent event) {
            if (event.getStateChange() == ItemEvent.SELECTED) {
                _selectionListener.disable();
                String widgetClassName = (String) _widgetSelectionBox.getSelectedItem();
                if (widgetClassName == WidgetClassNameRenderer.NONE) {
                    widgetClassName = null;
                }
                getCurrentWidget().replaceSelectedWidget(widgetClassName);
                if (widgetClassName == null) {
                    updateWidgetsBox();
                }
                _selectionListener.enable();
            }
        }
    };

    private SwitchableSelectionListener _selectionListener = new SwitchableSelectionListener() {
        public void changed(SelectionEvent event) {
            updateWidgetsBox();
        }
    };

    private SwitchableActionListener _browserKeyListener = new SwitchableActionListener() {
        public void changed(ActionEvent event) {
            Object selection = _browserKeyBox.getSelectedItem();
            Slot slot;
            if (selection == INHERITED) {
                slot = null;
            } else {
                slot = (Slot) selection;
            }
            ClsWidget widget = getCurrentWidget();
            if (widget != null) {
                widget.getCls().setDirectBrowserSlot(slot);
            }
        }
    };

    class BrowserKeyRenderer extends DefaultRenderer {
        private Icon theDirectSlotIcon = Icons.getDirectSlotIcon();
        private Icon theInheritedSlotIcon = Icons.getInheritedSlotIcon();

        public void load(Object o) {
            if (o instanceof Slot) {
                Slot slot = (Slot) o;
                setMainText(slot.getBrowserText());
                setMainIcon(theDirectSlotIcon);
            } else if (o == INHERITED) {
                setMainText(INHERITED);
            } else {
                Assert.fail("unknown object");
            }
        }
    }

    public FormDisplay(Project project) {
        _project = project;
        _project.addProjectListener(_projectListener);
        setLayout(new BorderLayout());
        add(createNorthPanel(), BorderLayout.NORTH);
        add(createMainPanel(), BorderLayout.CENTER);
    }

    private void addCls(Cls cls) {
        ClsWidget widget = _project.getDesignTimeClsWidget(cls);
        JComponent c = (JComponent) widget;
        _mainPanel.add(new JScrollPane(c));
        c.revalidate();
        c.repaint();
        loadBrowserKeySlotsBox(widget);
        clearWidgetSelectionBox();
        widget.addSelectionListener(_selectionListener);
    }

    private void clearWidgetSelectionBox() {
        loadWidgetsBox(null);
    }

    private JComponent createBrowserKeySelection() {
        _browserKeyBox = ComponentFactory.createComboBox();
        _browserKeyBox.setRenderer(new BrowserKeyRenderer());
        _browserKeyBox.addActionListener(_browserKeyListener);
        return new LabeledComponent("Form Browser Key", _browserKeyBox);
    }

    private JComponent createMainPanel() {
        _mainPanel = new JPanel();
        _mainPanel.setLayout(new BorderLayout());
        return _mainPanel;
    }

    private JComponent createNorthPanel() {
        JComponent left = new JPanel();
        left.setLayout(new GridLayout(1, 2));
        left.add(createBrowserKeySelection());
        left.add(createWidgetSelection());
        JComponent right = new JPanel();
        right.setLayout(new BorderLayout());
        JComponent c = new JPanel();
        c.setLayout(new BorderLayout());
        c.add(left, BorderLayout.CENTER);
        c.add(right, BorderLayout.EAST);
        return c;
    }

    private JComponent createWidgetSelection() {
        _widgetSelectionBox = ComponentFactory.createComboBox();
        _widgetSelectionBox.setRenderer(new WidgetClassNameRenderer());
        _widgetSelectionBox.addItemListener(_descriptorChoiceListener);
        return new LabeledComponent("Selected Widget Type", _widgetSelectionBox);
    }

    public void dispose() {
        _project.removeProjectListener(_projectListener);
    }

    private FormWidget getCurrentWidget() {
        FormWidget widget;
        int count = _mainPanel.getComponentCount();
        if (count == 0) {
            widget = null;
        } else {
            JScrollPane pane = (JScrollPane) _mainPanel.getComponent(0);
            widget = (FormWidget) pane.getViewport().getView();
        }
        return widget;
    }

    private boolean haveFocus() {
        return SwingUtilities.findFocusOwner(this) != null;
    }

    private void loadBrowserKeySlotsBox(ClsWidget widget) {
        _browserKeyListener.disable();
        Cls cls = widget.getCls();
        List slots = new ArrayList(cls.getTemplateSlots());
        Collections.sort(slots, new FrameComparator());
        slots.add(0, INHERITED);
        _browserKeyBox.setModel(new DefaultComboBoxModel(slots.toArray()));
        Object selection = cls.getDirectBrowserSlot();
        if (selection == null) {
            selection = INHERITED;
        }
        _browserKeyBox.setSelectedItem(selection);
        _browserKeyListener.enable();
    }

    private void loadWidgetsBox(SlotWidget widget) {
        String widgetClassName = (widget == null) ? (String) null : widget.getClass().getName();
        _descriptorChoiceListener.disable();
        // HACK: avoid JDK 1.2 swing bug
        if (_widgetSelectionBox.getItemCount() > 0) {
            _widgetSelectionBox.removeAllItems();
        }
        if (widget != null) {
            Cls cls = widget.getCls();
            Slot slot = widget.getSlot();
            _widgetSelectionBox.addItem(WidgetClassNameRenderer.NONE);
            Iterator i = _project.getSuitableWidgetClassNames(cls, slot, null).iterator();
            while (i.hasNext()) {
                _widgetSelectionBox.addItem(i.next());
            }
            _widgetSelectionBox.setSelectedItem(widgetClassName);
        }
        _descriptorChoiceListener.enable();
    }

    private void removeCurrentWidget() {
        _mainPanel.removeAll();
        _browserKeyBox.setModel(new DefaultComboBoxModel());
        revalidate();
        repaint();
    }

    public void setWidgetCls(Cls cls) {
        removeCurrentWidget();
        if (cls != null) {
            addCls(cls);
        }
    }

    private void updateWidgetsBox() {
        SlotWidget w = (SlotWidget) CollectionUtilities.getFirstItem(getCurrentWidget().getSelection());
        loadWidgetsBox(w);
    }
}
