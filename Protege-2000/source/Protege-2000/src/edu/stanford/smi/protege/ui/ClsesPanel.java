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

import java.awt.BorderLayout;
import java.awt.datatransfer.*;
import java.awt.dnd.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import javax.swing.tree.*;
import edu.stanford.smi.protege.action.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.protege.widget.*;

/**
 * The left upper display of the classes tab.  This holds the tree, the relationship-selection drop-down list,
 * and the class find component.
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class ClsesPanel extends SelectableContainer {
    protected final static String SUBCLASS_RELATIONSHIP = "Superclass";
    protected final static String REFERENCED_RELATIONSHIP = "References";

    protected Project _project;
    protected LabeledComponent _labeledComponent;
    protected JComboBox _relationshipView;
    protected AllowableAction _createAction;
    protected Action _viewAction;
    protected AllowableAction _deleteAction;
    protected SubclassPane _subclassPane;
    protected RelationshipPane _relationshipPane;

    protected SwitchableActionListener _relationshipListener = new SwitchableActionListener() {
        public void changed(ActionEvent e) {
            relationshipChanged();
        }
    };

    public ClsesPanel(Project project) {
        _project = project;

        _viewAction = getViewClsAction();
        _createAction = getCreateClsAction();
        _deleteAction = getDeleteClsAction();
        createPanes();
        _labeledComponent = new LabeledComponent("Relationship", _subclassPane, true);

        _labeledComponent.addHeaderButton(_viewAction);
        _labeledComponent.addHeaderButton(_createAction);
        _labeledComponent.addHeaderButton(new ReferencersAction(this));
        _labeledComponent.addHeaderButton(_deleteAction);

        _labeledComponent.setHeaderComponent(createRelationshipView());
        add(_labeledComponent);
        relationshipViewLoad();
        setSelectable(_subclassPane);
        updateDeleteActionState();
    }

    protected void createPanes() {
        _subclassPane = createSubclassPane(_viewAction, getKnowledgeBase().getRootCls(),
                _createAction, _deleteAction);
        _relationshipPane = createRelationshipPane(_viewAction);
    }

    protected RelationshipPane createRelationshipPane(Action viewAction) {
        return new RelationshipPane(viewAction);
    }

    protected JComponent createRelationshipView() {
        _relationshipView = ComponentFactory.createComboBox();
        _relationshipView.addActionListener(_relationshipListener);
        _relationshipView.setRenderer(FrameRenderer.createInstance());
        return _relationshipView;
    }

    protected SubclassPane createSubclassPane(Action viewAction, Cls root, Action createAction, Action action) {
        return new SubclassPane(viewAction, root, createAction, action);
    }

    protected void enableButton(AllowableAction action, boolean enabled) {
        if (action != null) {
            action.setAllowed(enabled);
        }
    }

    protected void enableButtons(boolean enable) {
        enableButton(_createAction, enable);
        updateDeleteActionState();
    }

    public JTree getClsesTree() {
        return (JTree) _subclassPane.getDropComponent();
    }

    protected AllowableAction getCreateClsAction() {
        return new CreateAction("Create subclass") {
            public void onCreate() {
                Collection parents = _subclassPane.getSelection();
                if (!parents.isEmpty()) {
                    Cls cls = getKnowledgeBase().createCls(null, parents);
                    _subclassPane.extendSelection(cls);
                }
            }
        };
    }

    protected AllowableAction getDeleteClsAction() {
        AllowableAction action = new DeleteInstancesAction("Delete selected class", this) {
            public void onAboutToDelete() {
                _subclassPane.removeSelection();
            }
            public void onSelectionChange() {
                updateDeleteActionState();
            }
        };
        action.setEnabled(true);
        return action;
    }

    protected JComponent getDisplayedComponent() {
        return (JComponent) _labeledComponent.getCenterComponent();
    }

    public Cls getDisplayParent() {
        return _subclassPane.getDisplayParent();
    }

    public JComponent getDropComponent() {
        return _subclassPane.getDropComponent();
    }

    protected KnowledgeBase getKnowledgeBase() {
        return _project.getKnowledgeBase();
    }

    /**
     *
     * @return edu.stanford.smi.protege.model.Project
     */
    public Project getProject() {
        return _project;
    }

    protected Selectable getRelationshipPane() {
        return _relationshipPane;
    }

    protected JComboBox getRelationshipView() {
        return _relationshipView;
    }

    public Collection getSelection() {
        return ((Selectable) getDisplayedComponent()).getSelection();
    }

    protected SubclassPane getSubclassPane() {
        return _subclassPane;
    }

    protected Action getViewClsAction() {
        return new ViewAction("View selected class", this) {
            public void onView(Object o) {
                showInstance((Cls) o);
            }
        };
    }

    protected boolean isDisplayingSubclasses() {
        return _labeledComponent.getCenterComponent() == _subclassPane;
    }

    protected void loadComponent(Selectable component) {
        _labeledComponent.setCenterComponent((JComponent) component);
        setSelectable(component);
    }

    public void notifySelectionListeners() {
        super.notifySelectionListeners();
        if (_relationshipView != null) {
            relationshipViewLoad();
        }
    }

    protected void relationshipChanged() {
        _relationshipListener.disable();
        reload();
        // setSelection(selectedCls);
        _relationshipListener.enable();
    }

    protected void relationshipViewLoad() {
        _relationshipListener.disable();
        Object selection = _relationshipView.getSelectedItem();
        if (selection == null) {
            selection = SUBCLASS_RELATIONSHIP;
        }
        Collection slots = new HashSet();
        Collection c = getSelection();
        if (c.size() == 1) {
            Frame selectedFrame = (Frame) c.iterator().next();
            if (selectedFrame instanceof Cls) {
                Cls selectedCls = (Cls) selectedFrame;
                Iterator i = selectedCls.getTemplateSlots().iterator();
                while (i.hasNext()) {
                    Slot slot = (Slot) i.next();
                    ValueType type = selectedCls.getTemplateSlotValueType(slot);
                    if (type == ValueType.INSTANCE || type == ValueType.CLS) {
                        slots.add(slot);
                    }
                }
            }
            Iterator j = selectedFrame.getOwnSlots().iterator();
            while (j.hasNext()) {
                Slot slot = (Slot) j.next();
                String slotName = slot.getName();
                String frameName = selectedFrame.getName();
                ValueType type = selectedFrame.getOwnSlotValueType(slot);
                if (!slot.isSystem() && (type == ValueType.INSTANCE || type == ValueType.CLS)) {
                    slots.add(slot);
                }
            }
        }
        List choices = new ArrayList(slots);
        Collections.sort(choices, new FrameComparator());
        choices.add(0, SUBCLASS_RELATIONSHIP);
        choices.add(1, REFERENCED_RELATIONSHIP);
        if (!choices.contains(selection)) {
            choices.add(selection);
        }
        _relationshipView.setModel(new DefaultComboBoxModel(choices.toArray()));
        _relationshipView.setSelectedItem(selection);
        _relationshipListener.enable();
    }

    /**
     * reload the tree as a result of a change in the displayed relationship
     */
    protected void reload() {
        Frame selectedFrame = (Frame) CollectionUtilities.getFirstItem(getSelection());
        Object selection = null;
        if (_relationshipView != null) {
            selection = _relationshipView.getSelectedItem();
        }
        if (selection == null) {
            selection = SUBCLASS_RELATIONSHIP;
        }
        if (selection.equals(SUBCLASS_RELATIONSHIP)) {
            if (selectedFrame instanceof Cls) {
                _subclassPane.setSelectedCls((Cls) selectedFrame);
            }
            loadComponent(_subclassPane);
            enableButtons(true);
        } else if (selection.equals(REFERENCED_RELATIONSHIP)) {
            _relationshipPane.load(selectedFrame, null);
            loadComponent(_relationshipPane);
            enableButtons(false);
        } else {
            Slot slot = (Slot) selection;
            _relationshipPane.load(selectedFrame, slot);
            loadComponent(_relationshipPane);
            enableButtons(false);
        }
        notifySelectionListeners();
    }

    /**
     * An obscure method to change the displayed parent of the selected class.
     * Imagine a selected class A with two parents "B" and "C".
     * Currently "A" is selected beneath "B".  Calling setDisplayParent("C") will
     * cause "A" to be displayed beneath "C".   This is the method used by the
     * component below the classes panel in the classes tab.
     */
    public void setDisplayParent(Cls cls) {
        if (isDisplayingSubclasses()) {
            _subclassPane.setDisplayParent(cls);
        }
    }

    public void setExpandedCls(Cls cls, boolean expanded) {
        if (isDisplayingSubclasses()) {
            _subclassPane.setExpandedCls(cls, expanded);
        } else {
        }
    }

    public void setFinderComponent(JComponent c) {
        _subclassPane.setFinderComponent(c);
    }

    public void setRenderer(DefaultRenderer renderer) {
        _subclassPane.setRenderer(renderer);
    }

    public void setSelectedCls(Cls cls) {
        if (isDisplayingSubclasses()) {
            _subclassPane.setSelectedCls(cls);
        } else {
        }
    }

    protected void showInstance(Instance instance) {
        _project.show(instance);
    }

    protected void updateDeleteActionState() {
        if (_deleteAction != null) {
            boolean isEditable = false;
            Frame frame = (Frame) CollectionUtilities.getFirstItem(getSelection());
            if (frame != null) {
                isEditable = frame.isEditable();
            }
            boolean isCorrectView = (_relationshipView.getSelectedItem() == SUBCLASS_RELATIONSHIP);
            _deleteAction.setAllowed(isEditable && isCorrectView);
        }
    }
}
