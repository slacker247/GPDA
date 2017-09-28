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

import edu.stanford.smi.protege.ui.*;
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

public class RemoteKBTabClsesPanel extends SelectableContainer {
    private static final String SUBCLASS_RELATIONSHIP   = "Subclass";
    private static final String REFERENCED_RELATIONSHIP = "Reference";

    private Project itsProject;
    private LabeledComponent itsLabeledComponent;
    private JComboBox itsRelationshipView;
    private AllowableAction itsCreateAction;
    private Action itsViewAction;
    private AllowableAction itsDeleteAction;
    private RemoteKBTabSubClassPane itsSubclassPane;
    private RelationshipPane itsRelationshipPane;

    private SwitchableActionListener itsRelationshipListener = new SwitchableActionListener() {
        public void changed(ActionEvent e) {
        	relationshipChanged();
        }
    };

    public RemoteKBTabClsTreeFinder getClsFinder() {
       return itsSubclassPane.getTreeFinder();
    }

    public RemoteKBTabClsesPanel(Project project) {
        itsProject = project;

        itsViewAction = getViewClsAction();
        itsCreateAction = getCreateClsAction();
        itsDeleteAction = getDeleteClsAction();
        createPanes();
        itsLabeledComponent = new LabeledComponent("Relationship", itsSubclassPane, true);

        itsLabeledComponent.addHeaderButton(itsViewAction);
        itsLabeledComponent.addHeaderButton(itsCreateAction);
        //itsLabeledComponent.addHeaderButton(new ReferencersAction(itsProject, this));
        itsLabeledComponent.addHeaderButton(new ReferencersAction(this));
        itsLabeledComponent.addHeaderButton(itsDeleteAction);

        itsLabeledComponent.setHeaderComponent(createRelationshipView());
		// c.setFooter(new TreeFinder(getTree(), "Find Class"));

        add(itsLabeledComponent);
		relationshipViewLoad();
        setSelectable(itsSubclassPane);
        updateDeleteActionState();
	}

    public void setDisplayParent(Cls cls) {
        if (isDisplayingSubclasses()) {
            itsSubclassPane.setDisplayParent(cls);
        }
    }

    public void setRenderer(DefaultRenderer renderer) {
        itsSubclassPane.setRenderer(renderer);
    }

    public Cls getDisplayParent() {
        return itsSubclassPane.getDisplayParent();
    }

    private boolean isDisplayingSubclasses() {
        return itsLabeledComponent.getCenterComponent() == itsSubclassPane;
    }

    private void createPanes() {
        itsSubclassPane = new RemoteKBTabSubClassPane(itsViewAction, getKnowledgeBase().getRootCls(),
                itsCreateAction, itsDeleteAction);
        itsRelationshipPane = new RelationshipPane(itsViewAction);
    }

    private Action getViewClsAction() {
    	return new ViewAction("View selected class", this) {
        	public void onView(Object o) {
            	showInstance((Cls) o);
            }
        };
    }

    private AllowableAction getCreateClsAction() {
    	return new CreateAction("Create subclass") {
        	public void onCreate() {
            	Collection parents = itsSubclassPane.getSelection();
                if (!parents.isEmpty()) {
                	Cls cls = getKnowledgeBase().createCls(null, parents);
                    itsSubclassPane.extendSelection(cls);
                }
            }
        };
    }

    private AllowableAction getDeleteClsAction() {
    	AllowableAction action = new DeleteAction("Delete selected class", this) {
        	public void onDelete(Object cls) {
                itsSubclassPane.removeSelection();
            	getKnowledgeBase().deleteCls((Cls)cls);
            }
            public void onSelectionChange() {
                updateDeleteActionState();
            }
        };
        action.setEnabled(true);
        return action;
    }

    private void updateDeleteActionState() {
        if (itsDeleteAction != null) {
            boolean isEditable = false;
            Frame frame = (Frame) CollectionUtilities.getFirstItem(getSelection());
            if (frame != null) {
                isEditable = frame.isEditable();
            }
            boolean isCorrectView = (itsRelationshipView.getSelectedItem() == SUBCLASS_RELATIONSHIP);
            itsDeleteAction.setAllowed(isEditable && isCorrectView);
        }
    }


    private JComponent createRelationshipView() {
    	itsRelationshipView = ComponentFactory.createComboBox();
        itsRelationshipView.addActionListener(itsRelationshipListener);
        itsRelationshipView.setRenderer(new FrameRenderer());
        return itsRelationshipView;
    }

	public void notifySelectionListeners() {
		super.notifySelectionListeners();
        if (itsRelationshipView != null) {
        	relationshipViewLoad();
        }
    }

    public Collection getSelection() {
        return ((Selectable)getDisplayedComponent()).getSelection();
    }

    private JComponent getDisplayedComponent() {
        return (JComponent) itsLabeledComponent.getCenterComponent();
    }

    private void enableButtons(boolean enable) {
        enableButton(itsCreateAction, enable);
        updateDeleteActionState();
    }

    private void enableButton(AllowableAction action, boolean enabled) {
        if (action != null) {
            action.setAllowed(enabled);
        }
    }

    private void relationshipViewLoad() {
    	itsRelationshipListener.disable();
        Object selection = itsRelationshipView.getSelectedItem();
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
    	itsRelationshipView.setModel(new DefaultComboBoxModel(choices.toArray()));
        itsRelationshipView.setSelectedItem(selection);
        itsRelationshipListener.enable();
    }

    private void relationshipChanged() {
    	itsRelationshipListener.disable();
        reload();
        // setSelection(selectedCls);
        itsRelationshipListener.enable();
    }

    private void reload() {
    	Frame selectedFrame = (Frame) CollectionUtilities.getFirstItem(getSelection());
    	Object selection = null;
        if (itsRelationshipView != null) {
        	selection = itsRelationshipView.getSelectedItem();
        }
        if (selection == null) {
        	selection = SUBCLASS_RELATIONSHIP;
        }
        if (selection.equals(SUBCLASS_RELATIONSHIP)) {
            if (selectedFrame instanceof Cls) {
                itsSubclassPane.setSelectedCls((Cls)selectedFrame);
            }
            loadComponent(itsSubclassPane);
            enableButtons(true);
        } else if (selection.equals(REFERENCED_RELATIONSHIP)) {
            itsRelationshipPane.load(selectedFrame, null);
            loadComponent(itsRelationshipPane);
            enableButtons(false);
        } else {
        	Slot slot = (Slot) selection;
            itsRelationshipPane.load(selectedFrame, slot);
            loadComponent(itsRelationshipPane);
            enableButtons(false);
        }
        notifySelectionListeners();
    }

    private void loadComponent(Selectable component) {
        itsLabeledComponent.setCenterComponent((JComponent)component);
        setSelectable(component);
    }

    private KnowledgeBase getKnowledgeBase() {
        return itsProject.getKnowledgeBase();
    }

    private void showInstance(Instance instance) {
        itsProject.show(instance);
    }

    public JComponent getDropComponent() {
        return itsSubclassPane.getDropComponent();
    }
}


class RelationshipPane extends SelectableContainer {
    private Frame itsFrame;
    private Slot itsSlot;

    public RelationshipPane(Action doubleClickAction) {
        SelectableTree tree = ComponentFactory.createSelectableTree(doubleClickAction);
        tree.setCellRenderer(new FrameRenderer());
        setSelectable(tree);
        add(new JScrollPane(tree), BorderLayout.CENTER);
    }

    public void load(Frame frame, Slot slot) {
        if (frame != itsFrame || slot != itsSlot) {
            itsFrame = frame;
            itsSlot = slot;
            rebuild();
        }
    }

    private void rebuild() {
        getTree().setRoot(new RemoteKBTabReferenceRoot(itsFrame.getKnowledgeBase(), itsFrame, itsSlot));
        getTree().setSelectionRow(0);
    }

    private SelectableTree getTree() {
        return (SelectableTree) getSelectable();
    }
}


