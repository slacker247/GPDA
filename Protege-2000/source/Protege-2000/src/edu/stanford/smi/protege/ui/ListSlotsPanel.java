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
import edu.stanford.smi.protege.action.*;
import edu.stanford.smi.protege.event.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.model.Frame;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class ListSlotsPanel extends SelectableContainer {
    private Comparator _comparator = new FrameComparator();
    private Project _project;
    private KnowledgeBase _knowledgeBase;
    private JList _list;

    private KnowledgeBaseListener _listener =
        new KnowledgeBaseAdapter() {
            public void frameNameChanged(KnowledgeBaseEvent event) {
                Frame frame = event.getFrame();
                if (frame instanceof Slot) {
                    SystemUtilities.debugBreak();
                    // setNotificationsEnabled(false);
                    ComponentUtilities.reposition(_list, frame, _comparator);
                    // setNotificationsEnabled(true);
                    repaint();
                }
            }

            public void slotCreated(KnowledgeBaseEvent event) {
                ComponentUtilities.addListValue(_list, event.getSlot(), _comparator);
            }

            public void slotDeleted(KnowledgeBaseEvent event) {
                // Log.enter(this, "slotDeleted");
                ComponentUtilities.removeListValue(_list, event.getSlot());
            }
        }
    ;

    public ListSlotsPanel(Project p) {
        _project = p;
        _knowledgeBase = p.getKnowledgeBase();
        _knowledgeBase.addKnowledgeBaseListener(_listener);
        createUI();
        loadList();
        _list.setSelectedIndex(0);
    }

    private JList createList(Action action) {
        SelectableList list = ComponentFactory.createSelectableList(action, false);
        list.setCellRenderer(FrameRenderer.createInstance());
        setSelectable(list);
        return list;
    }

    private void createUI() {
        setLayout(new BorderLayout());
        Action viewAction = getViewAction();
        _list = createList(viewAction);
        LabeledComponent c = new LabeledComponent("Slots", ComponentFactory.createScrollPane(_list));
        c.addHeaderButton(viewAction);
        c.addHeaderButton(getCreateAction());
        c.addHeaderButton(getReferencersAction());
        c.addHeaderButton(getDeleteAction());
        add(c, BorderLayout.CENTER);
        add(new ListFinder(_list, "Find Slot"), BorderLayout.SOUTH);
    }

    public void dispose() {
        _knowledgeBase.removeKnowledgeBaseListener(_listener);
    }

    private Action getCreateAction() {
        return
            new CreateAction("Create New Slot") {
                public void onCreate() {
                    Slot slot = _knowledgeBase.createSlot(null);
                    _list.setSelectedValue(slot, true);
                }
            }
        ;
    }

    private Action getDeleteAction() {
        return new DeleteAction("Delete Selected Slots", this) {
            public void onDelete(Object o) {
                _knowledgeBase.deleteSlot((Slot) o);
            }

            public void onSelectionChange() {
                Slot slot = (Slot) CollectionUtilities.getFirstItem(this.getSelection());
                if (slot != null) {
                    setAllowed(slot.isEditable());
                }
            }
        };
    }

    private Action getReferencersAction() {
        return new ReferencersAction(this);
    }

    private Action getViewAction() {
        return new ViewAction("View Selected Slots", this) {
            public void onView(Object o) {
                _project.show((Slot) o);
            }
        };
    }

    private void loadList() {
        ArrayList slots = new ArrayList(_knowledgeBase.getSlots());
        Collections.sort(slots, _comparator);
        ComponentUtilities.setListValues(_list, slots);
    }
}
