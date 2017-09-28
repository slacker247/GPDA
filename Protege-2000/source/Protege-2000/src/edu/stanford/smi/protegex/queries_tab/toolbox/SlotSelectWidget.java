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

package edu.stanford.smi.protegex.queries_tab.toolbox;

import javax.swing.*;
import java.io.*;
import java.util.*;
import java.awt.*;
import java.awt.event.*;
import java.util.List;
import javax.swing.event.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protegex.queries_tab.*;
import edu.stanford.smi.protege.event.*;

public class SlotSelectWidget extends AbstractListValueWidget{

  private Slot itsSlot;

    /** Constructor. */
    public SlotSelectWidget(SearchWidget widget) {
        super(widget);
        constraints = ConstraintsModel.getClsConstraints();
        this.label = "slot";
        createComponents(null);
    }

    /** Constructor with label, slot, and actions. */
    public SlotSelectWidget(SearchWidget widget, String label, String slot) {
        super(widget);
        setSlotName(slot);
        this.label = label;
    }

    protected void addActions() {

        LabeledComponent c = new LabeledComponent("Slot", itsList);

        itsSelectAction = getSelectClsAction();
        itsViewAction = getViewClsAction();
        itsRemoveAction = getRemoveClsAction();

        c.addHeaderButton(itsViewAction);
        c.addHeaderButton(itsSelectAction);
        c.addHeaderButton(itsRemoveAction);

        itsComp = c;
    }

    // This is used to clear the display only.
    public void clearList() {
        ArrayList displayInstances = new ArrayList(CollectionUtilities.createCollection(null));
        Collections.sort(displayInstances, theComparator);
        ComponentUtilities.setListValues(itsList, displayInstances);
    }

    /** Create components in this relation display. */
    protected void createListener() {
        itsKBListener = new KnowledgeBaseAdapter() {
            public void slotDeleted(KnowledgeBaseEvent event) {
                if (itsSlot == null)
                    return;
                edu.stanford.smi.protege.model.Frame frame = event.getFrame();
                if (frame instanceof Slot) {
                    String name = ((Slot) frame).getName();
                    if (frame.getFrameID().getValue() == itsSlot.getFrameID().getValue()) {
                        removeSlot();
                        setDisplayName(name);
                        updateSlots((Slot) frame);
                        itsTab.enableSearch();

                    }
                }
            }
        };
        itsWidget.getKB().addKnowledgeBaseListener(itsKBListener);
    }

    /** Return the relation display component. */
    public JComponent getComponent() {
        return itsComp;
    }

    /** Get the data from the text area and return it as a string array. */
    public Object[] getData() {
        String[] itsData = new String[1];
        if (itsSlot == null)
            return null;
        itsData[0] = itsSlot.getName();
        return itsData;
    }

    private Action getRemoveClsAction() {
        return new AbstractAction("Remove Slot", Icons.getRemoveIcon()) {
            public void actionPerformed(ActionEvent event) {
                removeSlot();
                itsTab.enableSearch();
            }
        };
    }

    /** Get Select(+) Instance Action. */
    private Action getSelectClsAction() {
        return new AbstractAction("Select Slot", Icons.getAddIcon()) {
            public void actionPerformed(ActionEvent event) {
                Slot tmpSlot = itsSlot;
                Collection itsSlots = itsWidget.getSlotsModel().getSlots();
                String mSlot = null;
                List slotsList = new ArrayList(itsSlots);
                Collection slots = DisplayUtilities.pickInstancesFromCollection(itsComp, slotsList, "Select Slot");

                if (slots == null) {
                    mSlot = null;
                } else {

                    Iterator j = slots.iterator();
                    while (j.hasNext()) {
                        itsSlot = (Slot) j.next();
                        mSlot = itsSlot.getName();
                        break;
                    }
                }

                if (mSlot != null) {
                    if (tmpSlot != null && tmpSlot.getName().equalsIgnoreCase(itsSlot.getName()))
                        itsSlot = tmpSlot;
                    else
                        setDisplayedSlot(itsSlot);
                }

                if (itsSlot != null)
                    setActionsEnabled(true);
                else
                    setActionsEnabled(false);

                itsTab.enableSearch();
            }
        };
    }

    /** Get the selected string in text area and return them as a string array. */
    public String[] getSelectedItems() {
        String[] text = new String[1];
        text[0] = itsSlot.getName();
        return text;
    }

    /** Get the selected object which is a slot for slot. */
    public Object getSelectedObject() {
        return (Slot) itsSlot;
    }

    /** Get slot return the current slot. */
    public Slot getSlot() {
        if (slotName == null)
            return null;
        Slot slot = selection.getProject().getKnowledgeBase().getSlot(slotName);
        return slot;
    }

    public String getSlotName() {
        if (itsSlot != null)
            return itsSlot.getName();
        else
            return null;
    }

    /** View the instance. */
    private Action getViewClsAction() {
        return new AbstractAction("View Slot", Icons.getViewIcon()) {
            public void actionPerformed(ActionEvent event) {
                viewObject();
            }
        };
    }

    /** Return the embedded widget in the relation display, the JTextArea. */
    public JComponent getWidget() {
        return itsComp;
    }

    /** Set the slot not a single value slot. */
    public boolean isSlotSingleValued() {
        return true;
    }

    private void removeDisplayedInstance() {
        replaceSlot(null);
        updateList();
        valueChanged();
    }

    private void removeSlot() {
        removeDisplayedInstance();
        setActionsEnabled(false);
    }

    private void replaceSlot(Slot s) {
        itsSlot = s;
    }

    public Collection search(Collection instances, Slot slot, String operation, Object obj) {
        return null;
    }

    public void setActionsEnabled(boolean b) {
        if (isViewEnabled)
            itsViewAction.setEnabled(b);
        else
            itsViewAction.setEnabled(false);
        itsRemoveAction.setEnabled(b);
    }

    /** Overwrite the setCls to setup the selected cls */
    public void setCls(Cls cls) {
        selection = cls;
    }

    /** Set the specified string to the text area. */
    public void setData(Object[] data) {
        itsList.clearSelection();
        itsWidget.setReady(true);

        setActionsEnabled(false);

        itsSlot = null;
        if (data == null || (String) data[0] == null) {
            return;
        }
        if (selection == null)
            return;
        slotName = (String) data[0];
        instanceSlot = getSlot();
        if (instanceSlot == null)
            return;

    }

    /** Set the input instance as the selected classs */
    public void setDisplayedSlot(Slot s) {
        replaceSlot(s);
        updateList();
        valueChanged();
    }

    public void setSelectedObject(Object obj) {
        setDisplayedSlot((Slot) obj);
    }

    public void setViewEnabled(boolean b) {
        isViewEnabled = b;
        itsViewAction.setEnabled(isViewEnabled);
    }

    public void showInstance(Instance instance) {
        if (instance != null && selection != null)
            selection.getProject().show(instance);
    }

    protected void updateList() {

        ArrayList displaySlots = new ArrayList(CollectionUtilities.createCollection(itsSlot));
        Collections.sort(displaySlots, theComparator);
        ComponentUtilities.setListValues(itsList, displaySlots);
        itsList.setForeground(Color.black);
        itsList.setToolTipText("");

    }

    private void updateSlots(Slot tmpslot) {
        SlotsModel model = itsWidget.getSlotsModel();
        Collection slots = model.getSlots();

        slots.remove(tmpslot);

        int nSlots = slots.size();
        String[] slotsStr = new String[nSlots];
        String[] slotsType = new String[nSlots];

        Iterator i = slots.iterator();

        int j = 0;
        while (i.hasNext()) {
            Slot slot = (Slot) i.next();
            slotsStr[j] = slot.getName();
            slotsType[j] = Helper.getSlotType(slot);
            j++;
        }

        model.initialize(slotsStr, slotsType, slots);
    }

    private void valueChanged() {
        String slotName = null;
        if (itsSlot == null)
            itsWidget.updateSearchWidget("null");
        else {
            slotName = itsSlot.getName();
            if (slotName == null || slotName.length() < 1)
                itsWidget.updateSearchWidget("null");
        }

        if (slotName != null && itsSlot != null) {
            setActionsEnabled(true);
            itsWidget.updateSearchWidget(slotName);

            itsWidget.clearSearchObjectContent(slotName);
        } else
            setActionsEnabled(false);
    }

    public void viewObject() {
        if (itsSlot == null)
            return;
        itsWidget.getKB().getProject().show(itsSlot);
    }
}
