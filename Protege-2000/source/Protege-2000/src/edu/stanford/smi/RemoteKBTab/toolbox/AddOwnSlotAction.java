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

import javax.swing.*;
import edu.stanford.smi.RemoteKBTab.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.model.*;
import java.awt.event.*;
import java.util.*;

/** Add the selected string(s) to the own slot on the selected Instance
 *  (or Class?). This action asks the RelationDisplay for its associated slot
 *  and then adds the selected value to that own slot on the selected
 *  Instance. */
public class AddOwnSlotAction extends AbstractAction implements AbstractRemoteKBAction {

    RelationDisplay display;
    KnowledgeBase kb;
    RemoteKBTab tab;
    String slotName;
    Slot slot;

    public AddOwnSlotAction(RemoteKBTab tab) {
        this("add selected item as slot value", tab, Icons.getSlotIcon());
    }

    public AddOwnSlotAction(String tooltip, RemoteKBTab tab, Icon i) {
        super(tooltip, i);
        this.tab = tab;
        kb = tab.getProject().getKnowledgeBase();
    }

    /**  All AbstractRemoteKBAction classes must have the method
     *   setRelationDisplay, so the action can be associated with the
     *   RelationDisplay. */
    public void setRelationDisplay(RelationDisplay rd) {
        display = rd;
        slotName = rd.getSlotName();
        if (slotName == null) { return; }
        slot = createSlot(slotName);
        slot.setAllowsMultipleValues(!rd.isSlotSingleValued());
    }

    /** Perform the add own slot action. Add selected items as own slot values.
     *  Set selected string to the relation display specified own slot. */
    public void actionPerformed(ActionEvent e) {
        Frame frame = tab.getSelectedFrame();
	if (slot == null || frame == null) { return; }

        String[] values = display.getSelectedItems();
        if (values == null) { return; }

        String value = values[0];
        if (!frame.hasOwnSlot(slot)) {
            return;
	}

	List valueList = Arrays.asList(values);

	if (((Instance)frame).getDirectType().getTemplateSlotAllowsMultipleValues(slot)) {
            Collection currentValues = frame.getOwnSlotValues(slot);
	    Vector fullList = new Vector();
	    if (currentValues != null) {
                Iterator it = currentValues.iterator();
                while(it.hasNext()) {
                    Object next = it.next();
		    if (!valueList.contains(next)) {
                        fullList.add(next);
                    }
                }
            }
	    fullList.addAll(valueList);
            frame.setOwnSlotValues(slot, fullList);														//has an own slot then it is an instance
        } else {
            frame.setOwnSlotValue(slot, value);
        }
    }

    /** Create specified slot. */
    private Slot createSlot(String name) {
        Slot check = kb.getSlot(name);
        if (check == null) {
            check = kb.createSlot(name);
        }
	return check;
    }
}