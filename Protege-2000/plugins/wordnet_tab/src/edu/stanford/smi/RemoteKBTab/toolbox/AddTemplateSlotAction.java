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

import edu.stanford.smi.RemoteKBTab.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.model.*;
import java.awt.event.*;

/** Add the selected string(s) to the template slot on the selected Instance
 *  (or Class?). This action asks the RelationDisplay for its associated slot
 *  and then adds the selected value to that template slot on the selected
 *  Instance. */
public class AddTemplateSlotAction extends AddOwnSlotAction {

    public AddTemplateSlotAction(RemoteKBTab tab) {
        super("add selected item as slot value", tab, Icons.getSlotIcon());
    }

    /** Perform the add template slot action. Add selected items as template
     *  slot values. Set selected string to the relation display specified
     *  template slot. */
    public void actionPerformed(ActionEvent e) {
        Frame frame = tab.getSelectedFrame();
        if (slot == null || frame == null) { return; }

        String[] values = display.getSelectedItems();
        String value;
        if (values == null) {
            value = null;
            return;
        } else {
            value = values[0];
        }

        if (frame instanceof Cls) {
            Cls cls = (Cls) frame;
            if (cls.getTemplateSlotAllowsMultipleValues(slot)) {
                cls.addOwnSlotValue(slot, values);
            } else {
                cls.addOwnSlotValue(slot, value);
            }
        }
    }
}