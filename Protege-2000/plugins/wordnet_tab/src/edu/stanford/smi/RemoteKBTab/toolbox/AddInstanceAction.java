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

/** AddInstanceAction is used to add selected items as instances to the
 *  specified class in protege knowledge base. */
public class AddInstanceAction extends AddClassAction {

    public AddInstanceAction(RemoteKBTab tab) {
        super("add selected item as an instance", tab, Icons.getInstanceIcon());
    }

    /** Perform the add instance action. Add selected items as instances. Set
     *  selected string to the 'Name' slot of the new instance. */
    public void actionPerformed(ActionEvent e) {
        String[] values = display.getSelectedItems();
        if (values.length == 0) {
            JOptionPane.showMessageDialog(tab, "Please select an item in the UMLS Narrow Tree",
                                          "UMLS Tab", JOptionPane.INFORMATION_MESSAGE);
        } else {
            Cls selectedCls = tab.getSelectedCls();
            if (selectedCls != null) {
                for (int i=0; i<values.length; i++) {
                    Instance newInstance = kb.createInstance(null, selectedCls);
                    ModelUtilities.setOwnSlotValue(newInstance, "name", values[i]);
                }
            } else {
                JOptionPane.showMessageDialog(tab, "Please select a class",
                                              "UMLS Tab", JOptionPane.INFORMATION_MESSAGE);
            }
        }
    }
}