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

import java.util.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.protege.widget.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class RoleWidget extends ComboBoxWidget {

    public void comboBoxValueChanged(String newValue) {
        if (RoleConstraint.isAbstract(newValue)) {
            Cls cls = (Cls) getInstance();
            int instanceCount = (cls == null) ? 0 : cls.getDirectInstanceCount();
            if (instanceCount > 0) {
                String s = (instanceCount == 1) ? "" : "s";
                String text =
                    "This class has "
                        + instanceCount
                        + " direct instance"
                        + s
                        + ".\n"
                        + "Changing the role to \"abstract\" will cause these instances to be deleted.\n"
                        + "\n"
                        + "Do you really want to make this change?";
                int response = ModalDialog.showMessageDialog(RoleWidget.this, text, ModalDialog.MODE_YES_NO);
                if (response == ModalDialog.OPTION_YES) {
                    Iterator i = new ArrayList(cls.getDirectInstances()).iterator();
                    while (i.hasNext()) {
                        Instance instance = (Instance) i.next();
                        instance.delete();
                    }
                    valueChanged();
                } else {
                    setComboBoxValue(RoleConstraint.CONCRETE.toString());
                }
            } else {
                valueChanged();
            }
        } else {
            valueChanged();
        }
    }

    public void initialize() {
        setDisplayNullEntry(false);
        super.initialize();
        setRenderer(new RoleRenderer());
    }

    public static boolean isSuitable(Cls cls, Slot slot, Facet facet) {
        return slot.getName().equals(Model.Slot.ROLE);
    }
}
