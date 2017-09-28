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

import java.awt.*;
import java.util.*;
import javax.swing.*;
import javax.swing.event.*;
import edu.stanford.smi.protege.event.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class InstanceNameWidget extends TextFieldWidget {

    public void addNotify() {
        super.addNotify();
        if (isRuntime() && needsNameChange()) {
            SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                    selectAll();
                }
            });
        }
    }

    /*
     * public void setText(String s) {
     * super.setText(s);
     * JTextField field = getTextField();
     * Color color = isOK() ? Color.black : Color.red;
     * getTextField().setForeground(color);
     * }
     * private boolean isOK() {
     * String s = getText();
     * return s.equals(getNameToDisplay()) || getKnowledgeBase().getFrame(s) == null;
     * }
     */

    public static boolean isSuitable(Cls cls, Slot slot, Facet facet) {
        return slot.getName().equals(Model.Slot.NAME);
    }

    private boolean needsNameChange() {
        boolean needsNameChange = false;
        if (isEditable()) {
            String name = getInstance().getName();
            int index = name.lastIndexOf('_');
            String possibleIntegerString = name.substring(index + 1);
            try {
                Integer.parseInt(possibleIntegerString);
                needsNameChange = true;
            } catch (Exception e) {
            }
        }
        return needsNameChange;
    }

    public void setAssociatedCls(Cls associatedCls) {
        super.setAssociatedCls(associatedCls);
        if (associatedCls != null) {
            setEditable(false);
        }
    }

    public void setEditable(boolean b) {
        super.setEditable(b && !isSlotAtCls());
    }

    public void setInstance(Instance instance) {
        super.setInstance(instance);
        if (needsNameChange()) {
            selectAll();
        }
    }

    public void setInstanceValues() {
        String name = (String) CollectionUtilities.getFirstItem(getValues());
        if (name != null) {
            name = name.trim();
            if (getKnowledgeBase().containsFrame(name)) {
                ModalDialog.showMessageDialog(this, "Duplicate frame name: Unable to change name to " + name);
                // Log.stack("name not changed: " + name, this, "setInstanceValues");
                setText(getInstance().getName());
                getTextField().requestFocus();
            } else {
                Instance instance = getInstance();
                Slot slot = getSlot();
                if (instance instanceof Cls && getCls() == slot.getDirectType()) {
                    String oldName = slot.getName();
                    slot.setName(name);
                } else {
                    instance.setName(name);
                }
            }
        }
    }

    public void setWidgetValues() {
        if (isSlotAtCls()) {
            setText(getInstance().getName());
        } else {
            super.setWidgetValues();
        }
    }
}
