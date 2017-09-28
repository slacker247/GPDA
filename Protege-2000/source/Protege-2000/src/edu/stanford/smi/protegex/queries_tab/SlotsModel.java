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

package edu.stanford.smi.protegex.queries_tab;

import java.util.*;
import javax.swing.*;
import java.awt.*;

public class SlotsModel extends DefaultComboBoxModel{
   private Collection slots;
   private String[] slotNames;
   private String[] slotTypes;
   private String type;

    public SlotsModel(String[] names, String[] types, Collection slotCol) {
        super();
        initialize(names, types, slotCol);
    }

    private void addAny() {
        if (slotNames == null)
            return;
        for (int i = 0; i < slotNames.length; i++) {
            addElement(slotNames[i]);
        }
    }

    private void addType(String type) {
        if (type == null || slotNames == null)
            addAny();
        for (int i = 0; i < slotNames.length; i++) {
            if (slotTypes[i].equals(type))
                addElement(slotNames[i]);
        }
    }

    public String[] getSlotNames() {
        return slotNames;
    }

    public Collection getSlots() {
        return slots;
    }

    public String[] getSlotTypes() {
        return slotTypes;
    }

    public String getType() {
        return type;
    }

    public void initialize(String[] names, String[] types, Collection slotCol) {
        slotNames = names;
        slotTypes = types;
        this.slots = slotCol;
    }

    public void removeName(String name) {

    }

    public void setUpComboBox(String type) {
        if (getSize() > 0)
            removeAllElements();
        if (type == null || type.equals("ANY"))
            addAny();
        else
            addType(type);
    }
}
