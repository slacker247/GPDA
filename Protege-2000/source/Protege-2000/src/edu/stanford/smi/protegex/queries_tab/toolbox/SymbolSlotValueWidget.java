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
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.protegex.queries_tab.*;

public class SymbolSlotValueWidget extends AbstractSlotValueWidget{

  JComboBox itsCombo;
  SymbolSlotModel itsModel;
  JComponent itsComp;

    /** Constructor. */
    public SymbolSlotValueWidget(SearchWidget widget) {
        super(widget);
        constraints = ConstraintsModel.getSymbolConstraints();
        this.label = "symbol";
        createComponents(null);
    }

    /** Constructor with label, slot, and actions. */
    public SymbolSlotValueWidget(SearchWidget widget, String label, String slot) {
        super(widget);
        setSlotName(slot);
        this.label = label;
    }

    /** Create a Combobox. */
    private JComboBox createComboBox() {
        itsModel = new SymbolSlotModel();
        JComboBox combo = new JComboBox(itsModel);
        combo.setPreferredSize(new Dimension(FIELDWIDTH, FIELDHEIGHT));
        return combo;
    }

    /** Create components in this relation display. */
    private void createComponents(String label) {
        itsCombo = createComboBox();
        JScrollPane scroll = new JScrollPane(itsCombo);
        scroll.setPreferredSize(new Dimension(WIDTH, HEIGHT));

        LabeledComponent c = new LabeledComponent("Symbol", itsCombo);
        itsComp = c;
    }

    private boolean doTest(int testIndex, String instanceVal, String value) {
        boolean testResult = false;
        switch (testIndex) {
            case 0 : // is equal to
                if (instanceVal == null)
                    return false;
                if (instanceVal.equalsIgnoreCase(value))
                    testResult = true;
                break;
            case 1 : // is not equal to
                if (instanceVal == null)
                    return true;
                if (!instanceVal.equalsIgnoreCase(value))
                    testResult = true;
                break;
            default :
                break;
        }

        return testResult;

    }

    /** get the combobox model. */
    public SymbolSlotModel getComboModel() {
        return itsModel;
    }

    /** Return the relation display component. */
    public JComponent getComponent() {
        return itsComp;
    }

    /** Get the data from the text area and return it as a string array. */
    public Object[] getData() {
        String[] itsData = new String[1];
        itsData[0] = new String(getSelectedItem());
        return itsData;
    }

    /** Get the selected string in text area and return them as a string array. */
    public String getSelectedItem() {
        return (String) itsCombo.getSelectedItem();
    }

    /** Get the selected object which is a string for symbol slot. */
    public Object getSelectedObject() {
        return (String) itsCombo.getSelectedItem();
    }

    private int getTestIndex(String constraint) {
        for (int i = 0; i < constraints.length; i++)
            if (constraint.toLowerCase().equals(constraints[i]))
                return i;
        return -1;
    }

    /** Return the embedded widget in the relation display, the JTextArea. */
    public JComponent getWidget() {
        return itsComp;
    }

    /** Set the slot not a single value slot. */
    public boolean isSlotSingleValued() {
        return true;
    }

    /** Transfer list to a string of array. */
    public static String[] listToStringArray(java.util.List list) {
        if (list == null)
            return null;
        String[] result = new String[list.size()];
        ListIterator iterator = list.listIterator();
        int i = 0;
        while (iterator.hasNext()) {
            result[i] = (String) iterator.next();
            i++;
        }
        return result;
    }

    public Collection search() {
        if (specification.getType() == null)
            return null;
        String slotType = specification.getType();
        if (!slotType.toLowerCase().equals("symbol"))
            return null;

        // Here we first test the function based on the simplest symbol cases
        ArrayList resultInstances = new ArrayList();
        Collection instances = itsInstances;
        Iterator i = instances.iterator();
        Slot slot = itsWidget.getKB().getSlot(specification.getName());
        while (i.hasNext()) {
            Instance instance = (Instance) i.next();
            if (testSymbol(getTestIndex(specification.getConstraint()), instance, slot))
                resultInstances.add(instance);
        }
        return resultInstances;
    }

    // This is used for Query test
    public Collection search(Collection instances, Slot slot, String operation, Object obj) {
        ArrayList resultInstances = new ArrayList();
        Iterator i = instances.iterator();
        String value = (String) obj;
        while (i.hasNext()) {
            Instance instance = (Instance) i.next();
            if (testSymbol(getTestIndex(operation), instance, slot, value))
                resultInstances.add(instance);
        }
        return resultInstances;
    }

    /** Set the specified string to the text area. */
    public void setData(Object[] data) {
        itsWidget.setReady(true);
        Slot slot = itsWidget.getKB().getSlot((String) data[0]);
        itsModel.initialize(listToStringArray(new ArrayList(slot.getAllowedValues())));
        return;
    }

    public void setSelectedObject(Object obj) {
        itsCombo.setSelectedItem(obj);
    }

    private boolean testSymbol(int testIndex, Instance instance, Slot slot) {
        String value;
        if (specification.getValue() == null)
            return false;
        if (((String) specification.getValue()).length() < 1)
            return false;
        value = (String) specification.getValue();
        return testSymbol(testIndex, instance, slot, value);
    }

    /** Main test subroutine for symbol slot. */
    private boolean testSymbol(int testIndex, Instance instance, Slot slot, String value) {
        boolean testResult = false;
        if (instance == null)
            return testResult;
        Collection instanceVals = instance.getOwnSlotValues(slot);

        Iterator i = instanceVals.iterator();
        while (i.hasNext()) {
            String instanceVal = (String) i.next();
            if (doTest(testIndex, instanceVal, value)) {
                testResult = true;
                break;
            }
        }
        return testResult;
    }
}
