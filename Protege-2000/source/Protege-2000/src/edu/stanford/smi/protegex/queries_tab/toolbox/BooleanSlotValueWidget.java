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

public class BooleanSlotValueWidget extends AbstractSlotValueWidget{
  JComboBox itsCombo;
  JComponent itsComp;
  String[] values = {"true", "false"};

    /** Constructor. */
    public BooleanSlotValueWidget(SearchWidget widget) {
        super(widget);
        constraints = ConstraintsModel.getBooleanConstraints();
        this.label = "boolean";
        createComponents(null);
    }

    /** Constructor with label, slot, and actions. */
    public BooleanSlotValueWidget(SearchWidget widget, String label, String slot) {
        super(widget);
        setSlotName(slot);
        this.label = label;
    }

    /** Create a Combobox. */
    private JComboBox createComboBox() {

        JComboBox combo = new JComboBox(values);
        combo.setPreferredSize(new Dimension(120, 25));
        return combo;
    }

    /** Create components in this relation display. */
    private void createComponents(String label) {
        itsCombo = createComboBox();
        JScrollPane scroll = new JScrollPane(itsCombo);
        scroll.setPreferredSize(new Dimension(WIDTH, HEIGHT));

        LabeledComponent c = new LabeledComponent("Boolean", itsCombo);
        itsComp = c;

    }

    /** Create a textarea. */
    private JTextField createTextField() {
        JTextField text = new JTextField();
        text.setPreferredSize(new Dimension(120, 25));
        text.setText(label);
        return text;
    }

    /** Return the relation display component. */
    public JComponent getComponent() {
        return itsComp;
    }

    /** Get the data from the text area and return it as a string array. */
    public Object[] getData() {
        String[] itsData = new String[1];
        itsData[0] = getSelectedItem();
        return itsData;
    }

    /** Get the selected string in text area and return them as a string array. */
    public String getSelectedItem() {
        return (String) itsCombo.getSelectedItem();
    }

    /** Get the selected object which is a string for symbol slot. */
    public Object getSelectedObject() {
        return getSelectedItem();
    }

    private int getTestIndex(String constraint) {
        return 0;
    }

    /** Return the embedded widget in the relation display, the JTextArea. */
    public JComponent getWidget() {
        return itsComp;
    }

    /** Set the slot not a single value slot. */
    public boolean isSlotSingleValued() {
        return true;
    }

    public Collection search() {
        if (specification.getType() == null)
            return null;
        String slotType = specification.getType();
        if (!slotType.toLowerCase().equals("boolean"))
            return null;

        // Here we first test the function based on the simplest Boolean cases
        ArrayList resultInstances = new ArrayList();
        Collection instances = itsInstances;
        Iterator i = instances.iterator();

        Slot slot = itsWidget.getKB().getSlot(specification.getName());

        while (i.hasNext()) {
            Instance instance = (Instance) i.next();
            if (testBoolean(getTestIndex(specification.getConstraint()), instance, slot))
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
            if (testBoolean(getTestIndex(operation), instance, slot, value))
                resultInstances.add(instance);
        }
        return resultInstances;
    }

    /** Set the specified string to the text area. */
    public void setData(Object[] data) {

        itsWidget.setReady(true);
        Slot slot = itsWidget.getKB().getSlot((String) data[0]);
        itsCombo.setSelectedItem(data[0]);
        return;

    }

    public void setSelectedObject(Object obj) {
        itsCombo.setSelectedItem(obj);
    }

    private boolean testBoolean(int testIndex, Instance instance, Slot slot) {
        String value;
        if (specification.getValue() == null)
            return false;
        if (((String) specification.getValue()).length() < 1)
            return false;
        value = (String) specification.getValue();
        return testBoolean(testIndex, instance, slot, value);
    }

    /** Main test subroutine for Integer slot. */
    //private boolean testInteger(int testIndex, Instance instance, Slot slot, int value) {

    /** Main test subroutine for Boolean slot. */
    private boolean testBoolean(int testIndex, Instance instance, Slot slot, String value) {
        boolean testResult = false;
        if (instance == null)
            return testResult;
        String instanceVal;

        switch (testIndex) {
            case 0 : // is equal to
                if (instance.getOwnSlotValue(slot) == null)
                    return false;
                instanceVal = instance.getOwnSlotValue(slot).toString();
                if (instanceVal.equalsIgnoreCase(value))
                    testResult = true;
                break;

            default :
                break;
        }
        return testResult;
    }
}
