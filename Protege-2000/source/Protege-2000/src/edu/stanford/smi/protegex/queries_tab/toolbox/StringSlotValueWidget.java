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

public class StringSlotValueWidget extends AbstractSlotValueWidget{
    JTextField itsField;
  JComponent itsComp;

    /** Constructor. */
    public StringSlotValueWidget(SearchWidget widget) {
        super(widget);
        constraints = ConstraintsModel.getStringConstraints();
        this.label = "string";
        createComponents(null);
    }

    /** Constructor with label, slot, and actions. */
    public StringSlotValueWidget(SearchWidget widget, String label, String slot) {
        super(widget);
        setSlotName(slot);
        this.label = label;
    }

    /** Decide whether firstString contains targetString or not. */
    private boolean containsString(String firstString, String targetStr) {
        if (firstString.length() < targetStr.length())
            return false;
        else {
            for (int i = 0; i < firstString.length() - targetStr.length() + 1; i++) {
                if (firstString.regionMatches(true, i, targetStr, 0, targetStr.length()))
                    return true;
            }

        }
        return false;
    }

    /** Create components in this relation display. */
    private void createComponents(String label) {
        itsField = createTextField();
        JScrollPane scroll = new JScrollPane(itsField);
        scroll.setPreferredSize(new Dimension(150, 40));

        LabeledComponent c = new LabeledComponent("String", itsField);
        itsComp = c;
    }

    /** Create a textarea. */
    private JTextField createTextField() {
        JTextField text = new JTextField();
        text.setPreferredSize(new Dimension(FIELDWIDTH, FIELDHEIGHT));
        text.setText("");
        return text;
    }

    private boolean doTest(int testIndex, String stringVal, String value) {
        boolean testResult = false;
        switch (testIndex) {
            case 0 : // contains

                if (stringVal == null)
                    return false;
                testResult = containsString(stringVal.trim(), value.trim());
                break;
            case 1 : // doesn't contain

                if (stringVal == null)
                    return true;
                testResult = !containsString(stringVal.trim(), value.trim());
                break;
            case 2 : // is
                if (stringVal == null)
                    return false;
                if (stringVal.equalsIgnoreCase(value.trim()))
                    testResult = true;
                break;
            case 3 : // isn't
                if (stringVal == null)
                    return true;
                testResult = !(stringVal.equalsIgnoreCase(value.trim()));
                break;
            case 4 : // begins with
                if (stringVal == null)
                    return false;
                testResult = stringVal.toLowerCase().startsWith(value.trim().toLowerCase());
                break;

            case 5 : // ends with
                if (stringVal == null)
                    return false;
                testResult = stringVal.toLowerCase().endsWith(value.trim().toLowerCase());
                break;
            default :
                break;
        }
        return testResult;
    }

    /** Return the relation display component. */
    public JComponent getComponent() {
        return itsComp;
    }

    /** Get the data from the text area and return it as a string array. */
    public Object[] getData() {
        String[] itsData = new String[1];
        itsData[0] = new String(itsField.getText());
        return itsData;
    }

    /** Get the selected string in text area and return them as a string array. */
    public String[] getSelectedItems() {
        String[] text = new String[1];
        text[0] = itsField.getSelectedText();
        if (text[0] == null)
            text[0] = itsField.getText();
        return text;
    }

    /** Get the selected object which is a string for symbol slot. */
    public Object getSelectedObject() {
        return (String) itsField.getText();
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

    public Collection search() {
        if (specification.getType() == null)
            return null;
        String slotType = specification.getType();
        if (!slotType.toLowerCase().equals("string"))
            return null;

        // Here we first test the function based on the simplest String cases
        ArrayList resultInstances = new ArrayList();

        Collection instances = itsInstances;
        Iterator i = instances.iterator();

        Slot slot = itsWidget.getKB().getSlot(specification.getName());
        while (i.hasNext()) {
            Instance instance = (Instance) i.next();
            if (testString(getTestIndex(specification.getConstraint()), instance, slot))
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
            if (testString(getTestIndex(operation), instance, slot, value))
                resultInstances.add(instance);
        }
        return resultInstances;
    }

    /** Set the specified string to the text area. */
    public void setData(Object[] data) {

        itsField.setText("");
        itsField.repaint();
        itsWidget.setReady(true);
        return;
    }

    public void setSelectedObject(Object obj) {
        itsField.setText((String) obj);
        itsField.repaint();
        itsWidget.setReady(true);
        return;
    }

    private boolean testString(int testIndex, Instance instance, Slot slot) {
        String value;
        if (specification.getValue() == null)
            return false;
        if (((String) specification.getValue()).length() < 1)
            return false;
        value = (String) specification.getValue();
        return testString(testIndex, instance, slot, value);
    }

    /** Main test subroutine for String slot. */
    private boolean testString(int testIndex, Instance instance, Slot slot, String value) {
        boolean testResult = false;
        if (instance == null)
            return testResult;

        if (value == null || value.length() < 1)
            return false;

        Collection stringVals = instance.getOwnSlotValues(slot);
        Iterator i = stringVals.iterator();

        while (i.hasNext()) {

            String stringVal = (String) i.next();
            if (doTest(testIndex, stringVal, value)) {
                testResult = true;
                break;
            }
            continue;
        }
        return testResult;
    }
}
