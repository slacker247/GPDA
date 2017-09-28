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
import javax.swing.event.*;
import java.awt.event.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.protegex.queries_tab.*;
import edu.stanford.smi.protege.widget.*;

public class FloatSlotValueWidget extends AbstractSlotValueWidget{
    JTextField itsField;
    JComponent itsComp;

    private class TextFieldDocumentListener extends DocumentChangedListener {
         public void stateChanged(ChangeEvent e) {

             String stringifiedNumber = itsField.getText();
         if ((null == stringifiedNumber) || (stringifiedNumber.equals(""))) {
             slotIsCorrect();
             return;
         }

         if ( _validator == null)  _validator = getValidator();
         if (_validator != null) {
            String validationResponse = _validator.validateString(stringifiedNumber);
            if (null!=validationResponse) {
                itsField.setForeground(Color.red);
                itsField.setToolTipText(validationResponse);
              itsWidget.setReady(false);
            } else {
                slotIsCorrect();
            }
              }
        }

   private void slotIsCorrect() {
            itsField.setForeground(Color.black);
            itsField.setToolTipText("");
            itsWidget.setReady(true);
        }
    }

    private class TextFieldFocusListener extends FocusAdapter {
        public void focusLost(FocusEvent e) {
        }
  }

    /** Constructor. */
    public FloatSlotValueWidget(SearchWidget widget) {
        super(widget);
        constraints = ConstraintsModel.getFloatConstraints();
        this.label = "float";
        createComponents(null);
    }

    /** Constructor with label, slot, and actions. */
    public FloatSlotValueWidget(SearchWidget widget, String label, String slot) {
        super(widget);
        setSlotName(slot);
        this.label = label;
    }

    /** Create components in this relation display. */
    private void createComponents(String label) {
        itsField = createTextField();
        JScrollPane scroll = new JScrollPane(itsField);
        scroll.setPreferredSize(new Dimension(WIDTH, HEIGHT));

        LabeledComponent c = new LabeledComponent("Float", itsField);

        itsComp = c;
        itsField.getDocument().addDocumentListener(new TextFieldDocumentListener());
        itsField.addFocusListener(new TextFieldFocusListener());

    }

    /** Create a textarea. */
    private JTextField createTextField() {
        JTextField text = new JTextField();
        text.setPreferredSize(new Dimension(FIELDWIDTH, FIELDHEIGHT));
        text.setText("");
        return text;
    }

    private boolean doTest(int testIndex, float instanceVal, float value) {
        boolean testResult = false;
        switch (testIndex) {
            case 0 : // is equal to
                if (instanceVal == value)
                    testResult = true;
                break;
            case 1 : // is greater than
                if (instanceVal > value)
                    testResult = true;
                break;
            case 2 : // is less than
                if (instanceVal < value)
                    testResult = true;
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

    /** Get the selected object which is a string for float slot. */
    public Object getSelectedObject() {
        return (String) itsField.getText();
    }

    private int getTestIndex(String constraint) {
        for (int i = 0; i < constraints.length; i++)
            if (constraint.toLowerCase().equals(constraints[i]))
                return i;
        return -1;
    }

    public AbstractTemplateSlotNumberValidator getValidator() {

        slotName = itsWidget.getSearchSubject();

        if (slotName == null || selection == null)
            return null;
        return new FloatTemplateSlotValidator(selection, itsWidget.getKB().getSlot(slotName));
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
        if (!slotType.toLowerCase().equals("float"))
            return null;

        // Here we first test the function based on the simplest integer cases
        ArrayList resultInstances = new ArrayList();
        Collection instances = itsInstances;
        Iterator i = instances.iterator();
        Slot slot = itsWidget.getKB().getSlot(specification.getName());
        while (i.hasNext()) {
            Instance instance = (Instance) i.next();
            if (testFloat(getTestIndex(specification.getConstraint()), instance, slot))
                resultInstances.add(instance);
        }
        return resultInstances;
    }

    // This is used for Query test
    public Collection search(Collection instances, Slot slot, String operation, Object obj) throws NumberFormatException {
        ArrayList resultInstances = new ArrayList();
        Iterator i = instances.iterator();

        Number num1 = new Float((String) obj);
        float value = num1.floatValue();

        while (i.hasNext()) {
            Instance instance = (Instance) i.next();
            if (testFloat(getTestIndex(operation), instance, slot, value))
                resultInstances.add(instance);
        }
        return resultInstances;
    }

    public Collection search(Collection instances, String constraint, Slot slot) {
        ArrayList resultInstances = new ArrayList();
        Iterator i = instances.iterator();
        while (i.hasNext()) {
            Instance instance = (Instance) i.next();
            if (testFloat(getTestIndex(constraint), instance, slot))
                resultInstances.add(instance);
        }
        return resultInstances;
    }

    /** Set the specified string to the text area. */
    public void setData(Object[] data) {

        itsField.setText("");
        itsField.repaint();
        if (selection != null)
            itsWidget.setReady(false);

        return;
    }

    public void setSelectedObject(Object obj) {
        itsField.setText((String) obj);
        itsField.repaint();

    }

    private boolean testFloat(int testIndex, Instance instance, Slot slot) throws NumberFormatException {
        float value;
        if (specification.getValue() == null)
            return false;
        if (((String) specification.getValue()).length() < 1)
            return false;
        Number num1 = new Float((String) specification.getValue());
        value = num1.floatValue();
        return testFloat(testIndex, instance, slot, value);
    }

    /** Main test routine for Float Slot */
    private boolean testFloat(int testIndex, Instance instance, Slot slot, float value) {
        boolean testResult = false;
        if (instance == null)
            return testResult;
        if (instance.getOwnSlotValue(slot) == null)
            return false;

        Collection instanceVals = instance.getOwnSlotValues(slot);
        Iterator i = instanceVals.iterator();

        while (i.hasNext()) {
            float instanceVal = ((Float) i.next()).floatValue();
            if (doTest(testIndex, instanceVal, value)) {
                testResult = true;
                break;
            }
        }
        return testResult;
    }
}
