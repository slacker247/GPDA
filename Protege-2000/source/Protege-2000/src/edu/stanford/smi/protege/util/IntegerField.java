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

package edu.stanford.smi.protege.util;


import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import edu.stanford.smi.protege.resource.*;

public class IntegerField extends JComponent {
    private ActionListener itsActionListener;
    private JTextField _textField;
    private JLabel _label;

    public IntegerField(String text) {
        setLayout(new BorderLayout());
        _label = new JLabel(text);
        _label.setForeground(UIManager.getColor("Checkbox.foreground"));
        createTextField();

        JPanel north = new JPanel(new BorderLayout());
        north.add(_label, BorderLayout.WEST);
        north.add(_textField, BorderLayout.CENTER);
        add(north, BorderLayout.NORTH);
    }

    public void addActionListener(ActionListener listener) {
        itsActionListener = listener;
    }

    public void clearValue() {
        _textField.setText("");
    }

    private JComponent createTextField() {
        _textField = ComponentFactory.createTextField();
        _textField.setHorizontalAlignment(JTextField.RIGHT);
        _textField.getDocument().addDocumentListener(new DocumentChangedListener() {
            public void stateChanged(ChangeEvent event) {
                notifyChanged();
            }
        });
        _textField.setColumns(3);
        return _textField;
    }

    public Integer getValue() {
        Integer i;
        try {
            String s = _textField.getText();
            i = new Integer(s);
        } catch (NumberFormatException e) {
            i = null;
        }
        return i;
    }

    private void notifyChanged() {
        if (itsActionListener != null) {
            itsActionListener.actionPerformed(new ActionEvent(this, ActionEvent.ACTION_PERFORMED, null));
        }
    }

    public void removeActionListener(ActionListener listener) {
        itsActionListener = null;
    }

    public void setEnabled(boolean b) {
        super.setEnabled(b);
        _textField.setEnabled(b);
        _label.setEnabled(b);
    }

    public void setValue(int i) {
        _textField.setText(String.valueOf(i));
        _textField.repaint();
    }
}
