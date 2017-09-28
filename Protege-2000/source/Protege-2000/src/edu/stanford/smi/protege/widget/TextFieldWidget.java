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
import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import javax.swing.event.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class TextFieldWidget extends AbstractSlotWidget {
    private JTextField _textField;
    private Color _defaultColor;
    private static Color _invalidColor = Color.red;
    private boolean _isDirty;
    private DocumentChangedListener _documentListener = new DocumentChangedListener() {
        public void stateChanged(ChangeEvent event) {
            _isDirty = true;
            validateText();
        }
    };
    private FocusListener _focusListener = new FocusAdapter() {
        public void focusLost(FocusEvent event) {
            commitIfNecessary();
        }
    };

    private void commitIfNecessary() {
        if (_isDirty) {
            valueChanged();
        }
    }

    public JTextField createTextField() {
        return ComponentFactory.createTextField();
    }

    public void dispose() {
        // Log.stack("", this, "dispose");
        super.dispose();
        commitIfNecessary();
        // These calls should be unneccessary but this object doesn't get gc'd if we
        // don't remove the listeners!  (JDK1.2)
        _textField.getDocument().removeDocumentListener(_documentListener);
        _textField.removeFocusListener(_focusListener);
    }

    protected String getInvalidTextDescription(String text) {
        return null;
    }

    public String getText() {
        return _textField.getText();
    }

    public JTextField getTextField() {
        return _textField;
    }

    public Collection getValues() {
        _isDirty = false;
        String s = _textField.getText();
        if (s.length() == 0) {
            s = null;
        }
        return CollectionUtilities.createList(s);
    }

    public void initialize() {
        _textField = createTextField();
        _textField.getDocument().addDocumentListener(_documentListener);
        _textField.addFocusListener(_focusListener);
        _defaultColor = _textField.getForeground();
        add(new LabeledComponent(getLabel(), _textField));
        setPreferredColumns(2);
        setPreferredRows(1);
    }

    public boolean isEditable() {
        return _textField.isEditable();
    }

    public static boolean isSuitable(Cls cls, Slot slot, Facet facet) {
        boolean isSuitable;
        if (cls == null || slot == null) {
            isSuitable = false;
        } else {
            ValueType type = cls.getTemplateSlotValueType(slot);
            boolean isString = type == ValueType.STRING;
            boolean isSymbol = type == ValueType.SYMBOL;
            boolean isMultiple = cls.getTemplateSlotAllowsMultipleValues(slot);
            isSuitable = (isString || isSymbol) && !isMultiple;
        }
        return isSuitable;
    }

    public void selectAll() {
        _textField.selectAll();
        _textField.requestFocus();
    }

    public void setEditable(boolean b) {
        _textField.setEditable(b);
    }

    public void setText(String text) {
        _documentListener.disable();
        _textField.setText(text == null ? "" : text);
        _isDirty = false;
        _documentListener.enable();
    }

    public void setValues(final Collection values) {
        Object o = CollectionUtilities.getFirstItem(values);
        String text = o == null ? (String) null : o.toString();
        setText(text);
    }

    private void validateText() {
        String text = getInvalidTextDescription(_textField.getText());
        if (text == null) {
            _textField.setForeground(_defaultColor);
            _textField.setToolTipText(null);
        } else {
            _textField.setForeground(_invalidColor);
            _textField.setToolTipText(text);
        }
    }
}
