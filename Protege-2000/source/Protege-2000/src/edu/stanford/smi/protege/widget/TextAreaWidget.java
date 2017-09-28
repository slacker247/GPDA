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
public class TextAreaWidget extends AbstractSlotWidget {
    private JTextArea _textArea;
    private boolean _isDirty;

    private DocumentChangedListener _documentListener =
        new DocumentChangedListener() {
            public void stateChanged(ChangeEvent event) {
                // valueChanged();   // character by character update
                _isDirty = true;
            }
        }
    ;
    private FocusListener _focusListener =
        new FocusAdapter() {
            public void focusLost(FocusEvent event) {
                if (_isDirty) {
                    valueChanged();
                }
            }
        }
    ;

    public JTextArea createTextArea() {
        return ComponentFactory.createTextArea();
    }

    public void dispose() {
        super.dispose();
        if (_isDirty) {
            valueChanged();
            _textArea.getDocument().removeDocumentListener(_documentListener);
            _textArea.removeFocusListener(_focusListener);
        }
    }

    public JTextArea getTextArea() {
        return _textArea;
    }

    public Collection getValues() {
        _isDirty = false;
        String s = _textArea.getText();
        if (s.length() == 0) {
            s = null;
        }
        return CollectionUtilities.createList(s);
    }

    public void initialize() {
        _textArea = createTextArea();
        _textArea.getDocument().addDocumentListener(_documentListener);
        _textArea.addFocusListener(_focusListener);
        add(new LabeledComponent(getLabel(), new JScrollPane(_textArea)));
        // setCanStretchVertically(true);
        setPreferredColumns(2);
        setPreferredRows(2);
    }

    public static boolean isSuitable(Cls cls, Slot slot, Facet facet) {
        boolean isSuitable;
        if (cls == null || slot == null) {
            isSuitable = false;
        } else {
            boolean isString = cls.getTemplateSlotValueType(slot) == ValueType.STRING;
            boolean isMultiple = cls.getTemplateSlotAllowsMultipleValues(slot);
            isSuitable = isString && !isMultiple;
        }
        return isSuitable;
    }

    public void setEditable(boolean b) {
        _textArea.setEditable(b);
    }

    public void setText(String text) {
        _documentListener.disable();
        _textArea.setText(text == null ? "" : text);
        _documentListener.enable();
        _isDirty = false;
    }

    public void setValues(Collection values) {
        setText((String) CollectionUtilities.getFirstItem(values));
    }
}
