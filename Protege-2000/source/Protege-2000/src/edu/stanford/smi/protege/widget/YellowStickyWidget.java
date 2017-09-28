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
import javax.swing.border.*;
import javax.swing.event.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;
/**
 * Slot widget for handling annotation text for instance annotations (yellow stickies)
 *
 * @author Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class YellowStickyWidget extends AbstractSlotWidget {
    private static final Color YELLOW_STICKY_COLOR = new Color(255, 255, 204);
    private JTextArea _textArea;

    public Collection getValues() {
        Collection result;
        String text = _textArea.getText();
        text = text.trim();
        if (text.length() == 0) {
            result = Collections.EMPTY_LIST;
        } else {
            result = CollectionUtilities.createCollection(text);
        }
        return result;
    }

    public void initialize() {
        _textArea = ComponentFactory.createTextArea();
        _textArea.setBackground(YELLOW_STICKY_COLOR);
        Font oldFont = _textArea.getFont();
        Font newFont = new Font(oldFont.getName(), Font.ITALIC, oldFont.getSize());
        _textArea.setFont(newFont);
        _textArea.getDocument().addDocumentListener(new DocumentChangedListener() {
            public void stateChanged(ChangeEvent event) {
                valueChanged();
            }
        });
        JScrollPane pane = new JScrollPane(_textArea);
        pane.setBorder(null);
        add(pane);
        setPreferredRows(2);
        setPreferredColumns(2);
    }

    public static boolean isSuitable(Cls cls, Slot slot, Facet facet) {
        return TextFieldWidget.isSuitable(cls, slot, facet);
    }

    public void setBorder(Border b) {
        /* at design time we need a border because the selection rectangle is a border.
         * At runtime it is not essential and the widget looks better without one
         */
        if (isDesignTime()) {
            super.setBorder(b);
        } else {
            // do nothing
        }
    }

    public void setEditable(boolean editable) {
        _textArea.setEnabled(editable);
    }

    public void setValues(Collection values) {
        String text = (String) CollectionUtilities.getFirstItem(values);
        if (text == null) {
            text = "";
        }
        _textArea.setText(text);
    }
}
