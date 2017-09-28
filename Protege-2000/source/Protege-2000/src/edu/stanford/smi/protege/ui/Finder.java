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

package edu.stanford.smi.protege.ui;


import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.util.List;
import javax.swing.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public abstract class Finder extends JComponent {
    private JTextField _textField;
    private Action _findAction;

    public Finder(String description) {
        _findAction =
            new AbstractAction("", Icons.getFindIcon()) {
                public void actionPerformed(ActionEvent e) {
                    doFind();
                }
            }
        ;
        setLayout(new BorderLayout());
        add(createTextField(), BorderLayout.CENTER);
        add(createFindButton(description), BorderLayout.EAST);
    }

    private JComponent createFindButton(String description) {
        JButton button = ComponentFactory.createButton(_findAction);
        button.setPreferredSize(ComponentFactory.STANDARD_BUTTON_SIZE);
        button.setToolTipText(description);
        return button;
    }

    private JComponent createTextField() {
        _textField = ComponentFactory.createTextField();
        _textField.addActionListener(_findAction);
        return _textField;
    }

    private void doFind() {
        String text = _textField.getText();
        if (text.length() != 0) {
            WaitCursor cursor = new WaitCursor(this);
            long startTime = System.currentTimeMillis();
            List matches = getMatches(text, 1000);
            long stopTime = System.currentTimeMillis();
            // Log.trace("match time (msec) = " + (stopTime - startTime), this, "doFind");
            cursor.hide();
            if (matches.isEmpty()) {
                getToolkit().beep();
            } else if (matches.size() == 1) {
                select(matches.get(0));
            } else {
                String title = "Select from search results (" + matches.size() + " matches)";
                int initialSelection = getBestMatch(matches, text);
                Object o = DisplayUtilities.pickInstanceFromCollection(this, matches,
                        initialSelection, title);
                if (o != null) {
                    select(o);
                }
            }
        }
    }

    protected abstract int getBestMatch(List matches, String text);

    protected abstract List getMatches(String text, int maxMatches);

    protected abstract void select(Object o);
}
