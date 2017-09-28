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
import java.util.*;
import java.util.List;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.undo.*;
import edu.stanford.smi.protege.action.*;
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.protege.widget.*;

class CommandListModel extends AbstractListModel {

    // constants
    private final static String INSERT_MARKER = "<----- Current Position -----";

    // attributes
    private List _commands;

    // initializers
    public CommandListModel() {
        _commands = new ArrayList();
        addChangeListener();
        loadCommands();
    }

    // implementation
    private void addChangeListener() {
        ChangeListener listener =
            new ChangeListener() {
                public void stateChanged(ChangeEvent event) {
                    loadCommands();
                }
            }
        ;
        UndoEventManager.getInstance().addChangeListener(listener);
    }

    public Object getElementAt(int i) {
        return _commands.get(i);
    }

    public int getSize() {
        return _commands.size();
    }

    private void loadCommands() {
        fireIntervalRemoved(this, 0, _commands.size() - 1);
        _commands = new ArrayList();
        _commands.addAll(UndoEventManager.getInstance().getEdits());
        UndoableEdit edit = UndoEventManager.getInstance().editToBeRedone();
        if (edit == null) {
            _commands.add(INSERT_MARKER);
        } else {
            int index = _commands.indexOf(edit);
            _commands.add(index, INSERT_MARKER);
        }
        fireIntervalAdded(this, 0, _commands.size() - 1);
    }
}
