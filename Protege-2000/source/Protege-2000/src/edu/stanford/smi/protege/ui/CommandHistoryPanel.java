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

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class CommandHistoryPanel extends JPanel {
    // attributes
    private JList _list;
    private JButton _undoButton;
    private JButton _redoButton;

    // initializers
    public CommandHistoryPanel() {
        _list = createList();
        _undoButton = createUndoButton();
        _redoButton = createRedoButton();
        layoutWidgets();
        setPreferredSize(new Dimension(450, 300));
    }

    private JList createList() {
        JList list = ComponentFactory.createList(null);
        list.setModel(new CommandListModel());
        return list;
    }

    private JButton createRedoButton() {
        JButton button = ComponentFactory.createButton(new RedoAction());
        return button;
    }

    private JButton createUndoButton() {
        JButton button = ComponentFactory.createButton(new UndoAction());
        return button;
    }

    // queries
    public String getTitle() {
        return "Command History";
    }

    // implementation
    private void layoutWidgets() {
        JPanel innerPane = ComponentFactory.createPanel();
        innerPane.setLayout(new GridLayout(1, 2, 10, 10));
        innerPane.add(_undoButton);
        innerPane.add(_redoButton);

        JPanel buttonPane = ComponentFactory.createPanel();
        buttonPane.setLayout(new FlowLayout());
        buttonPane.add(innerPane);

        setLayout(new BorderLayout());
        // add(new LabeledComponent(itsList, "Commands", true), BorderLayout.CENTER);
        add(buttonPane, BorderLayout.SOUTH);
    }
}
