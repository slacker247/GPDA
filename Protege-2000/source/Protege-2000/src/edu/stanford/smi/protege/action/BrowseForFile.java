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

package edu.stanford.smi.protege.action;


import java.awt.event.*;
import java.io.*;
import javax.swing.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.util.*;

/**
 * Action to bring up a file chooser dialog and pick a file.
 * When the file is chosen and OK is pressed #onFileChosen is called.
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public abstract class BrowseForFile extends AbstractAction {
    private String _extension;
    private String _description;
    private JComponent _parent;

    public BrowseForFile(JComponent parent, String description) {
        this(parent, description, null);
    }

    public BrowseForFile(JComponent parent, String description, String extension) {
        super("Browse for " + description, Icons.getFindIcon());
        this._extension = extension;
        this._description = description;
        this._parent = parent;
    }

    public void actionPerformed(ActionEvent event) {
        JFileChooser chooser = ComponentFactory.createFileChooser(_description, _extension);
        int rval = chooser.showOpenDialog(_parent);
        if (rval == JFileChooser.APPROVE_OPTION) {
            File file = chooser.getSelectedFile();
            onFileChosen(file);
        }
    }

    public abstract void onFileChosen(File file);
}
