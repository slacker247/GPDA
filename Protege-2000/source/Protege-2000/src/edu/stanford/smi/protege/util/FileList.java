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
import java.io.*;
import java.util.*;
import javax.swing.*;
import javax.swing.event.*;
import edu.stanford.smi.protege.resource.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class FileList extends AbstractSelectableComponent {
    private JList _fileList;
    private String _description;
    private String _extension;
    private ChangeListener _changeListener;

    public FileList(String label, Collection files, String extension, String description) {
        _description = description;
        _extension = extension;
        setLayout(new BorderLayout());
        LabeledComponent c = new LabeledComponent(label, createComponent(files));
        c.addHeaderButton(newAddAction());
        c.addHeaderButton(newRemoveAction());
        add(c);
        setPreferredSize(new Dimension(300, 200));
    }

    private void browse() {
        JFileChooser chooser = ComponentFactory.createFileChooser(_description, _extension);
        chooser.setMultiSelectionEnabled(true);
        int openDialogResult = chooser.showOpenDialog(this);
        switch (openDialogResult) {
            case JFileChooser.ERROR_OPTION :
                // Get this on 'close"
                break;
            case JFileChooser.CANCEL_OPTION :
                break;
            case JFileChooser.APPROVE_OPTION :
                File[] files = chooser.getSelectedFiles();
                if (files != null) {
                   for (int i = 0; i < files.length ; ++i) {
                      ComponentUtilities.addSelectedListValue(_fileList, files[i].getPath());
                    }
                }
                break;
            default :
                Assert.fail("bad result: " + openDialogResult);
        }
    }

    public void clearSelection() {
        _fileList.clearSelection();
    }

    private JComponent createComponent(Collection files) {
        _fileList = ComponentFactory.createList(newAddAction());
        _fileList.addListSelectionListener(new ListSelectionListenerAdapter(this));
        if (files != null) {
            ComponentUtilities.setListValues(_fileList, files);
        }
        return new JScrollPane(_fileList);
    }

    private LabeledComponent getLabeledComponent() {
        return (LabeledComponent) getComponent(0);
    }

    public Collection getPaths() {
        return ComponentUtilities.getListValues(_fileList);
    }

    public Collection getSelection() {
        return ComponentUtilities.getSelection(_fileList);
    }

    private Action newAddAction() {
        return new AddAction("Browse for File") {
            public void onAdd() {
                browse();
            }
        };
    }

    private Action newRemoveAction() {
        return new RemoveAction("Remove File", this) {
            public void onRemove(Object o) {
                ComponentUtilities.removeListValue(_fileList, o);
            }
        };
    }
}
