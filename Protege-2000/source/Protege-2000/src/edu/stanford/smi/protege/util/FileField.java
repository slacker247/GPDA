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
import javax.swing.*;
import javax.swing.event.*;
import edu.stanford.smi.protege.resource.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class FileField extends JComponent {
    private JTextField _textField;
    private String _description;
    private String _extension;
    private ChangeListener _changeListener;

    public FileField(String label, String path, String extension, String description) {
        _description = description;
        _extension = extension;
        setLayout(new BorderLayout());
        LabeledComponent c = new LabeledComponent(label, createComponent(path));
        c.addHeaderButton(new AbstractAction("Browse for File", Icons.getAddIcon()) {
            public void actionPerformed(ActionEvent event) {
                browse();
            }
        });
        add(c);
    }

    public void addChangeListener(ChangeListener listener) {
        Assert.assertNull("existing change listener", _changeListener);
        _changeListener = listener;
    }

    private void browse() {
        JFileChooser chooser = ComponentFactory.createFileChooser(_description, _extension);
        chooser.setApproveButtonText("Select");
        int openDialogResult = chooser.showOpenDialog(this);
        switch (openDialogResult) {
            case JFileChooser.ERROR_OPTION :
                // Get this on 'close"
                break;
            case JFileChooser.CANCEL_OPTION :
                break;
            case JFileChooser.APPROVE_OPTION :
                _textField.setText(chooser.getSelectedFile().getPath());
                break;
            default :
                Assert.fail("bad result: " + openDialogResult);
        }
    }

    private JComponent createComponent(String text) {
        _textField = ComponentFactory.createTextField();
        _textField.setColumns(40);
        if (text == null || text.length() == 0) {
            text = FileUtilities.getCurrentWorkingDirectory();
        }
        _textField.setText(text);
        _textField.getDocument().addDocumentListener(
            new DocumentChangedListener() {
                public void stateChanged(ChangeEvent event) {
                    notifyListener();
                }
            }
        );
        return _textField;
    }

    private LabeledComponent getLabeledComponent() {
        return (LabeledComponent) getComponent(0);
    }

    public String getPath() {
        String text = _textField.getText();
        return (text.length() == 0) ? (String) null : text;
    }

    private void notifyListener() {
        if (_changeListener != null) {
            _changeListener.stateChanged(new ChangeEvent(this));
        }
    }

    public void setPath(String text) {
        _textField.setText(text);
    }
}
