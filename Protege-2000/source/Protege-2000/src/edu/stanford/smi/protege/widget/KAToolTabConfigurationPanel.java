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
import javax.swing.*;
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.protege.ui.*;

/**
 * Description of Type
 *
 * @author Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class KAToolTabConfigurationPanel extends AbstractValidatableComponent {
    private InstanceField _topLevelInstanceField;
    private JTextField _labelField;
    private KAToolTab _tab;

    public KAToolTabConfigurationPanel(KAToolTab tab) {
        this._tab = tab;

        _topLevelInstanceField = new InstanceField("Top-Level Instance", tab.getKnowledgeBase().getRootClses());
        _topLevelInstanceField.setInstance(tab.getTopLevelInstance());
        _topLevelInstanceField.createSelectInstanceAction();
        _topLevelInstanceField.createRemoveInstanceAction();

        _labelField = ComponentFactory.createTextField();
        _labelField.setText(tab.getLabel());

        setLayout(new GridLayout(2, 1));
        add(new LabeledComponent("Label", _labelField));
        add(_topLevelInstanceField);
    }

    /**
     * saveContents method comment.
     */
    public void saveContents() {
        _tab.setLabel(_labelField.getText());
        _tab.setTopLevelInstance(_topLevelInstanceField.getInstance());
    }

    /**
     * saveContents method comment.
     */
    public boolean validateContents() {
        // do nothing
        return true;
    }
}
