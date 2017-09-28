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

class ButtonControlPanel extends JComponent {

    private String _name;
    private PropertyList _propertyList;
    private JCheckBox _enabledComponent;
    private JTextField _textComponent;


    public ButtonControlPanel(String name, String defaultDescription, boolean defaultState, PropertyList propertyList) {
        _name = name;
        _propertyList = propertyList;

        _enabledComponent = ComponentFactory.createCheckBox();
        Boolean value = _propertyList.getBoolean(displayStringProperty());
        _enabledComponent.setSelected(value == null ? defaultState : value.booleanValue());

        _enabledComponent.setText("Show " + name + " Button");
        _textComponent = ComponentFactory.createTextField();
        String text = _propertyList.getString(descriptionProperty());
        if (text == null) {
            text = defaultDescription;
        }
        _textComponent.setText(text);

        setLayout(new BorderLayout());
        add(_enabledComponent, BorderLayout.NORTH);
        JPanel panel = new JPanel(new BorderLayout());
        panel.add(_textComponent, BorderLayout.NORTH);
        add(panel, BorderLayout.CENTER);
    }

    private String descriptionProperty() {
        return ButtonConfigurationPanel.getDescriptionPropertyName(_name);
    }

    private String displayStringProperty() {
        return ButtonConfigurationPanel.getDisplayPropertyName(_name);
    }

    public void saveContents() {
        boolean isEnabled = _enabledComponent.isSelected();
        _propertyList.setBoolean(displayStringProperty(), new Boolean(isEnabled));
        String text = _textComponent.getText();
        _propertyList.setString(descriptionProperty(), text);
    }
}
