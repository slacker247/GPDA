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

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class ButtonConfigurationPanel extends AbstractValidatableComponent {
    private PropertyList _propertyList;

    public ButtonConfigurationPanel(PropertyList propertyList) {
        _propertyList = propertyList;
    }

    public void addButton(String name, String defaultDescription, boolean defaultState) {
        add(new ButtonControlPanel(name, defaultDescription, defaultState, _propertyList));
        setLayout(new GridLayout(getComponentCount(), 1, 10, 10));
    }

    public static String getDescriptionPropertyName(String buttonName) {
        return "ButtonDescription-" + buttonName;
    }

    public static String getDisplayPropertyName(String buttonName) {
        return "ButtonDisplayed-" + buttonName;
    }

    public Dimension getPreferredSize() {
        Dimension d = super.getPreferredSize();
        d.width = Math.max(d.width, 300);
        return d;
    }

    public void saveContents() {
        int nComponents = getComponentCount();
        for (int i = 0; i < nComponents; ++i) {
            ButtonControlPanel panel = (ButtonControlPanel) getComponent(i);
            panel.saveContents();
        }
    }

    public boolean validateContents() {
        return true;
    }
}
