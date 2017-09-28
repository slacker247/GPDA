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
import java.util.*;
import java.util.List;
import javax.swing.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class FakeToolBar extends JComponent {
    private JComponent _buttons;
    private Dimension _defaultButtonSize;

    public FakeToolBar(int justification) {
        this(justification, ComponentFactory.STANDARD_BUTTON_SIZE);
    }

    public FakeToolBar(int justification, Dimension buttonSize) {
        _defaultButtonSize = new Dimension(buttonSize);
        _buttons = new JPanel();
        setLayout(new BorderLayout());
        String placement = (justification == SwingConstants.LEFT) ? BorderLayout.WEST : BorderLayout.EAST;
        add(_buttons, placement);
    }

    public void addButton(AbstractButton button) {
        button.setPreferredSize(_defaultButtonSize);
        _buttons.add(button);
        updateLayout();
    }

    public List getButtons() {
        return Arrays.asList(_buttons.getComponents());
    }

    public void removeButton(int index) {
        _buttons.remove(index);
        updateLayout();
    }

    private void updateLayout() {
        int count = getComponentCount();
        _buttons.setLayout(new GridLayout(1, count, 0, 0));
    }
}
