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
import java.util.*;
import java.util.List;
import javax.swing.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class LabeledComponent extends JComponent {
    private List actions = new ArrayList();
    private JComponent _header;
    private FakeToolBar _toolBar;
    private JLabel _label;
    private JComponent _headerComponentHolder;
    private JComponent _centerComponentHolder;
    private JComponent _footerComponentHolder;
    private boolean _isVerticallyStretchable;

    public LabeledComponent(String label, Component c) {
        this(label, c, false);
    }

    public LabeledComponent(String label, Component c, boolean verticallyStretchable) {
        setLayout(new BorderLayout());
        add(createHeader(), BorderLayout.NORTH);
        add(createCenterComponentHolder(), BorderLayout.CENTER);
        add(createFooterComponentHolder(), BorderLayout.SOUTH);

        _isVerticallyStretchable = verticallyStretchable || (c instanceof JScrollPane);
        setHeaderLabel(label);
        setCenterComponent(c);
    }

    public LabeledComponent(String label, JScrollPane c) {
        this(label, c, true);
    }

    public void addHeaderButton(Action action) {
        actions.add(action);
        ComponentFactory.addToolBarButton(_toolBar, action);
    }

    private JComponent createCenterComponentHolder() {
        _centerComponentHolder = new JPanel();
        _centerComponentHolder.setLayout(new BorderLayout());
        return _centerComponentHolder;
    }

    private JComponent createFooterComponentHolder() {
        _footerComponentHolder = new JPanel();
        _footerComponentHolder.setLayout(new BorderLayout());
        return _footerComponentHolder;
    }

    private JComponent createHeader() {
        _header = ComponentFactory.createButtonPreferredHeightPanel();
        _header.setLayout(new BorderLayout());
        _header.add(createHeaderLabel(), BorderLayout.WEST);
        _header.add(createHeaderComponentHolder(), BorderLayout.CENTER);
        _header.add(createHeaderToolbar(), BorderLayout.EAST);
        return _header;
    }

    private JComponent createHeaderComponentHolder() {
        _headerComponentHolder = new JPanel();
        _headerComponentHolder.setLayout(new BorderLayout());
        return _headerComponentHolder;
    }

    private JComponent createHeaderLabel() {
        _label = ComponentFactory.createLabel();
        _label.setBorder(BorderFactory.createEmptyBorder(0, 2, 0, 2));
        return _label;
    }

    private JComponent createHeaderToolbar() {
        _toolBar = ComponentFactory.createFakeToolBar();
        return _toolBar;
    }

    public Component getCenterComponent() {
        Component component;
        int count = _centerComponentHolder.getComponentCount();
        if (count == 0) {
            component = null;
        } else {
            component = _centerComponentHolder.getComponent(0);
        }
        return component;
    }

    public Component getFooterComponent() {
        int count = _footerComponentHolder.getComponentCount();
        return (count == 0) ? (Component) null : _footerComponentHolder.getComponent(0);
    }

    public Collection getHeaderButtonActions() {
        return Collections.unmodifiableCollection(actions);
    }

    public Collection getHeaderButtons() {
        return _toolBar.getButtons();
    }

    public Component getHeaderComponent() {
        int count = _headerComponentHolder.getComponentCount();
        return (count == 0) ? (Component) null : _headerComponentHolder.getComponent(0);
    }

    public String getHeaderLabel() {
        return _label.getText();
    }

    public boolean isVerticallyStretchable() {
        return _isVerticallyStretchable;
    }

    public void removeHeaderButton(int index) {
        _toolBar.removeButton(index);
    }

    public void setCenterComponent(Component c) {
        _centerComponentHolder.removeAll();
        if (c != null) {
            String location = (_isVerticallyStretchable) ? BorderLayout.CENTER : BorderLayout.NORTH;
            _centerComponentHolder.add(c, location);
        }
        revalidate();
        repaint();
    }

    public void setFooterComponent(JComponent c) {
        _footerComponentHolder.removeAll();
        if (c != null) {
            _footerComponentHolder.add(c);
        }
    }

    public void setHeaderComponent(JComponent component) {
        _headerComponentHolder.removeAll();
        if (component != null) {
            _headerComponentHolder.add(component);
        }
    }

    public void setHeaderLabel(String label) {
        _label.setText(label);
    }

    public void setVerticallyStretchable(boolean b) {
        if (b != _isVerticallyStretchable) {
            _isVerticallyStretchable = b;
            setCenterComponent(getCenterComponent());
        }
    }
}
