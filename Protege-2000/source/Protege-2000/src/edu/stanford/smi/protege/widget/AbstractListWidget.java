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


import java.awt.dnd.*;
import java.awt.datatransfer.*;
import java.util.*;
import javax.swing.*;
import javax.swing.event.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protege.util.*;

/**
 * Common functionality for widgets which are composed of a list box in a labeled component
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public abstract class AbstractListWidget extends AbstractSlotWidget {
    private JList _list;
    private LabeledComponent _labeledComponent;
    private SwitchableListSelectionListener _listListener = new ListSelectionListenerAdapter(this);

    public AbstractListWidget() {
        setPreferredColumns(2);
        setPreferredRows(2);
    }

    public void addButton(Action action) {
        addButton(action, true);
    }

    public void addButton(Action action, boolean defaultState) {
        if (action != null) {
            addButtonConfiguration(action, defaultState);
            if (displayButton(action)) {
                _labeledComponent.addHeaderButton(action);
            }
        }
    }

    public void addItem(Object o) {
        ComponentUtilities.addListValue(_list, o);
    }

    public void addItems(Collection items) {
        ComponentUtilities.addListValues(_list, items);
    }

    public boolean contains(Object o) {
        return ComponentUtilities.listValuesContain(_list, o);
    }

    private JComponent createLabeledComponent(Action action) {
        _list = ComponentFactory.createList(action, true);
        _list.getModel().addListDataListener(new ListDataListener() {
            public void contentsChanged(ListDataEvent event) {
                valueChanged();
            }

            public void intervalAdded(ListDataEvent event) {
                valueChanged();
            }

            public void intervalRemoved(ListDataEvent event) {
                valueChanged();
            }
        });
        _list.setCellRenderer(FrameRenderer.createInstance());
        _list.addListSelectionListener(_listListener);
        _labeledComponent = new LabeledComponent(getLabel(), new JScrollPane(_list));
        return _labeledComponent;
    }

    protected LabeledComponent getLabeledComponent() {
        return _labeledComponent;
    }

    public Collection getSelection() {
        return ComponentUtilities.getSelection(_list);
    }

    public Collection getValues() {
        return ComponentUtilities.getListValues(_list);
    }

    public void initialize() {
        initialize(getDoubleClickAction());
    }

    public void initialize(Action action) {
        add(createLabeledComponent(action));
    }

    public void removeAllItems() {
        ComponentUtilities.clearListValues(_list);
    }

    public void removeItem(Object o) {
        ComponentUtilities.removeListValue(_list, o);
        // notifySelectionListeners();        // workaround for JDK1.2 ListSelectionModel bug
    }

    public void removeItems(Collection items) {
        ComponentUtilities.removeListValues(_list, items);
        // notifySelectionListeners();     // workaround for JDK1.2 ListSelectionModel bug
    }

    public void replaceItem(Object oldItem, Object newItem) {
        ComponentUtilities.replaceListValue(_list, oldItem, newItem);
    }

    public void setEditable(boolean b) {
        Iterator i = _labeledComponent.getHeaderButtonActions().iterator();
        while (i.hasNext()) {
            Action action = (Action) i.next();
            if (action instanceof CreateAction || action instanceof AddAction ||
                    action instanceof RemoveAction || action instanceof DeleteAction) {
                ((AllowableAction)action).setAllowed(b);
            }

        }
    }

    public void setInstance(Instance instance) {
        super.setInstance(instance);
        _list.setCellRenderer(new OwnSlotValueFrameRenderer(instance, getSlot()));
    }

    public void setRenderer(ListCellRenderer renderer) {
        _list.setCellRenderer(renderer);
    }

    public void setSelection(Object o) {
        _list.setSelectedValue(o, true);
    }

    public void setValues(Collection values) {
        ComponentUtilities.setListValues(_list, values);
    }
}
