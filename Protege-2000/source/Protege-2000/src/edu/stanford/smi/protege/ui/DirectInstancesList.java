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
import javax.swing.*;
import javax.swing.event.*;
import edu.stanford.smi.protege.action.*;
import edu.stanford.smi.protege.event.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class DirectInstancesList extends SelectableContainer implements Disposable {
    private Collection _clses = Collections.EMPTY_LIST;
    private SelectableList _list;
    private Project _project;
    private AllowableAction _createAction;
    private AllowableAction _deleteAction;

    private ClsListener _clsListener = new ClsAdapter() {
        public void directInstanceCreated(ClsEvent event) {
                // Log.enter(this, "directInstanceCreated", event);
    super.directInstanceCreated(event);
            Instance instance = event.getInstance();
            ComponentUtilities.addListValue(_list, instance);
            instance.addFrameListener(_instanceFrameListener);
        }

        public void directInstanceDeleted(ClsEvent event) {
            super.directInstanceDeleted(event);
            removeInstance(event.getInstance());
        }
    };
    private FrameListener _clsFrameListener = new FrameAdapter() {
        public void ownSlotValueChanged(FrameEvent event) {
            super.ownSlotValueChanged(event);
            updateCreateButton((Cls) event.getFrame());
        }
    };
    private FrameListener _instanceFrameListener = new FrameAdapter() {
        public void browserTextChanged(FrameEvent event) {
            super.browserTextChanged(event);
            repaint();
        }
    };

    public DirectInstancesList(Project project) {
        _project = project;
        Action viewAction = createViewAction();
        // itsList = ComponentFactory.createSelectableList(viewAction, true);
        _list = ComponentFactory.createSelectableList(viewAction);
        _list.setCellRenderer(FrameRenderer.createInstance());
        _list.addListSelectionListener(new ListSelectionListener() {
            public void valueChanged(ListSelectionEvent event) {
                notifySelectionListeners();
            }
        });
        setLayout(new BorderLayout());
        LabeledComponent c = new LabeledComponent("Direct Instances", new JScrollPane(_list));
        c.addHeaderButton(viewAction);
        c.addHeaderButton(createCreateAction());
        c.addHeaderButton(createReferencersAction());
        c.addHeaderButton(createDeleteAction());
        c.setFooterComponent(new ListFinder(_list, "Find Instance"));
        add(c);
        setSelectable(_list);
    }

    private void addListeners() {
        Iterator i = _clses.iterator();
        while (i.hasNext()) {
            Cls cls = (Cls) i.next();
            cls.addClsListener(_clsListener);
            cls.addFrameListener(_clsFrameListener);
        }
        i = getModel().getValues().iterator();
        while (i.hasNext()) {
            Instance instance = (Instance) i.next();
            instance.addFrameListener(_instanceFrameListener);
        }
    }

    private Action createCreateAction() {
        _createAction =
            new CreateAction("Create Instance") {
                public void onCreate() {
                    Cls cls = (Cls) CollectionUtilities.getFirstItem(_clses);
                    if (cls != null) {
                        KnowledgeBase kb = _project.getKnowledgeBase();
                        Instance instance = kb.createInstance(null, cls);
                        if (instance instanceof Cls) {
                            Cls newCls = (Cls) instance;
                            newCls.addDirectSuperclass(kb.getRootCls());
                        }
                        _list.setSelectedValue(instance, true);
                    }
                }
            }
        ;
        return _createAction;
    }

    private Action createDeleteAction() {
        _deleteAction = new DeleteInstancesAction("Delete Selected Instances", this);
        return _deleteAction;
    }

    private Action createReferencersAction() {
        return new ReferencersAction(this);
    }

    private Action createViewAction() {
        return
            new ViewAction("View Instance", this) {
                public void onView(Object o) {
                    _project.show((Instance) o);
                }
            }
        ;
    }

    public void dispose() {
        removeListeners();
    }

    public JComponent getDragComponent() {
        return _list;
    }

    private SimpleListModel getModel() {
        return (SimpleListModel) _list.getModel();
    }

    public void initializeSelection() {
        if (_list.getModel().getSize() > 0) {
            _list.setSelectedIndex(0);
        }
    }

    private boolean isSelectionEditable() {
        boolean isEditable = true;
        Iterator i = getSelection().iterator();
        while (i.hasNext()) {
            Instance instance = (Instance) i.next();
            if (!instance.isEditable()) {
                isEditable = false;
                break;
            }
        }
        return isEditable;
    }

    public void onSelectionChange() {
        // Log.enter(this, "onSelectionChange");
        boolean editable = isSelectionEditable();
        ComponentUtilities.setDragAndDropEnabled(_list, editable);
    }

    private void removeInstance(Instance instance) {
        ComponentUtilities.removeListValue(_list, instance);
        instance.removeFrameListener(_instanceFrameListener);
    }

    private void removeListeners() {
        Iterator i = _clses.iterator();
        while (i.hasNext()) {
            Cls cls = (Cls) i.next();
            cls.removeClsListener(_clsListener);
            cls.removeFrameListener(_clsFrameListener);
        }
        i = getModel().getValues().iterator();
        while (i.hasNext()) {
            Instance instance = (Instance) i.next();
            instance.removeFrameListener(_instanceFrameListener);
        }
    }

    public void setClses(Collection newClses) {
        removeListeners();
        _clses = new ArrayList(newClses);
        ArrayList instances = new ArrayList();
        Iterator i = _clses.iterator();
        while (i.hasNext()) {
            Cls cls = (Cls) i.next();
            instances.addAll(cls.getDirectInstances());
        }
        Collections.sort(instances, new FrameComparator());
        getModel().setValues(instances);
        updateCreateButton((_clses.size() == 1) ? (Cls) CollectionUtilities.getFirstItem(_clses) : null);
        addListeners();
        notifySelectionListeners();
    }

    public void setSelectedInstance(Instance instance) {
        _list.setSelectedValue(instance, true);
    }

    private void updateCreateButton(Cls cls) {
        _createAction.setEnabled(cls == null ? false : cls.isConcrete());
    }
}
