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
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protege.util.*;

/**
 * Component for acquiring and displaying an instance of a class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class InstanceField extends SelectableContainer {
    private LabeledComponent _labeledComponent;
    private SelectableList _listComponent;
    private AllowableAction _viewAction;
    private AllowableAction _createAction;
    private AllowableAction _addAction;
    private AllowableAction _removeAction;
    private AllowableAction _deleteAction;

    private Collection _allowedClses;
    private Project _project;

    private FrameListener _instanceListener = new FrameAdapter() {
        public void browserTextChanged(FrameEvent event) {
            _listComponent.repaint();
        }
    };

    public InstanceField(String label, Collection allowedClses) {
        this._allowedClses = new ArrayList(allowedClses);

        _listComponent = ComponentFactory.createSingleItemList(null);
        setSelectable(_listComponent);
        _listComponent.setCellRenderer(FrameRenderer.createInstance());

        _labeledComponent = new LabeledComponent(label, _listComponent);

        setLayout(new BorderLayout());
        add(_labeledComponent);
    }

    public void createCreateInstanceAction() {
        _createAction = new CreateAction("Create Instance") {
            public void onCreate() {
                Cls cls = DisplayUtilities.pickConcreteCls(InstanceField.this, _allowedClses);
                if (cls != null) {
                    Instance instance = getKnowledgeBase().createInstance(null, cls);
                    _project.show(instance);
                    setInstance(instance);
                }
            }
        };
        _labeledComponent.addHeaderButton(_createAction);
    }

    public void createDeleteInstancesAction() {
        _deleteAction = new DeleteInstancesAction("Delete Instance", this);
        _labeledComponent.addHeaderButton(_deleteAction);
    }

    public void createRemoveInstanceAction() {
        _removeAction = new RemoveAction("Remove Instance", this) {
            public void onRemove(Object o) {
                setInstance(null);
            }
        };
        _labeledComponent.addHeaderButton(_removeAction);
    }

    public void createSelectInstanceAction() {
        _addAction = new AddAction("Add Instance") {
            public void onAdd() {
                Instance instance = DisplayUtilities.pickInstance(InstanceField.this, _allowedClses);
                if (instance != null) {
                    setInstance(instance);
                }
            }
        };
        _labeledComponent.addHeaderButton(_addAction);
    }

    public void createViewInstanceAction() {
        _viewAction = new ViewAction("View Instance", this) {
            public void onView(Object o) {
                _project.show((Instance) o);
            }
        };
        _labeledComponent.addHeaderButton(_viewAction);
        _listComponent.addMouseListener(new DoubleClickActionAdapter(_viewAction));
    }

    public void dispose() {
        super.dispose();
        Instance instance = getInstance();
        if (instance != null) {
            instance.removeFrameListener(_instanceListener);
        }
    }

    public Instance getInstance() {
        return (Instance) CollectionUtilities.getFirstItem(ComponentUtilities.getListValues(_listComponent));
    }

    private KnowledgeBase getKnowledgeBase() {
        return _project.getKnowledgeBase();
    }

    public void setEditable(boolean b) {
        _createAction.setAllowed(b);
        _addAction.setAllowed(b);
        _removeAction.setAllowed(b);
        _deleteAction.setAllowed(b);
    }

    public void setInstance(Instance instance) {
        Instance currentInstance = getInstance();
        if (currentInstance != null) {
            currentInstance.removeFrameListener(_instanceListener);
        }
        if (instance != null) {
            instance.addFrameListener(_instanceListener);
        }
        ComponentUtilities.setListValues(_listComponent, CollectionUtilities.createCollection(instance));
        notifySelectionListeners();
    }
}
