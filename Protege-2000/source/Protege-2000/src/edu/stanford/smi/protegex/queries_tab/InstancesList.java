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

package edu.stanford.smi.protegex.queries_tab;

import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.protege.widget.*;
import edu.stanford.smi.protege.action.*;
import edu.stanford.smi.protege.event.*;
import edu.stanford.smi.protege.ui.*;
import java.awt.*;
import java.util.*;
import javax.swing.*;
import javax.swing.event.*;

public class InstancesList extends SelectableContainer implements Disposable {
    private Collection itsClses = Collections.EMPTY_LIST;
    private SelectableList itsList;
    private Project itsProject;
    private AllowableAction itsCreateAction;
    private AllowableAction itsDeleteAction;

  private LabeledComponent c;

    private ClsListener itsClsListener = new ClsAdapter() {
        public void directInstanceCreated(ClsEvent event) {
            // Log.enter(this, "directInstanceCreated", event);
            super.directInstanceCreated(event);
            Instance instance = event.getInstance();
            ComponentUtilities.addListValue(itsList, instance);
            instance.addFrameListener(itsInstanceFrameListener);
        }
        public void directInstanceDeleted(ClsEvent event) {
            super.directInstanceDeleted(event);
            removeInstance(event.getInstance());
        }
    };
    private FrameListener itsClsFrameListener = new FrameAdapter() {
        public void ownSlotValueChanged(FrameEvent event) {
            super.ownSlotValueChanged(event);
            updateCreateButton((Cls)event.getFrame());
        }
    };
    private FrameListener itsInstanceFrameListener = new FrameAdapter() {
        public void browserTextChanged(FrameEvent event) {
            super.browserTextChanged(event);
            repaint();
        }
    };

    public InstancesList(Project project) {
        itsProject = project;
        Action viewAction = createViewAction();
        itsList = ComponentFactory.createSelectableList(viewAction, true);
        QueriesTabRenderer itsRenderer = new QueriesTabRenderer();
        itsRenderer.setDisplayStringType(true);
        itsList.setCellRenderer(itsRenderer);

        itsList.addListSelectionListener(new ListSelectionListener() {
            public void valueChanged(ListSelectionEvent event) {
                notifySelectionListeners();
            }
        });
        setLayout(new BorderLayout());
        c = new LabeledComponent("Search Results", new JScrollPane(itsList));
        c.addHeaderButton(viewAction);
        c.addHeaderButton(createReferencersAction());
        c.setFooterComponent(new ListFinder(itsList, "Find Instance"));
        add(c);
        setSelectable(itsList);
    }

    private void addListeners() {

        Iterator i = itsClses.iterator();
        while (i.hasNext()) {
            Cls cls = (Cls) i.next();
            cls.addClsListener(itsClsListener);
            cls.addFrameListener(itsClsFrameListener);
        }
        i = getModel().getValues().iterator();
        while (i.hasNext()) {

            Instance instance = (Instance) i.next();
            instance.addFrameListener(itsInstanceFrameListener);
        }

    }

    private Action createCreateAction() {
        itsCreateAction = new CreateAction("Create Instance") {
            public void onCreate() {
                Cls cls = (Cls) CollectionUtilities.getFirstItem(itsClses);
                if (cls != null) {
                    KnowledgeBase kb = itsProject.getKnowledgeBase();
                    Instance instance = kb.createInstance(null, cls);
                    if (instance instanceof Cls) {
                        Cls newCls = (Cls) instance;
                        newCls.addDirectSuperclass(kb.getRootCls());
                    }
                    itsList.setSelectedValue(instance, true);
                }
            }
        };
        return itsCreateAction;
    }

    private Action createDeleteAction() {
        itsDeleteAction = new DeleteInstancesAction("Delete Selected Instances", this);
        return itsDeleteAction;
    }

    private Action createReferencersAction() {
        //return new ReferencersAction(itsProject, this);
        return new ReferencersAction(this);
    }

    private Action createViewAction() {
        return new ViewAction("View Instance", this) {
            public void onView(Object o) {
                itsProject.show((Instance) o);
            }
        };
    }

    public void dispose() {
        //removeListeners();
    }

    public LabeledComponent getLabeledComponent() {
        return c;
    }

    private SimpleListModel getModel() {
        return (SimpleListModel) itsList.getModel();
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
        ComponentUtilities.setDragAndDropEnabled(itsList, editable);
    }

    private void removeInstance(Instance instance) {
        ComponentUtilities.removeListValue(itsList, instance);
        if (itsInstanceFrameListener != null)
            instance.removeFrameListener(itsInstanceFrameListener);
    }

    private void removeListeners() {
        Iterator i = itsClses.iterator();
        while (i.hasNext()) {
            Cls cls = (Cls) i.next();

            if (itsClsListener != null)
                cls.removeClsListener(itsClsListener);
            if (itsClsFrameListener != null)
                cls.removeFrameListener(itsClsFrameListener);
        }
        i = getModel().getValues().iterator();
        while (i.hasNext()) {
            Instance instance = (Instance) i.next();
            if (itsInstanceFrameListener != null)
                instance.removeFrameListener(itsInstanceFrameListener);
        }
    }

    public void setClses(Collection clses) {
        if (clses == null)
            return;
        removeListeners();
        itsClses = new ArrayList(clses);
        ArrayList instances = new ArrayList();
        Iterator i = itsClses.iterator();
        while (i.hasNext()) {
            Cls cls = (Cls) i.next();
            instances.addAll(cls.getDirectInstances());
        }
        Collections.sort(instances, new FrameComparator());
        getModel().setValues(instances);
        if (getModel().getSize() > 0) {
            itsList.setSelectedIndex(0);
        }
        updateCreateButton((clses.size() == 1) ? (Cls) CollectionUtilities.getFirstItem(clses) : null);
        addListeners();
        notifySelectionListeners();
    }

    // This is the part of code which is different from directInstancesList

    public void setInstances(Collection instances) {
        if (instances == null) {
            getModel().setValues(new ArrayList());
            return;
        }
        getModel().setValues(instances);
    }

    private void updateCreateButton(Cls cls) {
        itsCreateAction.setEnabled(cls == null ? false : cls.isConcrete());
    }
}
