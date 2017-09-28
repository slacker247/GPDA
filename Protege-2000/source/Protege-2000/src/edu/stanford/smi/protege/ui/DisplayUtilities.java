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
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class DisplayUtilities {

    public static String editString(final Component component, String label, Object startValue, final Validator validator) {
        final EditStringPanel panel = new EditStringPanel(startValue, label);

        ModalDialog.CloseCallback callback =
            new ModalDialog.CloseCallback() {
                public boolean canClose(int result) {
                    boolean canClose;
                    if (result == ModalDialog.OPTION_OK && validator != null) {
                        String text = panel.getText();
                        canClose = validator.isValid(text);
                        if (!canClose) {
                            ModalDialog.showMessageDialog(component, validator.getErrorMessage());
                        }
                    } else {
                        canClose = true;
                    }
                    return canClose;
                }
            }
        ;
        String endValue = null;
        int result = ModalDialog.showDialog(component, panel, label, ModalDialog.MODE_OK_CANCEL, callback);
        if (result == ModalDialog.OPTION_OK) {
            endValue = panel.getText();
        }
        return endValue;
    }

    private static Collection getFirstTwoConcreteClses(Collection allowedClses) {
        Collection concreteClses = new HashSet();
        getFirstTwoConcreteClses(allowedClses, concreteClses);
        return concreteClses;
    }

    private static void getFirstTwoConcreteClses(Collection allowedClses, Collection concreteClses) {
        Iterator i = allowedClses.iterator();
        while (i.hasNext() && concreteClses.size() != 2) {
            Cls cls = (Cls) i.next();
            if (cls.isConcrete()) {
                concreteClses.add(cls);
                if (concreteClses.size() == 2) {
                    break;
                }
            }
            getFirstTwoConcreteClses(cls.getDirectSubclasses(), concreteClses);
        }
    }

    public static boolean hasMultipleConcreteClses(Collection allowedClses) {
        return getFirstTwoConcreteClses(allowedClses).size() == 2;
    }

    private static boolean hasOneClass(Collection rootClses) {
        boolean hasOneClass;
        if (rootClses.size() == 1) {
            Cls cls = (Cls) CollectionUtilities.getFirstItem(rootClses);
            hasOneClass = cls.getDirectSubclassCount() == 0;
        } else {
            hasOneClass = false;
        }
        return hasOneClass;
    }

    public static Cls pickCls(Component component, Collection rootClses) {
        return pickCls(component, rootClses, "Select Class");
    }

    public static Cls pickCls(Component component, Collection rootClses, String label) {
        return (Cls) CollectionUtilities.getFirstItem(pickClses(component, rootClses, label));
    }

    public static Collection pickClses(Component component, KnowledgeBase kb) {
        return pickClses(component, kb, "Select Classes");
    }

    public static Collection pickClses(Component component, KnowledgeBase kb, String label) {
        Collection rootClses = CollectionUtilities.createCollection(kb.getRootCls());
        return pickClses(component, rootClses, label);
    }

    public static Collection pickClses(Component component, Collection rootClses) {
        return pickClses(component, rootClses, "Select Classes");
    }

    public static Collection pickClses(Component component, Collection rootClses, String label) {
        Collection clses;
        if (rootClses.isEmpty()) {
            clses = Collections.EMPTY_LIST;
        } else if (hasOneClass(rootClses)) {
            clses = rootClses;
        } else {
            SelectClsesPanel p = new SelectClsesPanel(rootClses);
            int result = ModalDialog.showDialog(component, p, label, ModalDialog.MODE_OK_CANCEL);
            if (result == ModalDialog.OPTION_OK) {
                clses = p.getSelection();
            } else {
                clses = Collections.EMPTY_LIST;
            }
        }
        return clses;
    }

    public static Cls pickConcreteCls(Component component, KnowledgeBase kb, String label) {
        Collection rootClses = CollectionUtilities.createCollection(kb.getRootCls());
        return pickConcreteCls(component, rootClses, label);
    }

    public static Cls pickConcreteCls(Component component, Collection allowedClses) {
        return pickConcreteCls(component, allowedClses, "Select Concrete Cls");
    }

    public static Cls pickConcreteCls(Component component, Collection allowedClses, String label) {
        Cls cls;
        Collection concreteClses = getFirstTwoConcreteClses(allowedClses);
        switch (concreteClses.size()) {
            case 0 :
                ModalDialog.showMessageDialog(component, "There are no concreate allowed classes");
                cls = null;
                break;
            case 1 :
                cls = (Cls) CollectionUtilities.getFirstItem(concreteClses);
                break;
            case 2 :
                cls = promptForConcreteCls(component, allowedClses, label);
                break;
            default :
                Assert.fail("bad size: " + concreteClses.size());
                cls = null;
                break;
        }
        return cls;
    }

    public static Cls pickForm(Component component, Project project) {
        return pickForm(component, project, "Select Prototype Form");
    }

    public static Cls pickForm(Component component, Project project, String label) {
        Cls cls;
        SelectClsesPanel p = new SelectClsesPanel(project.getKnowledgeBase(), new FormRenderer(project));
        int result = ModalDialog.showDialog(component, p, label, ModalDialog.MODE_OK_CANCEL);
        if (result == ModalDialog.OPTION_OK) {
            cls = (Cls) CollectionUtilities.getFirstItem(p.getSelection());
        } else {
            cls = null;
        }
        return cls;
    }

    public static Instance pickInstance(Component component, KnowledgeBase kb) {
        Collection allowedClses = Collections.singleton(kb.getRootCls());
        return pickInstance(component, allowedClses, "Select Instance");
    }

    public static Instance pickInstance(Component component, Collection allowedClses) {
        return pickInstance(component, allowedClses, "Select Instance");
    }

    public static Instance pickInstance(Component component, Collection allowedClses, String label) {
        return (Instance) CollectionUtilities.getFirstItem(pickInstances(component, allowedClses, label));
    }

    public static Instance pickInstanceFromCollection(
        Component component,
        Collection collection,
        int initialSelection,
        String label) {
        Instance instance;
        SelectInstanceFromCollectionPanel panel = new SelectInstanceFromCollectionPanel(collection, initialSelection);
        int result = ModalDialog.showDialog(component, panel, label, ModalDialog.MODE_OK_CANCEL);
        if (result == ModalDialog.OPTION_OK) {
            instance = panel.getSelection();
        } else {
            instance = null;
        }
        return instance;
    }

    public static Collection pickInstances(Component component, Collection allowedClses) {
        return pickInstances(component, allowedClses, "Select Instances");
    }

    public static Collection pickInstances(Component component, Collection allowedClses, String label) {
        Collection instances = Collections.EMPTY_LIST;
        if (!allowedClses.isEmpty()) {
            SelectInstancesPanel panel = new SelectInstancesPanel(allowedClses);
            int result = ModalDialog.showDialog(component, panel, label, ModalDialog.MODE_OK_CANCEL);
            if (result == ModalDialog.OPTION_OK) {
                instances = panel.getSelection();
            }
        }
        return instances;
    }

    public static Collection pickInstancesFromCollection(Component component, Collection instances, String label) {
        Collection selectedSlots = Collections.EMPTY_LIST;
        SelectInstancesFromCollectionPanel panel = new SelectInstancesFromCollectionPanel(instances);
        int result = ModalDialog.showDialog(component, panel, label, ModalDialog.MODE_OK_CANCEL);
        switch (result) {
            case ModalDialog.OPTION_OK :
                selectedSlots = panel.getSelection();
                break;
            case ModalDialog.OPTION_CANCEL :
                break;
            default :
                Assert.fail("bad result: " + result);
        }
        return selectedSlots;
    }

    public static Slot pickSlot(Component component, Collection slots) {
        return pickSlot(component, slots, "Select Slot");
    }

    public static Slot pickSlot(Component component, Collection slots, String label) {
        return (Slot) CollectionUtilities.getFirstItem(pickInstancesFromCollection(component, slots, label));
    }

    public static Collection pickSlots(Component component, Collection slots) {
        return pickInstancesFromCollection(component, slots, "Select Slots");
    }

    public static Collection pickSlots(Component component, Collection slots, String label) {
        return pickInstancesFromCollection(component, slots, label);
    }

    public static String pickSymbol(Component component, String label, String initialValue, Collection allowedValues) {
        String value = initialValue;
        PickSymbolPanel panel = new PickSymbolPanel(label, initialValue, allowedValues);
        int result = ModalDialog.showDialog(component, panel, label, ModalDialog.MODE_OK_CANCEL);
        if (result == ModalDialog.OPTION_OK) {
            value = panel.getSelectedValue();
        }
        return value;
    }

    private static Cls promptForConcreteCls(final Component component, Collection clses, final String label) {
        final SelectClsesPanel p = new SelectClsesPanel(clses);

        ModalDialog.CloseCallback callback =
            new ModalDialog.CloseCallback() {
                public boolean canClose(int result) {
                    boolean canClose;
                    if (result == ModalDialog.OPTION_OK) {
                        Cls cls = (Cls) CollectionUtilities.getFirstItem(p.getSelection());
                        canClose = cls != null && cls.isConcrete();
                        if (!canClose) {
                            ModalDialog.showMessageDialog(component, label);
                        }
                    } else {
                        canClose = true;
                    }
                    return canClose;
                }
            }
        ;

        Cls cls;
        int result = ModalDialog.showDialog(component, p, label, ModalDialog.MODE_OK_CANCEL, callback);
        if (result == ModalDialog.OPTION_OK) {
            cls = (Cls) CollectionUtilities.getFirstItem(p.getSelection());
        } else {
            cls = null;
        }
        return cls;
    }
}
