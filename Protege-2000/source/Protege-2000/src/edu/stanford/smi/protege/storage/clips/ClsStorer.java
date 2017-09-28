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

package edu.stanford.smi.protege.storage.clips;


import java.io.*;
import java.util.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Store a set of classes in Clips ontology file format Clips requires that
 *  clses be stored so that there are no forward references so we start at root
 *  and descendend the class hierarchy writing out classes whose parents have
 *  already been written out.
 *
 * @author Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class ClsStorer extends ClipsFileWriter {
    private static Map _typeStrings = new HashMap(); // <valueType, String>
    private Collection _storedClses = new HashSet();

    static {
        _typeStrings.put(ValueType.ANY, "ANY");
        _typeStrings.put(ValueType.BOOLEAN, "SYMBOL");
        _typeStrings.put(ValueType.CLS, "SYMBOL");
        _typeStrings.put(ValueType.FLOAT, "FLOAT");
        _typeStrings.put(ValueType.INSTANCE, "INSTANCE");
        _typeStrings.put(ValueType.INTEGER, "INTEGER");
        _typeStrings.put(ValueType.STRING, "STRING");
        _typeStrings.put(ValueType.SYMBOL, "SYMBOL");
    }

    private Collection _errors;

    public ClsStorer(Writer writer) {
        super(writer);
    }

    private boolean isStorable(Cls cls) {
        return !cls.isSystem() && !cls.isIncluded();
    }

    private void storeAccessorFacet() {
        println();
        print("\t\t(create-accessor read-write)");
    }

    private void storeAllowedClsesFacet(Cls cls, Slot slot) {
        ValueType type = cls.getTemplateSlotValueType(slot);
        if (type == ValueType.INSTANCE) {
            Collection clses = cls.getTemplateSlotAllowedClses(slot);
            storeCollectionFacet("allowed-classes", clses, true, ValueType.CLS, true);
        }
    }

    private void storeAllowedParentsFacet(Cls cls, Slot slot) {
        ValueType type = cls.getTemplateSlotValueType(slot);
        if (type == ValueType.CLS) {
            Collection clses = cls.getTemplateSlotAllowedParents(slot);
            storeCollectionFacet("allowed-parents", clses, true, type, true);
        }
    }

    private void storeAllowedValuesFacet(Cls cls, Slot slot) {
        ValueType type = cls.getTemplateSlotValueType(slot);
        Collection values = null;
        if (type == ValueType.BOOLEAN) {
            values = new ArrayList();
            values.add(Boolean.FALSE);
            values.add(Boolean.TRUE);
        } else if (type == ValueType.SYMBOL) {
            values = cls.getTemplateSlotAllowedValues(slot);
        }
        storeCollectionFacet("allowed-values", values, false, type, false);
    }

    private void storeAssociatedFacet(Slot slot) {
        Facet facet = slot.getAssociatedFacet();
        if (facet != null) {
            println();
            print(";+\t\t(associated-facet ");
            printFrame(facet);
            print(")");
        }
    }

    private void storeCardinalityFacet(Cls cls, Slot slot, boolean allowsMultiple) {
        int min = cls.getTemplateSlotMinimumCardinality(slot);
        int max = cls.getTemplateSlotMaximumCardinality(slot);
        if (min != 0 || max != KnowledgeBase.MAXIMUM_CARDINALITY_UNBOUNDED) {
            println();
            if (!allowsMultiple) {
                print(";+");
            }
            print("\t\t(cardinality ");
            print(min);
            print(" ");
            if (max == KnowledgeBase.MAXIMUM_CARDINALITY_UNBOUNDED) {
                print("?VARIABLE");
            } else {
                print(max);
            }
            print(")");
        }
    }

    private void storeCls(Cls cls) {
        // Log.enter(this, "storeCls", cls);
        try {
            if (isStorable(cls)) {
                println();
                println();
                print("(defclass ");
                printFrame(cls);

                storeComment(cls);
                storeSuperclasses(cls);
                storeRole(cls);
                storeSlots(cls);
                print(")");
            }
        } catch (Exception e) {
            Log.exception(e, this, "storeCls", cls);
            _errors.add(e);
        }
    }

    private void storeClsAndSubclasses(Cls cls) {
        _storedClses.add(cls);
        storeCls(cls);
        storeSubclasses(cls);
    }

    public void storeClses(KnowledgeBase kb, Collection errors) {
        _errors = errors;
        storeTopLevelSlots(kb);
        Cls root = kb.getRootCls();
        _storedClses.add(root);
        storeSubclasses(root);
        flush();
        if (!printSucceeded()) {
            errors.add("Store classes failed");
        }
    }

    private void storeCollectionFacet(String name, Collection c, boolean isExtension,
            ValueType type, boolean storeIfEmpty) {
        if (c != null && (storeIfEmpty || !c.isEmpty())) {
            println();
            if (isExtension) {
                print(";+");
            }
            print("\t\t(");
            print(name);
            Iterator i = c.iterator();
            while (i.hasNext()) {
                print(" ");
                Object o = i.next();
                String s;
                if (o instanceof Cls || o instanceof Slot || o instanceof Facet) {
                    s = ((Instance) o).getName();
                } else if (o instanceof Instance) {
                    s = "[" + ((Instance) o).getName() + "]";
                } else if (o instanceof Boolean) {
                    s = ((Boolean) o).booleanValue() ? ClipsUtil.TRUE : ClipsUtil.FALSE;
                } else {
                    s = o.toString();
                }
                if (type == ValueType.STRING) {
                    s = ClipsUtil.toExternalString(s);
                } else {
                    s = ClipsUtil.toExternalSymbol(s);
                }
                print(s);
            }
            print(")");
        }
    }

    private void storeComment(Cls cls) {
        String text = (String) CollectionUtilities.getFirstItem(cls.getDocumentation());
        if (text != null) {
            print(" ");
            print(ClipsUtil.toExternalString(text));
        }
    }

    private void storeComment(Cls cls, Slot slot) {
        Iterator i = cls.getTemplateSlotDocumentation(slot).iterator();
        if (i.hasNext()) {
            String text = (String) i.next();
            println();
            print(";+\t\t(comment ");
            print(ClipsUtil.toExternalString(text));
            print(")");
        }
    }

    private void storeDefaultValueFacet(Cls cls, Slot slot) {
        ValueType type = cls.getTemplateSlotValueType(slot);
        storeCollectionFacet("default", cls.getTemplateSlotDefaultValues(slot), false, type, false);
    }

    private void storeInverseProperty(Slot slot) {
        Slot inverseSlot = slot.getInverseSlot();
        if (inverseSlot != null) {
            println();
            print(";+\t\t(inverse-slot ");
            printFrame(inverseSlot);
            print(")");

        }
    }

    private void storeRangeFacet(Cls cls, Slot slot) {
        ValueType type = cls.getTemplateSlotValueType(slot);
        if (type == ValueType.INTEGER || type == ValueType.FLOAT) {
            Number minValue = cls.getTemplateSlotMinimumValue(slot);
            Number maxValue = cls.getTemplateSlotMaximumValue(slot);

            if (minValue != null || maxValue != null) {
                Collection c = new ArrayList();
                if (type == ValueType.INTEGER) {
                    minValue = (minValue == null) ? (Number) null : new Integer(minValue.intValue());
                    maxValue = (maxValue == null) ? (Number) null : new Integer(maxValue.intValue());
                }
                c.add((minValue == null) ? "?VARIABLE" : minValue.toString());
                c.add((maxValue == null) ? "?VARIABLE" : maxValue.toString());
                storeCollectionFacet("range", c, false, type, false);
            }
        }
    }

    private void storeRole(Cls cls) {
        println();
        print("\t(role ");
        String text = cls.isAbstract() ? "abstract" : "concrete";
        print(text);
        print(")");
    }

    private void storeSlot(Cls cls, Slot slot) {
        try {
            println();
            boolean allowsMultiple = cls.getTemplateSlotAllowsMultipleValues(slot);
            if (allowsMultiple) {
                print("\t(multislot ");
            } else {
                print("\t(single-slot ");
            }
            String name = slot.getName();
            if (name.equals("name")) {
                print("name_");
            } else if (name.equals("is-a")) {
                print("is-a_");
            } else {
                printFrame(slot);
            }

            storeSlotDocumentation(cls, slot);
            storeTypeFacet(cls, slot);
            storeAllowedValuesFacet(cls, slot);
            storeAllowedParentsFacet(cls, slot);
            storeAllowedClsesFacet(cls, slot);
            storeRangeFacet(cls, slot);
            storeDefaultValueFacet(cls, slot);
            storeValueFacet(cls, slot);
            storeCardinalityFacet(cls, slot, allowsMultiple);
            if (cls.getName().equals(ClipsUtil.TOP_LEVEL_SLOT_CLASS)) {
                storeInverseProperty(slot);
                storeSuperslotProperty(slot);
                storeAssociatedFacet(slot);
            }
            storeUserFacets(cls, slot);
            storeAccessorFacet();
            print(")");
        } catch (Exception e) {
            Log.exception(e, this, "storeSlot", cls, slot);
            _errors.add(e);
        }
    }

    private void storeSlotDocumentation(Cls cls, Slot slot) {
        String text = (String) CollectionUtilities.getFirstItem(cls.getTemplateSlotDocumentation(slot));
        if (text != null) {
            println();
            print(";+\t\t(comment ");
            print(ClipsUtil.toExternalString(text));
            print(")");
        }
    }

    private void storeSlots(Cls cls) {
        Iterator i = cls.getTemplateSlots().iterator();
        while (i.hasNext()) {
            Slot slot = (Slot) i.next();
            if (cls.hasDirectTemplateSlot(slot) || cls.hasDirectlyOverriddenTemplateSlot(slot)) {
                storeSlot(cls, slot);
            }
        }
    }

    private void storeSubclasses(Cls cls) {
        Iterator i = cls.getDirectSubclasses().iterator();
        while (i.hasNext()) {
            Cls subclass = (Cls) i.next();
            if (!_storedClses.contains(subclass)) {
                if (superclassesStored(subclass)) {
                    storeClsAndSubclasses(subclass);
                }
            }
        }
    }

    private void storeSuperclasses(Cls cls) {
        Cls rootCls = cls.getKnowledgeBase().getRootCls();
        println();
        print("\t(is-a");
        Iterator i = cls.getDirectSuperclasses().iterator();
        while (i.hasNext()) {
            Cls superclass = (Cls) i.next();
            if (superclass == rootCls) {
                print(" USER");
            } else {
                print(" ");
                printFrame(superclass);
            }
        }
        print(")");
    }

    private void storeSuperslotProperty(Slot slot) {
        Collection superslots = slot.getDirectSuperslots();
        if (!superslots.isEmpty()) {
            println();
            print(";+\t\t(subslot-of ");
            boolean isFirst = true;
            Iterator i = superslots.iterator();
            while (i.hasNext()) {
                if (isFirst) {
                    isFirst = false;
                } else {
                    print(" ");
                }
                Slot superslot = (Slot) i.next();
                printFrame(superslot);
            }
            print(")");

        }
    }

    /**
     *  Top level slots are stored as slots on a fake class
     *
     * @param  kb  Description of Parameter
     */
    private void storeTopLevelSlots(KnowledgeBase kb) {
        Cls cls = kb.createCls(ClipsUtil.TOP_LEVEL_SLOT_CLASS, CollectionUtilities.createCollection(kb.getRootCls()));
        Iterator i = kb.getSlots().iterator();
        while (i.hasNext()) {
            Slot slot = (Slot) i.next();
            if (!slot.isIncluded()) {
                cls.addDirectTemplateSlot(slot);
            }
        }
        storeCls(cls);
        kb.deleteCls(cls);
    }

    private void storeTypeFacet(Cls cls, Slot slot) {
        println();
        ValueType type = cls.getTemplateSlotValueType(slot);
        if (type == ValueType.ANY) {
            print(";+");
        }
        print("\t\t(type ");
        String typeName = (String) _typeStrings.get(type);
        print(typeName);
        print(")");
    }

    private void storeUserFacet(Cls cls, Slot slot, Facet facet) {
        if (cls.hasDirectlyOverriddenTemplateFacet(slot, facet)) {
            Collection values = cls.getDirectTemplateFacetValues(slot, facet);
            ValueType type = facet.getValueType();
            storeCollectionFacet("user-facet " + facet.getName(), values, true, type, true);
        }
    }

    private void storeUserFacets(Cls cls, Slot slot) {
        Iterator i = cls.getTemplateFacets(slot).iterator();
        while (i.hasNext()) {
            Facet facet = (Facet) i.next();
            if (!facet.isSystem()) {
                storeUserFacet(cls, slot, facet);
            }
        }
    }

    private void storeValueFacet(Cls cls, Slot slot) {
        ValueType type = cls.getTemplateSlotValueType(slot);
        storeCollectionFacet("value", cls.getTemplateSlotValues(slot), true, type, false);
    }

    private boolean superclassesStored(Cls cls) {
        boolean superclassesStored = true;
        Iterator i = cls.getDirectSuperclasses().iterator();
        while (i.hasNext()) {
            Cls superclass = (Cls) i.next();
            if (!_storedClses.contains(superclass)) {
                superclassesStored = false;
                break;
            }
        }
        return superclassesStored;
    }
}
