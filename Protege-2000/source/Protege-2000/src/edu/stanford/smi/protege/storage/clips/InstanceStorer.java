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
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.protege.ui.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class InstanceStorer extends ClipsFileWriter {
    private Collection _errors;
    private Map _clsToSlotsMap = new HashMap();
    private Slot _constraintsSlot;
    private Set _slotNamesToNotSave = new HashSet();

    {
        _slotNamesToNotSave.add(Model.Slot.NAME);
        _slotNamesToNotSave.add(Model.Slot.DIRECT_TYPE);
        _slotNamesToNotSave.add(Model.Slot.DIRECT_INSTANCES);
        _slotNamesToNotSave.add(Model.Slot.DIRECT_SUPERCLASSES);
        _slotNamesToNotSave.add(Model.Slot.DIRECT_SUBCLASSES);
        _slotNamesToNotSave.add(Model.Slot.DIRECT_TEMPLATE_SLOTS);
        _slotNamesToNotSave.add(Model.Slot.DOCUMENTATION);
        _slotNamesToNotSave.add(Model.Slot.ROLE);
        _slotNamesToNotSave.add(Model.Slot.VALUE_TYPE);
        _slotNamesToNotSave.add(Model.Slot.MINIMUM_CARDINALITY);
        _slotNamesToNotSave.add(Model.Slot.MAXIMUM_CARDINALITY);
        _slotNamesToNotSave.add(Model.Slot.NUMERIC_MINIMUM);
        _slotNamesToNotSave.add(Model.Slot.NUMERIC_MAXIMUM);
        _slotNamesToNotSave.add(Model.Slot.DEFAULTS);
        _slotNamesToNotSave.add(Model.Slot.VALUES);
        _slotNamesToNotSave.add(Model.Slot.INVERSE);
        _slotNamesToNotSave.add(Model.Slot.DIRECT_SUBSLOTS);
        _slotNamesToNotSave.add(Model.Slot.DIRECT_SUPERSLOTS);
    }

    public InstanceStorer(Writer writer) {
        super(writer);
    }

    private Collection anyToStrings(Collection anyValues) {
        Collection strings = new ArrayList();
        Iterator i = anyValues.iterator();
        while (i.hasNext()) {
            String value;
            Object o = i.next();
            if (o instanceof Frame) {
                value = "[" + toExternalFrameName((Frame) o) + "]";
            } else if (o instanceof String) {
                value = ClipsUtil.toExternalString((String) o);
            } else if (o instanceof Boolean) {
                value = ((Boolean) o).booleanValue() ? ClipsUtil.TRUE : ClipsUtil.FALSE;
            } else {
                value = o.toString();
            }
            strings.add(value);
        }
        return strings;
    }

    private Collection booleansToStrings(Collection booleans) {
        Collection strings = new ArrayList();
        Iterator i = booleans.iterator();
        while (i.hasNext()) {
            Boolean b = (Boolean) i.next();
            String s = (b.booleanValue()) ? ClipsUtil.TRUE : ClipsUtil.FALSE;
            strings.add(s);
        }
        return strings;
    }

    private Collection clsesToStrings(Collection clses) {
        Collection strings = new ArrayList();
        Iterator i = clses.iterator();
        while (i.hasNext()) {
            Object o = i.next();
            if (o instanceof Cls) {
                Cls cls = (Cls) o;
                strings.add(toExternalFrameName(cls));
            } else {
                Log.error("not a class", this, "clsesToStrings", o);
            }
        }
        return strings;
    }

    private Collection getSlots(Instance instance) {
        Cls type = instance.getDirectType();
        List slots = (List) _clsToSlotsMap.get(type);
        if (slots == null) {
            slots = new ArrayList(type.getTemplateSlots());
            _clsToSlotsMap.put(type, slots);
        }
        return slots;
    }

    private boolean hasConstraintSlotValues(Instance instance) {
        if (_constraintsSlot == null) {
            _constraintsSlot = (Slot) instance.getKnowledgeBase().getFrame(Model.Slot.CONSTRAINTS);
        }
        return instance.getOwnSlotValueCount(_constraintsSlot) != 0;
    }

    private Collection instancesToStrings(Collection instances) {
        Collection strings = new ArrayList();
        Iterator i = instances.iterator();
        while (i.hasNext()) {
            Instance instance = (Instance) i.next();
            strings.add("[" + toExternalFrameName(instance) + "]");
        }
        return strings;
    }

    private Collection internalToExternalStrings(Collection internalStrings) {
        Collection externalStrings = new ArrayList();
        Iterator i = internalStrings.iterator();
        while (i.hasNext()) {
            String s = (String) i.next();
            if (s.length() > 0) {
                externalStrings.add(ClipsUtil.toExternalString(s));
            }
        }
        return externalStrings;
    }

    private Collection internalToExternalSymbols(Collection internalSymbols) {
        Collection externalStrings = new ArrayList();
        Iterator i = internalSymbols.iterator();
        while (i.hasNext()) {
            String s = (String) i.next();
            if (s.length() > 0) {
                externalStrings.add(ClipsUtil.toExternalSymbol(s));
            }
        }
        return externalStrings;
    }

    private boolean isStandardClsSlot(Instance instance) {
        Cls type = instance.getDirectType();
        FrameID typeID = type.getFrameID();
        return typeID == Model.Cls.ID.STANDARD_CLASS || typeID == Model.Cls.ID.STANDARD_SLOT;
    }

    private boolean isStorableInstance(Instance instance) {
        boolean isStorable;
        if (instance.getDirectType() == null) {
            isStorable = false;
            Log.error("null type", this, "isStorableInstance", instance);
        } else if (isStandardClsSlot(instance)) {
            isStorable = hasConstraintSlotValues(instance);
        } else {
            isStorable = !instance.isIncluded();
        }
        return isStorable;
    }

    private boolean isStorableSlot(Instance instance, Slot slot) {
        return !_slotNamesToNotSave.contains(slot.getName());
    }

    private void printValues(Slot slot, Collection values, boolean onePerLine, boolean allowsMultiple) {
        println();
        print("\t(");
        printFrame(slot);
        if (values.size() == 1 || !allowsMultiple) {
            print(" ");
            print(values.iterator().next().toString());
            print(")");
        } else {
            Iterator i = values.iterator();
            while (i.hasNext()) {
                if (onePerLine) {
                    println();
                    print("\t\t");
                } else {
                    print(" ");
                }
                String value = i.next().toString();
                print(value);
            }
            print(")");
        }
    }

    private void storeInstance(Instance instance) {
        // Log.enter(this, "storeInstance", instance);
        try {
            if (isStorableInstance(instance)) {
                println();
                print("([");
                printFrame(instance);
                print("] of ");
                printFrame(instance.getDirectType());
                storeSlotValues(instance);
                println(")");
            }
        } catch (Exception e) {
            Log.exception(e, this, "storeInstance", instance);
            _errors.add(e);
        }
    }

    public void storeInstances(KnowledgeBase kb, Collection errors) {
        _errors = errors;
        List instances = new ArrayList(kb.getInstances());
        // Log.trace("instance=" + instances, this, "storeInstances");
        Collections.sort(instances, new FrameNameComparator());
        Iterator i = instances.iterator();
        while (i.hasNext()) {
            Instance instance = (Instance) i.next();
            storeInstance(instance);
        }
        flush();
        if (!printSucceeded()) {
            errors.add("Store instances failed");
        }
    }

    private void storeSlotValue(Instance instance, Slot slot, boolean multiple) {
        try {
            // Log.enter(this, "storeSlotValue", instance, slot);
            Collection values = instance.getOwnSlotValues(slot);
            if (!values.isEmpty()) {
                boolean onePerLine = false;
                ValueType type = instance.getOwnSlotValueType(slot);
                if (type == ValueType.BOOLEAN) {
                    values = booleansToStrings(values);
                } else if (type == ValueType.INSTANCE) {
                    values = instancesToStrings(values);
                    onePerLine = true;
                } else if (type == ValueType.CLS) {
                    values = clsesToStrings(values);
                    onePerLine = true;
                } else if (type == ValueType.STRING) {
                    values = internalToExternalStrings(values);
                    onePerLine = true;
                } else if (type == ValueType.SYMBOL) {
                    values = internalToExternalSymbols(values);
                } else if (type == ValueType.ANY) {
                    values = anyToStrings(values);
                }
                if (!values.isEmpty()) {
                    printValues(slot, values, onePerLine, multiple);
                }
            }
        } catch (Exception e) {
            Log.exception(e, this, "storeSlotValue", instance, slot);
            _errors.add(e);
        }
    }

    private void storeSlotValues(Instance instance) {
        Iterator i = getSlots(instance).iterator();
        while (i.hasNext()) {
            Slot slot = (Slot) i.next();
            if (isStorableSlot(instance, slot)) {
                // The "allowsMultiple" flag is a hack for the Protege feature of allowing multiple
                // values for a cardinality single slot.
                boolean allowsMultiple = instance.getOwnSlotAllowsMultipleValues(slot);
                storeSlotValue(instance, slot, allowsMultiple);
            }
        }
    }
}
