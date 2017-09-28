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

package edu.stanford.smi.protege.model;

import java.util.*;
import junit.framework.*;
import edu.stanford.smi.protege.event.*;
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.protege.storage.clips.*;

/**
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class DefaultKnowledgeBase_Test extends DefaultTestCase {
    public static class TestSimpleInstance extends DefaultSimpleInstance {
        public TestSimpleInstance(KnowledgeBase kb, FrameID id) {
            super(kb, id);
        }
    }

    public DefaultKnowledgeBase_Test(String s) {
        super(s);
    }

    public void _testSaveLoadTypeAnyValues() {
        Cls c = createCls();
        Slot slot = createMultiValuedSlot(ValueType.ANY);
        String slotName = slot.getName();
        c.addDirectTemplateSlot(slot);
        Instance instance = createInstance(c);
        Collection originalValues = new ArrayList();
        originalValues.add(new String("foo"));
        originalValues.add(Boolean.TRUE);
        originalValues.add(new Float(10.6));
        originalValues.add(new Integer(8));
        // originalValues.add(c);
        // originalValues.add(slot);
        // originalValues.add(instance);
        String instanceName = instance.getName();
        instance.setOwnSlotValues(slot, originalValues);
        saveAndReload();
        Collection loadedValues = new ArrayList(getInstance(instanceName).getOwnSlotValues(getSlot(slotName)));
        assertEquals("values", originalValues, loadedValues);
    }

    public static void main(String[] args) {
        run(DefaultKnowledgeBase_Test.class);
    }

    public void testAddInverseSlotValue() {
        Cls c = createCls();
        Slot s1 = createMultiValuedSlot(ValueType.INSTANCE);
        Slot s2 = createMultiValuedSlot(ValueType.INSTANCE);
        s1.setInverseSlot(s2);
        c.addDirectTemplateSlot(s1);
        c.addDirectTemplateSlot(s2);
        Instance instanceA = createInstance(c);
        Instance instanceB = createInstance(c);
        instanceA.addOwnSlotValue(s1, instanceB);
        assertSame("forward", instanceB, instanceA.getOwnSlotValue(s1));
        assertSame("backward", instanceA, instanceB.getOwnSlotValue(s2));
    }

    public void testAddOwnSlotValue() {
        Cls c = createCls();
        Slot s = createMultiValuedSlot(ValueType.INSTANCE);
        Instance instance = createInstance(c);
        Instance value1 = createInstance(c);
        Instance value2 = createInstance(c);
        instance.setOwnSlotValue(s, value1);
        instance.addOwnSlotValue(s, value2);
        assertEquals("pass 1", 2, instance.getOwnSlotValues(s).size());

        instance.setOwnSlotValue(s, value1);
        instance.addOwnSlotValue(s, value2);
        assertEquals("pass 2", 2, instance.getOwnSlotValues(s).size());

        assertSame("add doesn't replace", value1, instance.getOwnSlotValue(s));
    }

    public void testAreValidOwnSlotValues() {
        Cls cls = createCls();
        Slot slot = createSingleValuedSlot(ValueType.INTEGER);
        slot.setMinimumValue(new Integer(2));
        cls.addDirectTemplateSlot(slot);
        Instance instance = createInstance(cls);
        assertTrue("1", !instance.areValidOwnSlotValues(slot, Collections.singleton(new Integer(1))));
        assertTrue("2", instance.areValidOwnSlotValues(slot, Collections.singleton(new Integer(2))));
        assertTrue("3", instance.areValidOwnSlotValues(slot, Collections.singleton(new Integer(3))));
    }

    public void testCreateSubslot() {
        Cls c = createCls();
        Slot a = createMultiValuedSlot(ValueType.INSTANCE, c);
        Slot b = createSubSlot(a);
        assertSame("a type", ValueType.INSTANCE, a.getValueType());
        assertSame("b type", ValueType.INSTANCE, b.getValueType());
        assertTrue("a cardinality", a.getAllowsMultipleValues());
        assertTrue("b cardinality", b.getAllowsMultipleValues());
    }

    public void testDBJavaPackages() {
        setDatabaseProject();
        testJavaPackages();
        setFileProject();
    }

    public void testDBModficationFacets() throws java.text.ParseException {
        setDatabaseProject();
        testModificationFacets();
        setFileProject();
    }

    public void testDeleteCls() {
        Cls cls = createCls();
        Cls subCls1 = createSubCls(cls);
        Cls subCls2 = createSubCls(cls);
        Instance instance1 = createInstance(subCls1);
        Instance instance2 = createInstance(subCls2);

        int nFrames = getFrameCount();
        try {
            deleteFrame(subCls1);
            fail();
        } catch (RuntimeException e) {
        }
        assertEquals("delete with direct instance", nFrames, getFrameCount());
        try {
            deleteFrame(cls);
        } catch (RuntimeException e) {
        }
        assertEquals("delete with indirect instance", nFrames, getFrameCount());

        deleteFrame(instance1);
        deleteFrame(subCls1);
        nFrames -= 2;
        assertEquals("delete with no direct instance", nFrames, getFrameCount());
        deleteFrame(instance2);
        deleteFrame(cls);
        nFrames -= 3;
        assertEquals("delete with no instances", nFrames, getFrameCount());
    }

    public void testDeleteClsEvent() {
        final KnowledgeBaseEvent[] firedEvent = new KnowledgeBaseEvent[1];
        Cls cls = createCls();
        String name = cls.getName();
        KnowledgeBaseListener listener = new KnowledgeBaseAdapter() {
            public void clsDeleted(KnowledgeBaseEvent event) {
                firedEvent[0] = event;
            }
        };
        cls.getKnowledgeBase().addKnowledgeBaseListener(listener);
        deleteFrame(cls);
        cls.getKnowledgeBase().removeKnowledgeBaseListener(listener);
        assertNotNull("fired", firedEvent[0]);
        assertEquals("name available", name, firedEvent[0].getOldName());
    }

    public void testFacetOverrides() {
        Cls a = createCls();
        Cls b = createSubCls(a);
        Cls c = createSubCls(b);
        Slot s = createSingleValuedSlot(ValueType.BOOLEAN);
        a.addDirectTemplateSlot(s);
        assertEquals(ValueType.BOOLEAN, s.getValueType());
        assertTrue("a not overridden", !a.hasOverriddenTemplateSlot(s));
        assertTrue("b not overridden", !b.hasOverriddenTemplateSlot(s));

        a.setTemplateSlotValueType(s, ValueType.FLOAT);
        assertEquals(ValueType.BOOLEAN, s.getValueType());
        assertEquals(ValueType.FLOAT, a.getTemplateSlotValueType(s));

        assertTrue("a overridden", a.hasOverriddenTemplateSlot(s));
        assertTrue("b overridden", b.hasOverriddenTemplateSlot(s));
        assertTrue("a directly overridden", a.hasDirectlyOverriddenTemplateSlot(s));
        assertTrue("b not directly overridden", !b.hasDirectlyOverriddenTemplateSlot(s));
    }

    public void testGetDBReferences() {
        init();
        setDatabaseProject();
        Slot s = createMultiValuedSlot(ValueType.STRING);
        Cls a = createCls();
        a.addDirectTemplateSlot(s);
        Instance inst1 = createInstance(a);

        inst1.addOwnSlotValue(s, "zabcy");
        inst1.addOwnSlotValue(s, "abcz");
        inst1.addOwnSlotValue(s, "qqq");
        assertEquals("exact", 0, getDomainKB().getReferences("z", 0).size());
        assertEquals("starts", 1, getDomainKB().getReferences("z*", 0).size());
        assertEquals("contains", 2, getDomainKB().getReferences("*z*", 0).size());
        assertEquals("contains insensitive", 2, getDomainKB().getReferences("*Z*", 0).size());
        assertEquals("contains 2", 2, getDomainKB().getReferences("*abc*", 0).size());
    }

    public void testGetDirectInstances() {
        setDatabaseProject();
        Cls cls = createCls();
        String name = cls.getName();
        createInstance(cls);
        createInstance(cls);
        createInstance(cls);
        assertEquals("direct instance count", 3, cls.getDirectInstanceCount());
        saveAndReload();
        cls = getCls(name);
        createInstance(cls);
        assertEquals("direct instance count after reload", 4, cls.getDirectInstanceCount());
    }

    public void testGetInstances() {
        List c = new ArrayList(getProject().getSources().getKnowledgeBase().getInstances());
        Collections.sort(c, new edu.stanford.smi.protege.ui.FrameNameComparator());
        Iterator i = c.iterator();
        while (i.hasNext()) {
            Instance instance = (Instance) i.next();
            assertNotNull(instance.getName());
        }

    }

    public void testGetMatchingFrameNames() {
    }

    public void testInverseSlotRelationship() {
        Slot s1 = createSingleValuedSlot(ValueType.INSTANCE);
        Slot s2 = createSingleValuedSlot(ValueType.INSTANCE);
        s1.setInverseSlot(s2);
        assertSame("created", s1, s2.getInverseSlot());
        s1.setInverseSlot(null);
        assertNull("null", s2.getInverseSlot());
    }

    public void testJavaPackages() {
        String packageName = "edu.stanford.smi.protege.model";
        String clsName = "DefaultKnowledgeBase_Test$TestSimpleInstance";
        Class testJavaClass = DefaultKnowledgeBase_Test.TestSimpleInstance.class;

        getProject().addJavaPackageName(packageName);
        Cls testCls = getCls(clsName);
        if (testCls == null) {
            testCls = createCls(clsName);
        }
        Instance instance = createInstance(testCls);
        assertTrue("correct class: " + instance, testJavaClass.isInstance(instance));

        saveAndReload();
        testCls = getCls(clsName);
        assertNotNull("after reload", testCls);
        getProject().removeJavaPackageName(packageName);
        Iterator i = testCls.getDirectInstances().iterator();
        while (i.hasNext()) {
            Instance inst = (Instance) i.next();
            assertTrue("correct class: " + inst, testJavaClass.isInstance(instance));
        }
    }

    public void testModificationFacets() throws java.text.ParseException {
        final String USERNAME = "tester";
        Date start = new Date();
        SystemUtilities.sleepMsec(100);
        KnowledgeBase kb = getDomainKB();
        kb.setAutoUpdateFacetValues(true);
        kb.setUserName(USERNAME);
        Cls metaSlot = createSubCls(getCls(Model.Cls.STANDARD_SLOT));
        metaSlot.addDirectTemplateSlot(getSlot(Model.Slot.MODIFIER));
        metaSlot.addDirectTemplateSlot(getSlot(Model.Slot.MODIFICATION_TIMESTAMP));
        Slot slot = (Slot) createInstance(metaSlot);
        Cls a = createCls();
        a.addDirectTemplateSlot(slot);
        Instance instance = createInstance(a);
        instance.setOwnSlotValue(slot, "foo");

        kb.setUserName(null);
        String modifier = kb.getSlotValueLastModifier(instance, slot, false);
        assertNotNull("modifier", modifier);
        String modStamp = kb.getSlotValueLastModificationTimestamp(instance, slot, false);
        assertNotNull("timestamp", modStamp);
        Date modDate = new StandardDateFormat().parse(modStamp);
        SystemUtilities.sleepMsec(100);
        Date end = new Date();
        assertEquals("name", USERNAME, modifier);
        assertTrue("timestamp after begin: " + modDate + " - " + start, modDate.after(start));
        assertTrue("timestamp before end: " + modDate + " - " + end, modDate.before(end));
        kb.setAutoUpdateFacetValues(false);
    }

    public void testModificationSlots() throws java.text.ParseException {
        final String USERNAME = "tester";
        Date start = new Date();
        SystemUtilities.sleepMsec(100);
        KnowledgeBase kb = getDomainKB();
        kb.setAutoUpdateFacetValues(true);
        kb.setUserName(USERNAME);
        Cls metaCls = createSubCls(getCls(Model.Cls.STANDARD_CLASS));
        metaCls.addDirectTemplateSlot(getSlot(Model.Slot.CREATOR));
        metaCls.addDirectTemplateSlot(getSlot(Model.Slot.CREATION_TIMESTAMP));
        metaCls.addDirectTemplateSlot(getSlot(Model.Slot.MODIFIER));
        metaCls.addDirectTemplateSlot(getSlot(Model.Slot.MODIFICATION_TIMESTAMP));
        Slot slot = createSingleValuedSlot(ValueType.INSTANCE);
        Cls a = (Cls) createInstance(metaCls);
        assertEquals("creator", USERNAME, kb.getFrameCreator(a));
        String creationString = kb.getFrameCreationTimestamp(a);
        assertNotNull("creation stamp", creationString);
        Date createDate = new StandardDateFormat().parse(creationString);
        SystemUtilities.sleepMsec(100);
        Date endDate = new Date();
        assertTrue("create timestamp after begin: " + createDate + " - " + start, createDate.after(start));
        assertTrue("create timestamp before end: " + createDate + " - " + endDate, createDate.before(endDate));

        a.addDirectTemplateSlot(slot);
        SystemUtilities.sleepMsec(100);

        kb.setUserName(null);

        String modifier = kb.getFrameLastModifier(a);
        assertEquals("name", USERNAME, modifier);
        String modStamp = kb.getFrameLastModificationTimestamp(a);
        assertNotNull("timestamp", modStamp);
        Date modDate = new StandardDateFormat().parse(modStamp);
        SystemUtilities.sleepMsec(100);
        Date end = new Date();
        assertTrue("mod timestamp after begin: " + modDate + " - " + start, modDate.after(start));
        assertTrue("mod timestamp before end: " + modDate + " - " + end, modDate.before(end));
        kb.setAutoUpdateFacetValues(false);
    }

    public void testRecursiveDelete() {
        Cls clsa = createCls();
        Cls clsb = createSubCls(clsa);
        Cls clsc = createSubCls(clsb);
        int count = getFrameCount();
        deleteFrame(clsb);
        int newCount = count - 2;
        assertEquals(newCount, getFrameCount());
        saveAndReload();
        assertEquals(newCount, getFrameCount());
    }

    public void testRemoveInverseSlotValue() {
        Cls c = createCls();
        Slot s1 = createSingleValuedSlot(ValueType.INSTANCE);
        Slot s2 = createSingleValuedSlot(ValueType.INSTANCE);
        s1.setInverseSlot(s2);
        c.addDirectTemplateSlot(s1);
        c.addDirectTemplateSlot(s2);
        Instance instanceA = createInstance(c);
        Instance instanceB = createInstance(c);
        instanceA.setOwnSlotValues(s1, Collections.singleton(instanceB));
        instanceA.setOwnSlotValue(s1, null);
        assertNull("forward", instanceA.getOwnSlotValue(s1));
        assertNull("backward", instanceB.getOwnSlotValue(s2));
    }

    public void testRemoveOwnSlotValue() {
        Cls c = createCls();
        Slot s = createSingleValuedSlot(ValueType.INSTANCE);
        c.addDirectTemplateSlot(s);
        Instance instanceA = createInstance(c);
        Instance instanceB = createInstance(c);
        Instance instanceC = createInstance(c);

        Collection values = new ArrayList();
        values.add(instanceB);
        values.add(instanceC);

        instanceA.setOwnSlotValues(s, values);
        instanceA.removeOwnSlotValue(s, instanceC);
        assertEquals("size", 1, instanceA.getOwnSlotValueCount(s));
        assertSame("1st value", instanceB, instanceA.getOwnSlotValue(s));

        instanceA.setOwnSlotValues(s, values);
        instanceA.removeOwnSlotValue(s, instanceB);
        assertEquals("size", 1, instanceA.getOwnSlotValueCount(s));
        assertSame("2nd value", instanceC, instanceA.getOwnSlotValue(s));
    }

    public void testSaveLoadTemplateSlotValues() {
        Cls c1 = createCls();
        Cls c2 = createCls();
        String c2Name = c2.getName();
        Slot s = createSingleValuedSlot(ValueType.INSTANCE);
        String sName = s.getName();
        c2.addDirectTemplateSlot(s);
        Collection values = Collections.singleton(c1);
        s.setValues(values);
        c2.setTemplateSlotValues(s, values);
        saveAndReload();
        s = getSlot(sName);
        c2 = getCls(c2Name);
        assertEquals(1, s.getValues().size());
        assertEquals(1, c2.getTemplateSlotValues(s).size());
    }

    public void testSetInverseSlotValue() {
        Cls c = createCls();
        Slot s1 = createMultiValuedSlot(ValueType.INSTANCE);
        Slot s2 = createMultiValuedSlot(ValueType.INSTANCE);
        s1.setInverseSlot(s2);
        c.addDirectTemplateSlot(s1);
        c.addDirectTemplateSlot(s2);
        Instance instanceA = createInstance(c);
        Instance instanceB = createInstance(c);

        instanceA.setOwnSlotValue(s1, instanceB);
        assertSame("set with value", instanceA, instanceB.getOwnSlotValue(s2));
        instanceA.setOwnSlotValue(s1, null);
        assertNull("set with null", instanceB.getOwnSlotValue(s2));
    }

    public void testSetInverseSlotValues1N() {
        Cls cls = createCls();
        Slot s1 = createSingleValuedSlot(ValueType.INSTANCE);
        Slot sN = createMultiValuedSlot(ValueType.INSTANCE);
        s1.setInverseSlot(sN);
        cls.addDirectTemplateSlot(s1);
        cls.addDirectTemplateSlot(sN);
        Instance instanceA = createInstance(cls);
        Instance instanceB = createInstance(cls);
        Instance instanceC = createInstance(cls);

        instanceB.setOwnSlotValue(s1, instanceC);
        assertSame("b connected to c", instanceC, instanceB.getOwnSlotValue(s1));
        assertSame("c connected to b", instanceB, instanceC.getOwnSlotValue(sN));

        instanceA.setOwnSlotValues(sN, Collections.singleton(instanceB));
        assertEquals("size b", 1, instanceB.getOwnSlotValues(s1).size());
        assertSame("b connected to a", instanceA, instanceB.getOwnSlotValue(s1));
        assertNull("c disconnected", instanceC.getOwnSlotValue(sN));
    }

    public void testSetInverseSlotValuesNN() {
        Cls cls = createCls();
        Slot s1 = createMultiValuedSlot(ValueType.INSTANCE);
        Slot s2 = createMultiValuedSlot(ValueType.INSTANCE);
        s1.setInverseSlot(s2);
        cls.addDirectTemplateSlot(s1);
        cls.addDirectTemplateSlot(s2);
        Instance instanceA = createInstance(cls);
        Instance instanceB = createInstance(cls);

        instanceA.setOwnSlotValues(s1, Collections.singleton(instanceB));
        assertSame("set with values", instanceA, instanceB.getOwnSlotValue(s2));
        instanceA.setOwnSlotValues(s1, Collections.EMPTY_LIST);
        assertNull("set with empty collection", instanceB.getOwnSlotValue(s2));
    }

    public void testSetOwnSlotValue() {
        Cls c = createCls();
        Slot s = createMultiValuedSlot(ValueType.INSTANCE);
        c.addDirectTemplateSlot(s);
        Instance instanceA = createInstance(c);
        Instance instanceB = createInstance(c);

        instanceA.setOwnSlotValue(s, instanceB);
        Collection values = instanceA.getOwnSlotValues(s);
        assertEquals(1, values.size());
        assertSame(instanceB, CollectionUtilities.getFirstItem(values));

        instanceA.setOwnSlotValue(s, instanceA);
        values = instanceA.getOwnSlotValues(s);
        assertEquals(1, values.size());
        assertSame(instanceA, CollectionUtilities.getFirstItem(values));
    }

    public void testSetOwnSlotValues() {
        getDomainKB().setValueChecking(true);
        Cls cls = createCls();
        Slot slot = createSingleValuedSlot(ValueType.STRING);
        cls.addDirectTemplateSlot(slot);
        Instance i = createInstance(cls);
        Collection values = new ArrayList();
        values.add(new String());
        i.setOwnSlotValues(slot, values);
        values.add(new String());
        try {
            i.setOwnSlotValues(slot, values);
            fail("should have thrown exception");
        } catch (IllegalArgumentException e) {
        } finally {
            getDomainKB().setValueChecking(false);
        }
    }

    public void testSubslots() {
        Cls cls = createCls();
        Instance instanceA = createInstance(cls);
        String instanceAName = instanceA.getName();
        Slot a = createMultiValuedSlot(ValueType.STRING);
        Slot b = createSubSlot(a);
        String obj1 = new String("a");
        String obj2 = new String("b");
        String aSlotName = a.getName();

        cls.addDirectTemplateSlot(a);
        cls.addDirectTemplateSlot(b);
        instanceA.setOwnSlotValue(a, obj1);
        instanceA.setOwnSlotValue(b, obj2);
        Collection aValues = instanceA.getOwnSlotValues(a);
        assertEquals("a", 1, aValues.size());
        assertTrue("a contains", aValues.contains(obj1));
        Collection bValues = instanceA.getOwnSlotValues(b);
        assertEquals("b", 1, bValues.size());
        assertTrue("b contains", bValues.contains(obj2));
        Collection allAValues = instanceA.getOwnSlotAndSubslotValues(a);
        assertEquals("a and b", 2, allAValues.size());
        assertTrue("a and b contains 1", allAValues.contains(obj1));
        assertTrue("a and b contains 2", allAValues.contains(obj2));
        Collection allBValues = instanceA.getOwnSlotAndSubslotValues(b);
        assertEquals("b but not a", 1, allBValues.size());
        assertTrue("b but not a contains 2", allBValues.contains(obj2));

        saveAndReload();
        a = getSlot(aSlotName);
        instanceA = getInstance(instanceAName);
        assertNotNull("slot a", a);
        allAValues = instanceA.getOwnSlotAndSubslotValues(a);
        assertEquals("save", 2, allAValues.size());
        assertTrue("save value 1", allAValues.contains(obj1));
        assertTrue("save value 2", allAValues.contains(obj2));
    }
}
