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

package edu.stanford.smi.protege.storage.jdbc;


import java.sql.*;
import java.util.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.model.framedb.*;
import edu.stanford.smi.protege.util.*;

/**
 * Implementation of storage interfaces for database.  This implementation delegates
 * in memory behavior to FrameDBStorage and additionally sends commands along to its
 * DatabaseManager (from whence they make it to the real db).
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class DatabaseStorage implements Storage, DatabaseConstants {
    private DatabaseManager _databaseManager;
    private FrameDBStorage _delegate;
    private DefaultKnowledgeBase _knowledgeBase;
    private Slot _nameSlot;

    public DatabaseStorage(DefaultKnowledgeBase kb, DatabaseManager manager, FrameDBStorage delegate) {
        _databaseManager = manager;
        _delegate = delegate;
        _knowledgeBase = kb;
        _nameSlot = kb.getSlot(Model.Slot.NAME);
        // itsDirectTypeSlot = kb.getSlot(Model.Slot.DIRECT_TYPE);
        updateDelegate();
    }

    public void addFrame(Frame frame) {
        _delegate.addFrame(frame);
    }

    public void addValue(Frame frame, Slot slot, Facet facet, boolean isTemplate, Object value) {
        // Log.enter(this, "addValue", frame, slot, facet, value);
        try {
            if (isLoaded(frame, slot, facet, isTemplate)) {
                _delegate.addValue(frame, slot, facet, isTemplate, value);
            }
            _databaseManager.addValue(frame, slot, facet, isTemplate, value);
        } catch (SQLException e) {
            Log.exception(e, this, "addValue", frame, slot, facet, value);
        }
    }

    public void addValue(Frame frame, Slot slot, Facet facet, boolean isTemplate, Object value, int index) {
        try {
            // itsDatabaseManager.addValue(frame, slot, facet, isTemplate, value, index);
            _delegate.addValue(frame, slot, facet, isTemplate, value, index);
            updateDB(frame, slot, facet, isTemplate);
        } catch (SQLException e) {
            Log.exception(e, this, "addValue", frame, slot, facet, value);
        }
    }

    public boolean beginTransaction() {
        return _databaseManager.beginTransaction();
    }

    public boolean containsFrame(Frame frame) {
        return _delegate.containsFrame(frame);
    }

    public boolean containsFrame(String name) {
        boolean exists = _delegate.containsFrame(name);
        if (!exists) {
            exists = getDBFrameID(name) != null;
        }
        return exists;
    }

    public void dispose() {
        _databaseManager.dispose();
        // itsLoadedReferences = null;
    }

    public boolean endTransaction(boolean doCommit) {
        boolean committed = _databaseManager.endTransaction(doCommit);
        if (!committed) {
            _delegate.flush();
        }
        return committed;
    }

    public DatabaseManager getDatabaseManager() {
        return _databaseManager;
    }

    private FrameID getDBDirectType(FrameID id) {
        FrameID result;
        try {
            Collection frameIDs = _databaseManager.getValues(id, Model.Slot.ID.DIRECT_TYPE, null, false);
            result = (FrameID) CollectionUtilities.getFirstItem(frameIDs);
        } catch (SQLException e) {
            result = null;
        }
        return result;
    }

    private FrameID getDBFrameID(String name) {
        FrameID id;
        try {
            Collection ids = _databaseManager.getFrameIDs(Model.Slot.ID.NAME, null, false, name);
            if (ids.size() > 1) {
                Log.error(ids.size() + " frames with the same name", this, "getDBFrameID", name);
            }
            id = (FrameID) CollectionUtilities.getFirstItem(ids);
        } catch (SQLException e) {
            id = null;
        }
        return id;
    }

    private String getDBName(FrameID id) {
        String name;
        try {
            Collection values = _databaseManager.getValues(id, Model.Slot.ID.NAME, null, false);
            if (values.size() != 1) {
                Log.warning("problem with frame name", this, "getDBName", id, values);
            }
            name = (String) CollectionUtilities.getFirstItem(values);
        } catch (SQLException e) {
            name = null;
        }
        return name;
    }

    public int getDBValueCount(Frame frame, Slot slot, Facet facet, boolean isTemplate) {
        int count;
        try {
            count = _databaseManager.getValueCount(getID(frame), getID(slot), getID(facet), isTemplate);
        } catch (SQLException e) {
            Log.exception(e, this, "getDBValueCount", frame, slot, facet);
            count = 0;
        }
        return count;
    }

    public Frame getFrame(FrameID id) {
        Frame frame = _delegate.getFrame(id);
        if (frame == null) {
            frame = loadFrameFromDB(id);
        }
        return frame;
    }

    public Frame getFrame(String name) {
        Frame frame = _delegate.getFrame(name);
        if (frame == null) {
            FrameID id = getDBFrameID(name);
            if (id != null) {
                frame = _delegate.getFrame(id);
                if (frame == null) {
                    frame = loadFrameFromDB(id, name);
                }
            }
        }
        return frame;
    }

    public int getFrameCount() {
        // there should be a better way to get this information...
        int numberOfSystemFramesNotInDatabase = 55;
        return getFrameCount(TYPE_INVALID) + numberOfSystemFramesNotInDatabase;
    }

    public int getSlotCount() {
        // there should be a better way to get this information...
        int numberOfSystemSlotsNotInDatabase = 31;
        return getFrameCount(TYPE_SLOT) + numberOfSystemSlotsNotInDatabase;
    }

    public int getFacetCount() {
        // there should be a better way to get this information...
        int numberOfSystemFacetNotInDatabase = 12;
        return getFrameCount(TYPE_FACET) + numberOfSystemFacetNotInDatabase;
    }
    public int getClsCount() {
        // there should be a better way to get this information...
        int numberOfSystemClsesNotInDatabase = 12;
        return getFrameCount(TYPE_CLASS) + numberOfSystemClsesNotInDatabase;
    }

    private int getFrameCount(int type) {
        int count = 0;
        try {
            count = _databaseManager.getFrameCount(type);
        } catch (SQLException e) {
            Log.exception(e, this, "getFrameCount");
        }
        return count;
    }

    public Collection getFrames() {
        Collection frames = new HashSet(_delegate.getFrames());
        try {
            Collection frameIDs = _databaseManager.getAllFrameIDs();
            Iterator i = frameIDs.iterator();
            while (i.hasNext()) {
                FrameID id = (FrameID) i.next();
                frames.add(getFrame(id));
            }
        } catch (SQLException e) {
            Log.exception(e, this, "getFrames");
        }
        return frames;
    }

    // private int nLoadedFrames;

    // private Collection itsLoadingFrameIDs = new HashSet();

    private int getFrameType(FrameID id) {
        int result;
        try {
            result = _databaseManager.getFrameType(id);
        } catch (SQLException e) {
            Log.exception(e, this, "getFrameType", id);
            result = TYPE_INVALID;
        }
        return result;
    }

    private FrameID getID(Frame frame) {
        return (frame == null) ? (FrameID) null : frame.getFrameID();
    }

    public Collection getMatchingFrames(Slot slot, Facet facet, boolean isTemplate, String matchString, int maxMatches) {
        Collection matchedFrames;

        // need to query the db
        try {
            FrameID slotID = slot.getFrameID();
            FrameID facetID = getID(facet);
            Collection frameIDs = _databaseManager.getMatchingFrameIDs(slotID, facetID, isTemplate, matchString, maxMatches);
            matchedFrames = new ArrayList(frameIDs.size());
            Iterator i = frameIDs.iterator();
            while (i.hasNext()) {
                FrameID id = (FrameID) i.next();
                Frame frame = getFrame(id);
                matchedFrames.add(frame);
            }
        } catch (SQLException e) {
            matchedFrames = Collections.EMPTY_LIST;
            Log.exception(e, this, "getMatchingFrames", matchString);
        }
        return matchedFrames;
    }

    private String getName(FrameID id) {
        Frame frame = _delegate.getFrame(id);
        String name;
        if (frame == null) {
            name = getDBName(id);
        } else {
            name = frame.getName();
        }
        return name;
    }

    public Collection getReferences(Object o, int maxReferences) {
        ArrayList references = new ArrayList();
        try {
            Collection dbReferences = _databaseManager.getDBReferences(o, maxReferences);
            Iterator i = dbReferences.iterator();
            while (i.hasNext()) {
                DBReference dbRef = (DBReference) i.next();
                Frame frame = getFrame(dbRef.getFrame());
                Slot slot = (Slot) getFrame(dbRef.getSlot());
                FrameID facetID = dbRef.getFacet();
                Facet facet = (facetID == null) ? (Facet) null : (Facet) getFrame(facetID);
                boolean isTemplate = dbRef.getIsTemplate();
                references.add(new MemoryReference(frame, slot, facet, isTemplate));
            }
        } catch (SQLException e) {
            Log.exception(e, this, "getReferences", o);
        }
        return references;
    }

    public Object getValue(Frame frame, Slot slot, Facet facet, boolean isTemplate) {
        Collection values = loadAndGetDBValues(frame, slot, facet, isTemplate);
        Object result = CollectionUtilities.getFirstItem(values);
        if (result instanceof FrameID) {
            FrameID id = (FrameID) result;
            result = getFrame(id);
        }
        return result;
    }

    public int getValueCount(Frame frame, Slot slot, Facet facet, boolean isTemplate) {
        /*
        int count = _delegate.getValueCount(frame, slot, facet, isTemplate);
        if (!isLoaded(frame, slot, facet, isTemplate)) {
            count += getDBValueCount(frame, slot, facet, isTemplate);
        }
        */
        return loadAndGetDBValues(frame, slot, facet, isTemplate).size();
    }

    private int getValueIndex(Frame frame, Slot slot, Facet facet, boolean isTemplate, Object value) {
        return _delegate.getValues(frame, slot, facet, isTemplate).indexOf(value);
    }

    public ArrayList getValues(Frame frame, Slot slot, Facet facet, boolean isTemplate) {
        ArrayList values = new ArrayList(loadAndGetDBValues(frame, slot, facet, isTemplate));
        for (int i = 0; i < values.size(); ++i) {
            Object o = values.get(i);
            if (o instanceof FrameID) {
                FrameID id = (FrameID) o;
                values.set(i, getFrame(id));
            }
        }
        return values;
    }

    private List getValuesFromDB(FrameID frame, FrameID slot, FrameID facet, boolean isTemplate) {
        List values;
        try {
            values = _databaseManager.getValues(frame, slot, facet, isTemplate);
        } catch (SQLException e) {
            Log.exception(e, this, "getValues");
            values = Collections.EMPTY_LIST;
        }
        return values;
    }

    private ValueType getValueType(Frame frame, Slot slot, Facet facet, boolean isTemplate) {
        ValueType valueType;
        if (facet == null) {
            if (isTemplate) {
                valueType = ((Cls) frame).getTemplateSlotValueType(slot);
            } else {
                if (slot.getFrameID() == Model.Slot.ID.DEFAULTS) {
                    valueType = ((Slot) frame).getValueType();
                } else if (slot.isSystem()) {
                    valueType = slot.getValueType();
                } else {
                    valueType = frame.getOwnSlotValueType(slot);
                }
            }
        } else {
            if (facet.getFrameID() == Model.Facet.ID.DEFAULTS) {
                valueType = ((Cls) frame).getTemplateSlotValueType(slot);
            } else {
                valueType = facet.getAssociatedSlot().getValueType();
            }
        }
        return valueType;
    }

    public boolean hasValue(Frame frame, Slot slot, Facet facet, boolean isTemplate, Object value) {
        return getValues(frame, slot, facet, isTemplate).contains(value);
    }

    public boolean hasValueAtSomeFrame(Slot slot, Facet facet, boolean isTemplate) {
        boolean result = _delegate.hasValueAtSomeFrame(slot, facet, isTemplate);
        if (!result) {
            try {
                result = _databaseManager.hasValueAtSomeFrame(slot, facet, isTemplate);
            } catch (SQLException e) {
                Log.exception(e, this, "hasValueAtSomeFrame", slot);
            }
        }
        return result;
    }

    private boolean isLoaded(Frame frame, Slot slot, Facet facet, boolean isTemplate) {
        return _delegate.getLoadedValues(getID(frame), getID(slot), getID(facet), isTemplate) != null;
    }

    private boolean isValueType(Slot slot, Facet facet) {
        boolean isValueType;
        if (facet == null) {
            isValueType = slot.getFrameID() == Model.Slot.ID.VALUE_TYPE;
        } else {
            isValueType = facet.getFrameID() == Model.Facet.ID.VALUE_TYPE;
        }
        return isValueType;
    }

    private List loadAndGetDBValues(Frame frame, Slot slot, Facet facet, boolean isTemplate) {
        FrameID frameID = getID(frame);
        FrameID slotID = getID(slot);
        FrameID facetID = getID(facet);
        List values = _delegate.getLoadedValues(frameID, slotID, facetID, isTemplate);
        if (values == null) {
            values = getValuesFromDB(frameID, slotID, facetID, isTemplate);
            _delegate.setValues(frameID, slotID, facetID, isTemplate, values);
        }
        return values;
    }

    private Frame loadFrameFromDB(FrameID id) {
        String name = getDBName(id);
        return loadFrameFromDB(id, name);
    }

    private Frame loadFrameFromDB(FrameID id, String name) {
        int frameType = getFrameType(id);
        Frame frame;
        switch (frameType) {
            case TYPE_CLASS :
                frame = new DefaultCls(_knowledgeBase, id);
                break;
            case TYPE_SLOT :
                frame = new DefaultSlot(_knowledgeBase, id);
                break;
            case TYPE_FACET :
                frame = new DefaultFacet(_knowledgeBase, id);
                break;
            case TYPE_SIMPLE_INSTANCE :
                String typeName = getName(getDBDirectType(id));
                frame = _knowledgeBase.makeSimpleInstance(typeName, id);
                break;
            default :
                Assert.fail("bad frame type: " + frameType);
                frame = null;
        }
        _delegate.addFrame(frame);
        _delegate.setValue(frame, _nameSlot, null, false, name);
        return frame;
    }

    private Frame loadFrameFromDB(String name) {
        FrameID id = getDBFrameID(name);
        return (id == null) ? (Frame) null : loadFrameFromDB(id, name);
    }

    public void moveValue(Frame frame, Slot slot, Facet facet, boolean isTemplate, int from, int to) {
        try {
            // itsDatabaseManager.moveValue(frame, slot, facet, isTemplate, from, to);
            _delegate.moveValue(frame, slot, facet, isTemplate, from, to);
            updateDB(frame, slot, facet, isTemplate);
        } catch (SQLException e) {
            Log.exception(e, this, "moveValue", frame, slot, facet);
        }
    }

    private boolean needsUpdate(FrameID slot) {
        return slot == Model.Slot.ID.DIRECT_INSTANCES
            || slot == Model.Slot.ID.DIRECT_SUBCLASSES
            || slot == Model.Slot.ID.DIRECT_SUBSLOTS;
    }

    public void remove(Frame frame, Slot slot, Facet facet, boolean isTemplate) {
        try {
            _delegate.remove(frame, slot, facet, isTemplate);
            _databaseManager.removeAllValues(frame, slot, facet, isTemplate);
        } catch (SQLException e) {
            Log.exception(e, this, "remove", frame, slot, facet);
        }
    }

    public void removeFrame(Frame frame) {
        try {
            _databaseManager.removeAllReferences(frame);
            _delegate.removeFrame(frame);
        } catch (SQLException e) {
            Log.exception(e, this, "removeFrame", frame);
        }
    }

    public void removeSingleValue(Frame frame, Slot slot, Facet facet, boolean isTemplate, Object value) {
        // Log.enter(this, "removeValue", frame, slot, facet, value);
        try {
            if (isLoaded(frame, slot, facet, isTemplate)) {
                // itsDatabaseManager.removeValue(frame, slot, facet, isTemplate, index);
                _delegate.removeSingleValue(frame, slot, facet, isTemplate, value);
            }
            _databaseManager.removeSingleValue(frame, slot, facet, isTemplate, value);
        } catch (SQLException e) {
            Log.exception(e, this, "removeValue", frame, slot, facet, value);
        }
    }

    public void removeValues(Slot slot, Facet facet, boolean isTemplate, Cls cls) {
        _delegate.removeValues(slot, facet, isTemplate, cls);
        try {
            if (cls == null) {
                _databaseManager.removeFramesWithValue(slot, facet, isTemplate);
            } else {
                Iterator i = _databaseManager.getFramesWithValue(slot, facet, isTemplate).iterator();
                while (i.hasNext()) {
                    FrameID frameID = (FrameID) i.next();
                    Instance instance = (Instance) getFrame(frameID);
                    if (instance.hasType(cls)) {
                        _databaseManager.removeAllValues(instance, slot, facet, isTemplate);
                    }
                }
            }
        } catch (SQLException e) {
            Log.exception(e, this, "removeValues", slot);
        }
    }

    public void replace(Frame from, Frame to) {
        Assert.fail("not implemented");
    }

    public void setValue(Frame frame, Slot slot, Facet facet, boolean isTemplate, Object value) {
        try {
            _delegate.setValue(frame, slot, facet, isTemplate, value);
            _databaseManager.setValue(frame, slot, facet, isTemplate, value);
        } catch (SQLException e) {
            Log.exception(e, this, "setValue", frame, slot, facet);
        }
    }

    public void setValues(Frame frame, Slot slot, Facet facet, boolean isTemplate, Collection values) {
        try {
            _delegate.setValues(frame, slot, facet, isTemplate, values);
            _databaseManager.setValues(frame, slot, facet, isTemplate, values);
        } catch (SQLException e) {
            Log.exception(e, this, "setValues", frame, slot, facet);
        }
    }

    private Frame stringToFrame(String idString) {
        FrameID id = new FrameID(idString);
        return getFrame(id);
    }

    public boolean supportsTransactions() {
        return _databaseManager.supportsTransactions();
    }

    public String toString() {
        return "DatabaseStorage";
    }

    private void updateDB(Frame frame, Slot slot, Facet facet, boolean isTemplate) throws SQLException {
        Collection values = _delegate.getValues(frame, slot, facet, isTemplate);

        /*
         * The following unfortunate logic keeps us from recording such things as that :CLASS is a child of :THING.
         * We don't want the system class hierarchy stored in a database because it is still evolving.
         * Nevertheless we have to record that a user defined class is a subclass of :THING.  Thus
         * we need to store some of the subclasses of :THING but not all of them.
         */
        if (slot.getFrameID() == Model.Slot.ID.DIRECT_SUBCLASSES) {
            values = new ArrayList(values);
            Iterator i = values.iterator();
            while (i.hasNext()) {
                Cls cls = (Cls) i.next();
                if (cls.isIncluded()) {
                    i.remove();
                }
            }
        }

        _databaseManager.setValues(frame, slot, facet, isTemplate, values);
    }

    private void updateDelegate() {
        Iterator i = _delegate.getRecords().iterator();
        while (i.hasNext()) {
            Record record = (Record) i.next();
            FrameID frame = record.getFrame();
            FrameID slot = record.getSlot();
            FrameID facet = record.getFacet();
            boolean isTemplate = record.getIsTemplate();
            if (needsUpdate(slot)) {
                List values = getValuesFromDB(frame, slot, facet, isTemplate);
                if (!values.isEmpty()) {
                    // Log.trace("adding " + values.get(0) + " ( " + values.size() + ") to " + frame + " " + slot + " " + facet, this, "updateDelegate");
                    _delegate.addValues(frame, slot, facet, isTemplate, values);
                }
            }
        }
    }
}
