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
import edu.stanford.smi.protege.util.*;

/**
 * Interface to a database.  All SQL commands are hidden away in the implementation of
 * this interface.
 *
 * @author Ray Fergerson <fergerson@smi.stanford.edu>
 */
public interface DatabaseManager extends DatabaseConstants, Disposable {

    void addValue(Frame frame, Slot slot, Facet facet, boolean isTemplate, Object value)
             throws SQLException;

    public boolean beginTransaction();

    public boolean endTransaction(boolean doCommit);

    Collection getAllFrameIDs()
             throws SQLException;

    int getFrameCount(int type) throws SQLException;

    // FrameInfo getFrameInfo(FrameID id) throws SQLException;
    /**
     *  returns a collection of DBReference objects
     *
     * @return                   The DBReferences value
     * @param  o                 Description of Parameter
     * @exception  SQLException  Description of Exception
     */
    Collection getDBReferences(Object o, int maxReferences)
             throws SQLException;

    String getDriverName();

    Collection getFrameIDs(FrameID slot, FrameID facet, boolean isTemplate, String value) throws SQLException;

    /**
     *  returns a collection of Objects. A frame is returned as a frameID
     */
    Collection getFramesWithValue(Slot slot, Facet facet, boolean isTemplate)
             throws SQLException;

    int getFrameType(FrameID id) throws SQLException;

    Collection getMatchingFrameIDs(FrameID slot, FrameID facet, boolean isTemplate,
            String matchString, int maxMatches) throws SQLException;

    String getTableName();

    int getValueCount(FrameID frame, FrameID slot, FrameID facet, boolean isTemplate)
             throws SQLException;

    /**
     *  returns a collection of Objects. A frame is returned as a frameID
     */
    List getValues(FrameID frame, FrameID slot, FrameID facet, boolean isTemplate)
             throws SQLException;

    boolean hasValueAtSomeFrame(Slot slot, Facet facet, boolean isTemplate) throws SQLException;

    void removeAllReferences(Frame frame)
             throws SQLException;

    void removeAllValues(Frame frame, Slot slot, Facet facet, boolean isTemplate)
             throws SQLException;

    void removeFramesWithValue(Slot slot, Facet facet, boolean isTemplate) throws SQLException;

    void removeSingleValue(Frame frame, Slot slot, Facet facet, boolean isTemplate, Object value) throws SQLException;

    void saveKnowledgeBase(KnowledgeBase kb)
             throws SQLException;

    void setTracing(boolean b);

    void setValue(Frame frame, Slot slot, Facet facet, boolean isTemplate, Object value)
             throws SQLException;

    void setValues(Frame frame, Slot slot, Facet facet, boolean isTemplate, Collection values)
             throws SQLException;

    public boolean supportsTransactions();
}
