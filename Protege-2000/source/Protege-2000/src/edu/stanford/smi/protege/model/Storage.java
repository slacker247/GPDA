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
import edu.stanford.smi.protege.util.*;

/**
 * Low-level holder of frames.
 *
 * @author Ray Fergerson <fergerson@smi.stanford.edu>
 */
public interface Storage extends Disposable {

    void addFrame(Frame frame);

    void addValue(Frame frame, Slot slot, Facet facet, boolean isTemplate, Object value);

    void addValue(Frame frame, Slot slot, Facet facet, boolean isTemplate, Object value, int index);

    public boolean beginTransaction();

    boolean containsFrame(Frame frame);

    boolean containsFrame(String name);

    public boolean endTransaction(boolean doCommit);

    Frame getFrame(FrameID id);

    Frame getFrame(String name);

    int getFrameCount();
    int getFacetCount();
    int getSlotCount();
    int getClsCount();

    Collection getFrames();

    Collection getMatchingFrames(Slot slot, Facet facet, boolean isTemplate, String s, int maxMatches);

    // back references
    Collection getReferences(Object o, int maxReferences);

    Object getValue(Frame frame, Slot slot, Facet facet, boolean isTemplate);

    int getValueCount(Frame frame, Slot slot, Facet facet, boolean isTemplate);

    ArrayList getValues(Frame frame, Slot slot, Facet facet, boolean isTemplate);

    boolean hasValue(Frame frame, Slot slot, Facet facet, boolean isTemplate, Object value);

    boolean hasValueAtSomeFrame(Slot slot, Facet facet, boolean isTemplate);

    void moveValue(Frame frame, Slot slot, Facet facet, boolean isTemplate, int from, int to);

    void remove(Frame frame, Slot slot, Facet facet, boolean isTemplate);

    void removeFrame(Frame frame);

    /** remove a single occurance of value from the slot/facet value */
    void removeSingleValue(Frame frame, Slot slot, Facet facet, boolean isTemplate, Object value);

    void removeValues(Slot slot, Facet facet, boolean isTemplate, Cls cls);

    void replace(Frame from, Frame to);

    void setValue(Frame frame, Slot slot, Facet facet, boolean isTemplate, Object value);

    void setValues(Frame frame, Slot slot, Facet facet, boolean isTemplate, Collection values);

    public boolean supportsTransactions();
}
