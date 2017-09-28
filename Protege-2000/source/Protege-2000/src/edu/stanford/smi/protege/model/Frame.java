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
import edu.stanford.smi.protege.event.*;

/**
 * A container for slot and facet values.  This definition of a Frame is
 * roughly consistent with the OKBC notion.
 *
 * OKBC introduces a notion of a slot being attached to a frame as
 * either an "own-slot" or as a "template-slot".
 * A "templates-slot" is a slot on a class which is automatically attached to
 * subclasses and is automatically attached at all instances of the class.  An
 * "own-slot" is a slot on any frame (either a class or not) which is
 * not automatically inherited to subclasses and is not automatically attached
 * to instances of the frame.  Note that the difference between these two things
 * is only relavent for class frames and we only support template slots on
 * class frames.
 *
 * Own-slot values, even on a class, are not inherited to subclasses and do not
 * occur on instances of the class.  Template-slot values are inherited to subclasses
 * and appear on instances of the class.
 *
 * get/set slot and facet value methods return/take a collection of objects.  The type of object returned
 * or required depends on the value-type of the slot.  The mapping between
 * protege value-types and object type is given below:
 * <pre>
 * Boolean    --> java.lang.Boolean
 * Class      --> edu.stanford.smi.protege.model.Cls
 * Float      --> java.lang.Float
 * Instance   --> edu.stanford.smi.protege.model.Instance
 * Integer    --> java.lang.Integer
 * Symbol     --> java.lang.String
 * String     --> java.lang.String
 * </pre>
 *
 * @author Ray Fergerson <fergerson@smi.stanford.edu>
 */
public interface Frame extends Comparable {

    void addFrameListener(FrameListener listener);

    boolean addOwnFacetValue(Slot slot, Facet facet, Object value);

    void addOwnSlotValue(Slot slot, Object value);

    boolean areValidOwnSlotValues(Slot slot, Collection values);

    Frame deepCopy(KnowledgeBase kb, Map valueMap);

    void delete();

    String getBrowserText();

    Collection getDocumentation();

    FrameID getFrameID();

    String getInvalidOwnSlotValuesText(Slot slot, Collection values);

    String getInvalidOwnSlotValueText(Slot slot, Object value);

    KnowledgeBase getKnowledgeBase();

    String getName();

    /** see {@link Frame} for a description of the return type */
    Object getOwnFacetValue(Slot slot, Facet facet);

    /** see {@link Frame} for a description of the return type */
    Collection getOwnFacetValues(Slot slot, Facet facet);

    boolean getOwnSlotAllowsMultipleValues(Slot slot);

    /**
     * Returns the own slot values for this slot and all of its subslots.
     * see {@link Frame} for a description of the return type
     */
    Collection getOwnSlotAndSubslotValues(Slot slot);

    /** see {@link Frame} for a description of the return type */
    Collection getOwnSlotDefaultValues(Slot slot);

    Collection getOwnSlotFacets(Slot slot);

    Collection getOwnSlotFacetValues(Slot slot, Facet facet);

    Collection getOwnSlots();

    /** see {@link Frame} for a description of the return type */
    Object getOwnSlotValue(Slot slot);

    int getOwnSlotValueCount(Slot slot);

    /** see {@link Frame} for a description of the return type */
    Collection getOwnSlotValues(Slot slot);

    ValueType getOwnSlotValueType(Slot slot);

    Project getProject();

    public Collection getReferences();

    Collection getReferences(int maxReferences);

    boolean hasOwnSlot(Slot slot);

    boolean isEditable();

    boolean isIncluded();

    boolean isSystem();

    boolean isValid();

    boolean isValidOwnSlotValue(Slot slot, Object item);

    boolean isVisible();

    public void moveOwnSlotValue(Slot slot, int fromIndex, int toIndex);

    void removeFrameListener(FrameListener listener);

    /** remove _all_ occurrances of this value */
    public void removeOwnSlotValue(Slot slot, Object value);

    void setDocumentation(String documentation);

    void setDocumentation(Collection documentation);

    void setEditable(boolean b);

    void setIncluded(boolean b);

    void setName(String newName);

    /** see {@link Frame} for a description of the value type */
    void setOwnFacetValue(Slot slot, Facet facet, Object value);

    /** see {@link Frame} for a description of the value type */
    void setOwnFacetValues(Slot slot, Facet facet, Collection values);

    /** see {@link Frame} for a description of the value type
     *
     * This method set the value of an own slot at a frame to a single value.  The value passed in cannot be a Collection.
     * To pass in a collection instead use #setOwnSlotValues (note the final "s").  This method can be called for
     * either cardinality-single slots or cardinality-multiple slots.  For a cardinality multiple slot the result is a
     * collection with a single element.
     */
    void setOwnSlotValue(Slot slot, Object value);

    /** see {@link Frame} for a description of the value type
     *
     * This method set the value of an own slot at a frame to a collection of values.
     * This method can be called for
     * either cardinality-single slots or cardinality-multiple slots.  For a cardinality single slot the collection
     * passed in must be have no more than one element.
     */
    void setOwnSlotValues(Slot slot, Collection values);

    void setVisible(boolean b);
}
