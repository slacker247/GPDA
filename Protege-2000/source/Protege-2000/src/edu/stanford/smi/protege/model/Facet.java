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
 * A description of a constraint on the value of a frame-slot binding.
 * This constraint may be placed on any number of frame-slot bindings.
 *
 * @author Ray Fergerson <fergerson@smi.stanford.edu>
 */
public interface Facet extends Instance {

    void addFacetListener(FacetListener listener);

    boolean areValidValues(Frame frame, Slot slot, Collection values);

    Slot getAssociatedSlot();

    FacetConstraint getConstraint();

    String getInvalidValuesText(Frame frame, Slot slot, Collection values);

    String getInvalidValueText(Frame frame, Slot slot, Object item);

    ValueType getValueType();

    boolean isValidValue(Frame frame, Slot slot, Object value);

    void removeFacetListener(FacetListener listener);

    void setAssociatedSlot(Slot slot);

    void setConstraint(FacetConstraint constraint);
}
