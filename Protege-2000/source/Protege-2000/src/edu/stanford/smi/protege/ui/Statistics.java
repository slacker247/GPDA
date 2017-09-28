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
import javax.swing.*;
import javax.swing.table.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.model.Frame;
import edu.stanford.smi.protege.util.*;

class Statistics {

    // -- Frames
    public int nFrames;
    public int nSystemFrames;
    public int nIncludedFrames;
    public int nDirectFrames;

    // -- Cls
    public int nClses;
    public int nSystemClses;
    public int nIncludedClses;
    public int nDirectClses;

    public int maxParents;
    private long sumParents;
    private long squareParents;
    public double meanParents;
    public double sdParents;

    public int maxChildren;
    private long sumChildren;
    private long squareChildren;
    public double meanChildren;
    public double sdChildren;

    public int maxClsRelations;
    public long sumClsRelations;
    public long squareClsRelations;
    public double meanClsRelations;
    public double sdClsRelations;

    public int maxDirectSlots;
    private long sumDirectSlots;
    private long squareDirectSlots;
    public double meanDirectSlots;
    public double sdDirectSlots;

    public int maxSlots;
    private long sumSlots;
    private long squareSlots;
    public double meanSlots;
    public double sdSlots;

    public int maxDirectInstances;
    private long sumDirectInstances;
    private long squareDirectInstances;
    public double meanDirectInstances;
    public double sdDirectInstances;

    // -- Slot
    public int nSlots;
    public int nSystemSlots;
    public int nIncludedSlots;
    public int nDirectSlots;
    public int maxClsesDirectlyAttached;
    public double meanClsesDirectlyAttached;
    public double sdClsesDirectlyAttached;
    public long sumClsesDirectlyAttached;
    public long squareClsesDirectlyAttached;

    // Facet
    public int nFacets;
    public int nSystemFacets;
    public int nIncludedFacets;
    public int nDirectFacets;
    public int maxDirectBindings;
    public double meanDirectBindings;
    public double sdDirectBindings;
    public long sumDirectBindings;
    public long squareDirectBindings;

    // Simple Instance
    public int nInstances;
    public int nSystemInstances;
    public int nIncludedInstances;
    public int nDirectInstances;
    public int maxInstanceReferences;
    public double meanInstanceReferences;
    public double sdInstanceReferences;
    public long sumInstanceReferences;
    public long squareInstanceReferences;
    public int maxInstanceReferencers;
    public double meanInstanceReferencers;
    public double sdInstanceReferencers;
    public long sumInstanceReferencers;
    public long squareInstanceReferencers;
    private Map referencerCount = new HashMap();

    public Statistics(KnowledgeBase kb) {
        Iterator i = new ArrayList(kb.getFrames()).iterator();
        while (i.hasNext()) {
            Frame frame = (Frame) i.next();
            updateFrameStatistics(frame);
            if (frame instanceof Cls) {
                updateClsStatistics((Cls) frame);
            } else if (frame instanceof Slot) {
                updateSlotStatistics((Slot) frame);
            } else if (frame instanceof Facet) {
                updateFacetStatistics((Facet) frame);
            } else {
                updateInstanceStatistics((Instance) frame);
            }
        }
        finishFrameCalculations();
        finishClsCalculations();
        finishSlotCalculations();
        finishFacetCalculations();
        finishInstanceCalculations();
    }

    private void finishClsCalculations() {
        meanParents = mean(nClses, sumParents);
        sdParents = sd(nClses, meanParents, sumParents, squareParents);
        meanChildren = mean(nClses, sumChildren);
        sdChildren = sd(nClses, meanChildren, sumChildren, squareChildren);
        meanDirectSlots = mean(nClses, sumDirectSlots);
        sdDirectSlots = sd(nClses, meanDirectSlots, sumDirectSlots, squareDirectSlots);
        meanSlots = mean(nClses, sumSlots);
        sdSlots = sd(nClses, meanSlots, sumSlots, squareSlots);
        meanDirectInstances = mean(nClses, sumDirectInstances);
        sdDirectInstances = sd(nClses, meanDirectInstances, sumDirectInstances, squareDirectInstances);
        meanClsRelations = mean(nClses, sumClsRelations);
        sdClsRelations = sd(nClses, meanClsRelations, sumClsRelations, squareClsRelations);
    }

    private void finishFacetCalculations() {
        meanDirectBindings = mean(nFacets, sumDirectBindings);
        sdDirectBindings = sd(nFacets, meanDirectBindings, sumDirectBindings, squareDirectBindings);
    }

    private void finishFrameCalculations() {
    }

    private void finishInstanceCalculations() {
        meanInstanceReferences = mean(nInstances, sumInstanceReferences);
        sdInstanceReferences = sd(nInstances, meanInstanceReferences, sumInstanceReferences,
                squareInstanceReferences);

        Iterator i = referencerCount.values().iterator();
        while (i.hasNext()) {
            Integer nRefs = (Integer) i.next();
            int refs = nRefs.intValue();
            maxInstanceReferencers = Math.max(maxInstanceReferencers, refs);
            sumInstanceReferencers += refs;
            squareInstanceReferencers += refs * refs;
        }
        meanInstanceReferencers = mean(nInstances, sumInstanceReferencers);
        sdInstanceReferencers = sd(nInstances, meanInstanceReferencers, sumInstanceReferencers,
                squareInstanceReferencers);
    }

    private void finishSlotCalculations() {
        meanClsesDirectlyAttached = mean(nSlots, sumClsesDirectlyAttached);
        sdClsesDirectlyAttached = sd(nSlots, meanClsesDirectlyAttached, sumClsesDirectlyAttached,
                squareClsesDirectlyAttached);
    }

    private int getDirectTemplateSlotCount(Cls cls) {
        int directTemplateSlotCount = 0;
        Iterator i = cls.getTemplateSlots().iterator();
        while (i.hasNext()) {
            Slot slot = (Slot) i.next();
            if (cls.hasDirectTemplateSlot(slot)) {
                ++directTemplateSlotCount;
            }
        }
        return directTemplateSlotCount;
    }

    private double mean(int n, long sum) {
        return (n == 0) ? 0 : ((double) sum) / n;
    }

    private double sd(int n, double mean, long sum, long square) {
        double variance = (n == 0) ? 0 : (n * mean * mean + square - 2 * mean * sum) / (n - 1);
        return Math.sqrt(variance);
    }

    private void updateClsStatistics(Cls cls) {
        ++nClses;
        if (cls.isSystem()) {
            ++nSystemClses;
        } else if (cls.isIncluded()) {
            ++nIncludedClses;
        } else {
            ++nDirectClses;
        }

        int nParents = cls.getDirectSuperclasses().size();
        maxParents = Math.max(maxParents, nParents);
        sumParents += nParents;
        squareParents += nParents * nParents;

        int nChildren = cls.getDirectSubclasses().size();
        maxChildren = Math.max(maxChildren, nChildren);
        sumChildren += nChildren;
        squareChildren += nChildren * nChildren;

        int nDirectSlots = getDirectTemplateSlotCount(cls);
        maxDirectSlots = Math.max(maxDirectSlots, nDirectSlots);
        sumDirectSlots += nDirectSlots;
        squareDirectSlots += nDirectSlots * nDirectSlots;

        int nSlots = cls.getTemplateSlots().size();
        maxSlots = Math.max(maxSlots, nSlots);
        sumSlots += nSlots;
        squareSlots += nSlots * nSlots;

        int nDirectInstances = cls.getDirectInstanceCount();
        maxDirectInstances = Math.max(maxDirectInstances, nDirectInstances);
        sumDirectInstances += nDirectInstances;
        squareDirectInstances += nDirectInstances * nDirectInstances;

        int nClsRelations = 0;
        Iterator i = cls.getTemplateSlots().iterator();
        while (i.hasNext()) {
            Slot slot = (Slot) i.next();
            if (cls.getTemplateSlotValueType(slot) == ValueType.INSTANCE) {
                nClsRelations += cls.getTemplateSlotAllowedClses(slot).size();
            }
        }
        maxClsRelations = Math.max(maxClsRelations, nClsRelations);
        sumClsRelations += nClsRelations;
        squareClsRelations += nClsRelations * nClsRelations;
    }

    private void updateFacetStatistics(Facet facet) {
        ++nFacets;
        if (facet.isSystem()) {
            ++nSystemFacets;
        } else if (facet.isIncluded()) {
            ++nIncludedFacets;
        } else {
            ++nDirectFacets;
        }
        int nBindings = 0;
        // facet.getFrameSlotPairs().size();
        sumDirectBindings += nBindings;
        squareDirectBindings += nBindings * nBindings;
        maxDirectBindings = Math.max(nBindings, maxDirectBindings);
    }

    private void updateFrameStatistics(Frame frame) {
        ++nFrames;
        if (frame.isSystem()) {
            ++nSystemFrames;
        } else if (frame.isIncluded()) {
            ++nIncludedFrames;
        } else {
            ++nDirectFrames;
        }
    }

    // <Instance, nreferencers>
    private void updateInstanceStatistics(Instance instance) {
        ++nInstances;
        if (instance.isSystem()) {
            ++nSystemInstances;
        } else if (instance.isIncluded()) {
            ++nIncludedInstances;
        } else {
            ++nDirectInstances;
        }
        int nReferences = 0;
        Iterator i = instance.getOwnSlots().iterator();
        while (i.hasNext()) {
            Slot slot = (Slot) i.next();
            if (instance.getOwnSlotValueType(slot) == ValueType.INSTANCE) {
                Collection references = instance.getOwnSlotValues(slot);
                nReferences += references.size();
                Iterator j = references.iterator();
                while (j.hasNext()) {
                    Instance reference = (Instance) j.next();
                    Integer nrefs = (Integer) referencerCount.get(reference);
                    if (nrefs == null) {
                        nrefs = new Integer(1);
                    } else {
                        nrefs = new Integer(nrefs.intValue() + 1);
                    }
                    referencerCount.put(reference, nrefs);
                }
            }
        }
        maxInstanceReferences = Math.max(maxInstanceReferences, nReferences);
        sumInstanceReferences += nReferences;
        squareInstanceReferences += nReferences * nReferences;
    }

    private void updateSlotStatistics(Slot slot) {
        ++nSlots;
        if (slot.isSystem()) {
            ++nSystemSlots;
        } else if (slot.isIncluded()) {
            ++nIncludedSlots;
        } else {
            ++nDirectSlots;
        }

        int nClses = slot.getTemplateSlotClses().size();
        maxClsesDirectlyAttached = Math.max(maxClsesDirectlyAttached, nClses);
        sumClsesDirectlyAttached += nClses;
        squareClsesDirectlyAttached += nClses * nClses;
    }
}
