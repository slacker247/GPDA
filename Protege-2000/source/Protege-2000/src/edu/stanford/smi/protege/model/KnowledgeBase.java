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

import edu.stanford.smi.protege.event.*;
import edu.stanford.smi.protege.util.*;

import java.util.*;

/**
 *  A container for frames.  Frame creation is funneled through here.
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public interface KnowledgeBase extends Disposable {
    int MAXIMUM_CARDINALITY_UNBOUNDED = -1;

    /**
     * This method requires some background information.  When Protege wants to
     * create a simple instance in the kb it creates an instance of the java
     * class "DefaultSimpleInstance".  Programmers can subclass this class and
     * give the subclass the name of a class in the kb.  Then when protege wants
     * to create an instance of the class in the kb it will instead make an
     * instance of the programmers java class rather than DefaultSimpleInstance.
     * In order for this to work the user must specify the package that their
     * java class appears in.  (The class file must also be in the plugins directory.)
     * For example, consider a protege project with a class A.  Now a programmer
     * creates a subclass of model.DefaultSimpleInstance and calls it org.me.A.
     * He puts this .class file in the plugins/org/me directory.
     * (Presumably he creates the subclass  this in order to add additional
     * methods onto org.me.A
     * to provide some sort of desired functionality.  The desired behavior is
     * that when the user creates an instance of the kb class A that the system
     * will create an instance of org.me.A and put it in the kb.  Then when the
     * programmer queries the kb for "get me instances of A" he will get back instances
     * which can be cast to org.me.A.
     *
     * So, what this method does is to tell the system what package to search to
     * find the java class to create.  When the system creates an instance of
     * A it searches all the packages, in order, to find a java class with the
     * name <package_name>.A.  If it finds one then it uses it to create an instance.
     * If it doesn't find any matches then it creates an instance of
     * DefaultSimpleInstance.
     */
    void addJavaLoadPackage(String path);

    void addKnowledgeBaseListener(KnowledgeBaseListener listener);

    boolean areValidOwnSlotValues(Frame frame, Slot slot, Collection values);

    void changeFrameName(Frame oldFrame, String newFrameName);

    boolean containsFrame(String name);

    Cls createCls(String name, Collection parents);

    Cls createCls(String name, Collection parents, Cls metaCls);

    Cls createCls(String name, Collection parents, Cls metaCls, boolean isNew);

    Facet createFacet(String name);

    Facet createFacet(String name, Cls metaCls);

    Facet createFacet(String name, Cls metaCls, boolean isNew);

    Instance createInstance(String name, Cls cls);

    Instance createInstance(String name, Cls cls, boolean isNew);

    Slot createSlot(String name);

    Slot createSlot(String name, Cls metaCls);

    Slot createSlot(String name, Cls metaCls, Collection superslots, boolean isNew);

    Slot createSlot(String name, Cls metaCls, boolean isNew);

    /**
     *
     *
     * @deprecated    pass "null" in as a frame name to a create method to get a
     *      gensym name
     */
    String createUniqueFrameName(String name);

    void deleteCls(Cls cls);

    void deleteFacet(Facet facet);

    void deleteFrame(Frame frame);

    void deleteInstance(Instance instance);

    void deleteSlot(Slot slot);

    /**
     * Returns a string that contains something like "build 840" that refers to the
     * build of Protege in use when this kb was last saved.
     * This string format is not reliable and may change.
     */
    String getBuildString();

    /**
     * Allows a programmer to hang arbitrary information on a kb
     * and retrieve it later.  This information is not persistent.
     * Since all programmers share the same "client information space" we
     * recommend that your "key" be your java.lang.Class object.  If you want
     * to store more than one piece of information then you can make your
     * value a set (or a map).  Thus the common usage would be:
     * <pre> <code>
     *         Map myGoodStuff = new HashMap();
     *      myGoodStuff.put("foo", "bar");
     *       kb.setClientInformation(myClass.class, myGoodStuff);
     *        // ... later
     *       Map myGoodStuff = (Map) kb.getClientInformation(myClass.class);
     * </code></pre>
     *
     * Widget writers typically don't need this sort of thing but backend
     * authors do because one instance of the backend may need to communicate
     * information to another instance (the loader to the storer, for example).
     * This is very similar to the "client property" feature on
     * javax.swing.JComponent or the client information on MS Windows windows
     * and it exists for the same reasons.  It allows you to store anything
     * you want.
     */
    Object getClientInformation(Object key);

    Cls getCls(String name);

    int getClsCount();

    Collection getClses();

    /**
     * Not a regexp.  Allows "*" for "match any sequence" at
     * the begining or end.
     *
     * Case-sensitivity is screwy.  For systems
     * with all of the information in memory (text files) then the search
     * is case-insensitive.  For database backend systems then the case-sensitivity
     * is determined by the case-sensitivety of searching in the database.  This
     * means that mySQL and MS Access provide case-insensitive searchs while Oracle
     * provides case-sensitive searches.  This different behavior is less than ideal and will be dealt with someday.
     */

    Collection getClsNameMatches(String s, int maxMatches);

    Cls getDefaultClsMetaCls();

    Cls getDefaultFacetMetaCls();

    Cls getDefaultSlotMetaCls();

    Facet getFacet(String name);

    int getFacetCount();

    Collection getFacets();

    Frame getFrame(String name);

    int getFrameCount();

    String getFrameCreationTimestamp(Frame frame);

    String getFrameCreator(Frame frame);

    String getFrameLastModificationTimestamp(Frame frame);

    String getFrameLastModifier(Frame frame);

    Collection getFrameNameMatches(String s, int maxMatches);

    String getFrameNamePrefix();

    Collection getFrames();

    Instance getInstance(String fullname);

    Collection getInstances();

    /**
     *
     *
     * @deprecated    use cls.getInstances()
     */
    Collection getInstances(Cls cls);

    String getInvalidOwnSlotValuesText(Frame frame, Slot slot, Collection values);

    String getInvalidOwnSlotValueText(Frame frame, Slot slot, Object value);

    KnowledgeBaseFactory getKnowledgeBaseFactory();

    Collection getMatchingFrames(Slot slot, Facet facet, boolean isTemplate, String s, int maxMatches);

    String getName();

    int getNextFrameNumber();

    Project getProject();

    Collection getReachableSimpleInstances(Collection roots);

    /**
     * @returns A collection of #Reference instances.
     */
    Collection getReferences(Object o, int maxReferences);

    Cls getRootCls();

    /**
     * A convenience method that returns ":THING" wrapped in a collection.
     */
    Collection getRootClses();

    Cls getRootClsMetaCls();

    Cls getRootFacetMetaCls();

    Cls getRootSlotMetaCls();

    Collection getRootSlots();

    Slot getSlot(String name);

    int getSlotCount();

    Collection getSlots();

    String getSlotValueLastModificationTimestamp(Frame frame, Slot slot, boolean isTemplate);

    String getSlotValueLastModifier(Frame frame, Slot slot, boolean isTemplate);

    /**
     *
     *
     * @deprecated    use cls.getSubclasses()
     */
    Collection getSubclasses(Cls cls);

    Collection getUnreachableSimpleInstances(Collection roots);

    String getUserName();

    /*
     * Returns a string that contains something like "Version 1.6.2" that refers to the
     * version of Protege in use when this kb was last saved.
     * This string format is not reliable and may change.
     */
    String getVersionString();

    boolean hasChanged();

    boolean isAutoUpdatingFacetValues();

    boolean isClsMetaCls(Cls cls);

    boolean isDefaultClsMetaCls(Cls cls);

    boolean isDefaultFacetMetaCls(Cls cls);

    boolean isDefaultSlotMetaCls(Cls cls);

    boolean isFacetMetaCls(Cls cls);

    boolean isLoading();

    boolean isSlotMetaCls(Cls cls);

    boolean isValidOwnSlotValue(Frame frame, Slot slot, Object value);

    void removeJavaLoadPackage(String path);

    void removeKnowledgeBaseListener(KnowledgeBaseListener listener);

    void setAutoUpdateFacetValues(boolean b);

    void setBuildString(String s);

    void setChanged(boolean b);

    Object setClientInformation(Object key, Object value);

    void setDefaultClsMetaCls(Cls cls);

    void setDefaultFacetMetaCls(Cls cls);

    void setDefaultSlotMetaCls(Cls cls);

    boolean setEventsEnabled(boolean enabled);

    void setFrameNamePrefix(String name);

    void setLoading(boolean b);

    void setName(String name);

    void setNextFrameNumber(int i);

    void setProject(Project project);

    void setUserName(String name);

    void setValueChecking(boolean b);

    void setVersionString(String s);
}
