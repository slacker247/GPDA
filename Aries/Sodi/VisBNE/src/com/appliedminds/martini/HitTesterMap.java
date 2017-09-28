package com.appliedminds.martini;


import java.util.HashMap;
import java.util.LinkedList;
import java.util.Iterator;
import java.util.NoSuchElementException;


/**
 * This class keeps track of the relationship between
 * DrawableGrapheElements (nodes and edges) and their HitTester
 * objects.  
 *
 * <P>HitTester objects hold actual screen coordinates related to the
 * nodes and edges.  This data can not be stored in the DrawableGraph
 * since we may be displaying the same grapn in multiple windows
 * (which implies mutliple viewports).  Therefore, we use this
 * separate object.
 *
 * <p>The GraphUI objects are supposed to store HitTester objects in
 * here.  When you add hit testers to a node or edge, the hit testers
 * that you add later are "on top" of the ones added earlier.  When
 * you request a list of the hit testers they are returned in order
 * from the topmost to the bottom most.
 *
 * <P>Hit testing is performed by the DrawableGraphElement.
 *
 *
 * @author mathias@apmindsf.com
 * @author will@apmindsf.com
 */
public class HitTesterMap {


  private static Iterator EMPTY_ITERATOR =
    new Iterator() {
      public boolean hasNext() { return false; }
      public Object next() { throw new NoSuchElementException(); }
      public void remove() { throw new UnsupportedOperationException(); }
    };


  private HashMap _map;
  private HashMap _disabledMap; // holds disabled hitTesters


  /** 
   * Create a new, empty, hit tester map.
   */
  public HitTesterMap() {
    _map = new HashMap();
    _disabledMap = new HashMap();
  }


  /** 
   * Add a hit tester to a graph element (node or edge).  Hit testers
   * for a particular node or edge are added such that later additions
   * are "on top of" earlier additions.  Well, that is the idea
   * anyway.  Actual interpretation of the ordering is up to the code
   * that performs the hit testing.
   *
   * @param e the node or edge.
   * @param t the hit tester.
   */
  public void addHitTester(DrawableGraphElement e, HitTester t) {
    LinkedList list = (LinkedList) _map.get(e);
    if (list == null) {
      list = new LinkedList();
      _map.put(e, list);
    }
    list.addFirst(t);
  }


  /**
   * Remove all hit testers from a node or edge.
   *
   * @param e the node or edge.
   */
  public void removeAllHitTesters(DrawableGraphElement e) {
    _map.remove(e);
  }


  /**
   * List hit testers.  Hit testers are returned in the opposite order
   * in which they are added.
   *
   * @param e the node or edge.
   * @return a list of HitTester objects.
   */
  public Iterator listHitTesters(DrawableGraphElement e) {
    LinkedList list = (LinkedList) _map.get(e);
    if (list != null) {
      return(list.iterator());
    }
    else {
      return(EMPTY_ITERATOR);
    }
  }


  /**
   * Disable the HitTester instance for a DrawableGraphElement. It may
   * be reenabled later.
   *
   * @param e the graph element whose HitTester we want to disable.
   */
  public void disableHitTester(DrawableGraphElement e) {
    _disabledMap.put(e, _map.get(e));
    _map.remove(e);
  }


  /**
   * Enable a previously disabled HitTester for a graph element.
   *
   * @param e the graph element whose HitTesters we want to reenable.
   */
  public void enableHitTester(DrawableGraphElement e) {
    _map.put(e, _disabledMap.get(e));
    _disabledMap.remove(e);
  }

} // end class HitTesterMap
