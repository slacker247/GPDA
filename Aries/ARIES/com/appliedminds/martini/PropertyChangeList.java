package com.appliedminds.martini;

import java.util.HashMap;
import java.util.Iterator;


/**
 * A collection PropertyChange records.
 *
 * @author daepark@apmindsf.com
 * @see PropertyChange
 */
public class PropertyChangeList {

  private HashMap _changeMap;


  /**
   * Initialize the collection.
   */
  public PropertyChangeList() {
    _changeMap = new HashMap();
  }


  /**
   * Add a PropertyChange record.
   *
   * @param change a PropertyChange record.
   */
  public void add(PropertyChange change) {
    _changeMap.put(change.getProperty(), change);
  }


  /**
   * @return a list of PropertyChange records from this collection.
   */
  public Iterator getChanges() {
    return (_changeMap.values().iterator());
  }


  /**
   * See if this collection contains a PropertyChange record for the
   * given property.
   *
   * @param property the property to look for
   * @return TRUE if this collection contains a PropertyChange record
   * for the give property, otherwise return FALSE.
   */
  public boolean hasPropertyChanged(String property) {
    return (_changeMap.containsKey(property));
  }


  /**
   * Clear this collection of all PropertyChange records.
   */
  public void clear() {
    _changeMap = new HashMap();
  }

        
  /**
   * @return the number of PropertyChange records in this collection.
   */ 
  public int size() {
    return (_changeMap.values().size());
  }

} // end class PropertyChangeList
