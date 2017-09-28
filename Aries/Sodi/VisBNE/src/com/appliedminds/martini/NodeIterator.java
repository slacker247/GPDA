package com.appliedminds.martini;

/**
 * Just like an iterator, but returns DrawableNodes.
 *
 * @author mathias@apmindsf.com
 */
public interface NodeIterator {

  /**
   * Returns true if the iteration has more elements. (In other words,
   * returns true if next would return an element rather than throwing
   * an exception.)
   *
   * @return true if the iterator has more elements.
   */
  boolean hasNext();

  /**
   * Returns the next element in the iteration.
   *
   * @return the next element in the iteration.
   */
  DrawableNode next();

} // end interface NodeIterator

