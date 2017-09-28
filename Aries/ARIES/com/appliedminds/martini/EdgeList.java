package com.appliedminds.martini;

import java.util.NoSuchElementException;

/**
 * A simple linked list for DrawableEdges.
 *
 * @author daepark@apmindsf.com
 */
public class EdgeList {

  private Element _head;
  private Element _tail;
  private int _len;


  /**
   * Create an empty list.
   */
  public EdgeList() {
    _head = new Element();
    _tail = new Element();
    _head.next = _tail;
    _tail.prev = _head;

    _len = 0;
  }


  /**
   * Add the specified edge to the end of the list.
   *
   * @param n the DrawableEdge to add.
   */
  public void add(DrawableEdge n) {
    Element newElt = new Element();

    newElt.edge = n;

    _tail.prev.next = newElt;
    newElt.prev = _tail.prev;

    _tail.prev = newElt;
    newElt.next = _tail;

    ++_len;
  }


  /**
   * Get the number of elements in this list.
   *
   * @return the number of elements in this list.
   */
  public int size() {
    return (_len);
  }


  /**
   * Test for the presence of the specified DrawableEdge in this list.
   *
   * @param n element whose presence in this list is to be tested.
   * @return true if this list contains the specified element.
   */
  public boolean contains(DrawableEdge n) {
    Element current = _head;

    while (current.next != _tail) {
      current = current.next;

      if (current.edge == n) {
        return (true);
      }
    }

    return (false);
  }


  /**
   * Remove the first occurence of the specified edge from this list.
   *
   * @param n element to be removed from this list, if present.
   */
  public void remove(DrawableEdge n) {
    Element current = _head;

    while (current.next != _tail) {
      current = current.next;

      if (current.edge == n) {
        current.next.prev = current.prev;
        current.prev.next = current.next;
        current = null;

        --_len;

        return;
      }
    }
  }


  /**
   * Removes all of the elements from this list..
   */
  public void clear() {
    _head.next = _tail;
    _tail.prev = _head;

    _len = 0;
  }


  /**
   * Get an EdgeIterator over the elements in this list.
   *
   * @return an EdgeIterator over the elements in this list.
   */
  public EdgeIterator iterator() {
    return(new EdgeIterator() {
        private Element __current = _head;

        public boolean hasNext() {
          return(__current.next != _tail);
        }
        public DrawableEdge next() {
          __current = __current.next;

          if (__current == _tail) {
            throw (new NoSuchElementException());
          }

          return(__current.edge);
        }
      });
  }


  //
  // Simple struct that supports linked behavior.
  //
  class Element {
    public Element next;
    public Element prev;
    public DrawableEdge edge;
  }

} // end class "EdgeList"
