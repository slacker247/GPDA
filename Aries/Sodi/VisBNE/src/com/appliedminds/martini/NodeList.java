package com.appliedminds.martini;

import java.util.NoSuchElementException;

/**
 * A simple linked list for DrawableNodes.
 *
 * @author daepark@apmindsf.com
 */
public class NodeList {

  private Element _head;
  private Element _tail;
  private int _len;


  /**
   * Create an empty list.
   */
  public NodeList() {
    _head = new Element();
    _tail = new Element();
    _head.next = _tail;
    _tail.prev = _head;

    _len = 0;
  }


  /**
   * Add the specified node to the end of the list.
   *
   * @param n the DrawableNode to add.
   */
  public void add(DrawableNode n) {
    Element newElt = new Element();

    newElt.node = n;

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
   * Test for the presence of the specified DrawableNode in this list.
   *
   * @param n element whose presence in this list is to be tested.
   * @return true if this list contains the specified element.
   */
  public boolean contains(DrawableNode n) {
    Element current = _head;

    while (current.next != _tail) {
      current = current.next;

      if (current.node == n) {
        return (true);
      }
    }

    return (false);
  }


  /**
   * Remove the first occurence of the specified node from this list.
   *
   * @param n element to be removed from this list, if present.
   */
  public void remove(DrawableNode n) {
    Element current = _head;

    while (current.next != _tail) {
      current = current.next;

      if (current.node == n) {
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
   * Get an NodeIterator over the elements in this list.
   *
   * @return an NodeIterator over the elements in this list.
   */
  public NodeIterator iterator() {
    return(new NodeIterator() {
        private Element __current = _head;

        public boolean hasNext() {
          return(__current.next != _tail);
        }
        public DrawableNode next() {
          __current = __current.next;

          if (__current == _tail) {
            throw (new NoSuchElementException());
          }

          return(__current.node);
        }
      });
  }


  //
  // Simple struct that supports linked behavior.
  //
  class Element {
    public Element next;
    public Element prev;
    public DrawableNode node;
  }

} // end class "NodeList"
