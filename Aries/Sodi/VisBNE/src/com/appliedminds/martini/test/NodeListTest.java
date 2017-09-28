package com.appliedminds.martini.test;


import junit.framework.*;

import com.appliedminds.martini.*;


public class NodeListTest extends TestCase {

  public NodeListTest(String name) {
    super(name);
  }


  public void testAdd() {
    // start with size 3
    NodeList list = new NodeList();

    DrawableNode a = new DrawableNode();
    a.setProperty("name", "a");
    DrawableNode b = new DrawableNode();
    b.setProperty("name", "b");
    DrawableNode c = new DrawableNode();
    c.setProperty("name", "c");
    DrawableNode d = new DrawableNode();
    d.setProperty("name", "d");

    list.add(a);
    list.add(b);
    list.add(c);

    // adding this should grow the list
    list.add(d);

    printDebug(list, "name");

    assertEquals("NodeList should have 4 elements", 4, list.size());
  }


  public void testRemove() {
    // start with size 3
    NodeList list = new NodeList();

    DrawableNode a = new DrawableNode();
    a.setProperty("name", "a");
    DrawableNode b = new DrawableNode();
    b.setProperty("name", "b");
    DrawableNode c = new DrawableNode();
    c.setProperty("name", "c");
    DrawableNode d = new DrawableNode();
    d.setProperty("name", "d");
    DrawableNode e = new DrawableNode();
    e.setProperty("name", "e");

    list.add(a);
    list.add(b);
    list.add(c);
    list.add(d);
    list.add(e);

    printDebug(list, "name");

    list.remove(c);
    printDebug(list, "name");
    assertEquals("NodeList should have 4 elements", 4, list.size());

    list.remove(a);
    printDebug(list, "name");
    assertEquals("NodeList should have 3 elements", 3, list.size());

    list.remove(e);
    printDebug(list, "name");
    assertEquals("NodeList should have 3 elements", 2, list.size());
  }



  private void printDebug(NodeList list, String propName) {
    StringBuffer buf = new StringBuffer();
    for (NodeIterator itr = list.iterator(); itr.hasNext();) {
      buf.append(itr.next().getProperty(propName)).append(", ");
    }

    System.err.println(buf.toString());
  }
}
