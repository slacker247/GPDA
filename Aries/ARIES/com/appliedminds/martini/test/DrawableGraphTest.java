package com.appliedminds.martini.test;

import javax.swing.JWindow;
import java.util.Iterator;
import java.util.ArrayList;
import junit.framework.*;
import com.appliedminds.martini.*;
import java.awt.event.MouseEvent;
import java.awt.geom.Rectangle2D;


public class DrawableGraphTest extends TestCase {


  private JWindow _component;



  public DrawableGraphTest(String name) {
    super(name);
  }


  public void setUp() {
    _component = new JWindow();
  }


  public void tearDown() {
    _component.dispose();
  }


  /**
   * Test the get/set/remove property methods.
   */
  public void testProperties() {
    try {
      DrawableGraph g = new DrawableGraph();

      g.setProperty("foo", "bar");
      g.setProperty("spy", "plane");

      assertEquals("Failed for 'foo'", "bar", g.getProperty("foo"));
      assertEquals("Failed for 'spy'", "plane", g.getProperty("spy"));

      Iterator it = g.getPropertyKeys();
      ArrayList list = new ArrayList();
      while(it.hasNext()) {
        list.add(it.next());
      }
      assertTrue("Key 'foo' missing!", list.contains("foo"));
      assertTrue("Key 'spy' missing!", list.contains("spy"));

      g.setProperty("foo", "foot");
      assertEquals("Failed for update of 'foo'", "foot", g.getProperty("foo"));

      g.removeProperty("spy");
      assertTrue("Failed to remove 'spy'", g.getProperty("spy") == null);
    }
    catch(Exception e) {
      e.printStackTrace();
      fail("ERR: " + e);
    }
  }


  /**
   * Test createEdge and also check the edge out a bit, and removeEdge.
   */
  public void testCreateEdge() {
    try {
      DrawableGraph g = new DrawableGraph();

      DrawableNode n1 = g.createNode();
      DrawableNode n2 = g.createNode();

      DrawableEdge e = g.createEdge(n1, n2); // head,tail

      EdgeIterator i = g.edgesIterator();
      assertTrue(i.hasNext());
      DrawableEdge r = i.next();
      assertEquals("Got wrong edge from iterator", e, r);
      assertTrue(! i.hasNext());

      assertEquals("Bad head node", n1, e.getHead());
      assertEquals("Bad tail node", n2, e.getTail());

      // remove it
      g.remove(e);
      i = g.edgesIterator();
      assertTrue("Failed to remove edge", ! i.hasNext());
    }
    catch(Exception e) {
      e.printStackTrace();
      fail("ERR: " + e);
    }
  }


  /**
   * Test createNode and removeNode
   */
  public void testCreateNode() {
    try {
      DrawableGraph g = new DrawableGraph();

      DrawableNode n1 = g.createNode();
      DrawableNode n2 = g.createNode();
      DrawableNode n3 = g.createNode();

      NodeIterator i = g.nodesIterator();
      ArrayList list = new ArrayList();
      while(i.hasNext()) {
        list.add(i.next());
      }

      assertEquals("Wrong number of nodes", 3, list.size());
      assertTrue(list.contains(n1));
      assertTrue(list.contains(n2));
      assertTrue(list.contains(n3));

      // test remove
      g.remove(n1);
      i = g.nodesIterator();
      list.clear();
      while(i.hasNext()) {
        list.add(i.next());
      }
      assertEquals("Wrong number of nodes", 2, list.size());
      assertTrue(list.contains(n2));
      assertTrue(list.contains(n3));

      g.remove(n2);
      g.remove(n3);

      i = g.nodesIterator();
      assertTrue(! i.hasNext());
    }
    catch(Exception e) {
      e.printStackTrace();
      fail("ERR: " + e);
    }
  }


  /**
   * Test out the HitTestListener callbacks.
   */
  public void testHitTestListener() {
    try {
      Htl tester = new Htl();
      DrawableGraph g = new DrawableGraph();
      g.addHitTestListener(tester);

      g.notifyHitMissedGraph(newMouseEvent());
      assertEquals("notifyHitMissedGraph broken",
                   1,
                   tester.calledHitTestMissedGraph);
      tester.reset();

      g.notifyHitTestSuccess("oink", g.createNode(), newMouseEvent(), "blah");
      assertEquals("notifyHitTestSuccess broken",
                   1,
                   tester.calledHitTestSuccess);
      tester.reset();

      g.notifyMultipleHitTestMissedGraph(new Rectangle2D.Double(0,0,0,0));
      assertEquals("notifyMultipleHitTestMissedGraph broken",
                   1,
                   tester.calledMultipleHitTestMissedGraph);
      tester.reset();

      g.notifyMultipleHitTestSuccess(new Object[]{},
                                     new Rectangle2D.Double(0,0,0,0));
      assertEquals("notifyMultipleHitTestSuccess broken",
                   1,
                   tester.calledMultipleHitTestSuccess);
      tester.reset();

    }
    catch(Exception e) {
      e.printStackTrace();
      fail("ERR: " + e);
    }
  }


  /*
   * Create a dummy mouse event.
   */
  private MouseEvent newMouseEvent() {
    return(new MouseEvent(_component, 0, 0L, 0, 0, 0, 0, false));
  }


  /*
   * A dummy HitTestListener for testing.
   */
  private class Htl implements HitTestListener {
    public int calledHitTestMissedGraph;
    public int calledHitTestSuccess;
    public int calledMultipleHitTestMissedGraph;
    public int calledMultipleHitTestSuccess;

    public Htl() {
      reset();
    }

    public void reset() {
      calledHitTestMissedGraph = 0;
      calledHitTestSuccess = 0;
      calledMultipleHitTestMissedGraph = 0;
      calledMultipleHitTestSuccess = 0;
    }


    public boolean hitTestSuccess(String decorationId,
                                  DrawableGraphElement el,
                                  Object ctx,
                                  MouseEvent evt)
    {
      ++calledHitTestSuccess;
      return(false);
    }

    public void hitTestMissedGraph(MouseEvent evt) {
      ++calledHitTestMissedGraph;
    }

    public void multipleHitTestSuccess(Object[] hitElements,
                                       Rectangle2D rect)
    {
      ++calledMultipleHitTestSuccess;
    }


    public void multipleHitTestMissedGraph(Rectangle2D rect) {
      ++calledMultipleHitTestMissedGraph;
    }
  }// end private class "Htl"

}
