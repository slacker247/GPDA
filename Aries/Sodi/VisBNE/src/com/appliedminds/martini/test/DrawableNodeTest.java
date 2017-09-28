package com.appliedminds.martini.test;


import java.awt.Frame;
import junit.framework.*;
import com.appliedminds.martini.*;
import java.awt.geom.Rectangle2D;
import java.awt.event.MouseEvent;
import java.util.Iterator;
import java.util.ArrayList;



/**
 * @author mathias@apmindsf.com
 */
public class DrawableNodeTest extends TestCase {


  public DrawableNodeTest(String name) {
    super(name);
  }


  /**
   * Test hit testing for the node.
   */
  public void testHitTest() {
      Frame dummy = new Frame();
      try {
        MouseEvent event = new MouseEvent(dummy, MouseEvent.MOUSE_PRESSED, System.currentTimeMillis(), 0, 0, 0, 1, false);

      DrawableGraph g = new DrawableGraph();
      DrawableNode n = g.createNode();
      HitTesterMap map = new HitTesterMap();

      Rectangle2D shape = new Rectangle2D.Double(0, 10, 10, 10);
      HitTester tester = new HitTester(shape, "blah", n);
      map.addHitTester(n, tester);

      Rectangle2D ptr = new Rectangle2D.Double(5, 15, 1, 1);

      assertTrue("INSANE!", shape.contains(ptr));
      assertTrue("HitTester is broken", tester.hitTest(ptr));

      //
      // Simple test with a single hit tester:
      //
      HitTestResult res = n.hitTest(map, ptr, event);
      assertTrue("Hit test missed", res.wasHit());

      //
      // Now add a second, overlapping tester with no hit being consumed.
      //
      Rectangle2D shape2 = new Rectangle2D.Double(3, 13, 4, 4);
      HitTester tester2 = new HitTester(shape2, "blah2", n);
      map.addHitTester(n, tester2);

      assertTrue("INSANE! (2)", shape2.contains(ptr));

      HitTestResult res2 = n.hitTest(map, ptr, event);
      assertTrue("Hit test missed (2)", res.wasHit());

      //
      // Since nothing is consuming the hits, I expect the "deepest"
      // object to be the one returned from the hit test method.
      //

      HitTester resHt = res2.getHitTester();
      assertEquals("Wrong hit tester returned", tester, resHt);

      //
      // Now, lets add a consumer for the smaller shape above.
      //

      g.addHitTestListener
        (new HitTestListener() {
            public boolean hitTestSuccess(String id,
                                          DrawableGraphElement el,
                                          Object context,
                                          MouseEvent e)
            {
              return("blah2".equals(id));
            }
            public void hitTestMissedGraph(MouseEvent evt) { }
            public void multipleHitTestSuccess(Object[] hitElements,
                                               Rectangle2D rect) { }
            public void multipleHitTestMissedGraph(Rectangle2D rect) { }
          });


      //
      // Then when we hit test, we should get back the small shape.
      //

      HitTestResult res3 = n.hitTest(map, ptr, event);
      assertTrue("Hit test missed (3)", res3.wasHit());
      assertTrue("Failed to consume!", res3.wasConsumed());
      assertEquals("Wrong hit tester returned (2)", tester2, res3.getHitTester());
      }
      catch(Exception e) {
        e.printStackTrace();
        fail("ERR: " + e);
      }
      finally {
        dummy = null;
      }
  }


  public void testGetConnectedNodes() {
    try {
      DrawableGraph dg = new DrawableGraph();

      DrawableNode node1 = dg.createNode();
      DrawableNode node2 = dg.createNode();
      DrawableNode node3 = dg.createNode();
      DrawableEdge edge1 = dg.createEdge(node1, node2);
      DrawableEdge edge2 = dg.createEdge(node1, node3);

      ArrayList list = new ArrayList();
      for(NodeIterator i = node1.getConnectedNodes(); i.hasNext(); ) {
        list.add(i.next());
      }
      assertEquals("Wrong on node 1", 2, list.size());
      assertTrue("Missing node2", list.contains(node2));
      assertTrue("Missing node2", list.contains(node3));

      list.clear();
      for(NodeIterator i = node2.getConnectedNodes(); i.hasNext(); ) {
        list.add(i.next());
      }
      assertEquals("Wrong on node 2", 1, list.size());
      assertTrue("Missing node1", list.contains(node1));

      list.clear();
      for(NodeIterator i = node3.getConnectedNodes(); i.hasNext(); ) {
        list.add(i.next());
      }
      assertEquals("Wrong on node 3", 1, list.size());
      assertTrue("Missing node1", list.contains(node1));
    }
    catch(Exception e) {
      e.printStackTrace();
      fail("ERR: " + e);
    }
  }

}
