package com.appliedminds.martini.test;


import junit.framework.*;
import com.appliedminds.martini.*;
import java.util.Iterator;
import java.awt.geom.Rectangle2D;
import java.awt.Graphics2D;


/** 
 * Tests for com.appliedminds.martini.HitTesterMap
 *
 *
 * @author mathias@apmindsf.com
 */
public class HitTesterMapTest extends TestCase {

  public HitTesterMapTest(String name) {
    super(name);
  }

  public void testListHitTesters() {
    try {
      DrawableGraphElement dum1 = 
        new DrawableGraphElement() {
          public DrawableGraph getGraph() { return null; }
          public void draw(Graphics2D g, 
                           Viewport vp, 
                           HitTesterMap hit, 
                           DrawableGraphContext ctx, 
                           boolean newBuffer, 
                           boolean erase) { }
          
        };

      HitTester tester1 = 
        new HitTester(new Rectangle2D.Double(0.0, 0.0, 1.0, 1.0), 
                      "blah1", 
                      null);

      HitTester tester2 = 
        new HitTester(new Rectangle2D.Double(0.0, 0.0, 1.0, 1.0), 
                      "blah2", 
                      null);

      HitTester tester3 = 
        new HitTester(new Rectangle2D.Double(0.0, 0.0, 1.0, 1.0), 
                      "blah3", 
                      null);

      HitTesterMap map = new HitTesterMap();

      map.addHitTester(dum1, tester1);
      map.addHitTester(dum1, tester2);
      map.addHitTester(dum1, tester3);

      // Expect: 3 is on top of 2 which is on top of 1

      Iterator i = map.listHitTesters(dum1);
      assertTrue("No hit testers returned", i.hasNext());
      
      assertEquals("Missed 3", tester3, i.next());
      assertEquals("Missed 2", tester2, i.next());
      assertEquals("Missed 1", tester1, i.next());

      assertTrue("Too many hit testers", ! i.hasNext());
    }
    catch(Exception e) {
      e.printStackTrace();
      fail("ERR: " + e);
    }
  }


}
