package com.appliedminds.martini.test;

import junit.framework.*;
import com.appliedminds.martini.*;
import java.awt.geom.Rectangle2D;



/**
 * @author mathias@apmindsf.com
 */
public class HitTesterTest extends TestCase {


  public HitTesterTest(String name) {
    super(name);
  }


  public void testHitTest() {
    try {
      Rectangle2D rect = new Rectangle2D.Double(5, 0, 5, 5);
      HitTester t = new HitTester(rect, "blah", null);
      
      Rectangle2D ptr = new Rectangle2D.Double(-5, 5, 2, 2);      
      assertTrue("Bad hit test", !t.hitTest(ptr));      

      ptr = new Rectangle2D.Double(2,2,1,1);
      assertTrue("Bad hit test (2)", ! t.hitTest(ptr));

      ptr = new Rectangle2D.Double(6,2,1,1);
      assertTrue("Missed hit test", t.hitTest(ptr));
    }
    catch(Exception e) {
      e.printStackTrace();
      fail("ERR:" + e);
    }
  }

}
