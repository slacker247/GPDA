package com.appliedminds.martini.test;

import java.awt.geom.Rectangle2D;
import junit.framework.*;
import com.appliedminds.martini.*;



/**
 * @author mathias@apmindsf.com
 */
public class HitTestResultTest extends TestCase {

  public HitTestResultTest(String name) {
    super(name);
  }

  /**
   * Test to make sure that the class is sane.
   */
  public void testFlags() {
    try {
      HitTester dummy = new HitTester(new Rectangle2D.Double(0,0,1,1),
                                      "TEST",
                                      new Integer(666));

      HitTestResult res = HitTestResult.hitTestConsumed(dummy);
      assertTrue("Bad consume flag", res.wasConsumed());
      assertTrue("Bad hit flag", res.wasHit());
      
      res = HitTestResult.hitTestHitNotConsumed(dummy);
      assertTrue("Bad consume flag (2)", !res.wasConsumed());
      assertTrue("Bad hit flag (2)", res.wasHit());

      res = HitTestResult.hitTestMissed();
      assertTrue("Bad consume flag (3)", !res.wasConsumed());
      assertTrue("Bad hit flag (3)", !res.wasHit());
    }
    catch(Exception e) {
      e.printStackTrace();
      fail("ERR:" + e);
    }
  }

}
