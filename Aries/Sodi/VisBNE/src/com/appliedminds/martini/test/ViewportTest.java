package com.appliedminds.martini.test;


import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import junit.framework.*;
import com.appliedminds.martini.Viewport;


/**
 * Tests for com.appliedminds.martini.Viewport
 *
 *
 * @author mathias@apmindsf.com
 */
public class ViewportTest extends TestCase {

  
  public ViewportTest(String name) {
    super(name);
  }


  public void testMapWorldToViewport() {
    try {

      // (Wx, Wy) --> (Vx, Vy)
      double[] pts =
        {
          -10.0, 10.0,   0.0, 0.0,
           10.0, 10.0,  30.0, 0.0,
          -10.0,-10.0,   0.0, 30.0,
           10.0, -10.0, 30.0, 30.0
        };

      Viewport viewport = new Viewport();
      viewport.setWorldBounds(new Rectangle2D.Double(-10.0, 10.0, 20, 20));
      viewport.setScale(1.5);
      
      for(int i=0; i < pts.length - 4; i+=4) {
        Point2D worldPt = new Point2D.Double(pts[i], pts[i+1]);
        Point2D viewPt = new Point2D.Double(pts[i+2], pts[i+3]);
        
        Point2D res = viewport.mapWorldToViewport(worldPt);
        assertEquals("Map world to viewport failed", viewPt, res);

        Point2D back = viewport.mapViewportToWorld(res);
        assertEquals("Map viewport to world failed", worldPt, back);
      }

    }
    catch(Exception e) {
      e.printStackTrace();
      fail("ERR:" + e);
    }
  }

} // end class ViewPortTest
