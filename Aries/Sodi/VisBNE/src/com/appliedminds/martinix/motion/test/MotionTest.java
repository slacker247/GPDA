package com.appliedminds.martinix.motion.test;

import java.awt.geom.Rectangle2D;
import java.awt.geom.Point2D;
import junit.framework.*;
import com.appliedminds.martinix.motion.Motion;


/**
 * Test for com.appliedminds.martinix.motion.Motion. We simply get back
 * a series of Points and print out their values for now.
 */
public class MotionTest extends TestCase
{
  private static double END = 10.0; // length of motion

  private double _startX = -1.0;
  private double _startY = 1.0;
  private double _endX = 4.5;
  private double _endY = -10.2;


  public MotionTest(String name)
  {
    super(name);
  }


  public void testDecelerate()
  {
    System.err.println("\nDeceleration:");
    for (double t=0.0; t<=END; t++)
    {
      Point2D p = Motion.decelerate(t, END, _startX, _startY, _endX, _endY);
      System.err.println("time " + t + " > " + p);
    }
  }


  public void testBounce()
  {
    System.err.println("\nBounce:");
    for (double t=0.0; t<=END; t++)
    {
      Point2D p = Motion.bounce(t, END, 0.4, 3, 0.3,
                                _startX, _startY, _endX, _endY);
      System.err.println("time " + t + " > " + p);
    }
  }

} // end class MotionTest
