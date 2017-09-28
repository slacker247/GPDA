package com.appliedminds.martinix.motion;

import java.awt.geom.Point2D;


/**
 * Motion functions, based on the Laws of Physics! This is SCIENCE!
 *
 * @author kurt@apmindsf.com
 * @author will@apmindsf.com
 */
public class Motion
{

  /**
   * Here's some note on the math, base on Kurt's comments:
   *
   * s0=0  Start position<br>
   * sT=1  Final distance is 1.<br>
   * vT=0  Final velocity is zero.<br>
   * <br>
   * Basic equations are:<br>
   *   s(t)=(A*t^2)/2 + V0*t + s0<br>
   *   v(t)=A*t+v0<br>
   *
   * Since we know s0,sT,vT, we can calculate:<br>
   *   v0=2/T<br>
   *   A=-2/T^2<br>
   *   s(t)=-t^2/T^2+2*t/T<br>
   *
   * @param t currenttime: This is the time for the current frame.
   *        It will likely be some multiple of T/n where n is the
   *        total number of frames.
   * @param T endtime: This is the total travel time desired.
   * @param startx x coordinate of the starting point
   * @param starty y coordinate of the starting point
   * @param endx x coordinate of the ending point
   * @param endy y coordinate of the ending point
   */
  public static Point2D decelerate(double t,
                                   double T,
                                   double startx, double starty,
                                   double endx,   double endy)
  {
    // this is normalized distance, between 0.0 and 1.0.
    double dist = - (t*t)/(T*T) + (2*t/T);

    double x = startx + (dist * (endx - startx));
    double y = starty + (dist * (endy - starty));

    return (new Point2D.Double(x, y));
  }



  /**
   * For bounce effect, we will use a damped harmonic oscillator (spring).
   * Assume a mass of 1 and a spring constant of 1.<p>
   *
   * Basic equation:<br>
   *   s(t) = e^(-g*t) * cos(t)<p>
   *
   * @param t current time
   * @param T total travel time
   * @param g damping constant (inverse of how big the bounces are)
   * @param b How many bounces are desired (1=simple deceleration)
   * @param p initial position before first approach (0-1)  Try using 0.3.
   * @param startx x coordinate of the starting point
   * @param starty y coordinate of the starting point
   * @param endx x coordinate of the ending point
   * @param endy y coordinate of the ending point
   */
  public static Point2D bounce(double t,
                               double T,
                               double g,
                               int b,
                               double p,
                               double startx, double starty,
                               double endx,   double endy)
  {
    // Calculate the initial position.
    if ((p < 0.0) || (p > 1.0))
    {
      return (new Point2D.Double(endx, endy));
    }

    double initPosFactor = p * Math.PI / 2.0d;

    // A = amplitude (total distance -- is determined by argument p)
    double A = Math.cos(initPosFactor);
    
    // Calc relative starting offset time
    double rot = (Math.PI/4.0d) * p;

    // Calc relative total time.
    if ( b < 1 ) // just return endpoint;
    {
      return (new Point2D.Double(endx, endy));
    }

    double RT = (Math.PI/4.0d) + (Math.PI/2.0d) * (b - 1.0d);

    // Calc relative current time
    double rt = (t/T) * (RT - rot) + rot;

    // Calc position (is normalized by A)
    double position = (A - (Math.exp(-g*rt) * Math.cos(rt)))/A;
    
    // Convert position to coordinates
    double x = (endx - startx) * position + startx;
    double y = (endy - starty) * position + starty;

    return (new Point2D.Double(x, y));
  }

} // end class Motion
