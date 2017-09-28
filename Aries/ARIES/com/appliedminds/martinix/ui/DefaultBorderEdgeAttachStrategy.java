package com.appliedminds.martinix.ui;

/**
 * The default edge attach strategy.
 *
 * @author daepark@apmindsf.com
 */
public class DefaultBorderEdgeAttachStrategy extends BorderEdgeAttachStrategy {

  private static final double DEFAULT_EW_ANGLE = 70.0;

  public double getEastWestAngle() {
    return (DEFAULT_EW_ANGLE);
  }

  public int getTailNodeAttachPoint() {
    return (RELATIVE);
  }

  public int getHeadNodeAttachPoint() {
    return (RELATIVE);
  }

} // end class "DefaultBorderEdgeAttachStrategy"

