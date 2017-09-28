package com.appliedminds.hmv.plaf;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;

/**
 * Border implementation that uses rounded corners
 *
 * @author darin@apmindsf.com
 */
class RoundedBorder extends AbstractBorder
{
  private static Insets insets = new Insets(2,2,2,2);

  /**
   * Returns the insets needed by this border
   *
   * @param c the component for which this border insets value applies
   * @return the border's insets
   */
  public Insets getBorderInsets(Component c)
  {
    return insets;
  }


  /**
   * @return false
   */
  public boolean isBorderOpaque()
  {
    return false;
  }


  /**
   * Does the real work of painting the border
   *
   * @param c the component for which this border is being painted
   * @param g the paint graphics
   * @param x the x position of the painted border
   * @param y the y position of the painted border
   * @param width the width of the painted border
   * @param height the height of the painted border
   */
  public void paintBorder(Component c, Graphics g, int x, int y, int width,
                          int height)
  {
    int adj = 25;
    Color baseColor = c.getBackground();
    int baseR = baseColor.getRed();
    int baseG = baseColor.getGreen();
    int baseB = baseColor.getBlue();
    Color innerHoriz = Color.WHITE;
    Color innerVert = new Color(baseR - adj, baseG - adj, baseB - adj);
    Color outerHoriz = new Color(baseR - adj*5, baseG - adj*5, baseB - adj*5);
    Color outerVert = new Color(baseR - adj*3, baseG - adj*3, baseB - adj*3);

    // outer border
    g.setColor(outerHoriz);
    g.drawLine(x + 1, y, x + width - 2, y);
    g.drawLine(x + 1, y + height - 1, x + width - 2, y + height - 1);
    g.setColor(outerVert);
    g.drawLine(x, y + 1, x, y + height - 2);
    g.drawLine(x + width - 1, y + 1, x + width - 1, y + height - 2);

    // inner border
    g.setColor(innerHoriz);
    g.drawLine(x + 1, y + 1, x + width - 2, y + 1);
    g.setColor(innerVert);
    g.drawLine(x + 1, y + 1, x + 1, y + height - 2);
    g.drawLine(x + width - 2, y + 1, x + width - 2, y + height - 2);
  }


} // end class
