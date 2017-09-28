package com.appliedminds.martinix.slider;

import java.util.Arrays;
import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.StringTokenizer;
import java.awt.geom.GeneralPath;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;

import com.appliedminds.martini.*;


/**
 * A collection of SliderUI-specific utilities and convenience methods, or
 * ass-kickin-time-savers if you will.
 *
 * @author will@apmindsf.com
 */
public class SliderUtil {

  /*
   * Get the node lable via a property.
   *
   * Could return null.
   */
  public static String getNodeLabel(DrawableNode n) {
    return (n.getProperty(SliderUIPrefs.getNodeLabelPropertyName()));
  }


  /**
   * Get the slider value from the node.
   */
  public static double getSliderValue(DrawableNode n) {
    String str = n.getProperty(SliderUIPrefs.getSliderValuePropertyName());
    if (str != null) {
      try {
        return(Double.parseDouble(str));
      }
      catch(NumberFormatException e) {
        ;
      }
    }
    return(0.0);
  }


  /**
   * Set the slider value for the node.
   */
  public static void setSliderValue(DrawableNode n, double v) {
    n.setProperty(SliderUIPrefs.getSliderValuePropertyName(),
                  Double.toString(v));
  }


  /*
   * Set the node lable via a property.
   *
   */
  public static void setNodeLabel(DrawableNode n, String label) {
    n.setProperty(SliderUIPrefs.getNodeLabelPropertyName(), label);
  }


  public static void setSliderVisible(DrawableNode n, boolean v) {
    n.setProperty("sliderVisible", v ? "true" : "false");
  }

  public static boolean getSliderVisible(DrawableNode n) {
    String res = n.getProperty("sliderVisible");
    if ((res != null) && (res.equals("true"))) {
      return true;
    }

    return false;
  }

  public static boolean getNodeInverseProbabilityMode(DrawableNode node)
  {
    String valStr =
      node.getProperty(SliderUIPrefs.getProbabilityModePropertyName());
    if ((valStr != null) && (valStr.equals("true"))) {
      return true;
    }
    else {
      return false;
    }
  }

  public static void setNodeInverseProbabilityMode(DrawableNode node, boolean b)
  {
    if (b) {
      node.setProperty(SliderUIPrefs.getProbabilityModePropertyName(), "true");
    }
    else {
      node.setProperty(SliderUIPrefs.getProbabilityModePropertyName(), "false");
    }
  }

  /**
   * Set the manual property of the specified node to FALSE.
   */
  /*
  public static void setAutomatic(DrawableNode n) {
    n.setProperty(SliderUIPrefs.getManualNodePropertyName(), "false");
  }
  */


  /**
   * Set the manual property of the specified node to TRUE.
   */
  /*
  public static void setManual(DrawableNode n) {
    n.setProperty(SliderUIPrefs.getManualNodePropertyName(), "true");
  }
  */


  /**
   * Is the node set to manual or automatic?
   */
  /*
  public static boolean isManual(DrawableNode n) {
    return ("true".equals(n.getProperty(SliderUIPrefs.getManualNodePropertyName())));
  }
  */

  /**
   * Does the element have the selected property set to "true"?
   */
  public static boolean isSelected(DrawableGraphElement element) {
    return ("true".equals(element.getProperty("selected")));
  }


  static public GeneralPath makePath(Point2D points[],
                                     Rectangle2D bounds,
                                     boolean closePath)
  {
    GeneralPath path = new GeneralPath();

    double minx = 0;
    double miny = 0;
    double maxx = 0;
    double maxy = 0;

    double x;
    double y;

    for (int i = 0; i < points.length; i++)
    {
      x = points[i].getX();
      y = points[i].getY();

      if (i == 0)
      {
        path.moveTo((float)x, (float)y);

        minx = x;
        miny = y;
        maxx = x;
        maxy = y;
      }
      else
      {
        path.lineTo((float)x, (float)y);

        if (x < minx)
        {
          minx = x;
        }
        if (y < miny)
        {
          miny = y;
        }
        if (x > maxx)
        {
          maxx = x;
        }
        if (y > maxy)
        {
          maxy = y;
        }
      }
    }

    if (closePath)
    {
      path.closePath();
    }

    bounds.setRect(minx, miny, maxx - minx, maxy - miny);

    return path;
  }

} // end class SliderUtil
