package com.appliedminds.martinix.fader;

import com.appliedminds.martini.*;

import java.awt.geom.GeneralPath;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import javax.swing.SwingConstants;


/**
 * FaderUtil.java
 *
 *
 * Created: Mon Aug 26 11:26:24 2002
 *
 * @author <a href="mailto: daepark@apmindsf.com"</a>
 * @version
 */

public class FaderUtil
{
  /**
   * Get the node label via a property. Return null if label property
   * is not present.
   */
  public static String getNodeLabel(DrawableNode node) {
    return (getLabel(node));
  }


  /**
   * Set the node lable via a property.
   */
  public static void setNodeLabel(DrawableNode n, String label) {
    n.setProperty("label", label);
  }


  /**
   * Get the edge label via a property. Return null if label property
   * is not present.
   */
  public static String getEdgeLabel(DrawableEdge edge) {
    return (getLabel(edge));
  }


  /**
   * Set the edge lable via a property.
   */
  public static void setEdgeLabel(DrawableEdge edge, String label) {
    edge.setProperty("label", label);
  }


  public static boolean isConnectorHubsVisible(DrawableNode node) {
    return ("true".equals(node.getProperty("connectorHubsVisible")));
  }

  public static void setConnectorHubsVisible(DrawableNode node, boolean b) {
    node.setProperty("connectorHubsVisible", (b ? "true" : "false"));
  }

  public static boolean isSliderToggleVisible(DrawableGraphElement elt) {
    return ("true".equals(elt.getProperty("sliderToggleVisible")));
  }

  public static void setSliderToggleVisible(DrawableGraphElement elt,
                                            boolean b)
  {
    elt.setProperty("sliderToggleVisible", (b ? "true" : "false"));
  }

  public static void setCreatingEdgeState(DrawableNode elt,
                                          boolean b)
  {
    elt.setProperty("creatingEdge", (b ? "true" : "false"));
  }

  public static void setEdgeConnectorHubSelected(DrawableNode elt,
                                                 int connection,
                                                 boolean b)
  {
    EdgeConnectorHub.checkConnection(connection);

    String prop = getEdgeConnectorHubSelectedProperty(connection);
    elt.setProperty(prop, (b ? "true" : "false"));
  }

  private static String getEdgeConnectorHubSelectedProperty(int connection)
  {
    switch (connection) {
    case SwingConstants.NORTH:
      return "NorthConnectorHubSelected";
    case SwingConstants.WEST:
      return "WestConnectorHubSelected";
    case SwingConstants.SOUTH:
      return "SouthConnectorHubSelected";
    case SwingConstants.EAST:
      return "EastConnectorHubSelected";
    }

    throw (new RuntimeException("Invalid edge connection : " +
                                connection));
  }

  public static boolean isEdgeConnectorHubSelected(DrawableNode elt,
                                                   int connection)
  {
    EdgeConnectorHub.checkConnection(connection);

    String prop = getEdgeConnectorHubSelectedProperty(connection);

    return ("true".equals(elt.getProperty(prop)));
  }

  public static void setCreatingEdge(DrawableNode elt,
                                     boolean b)
  {
    elt.setProperty("creatingEdge", (b ? "true" : "false"));
  }

  public static boolean isCreatingEdge(DrawableNode elt)
  {
    return ("true".equals(elt.getProperty("creatingEdge")));
  }


  /**
   * Get the slider value via a property.
   *
   * @return the slider value or the default value if property is not
   * set or value is invalid.
   */
  public static int getSliderValue(DrawableGraphElement elt, int value) {
    int retVal = value;
    String str = elt.getProperty("sliderValue");
    if (str != null) {
      try {
        retVal = Integer.parseInt(str);
      }
      catch(NumberFormatException e) { }
    }
    return(retVal);
  }


  /**
   * Get the slider value via a property.
   *
   * @return 0 if property is not set or value is invalid.
   */
  public static int getSliderValue(DrawableGraphElement elt) {
    return (getSliderValue(elt, 0));
  }


  /**
   * Set the node slider value via a property
   */
  public static void setSliderValue(DrawableGraphElement elt, int value) {
    elt.setProperty("sliderValue", String.valueOf(value));
  }


  public static void setSliderVisible(DrawableGraphElement elt, boolean v) {
    elt.setProperty("sliderVisible", v ? "true" : "false");
  }


  public static boolean isSliderVisible(DrawableGraphElement elt) {
    return ("true".equals(elt.getProperty("sliderVisible")));
  }


  /**
   * Does the element have the selected property set to "true"?
   */
  public static boolean isSelected(DrawableGraphElement elt) {
    return ("true".equals(elt.getProperty("selected")));
  }

  public static void setSelected(DrawableGraphElement elt, boolean b) {
    elt.setProperty("selected", (b ? "true" : "false"));
  }


  public static void setManual(DrawableGraphElement elt, boolean manual) {
    elt.setProperty("manual", manual ? "true": "false");
  }


  public static boolean isManual(DrawableGraphElement elt) {
    return ("true".equals(elt.getProperty("manual")));
  }


  public static boolean isEdgePositive(DrawableEdge edge) {
    //return ("true".equals(edge.getProperty("positive")));
    return (getSliderValue(edge) >= 0);
  }


  public static void setEdgePositive(DrawableEdge edge, boolean positive) {
    edge.setProperty("positive", positive ? "true" : "false");
  }


  public static int getNodeWeight(DrawableNode node, int defaultSliderValue)
  {
    int weight = 0;

    EdgeIterator itr = node.getOutgoingEdges();
    while (itr.hasNext()) {
      DrawableEdge edge = itr.next();
      weight = weight +
        FaderUtil.getSliderValue(edge, defaultSliderValue);
    }

    return (weight);
  }


  public static GeneralPath makePath(Point2D points[],
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


  public static void setInEdit(DrawableNode node, boolean inEdit)
  {
    node.setProperty("inEdit", (inEdit ? "true" : "false"));
  }

  public static boolean isInEdit(DrawableNode node)
  {
    return ("true".equals(node.getProperty("inEdit")));
  }


  /**
   * Get the label via a property. Return null if label property is
   * not present.
   */
  private static String getLabel(DrawableGraphElement elt) {
    return (elt.getProperty("label"));
  }

} // end class "FaderUtil"
