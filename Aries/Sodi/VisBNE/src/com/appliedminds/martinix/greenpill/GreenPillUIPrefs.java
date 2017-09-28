/* -*- Mode: Java; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
package com.appliedminds.martinix.greenpill;


import java.awt.Color;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;

import com.appliedminds.martinix.ui.GraphColor;



/**
 * <p>GreenPillUIPrefs is a container for all the properties needed to
 * lay out and draw a graph - things like dimensions, line styles,
 * colors, fonts, etcetera.
 *
 * <p>The units are arbitrary "world coordinate system" units,
 * <b>not</b> device units.

 * <p>In practise, for some output technologies, it is sometimes
 * harder to control the size of fonts than the size of simple
 * geometric objects. It may therefore be a good idea to express font
 * sizes first in terms of "points", then set other dimension
 * properties relative to the font sizes.
 *
 *
 * <p><i>This has been ported from the vge.core.GraphMetrics class so
 * some code in here may be confusing.  At some point all the old code
 * will be fixed up and this message will be removed.</i>
 *
 *
 * @author ben@sf.appliedminds.net
 * @author mathias@apmindsf.com
 * @author daepark@apmindsf.com
 */
public class GreenPillUIPrefs {


  private double nodeWidth_;
  private double nodeHeight_;
  private double nodeDepth_;

  private double _iconHeight;
  private double _iconWidth;
  private double _iconEdgeSpacing;
  private double _interIconSpacing;

  private double horizontalSpacing_;
  private double verticalSpacing_;

  private double arrowHeadLength_;
  private double arrowHeadFlair_;
  private double arrowHeadOffset_;

  private double edgeBubbleWidth_;
  private double edgeBubbleHeight_;

  DrawProps nodeDrawProps_;
  DrawProps _greyedOutNodeDrawProps;
  DrawProps edgeDrawProps_;
  DrawProps _greyedOutEdgeDrawProps;
  DrawProps _greyedOutSelectedEdgeDrawProps;
  DrawProps edgeBubbleDrawProps_;
  DrawProps _greyedOutEdgeBubbleDrawProps;

  DrawProps selectedNodeDrawProps_;
  DrawProps selectedRootNodeDrawProps_;
  DrawProps selectedEdgeDrawProps_;
  DrawProps selectedEdgeBubbleDrawProps_;

  DrawProps rootNodeDrawProps_;

  private GraphColor selectedEdgeArrowFillColor_;
  private GraphColor selectedEdgeArrowStrokeColor_;

  private double _readableNodeTextSize;
  private static final byte[] _lock = new byte[0];

  private static GreenPillUIPrefs _instance = null;


  /**
   * @return the one instance of GreenPillUIPrefs
   */
  public static GreenPillUIPrefs getInstance() {
    if (_instance == null) {
      synchronized(_lock) {
        if (_instance == null) {
          _instance = new GreenPillUIPrefs();
        }
      }
    }

    return (_instance);
  }


  /**
   * Default constructor behavior is to set values that
   * match our current "topic map visualization" design.
   */
  private GreenPillUIPrefs() {
    double nodeFontSize = 12.0;

    setNodeWidth(18.0);
    setNodeHeight(18.0);
    setNodeDepth(18.0);

    setReadableNodeTextSize(12.0);
    setIconHeight(23.0);
    setIconWidth(23.0);
    setIconEdgeSpacing(10.0);
    setInterIconSpacing(8.0);

    //setHorizontalSpacing(210.0);
    //setVerticalSpacing(96.0);
    setHorizontalSpacing(82.0);
    setVerticalSpacing(38.8);

    setArrowHeadLength(6.0);
    setArrowHeadFlair(0.4);
    setArrowHeadOffset(12.0);

    setEdgeBubbleWidth(12.0);
    setEdgeBubbleHeight(12.0);

    nodeDrawProps_ = new DrawProps(
                                   1.0,                              // strokeWidth
                                   new GraphColor(0x41, 0x84, 0x15), // strokeColor
                                   1.0,                              // strokeOpacity
                                   new GraphColor(0xA8, 0xC4, 0x51), // fillColor
                                   1.0,                              // fillOpacity
                                   new GraphColor(0x00, 0x00, 0x00), // fontColor
                                   nodeFontSize                      // fontSize
                                   );


    _greyedOutNodeDrawProps = new DrawProps(
                                            1.0,                              // strokeWidth
                                            new GraphColor(0x91, 0xCE, 0x1B), // strokeColor
                                            1.0,                              // strokeOpacity
                                            new GraphColor(0xD9, 0xF4, 0x98), // fillColor
                                            1.0,                              // fillOpacity
                                            new GraphColor(0x00, 0x00, 0x00), // fontColor
                                            nodeFontSize                      // fontSize
                                            );

    edgeDrawProps_ = new DrawProps(
                                   1.0,                              // strokeWidth
                                   new GraphColor(0x00, 0x00, 0x00), // strokeColor
                                   1.0,                              // strokeOpacity
                                   new GraphColor(0x00, 0x00, 0x00), // fillColor
                                   1.0,                              // fillOpacity
                                   new GraphColor(0x00, 0x00, 0x00), // fontColor
                                   12.0                              // fontSize
                                   );


    _greyedOutEdgeDrawProps = new DrawProps(
                                            1.0,                              // strokeWidth
                                            new GraphColor(0xCC, 0xCC, 0xCC), // strokeColor
                                            1.0,                              // strokeOpacity
                                            new GraphColor(0xCC, 0xCC, 0xCC), // fillColor
                                            1.0,                              // fillOpacity
                                            new GraphColor(0xCC, 0xCC, 0xCC), // fontColor
                                            12.0                              // fontSize
                                            );

    edgeBubbleDrawProps_ = new DrawProps(
                                         1.0,                              // strokeWidth
                                         new GraphColor(0x00, 0x00, 0x00), // strokeColor
                                         1.0,                              // strokeOpacity
                                         new GraphColor(
                                                        0x3B, 0x86, 0xFF,
                                                        0x00, 0x24, 0x9F,
                                                        GraphColor.VERTICAL_GRADIENT),  // fillColor
                                         1.0,                              // fillOpacity
                                         new GraphColor(0xFF, 0xFF, 0xFF), // fontColor
                                         8.0                               // fontSize
                                         );

    _greyedOutEdgeBubbleDrawProps = new DrawProps(
                                                  1.0,                              // strokeWidth
                                                  new GraphColor(0xCC, 0xCC, 0xCC), // strokeColor
                                                  1.0,                              // strokeOpacity
                                                  new GraphColor(0xFF, 0xFF, 0xFF), // fillColor
                                                  1.0,                              // fillOpacity
                                                  new GraphColor(0xCC, 0xCC, 0xCC), // fontColor
                                                  8.0                               // fontSize
                                                  );



    selectedNodeDrawProps_ = new DrawProps(nodeDrawProps_);
    selectedNodeDrawProps_.setStrokeColor(new GraphColor(0x00, 0x8C, 0xFF));
    selectedNodeDrawProps_.setFillColor(new GraphColor(0x33, 0x66, 0x99));

    selectedRootNodeDrawProps_ = new DrawProps(nodeDrawProps_);
    selectedRootNodeDrawProps_.setStrokeColor(new GraphColor(0x00, 0x8C, 0xFF));
    selectedRootNodeDrawProps_.setFillColor(new GraphColor(0xCC, 0x88, 0x65));

    selectedEdgeDrawProps_ = new DrawProps(edgeDrawProps_);
    selectedEdgeDrawProps_.setStrokeColor(new GraphColor(0x11, 0x11, 0xEE));
    selectedEdgeDrawProps_.setFillColor(new GraphColor(0x22, 0x22, 0xDD));

    _greyedOutSelectedEdgeDrawProps = new DrawProps(_greyedOutEdgeDrawProps);
    _greyedOutSelectedEdgeDrawProps.setStrokeColor(new GraphColor(0x99, 0x99, 0xDD));
    _greyedOutSelectedEdgeDrawProps.setFillColor(new GraphColor(0x99, 0x99, 0xDD));

    selectedEdgeBubbleDrawProps_ = new DrawProps(edgeBubbleDrawProps_);
    //    selectedEdgeBubbleDrawProps_.setStrokeColor();
    //    selectedEdgeBubbleDrawProps_.setFillColor();

    rootNodeDrawProps_ = new DrawProps(nodeDrawProps_);
    rootNodeDrawProps_.setStrokeColor(new GraphColor(0xCC, 0x46, 0x0B));
    rootNodeDrawProps_.setFillColor(new GraphColor(0xE8, 0xA1, 0x43));

    selectedEdgeArrowFillColor_ = new GraphColor(Color.red);
    selectedEdgeArrowStrokeColor_ = new GraphColor(Color.red.darker());
  }

  /**
   * Get the width (x-axis diameter) for nodes.
   *
   * @return the width (x-axis diameter) for nodes.
   */

  public double getNodeWidth() { return nodeWidth_; }

  /**
   * Get the height (y-axis diameter) for nodes.
   *
   * @return the height (y-axis diameter) for nodes.
   */

  public double getNodeHeight() { return nodeHeight_; }


  /**
   * Get the desired icon height.
   */
  public double getIconHeight() {
    return(_iconHeight);
  }


  /**
   * Get the icon width.
   */
  public double getIconWidth() {
    return(_iconWidth);
  }


  /**
   * Get the space between icons.
   */
  public double getInterIconSpacing() {
    return(_interIconSpacing);
  }



  /**
   * Get the space between an icon and the node edge.
   */
  public double getIconEdgeSpacing() {
    return(_iconEdgeSpacing);
  }


  /**
   * Get the depth (z-axis diameter) for nodes.
   *
   * @return the depth (z-axis diameter) for nodes.
   */

  public double getNodeDepth() { return nodeDepth_; }

  /**
   * Get the minimum horizontal node spacing for the layout algorithms.
   *
   * @return the minimum horizontal node spacing for the layout algorithms.
   */

  public double getHorizontalSpacing() { return horizontalSpacing_; }

  /**
   * Get the minimum vertical node spacing for the layout algorithms.
   *
   * @return the minimum vertical node spacing for the layout algorithms.
   */

  public double getVerticalSpacing() { return verticalSpacing_; }

  /**
   * Get the edge arror head length.
   *
   * @return the edge arrow head length.
   */

  public double getArrowHeadLength() { return arrowHeadLength_; }

  /**
   * Get the arrow head flair (0.0 &lt; 1.0) - smaller numbers make the arror head more pointy.
   *
   * @return the arrow head flair (0.0 &lt; 1.0) - smaller numbers make the arror head more pointy.
   */

  public double getArrowHeadFlair() { return arrowHeadFlair_; }

  /**
   * Get the edge arror head offset.
   *
   * @return the edge arrow head offset. This is the distance of
   * the arrow tail from the bubble point.
   */

  public double getArrowHeadOffset() { return arrowHeadOffset_; }

  /**
   * Get the width (x-axis diameter) for edge bubbles.
   *
   * @return the width (x-axis diameter) for edge bubbles.
   */

  public double getEdgeBubbleWidth() { return edgeBubbleWidth_; }

  /**
   * Get the height (y-axis diameter) for edge bubbles.
   *
   * @return the height (y-axis diameter) for edge bubbles.
   */

  public double getEdgeBubbleHeight() { return edgeBubbleHeight_; }

  /**
   * Get the default node DrawProps.
   *
   * @return <b>a copy of</b> the node DrawProps.
   */
  public DrawProps getNodeDrawProps() {
    return(getNodeDrawProps(false));
  }


  /**
   * Get the node DrawProps.
   *
   * @param greyedOut If set to true, then get the draw props for a
   * node that is "greyed" out.
   * @return <b>a copy of</b> the node DrawProps.
   */
  public DrawProps getNodeDrawProps(boolean greyedOut) {
    DrawProps dp = null;
    if(greyedOut) {
      dp = new DrawProps(_greyedOutNodeDrawProps);
    }
    else {
      dp = new DrawProps(nodeDrawProps_);
    }
    return(dp);
  }


  /**
   * Get the point size that is considered to be "readable"
   * for node text.
   *
   * @return a font size in "points" (pixels).
   */
  public double getReadableNodeTextSize() {
    return(_readableNodeTextSize);
  }


  /**
   * Get the edge DrawProps.
   *
   * @return <b>a copy of</b> the edge DrawProps.
   */
  public DrawProps getEdgeDrawProps() {
    return(getEdgeDrawProps(false, false));
  }

  /**
   * Get the edge DrawProps.
   *
   * @param greyedOut If set to true, then get the draw props for an
   * edge that is "greyed" out.
   * @return <b>a copy of</b> the edge DrawProps.
   */
  public DrawProps getEdgeDrawProps(boolean greyedOut, boolean selected) {
    DrawProps dp = null;
    if (greyedOut && !selected) {
      dp = new DrawProps(_greyedOutEdgeDrawProps);
    }
    else if (!greyedOut && selected) {
      dp = new DrawProps(selectedEdgeDrawProps_);
    }
    else if (greyedOut && selected) {
      dp = new DrawProps(_greyedOutSelectedEdgeDrawProps);
    }
    else {
      dp = new DrawProps(edgeDrawProps_);
    }
    return(dp);
  }

  /**
   * Get the edge bubble DrawProps.
   *
   * @return <b>a copy of</b> the edge bubble DrawProps.
   */

  public DrawProps getEdgeBubbleDrawProps() {
    return(getEdgeBubbleDrawProps(false));
  }

  /**
   * Get the edge bubble DrawProps.
   *
   * @param greyedOut If set to true, then get the draw props for
   * an edge that is "greyed" out.
   * @return <b>a copy of</b> the edge bubble DrawProps.
   */
  public DrawProps getEdgeBubbleDrawProps(boolean greyedOut) {
    DrawProps dp = null;
    if(greyedOut) {
      dp = new DrawProps(_greyedOutEdgeBubbleDrawProps);
    }
    else {
      dp = new DrawProps(edgeBubbleDrawProps_);
    }
    return(dp);
  }

  /**
   * Get the selected node DrawProps.
   *
   * @return <b>a copy of</b> the selected node DrawProps.
   */

  public DrawProps getSelectedNodeDrawProps() { return new DrawProps(selectedNodeDrawProps_); }

  /**
   * Get the selected root node DrawProps.
   *
   * @return <b>a copy of</b> the selected root node DrawProps.
   */

  public DrawProps getSelectedRootNodeDrawProps() { return new DrawProps(selectedRootNodeDrawProps_); }

  /**
   * Get the selected edge DrawProps.
   *
   * @return <b>a copy of</b> the selected edge DrawProps.
   */

  public DrawProps getSelectedEdgeDrawProps() { return new DrawProps(selectedEdgeDrawProps_); }

  /**
   * Get the selected edge bubble DrawProps.
   *
   * @return <b>a copy of</b> the selected edge bubble DrawProps.
   */

  public DrawProps getSelectedEdgeBubbleDrawProps() { return new DrawProps(selectedEdgeBubbleDrawProps_); }

  /**
   * Get the root node DrawProps.
   *
   * @return <b>a copy of</b> the root node DrawProps.
   */

  public DrawProps getRootNodeDrawProps() { return new DrawProps(rootNodeDrawProps_); }

  /**
   * Set the width (x-axis diameter) for nodes.
   *
   * @param nodeWidth
   */

  public void setNodeWidth(double nodeWidth)
  {
    nodeWidth_ = nodeWidth;
  }

  /**
   * Set the height (y-axis diameter) for nodes.
   *
   * @param nodeHeight
   */

  public void setNodeHeight(double nodeHeight)
  {
    nodeHeight_ = nodeHeight;
  }



  /**
   * Set the icon height in world units.
   *
   * @param h the icon height.
   */
  public void setIconHeight(double h) {
    _iconHeight = h;
  }


  /**
   * Set the icon width.
   *
   * @param d the width in world units.
   */
  public void setIconWidth(double d) {
    _iconWidth = d;
  }


  /**
   * Set the spacing between the icon and the E/W edge of the
   * node shape.
   * @param d the spacing.
   */
  public void setIconEdgeSpacing(double d) {
    _iconEdgeSpacing = d;
  }


  /**
   * Set the spacing between icons.
   *
   * @param d the spacing.
   */
  public void setInterIconSpacing(double d) {
    _interIconSpacing = d;
  }


  /**
   * Set the depth (z-axis diameter) for nodes.
   *
   * @param nodeDepth
   */

  public void setNodeDepth(double nodeDepth)
  {
    nodeDepth_ = nodeDepth;
  }

  /**
   * Set the minimum horizontal node spacing for the layout algorithms.
   *
   * @param horizontalSpacing
   */

  public void setHorizontalSpacing(double horizontalSpacing)
  {
    horizontalSpacing_ = horizontalSpacing;
  }

  /**
   * Set the minimum vertical node spacing for the layout algorithms.
   *
   * @param verticalSpacing
   */

  public void setVerticalSpacing(double verticalSpacing)
  {
    verticalSpacing_ = verticalSpacing;
  }

  /**
   * Set the edge arror head length.
   *
   * @param arrowHeadLength
   */

  public void setArrowHeadLength(double arrowHeadLength)
  {
    arrowHeadLength_ = arrowHeadLength;
  }

  /**
   * Set the arrow head flair (0.0 &lt; 1.0) - smaller numbers make the arror head more pointy.
   *
   * @param arrowHeadFlair
   */

  public void setArrowHeadFlair(double arrowHeadFlair)
  {
    arrowHeadFlair_ = arrowHeadFlair;
  }

  /**
   * Set the edge arrow head offset.
   *
   * @param arrowHeadOffset This is the distance of
   * the arrow tail from the bubble point.
   */

  public void setArrowHeadOffset(double arrowHeadOffset)
  {
    arrowHeadOffset_ = arrowHeadOffset;
  }

  /**
   * Set the width (x-axis diameter) for edge bubbles.
   *
   * @param edgeBubbleWidth
   */

  public void setEdgeBubbleWidth(double edgeBubbleWidth)
  {
    edgeBubbleWidth_ = edgeBubbleWidth;
  }

  /**
   * Set the height (y-axis diameter) for edge bubbles.
   *
   * @param edgeBubbleHeight
   */

  public void setEdgeBubbleHeight(double edgeBubbleHeight)
  {
    edgeBubbleHeight_ = edgeBubbleHeight;
  }


  /**
   * An arrow can be hilited, this gets the fill color for when it is.
   */
  public GraphColor getSelectedEdgeArrowFillColor() {
    return(selectedEdgeArrowFillColor_);
  }

  /**
   * An arrow can be hilited, this gets the stroke color for when it is.
   */
  public GraphColor getSelectedEdgeArrowStrokeColor() {
    return(selectedEdgeArrowStrokeColor_);
  }


  /**
   * Set the font point size for "readable" node text.
   */
  public void setReadableNodeTextSize(double s) {
    _readableNodeTextSize = s;
  }


  public int getSmallestVisibleNodeHeight() {
    return(1);
  }

  public int getSmallestVisibleFontSize() {
    return(3);
  }

  public int getMaxLabelLength() {
    return(35);
  }

  /**
   * Required node property.  Value: A text label.
   */
  public static String getNodeLabelPropertyName() {
    return("label");
  }

  /**
   * Required node proeprty.  Value: true or false.
   */
  public static String getSelectedPropertyName() {
    return("selected");
  }

  /**
   * Required node property.  Value: the number of hops that a node is
   * from the root node.
   */
  public static String getDistanceFromRootPropertyName() {
    return("rootdx");
  }


  public static String getRootNodePropertyName() {
    return("root");
  }

  public String getNodeFontResourcePath() {
    return("com/appliedminds/martinix/greenpill/resources/InterstateRegular.ttf");
  }


  /**
   * All graphs returned by the QueryTopicGraphGML servlet will
   * contain a list of all known node and edge types. These types will
   * be returned as an array represented as string in the following form:
   *
   * For example:
   *
   * <pre>
   *   graph [
   *     ...
   *     edgetypes "(is related to, is near, works for)"
   *     nodetypes "(person, place, thing)"
   *     ...
   *   ]
   * </pre>
   */
  public static String getGraphNodeTypesPropertyName() {
    return ("nodeTypes");
  }

  /**
   * @see #getGraphEdgeTypesPropertyName
   */
  public static String getGraphEdgeTypesPropertyName() {
    return ("edgeTypes");
  }


  public static String getEdgeTypeDirectionPropertyName() {
    return ("edgeTypeDirection");
  }


  public static String getEdgeTypePropertyName() {
    return ("edgeType");
  }


  public static String getEdgeTypeVisibilityPropertyName() {
    return ("edgeTypeVisibility");
  }


  public static Color getCaretColor() {
    return(Color.green);
  }


  public static Color getSelectedTextColor() {
    return(Color.green);
  }


  public URL getTypeMapURL() {
    try {
      URL dir = getClass().getClassLoader().getResource("com/appliedminds/martinix/greenpill/resources/");
      URL u = new URL(dir.toString() + "dataset.xml");
      return(u);
    }
    catch(MalformedURLException e) {
      throw(new GreenPillUIError("GreenPillUIPrefs: bad type map url.  Error = " + e));
    }
  }


} // end class GreenPillUIPrefs
