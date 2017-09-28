/* -*- Mode: Java; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
package com.appliedminds.martinix.slider;

import java.awt.Color;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;

import com.appliedminds.martinix.ui.GraphColor;

/**
 * <p>SliderUIPrefs is a container for all the properties needed to
 * lay out and draw a graph - things like dimensions, line styles,
 * colors, fonts, etcetera.
 *
 * <p>The units are arbitrary "world coordinate system" units,
 * <b>not</b> device units.
 *
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
 * @author will@apmindsf.com
 */
public class SliderUIPrefs {


  private double _nodeWidth;
  private double _nodeHeight;
  private double _nodeDepth;

  private double _horizontalSpacing;
  private double _verticalSpacing;

  private double _arrowHeadLength;
  private double _arrowHeadFlair;
  private double _arrowHeadOffset;

  /**
   * The default node and edge fill and stroke colors will be
   * interpolated between the colors 1 and 2, given some alpha value
   * (0.0 to 1.0).
   */
  GraphColor _nodeFillColor1;
  GraphColor _nodeFillColor2;
  GraphColor _edgeStrokeColor1;
  GraphColor _edgeStrokeColor2;

  /**
   * The edge stroke width will be interpolated between the stroke
   * widths 1 and 2, given some alpha value (0.0 to 1.0).
   */
  double _edgeStrokeWidth1;
  double _edgeStrokeWidth2;

  boolean _curvedEdge;

  DrawProps _nodeDrawProps;
  DrawProps _selectedNodeDrawProps;
  DrawProps _manualNodeDrawProps;
  DrawProps _selectedManualNodeDrawProps;

  DrawProps _edgeDrawProps;
  DrawProps _selectedEdgeDrawProps;

  private double _readableNodeTextSize;

  private int _maxLabelWidth;
  private boolean _useOneMinusToggles;

  private static final byte[] _lock = new byte[0];

  private static SliderUIPrefs _instance = null;





  /**
   * @return the one instance of SliderUIPrefs
   */
  public static SliderUIPrefs getInstance() {
    if (_instance == null) {
      synchronized(_lock) {
        if (_instance == null) {
          _instance = new SliderUIPrefs();
        }
      }
    }

    return (_instance);
  }




  /**
   * Default constructor behavior is to set values that
   * match our current "topic map visualization" design.
   */
  private SliderUIPrefs()
  {
    double nodeFontSize = 14.0;

    setNodeWidth(18.0);
    setNodeHeight(30.0);
    setNodeDepth(18.0);

    setReadableNodeTextSize(12.0);

    setHorizontalSpacing(82.0);
    setVerticalSpacing(38.8);

    setArrowHeadLength(10.0);
    setArrowHeadFlair(2.5);
    setArrowHeadOffset(20.0);

    // default node fill color (interpolated between 1 and 2)
    _nodeFillColor1 = new GraphColor(0xFF, 0xFF, 0xFF); // white
    _nodeFillColor2 = new GraphColor(0x7C, 0x7C, 0x7C); // dark gray

    // default edge stroke color (interpolated between 1 and 2)
    _edgeStrokeColor1 = new GraphColor(0xCC, 0xCC, 0xCC);  // light gray
    _edgeStrokeColor2 = new GraphColor(0x00, 0x00, 0x00);  // black

    // default edge stroke width (interpolated between 1 and 2)
    _edgeStrokeWidth1 = 0.5;
    _edgeStrokeWidth2 = 3.0;

    // default use curved lines
    _curvedEdge = true;

    // default node props
    _nodeDrawProps =
      new DrawProps(_edgeStrokeWidth2,                // strokeWidth
                    _edgeStrokeColor2,                // strokeColor
                    1.0,                              // strokeOpacity
                    _nodeFillColor2,                  // fillColor
                    1.0,                              // fillOpacity
                    new GraphColor(0xDD, 0xDD, 0xDD), // switch off color
                    new GraphColor(0xDD, 0x00, 0x00), // switch on color
                    new GraphColor(0x00, 0x00, 0x00), // fontColor
                    nodeFontSize,                     // fontSize
                    200                               // maxLabelWidth
                    );


    // selected node props - yellow outline
    _selectedNodeDrawProps = new DrawProps(_nodeDrawProps);
    _selectedNodeDrawProps.setStrokeColor(new GraphColor(0xFF, 0xEB, 0x14));

    // manual node (same as default node props)
    _manualNodeDrawProps = new DrawProps(_nodeDrawProps);

    // selected manual node props (same as selected node props)
    _selectedManualNodeDrawProps = new DrawProps(_selectedNodeDrawProps);

    // default edge props
    _edgeDrawProps =
      new DrawProps(_edgeStrokeWidth2,                // strokeWidth
                    _edgeStrokeColor2,                // strokeColor
                    1.0,                              // strokeOpacity
                    _edgeStrokeColor2,                // fillColor
                    1.0,                              // fillOpacity
                    new GraphColor(0xDD, 0xDD, 0xDD), // switch off color
                    new GraphColor(0xDD, 0x00, 0x00), // switch on color
                    new GraphColor(0x00, 0x00, 0x00), // fontColor
                    12.0,                             // fontSize
                    200                               // maxLabelWidth
                    );

    // selected edge props (same as default edge props)
    _selectedEdgeDrawProps = new DrawProps(_edgeDrawProps);

    _maxLabelWidth = 200;
    _useOneMinusToggles = false;
  }

  /**
   * Get the width (x-axis diameter) for nodes.
   *
   * @return the width (x-axis diameter) for nodes.
   */

  public double getNodeWidth() { return _nodeWidth; }

  /**
   * Get the height (y-axis diameter) for nodes.
   *
   * @return the height (y-axis diameter) for nodes.
   */

  public double getNodeHeight() { return _nodeHeight; }




  /**
   * Get the depth (z-axis diameter) for nodes.
   *
   * @return the depth (z-axis diameter) for nodes.
   */

  public double getNodeDepth() { return _nodeDepth; }

  /**
   * Get the minimum horizontal node spacing for the layout algorithms.
   *
   * @return the minimum horizontal node spacing for the layout algorithms.
   */

  public double getHorizontalSpacing() { return _horizontalSpacing; }

  /**
   * Get the minimum vertical node spacing for the layout algorithms.
   *
   * @return the minimum vertical node spacing for the layout algorithms.
   */

  public double getVerticalSpacing() { return _verticalSpacing; }

  /**
   * Get the edge arror head length.
   *
   * @return the edge arrow head length.
   */

  public double getArrowHeadLength() { return _arrowHeadLength; }

  /**
   * Get the arrow head flair (0.0 &lt; 1.0) - smaller numbers make
   * the arror head more pointy.
   *
   * @return the arrow head flair (0.0 &lt; 1.0) - smaller numbers
   * make the arror head more pointy.
   */

  public double getArrowHeadFlair() { return _arrowHeadFlair; }

  /**
   * Get the edge arror head offset.
   *
   * @return the edge arrow head offset. This is the distance of
   * the arrow tail from the bubble point.
   */

  public double getArrowHeadOffset() { return _arrowHeadOffset; }


  /**
   * Get the node DrawProps.
   *
   * @param ctx the context may have custom colors for the node fade effect or
   * null if use SliderUIPrefs default values.
   * @return <b>a copy of</b> the node DrawProps.
   */
  public DrawProps getNodeDrawProps(boolean selected,
                                    boolean manual,
                                    double sliderValue)
  {
    DrawProps props = null;
    double ratio = sliderValue / 100.0;

    if (selected) {
      if (manual) {
        props = new DrawProps(_selectedManualNodeDrawProps);
        props.setFillColor(getInterpolatedNodeFillColor(ratio));
        props.setStrokeWidth(getInterpolatedEdgeStrokeWidth(ratio));
      }
      else {
        props = new DrawProps(_selectedNodeDrawProps);
        props.setFillColor(getInterpolatedNodeFillColor(ratio));
        props.setStrokeWidth(getInterpolatedEdgeStrokeWidth(ratio));
      }
    }
    else {
      if (manual) {
        props = new DrawProps(_manualNodeDrawProps);
        props.setFillColor(getInterpolatedNodeFillColor(ratio));
        props.setStrokeColor(getInterpolatedEdgeStrokeColor(ratio));
        props.setStrokeWidth(getInterpolatedEdgeStrokeWidth(ratio));
      }
      else {
        props = new DrawProps(_nodeDrawProps);
        props.setFillColor(getInterpolatedNodeFillColor(ratio));
        props.setStrokeColor(getInterpolatedEdgeStrokeColor(ratio));
        props.setStrokeWidth(getInterpolatedEdgeStrokeWidth(ratio));
      }
    }

    return (props);
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
   * @return the maximum width of the label in pixels.
   */
  public int getMaxLabelWidth() {
    return (_maxLabelWidth);
  }


  /**
   * @return number of pixels between the edge of a label and the edge of
   * the node.
   */
  public int getLabelHorizontalMargin() {
    return (10);
  }

  /**
   * @return number of pixels between the top or bottom of a label and the
   * corresponding node edge.
   */
  public int getLabelVerticalMargin() {
    return (8);
  }


  /**
   * Get the edge DrawProps.
   *
   * @param ctx the context may have custom colors for the edge fade effect or
   * null if use SliderUIPrefs default values.
   * @return <b>a copy of</b> the edge DrawProps.
   */
  public DrawProps getEdgeDrawProps(double sourceSliderValue)
  {
    double ratio = sourceSliderValue / 100.0;
    DrawProps props = new DrawProps(_edgeDrawProps);

    props.setFillColor(getInterpolatedEdgeStrokeColor(ratio));
    props.setStrokeColor(getInterpolatedEdgeStrokeColor(ratio));
    props.setStrokeWidth(getInterpolatedEdgeStrokeWidth(ratio));

    return (props);
  }


  /**
   * Set the width (x-axis diameter) for nodes.
   *
   * @param nodeWidth
   */

  public void setNodeWidth(double nodeWidth)
  {
    _nodeWidth = nodeWidth;
  }

  /**
   * Set the height (y-axis diameter) for nodes.
   *
   * @param nodeHeight
   */

  public void setNodeHeight(double nodeHeight)
  {
    _nodeHeight = nodeHeight;
  }


  /**
   * Set the depth (z-axis diameter) for nodes.
   *
   * @param nodeDepth
   */

  public void setNodeDepth(double nodeDepth)
  {
    _nodeDepth = nodeDepth;
  }

  /**
   * set the max label width (in viewport coordinate)
   */
  public void setMaxLabelWidth(int width) {
    _maxLabelWidth = width;
  }


  /**
   * Set the minimum horizontal node spacing for the layout algorithms.
   *
   * @param horizontalSpacing
   */

  public void setHorizontalSpacing(double horizontalSpacing)
  {
    _horizontalSpacing = horizontalSpacing;
  }

  /**
   * Set the minimum vertical node spacing for the layout algorithms.
   *
   * @param verticalSpacing
   */

  public void setVerticalSpacing(double verticalSpacing)
  {
    _verticalSpacing = verticalSpacing;
  }

  /**
   * Set the edge arror head length.
   *
   * @param arrowHeadLength
   */

  public void setArrowHeadLength(double arrowHeadLength)
  {
    _arrowHeadLength = arrowHeadLength;
  }

  /**
   * Set the arrow head flair (0.0 &lt; 1.0) - smaller numbers make the
   * arror head more pointy.
   *
   * @param arrowHeadFlair
   */

  public void setArrowHeadFlair(double arrowHeadFlair)
  {
    _arrowHeadFlair = arrowHeadFlair;
  }

  /**
   * Set the edge arrow head offset.
   *
   * @param arrowHeadOffset This is the distance of
   * the arrow tail from the bubble point.
   */

  public void setArrowHeadOffset(double arrowHeadOffset)
  {
    _arrowHeadOffset = arrowHeadOffset;
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


  /**
   * how many characters does a label have until it's ellipsifed.
   */
  public int getMaxLabelLength() {
    return(35);
  }

  /**
   * If true, then the UI will draw the little switch on the node to
   * turn on the (1 - P) function for the probability.
   *
   * @param to use the one minus toggles in our UI or not.
   */
  public void setDrawModeSwitchOnNode(boolean u) {
    _useOneMinusToggles = u;
  }


  /**
   * If true, then the UI should draw the little switch on the node to
   * turn on the (1 - P) function for the probability.
   */
  public boolean getDrawModeSwitchOnNode() {
    return (_useOneMinusToggles);
  }

  /**
   * Required node property.  Value: A text label.
   */
  public static String getNodeLabelPropertyName() {
    return("label");
  }

  /**
   * This is an optional node property. If this is present and set to "true",
   * the probability at the node will be (1 - p) instead of p, where p
   * is the product of the probabilities of all incoming nodes.
   */
  public static String getProbabilityModePropertyName() {
    return("prmode");
  }

  public static String getSliderValuePropertyName() {
    return("slidervalue");
  }

  public String getNodeFontResourcePath() {
    return("com/appliedminds/martinix/slider/resources/InterstateRegular.ttf");
  }


  public static Color getCaretColor() {
    return(Color.green);
  }


  public static Color getSelectedTextColor() {
    return(Color.green);
  }


  /**
   * A node is either set to manual or automatic state.
   */
  public static String getManualNodePropertyName() {
    return ("manual");
  }


  /**
   * BNE node color will be interpolated between a source and a
   * destination color depending on the node's slider value.
   */
  public Color getNodeFadeSourceColor() {
    return (_nodeFillColor1.getColor());
  }


  /**
   * BNE node color will be interpolated between a source and a
   * destination color depending on the node's slider value.
   */
  public void setNodeFadeSourceColor(Color color) {
    _nodeFillColor1 = new GraphColor(color);
  }


  /**
   * BNE node color will be interpolated between a source and a
   * destination color depending on the node's slider value.
   */
  public Color getNodeFadeDestinationColor() {
    return (_nodeFillColor2.getColor());
  }


  /**
   * BNE node color will be interpolated between a source and a
   * destination color depending on the node's slider value.
   */
  public void setNodeFadeDestinationColor(Color color) {
    _nodeFillColor2 = new GraphColor(color);
  }


  /**
   * BNE edge color will be interpolated between a source and a
   * destination color depending on the tail's slider value.
   */
  public Color getEdgeFadeSourceColor() {
    return (_edgeStrokeColor1.getColor());
  }


  /**
   * BNE edge color will be interpolated between a source and a
   * destination color depending on the tail's slider value.
   */
  public void setEdgeFadeSourceColor(Color color) {
    _edgeStrokeColor1 = new GraphColor(color);
  }


  /**
   * BNE edge color will be interpolated between a source and a
   * destination color depending on the tail's slider value.
   */
  public Color getEdgeFadeDestinationColor() {
    return (_edgeStrokeColor2.getColor());
  }


  /**
   * BNE edge color will be interpolated between a source and a
   * destination color depending on the tail's slider value.
   */
  public void setEdgeFadeDestinationColor(Color color) {
    _edgeStrokeColor2 = new GraphColor(color);
  }


  /**
   * BNE edge width will be interpolated between a source and a
   * destination value depending on the tail's slider value.
   */
  public double getEdgeStrokeSourceWidth() {
    return (_edgeStrokeWidth1);
  }


  /**
   * BNE edge width will be interpolated between a source and a
   * destination value depending on the tail's slider value.
   */
  public void setEdgeStrokeSourceWidth(double value) {
    _edgeStrokeWidth1 = value;
  }


  /**
   * BNE edge width will be interpolated between a source and a
   * destination value depending on the tail's slider value.
   */
  public double getEdgeStrokeDestinationWidth() {
    return (_edgeStrokeWidth2);
  }


  /**
   * BNE edge width will be interpolated between a source and a
   * destination value depending on the tail's slider value.
   */
  public void setEdgeStrokeDestinationWidth(double value) {
    _edgeStrokeWidth2 = value;
  }


  /**
   * BNE edge may be curved or straight.
   */
  public boolean getCurvedLines() {
    return (_curvedEdge);
  }


  /**
   * BNE edge may be curved or straight.
   */
  public void setCurvedLines(boolean value) {
    _curvedEdge = value;
  }






  /**
   * Get the interpolated node fill color value.
   */
  private GraphColor getInterpolatedNodeFillColor(double ratio)
  {
    GraphColor src = _nodeFillColor1;
    GraphColor dst = _nodeFillColor2;
    return (getInterpolatedColor(ratio, src, dst));
  }


  /**
   * Get the interpolated edge stroke color value.
   */
  private GraphColor getInterpolatedEdgeStrokeColor(double ratio)
  {
    GraphColor src = _edgeStrokeColor1;
    GraphColor dst = _edgeStrokeColor2;

    return (getInterpolatedColor(ratio, src, dst));
  }


  /**
   * Get the interpolated edge stroke width value.
   */
  private double getInterpolatedEdgeStrokeWidth(double ratio)
  {
    double src = _edgeStrokeWidth1;
    double dst = _edgeStrokeWidth2;
    return (getInterpolatedDoubleValue(ratio, src, dst));
  }


  /**
   * Interpolate between the source and destination color given the ratio.
   */
  private GraphColor getInterpolatedColor(double ratio,
                                          GraphColor source,
                                          GraphColor destination)
  {
    if (ratio < 0) {
      return (source);
    }

    if (ratio > 1) {
      return (destination);
    }

    Color fSource = source.getColor();
    Color fDestination = destination.getColor();

    float red = (float) (fSource.getRed() + (ratio * (fDestination.getRed() - fSource.getRed())));
    float green = (float) (fSource.getGreen() + (ratio * (fDestination.getGreen() - fSource.getGreen())));
    float blue = (float) (fSource.getBlue() + (ratio * (fDestination.getBlue() - fSource.getBlue())));
    float alpha = (float) (fSource.getAlpha() + (ratio * (fDestination.getAlpha() - fSource.getAlpha())));

    return (new GraphColor(new Color(red/255, green/255, blue/255, alpha/255)));
  }


  /**
   * Interpolate between the source and destination value given the ratio.
   */
  private double getInterpolatedDoubleValue(double ratio,
                                            double source,
                                            double destination)
  {
    if (ratio < 0) {
      return (source);
    }

    if (ratio > 1) {
      return (destination);
    }

    if (destination < source) {
      return (destination);
    }

    return (source + ratio * (destination - source));
  }

} // end class SliderUIPrefs
