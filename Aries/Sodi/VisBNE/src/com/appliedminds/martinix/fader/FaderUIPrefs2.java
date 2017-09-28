package com.appliedminds.martinix.fader;

import com.appliedminds.martinix.ui.GraphColor;
import com.appliedminds.martinix.ui.BorderEdgeAttachStrategy;
import java.awt.Color;


public class FaderUIPrefs2 extends FaderUIPrefs {

  private static final GraphColor WHITE = new GraphColor(0xFF, 0xFF, 0xFF);
  private static final GraphColor BLACK = new GraphColor(0x00, 0x00, 0x00);

  private DrawProps _nodeDrawProps;
  private DrawProps _nodeOutlineDrawProps;
  private DrawProps _nodeHaloDrawProps;
  private DrawProps _edgeHaloDrawProps;
  private DrawProps _sliderToggleCircleDrawProps;
  private DrawProps _sliderToggleArrowDrawProps;
  private DrawProps _edgeConnectorHubDrawProps;
  private DrawProps _edgeDrawProps;

  private GraphColor _nodeColor;
  private GraphColor _defaultColor;
  private GraphColor _refutingColor;
  private GraphColor _supportingColor;
  private GraphColor _backgroundColor;
  private GraphColor _nodeHaloColor;
  private GraphColor _createEdgeHaloColor;
  private GraphColor _nodeFontColor;
  private GraphColor _edgeFontColor;
  private GraphColor _sliderToggleCircleColor;

  private double _connectorHubDiameter;
  private double _sliderToggleDiameter;
  private double _nodeFontSize;
  private double _edgeFontSize;
  private double _minEdgeWidth;
  private double _maxEdgeWidth;
  private double _nodeOutlinePadding;  // the padding between node outline and the node
  private double _nodeHaloPadding;     // the padding between node halo and the node
  private double _nodeRadius;        // the radius of the rounded rectangle node
  private double _nodeOutlineRadius; // the radius of the rounded rectangle node outline
  private double _nodeHaloRadius;    // the radius of the rounded rectangle node halo

  private int _maxLabelWidth;

  private float _minAlpha = 0.05f;

  public FaderUIPrefs2() {
    super();

    _nodeColor = new GraphColor(0xFF, 0xFF, 0xFF,
                                0xE8, 0xE8, 0xE8,
                                GraphColor.VERTICAL_GRADIENT);
    _defaultColor = new GraphColor(0x64, 0x64, 0x64);
    _refutingColor = new GraphColor(0xAC, 0x00, 0x10);
    _supportingColor = new GraphColor(0x94, 0xAA, 0x00);
    _backgroundColor = new GraphColor(0xFF, 0xFF, 0xFF);
    _nodeHaloColor = new GraphColor(0x35, 0x7C, 0xE8);
    _createEdgeHaloColor = new GraphColor(0x62, 0x99, 0xED);
    _nodeFontColor = new GraphColor(0x1C, 0x1C, 0x1C);
    _edgeFontColor = new GraphColor(0xFF, 0xFF, 0xFF);
    _sliderToggleCircleColor = new GraphColor(0x5B, 0x60, 0x68);

    _connectorHubDiameter = 5.0;
    _sliderToggleDiameter = 14.0;
    _nodeFontSize = 12.0;
    _edgeFontSize = 12.0;
    _minEdgeWidth = 1.0;
    _maxEdgeWidth = 16.0;
    _maxLabelWidth = 140;
    _nodeOutlinePadding = 8.0;
    _nodeHaloPadding = 10.0;
    _nodeRadius = 3.0;
    _nodeOutlineRadius = 6.0;
    _nodeHaloRadius = 6.0;

    _nodeDrawProps = new DrawProps(1.0,                // strokeWidth
                                   _nodeColor,         // strokeColor
                                   1.0,                // strokeOpacity
                                   _nodeColor,         // fillColor
                                   1.0,                // fillOpacity
                                   _nodeFontColor,     // fontColor
                                   _nodeFontSize,      // fontSize
                                   _maxLabelWidth);    // maxLabelWidth

    _nodeOutlineDrawProps = new DrawProps(_nodeDrawProps);

    _nodeHaloDrawProps = new DrawProps(1.0,            // strokeWidth
                                       _nodeHaloColor, // strokeColor
                                       1.0,            // strokeOpacity
                                       _nodeHaloColor, // fillColor
                                       1.0,            // fillOpacity
                                       _nodeFontColor, // fontColor
                                       _nodeFontSize,  // fontSize
                                       _maxLabelWidth);// maxLabelWidth;

    _sliderToggleCircleDrawProps = new DrawProps(1.0,
                                                 _sliderToggleCircleColor,
                                                 1.0,
                                                 _sliderToggleCircleColor,
                                                 1.0,
                                                 _nodeFontColor,
                                                 _nodeFontSize,
                                                 _maxLabelWidth);

    _sliderToggleArrowDrawProps = new DrawProps(1.0,
                                                _nodeColor,
                                                1.0,
                                                _nodeColor,
                                                1.0,
                                                _nodeFontColor,
                                                _nodeFontSize,
                                                _maxLabelWidth);

    _edgeConnectorHubDrawProps =
      new DrawProps(1.0,             // strokeWidth
                    _defaultColor,   // strokeColor
                    1.0,             // strokeOpacity
                    _nodeColor,      // fillColor
                    1.0,             // fillOpacity
                    _nodeFontColor,  // fontColor
                    _nodeFontSize,   // fontSize
                    _maxLabelWidth); // maxLabelWidth;

    _edgeDrawProps = new DrawProps(1.0,                // strokeWidth
                                   _defaultColor,      // strokeColor
                                   1.0,                // strokeOpacity
                                   _defaultColor,      // fillColor
                                   1.0,                // fillOpacity
                                   _edgeFontColor,     // fontColor
                                   _edgeFontSize,      // fontSize
                                   _maxLabelWidth);    // maxLabelWidth

    _edgeHaloDrawProps = new DrawProps(3.0,            // strokeWidth
                                       _nodeHaloColor, // strokeColor
                                       1.0,            // strokeOpacity
                                       _nodeHaloColor, // fillColor
                                       1.0,            // fillOpacity
                                       _nodeFontColor, // fontColor
                                       _nodeFontSize,  // fontSize
                                       _maxLabelWidth);// maxLabelWidth;
  }

  public int getDefaultEdgeSliderValue() {
    return (0);
  }

  public int getDefaultNodeSliderValue() {
    return (50);
  }

  public double getConnectorHubDiameter() {
    return (_connectorHubDiameter);
  }

  public double getSliderToggleDiameter() {
    return (_sliderToggleDiameter);
  }

  public double getNodeHaloPadding() {
    return (_nodeHaloPadding);
  }

  public double getNodeOutlinePadding() {
    return (_nodeOutlinePadding);
  }

  public double getNodeRadius() {
    return (_nodeRadius);
  }

  public double getNodeOutlineRadius() {
    return (_nodeOutlineRadius);
  }

  public double getNodeHaloRadius() {
    return (_nodeHaloRadius);
  }

  public GraphColor getBackgroundColor() {
    return (_backgroundColor);
  }


  /**
   * Get the draw props of the main node element containing the node
   * label.
   */
  public DrawProps getNodeDrawProps(boolean mouseOver,
                                    boolean mouseDown)
  {
    DrawProps props = new DrawProps(_nodeDrawProps);

    if (mouseOver) {
      // nothing for now
    }

    if (mouseDown) {
      // 40% black overlay
    }

    return (props);
  }

  /**
   * Get the node outline draw props.
   *
   * @param sliderValue value between 0 and 100.
   * @param weight 0 means default (neither supporting nor
   * refuting node), positive value (> 0) means supporting node and
   * negative value (< 0) means refuting node.
   */
  public DrawProps getNodeOutlineDrawProps(int sliderValue,
                                           int weight,
                                           boolean mouseOver,
                                           boolean mouseDown)
  {
    DrawProps props = new DrawProps(_nodeOutlineDrawProps);

    double ratio = sliderValue * 0.01;

    GraphColor color = getInterpolatedNodeOutlineColor(ratio, weight);
    props.setStrokeColor(color);
    props.setFillColor(color);

    if (mouseOver) {
      // nothing for now
    }

    if (mouseDown) {
      // 40% black overlay
    }

    return (props);
  }


  public DrawProps getNodeHaloDrawProps(boolean creatingEdge) {
    DrawProps props = new DrawProps(_nodeHaloDrawProps);

    if (creatingEdge) {
      props.setStrokeColor(_createEdgeHaloColor);
      props.setFillColor(_createEdgeHaloColor);
    }

    return (props);
  }

  public DrawProps getEdgeHaloDrawProps() {
    return (new DrawProps(_edgeHaloDrawProps));
  }

  public DrawProps getSliderToggleCircleDrawProps(boolean mouseOver,
                                                  boolean mouseDown)
  {
    DrawProps props = new DrawProps(_sliderToggleCircleDrawProps);

    if (mouseOver) {
      // nothing for now
    }

    if (mouseDown) {
      props.setStrokeColor(BLACK);
      props.setFillColor(BLACK);
    }

    return (props);
  }


  public DrawProps getSliderToggleArrowDrawProps(boolean mouseOver,
                                                  boolean mouseDown)
  {
    DrawProps props = new DrawProps(_sliderToggleArrowDrawProps);

    if (mouseOver) {
      // nothing for now
    }

    if (mouseDown) {
      // nothing for now
    }

    return (props);
  }

  public DrawProps getEdgeConnectorHubDrawProps(boolean selected,
                                                boolean mouseOver,
                                                boolean mouseDown)
  {
    DrawProps props = new DrawProps(_edgeConnectorHubDrawProps);

    if (selected) {
      props.setFillColor(_createEdgeHaloColor);
    }

    if (mouseOver) {
      // nothing for now
    }

    if (mouseDown) {
      // 40% black overlay
    }

    return (props);
  }

  /**
   * @param sliderVal the current value of the edge slider
   * @return the DrawProps for edges without any slider-based color
   * interpolation
   */
  public DrawProps getUninterpolatedEdgeDrawProps(int sliderVal) {
    if (sliderVal >= 0) {
      DrawProps props = new DrawProps(_edgePositiveDrawProps);
      if (sliderVal == 0)
        props.setFillColor(_defaultColor);
      else
        props.setFillColor(_supportingColor);
      return props;
    }
    else {
      DrawProps props = new DrawProps(_edgeNegativeDrawProps);
      props.setFillColor(_refutingColor);
      return props;
    }
  }


  public DrawProps getEdgeDrawProps(int sliderValue,
                                    int sourceValue)
  {
    DrawProps props = new DrawProps(_edgeDrawProps);

    GraphColor color = _defaultColor;
    if (sliderValue > 0) { // supporting
      color = _supportingColor;
    }
    else if (sliderValue < 0) { // refuting
      color = _refutingColor;
    }

    sliderValue = Math.abs(sliderValue);

    // edge thickness
    double ratio =
      (double)(sliderValue - _edgeSliderMinimum) * _edgeSliderRangeInverse;;

    // edge transparency
    float alpha = (float) ((sourceValue - _nodeSliderMinimum) *
                           _nodeSliderRangeInverse);

    color = getInterpolatedColor(ratio, color, color, alpha);
    double width = getInterpolatedDoubleValue(ratio,
                                              _minEdgeWidth,
                                              _maxEdgeWidth);
    props.setStrokeColor(color);
    props.setFillColor(color);
    props.setStrokeWidth(width);

    return (props);
  }


  public void setDefaultColor(Color c)
  {
    _defaultColor = new GraphColor(c);
  }

  public void setSupportingColor(Color c)
  {
    _supportingColor = new GraphColor(c);
  }

  public void setRefutingColor(Color c)
  {
    _refutingColor = new GraphColor(c);
  }


  private GraphColor getInterpolatedNodeOutlineColor(double ratio,
                                                     int weight) {
    GraphColor color = _defaultColor;
    if (weight > 0) {
      color = _supportingColor;
    }
    else if (weight < 0) {
      color = _refutingColor;
    }

    return (getInterpolatedColor(ratio, color, color,
                                 Math.max((float)ratio, _minAlpha)));
  }


} // end class "FaderUIPrefs2"
