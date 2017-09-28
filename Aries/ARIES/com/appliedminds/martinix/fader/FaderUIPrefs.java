package com.appliedminds.martinix.fader;

import com.appliedminds.martinix.ui.GraphColor;
import com.appliedminds.martinix.ui.BorderEdgeAttachStrategy;
import java.awt.Color;
import java.net.MalformedURLException;
import java.net.URL;


/**
 * FaderUIPrefs.java
 *
 *
 * Created: Mon Aug 26 11:07:38 2002
 *
 * @author <a href="mailto: daepark@apmindsf.com"</a>
 * @version
 */
public class FaderUIPrefs {

  private static final float _255 = (float)(1.0 / 255.0);

  protected int _edgeSliderMinimum;
  protected int _edgeSliderMaximum;
  protected double _edgeSliderRangeInverse;

  protected int _nodeSliderMinimum;
  protected int _nodeSliderMaximum;
  protected double _nodeSliderRangeInverse;

  protected int _productMin;
  protected int _productMax;
  protected float _productRangeInverse;

  private float _minEdgeColorAlpha;

  private double _nodeWidth;
  private double _nodeHeight;
  private double _nodeDepth;

  private double _horizontalSpacing;
  private double _verticalSpacing;

  private double _arrowHeadLength;
  private double _arrowHeadFlair;
  private double _arrowHeadOffset;

  /**
   * The default node and edge (positive and negative) fill and stroke
   * colors will be interpolated between the colors 1 and 2, given
   * some alpha value (0.0 to 1.0).
   */
  GraphColor _nodeFillColor1;
  GraphColor _nodeFillColor2;

  GraphColor _nodeStrokeColor1;
  GraphColor _nodeStrokeColor2;

  GraphColor _edgePositiveStrokeColor1;
  GraphColor _edgePositiveStrokeColor2;

  GraphColor _edgeNegativeStrokeColor1;
  GraphColor _edgeNegativeStrokeColor2;

  /**
   * The node stroke width will be interpolated between the stroke
   * widths 1 and 2, given some alpha value (0.0 to 1.0).
   */
  double _nodeStrokeWidth1;
  double _nodeStrokeWidth2;

  /**
   * The edge stroke width will be interpolated between the stroke
   * widths 1 and 2, given some alpha value (0.0 to 1.0).
   */
  double _edgeStrokeWidth1;
  double _edgeStrokeWidth2;

  boolean _curvedEdge;
  boolean _sliders;

  DrawProps _nodeDrawProps;
  DrawProps _selectedNodeDrawProps;

  DrawProps _edgePositiveDrawProps;
  DrawProps _edgeNegativeDrawProps;

  private double _readableNodeTextSize;
  private int _maxLabelWidth;

  private double _iconHeight;
  private double _iconWidth;
  private double _interIconSpacing;

  private BorderEdgeAttachStrategy _attachStrategy;

  private double _sliderValueFontSize;
  private GraphColor _sliderValueFontColor;
  private GraphColor _manualBackgroundColor;


  /*
   * Use getInstance for an instance of FaderUIPrefs.
   */
  public FaderUIPrefs() {
    _edgeSliderMinimum = 0;
    _edgeSliderMaximum = 10;
    _edgeSliderRangeInverse =
      1.0 / (double)(_edgeSliderMaximum - _edgeSliderMinimum);

    _nodeSliderMinimum = 0;
    _nodeSliderMaximum = 100;
    _nodeSliderRangeInverse =
      1.0 / (double)(_nodeSliderMaximum - _nodeSliderMinimum);

    _productMin = _edgeSliderMinimum * _nodeSliderMinimum;
    _productMax = _edgeSliderMaximum * _nodeSliderMaximum;
    _productRangeInverse =
      1.0f / (float)(_productMax - _productMin);

    _minEdgeColorAlpha = 0.10f;

    double nodeFontSize = 12.0;
    _sliderValueFontSize = 12.0;
    _sliderValueFontColor = new GraphColor(0x1C, 0x1C, 0x1C);

    _manualBackgroundColor =
      new GraphColor(new Color(0x47, 0x95, 0xAA, 0x20));

    setNodeWidth(18.0);
    setNodeHeight(30.0);
    setNodeDepth(18.0);

    setReadableNodeTextSize(12.0);

    setHorizontalSpacing(82.0);
    setVerticalSpacing(38.8);

    setArrowHeadLength(15.0);
    setArrowHeadFlair(3.0);
    setArrowHeadOffset(1.0);

    // default node fill color (interpolated between 1 and 2)
//     _nodeFillColor1 = new GraphColor(0xFF, 0xFF, 0xFF); // white
//     _nodeFillColor2 = new GraphColor(0xFF, 0xFF, 0xFF); // white
    _nodeFillColor1 = new GraphColor(0xDD, 0xE5, 0xEC); // light blue
    _nodeFillColor2 = new GraphColor(0x0E, 0x5D, 0x93); // dark blue

    // default node stroke color (interpolated between 1 and 2)
//     _nodeStrokeColor1 = new GraphColor(0xCC, 0xCC, 0xCC); // light gray
//     _nodeStrokeColor2 = new GraphColor(0x00, 0x00, 0x00); // black
    _nodeStrokeColor1 = new GraphColor(0xFF, 0xFF, 0xFF); // white
    _nodeStrokeColor2 = new GraphColor(0xFF, 0xFF, 0xFF); // white

    // default node stroke width (interpolated between 1 and 2)
//     _nodeStrokeWidth1 = 0.5;
//     _nodeStrokeWidth2 = 3.0;
    _nodeStrokeWidth1 = 1.0;
    _nodeStrokeWidth2 = 1.0;

    // default positive edge stroke color (interpolated between 1 and 2)
//     _edgePositiveStrokeColor1 = new GraphColor(0xCC, 0xCC, 0xCC); // light gray
//     _edgePositiveStrokeColor2 = new GraphColor(0x00, 0x00, 0x00); // black
    _edgePositiveStrokeColor1 = new GraphColor(0x65, 0x65, 0x65); // gray
    _edgePositiveStrokeColor2 = new GraphColor(0x65, 0x65, 0x65); // gray

    // default negative edge stroke color (interpolated between 1 and 2)
//     _edgeNegativeStrokeColor1 = new GraphColor(0xCC, 0xCC, 0xCC); // light gray
//     _edgeNegativeStrokeColor2 = new GraphColor(0x00, 0x00, 0x00); // black
    _edgeNegativeStrokeColor1 = new GraphColor(0x65, 0x65, 0x65); // gray
    _edgeNegativeStrokeColor2 = new GraphColor(0x65, 0x65, 0x65); // gray

    // default edge stroke width (interpolated between 1 and 2)
//     _edgeStrokeWidth1 = 0.5;
//     _edgeStrokeWidth2 = 3.0;
    _edgeStrokeWidth1 = 1.0;
    _edgeStrokeWidth2 = 1.0;

    // default use curved lines
    _curvedEdge = true;

    // default draw sliders
    _sliders = true;

    _maxLabelWidth = 150;

    // default node props
    _nodeDrawProps =
      new DrawProps(_nodeStrokeWidth2,                // strokeWidth
                    _nodeStrokeColor2,                // strokeColor
                    1.0,                              // strokeOpacity
                    new GraphColor(_nodeFillColor1.getColor(),
                                   _nodeFillColor2.getColor(),
                                   GraphColor.VERTICAL_GRADIENT),// fillColor
                    1.0,                              // fillOpacity
                    new GraphColor(0x1C, 0x1C, 0x1C), // fontColor
                    nodeFontSize,                     // fontSize
                    _maxLabelWidth                    // maxLabelWidth
                    );

    // selected node props - yellow outline
    _selectedNodeDrawProps = new DrawProps(_nodeDrawProps);
    _selectedNodeDrawProps.setStrokeColor(new GraphColor(0xc4, 0x7f, 0x7f));


    // default positive edge props
    _edgePositiveDrawProps =
      new DrawProps(_edgeStrokeWidth2,                // strokeWidth
                    _edgePositiveStrokeColor2,        // strokeColor
                    1.0,                              // strokeOpacity
                    _edgePositiveStrokeColor2,        // fillColor
                    1.0,                              // fillOpacity
                    new GraphColor(0xFF, 0xFF, 0xFF), // fontColor
                    12.0,                             // fontSize
                    _maxLabelWidth                    // maxLabelWidth
                    );

    // default negative edge props
    _edgeNegativeDrawProps =
      new DrawProps(_edgeStrokeWidth2,                // strokeWidth
                    _edgeNegativeStrokeColor2,        // strokeColor
                    1.0,                              // strokeOpacity
                    _edgeNegativeStrokeColor2,        // fillColor
                    1.0,                              // fillOpacity
                    new GraphColor(0xFF, 0xFF, 0xFF), // fontColor
                    12.0,                             // fontSize
                    _maxLabelWidth                    // maxLabelWidth
                    );


    setIconHeight(18.0);
    setIconWidth(18.0);
    setInterIconSpacing(0.0);

    setBorderEdgeAttachStrategy(new BorderEdgeAttachStrategy() {
        public double getEastWestAngle() {
          return (45);
        }

        public int getTailNodeAttachPoint() {
          return (RELATIVE);
        }

        public int getHeadNodeAttachPoint() {
          return (RELATIVE);
        }
      });

  }


  /**
   * Get the minimum slider value of the node.
   */
  public int getNodeSliderMinimum() {
    return (_nodeSliderMinimum);
  }


  /**
   * Get the maximum slider value of the node.
   */
  public int getNodeSliderMaximum() {
    return (_nodeSliderMaximum);
  }


  protected double getNodeSliderRangeInverse() {
    return (_nodeSliderRangeInverse);
  }


  /**
   * Get the minimum slider value of the edge.
   */
  public int getEdgeSliderMinimum() {
    return (_edgeSliderMinimum);
  }


  /**
   * Get the maximum slider value of the edge.
   */
  public int getEdgeSliderMaximum() {
    return (_edgeSliderMaximum);
  }


  protected double getEdgeSliderRangeInverse() {
    return (_edgeSliderRangeInverse);
  }


  /**
   * Get the width (x-axis diameter) for nodes.
   *
   * @return the width (x-axis diameter) for nodes.
   */
  public double getNodeWidth() { return (_nodeWidth); }


  /**
   * Get the height (y-axis diameter) for nodes.
   *
   * @return the height (y-axis diameter) for nodes.
   */
  public double getNodeHeight() { return (_nodeHeight); }


  /**
   * Get the depth (z-axis diameter) for nodes.
   *
   * @return the depth (z-axis diameter) for nodes.
   */
  public double getNodeDepth() { return (_nodeDepth); }


  /**
   * Get the minimum horizontal node spacing for the layout algorithms.
   *
   * @return the minimum horizontal node spacing for the layout algorithms.
   */
  public double getHorizontalSpacing() { return (_horizontalSpacing); }


  /**
   * Get the minimum vertical node spacing for the layout algorithms.
   *
   * @return the minimum vertical node spacing for the layout algorithms.
   */
  public double getVerticalSpacing() { return (_verticalSpacing); }


  /**
   * Get the edge arror head length.
   *
   * @return the edge arrow head length.
   */
  public double getArrowHeadLength() { return (_arrowHeadLength); }


  /**
   * Get the arrow head flair (0.0 &lt; 1.0) - smaller numbers make
   * the arror head more pointy.
   *
   * @return the arrow head flair (0.0 &lt; 1.0) - smaller numbers
   * make the arror head more pointy.
   */
  public double getArrowHeadFlair() { return (_arrowHeadFlair); }


  /**
   * Get the edge arror head offset.
   *
   * @return the edge arrow head offset. This is the distance of
   * the arrow tail from the bubble point.
   */
  public double getArrowHeadOffset() { return (_arrowHeadOffset); }


  /**
   * Get the node DrawProps.
   *
   * @return <b>a copy of</b> the node DrawProps.
   */
  public DrawProps getNodeDrawProps(boolean selected,
                                    int sliderValue)
  {
    DrawProps props = new DrawProps(_nodeDrawProps);

    double ratio = sliderValue * 0.01;

    GraphColor color = getInterpolatedNodeFillColor(ratio);
    double width = getInterpolatedNodeStrokeWidth(ratio);

    if (selected) {
      props = new DrawProps(_selectedNodeDrawProps);
      props.setFillColor(color);
      props.setStrokeWidth(width);
    }
    else {
      props = new DrawProps(_nodeDrawProps);
      props.setFillColor(color);
      props.setStrokeColor(getInterpolatedNodeStrokeColor(ratio));
      props.setStrokeWidth(width);
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
    return (_readableNodeTextSize);
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
    return (13);
  }


  /**
   * @return number of pixels between the top or bottom of a label and the
   * corresponding node edge.
   */
  public int getLabelVerticalMargin() {
    return (13);
  }


  /**
   * Get the edge DrawProps.
   *
   * @param sliderValue the edge slider value
   * @param positive edges can be positive or negative types
   * @return <b>a copy of</b> the edge DrawProps.
   */
  public DrawProps getEdgeDrawProps(int sliderValue,
                                    int sourceValue,
                                    boolean positive)
  {
    double ratio =
      (double)(sliderValue - _edgeSliderMinimum) * _edgeSliderRangeInverse;

    float alpha = ((float)(sliderValue * sourceValue - _productMin)) *
      _productRangeInverse;

    DrawProps props = null;
    GraphColor color = null;

    if (positive) {
      props = new DrawProps(_edgePositiveDrawProps);
      color = getInterpolatedPositiveEdgeStrokeColor(ratio, alpha);
      props.setFillColor(color);
      props.setStrokeColor(color);
    }
    else {
      props = new DrawProps(_edgeNegativeDrawProps);
      color = getInterpolatedNegativeEdgeStrokeColor(ratio, alpha);
      props.setFillColor(color);
      props.setStrokeColor(color);
    }

    props.setStrokeWidth(getInterpolatedEdgeStrokeWidth(ratio));

    return (props);
  }


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
   * Get the edge attach strategy.
   *
   * @see #setBorderEdgeAttachStrategy
   */
  public BorderEdgeAttachStrategy getBorderEdgeAttachStrategy() {
    return (_attachStrategy);
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
   * Set the edge attach strategy.
   *
   * @param strategy a martinix.ui.BorderEdgeAttachStrategy
   */
  public void setBorderEdgeAttachStrategy(BorderEdgeAttachStrategy strategy) {
    _attachStrategy = strategy;
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
   * Get the arc width for the round rectangle for the node.
   */
  public double getNodeRectangleArcWidth() {
    return (10.0);
  }


  /**
   * Get the arc height for the round rectangle for the node.
   */
  public double getNodeRectangleArcHeight() {
    return (10.0);
  }


  /**
   * how many characters does a label have until it's ellipsifed.
   */
  public int getMaxLabelLength() {
    return(35);
  }


  public String getFontResourcePath() {
    //    return("com/appliedminds/martinix/fader/resources/InterstateRegular.ttf");
    return("com/appliedminds/martinix/fader/resources/myriad_roman.ttf");
  }


  public Color getCaretColor() {
    return(Color.green);
  }


  public Color getSelectedTextColor() {
    return(Color.green);
  }


  public URL getTypeMapURL() {
    try {
      URL dir = getClass().getClassLoader().getResource("com/appliedminds/martinix/fader/resources/");
      URL u = new URL(dir.toString() + "dataset.xml");
      return (u);
    }
    catch(MalformedURLException e) {
      throw (new RuntimeException("FaderUIPrefs: bad type map url.  Error = " + e));
    }
  }


  /**
   * A fader node color will be interpolated between a source and a
   * destination color depending on the node's slider value.
   */
  public Color getNodeFadeSourceColor() {
    return (_nodeFillColor1.getColor());
  }


  /**
   * A fader node color will be interpolated between a source and a
   * destination color depending on the node's slider value.
   */
  public void setNodeFadeSourceColor(Color color) {
    _nodeFillColor1 = new GraphColor(color);
  }


  /**
   * A fader node color will be interpolated between a source and a
   * destination color depending on the node's slider value.
   */
  public Color getNodeFadeDestinationColor() {
    return (_nodeFillColor2.getColor());
  }


  /**
   * A fader node color will be interpolated between a source and a
   * destination color depending on the node's slider value.
   */
  public void setNodeFadeDestinationColor(Color color) {
    _nodeFillColor2 = new GraphColor(color);
  }


  /**
   * A fader node color will be interpolated between a source and a
   * destination color depending on the node's slider value.
   */
  public Color getNodeStrokeSourceColor() {
    return (_nodeStrokeColor1.getColor());
  }


  /**
   * A fader node color will be interpolated between a source and a
   * destination color depending on the node's slider value.
   */
  public void setNodeStrokeSourceColor(Color color) {
    _nodeStrokeColor1 = new GraphColor(color);
  }


  /**
   * A fader node color will be interpolated between a source and a
   * destination color depending on the node's slider value.
   */
  public Color getNodeStrokeDestinationColor() {
    return (_nodeStrokeColor2.getColor());
  }


  /**
   * A fader node color will be interpolated between a source and a
   * destination color depending on the node's slider value.
   */
  public void setNodeStrokeDestinationColor(Color color) {
    _nodeStrokeColor2 = new GraphColor(color);
  }


  /**
   * A fader node stroke width will be interpolated between a source
   * and a destination value depending its slider value.
   */
  public double getNodeStrokeSourceWidth() {
    return (_nodeStrokeWidth1);
  }


  /**
   * A fader node stroke width will be interpolated between a source
   * and a destination value depending its slider value.
   */
  public void setNodeStrokeSourceWidth(double value) {
    _nodeStrokeWidth1 = value;
  }


  /**
   * A fader node stroke width will be interpolated between a source
   * and a destination value depending its slider value.
   */
  public double getNodeStrokeDestinationWidth() {
    return (_nodeStrokeWidth2);
  }


  /**
   * A fader node stroke width will be interpolated between a source
   * and a destination value depending its slider value.
   */
  public void setNodeStrokeDestinationWidth(double value) {
    _nodeStrokeWidth2 = value;
  }


  /**
   * A positive edge color will be interpolated between a source
   * and a destination color depending on its slider value.
   */
  public Color getEdgePositiveFadeSourceColor() {
    return (_edgePositiveStrokeColor1.getColor());
  }


  /**
   * A positive edge color will be interpolated between a source and a
   * destination color depending on its slider value.
   */
  public void setEdgePositiveFadeSourceColor(Color color) {
    _edgePositiveStrokeColor1 = new GraphColor(color);
  }


  /**
   * A positive edge color will be interpolated between a source and a
   * destination color depending on its slider value.
   */
  public Color getEdgePositiveFadeDestinationColor() {
    return (_edgePositiveStrokeColor2.getColor());
  }


  /**
   * A positive edge color will be interpolated between a source and a
   * destination color depending on its slider value.
   */
  public void setEdgePositiveFadeDestinationColor(Color color) {
    _edgePositiveStrokeColor2 = new GraphColor(color);
  }


  /**
   * A negative edge color will be interpolated between a source
   * and a destination color depending on its slider value.
   */
  public Color getEdgeNegativeFadeSourceColor() {
    return (_edgeNegativeStrokeColor1.getColor());
  }


  /**
   * A negative edge color will be interpolated between a source and a
   * destination color depending on its slider value.
   */
  public void setEdgeNegativeFadeSourceColor(Color color) {
    _edgeNegativeStrokeColor1 = new GraphColor(color);
  }


  /**
   * A negative edge color will be interpolated between a source and a
   * destination color depending on its slider value.
   */
  public Color getEdgeNegativeFadeDestinationColor() {
    return (_edgeNegativeStrokeColor2.getColor());
  }


  /**
   * A negative edge color will be interpolated between a source and a
   * destination color depending on its slider value.
   */
  public void setEdgeNegativeFadeDestinationColor(Color color) {
    _edgeNegativeStrokeColor2 = new GraphColor(color);
  }


  /**
   * A fader edge width will be interpolated between a source and a
   * destination value depending on its slider value.
   */
  public double getEdgeStrokeSourceWidth() {
    return (_edgeStrokeWidth1);
  }


  /**
   * A fader edge width will be interpolated between a source and a
   * destination value depending on its slider value.
   */
  public void setEdgeStrokeSourceWidth(double value) {
    _edgeStrokeWidth1 = value;
  }


  /**
   * A fader edge width will be interpolated between a source and a
   * destination value depending on its slider value.
   */
  public double getEdgeStrokeDestinationWidth() {
    return (_edgeStrokeWidth2);
  }


  /**
   * A fader edge width will be interpolated between a source and a
   * destination value depending on its slider value.
   */
  public void setEdgeStrokeDestinationWidth(double value) {
    _edgeStrokeWidth2 = value;
  }


  /**
   * Manual graph elements will have this color as it's background.
   *
   * @see FaderUtil#isManual
   */
  public Color getManualBackgroundColor() {
    return (_manualBackgroundColor.getColor());
  }


  /**
   * Manual graph elements will have this color as it's background.
   *
   * @see FaderUtil#isManual
   */
  public void setManualBackgroundColor(Color color) {
    _manualBackgroundColor = new GraphColor(color);
  }


  /**
   * A fader edge may be curved or straight.
   */
  public boolean getCurvedLines() {
    return (_curvedEdge);
  }


  /**
   * A fader edge may be curved or straight.
   */
  public void setCurvedLines(boolean value) {
    _curvedEdge = value;
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
   * Set the spacing between icons.
   *
   * @param d the spacing.
   */
  public void setInterIconSpacing(double d) {
    _interIconSpacing = d;
  }


  /**
   * Draw sliders or not.
   */
  public boolean getSliders() {
    return (_sliders);
  }


  /**
   * Draw sliders or not.
   */
  public void setSliders(boolean b) {
    _sliders = b;
  }


  public double getSliderValueFontSize() {
    return (_sliderValueFontSize);
  }


  public GraphColor getSliderValueFontColor() {
    return (_sliderValueFontColor);
  }


  /**
   * Get the interpolated node fill color value.
   */
  private GraphColor getInterpolatedNodeFillColor(double ratio) {
    GraphColor src = _nodeFillColor1;
    GraphColor dst = _nodeFillColor2;

    //    return (getInterpolatedColor(ratio, src, dst));
    return (new GraphColor(_nodeFillColor1.getColor(),
                           _nodeFillColor2.getColor(),
                           GraphColor.VERTICAL_GRADIENT));
  }


  /**
   * Get the interpolated node stroke color value.
   */
  private GraphColor getInterpolatedNodeStrokeColor(double ratio) {
    GraphColor src = _nodeStrokeColor1;
    GraphColor dst = _nodeStrokeColor2;

    return (getInterpolatedColor(ratio, src, dst));
  }


  /**
   * Get the interpolated node stroke width value.
   */
  private double getInterpolatedNodeStrokeWidth(double ratio) {
    double src = _nodeStrokeWidth1;
    double dst = _nodeStrokeWidth2;

    return (getInterpolatedDoubleValue(ratio, src, dst));
  }


  private GraphColor getInterpolatedPositiveEdgeStrokeColor(double ratio,
                                                            float alpha)
  {
    GraphColor src = _edgePositiveStrokeColor1;
    GraphColor dst = _edgePositiveStrokeColor2;

    return (getInterpolatedColor(ratio, src, dst, alpha));
  }


  private GraphColor getInterpolatedNegativeEdgeStrokeColor(double ratio,
                                                            float alpha)
  {
    GraphColor src = _edgeNegativeStrokeColor1;
    GraphColor dst = _edgeNegativeStrokeColor2;

    return (getInterpolatedColor(ratio, src, dst, alpha));
  }


  /**
   * Get the interpolated edge stroke width value.
   */
  private double getInterpolatedEdgeStrokeWidth(double ratio) {
    double src = _edgeStrokeWidth1;
    double dst = _edgeStrokeWidth2;

    return (getInterpolatedDoubleValue(ratio, src, dst));
  }


  /**
   * Interpolate between the source and destination color given the ratio.
   */
  protected GraphColor getInterpolatedColor(double ratio,
                                            GraphColor source,
                                            GraphColor destination)
  {
    return (getInterpolatedColor(ratio, source, destination, -1));
  }


  protected GraphColor getInterpolatedColor(double ratio,
                                            GraphColor source,
                                            GraphColor destination,
                                            float alpha)
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

    if (alpha < 0) {
      alpha = (float) (fSource.getAlpha() + (ratio * (fDestination.getAlpha() - fSource.getAlpha())));
      alpha *= _255;
    }

    return (new GraphColor(new Color(red * _255, green * _255, blue * _255, Math.max(alpha, _minEdgeColorAlpha))));
  }


  /**
   * Interpolate between the source and destination value given the ratio.
   */
  protected double getInterpolatedDoubleValue(double ratio,
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

} // FaderUIPrefs
