package com.appliedminds.hmv;

import com.appliedminds.core.config.ConfigBean;
import com.appliedminds.martini.DrawableGraphContext;
import com.appliedminds.martini.GraphPanel;
import com.appliedminds.martinix.fader.FaderUI;
import com.appliedminds.martinix.fader.FaderUIPrefs;

import java.awt.Color;


/**
 * <b>HMVConfigBean</b> is a configuration object used by HMV to adjust the
 * look of a HMV graph.
 *
 * @deprecated <b>Note that this version is designed to work with
 * FaderUI while HMVConfigBean2 works with FaderUI2.</b>
 *
 * @author daepark@apmindsf.com
 */
public class HMVConfigBean implements ConfigBean {

  /*
   * Defaults
   */
  private static Color  _defNodeFadeSourceColor;
  private static Color  _defNodeFadeDestinationColor;

  private static Color  _defNodeStrokeSourceColor;
  private static Color  _defNodeStrokeDestinationColor;

  private static double _defNodeStrokeSourceWidth;
  private static double _defNodeStrokeDestinationWidth;

  private static Color  _defSupportEdgeFadeSourceColor;
  private static Color  _defSupportEdgeFadeDestinationColor;

  private static Color  _defRefuteEdgeFadeSourceColor;
  private static Color  _defRefuteEdgeFadeDestinationColor;

  private static double _defEdgeStrokeSourceWidth;
  private static double _defEdgeStrokeDestinationWidth;

  private static boolean _defCurvedLines;

  private static Color  _defManualBackgroundColor;

  private static boolean _defRealTimeSliderResponse;

  /**
   * Initialize defaults
   */
  static {
    _defNodeFadeSourceColor      = new Color(0xFF, 0xFF, 0xFF);
    _defNodeFadeDestinationColor = new Color(0xE8, 0xE8, 0xE8);

    _defNodeStrokeSourceColor    = new Color(0x26, 0x49, 0x90);
    _defNodeStrokeDestinationColor = new Color(0x26, 0x49, 0x90);

    _defNodeStrokeSourceWidth      = 2.0;
    _defNodeStrokeDestinationWidth = 7.5;

    _defSupportEdgeFadeSourceColor      = new Color(0x8D, 0xA4, 0x81);
    _defSupportEdgeFadeDestinationColor = new Color(0x8D, 0xA4, 0x81);

    _defRefuteEdgeFadeSourceColor      = new Color(0x9C, 0x1C, 0x8C);
    _defRefuteEdgeFadeDestinationColor = new Color(0x9C, 0x1C, 0x8C);

    _defEdgeStrokeSourceWidth      = 2.0;
    _defEdgeStrokeDestinationWidth = 7.5;

    _defCurvedLines = true;

    _defManualBackgroundColor = new Color(0x47, 0x95, 0xAA, 0x20);

    _defRealTimeSliderResponse = true;
  }


  private Color  _nodeFadeSourceColor;
  private Color  _nodeFadeDestinationColor;

  private Color  _nodeStrokeSourceColor;
  private Color  _nodeStrokeDestinationColor;

  private double _nodeStrokeSourceWidth;
  private double _nodeStrokeDestinationWidth;

  private Color  _supportEdgeFadeSourceColor;
  private Color  _supportEdgeFadeDestinationColor;

  private Color  _refuteEdgeFadeSourceColor;
  private Color  _refuteEdgeFadeDestinationColor;

  private double _edgeStrokeSourceWidth;
  private double _edgeStrokeDestinationWidth;

  private boolean _curvedLines;

  private Color  _manualBackgroundColor;

  private boolean _realTimeSliderResponse;

  /**
   * Default constructor.
   */
  public HMVConfigBean() {
    initDefaults();
  }


  /**
   * Copy constructor.
   */
  public HMVConfigBean(HMVConfigBean copy) {
    _nodeFadeSourceColor =              copy._nodeFadeSourceColor;
    _nodeFadeDestinationColor =         copy._nodeFadeDestinationColor;

    _nodeStrokeSourceColor =            copy._nodeStrokeSourceColor;
    _nodeStrokeDestinationColor =       copy._nodeStrokeDestinationColor;

    _nodeStrokeSourceWidth =            copy._nodeStrokeSourceWidth;
    _nodeStrokeDestinationWidth =       copy._nodeStrokeDestinationWidth;

    _supportEdgeFadeSourceColor =       copy._supportEdgeFadeSourceColor;
    _supportEdgeFadeDestinationColor =  copy._supportEdgeFadeDestinationColor;

   _refuteEdgeFadeSourceColor =         copy._refuteEdgeFadeSourceColor;
    _refuteEdgeFadeDestinationColor =   copy._refuteEdgeFadeDestinationColor;

    _edgeStrokeSourceWidth =            copy._edgeStrokeSourceWidth;
    _edgeStrokeDestinationWidth =       copy._edgeStrokeDestinationWidth;

    _curvedLines =                      copy._curvedLines;

    _manualBackgroundColor =            copy._manualBackgroundColor;

    _realTimeSliderResponse     = copy._realTimeSliderResponse;
  }


  /**
   * HMV node color will be interpolated between a source and a
   * destination color depending on the node's probability value.
   */
  public Color getNodeFadeSourceColor() {
    return (_nodeFadeSourceColor);
  }


  /**
   * HMV node color will be interpolated between a source and a
   * destination color depending on the node's probability value.
   */
  public void setNodeFadeSourceColor(Color color) {
    _nodeFadeSourceColor = color;
  }


  /**
   * HMV node color will be interpolated between a source and a
   * destination color depending on the node's probability value.
   */
  public Color getNodeFadeDestinationColor() {
    return (_nodeFadeDestinationColor);
  }


  /**
   * HMV node color will be interpolated between a source and a
   * destination color depending on the node's probability value.
   */
  public void setNodeFadeDestinationColor(Color color) {
    _nodeFadeDestinationColor = color;
  }


  /**
   * HMV node color will be interpolated between a source and a
   * destination color depending on the node's probability value.
   */
  public Color getNodeStrokeSourceColor() {
    return (_nodeStrokeSourceColor);
  }


  /**
   * HMV node color will be interpolated between a source and a
   * destination color depending on the node's probability value.
   */
  public void setNodeStrokeSourceColor(Color color) {
    _nodeStrokeSourceColor = color;
  }


  /**
   * HMV node color will be interpolated between a source and a
   * destination color depending on the node's probability value.
   */
  public Color getNodeStrokeDestinationColor() {
    return (_nodeStrokeDestinationColor);
  }


  /**
   * HMV node color will be interpolated between a source and a
   * destination color depending on the node's probability value.
   */
  public void setNodeStrokeDestinationColor(Color color) {
    _nodeStrokeDestinationColor = color;
  }


  /**
   * HMV node stroke width will be interpolated between a source and a
   * destination value depending on its probability value.
   */
  public double getNodeStrokeSourceWidth() {
    return (_nodeStrokeSourceWidth);
  }


  /**
   * HMV node stroke width will be interpolated between a source and a
   * destination value depending on its probability value.
   */
  public void setNodeStrokeSourceWidth(double value) {
    _nodeStrokeSourceWidth = value;
  }


  /**
   * HMV node stroke width will be interpolated between a source and a
   * destination value depending on its probability value.
   */
  public double getNodeStrokeDestinationWidth() {
    return (_nodeStrokeDestinationWidth);
  }


  /**
   * HMV node stroke width will be interpolated between a source and a
   * destination value depending on its probability value.
   */
  public void setNodeStrokeDestinationWidth(double value) {
    _nodeStrokeDestinationWidth = value;
  }


  /**
   * HMV supportEdge color will be interpolated between a source and a
   * destination color depending on its probability value.
   */
  public Color getSupportEdgeFadeSourceColor() {
    return (_supportEdgeFadeSourceColor);
  }


  /**
   * HMV supportEdge color will be interpolated between a source and a
   * destination color depending on its probability value.
   */
  public void setSupportEdgeFadeSourceColor(Color color) {
    _supportEdgeFadeSourceColor = color;
  }


  /**
   * HMV supportEdge color will be interpolated between a source and a
   * destination color depending on its probability value.
   */
  public Color getSupportEdgeFadeDestinationColor() {
    return (_supportEdgeFadeDestinationColor);
  }


  /**
   * HMV supportEdge color will be interpolated between a source and a
   * destination color depending on its probability value.
   */
  public void setSupportEdgeFadeDestinationColor(Color color) {
    _supportEdgeFadeDestinationColor = color;
  }


  /**
   * HMV refuteEdge color will be interpolated between a source and a
   * destination color depending on its probability value.
   */
  public Color getRefuteEdgeFadeSourceColor() {
    return (_refuteEdgeFadeSourceColor);
  }


  /**
   * HMV refuteEdge color will be interpolated between a source and a
   * destination color depending on its probability value.
   */
  public void setRefuteEdgeFadeSourceColor(Color color) {
    _refuteEdgeFadeSourceColor = color;
  }


  /**
   * HMV refuteEdge color will be interpolated between a source and a
   * destination color depending on its probability value.
   */
  public Color getRefuteEdgeFadeDestinationColor() {
    return (_refuteEdgeFadeDestinationColor);
  }


  /**
   * HMV refuteEdge color will be interpolated between a source and a
   * destination color depending on its probability value.
   */
  public void setRefuteEdgeFadeDestinationColor(Color color) {
    _refuteEdgeFadeDestinationColor = color;
  }


  /**
   * HMV edge width will be interpolated between a source and a
   * destination value depending on its probability value.
   */
  public double getEdgeStrokeSourceWidth() {
    return (_edgeStrokeSourceWidth);
  }


  /**
   * HMV edge width will be interpolated between a source and a
   * destination value depending on its probability value.
   */
  public void setEdgeStrokeSourceWidth(double value) {
    _edgeStrokeSourceWidth = value;
  }


  /**
   * HMV edge width will be interpolated between a source and a
   * destination value depending on its probability value.
   */
  public double getEdgeStrokeDestinationWidth() {
    return (_edgeStrokeDestinationWidth);
  }


  /**
   * HMV edge width will be interpolated between a source and a
   * destination value depending on its probability value.
   */
  public void setEdgeStrokeDestinationWidth(double value) {
    _edgeStrokeDestinationWidth = value;
  }


  /**
   * HMV edge may be curved or straight.
   */
  public boolean getCurvedLines() {
    return (_curvedLines);
  }


  /**
   * HMV edge may be curved or straight.
   */
  public void setCurvedLines(boolean value) {
    _curvedLines = value;
  }


  /**
   * Manual graph elements will have this color as it's background.
   *
   * @see FaderUtil#isManual
   */
  public Color getManualBackgroundColor() {
    return (_manualBackgroundColor);
  }


  /**
   * Manual graph elements will have this color as it's background.
   *
   * @see FaderUtil#isManual
   */
  public void setManualBackgroundColor(Color color) {
    _manualBackgroundColor = color;
  }


  /**
   * HMV sliders can be updated in real time or deferred.
   */
  public boolean getRealTimeSliderResponse() {
    return (_realTimeSliderResponse);
  }


  /**
   * HMV sliders can be updated in real time or deferred.
   */
  public void setRealTimeSliderResponse(boolean value) {
    _realTimeSliderResponse = value;
  }

  /**
  /**
   * Apply configuration settings into the DrawableGraphContext used
   * by the FaderUI.
   */
  public void applyConfiguration(HMV app)
  {
    FaderUIPrefs prefs = app.getFaderUIPrefs();

    // node color
    prefs.setNodeFadeSourceColor(getNodeFadeSourceColor());
    prefs.setNodeFadeDestinationColor(getNodeFadeDestinationColor());

    // node stroke color
    prefs.setNodeStrokeSourceColor(getNodeStrokeSourceColor());
    prefs.setNodeStrokeDestinationColor(getNodeStrokeDestinationColor());

    // node stroke width
    prefs.setNodeStrokeSourceWidth(getNodeStrokeSourceWidth());
    prefs.setNodeStrokeDestinationWidth(getNodeStrokeDestinationWidth());

    // support edge
    prefs.setEdgePositiveFadeSourceColor(getSupportEdgeFadeSourceColor());
    prefs.setEdgePositiveFadeDestinationColor(getSupportEdgeFadeDestinationColor());

    // refute edge
    prefs.setEdgeNegativeFadeSourceColor(getRefuteEdgeFadeSourceColor());
    prefs.setEdgeNegativeFadeDestinationColor(getRefuteEdgeFadeDestinationColor());

    // edge stroke width
    prefs.setEdgeStrokeSourceWidth(getEdgeStrokeSourceWidth());
    prefs.setEdgeStrokeDestinationWidth(getEdgeStrokeDestinationWidth());

    // use curved edges
    prefs.setCurvedLines(getCurvedLines());

    // manual background color
    prefs.setManualBackgroundColor(getManualBackgroundColor());

    // realt time slider mode
    app.setRealTimeSliderMode(getRealTimeSliderResponse());
  }


  /**
   * Init config properties with defaults.
   */
  private void initDefaults() {
    _nodeFadeSourceColor        = _defNodeFadeSourceColor;
    _nodeFadeDestinationColor   = _defNodeFadeDestinationColor;

    _nodeStrokeSourceColor      = _defNodeStrokeSourceColor;
    _nodeStrokeDestinationColor = _defNodeStrokeDestinationColor;

    _nodeStrokeSourceWidth      = _defNodeStrokeSourceWidth;
    _nodeStrokeDestinationWidth = _defNodeStrokeDestinationWidth;

    _supportEdgeFadeSourceColor        = _defSupportEdgeFadeSourceColor;
    _supportEdgeFadeDestinationColor   = _defSupportEdgeFadeDestinationColor;

    _refuteEdgeFadeSourceColor        = _defRefuteEdgeFadeSourceColor;
    _refuteEdgeFadeDestinationColor   = _defRefuteEdgeFadeDestinationColor;

    _edgeStrokeSourceWidth      = _defEdgeStrokeSourceWidth;
    _edgeStrokeDestinationWidth = _defEdgeStrokeDestinationWidth;

    _curvedLines                = _defCurvedLines;

    _manualBackgroundColor      = _defManualBackgroundColor;

    _realTimeSliderResponse     = _defRealTimeSliderResponse;
  }

} // end class "HMVConfigBean"
