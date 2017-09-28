package com.appliedminds.hmv;

import com.appliedminds.core.config.ConfigBean;
import com.appliedminds.martinix.fader.FaderUIPrefs2;

import java.awt.Color;


/**
 * <b>HMVConfigBean2</b> is a configuration object used by HMV to 
 * adjust the look of a HMV graph. This version is designed to
 * work the FaderUI2.
 *
 * @author daepark@apmindsf.com
 * @author will@apmindsf.com
 */
public class HMVConfigBean2 implements ConfigBean {

  /*
   * Defaults
   */
  private static Color   _defNeutralColor;
  private static Color   _defSupportingColor;
  private static Color   _defRefutingColor;
  private static boolean _defCurvedLines;
  //private static boolean _defRealTimeSliderResponse;

  /**
   * Initialize defaults
   */
  static {
    _defNeutralColor    = new Color(0x64, 0x64, 0x64);
    _defSupportingColor = new Color(0x94, 0xAA, 0x00);
    _defRefutingColor   = new Color(0xAC, 0x00, 0x10);
    _defCurvedLines     = true;
    //_defRealTimeSliderResponse = true;
  }

  private Color  _neutralColor;
  private Color  _supportingColor;
  private Color  _refutingColor;
  private boolean _curvedLines;
  //private boolean _realTimeSliderResponse;

  /**
   * Default constructor.
   */
  public HMVConfigBean2() {
    initDefaults();
  }


  /**
   * Copy constructor.
   */
  public HMVConfigBean2(HMVConfigBean2 copy)
  {
    _neutralColor = copy._neutralColor;
    _supportingColor = copy._supportingColor;
    _refutingColor  = copy._refutingColor;
    _curvedLines  = copy._curvedLines;
    //_realTimeSliderResponse = copy._realTimeSliderResponse;
  }


  /**
   * @return color used by a neutral node/edge
   */
  public Color getNeutralColor() {
    return (_neutralColor);
  }

  /**
   * @color what color should neutral elements be
   */
  public void setNeutralColor(Color color) {
    _neutralColor = color;
  }


  /**
   * @return color used by a supporting node/edge
   */
  public Color getSupportingColor() {
    return (_supportingColor);
  }

  /**
   * @color what color should supporting elements be
   */
  public void setSupportingColor(Color color) {
    _supportingColor = color;
  }


  /**
   * @return color used by a refuting node/edge
   */
  public Color getRefutingColor() {
    return (_refutingColor);
  }

  /**
   * @color what color should refuting elements be
   */
  public void setRefutingColor(Color color) {
    _refutingColor = color;
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
   * HMV sliders can be updated in real time or deferred.
   */
  /*
  public boolean getRealTimeSliderResponse() {
    return (_realTimeSliderResponse);
  }
  */


  /**
   * HMV sliders can be updated in real time or deferred.
   */
  /*
  public void setRealTimeSliderResponse(boolean value) {
    _realTimeSliderResponse = value;
  }
  */


  /**
  /**
   * Apply configuration settings into the DrawableGraphContext used
   * by the FaderUI.
   */
  public void applyConfiguration(HMV app)
  {
    FaderUIPrefs2 prefs = app.getFaderUIPrefs();

    // colors
    prefs.setDefaultColor(getNeutralColor());
    prefs.setSupportingColor(getSupportingColor());
    prefs.setRefutingColor(getRefutingColor());

    // use curved edges
    prefs.setCurvedLines(getCurvedLines());

    // realt time slider mode
    //app.setRealTimeSliderMode(getRealTimeSliderResponse());
  }


  /**
   * Init config properties with defaults.
   */
  private void initDefaults()
  {
    _neutralColor    =  _defNeutralColor;
    _supportingColor =  _defSupportingColor;
    _refutingColor   =  _defRefutingColor;
    _curvedLines     =  _defCurvedLines;
    //_realTimeSliderResponse     = _defRealTimeSliderResponse;
  }

} // end class HMVConfigBean2
