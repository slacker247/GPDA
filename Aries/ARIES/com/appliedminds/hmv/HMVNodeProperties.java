package com.appliedminds.hmv;

import com.appliedminds.martini.DrawableNode;
import com.appliedminds.martinix.fader.FaderUtil;


/**
 * A bean for the various HMV node properties.
 *
 * @author daepark@apmindsf.com
 */
public class HMVNodeProperties extends GraphElementProperties {

  public static final int MAX_CERTAINTY_PERCENT = 100;
  public static final int MIN_CERTAINTY_PERCENT = 0;

  private static final String EMPTY_STRING = "";
  private static final String WHITE_SPACE  = " ";

  private String _hypothesis;
  private double _certainty;

  public HMVNodeProperties(DrawableNode node) {
    super(node);

    _hypothesis = FaderUtil.getNodeLabel(node);
    _certainty = (double)(FaderUtil.getSliderValue(node) / (double)MAX_CERTAINTY_PERCENT);
  }


  public String getHypothesis() {
    return (_hypothesis);
  }


  public void setHypothesis(String hypothesis) {
    if (hypothesis != null) {
      _hypothesis = hypothesis.replaceAll("[\n\r\f]", WHITE_SPACE).trim();
    }
    else {
      _hypothesis = EMPTY_STRING;
    }
  }


  public double getCertainty() {
    return (_certainty);
  }


  public void setCertainty(double certainty) {
    _certainty = certainty;
  }


  protected GraphElementProperties replicate() {
    HMVNodeProperties copy =
      new HMVNodeProperties((DrawableNode)getGraphElement());
    copy._hypothesis = this._hypothesis;
    copy._certainty = this._certainty;

    return (copy);
  }


  protected void applyConfiguration() {

    DrawableNode node = (DrawableNode)getGraphElement();

    // apply label change
    FaderUtil.setNodeLabel(node, _hypothesis);

    // apply slider value change
    int oldValue = FaderUtil.getSliderValue(node);
    int newValue = (int)Math.round(_certainty * MAX_CERTAINTY_PERCENT);
    if (newValue > MAX_CERTAINTY_PERCENT) {
      newValue = MAX_CERTAINTY_PERCENT;
    }
    else if (newValue < 0) {
      newValue = MIN_CERTAINTY_PERCENT;
    }

    if (oldValue != newValue) {
      FaderUtil.setManual(node, true);
      FaderUtil.setSliderValue(node, newValue);
    }
  }


} // end class "HMVNodeProperties"
