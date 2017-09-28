package com.appliedminds.martinix.slider;


import java.awt.geom.Rectangle2D;


/** 
 * Struct like class to hold the two parts of a Slider together and
 * provide a common interface.
 *
 *
 * @author mathias@apmindsf.com
 * @author will@apmindsf.com
 */
class Slider {

  private Slidee.SlideeState _state;
  private Slidee.SlideeGUI _gui;


  public Slider(Slidee.SlideeState state, Slidee.SlideeGUI gui) {
    _state = state;
    _gui = gui;
  }


  public Slidee.SlideeState getState() {
    return(_state);
  }


  public Slidee.SlideeGUI getGUI() {
    return(_gui);
  }


  public double calculateValueFromYCoord(int y, double max) {
    return(_gui.calculateValueFromYCoord(y, max));
  }


  public double getWidthVP() {
    return(_gui.getWidthVP());
  }


  public double getHeightVP() {
    return(_gui.getHeightVP());
  }


  /**
   * @param x X coordinate in VIEWPORT coordinate system.
   * @param y Y coordinate in VIEWPORT coordinate system.
   */
  public void setLocation(double x, double y) {
    _gui.setLocation(x, y);
  }


  public double getMax() {
    return(getState().getMax());
  }


  public void setValue(double v) {
    getState().setValue(v);
  }


  public double getValue() {
    return(getState().getValue());
  }


  /**
   * @return the bounds in VIEWPORT coordinate system.
   */  
  public Rectangle2D getBounds() {
    return(new Rectangle2D.Double(_gui.getX(), 
                                  _gui.getY(), 
                                  _gui.getWidthVP(),
                                  _gui.getHeightVP()));
  }

  public String toString() {
    return("slider{ " + getBounds() + " }");
  }

} // end class Slider
