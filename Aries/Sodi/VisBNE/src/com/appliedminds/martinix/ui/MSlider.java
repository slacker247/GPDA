package com.appliedminds.martinix.ui;

import com.appliedminds.martini.Viewport;
import com.appliedminds.martini.MartiniIcon;
import com.appliedminds.martini.Size;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import java.awt.geom.*;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.net.URL;
import java.net.MalformedURLException;


/**
 * <b>MSlider</b> is a customizable slider. The look of the slider is
 * determined by its MSliderResource.
 *
 * @see MSliderResource
 * @author daepark@apmindsf.com
 */
public class MSlider {

  private int _orientation;

  private Viewport _viewport;

  private Point _location;
  private Rectangle _knobRect;
  private Dimension _size;

  private BoundedRangeModel _sliderModel;

  private TrackListener _trackListener;

  private boolean _isDragging = false;

  private MSliderResource _resource;


  /**
   * Create a new slider at the given screen coordinates with the default
   * slider resource.
   *
   * @param orientation the orientation of the slider
   * (MSliderResource.VERTICAL or MSliderResource.HORIZTONAL).
   * @param min the minimum value of the slider.
   * @param max the maximum value of the slider.
   * @param value the default value of the slider such that min &lt;=
   * value &lt;= max.
   * @param vp the Viewport which maps the the world coordinates of
   * this slider object to the device coordinate this slider will be
   * ultimately rendered onto.
   */
  public MSlider(int orientation, int min, int max, int value, Viewport vp)
    throws IOException
  {
    this(orientation, min, max, value, vp, null);
  }


  /**
   * Create a new slider at the given screen coordinates with a custom
   * slider resource.
   *
   * @param orientation the orientation of the slider
   * (MSliderResource.VERTICAL or MSliderResource.HORIZTONAL).
   * @param min the minimum value of the slider.
   * @param max the maximum value of the slider.
   * @param value the default value of the slider such that min &lt;=
   * value &lt;= max.
   * @param vp the Viewport which maps the the world coordinates of
   * this slider object to the device coordinate this slider will be
   * ultimately rendered onto.
   * @param resource the custome slider resource.
   *
   * @see com.appliedminds.martinix.MSliderResource
   */
  public MSlider(int orientation, int min, int max, int value,
                 Viewport vp, MSliderResource resource)
    throws IOException
  {
    if (resource == null) {
      // create default slider resource
      resource = MSliderResource.create(null);
    }
    _resource = resource;

    MSliderResource.checkOrientation(orientation);
    _orientation = orientation;
    _viewport = vp;
    _sliderModel = new DefaultBoundedRangeModel(value, 0, min, max);

    _location = new Point();
    _knobRect = new Rectangle();

    _trackListener = new TrackListener();

    calculateKnobSize(); // this figures out the knob size
    setLocation(0, 0);

  }


  /**
   * Returns the sliders value.
   * @return the models value property
   * @see #setValue
   */
  public int getValue() {
    return (getModel().getValue());
  }


  /**
   * Sets the sliders current value.  This method just forwards
   * the value to the model.
   *
   * @see #getValue
   */
  public void setValue(int n) {
    BoundedRangeModel m = getModel();
    int oldValue = m.getValue();
    if (oldValue == n) {
      return;
    }
    m.setValue(n);
    calcKnobLocation();
  }


  /**
   * Returns the minimum value supported by the slider.
   *
   * @return the value of the models minimum property
   */
  public int getMinimum() {
    return (getModel().getMinimum());
  }


  /**
   * Returns the maximum value supported by the slider.
   *
   * @return the value of the models maximum property
   */
  public int getMaximum() {
    return (getModel().getMaximum());
  }


  /**
   * Return this slider's vertical or horizontal orientation.
   * @return VERTICAL or HORIZONTAL
   * @see #setOrientation
   */
  public int getOrientation() {
    return (_orientation);
  }


  /**
   * Stores the location of the slider in its VIEWPORT coordinates
   * into the "return value" rv and return rv. If rv is null a new
   * Point object is allocated.
   *
   * @param rv the return value, modified to the location of the
   * slider in its VIEWPORT coordinates.
   * @return rv
   */
  public Point getLocation(Point rv) {
    if (rv == null) {
      rv = new Point();
    }
    rv.setLocation(_location.x, _location.y);
    return (rv);
  }


  /**
   * Set the location of the slider in its VIEWPORT coordinates.
   */
  public void setLocation(int x, int y) {
    _location.setLocation(x, y);

    calcKnobLocation();
  }


  /**
   * Get the top-left x coordinate of the slider's location in its
   * VIEWPORT coordiantes.
   */
  public int getX() {
    return(_location.x);
  }


  /**
   * Get the top-left y coordinate of the slider's location in its
   * VIEWPORT coordiantes.
   */
  public int getY() {
    return(_location.y);
  }


  /**
   * @return the width in the slider's VIEWPORT coordinate.
   */
  public int getWidthVP() {
    Dimension d = getSize(null);

    return ((int)_viewport.mapWorldToViewport(d.getWidth()));
  }


  /**
   * @return the height in the slider's VIEWPORT coordinate.
   */
  public int getHeightVP() {
    Dimension d = getSize(null);

    return ((int)_viewport.mapWorldToViewport(d.getHeight()));
  }


  /**
   * Stores the size of the slider in its WORLD coordinates into the
   * "return value" rv and return rv. If rv is null a new Dimension
   * object is allocated.
   *
   * @param rv the return value, modified to the size of the slider in
   * its WORLD coordinates.
   * @return rv
   */
  public Dimension getSize(Dimension rv) {
    if (rv == null) {
      rv = new Dimension();
    }

    if (_size == null) {
      // All the slider track images should be the same so just get the
      // dimension of the first track image.
      _size = new Dimension();
      MartiniIcon icon = _resource.getTrackImage(getOrientation(), 0);
      _size.setSize(icon.getWidth(), icon.getHeight());
    }

    rv.setSize(_size.width, _size.height);

    return (rv);
  }


  /**
   * With respect to its position in VIEWPORT coordinates, calculate
   * the value for the given y coordinate. This method only makes
   * sense for VERTICAL sliders.
   */
  public int valueForYPosition(int yPos) {
    int value;
    int minValue = getMinimum();
    int maxValue = getMaximum();
    int trackLength = getHeightVP() - _knobRect.height;
    Point p = getLocation(null);
    int trackTop = p.y + _knobRect.height / 2;
    int trackBottom = trackTop + (trackLength - 1);

    if (yPos <= trackTop) {
      value = maxValue;
    }
    else if (yPos >= trackBottom) {
      value = minValue;
    }
    else {
      int distanceFromTrackTop = yPos - trackTop;
      double valueRange = (double)maxValue - (double)minValue;
      double valuePerPixel = valueRange / (double)trackLength;
      int valueFromTrackTop = (int)Math.round( distanceFromTrackTop * valuePerPixel );
      value = maxValue - valueFromTrackTop;
    }

    return (value);
  }


  /**
   * With respect to its position in VIEWPORT coordinates, calculate
   * the value for the given x coordinate. This method only makes
   * sense for HORIZONTAL sliders.
   */
  public int valueForXPosition(int xPos) {
    int value;
    int minValue = getMinimum();
    int maxValue = getMaximum();
    int trackLength = getWidthVP() - _knobRect.width;
    Point p = getLocation(null);
    int trackLeft = p.x + _knobRect.width / 2;
    int trackRight = trackLeft + (trackLength - 1);
    //       int trackLeft = p.x;
    //       int trackRight = p.x + (trackLength - 1);

    if ( xPos <= trackLeft ) {
      value = minValue;
    }
    else if ( xPos >= trackRight ) {
      value = maxValue;
    }
    else {
      int distanceFromTrackLeft = xPos - trackLeft;
      double valueRange = (double)maxValue - (double)minValue;
      double valuePerPixel = valueRange / (double)trackLength;
      int valueFromTrackLeft = (int)Math.round( distanceFromTrackLeft * valuePerPixel );

      value = minValue + valueFromTrackLeft;
    }

    return (value);
  }


  /**
   * Get the shape of the knob that can be used for hit testing.
   */
  public GeneralPath getKnobPath() {
    return (new GeneralPath(_knobRect));
  }


  /**
   * After determining a mouse press hit tested successfully with this
   * slider's knob, one should call this method in order to delegate
   * the knob drag mouse events to this slider.
   *
   * @param p The original mouse press location on the slider knob.
   * @see #knobMouseEvent
   * @see #stopKnobDrag
   */
  public void startKnobDrag(Point p) {
    _trackListener.__currentMouseX = p.x;
    _trackListener.__currentMouseY = p.y;

    // Clicked in the Thumb area?
    if (_knobRect.contains(_trackListener.__currentMouseX,
                           _trackListener.__currentMouseY))
      {
        switch (getOrientation()) {
        case MSliderResource.VERTICAL:
          _trackListener.__offset = _trackListener.__currentMouseY - _knobRect.y;
          break;
        case MSliderResource.HORIZONTAL:
          _trackListener.__offset = _trackListener.__currentMouseX - _knobRect.x;
          break;
        }
        _isDragging = true;
        return;
      }

    _isDragging = false;
  }


  /**
   * After determining a mouse event hit tested successfully with this
   * slider knob, one can delegate the mouse event to this method.
   */
  public void knobMouseEvent(MouseEvent e) {
    // delegate mouse event to TrackListener
    if (e.getID() == MouseEvent.MOUSE_DRAGGED) {
      _trackListener.mouseDragged(e);
    }
  }


  /**
   * After a mouse release event on the slider knob, one should call this
   * method to stop the knob drag.
   */
  public void stopKnobDrag() {
    _isDragging = false;
  }


  /**
   * Draw this slider on the specified graphics context.
   *
   * <p>This will first draw the slide track which may be determined
   * by the slider's value. Then the knob.
   */
  public DrawnObject[] draw(Graphics2D g2d, Viewport vp) {

    //
    // Draw the slider track image
    //
    MartiniIcon trackImage = _resource.getTrackImage(getOrientation(),
                                                     getMinimum(),
                                                     getMaximum(),
                                                     getValue());

    double h = trackImage.getHeight();
    double vh = vp.mapWorldToViewport(h);
    double zfactor = vh / h;

    double factor = trackImage.getScaleFactor(h) * zfactor;

    DrawnImage sliderTrack = drawImage(g2d,
                                       trackImage.getImage(),
                                       getLocation(null),
                                       factor);
    //
    // Draw the slider knob
    //
    MartiniIcon knob = _resource.getKnobImage(getOrientation(), _isDragging);
    DrawnImage sliderKnob = drawImage(g2d,
                                      knob.getImage(),
                                      getKnobLocation(null),
                                      factor);

    //
    // collect all drawn objects in order.
    //
    DrawnObject[] retVal = new DrawnObject[2];
    retVal[0] = sliderTrack;
    retVal[1] = sliderKnob;

    return (retVal);
  }


  /**
   * Given the slider value, calculate the y position (of the slider
   * knob).  This method is only valid for VERTICAL sliders.
   */
  protected int yPositionForValue(int value) {
    int min = getMinimum();
    int max = getMaximum();
    int trackLength = getHeightVP() - _knobRect.height;

    double valueRange = (double)max - (double)min;
    double pixelsPerValue = (double)trackLength / (double)valueRange;
    Point p = getLocation(null);
    int trackTop = p.y + _knobRect.height / 2;
    int trackBottom = trackTop + (trackLength - 1);

    int yPosition = trackTop;
    yPosition += Math.round(pixelsPerValue * ((double)max - value ));

    yPosition = Math.max(trackTop, yPosition);
    yPosition = Math.min(trackBottom, yPosition);

    return (yPosition);
  }


  /**
   * Given the slider value, calculate the x position (of the slider
   * knob).  This method is only valid for HORIZONTAL sliders.
   */
  protected int xPositionForValue( int value )    {
    int min = getMinimum();
    int max = getMaximum();
    int trackLength = getWidthVP() - _knobRect.width;
    double valueRange = (double)max - (double)min;
    double pixelsPerValue = (double)trackLength / valueRange;
    Point p = getLocation(null);
    int trackLeft = p.x + _knobRect.width / 2;
    int trackRight = trackLeft + (trackLength - 1);

    int xPosition = trackLeft;;
    xPosition += Math.round(pixelsPerValue * ((double)value - min));

    xPosition = Math.max(trackLeft, xPosition);
    xPosition = Math.min(trackRight, xPosition);

    return (xPosition);
  }


  /**
   * Get the current location of the slider knob in VIEWPORT coordinates.
   */
  protected Point getKnobLocation(Point p) {
    if (p == null) {
      p = new Point();
    }
    p.setLocation(_knobRect.x, _knobRect.y);
    return (p);
  }


  // Used exclusively by setKnobLocation()
  private static Rectangle unionRect = new Rectangle();


  /**
   * Slide the knob to the given location in VIEWPORT coordinates.
   */
  protected void setKnobLocation(int x, int y)  {
    unionRect.setBounds(_knobRect);

    _knobRect.setLocation(x, y);

    SwingUtilities.computeUnion(_knobRect.x, _knobRect.y, _knobRect.width, _knobRect.height, unionRect);
  }


  /** calculate the knob size */
  private void calculateKnobSize() {
    Dimension size = getKnobSize();
    _knobRect.setSize(size.width, size.height);
  }


  /** get the knob size */
  private Dimension getKnobSize() {
    MartiniIcon knob = _resource.getKnobImage(getOrientation());
    return (new Dimension((int)_viewport.mapWorldToViewport(knob.getWidth()),
                          (int)_viewport.mapWorldToViewport(knob.getHeight())));

  }


  /**
   * Returns data model that handles the sliders three
   * fundamental properties: minimum, maximum, value.
   *
   * @see #setModel
   */
  private BoundedRangeModel getModel() {
    return (_sliderModel);
  }


  /**
   * calculate the knob location that coincides with the slider's value
   */
  private void calcKnobLocation() {
    if (MSliderResource.VERTICAL == getOrientation()) {
      int valuePosition = yPositionForValue(getValue());
      _knobRect.x = getX();
      _knobRect.y = valuePosition - (_knobRect.height / 2);

    }
    else {
      int valuePosition = xPositionForValue(getValue());

      _knobRect.x = valuePosition - (_knobRect.width / 2);
      _knobRect.y = getY();
    }
  }


  /**
   * The object responsible for positioning the slider knob and
   * calculating its value due to mouse drags on the slider knob.
   */
  class TrackListener extends MouseInputAdapter {
    protected int __offset;
    protected int __currentMouseX;
    protected int __currentMouseY;

    /**
     * Set the models value to the position of the top/left
     * of the thumb relative to the origin of the track.
     */
    public void mouseDragged(MouseEvent e) {
      int knobMiddle = 0;
      int trackLength = 0;
      Point p = getLocation(null);

      __currentMouseX = e.getX();
      __currentMouseY = e.getY();

      if (!_isDragging) {
        return;
      }

      switch (getOrientation()) {
      case MSliderResource.VERTICAL:
        int halfKnobHeight = _knobRect.height / 2;
        int knobTop = e.getY() - __offset;

        trackLength = getHeightVP() - _knobRect.height;
        int trackTop = p.y + _knobRect.height / 2;
        int trackBottom = trackTop + (trackLength - 1);
        int vMax = yPositionForValue(getMaximum());

        trackTop = vMax;

        knobTop = Math.max(knobTop, trackTop - halfKnobHeight);
        knobTop = Math.min(knobTop, trackBottom - halfKnobHeight);

        setKnobLocation(_knobRect.x, knobTop);

        knobMiddle = knobTop + halfKnobHeight;
        setValue(valueForYPosition(knobMiddle));
        break;
      case MSliderResource.HORIZONTAL:
        int halfKnobWidth = _knobRect.width / 2;
        int knobLeft = e.getX() - __offset;

        trackLength = getWidthVP() - _knobRect.width;
        int trackLeft = p.x + _knobRect.width / 2;
        int trackRight = trackLeft + (trackLength - 1);

        int hMax = xPositionForValue(getMaximum());

        trackRight = hMax;

        knobLeft = Math.max(knobLeft, trackLeft - halfKnobWidth);
        knobLeft = Math.min(knobLeft, trackRight - halfKnobWidth);

        setKnobLocation(knobLeft, _knobRect.y);

        knobMiddle = knobLeft + halfKnobWidth;
        setValue(valueForXPosition(knobMiddle));
        break;
      default:
        return;
      }
    }

  } // end class "TrackListener"


  /**
   * Utility method to draw an image.
   */
  private static DrawnImage drawImage(Graphics2D g2d,
                                      BufferedImage img,
                                      Point2D pt,
                                      double scale)
  {
    Rectangle2D bounds = new Rectangle2D.Double(pt.getX(),
                                                pt.getY(),
                                                (img.getWidth() * scale),
                                                (img.getHeight() * scale));

    ImageScreenData data = new ImageScreenData(img, bounds);
    DrawnImage drawnImage = new DrawnImage(data, pt, scale);
    drawnImage.redraw(g2d);

    return (drawnImage);
  }


} // end class "MSlider"


