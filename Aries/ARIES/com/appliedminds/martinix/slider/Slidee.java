package com.appliedminds.martinix.slider;


import com.appliedminds.martini.Viewport;
import com.appliedminds.martinix.ui.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.font.*;
import java.awt.geom.*;
import javax.swing.*;



/**
 * Code for drawing the slider.
 *
 * (Slidee rhymes with yipee).
 *
 *
 * @author mathias@apmindsf.com
 * @author will@apmindsf.com
 */
public class Slidee {

  /*
   *
   * Most of these constants will eventually go in the properties
   * object for this UI.
   *
   *
   */

  // These are in world coordinates
  private static final int X_OFFSET = 18;
  private static final int Y_OFFSET = 8;

  private static final double WIDTH_PADDING = 2.0;
  private static final double HEIGHT_PADDING = 1.0;

  private static final double TICK_DEPTH = 6.0;
  private static final double SLIDER_RUN_LEN = 50.0;
  private static final double ARROW_LEN = 10.0;
  private static final double ARROW_HEIGHT = 6.0;
  private static final double ARROW_SPACE = 3.0;
  private static final double NUM_SPACE = 3.0;
  private static final double BGCIRCLE_RADIUS = 16.0;

  private static final Color FRAME_COLOR = new Color(60, 60, 60);
  private static final Color ARROW_COLOR = new Color(0, 0, 0);
  private static final Color NUM_COLOR = new Color(0, 0, 0);
  private static final Color BGCIRCLE_COLOR = new Color(255, 255, 255);

  private static final BasicStroke FRAME_STROKE =
    new BasicStroke(1.0f,
                    BasicStroke.CAP_ROUND,
                    BasicStroke.JOIN_MITER,
                    2.0f,
                    new float[]{ 2.0f },
                    1.0f);
  private static final Stroke ARROW_STROKE = new BasicStroke();
  private static final Stroke BGCIRCLE_STROKE = new BasicStroke();
  private static final Stroke NUM_STROKE = new BasicStroke();


  //
  // The FONT_NAME and FONT_OVERHANG are related.
  //
  private static final String FONT_NAME = "helvetica-bold-12";

  // 19 = approx. width of "100" of helvetica-bold-12
  private static final double FONT_OVERHANG = (22.0 - BGCIRCLE_RADIUS);


  private static final int WIDTH = (int) (FONT_OVERHANG +
                                          BGCIRCLE_RADIUS +
                                          NUM_SPACE +
                                          FRAME_STROKE.getLineWidth() +
                                          ARROW_SPACE +
                                          ARROW_LEN +
                                          WIDTH_PADDING);

  private static final int HEIGHT = (int) (SLIDER_RUN_LEN +
                                           BGCIRCLE_RADIUS +
                                           HEIGHT_PADDING);

  private static final Dimension SLIDEE_SIZE =
    new Dimension(WIDTH, HEIGHT);



  /**
   * The Slidee has a static size (in WORLD COORDINATE SYSTEM!)
   */
  public static Dimension getSize() {
    return(SLIDEE_SIZE);
  }


  /**
   * Draw a slider.
   *
   * @param g2 the graphics object
   * @param state state data
   * @param gui Gui data (modified).
   * @return a list of DrawnObjects in the order they were drawn.
   */
  public static DrawnObject[] drawSlidee(Graphics2D g2,
                                         Viewport vp,
                                         SlideeState state,
                                         SlideeGUI gui)
  {
    double x = gui.getX() + vp.mapWorldToViewport(X_OFFSET);
    double y = gui.getY() + vp.mapWorldToViewport(Y_OFFSET);

    Composite origComposite = g2.getComposite();
    Paint origPaint = g2.getPaint();
    Stroke origStroke = g2.getStroke();


    //
    // 2. The little slider line
    //
    double tickDepthVP = vp.mapWorldToViewport(TICK_DEPTH);
    double sliderRunLenVP = vp.mapWorldToViewport(SLIDER_RUN_LEN);

    Line2D line = new Line2D.Double(x + tickDepthVP, y,
                                    x + tickDepthVP, y + sliderRunLenVP);
    DrawnObject sliderLine = new
      DrawnShape(line, null, FRAME_COLOR, FRAME_STROKE);
    sliderLine.redraw(g2);

    //
    // 3. The arrow
    //
    double yOffsetVP = (state.getValue() / state.getMax()) * sliderRunLenVP;
    double arrowSpaceVP = vp.mapWorldToViewport(ARROW_SPACE);
    double arrowLenVP = vp.mapWorldToViewport(ARROW_LEN);
    double arrowHeightVP = vp.mapWorldToViewport(ARROW_HEIGHT);

    GeneralPath arrowp = new GeneralPath();
    double dx = x + tickDepthVP + arrowSpaceVP;
    double dy = y + sliderRunLenVP - yOffsetVP;

    arrowp.moveTo((float) dx, (float) dy);
    arrowp.lineTo((float) (dx + arrowLenVP),
                 (float) (dy - (arrowHeightVP / 2.0)));
    arrowp.lineTo((float) (dx + arrowLenVP),
                 (float) (dy + (arrowHeightVP / 2.0)));
    arrowp.lineTo((float) dx, (float) dy);

    gui.setArrowPath(arrowp);
    DrawnObject arrow =
      new DrawnShape(arrowp, ARROW_COLOR, null, ARROW_STROKE);
    arrow.redraw(g2);


    //
    // 4. Background circle
    //
    double numSpaceVP = vp.mapWorldToViewport(NUM_SPACE);
    double bgCircleRadiusVP = vp.mapWorldToViewport(BGCIRCLE_RADIUS);

    dx = x + tickDepthVP - numSpaceVP - bgCircleRadiusVP;
    dy = (y + sliderRunLenVP - yOffsetVP) - (bgCircleRadiusVP / 2.0);

    DrawnObject bgCircle = new DrawnBackgroundCircle(vp, dx, dy);
    bgCircle.redraw(g2);

    //
    // 5. Number
    //
    FontRenderContext ctx = g2.getFontRenderContext();

    String text = Integer.toString((int) state.getValue());
    TextLayout layout = new TextLayout(text, gui.getFont(), ctx);

    Rectangle2D textbounds = layout.getBounds();

    dx = x + tickDepthVP - numSpaceVP - textbounds.getWidth() - 1.0;
    dy = (y + sliderRunLenVP - yOffsetVP) + (textbounds.getHeight() / 2.0);

    DrawnObject number =
      new DrawnText(new TextScreenData(layout, textbounds, text),
                    NUM_COLOR,
                    (float)dx,
                    (float)dy);
    number.redraw(g2);

    g2.setPaint(origPaint);
    g2.setStroke(origStroke);
    g2.setComposite(origComposite);

    //
    // collect all drawn objects in order.
    //
    DrawnObject[] retVal = new DrawnObject[4];
    retVal[0] = sliderLine;
    retVal[1] = arrow;
    retVal[2] = bgCircle;
    retVal[3] = number;

    return (retVal);
  }



  /**
   * Object that holds state data for a single slider.
   */
  public static class SlideeState {
    private double __min, __max;
    private double __val;


    /**
     * Create a new state object.
     *
     * @param min the minimum value
     * @param max the maximum value
     * @param val the initial value.
     */
    public SlideeState(double min, double max, double val) {
      __min = min;
      __max = max;
      setValue(val);
    }

    /**
     * Get the current slider value.
     */
    public double getValue() {
      return(__val);
    }

    /**
     * Set the slider value.
     */
    public void setValue(double v) {
      if (v < __min) {
        __val = __min;
      }
      else if (v > __max) {
        __val = __max;
      }
      else {
        __val = v;
      }
    }

    /**
     * Get the maximum value.
     */
    public double getMax() {
      return(__max);
    }

  }// end class "SlideeState"



  /**
   * An object to hold all the GUI related data for a single slider.
   */
  public static class SlideeGUI {

    /** The font for drawing the number */
    private Font __font;

    /** The size of the slider "widget" (in WORLD coordiante system) */
    private static final Dimension __size = SLIDEE_SIZE;

    /** The line bounds */
    private Rectangle2D __lineBounds;

    private double __x, __y;

    private Viewport __viewport;

    private GeneralPath __arrow;



    /**
     * Create a new slider at the given screen coordinates.
     *
     * @param x the screen x coordinate for the top left of the slider
     * widget.
     * @param y the screen y coordinate for the top left of the slider
     * widget.
     */
    public SlideeGUI(Viewport vp, double x, double y) {
      __viewport = vp;
      __font = Font.decode(FONT_NAME);
      setLocation(x, y);
    }


    public void setArrowPath(GeneralPath p) {
      __arrow = p;
    }


    public Font getFont() {
      return(__font);
    }


    public Rectangle2D getLineBounds() {
      return(__lineBounds);
    }


    public GeneralPath getArrowPath() {
      return(__arrow);
    }


    /**
     * Set the location of the whole slider widget.
     *
     * @param x the top left x coordinate
     * @param y the top left y coordinate
     */
    public void setLocation(double x, double y) {
      double tickDepthVP = __viewport.mapWorldToViewport(TICK_DEPTH);
      double yOffsetVP = __viewport.mapWorldToViewport(Y_OFFSET);
      double sliderRunLenVP = __viewport.mapWorldToViewport(SLIDER_RUN_LEN);

      if (__lineBounds == null) {
        __lineBounds = new Rectangle2D.Double(x + (tickDepthVP/2.0),
                                            y + yOffsetVP,
                                            tickDepthVP,
                                            sliderRunLenVP);
      }
      else {
        __lineBounds.setFrame(x + (tickDepthVP / 2.0),
                            y + yOffsetVP,
                            tickDepthVP,
                            sliderRunLenVP);
      }
      __x = x;
      __y = y;
    }


    /**
     * @return the width in viewport coordinate system.
     */
    public int getWidthVP() {
      return ((int)__viewport.mapWorldToViewport(__size.getWidth()));
    }

    /**
     * @return the height in viewport coordinate system.
     */
    public int getHeightVP() {
      return ((int)__viewport.mapWorldToViewport(__size.getHeight()));
    }

    /**
     * Get the top-left x coordinate.
     */
    public double getX() {
      return(__x);
    }

    /**
     * Get the top left y coordinate.
     */
    public double getY() {
      return(__y);
    }

    /**
     * Get the run length of the slider (the length of the "line").
     */
    public int getRunLength() {
      return((int) __viewport.mapWorldToViewport(SLIDER_RUN_LEN));
    }


    /**
     * Figure out the proper value for the slider given a screen y
     * coordinate value and the max value for the slider.
     *
     * @param the max value (min is always 0.0).
     */
    public double calculateValueFromYCoord(int y, double max)
    {
      double sv = (__lineBounds.getY() + __lineBounds.getHeight()) - y;
      double sliderRunLenVP = __viewport.mapWorldToViewport(SLIDER_RUN_LEN);

      if (sv > sliderRunLenVP) {
        sv = sliderRunLenVP;
      }
      else if (sv < 0.0) {
        sv = 0.0;
      }

      return((sv / sliderRunLenVP) * max);
    }

  }// end class "SlideeGUI"



  /**
   * DrawnObject implementation for the circle containg the slider value
   */
  private static class DrawnBackgroundCircle extends DrawnObject {

    Viewport __viewport;
    double __dx;
    double __dy;

    public DrawnBackgroundCircle(Viewport vp, double dx, double dy) {
      super();
      __viewport = vp;
      __dx = dx;
      __dy = dy;
    }

    public void redraw(Graphics2D g2) {
      super.redraw(g2);

      g2.translate(__dx, __dy);
      g2.setPaint(BGCIRCLE_COLOR);
      g2.setStroke(BGCIRCLE_STROKE);
      double radiusVP = __viewport.mapWorldToViewport(BGCIRCLE_RADIUS);
      g2.fill(new Ellipse2D.Double(0.0, 0.0, radiusVP, radiusVP));
      g2.translate(-__dx, -__dy);
    }

  } // end class DrawnBackgroundCircle

}// end class "Slidee"
