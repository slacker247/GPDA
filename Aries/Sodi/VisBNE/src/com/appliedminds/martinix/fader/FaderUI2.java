package com.appliedminds.martinix.fader;

import com.appliedminds.martini.*;
import com.appliedminds.martinix.ui.*;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.GradientPaint;
import java.awt.Graphics2D;
import java.awt.Paint;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.Composite;
import java.awt.AlphaComposite;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.font.FontRenderContext;
import java.awt.font.LineBreakMeasurer;
import java.awt.font.TextLayout;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.awt.geom.Ellipse2D;
import java.awt.geom.GeneralPath;
import java.awt.geom.Line2D;
import java.awt.geom.PathIterator;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.awt.geom.RoundRectangle2D;
import java.awt.geom.QuadCurve2D;
import java.awt.image.BufferedImage;
import java.awt.image.ConvolveOp;
import java.io.IOException;
import java.net.URL;
import java.text.AttributedCharacterIterator;
import java.text.AttributedString;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.text.DecimalFormat;
import javax.swing.SwingConstants;


/**
 * FaderUI2.java
 *
 * <p>This GraphUI generates events for the following UI decoration
 * identifiers:
 *
 * <ul>
 * <li>NODE (graph element = DrawableNode, context = DrawableNode)
 * <li>NODE_TEXT (graph element = DrawableNode,
 *                context = martinix.ui.TextScreenData)
 * <li>EDGE (graph element = DrawableEdge, context = DrawableEdge)
 * <li>SLIDER_KNOB (graph element = DrawableNode, context = MSlider)
 * </ul>
 *
 * Created: Mon Aug 26 11:07:38 2002
 *
 * @author <a href="mailto: daepark@apmindsf.com"</a>
 * @version
 */
public class FaderUI2 implements GraphUI {

  public static final String DECORATION_NODE            = "NODE";
  public static final String DECORATION_NODE_TEXT       = "NODE_TEXT";
  public static final String DECORATION_NODE_ICON       = "NODE_ICON";
  public static final String DECORATION_EDGE            = "EDGE";
  public static final String DECORATION_EDGE_ICON       = "EDGE_ICON";
  public static final String DECORATION_SLIDER_KNOB     = "SLIDER_KNOB";
  public static final String DECORATION_EDGE_BUBBLE     = "EDGE_BUBBLE";
  public static final String DECORATION_SLIDER_VALUE    = "SLIDER_VAL";
  public static final String DECORATION_SLIDER_TOGGLE   = "SLIDER_TOGGLE";
  public static final String DECORATION_EDGE_HUB        = "EDGE_HUB";

  private static final double SLIDER_TOGGLE_OFFSET = 5.0;
  private static final double SLIDER_OFFSET = 25.0;
  private static final double SLIDER_VALUE_OFFSET = 6.0;
  private static final double CLEAR_NODE_RECT_PADDING = 15.0;

  private static final int EDIT_NONE = 0;
  private static final int EDIT_SLIDER = 2;

  protected static EdgeConnectorHub[] _edgeConnectorHub =
    new EdgeConnectorHub[] {
      EdgeConnectorHub.NORTH,
      EdgeConnectorHub.WEST,
      EdgeConnectorHub.SOUTH,
      EdgeConnectorHub.EAST
    };

  private SliderValueFormat _edgeSliderValueFormat =
    new EdgeSliderValueFormat();
  private SliderValueFormat _nodeSliderValueFormat =
    new NodeSliderValueFormat();

  private GraphPanel _graphPanel;
  private HashMap _drawnObjects;
  private FaderUIPrefs2 _prefs;
  private FontCache _defaultFontCache;
  private IconLoader _iconLoader;

  private MSlider      _currentSlider;
  private DrawableGraphElement _currentSliderElement;

  private MSliderResource _currentSliderResource;

  private NodeAttachStrategy _attachStrategy = new DefaultNodeAttachStrategy();

  //
  // This will be set to one of the EDIT_nnn constants defined above.
  //
  private int _editMode;


  // temporary buffer and graphics
  private BufferedImage _tmpBuf = new BufferedImage(10, 10,
                                                    BufferedImage.TYPE_INT_RGB);
  private Graphics2D _tmpGraphics = (Graphics2D) _tmpBuf.getGraphics();


  /**
   * Initialized with a preferences object.
   *
   * @param prefs a FaderUIPrefs
   * @throws RuntimeException if it cannot load node icon type map.
   */
  public FaderUI2(FaderUIPrefs2 prefs) {
    _drawnObjects = new HashMap();
    _prefs = prefs;

    _editMode = EDIT_NONE;

    _defaultFontCache = new FontCache(_prefs.getFontResourcePath());

    try {
      _iconLoader = IconLoader.create(_prefs.getTypeMapURL());
      if (_prefs.getSliders()) {
        _currentSliderResource = MSliderResource.create(null);
      }
    }
    catch (IOException e) {
      throw (new RuntimeException("FaderUI failed to init. Error = " + e));
    }
  }


  //////////////////////////
  // begin GraphUI interface

  public void setGraphPanel(GraphPanel graphPanel) {
    _graphPanel = graphPanel;
  }


  public Size getMinimumSize(Graphics2D g, DrawableNode node, double scale) {
    return (getPreferredSize(g, node, scale));
  }


  public Size getMaximumSize(Graphics2D g, DrawableNode node, double scale) {
    return  (getPreferredSize(g, node, scale));
  }


  public Size getPreferredSize(Graphics2D g, DrawableNode node, double scale) {
    return (calcTotalNodeWorldSize(g, node,
                                   FaderUtil.getNodeLabel(node), scale));
  }


  public void draw(Graphics2D graphics,
                   DrawableEdge edge,
                   Viewport viewport,
                   HitTesterMap hit,
                   DrawableGraphContext ctx,
                   boolean newBuffer,
                   boolean erase)
  {
    //
    // only draw the edge if it needs to be repainted
    // or has changed.
    //
    if (ctx.getNeedsRepaint(edge) || edge.hasChanged()) {

      if (!newBuffer && erase) {
        eraseEdge(graphics, edge, viewport, hit, ctx);
      }

      drawEdge(graphics, edge, viewport, hit, ctx);
    }
  }


  public void draw(Graphics2D graphics,
                   DrawableNode node,
                   Viewport viewport,
                   HitTesterMap hit,
                   DrawableGraphContext ctx,
                   boolean newBuffer,
                   boolean erase)
  {
    //
    // only draw the node if it needs to be repainted
    // or has changed.
    //
    if (ctx.getNeedsRepaint(node) || node.hasChanged()) {
      if (!newBuffer && erase) {
        //
        // if the node has changed or we need to repaint,
        // erase drawn node on the existing buffer
        //
        eraseNode(graphics, node, viewport, hit, ctx);
      }

      drawNode(graphics, node, viewport, hit, ctx);
    }
  }


  /**
   * @param g the graph to validate.
   * @return true if the graph is valid for the FaderUI.
   */
  public boolean validate(DrawableGraph g)
  {
    return (true);
  }


  public boolean setupEditSession(DrawableGraphElement el,
                                  Point loc,
                                  Object ctx)
  {
    if (ctx instanceof MSlider) {
      _currentSlider = (MSlider) ctx;
      _currentSliderElement = (DrawableGraphElement)el;

      _currentSlider.startKnobDrag(loc);
      setEditMode(EDIT_SLIDER);

      // accept the edit mode request.
      return(true); // BAIL
    }

    // deny the edit mode request
    return (false);
  }


  public byte keyPressed(KeyEvent ke) {
    if (!isEditModeSet()) {
      throw(new MartiniError("Call to keyPressed but no edit session in place!"));
    }

    byte flags = 0;

    return(flags);
  }


  public byte mouseEvent(MouseEvent e) {
    if (!isEditModeSet()) {
      throw(new MartiniError("Call to mouseEvent but no edit session in place!"));
    }

    byte flags = 0;

    /* Slider move */
    if (isEditModeSlider()) {

      _currentSlider.knobMouseEvent(e);
      FaderUtil.setSliderValue(_currentSliderElement,
                               _currentSlider.getValue());

      flags |= FLAG_CONTINUE_EDIT;
      flags |= FLAG_NEEDS_REPAINT;
    }

    return (flags);
  }


  public void teardownEditSession() {
    if (_currentSlider != null) {
      _currentSlider.stopKnobDrag();
      _currentSlider = null;
    }
    _currentSliderElement = null;
    setEditMode(EDIT_NONE);
  }

  // end GraphUI interface
  //////////////////////////


  /**
   * Helper method to drawEdge - determines if the given node has been made
   * invisible by a collapsed ancestor
   *
   * @param node the target node
   * @return true if the node is invisible under a collapsed node
   */
  private boolean nodeIsCollapsedInvisible(DrawableNode node) {
    String value = node.getProperty(DrawableNode.PROPERTY_COLLAPSED);
    if (value == null) 
      return false;
    else
      return (value.equals(DrawableNode.COLLAPSED_INVISIBLE));
  }


  /**
   * Draw a curved or straight line with a the edge label
   * horizontally drawn close to the center of the line.
   */
  private void drawEdge(Graphics2D graphics,
                        DrawableEdge edge,
                        Viewport viewport,
                        HitTesterMap hit,
                        DrawableGraphContext ctx)
  {
    // keep track of a list of DrawnObjects for this graph element
    List drawnList = getDrawnObjectsForGraphElement(edge);
    drawnList.clear();

    hit.removeAllHitTesters(edge);

    DrawableNode headNode = edge.getHead();
    DrawableNode tailNode = edge.getTail();

    if (!edge.isVisible() || 
        nodeIsCollapsedInvisible(tailNode) ||
        nodeIsCollapsedInvisible(headNode))
    {
      return; // Don't draw invisible edges
    }
    boolean isFauxEdge = (edge.getProperty(DrawableEdge.PROPERTY_FAUX) != null);
        
    DrawProps edgeDrawProps = getEdgeViewportDrawProps(edge, viewport);
    boolean curvedEdge = _prefs.getCurvedLines();

    // this is just the pill that contains the text
    Rectangle2D headNodeWorldBounds =
      getNodeWorldBounds(headNode, ctx, graphics, viewport);
    Rectangle2D tailNodeWorldBounds =
      getNodeWorldBounds(tailNode, ctx, graphics, viewport);

    // Find closest canonical connection point, N, E, S, or W, etc.:
    NodeAttachStrategy.AttachPoints attachPoints =
      _attachStrategy.findAttachPoints(tailNodeWorldBounds,
                                       headNodeWorldBounds);
    Point2D headAttachPointVP =
      viewport.mapWorldToViewport(attachPoints.targetPt);
    Point2D tailAttachPointVP =
      viewport.mapWorldToViewport(attachPoints.sourcePt);

    // the edge elements
    ArrowHead arrow = null;
    DrawnShape thickToThinDrawnShape = null;
    DrawnShape arrowHeadDrawnShape = null;
    DrawnShape bubbleDrawnShape = null;

    // reused variables
    DrawnShape drawnShape = null;
    Paint fillPaint = null;
    Stroke stroke = null;
    Paint strokePaint = null;

    // Calculate the center where the label or the edge bubble goes
    Point2D labelCenter =
      new Point2D.Double(tailAttachPointVP.getX() +
                         (headAttachPointVP.getX() -
                          tailAttachPointVP.getX()) / 2.0,
                         tailAttachPointVP.getY() +
                         (headAttachPointVP.getY() -
                          tailAttachPointVP.getY()) / 2.0);

    GeneralPath thickToThin = null;   // our thick to thin line path

    // figure out 2 base points of our thick to thin line base
    Point2D[] basePoints = new Point2D[2];

    // use the stroke width of the edge draw props as a reference
    // for the thickness of the edge base
    double baseWidth = edgeDrawProps.getStrokeWidth();

    switch(attachPoints.sourceAttachment) {
    case NodeAttachStrategy.NORTH:
    case NodeAttachStrategy.SOUTH:
      //
      basePoints[0] = new Point2D.Double(tailAttachPointVP.getX() -
                                         baseWidth / 2.0,
                                         tailAttachPointVP.getY());
      basePoints[1] = new Point2D.Double(tailAttachPointVP.getX() +
                                         baseWidth / 2.0,
                                         tailAttachPointVP.getY());
      break;
    default:
      basePoints[0] = new Point2D.Double(tailAttachPointVP.getX(),
                                         tailAttachPointVP.getY() -
                                         baseWidth / 2.0);
      basePoints[1] = new Point2D.Double(tailAttachPointVP.getX(),
                                         tailAttachPointVP.getY() +
                                         baseWidth / 2.0);
    }

    if (curvedEdge) {
      //
      // curved edge
      //

      // this is the key for the thick to thin curved lines
      Point2D[] controlPts = new Point2D[2];
      // figure out 2 control points for the 2 base points
      controlPts[0] = figureControlPoint(headAttachPointVP,
                                         basePoints[0]);
      controlPts[1] = figureControlPoint(headAttachPointVP,
                                         basePoints[1]);
      // create a shape from the 2 curved lines
      thickToThin = new GeneralPath();
      thickToThin.moveTo((float)basePoints[0].getX(),
                         (float)basePoints[0].getY());
      thickToThin.lineTo((float)basePoints[1].getX(),
                         (float)basePoints[1].getY());
      thickToThin.quadTo((float)controlPts[1].getX(),
                         (float)controlPts[1].getY(),
                         (float)headAttachPointVP.getX(),
                         (float)headAttachPointVP.getY());
      thickToThin.quadTo((float)controlPts[0].getX(),
                         (float)controlPts[0].getY(),
                         (float)basePoints[0].getX(),
                         (float)basePoints[0].getY());

      //
      // Calculate arrow points for the curved edge
      //
      Point2D controlPt = null;   // midpoint between the two control points
      controlPt = new Point2D.Double
        (
         controlPts[0].getX() + (controlPts[1].getX() -
                                 controlPts[0].getX()) / 2.0,
         controlPts[0].getY() + (controlPts[1].getY() -
                                 controlPts[0].getY()) / 2.0
         );
      QuadCurve2D curve = new QuadCurve2D.Double();
      curve.setCurve(tailAttachPointVP, controlPt, headAttachPointVP);

      Point2D arrowPtVP = null;
      Point2D arrowBottomPtVP = null;
      double arrowHeadOffsetVP =
        viewport.mapWorldToViewport(_prefs.getArrowHeadOffset());
      double arrowHeadLengthVP =
        viewport.mapWorldToViewport(_prefs.getArrowHeadLength());
      GeneralPath generalPath = new GeneralPath(curve);

      arrowPtVP = Intersect2D.getArcIntersectPoint(generalPath,
                                                 headAttachPointVP,
                                                 arrowHeadOffsetVP);
      if (arrowPtVP == null) {
        arrowPtVP = headAttachPointVP;
      }

      arrowBottomPtVP = Intersect2D.getArcIntersectPoint(generalPath,
                                                       headAttachPointVP,
                                                       arrowHeadOffsetVP +
                                                       arrowHeadLengthVP);

      if (arrowBottomPtVP == null) {
        arrowBottomPtVP = controlPt;
      }

      arrow = new ArrowHead(viewport.mapViewportToWorld(arrowPtVP),
                            viewport.mapViewportToWorld(arrowBottomPtVP),
                            _prefs.getArrowHeadLength(),
                            _prefs.getArrowHeadFlair());



      /*
       * Do some math to get a "pretty good" place for the edge label: find
       * the midpoint of the straight line between our two attach points; 
       * rotate the line by 90 degrees around the midpont; the intersection of 
       * the rotated line with the curve is the midpoint of the curve
       */
      AffineTransform trans = 
        AffineTransform.getRotateInstance(Math.toRadians(90), 
                                          labelCenter.getX(),
                                          labelCenter.getY());
      double[] sourceLinePts = { headAttachPointVP.getX(), 
                                 headAttachPointVP.getY(),
                                 tailAttachPointVP.getX(),
                                 tailAttachPointVP.getY() };
      double[] destLinePts = new double[4];
      trans.transform(sourceLinePts, 0, destLinePts, 0, 2);
      Line2D rotatedLine = new Line2D.Double(destLinePts[0], destLinePts[1],
                                             destLinePts[2], destLinePts[3]);
      Point newLabelCenter = 
        Intersect2D.getLineIntersectPoint(new GeneralPath(curve), rotatedLine);
      if (newLabelCenter == null)
        System.err.println("ERR: unable to calculate label center for edge");
      else
        labelCenter.setLocation(newLabelCenter.x, newLabelCenter.y);

    } // end curved edge

    else {
      //
      // straight edge
      //

      thickToThin = new GeneralPath();
      thickToThin.moveTo((float)basePoints[0].getX(),
                         (float)basePoints[0].getY());
      thickToThin.lineTo((float)basePoints[1].getX(),
                         (float)basePoints[1].getY());
      thickToThin.lineTo((float)headAttachPointVP.getX(),
                         (float)headAttachPointVP.getY());
      thickToThin.lineTo((float)basePoints[0].getX(),
                         (float)basePoints[0].getY());

      //
      // Calculate arrow points for the straight edge
      //
      arrow = new ArrowHead(attachPoints.targetPt,
                            attachPoints.sourcePt,
                            _prefs.getArrowHeadLength(),
                            _prefs.getArrowHeadFlair());
    }

    // Calculate thick to thin line shape
    if (isFauxEdge) {
      stroke = new BasicStroke(2.0f, 
                               BasicStroke.CAP_BUTT,
                               BasicStroke.JOIN_MITER,
                               10.0f,
                               new float[] {4.0f, 4.0f},
                               0.0f);
    }
    else {
      stroke = new BasicStroke(1.0f);
    }
    fillPaint = makePaint(edgeDrawProps.getFillColor(),
                          thickToThin.getBounds());
    strokePaint = makePaint(edgeDrawProps.getStrokeColor(),
                            thickToThin.getBounds());
    thickToThinDrawnShape =
      new DrawnShape(thickToThin, fillPaint, strokePaint, stroke);

    // Calculate arrow head shape
    DrawProps arrowDrawProps = new DrawProps(edgeDrawProps);
    arrowDrawProps.setStrokeWidth(1.0);
    if (arrow != null) {
      arrowHeadDrawnShape = drawEdgeArrow(graphics,
                                          arrow,
                                          viewport,
                                          arrowDrawProps,
                                          false);         // don't draw yet
    } // end straight edge


    // Calculate edge bubble shape
    double bubbleSize = 20.0;
    double h = bubbleSize;
    double w = bubbleSize;
    double vh = viewport.mapWorldToViewport(h);
    double vw = viewport.mapWorldToViewport(w);

    // calculate the position of the bubble
    double bx = labelCenter.getX() - vw * 0.5;
    double by = labelCenter.getY() - vh * 0.5;

    // draw bubble
    Ellipse2D.Double ellipse = new Ellipse2D.Double(bx, by, bubbleSize,
                                                    bubbleSize);
    int sliderVal = FaderUtil.getSliderValue(edge,
                                             _prefs.getDefaultEdgeSliderValue());
    DrawProps bubbleDrawProps =
      _prefs.getUninterpolatedEdgeDrawProps(sliderVal);
    fillPaint =
      makePaint(bubbleDrawProps.getFillColor(), ellipse.getBounds());
    bubbleDrawnShape = new DrawnShape(ellipse, fillPaint, null, null);


    // NOW DRAW ALL THE ELEMENTS

    /////////////////////////////
    // Draw edge halo if selected
    /////////////////////////////
    if (FaderUtil.isSelected(edge)) {
      // draw edge halo for thick to thin line, arrow head and edge bubble
      DrawProps props = getEdgeViewportHaloDrawProps(viewport);
      stroke = new BasicStroke((float)props.getStrokeWidth());
      fillPaint = makePaint(props.getFillColor(), thickToThin.getBounds());
      strokePaint = makePaint(props.getStrokeColor(), thickToThin.getBounds());

      // thick to thin line halo
      drawnShape = new DrawnShape(thickToThinDrawnShape.getDrawnShape(),
                                  fillPaint, strokePaint, stroke);
      drawnShape.redraw(graphics);
      drawnList.add(drawnShape);

      // arrow head halo
      drawnShape = new DrawnShape(arrowHeadDrawnShape.getDrawnShape(),
                                  fillPaint, strokePaint, stroke);
      drawnShape.redraw(graphics);
      drawnList.add(drawnShape);

      // edge bubble halo
      drawnShape = new DrawnShape(bubbleDrawnShape.getDrawnShape(),
                                  fillPaint, strokePaint, stroke);
      drawnShape.redraw(graphics);
      drawnList.add(drawnShape);
    }

    // now draw all the elements of the edge

    //////////////////////////
    // Draw thick to thin line
    //////////////////////////
    clear(graphics, thickToThinDrawnShape.getDrawnShape(), drawnList);
    thickToThinDrawnShape.redraw(graphics);
    drawnList.add(thickToThinDrawnShape);
    HitTester tester = new HitTester(thickToThinDrawnShape.getDrawnShape(),
                                     DECORATION_EDGE, edge);
    hit.addHitTester(edge, tester);

    //////////////////
    // Draw arrow head
    //////////////////
    clear(graphics, arrowHeadDrawnShape.getDrawnShape(), drawnList);
    arrowHeadDrawnShape.redraw(graphics);
    drawnList.add(arrowHeadDrawnShape);
    // For the purposes of hit testing, an edge arrow is as good as an
    // edge.
    tester = new HitTester(arrowHeadDrawnShape.getDrawnShape(),
                           DECORATION_EDGE, edge);
    hit.addHitTester(edge, tester);

    ///////////////////
    // Draw edge bubble
    ///////////////////
    if (!isFauxEdge) {
      bubbleDrawnShape.redraw(graphics);
      drawnList.add(bubbleDrawnShape);
      hit.addHitTester(edge, new HitTester(bubbleDrawnShape.getDrawnShape(),
                                           DECORATION_EDGE_BUBBLE,
                                           edge));
    }

    ///////////////////
    // Draw bubble text
    ///////////////////
    if (!isFauxEdge) {
      String text =
        String.valueOf(FaderUtil.getSliderValue(edge,
                                                _prefs.getDefaultEdgeSliderValue()));
      drawnList.add(drawText(graphics,
                             12,
                             bubbleDrawProps.getFontColor(),
                             (int)bubbleSize,
                             text,
                             new Point2D.Double(bx + bubbleSize/2,
                                                by + bubbleSize/2),
                             false,
                             true));
    }

    ////////////////////
    // Draw edge sliders
    ////////////////////
    if (FaderUtil.isSliderVisible(edge) && !isFauxEdge) {
      MSlider slider = drawEdgeSlider(graphics, edge, viewport, labelCenter,
                                      hit, ctx, drawnList);

      if (slider != null)
        {
          DrawnText sliderValue = drawSliderValue(graphics, viewport, slider,
                                                  SwingConstants.EAST,
                                                  _edgeSliderValueFormat);
          TextScreenData tdata = sliderValue.getDrawnTextScreenData();
          HitTester tester2 = new HitTester(tdata.getBounds(),
                                            DECORATION_SLIDER_VALUE,
                                            tdata.getBounds());
          hit.addHitTester(edge, tester2);
          drawnList.add(sliderValue);
        }
    }
  }


  /**
   * Draw a node with multi-line node label with the node type icons
   * located on the bottom-right.
   */
  private void drawNode(Graphics2D graphics,
                        DrawableNode node,
                        Viewport viewport,
                        HitTesterMap hit,
                        DrawableGraphContext ctx)
  {
    // keep track of a list of DrawnObjects for this graph element
    List drawnList = getDrawnObjectsForGraphElement(node);
    drawnList.clear();

    /*
     * Find out if the current node is in a mouse-over state - this is
     * currently set at the hit tester level, not at the DrawableGraphElement
     * level, although the HMV app does not actually use this level of
     * granularity (i.e. we only care if the mouse is somewhere over the node,
     * and don't concern ourselves with which part of the node). We are about
     * to clear the current hit tester list and create a new one, and we would
     * like to preserve the mouse-over state information. Ideally, we would
     * find out which specific hit tester has the mouse-over state set to true,
     * then set that state in the new hit tester generated for the corresponding
     * node element; for now, we'll just see if any of the hit testers has the
     * state set to true, and, if so, we'll set the state on the first new
     * hit tester we make. This is not an optimal solution, but given the
     * current requirements of the application, it will work.
     */
    boolean mouseIsOverNode = false;
    for (Iterator i = hit.listHitTesters(node); i.hasNext(); ) {
      HitTester ht = (HitTester)i.next();
      if (ht.isOver()) {
        mouseIsOverNode = true;
        break;
      }
    }

    hit.removeAllHitTesters(node);

    String collapsedValue = node.getProperty(DrawableNode.PROPERTY_COLLAPSED);
    if (!node.isVisible() ||
        (collapsedValue != null && 
         collapsedValue.equals(DrawableNode.COLLAPSED_INVISIBLE)))
    {
      return; // Don't need to draw invisible nodes
    }

    // node bounds must be the bounds of the main rectangle enclosing
    // just the node text
    Rectangle2D nodeBoundsW =
      getNodeWorldBounds(node, ctx, graphics, viewport);
    nodeBoundsW.setRect(nodeBoundsW.getX() + _prefs.getNodeOutlinePadding(),
                        nodeBoundsW.getY() - _prefs.getNodeOutlinePadding(),
                        nodeBoundsW.getWidth() - _prefs.getNodeOutlinePadding() * 2,
                        nodeBoundsW.getHeight() - _prefs.getNodeOutlinePadding() * 2);



    Rectangle2D nodeBoundsVP =
      viewport.mapWorldToViewport(nodeBoundsW);

    Rectangle2D totalBoundsW = ctx.getBounds(node);
    Rectangle2D totalBoundsVP = viewport.mapWorldToViewport(totalBoundsW);

    // reused variables
    DrawProps drawProps = null;
    DrawnShape drawnShape = null;
    HitTester ht = null;
    Paint fillPaint = null;
    Paint strokePaint = null;
    Rectangle2D rect = null;
    RoundRectangle2D roundRect = null;
    RoundRectangle2D nodeOutline = null;
    Shape shape = null;
    Stroke stroke = null;
    double arcRadius = 0.0;
    double d = 0.0;
    double padding = 0.0;
    double r = 0.0;
    Ellipse2D ellipse = null;

    String text = FaderUtil.getNodeLabel(node);
    boolean maskHalo = false;

    if (FaderUtil.isSelected(node)) {
      ////////////////////////////////////
      // Draw halo background, if selected
      ////////////////////////////////////
      drawProps = _prefs.getNodeHaloDrawProps(false);
      padding = viewport.mapWorldToViewport(_prefs.getNodeHaloPadding());
      arcRadius = 2 * viewport.mapWorldToViewport(_prefs.getNodeHaloRadius());
      roundRect =
        new RoundRectangle2D.Double(nodeBoundsVP.getX() - padding,
                                    nodeBoundsVP.getY() - padding,
                                    nodeBoundsVP.getWidth() + 2 * padding,
                                    nodeBoundsVP.getHeight() + 2 * padding,
                                    arcRadius,
                                    arcRadius);
      fillPaint = makePaint(drawProps.getFillColor(), null);
      strokePaint = makePaint(drawProps.getStrokeColor(), null);
      stroke = new BasicStroke((float)drawProps.getStrokeWidth());
      drawnShape = new DrawnShape(roundRect, fillPaint, strokePaint, stroke);
      drawnShape.redraw(graphics);
      drawnList.add(drawnShape);

      maskHalo = true;
    }

    if (FaderUtil.isCreatingEdge(node)) {
      ///////////////////////////////////////////////////////////////////
      // Draw creating edge halo, if one of the connector hub is selected
      ///////////////////////////////////////////////////////////////////
      drawProps = _prefs.getNodeHaloDrawProps(true);
      padding = viewport.mapWorldToViewport(_prefs.getNodeHaloPadding() + 1.0);
      arcRadius = 2 * viewport.mapWorldToViewport(_prefs.getNodeHaloRadius());

      roundRect =
        new RoundRectangle2D.Double(nodeBoundsVP.getX() - padding,
                                    nodeBoundsVP.getY() - padding,
                                    nodeBoundsVP.getWidth() + 2 * padding,
                                    nodeBoundsVP.getHeight() + 2 * padding,
                                    arcRadius,
                                    arcRadius);

      fillPaint = makePaint(drawProps.getFillColor(), roundRect.getBounds());
      strokePaint = makePaint(drawProps.getStrokeColor(),
                               roundRect.getBounds());
      stroke = new BasicStroke((float)drawProps.getStrokeWidth());

      drawnShape = new DrawnShape(roundRect, fillPaint, strokePaint, stroke);
      drawnShape.redraw(graphics);
      drawnList.add(drawnShape);

      maskHalo = true;
    }

    ////////////////////
    // Draw node outline
    ////////////////////
    drawProps = getNodeOutlineViewportDrawProps(node, viewport);
    padding = viewport.mapWorldToViewport(_prefs.getNodeOutlinePadding());
    arcRadius = 2 * viewport.mapWorldToViewport(_prefs.getNodeOutlineRadius());
    nodeOutline =
        new RoundRectangle2D.Double(nodeBoundsVP.getX() - padding,
                                    nodeBoundsVP.getY() - padding,
                                    nodeBoundsVP.getWidth() + 2 * padding,
                                    nodeBoundsVP.getHeight() + 2 * padding,
                                    arcRadius,
                                    arcRadius);
    fillPaint = makePaint(drawProps.getFillColor(), null);
    strokePaint = makePaint(drawProps.getStrokeColor(), null);
    stroke = new BasicStroke((float)drawProps.getStrokeWidth());

    if (maskHalo) {
      // Draw the background mask so that you cannot see the node halo
      // through transparent node outline.
      drawnShape = new DrawnShape(nodeOutline, _prefs.getBackgroundColor().getColor(), null, null);
      drawnShape.redraw(graphics);
      drawnList.add(drawnShape);
    }

    drawnShape = new DrawnShape(nodeOutline, fillPaint, strokePaint, stroke);
    drawnShape.redraw(graphics);

    // the slider toggle region is as good as the node outline
    d = viewport.mapWorldToViewport(_prefs.getSliderToggleDiameter());
    padding = viewport.mapWorldToViewport(SLIDER_TOGGLE_OFFSET);

    rect = new Rectangle2D.Double(nodeOutline.getX(),
                                  nodeOutline.getY(),
                                  nodeOutline.getWidth() + padding + d,
                                  nodeOutline.getHeight());
    ht = new HitTester(rect, DECORATION_NODE, node);
    hit.addHitTester(node, ht);
    // use this hit tester to preserve mouse-over state (see comment above
    // the declaration of mouseIsOverNode)
    if (mouseIsOverNode)
      ht.setIsOver(true);
    drawnList.add(drawnShape);

    if (FaderUtil.isSliderToggleVisible(node)) {
      /////////////////////
      // Draw slider toggle
      /////////////////////

      // draw the background circle
      // roundRect is the node outline
      ellipse = new Ellipse2D.Double(nodeOutline.getMaxX() + padding,
                                     nodeOutline.getMaxY() - d, d, d);
      drawProps = _prefs.getSliderToggleCircleDrawProps(false, false);
      fillPaint = makePaint(drawProps.getFillColor(), ellipse.getBounds());
      strokePaint = makePaint(drawProps.getStrokeColor(), ellipse.getBounds());
      stroke = new BasicStroke((float)drawProps.getStrokeWidth());
      drawnShape =
        new DrawnShape(ellipse, fillPaint, strokePaint, stroke);
      drawnShape.redraw(graphics);
      drawnList.add(drawnShape);
      ht = new HitTester(ellipse, DECORATION_SLIDER_TOGGLE, node);
      hit.addHitTester(node, ht);
      drawnList.add(drawnShape);

      // draw triangle inside the circle
      Point2D[] pts = new Point2D[] {
        new Point2D.Double(ellipse.getMaxX() - 4.0, ellipse.getCenterY()),
        new Point2D.Double(ellipse.getCenterX(), ellipse.getMinY() + 4.0),
        new Point2D.Double(ellipse.getCenterX(), ellipse.getMaxY() - 4.0)
      };

      drawProps = _prefs.getSliderToggleArrowDrawProps(false, false);
      drawnShape = drawPolygon(graphics, drawProps, pts);
      drawnList.add(drawnShape);
    }

    if (FaderUtil.isConnectorHubsVisible(node)) {
      ///////////////////////////
      // Draw edge connector hubs
      ///////////////////////////
      d = viewport.mapWorldToViewport(_prefs.getConnectorHubDiameter());
      r = d / 2.0;

      // roundRect is the node outline
      Point2D[] nwsePts = new Point2D[] {
        new Point2D.Double(nodeOutline.getCenterX(), nodeOutline.getMinY()),
        new Point2D.Double(nodeOutline.getMinX(), nodeOutline.getCenterY()),
        new Point2D.Double(nodeOutline.getCenterX(), nodeOutline.getMaxY()),
        new Point2D.Double(nodeOutline.getMaxX(), nodeOutline.getCenterY())
      };

      for (int i=0; i<nwsePts.length; i++) {
        ellipse = new Ellipse2D.Double(nwsePts[i].getX() - r,
                                       nwsePts[i].getY() - r, d, d);
        drawProps =
          _prefs.getEdgeConnectorHubDrawProps(FaderUtil.isEdgeConnectorHubSelected(node, _edgeConnectorHub[i].getConnection()), false, false);
        strokePaint = makePaint(drawProps.getStrokeColor(), null);
        stroke = new BasicStroke((float)drawProps.getStrokeWidth());
        fillPaint = makePaint(drawProps.getFillColor(), ellipse.getBounds());
        drawnShape = new DrawnShape(ellipse, fillPaint, strokePaint, stroke);
        drawnShape.redraw(graphics);
        drawnList.add(drawnShape);
        ht = new HitTester(ellipse, DECORATION_NODE,
                           new EdgeConnectorHub(ellipse.getCenterX(),
                                                ellipse.getCenterY(),
                                                _edgeConnectorHub[i].getConnection()));
        hit.addHitTester(node, ht);
      }
    }


    /////////////////////
    // Draw the main node
    /////////////////////
    drawProps = getNodeViewportDrawProps(node, viewport);
    arcRadius = 2 * viewport.mapWorldToViewport(_prefs.getNodeRadius());
    roundRect =
      new RoundRectangle2D.Double(nodeBoundsVP.getX(),
                                  nodeBoundsVP.getY(),
                                  nodeBoundsVP.getWidth(),
                                  nodeBoundsVP.getHeight(),
                                  arcRadius,
                                  arcRadius);
    fillPaint = makePaint(drawProps.getFillColor(), nodeBoundsVP);
    strokePaint = makePaint(drawProps.getStrokeColor(), nodeBoundsVP);
    stroke = new BasicStroke((float)drawProps.getStrokeWidth());
    drawnShape = new DrawnShape(roundRect, fillPaint, strokePaint, stroke);
    drawnShape.redraw(graphics);
    drawnList.add(drawnShape);


    //////////////////////////////////////////////
    // Draw the node text if it's not being edited
    //////////////////////////////////////////////
    if (text != null)
    {
      Point2D nodeCenter = new Point2D.Double(nodeBoundsVP.getCenterX(),
                                              nodeBoundsVP.getCenterY());

      // don't draw in the real buffer if we're editing. use the
      // temporary one to get the bounds of the text for HitTesterMap
      Graphics2D targetGraphics =
        FaderUtil.isInEdit(node) ? _tmpGraphics : graphics;

      DrawnText drawnText = drawText(targetGraphics,
                                     (int)drawProps.getFontSize(),
                                     drawProps.getFontColor(),
                                     drawProps.getMaxLabelWidth(),
                                     text, nodeCenter, true, true);
      TextScreenData tdata = drawnText.getDrawnTextScreenData();
      HitTester tester =
        new HitTester(tdata.getBounds(), DECORATION_NODE_TEXT, tdata);
      hit.addHitTester(node, tester);
      drawnList.add(drawnText);
    }

    if (_prefs.getSliders()) {
      //////////////////
      // Draw the slider
      //////////////////
      MSlider slider = drawNodeSlider(graphics, node, nodeBoundsVP,
                                      viewport, hit, ctx, drawnList);

      if (slider != null) {
        DrawnText sliderValue = drawSliderValue(graphics,
                                                viewport,
                                                slider,
                                                SwingConstants.SOUTH,
                                                _nodeSliderValueFormat);
        TextScreenData tdata = sliderValue.getDrawnTextScreenData();
        HitTester tester = new HitTester(tdata.getBounds(),
                                         DECORATION_SLIDER_VALUE,
                                         tdata.getBounds());
        hit.addHitTester(node, tester);
        drawnList.add(sliderValue);
      }
    }

    /*
     * Draw the "collapsed" icon, if needed
     */
    if (node.isCollapsed()) {
      MartiniIcon collapsedIcon;
      try {
        collapsedIcon = MartiniIcon.load("com/appliedminds/martinix/fader/resources/collapsed_icon.png");
      }
      catch (Exception e) {
        // not much we can do about this, and it's not too critical an error
        System.err.println("Unable to load collapsed icon");
        return;
      }
      Point2D iconLocation = 
        new Point2D.Double(nodeBoundsVP.getX() - collapsedIcon.getWidth() + 2, 
                           nodeBoundsVP.getY() + nodeBoundsVP.getHeight() - 2);
      DrawnImage image = drawImage(graphics, 
                                   collapsedIcon.getImage(), 
                                   iconLocation, 
                                   viewport.getScale());
      drawnList.add(image);

      // make the icon clickable
      Rectangle2D bounds = new Rectangle2D.Double(iconLocation.getX(),
                                                  iconLocation.getY(),
                                                  collapsedIcon.getWidth(),
                                                  collapsedIcon.getHeight());
      HitTester tester = new HitTester(bounds, DECORATION_NODE, bounds);
      hit.addHitTester(node, tester);
    }
  }


  /**
   * @return a valid TextScreenData object or NULL if the text is null.
   *
   * @param multiline if TRUE draw multi-line, otherwise draw
   * single-line text.
   * @param draw if TRUE draw on the graphics context, otherwise don't
   * draw yet.
   */
  private DrawnText drawText(Graphics2D graphics,
                             int fontSize,
                             GraphColor fontColor,
                             int maxLabelWidth,
                             String text,
                             Point2D center,
                             boolean multiLine,
                             boolean draw)
  {
    if (text == null)
      return (null); // BAIL

    Font font = _defaultFontCache.getFontBySize(fontSize);
    FontRenderContext frc = graphics.getFontRenderContext();

    Rectangle2D bounds;
    Rectangle2D drawnBounds;

    Paint fontPaint = null;
    MultiLineTextLayout mlayout = null;
    TextLayout slayout = null;

    if (text.length() > 0)
    {
      if (multiLine) {
        mlayout = new MultiLineTextLayout(maxLabelWidth,
                                          graphics, font, text);

        TextLayout lay = mlayout.getLayout(mlayout.getMaxIndex());

        drawnBounds =
          new Rectangle2D.Double(center.getX() - (mlayout.getWidth() * 0.5),
                                 center.getY() - (mlayout.getHeight() * 0.5),
                                 mlayout.getWidth(),
                                 mlayout.getHeight());

      }
      else {
        slayout = new TextLayout(text, font, frc);
        bounds = slayout.getBounds();
        drawnBounds =
          new Rectangle2D.Double(center.getX() - (bounds.getWidth() * 0.5),
                                 center.getY() - (slayout.getAscent() * 0.5),
                                 bounds.getWidth(),
                                 bounds.getHeight());
      }

      fontPaint = makePaint(fontColor, drawnBounds);
      graphics.setPaint(fontPaint);
    }
    else
    {
      // This degenerate case is hacked for in-place
      // text editing.

      // Java TextLayout object cannot be instantiated
      // with an empty String, so we will draw
      // two spaces and pretend like the caret is in
      // between them. This will give us access to a
      // caret shape of the appropriate size, even
      // whene there is no text to be drawn.
      if (multiLine) {
        mlayout = new MultiLineTextLayout(maxLabelWidth,
                                          graphics, font, " ");

        // We don't actually draw anything.
        // Zero-width
        drawnBounds =
          new Rectangle2D.Double(center.getX(),
                                 center.getY() - (mlayout.getHeight() * 0.5),
                                 0,
                                 mlayout.getHeight());
      }
      else {
        slayout = new TextLayout("  ", font, frc);
        bounds = slayout.getBounds();

        // We don't actually draw anything.

        // Zero-width
        drawnBounds =
          new Rectangle2D.Double(center.getX(),
                                 center.getY() - (slayout.getAscent() * 0.5),
                                 0,
                                 bounds.getHeight());
      }
    }

    DrawnText drawnText = null;
    if (multiLine) {
      TextScreenData data = new TextScreenData(mlayout, drawnBounds, text);
      drawnText = new DrawnMultiLineText(data, fontPaint, center, true);
    }
    else {
      TextScreenData data = new TextScreenData(slayout, drawnBounds, text);
      drawnText = new DrawnText(data, fontPaint, center);
    }

    if (draw) {
      drawnText.redraw(graphics);
    }

    return (drawnText);
  }


  private MSlider drawEdgeSlider(Graphics2D graphics,
                                 DrawableEdge edge,
                                 Viewport viewport,
                                 Point2D bubbleCenter,
                                 HitTesterMap hit,
                                 DrawableGraphContext ctx,
                                 List drawnList)
  {
    int value = FaderUtil.getSliderValue(edge,
                                         _prefs.getDefaultEdgeSliderValue());

    MSlider slider =
      (MSlider)ctx.getContextData(edge, "slider");

    // if slider is null or the slider resource has changed, create a
    // new slider
    if (slider == null)
    {
      try {
        slider = new MSlider(MSliderResource.HORIZONTAL,
                             -(_prefs.getEdgeSliderMaximum()),
                             _prefs.getEdgeSliderMaximum(),
                             value,
                             viewport,
                             _currentSliderResource);
        ctx.putContextData(edge, "slider", slider);
      }
      catch (IOException e) {
        throw (new RuntimeException("Could not create an MSlider: " +
                                    e.toString()));
      }
    }
    else {
      slider.setValue(value);
    }

    // If slider is invisible don't do anything else.
    if (!FaderUtil.isSliderVisible(edge)) {
      return (null); // BAIL
    }

    //
    // First figure out where to draw the slider on the screen.  We want
    // it right below the edge label.
    //
    double sx = bubbleCenter.getX() - slider.getWidthVP() * 0.5;
    double sy = bubbleCenter.getY() + viewport.mapWorldToViewport(SLIDER_OFFSET);

    slider.setLocation((int)sx, (int)sy);

    DrawnObject[] drawnObjs = slider.draw(graphics, viewport);

    for (int i=0; i<drawnObjs.length; i++) {
      drawnList.add(drawnObjs[i]);
    }

    hit.addHitTester(edge, new HitTester(slider.getKnobPath(),
                                         DECORATION_SLIDER_KNOB,
                                         slider));
    return (slider);
  }


  private MSlider drawNodeSlider(Graphics2D graphics,
                                 DrawableNode node,
                                 Rectangle2D nodeBounds,
                                 Viewport viewport,
                                 HitTesterMap hit,
                                 DrawableGraphContext ctx,
                                 List drawnList)
  {
    int value = FaderUtil.getSliderValue(node,
                                         _prefs.getDefaultNodeSliderValue());

    MSlider slider =
      (MSlider) ctx.getContextData(node, "slider");

    // if slider is null or the slider resource has changed, create a
    // new slider
    if (slider == null)
    {
      try {
        slider = new MSlider(MSliderResource.VERTICAL,
                             _prefs.getNodeSliderMinimum(),
                             _prefs.getNodeSliderMaximum(),
                             value,
                             viewport,
                             _currentSliderResource);
        ctx.putContextData(node, "slider", slider);
      }
      catch (IOException e) {
        throw (new RuntimeException("Could not create an MSlider: " +
                                    e.toString()));
      }
    }
    else {
      slider.setValue(value);
    }

    // If slider is invisible don't do anything else.
    if (!FaderUtil.isSliderVisible(node)) {
      return (null); // BAIL
    }

    //
    // First figure out where to draw the slider on the screen.  We want
    // it on top of the node and aligned with the right hand side of
    // the node.
    //
    // This code must work together with the code in
    // calcSizeOfNodeAndSlider(): If we modify where we draw the
    // slider then we must also modify the way that the overall bounds
    // is computed.
    //
    Size sliderSize = calcSliderSize(graphics, node, viewport.getScale());

    double sx = nodeBounds.getX() + nodeBounds.getWidth() +
      (viewport.mapWorldToViewport(SLIDER_OFFSET));
    double sy = nodeBounds.getY() -
      ((sliderSize.getHeight() - nodeBounds.getHeight()) * 0.5);

    slider.setLocation((int)sx, (int)sy);

    DrawnObject[] drawnObjs = slider.draw(graphics, viewport);

    for (int i=0; i<drawnObjs.length; i++) {
      drawnList.add(drawnObjs[i]);
    }

    hit.addHitTester(node, new HitTester(slider.getKnobPath(),
                                         DECORATION_SLIDER_KNOB,
                                         slider));
    return (slider);
  }


  private void clear(Graphics2D graphics, Shape shape, List drawnList) {
    Paint fillPaint = _prefs.getBackgroundColor().getColor();
    DrawnShape drawnShape = new DrawnShape(shape, fillPaint, null, null);
    drawnShape.redraw(graphics);
    if (drawnList != null) {
      drawnList.add(drawnShape);
    }
  }


  /**
   * @param position can be SwingConstants.SOUTH (default) or EAST.
   */
  private DrawnText drawSliderValue(Graphics2D graphics,
                                    Viewport viewport,
                                    MSlider slider,
                                    int position,
                                    SliderValueFormat format)
  {
    String text = format.format(slider.getValue());

    int fontSize =
      (int)viewport.mapWorldToViewport(_prefs.getSliderValueFontSize());
    Font font =
      _defaultFontCache.getFontBySize(fontSize);
    FontMetrics fm = graphics.getFontMetrics(font);
    int textHeight = fm.getHeight();
    int textWidth = fm.stringWidth(text);

    double centerX = -1;
    double centerY = -1;

    switch (position)
    {
    case SwingConstants.EAST:
      centerX = slider.getX() + slider.getWidthVP() +
        viewport.mapWorldToViewport(SLIDER_VALUE_OFFSET) + textWidth * 0.5;
      centerY = slider.getY() + textHeight * 0.5;
      break;

    default:
      centerX = slider.getX() + textWidth * 0.5;
      centerY = slider.getY() + slider.getHeightVP() +
        viewport.mapWorldToViewport(SLIDER_VALUE_OFFSET) + textHeight * 0.5;
    }

    DrawnText drawnText = drawText(graphics, fontSize,
                                   _prefs.getSliderValueFontColor(), -1,
                                   text, new Point2D.Double(centerX, centerY),
                                   false, true);

    return drawnText;
  }


  /**
   * Draw straight lines through the specified points
   */
  private DrawnShape drawPolyline(Graphics2D graphics,
                                  DrawProps drawProps,
                                  Point2D points[])
  {
    Rectangle2D bounds = new Rectangle2D.Double();
    GeneralPath path = FaderUtil.makePath(points, bounds, false);

    Stroke stroke = new BasicStroke((float)drawProps.getStrokeWidth(),
                                    BasicStroke.CAP_ROUND,
                                    BasicStroke.JOIN_ROUND);
    Paint strokePaint = makePaint(drawProps.getStrokeColor(), bounds);

    DrawnShape drawnShape = new DrawnShape(path, null, strokePaint, stroke);
    drawnShape.redraw(graphics);

    return (drawnShape);
  }


  private DrawnShape drawEdgeArrow(Graphics2D graphics,
                                   ArrowHead arrow,
                                   Viewport viewport,
                                   DrawProps drawProps,
                                   boolean draw)
  {
    Point2D points[] = new Point2D[arrow.getPointCount()];
    for (int i=0; i<arrow.getPointCount(); i++) {
      points[i] = viewport.mapWorldToViewport(arrow.getPoint(i));
    }

    return (drawPolygon(graphics,drawProps, points, draw));
  }


  private DrawnShape drawPolygon(Graphics2D graphics,
                                 DrawProps drawProps,
                                 Point2D points[])
  {
    return (drawPolygon(graphics, drawProps, points, true));
  }

  private DrawnShape drawPolygon(Graphics2D graphics,
                                 DrawProps drawProps,
                                 Point2D points[],
                                 boolean draw)
  {
    Rectangle2D bounds = new Rectangle2D.Double();
    GeneralPath path = FaderUtil.makePath(points, bounds, true);

    Paint fillPaint = makePaint(drawProps.getFillColor(), bounds);
    Stroke stroke = new BasicStroke((float)drawProps.getStrokeWidth(),
                                    BasicStroke.CAP_ROUND,
                                    BasicStroke.JOIN_ROUND);
    Paint strokePaint = makePaint(drawProps.getStrokeColor(), bounds);

    DrawnShape drawnShape = new DrawnShape(path, fillPaint,
                                           strokePaint, stroke);
    if (draw) {
      drawnShape.redraw(graphics);
    }

    return (drawnShape);
  }


  public DrawnImage drawImage(Graphics2D graphics,
                              BufferedImage img, Point2D pt, double scale)
  {
    Rectangle2D bounds = new Rectangle2D.Double(pt.getX(),
                                                pt.getY(),
                                                (img.getWidth() * scale),
                                                (img.getHeight() * scale));

    ImageScreenData data = new ImageScreenData(img, bounds);
    DrawnImage drawnImage = new DrawnImage(data, pt, scale);
    drawnImage.redraw(graphics);

    return (drawnImage);
  }


  /**
   * Erase the drawn element in the given graphics context. Currently,
   * this method simply clears the bounds of of the graph element and
   * redraws all intersecting elements.
   */
  private void eraseEdge(Graphics2D graphics,
                         DrawableEdge edge,
                         Viewport viewport,
                         HitTesterMap hit,
                         DrawableGraphContext ctx)
  {
    DrawableNode headNode = edge.getHead();
    DrawableNode tailNode = edge.getTail();

    if (nodeIsCollapsedInvisible(headNode)) {
      DrawableNode collapsed = headNode.getCollapsedAncestor();
      if (collapsed == null)
        System.err.println("ERR: unable to find collapsed ancestor for node " + 
                           FaderUtil.getNodeLabel(headNode));
      else 
        headNode = collapsed;
    }

    //
    // calculate rectangle to clear
    //
    Rectangle2D headNodeViewportBounds =
      calcNodeClearRect(headNode, ctx, viewport);

    Rectangle2D tailNodeViewportBounds =
      calcNodeClearRect(tailNode, ctx, viewport);

    // create the union of the two rectangles...
    headNodeViewportBounds.add(tailNodeViewportBounds);

    wipeAndRedrawAffected(graphics,
                          headNodeViewportBounds,
                          edge,
                          viewport,
                          hit,
                          ctx);
  }


  /**
   * Erase the drawn element in the given graphics context. Currently,
   * this method simply clears the bounds of of the graph element and
   * redraws all intersecting elements.
   */
  private void eraseNode(Graphics2D graphics,
                         DrawableNode node,
                         Viewport viewport,
                         HitTesterMap hit,
                         DrawableGraphContext ctx)
  {
    Rectangle2D nodeBounds = calcNodeClearRect(node, ctx, viewport);
    wipeAndRedrawAffected(graphics, nodeBounds, node, viewport, hit, ctx);
  }


  /*
   * Wipe out the given rectangular area, check to see what
   * was erased, and then draw those things that got erased back.
   *
   * Do not draw the element specified by the "me" parameter.
   */
  private void wipeAndRedrawAffected(Graphics2D graphics,
                                     Rectangle2D clearRect,
                                     DrawableGraphElement me,
                                     Viewport viewport,
                                     HitTesterMap hit,
                                     DrawableGraphContext ctx)
  {
    boolean isNode;

    if (me instanceof DrawableNode) {
      isNode = true;
    }
    else {
      isNode = false;
    }

    graphics.clearRect((int)clearRect.getX(), (int)clearRect.getY(),
                       (int)clearRect.getWidth(), (int)clearRect.getHeight());

    DrawableGraph graph = me.getGraph();
    Set intersectingElements = graph.getIntersectingElements(hit, clearRect);

    Set nodes = new HashSet();
    Set edges = new HashSet();

    for(Iterator itr = intersectingElements.iterator(); itr.hasNext(); ) {
      Object element = itr.next();
      if (element != me) {
        if (element instanceof DrawableNode) {
          nodes.add(element);
        }
        else if (element instanceof DrawableEdge) {
          edges.add(element);
        }
      }
    }

    //
    // Make sure that we don't draw anything that did not get erased
    // by setting the clip area.
    //
    // If we do not do this then we end up drawing on top of already
    // drawn pixels and this messes up transparent things like the
    // slider.
    //
    graphics.setClip(clearRect);

    for(Iterator itr = edges.iterator(); itr.hasNext(); ) {
      DrawableEdge edge = (DrawableEdge) itr.next();

      if (! (ctx.getNeedsRepaint(edge) || edge.hasChanged())) {
        List drawnList = (List)_drawnObjects.get(edge);
        for(Iterator itr2 = drawnList.iterator(); itr2.hasNext(); ) {
          ((DrawnObject)itr2.next()).redraw(graphics);
        }
      }
    }

    for(Iterator itr = nodes.iterator(); itr.hasNext(); ) {
      DrawableNode node = (DrawableNode) itr.next();
      if (! (ctx.getNeedsRepaint(node) || node.hasChanged())) {
        List drawnList = (List)_drawnObjects.get(node);
        for(Iterator itr2 = drawnList.iterator(); itr2.hasNext(); ) {
          ((DrawnObject)itr2.next()).redraw(graphics);
        }
      }
    }

    // Reset the clip area.
    graphics.setClip(null);
  }


  /**
   * This is the size that includes the node and all its decorations
   * including the slider
   */
  private Size calcTotalNodeWorldSize(Graphics2D graphics,
                                      DrawableNode node,
                                      String nodeLabel,
                                      double scale)
  {
    NodeSize nodeOnly =
      calcNodeWorldSize(graphics, node, nodeLabel, scale);

    Size sliderSize = calcSliderSize(graphics, node, scale);

    Size totalSize = null;
    if (_prefs.getSliders()) {
      totalSize = calcSizeOfNodeAndSlider(nodeOnly.getWidth(),
                                          nodeOnly.getHeight(),
                                          sliderSize.getWidth(),
                                          sliderSize.getHeight(),
                                          scale);
    }
    else {
      totalSize = nodeOnly;
    }

    return (totalSize);
  }


  /*
   * Given the sizes of the node pill shape and the slider, figure
   * out the overall size (bounds) of a "node" object.
   *
   * @param nw the node width
   * @param nh the node height
   * @param sw the slider width
   * @param sh the slider height
   * @param scale the current scale factor
   */
  private Size calcSizeOfNodeAndSlider(double nw,
                                       double nh,
                                       double sw,
                                       double sh,
                                       double scale)
  {
    //
    // The slider is drawn just to the right of the node.  And will
    // usually be taller than the node.
    //

    double h = nh;

    if (sh > nh) {
      h = sh;
    }

    double w = nw + (SLIDER_OFFSET * scale) + sw;

    return(new Size(w, h));
  }

  /**
   * Calculate just the size of the box big enough to contain the
   * multiline text and the node outline.
   */
  private NodeSize calcNodeWorldSize(Graphics2D graphics,
                                     DrawableNode node,
                                     String nodeLabel,
                                     double scale)
  {
    // First calculate the width required to hold the text:
    DrawProps props = _prefs.getNodeDrawProps(false, false);
    Font font =
      _defaultFontCache.getFontBySize((int)(props.getFontSize() * scale));

    MultiLineTextLayout layout =
      new MultiLineTextLayout((int)(_prefs.getMaxLabelWidth()*scale),
                              graphics, font, nodeLabel);

    double nodeWidth = (_prefs.getMaxLabelWidth() * scale) +
      (scale * _prefs.getLabelHorizontalMargin());
    double nodeHeight =
      layout.getHeight() + (scale * _prefs.getLabelVerticalMargin());

    // Perform a map from viewport --> world, and return in world
    // coordinates.
    NodeSize sz = new NodeSize((nodeWidth / scale),
                               (nodeHeight / scale),
                               layout);

    sz.setSize(sz.getWidth() + _prefs.getNodeOutlinePadding() * 2,
               sz.getHeight() + _prefs.getNodeOutlinePadding() * 2);

    return (sz);
  }


  /**
   * Calculate the bounds that completely encloses the node and all
   * its decorations including the icons.
   */
  private Rectangle2D calcNodeClearRect(DrawableNode n,
                                        DrawableGraphContext ctx,
                                        Viewport viewport)
  {
    Rectangle2D nodeBounds = calcDrawnNodeViewportBounds(n, ctx, viewport);


    double padding = viewport.mapWorldToViewport(10.0);

    // add the manual backround
    double offset = viewport.mapWorldToViewport(CLEAR_NODE_RECT_PADDING);
    nodeBounds.setRect(nodeBounds.getX() - offset - padding,
                       nodeBounds.getY() - padding ,
                       nodeBounds.getWidth() + offset + 2 * padding,
                       nodeBounds.getHeight() + 2 * padding);

    return(nodeBounds);
  }


  /*
   * Return a rectangle that includes the node plus any of its decorations
   */
  private Rectangle2D calcDrawnNodeViewportBounds(DrawableNode node,
                                                  DrawableGraphContext ctx,
                                                  Viewport viewport)
  {

    //
    // This value has already been calculated and is cached in the
    // context.
    //
    // see getNodeWorldBounds()
    //
    return(viewport.mapWorldToViewport(ctx.getBounds(node)));
  }


  /*
   * Compute the bounds of the node and store it in the context.
   *
   * This method stores two values in the context:
   *
   *  "bounds"   -> The world bounds of the node.
   *
   *  "nodeSize" -> The width and height of the node in world units (PLUS
   *                the TextLayout object... ie, NodeSize is a composite
   *                object).
   *
   */
  private Rectangle2D getNodeWorldBounds(DrawableNode n,
                                         DrawableGraphContext ctx,
                                         Graphics2D graphics,
                                         Viewport viewport)
  {
    // totalBounds = the bounds of the rectangle PLUS the bounds of
    // the icon (if any)
    Rectangle2D totalBounds = ctx.getBounds(n);

    NodeSize pillSize = calcNodeWorldSize(graphics,
                                          n,
                                          FaderUtil.getNodeLabel(n),
                                          viewport.getScale());

    // The x coordinate is the same as the one set by the layout.
    double x = totalBounds.getX();

    // Just put the pill Y coord in the middle of the totalBounds.
    //
    // Note that I have to subtract from the Y value since we are in
    // world coordinates.
    double y = totalBounds.getY() -
      ((totalBounds.getHeight() - pillSize.getHeight()) * 0.5);

    Rectangle2D bounds = new Rectangle2D.Double(x,
                                                y,
                                                pillSize.getWidth(),
                                                pillSize.getHeight());

    ctx.putContextData(n, "bounds", bounds);
    ctx.putContextData(n, "nodeSize", pillSize);

    return (bounds);
  }

  private DrawProps getNodeOutlineViewportDrawProps(DrawableNode node,
                                                    Viewport viewport)
  {
    int weight = FaderUtil.getNodeWeight(node,
                                         _prefs.getDefaultEdgeSliderValue());

    return (mapWorldToViewport
            (_prefs.getNodeOutlineDrawProps
             (FaderUtil.getSliderValue(node,
                                       _prefs.getDefaultNodeSliderValue()),
              weight,
              false,
              false),
             viewport.getScale()));
  }


  /*
   * Get the node DrawProps, adjusted for the view transform.
   */
  private DrawProps getNodeViewportDrawProps(DrawableNode node,
                                             Viewport viewport)
  {
    return (mapWorldToViewport (_prefs.getNodeDrawProps(false, false),
                                viewport.getScale()));
  }


  /**
   * Get the DrawProps for an edge.
   *
   * @param edge the edge
   */
  private DrawProps getEdgeViewportDrawProps(DrawableEdge edge,
                                             Viewport viewport)
  {
    DrawProps result;
    if (edge.getProperty(DrawableEdge.PROPERTY_FAUX) != null)
      result = _prefs.getEdgeDrawProps(0, 50); // simulate a "neutral" edge
    else {
      result = _prefs.getEdgeDrawProps
        (FaderUtil.getSliderValue(edge,
                                  _prefs.getDefaultEdgeSliderValue()),
         FaderUtil.getSliderValue(edge.getTail(),
                                  _prefs.getDefaultNodeSliderValue()));
    }

    // Transform size attributes
    result.setStrokeWidth(viewport.mapWorldToViewport(result.getStrokeWidth()));
    result.setFontSize(viewport.mapWorldToViewport(result.getFontSize()));
    result.setMaxLabelWidth((int)viewport.mapWorldToViewport(result.getMaxLabelWidth()));

    return (result);
  }


  private DrawProps getEdgeViewportHaloDrawProps(Viewport viewport)
  {
    DrawProps result = _prefs.getEdgeHaloDrawProps();

    // Transform size attributes
    result.setStrokeWidth(viewport.mapWorldToViewport(result.getStrokeWidth()));
    result.setFontSize(viewport.mapWorldToViewport(result.getFontSize()));
    result.setMaxLabelWidth((int)viewport.mapWorldToViewport(result.getMaxLabelWidth()));

    return (result);
  }


  /*
   * Create new DrawProps based on the one passed in, except that the
   * new one is mapped to viewport coordinates.
   */
  private DrawProps mapWorldToViewport(DrawProps props, double scale)
  {
    DrawProps result = new DrawProps(props);
    result.setStrokeWidth(result.getStrokeWidth() * scale);
    result.setFontSize(result.getFontSize() * scale);
    result.setMaxLabelWidth((int)(result.getMaxLabelWidth() * scale));

    return (result);
  }


  /**
   * Get the list of DrawnObjects for the given graph element.
   */
  private List getDrawnObjectsForGraphElement(DrawableGraphElement element)
  {
    List drawnList = (List)_drawnObjects.get(element);
    if (drawnList == null)
    {
      drawnList = new ArrayList();
      _drawnObjects.put(element, drawnList);
    }

    return (drawnList);
  }


  // cheesy hack to support color gradients
  private Paint makePaint(GraphColor graphColor, Rectangle2D bounds)
  {
    Paint result = null;

    if (graphColor.isHorizontalGradient())
    {
      Color color1 = graphColor.getColor1();
      Color color2 = graphColor.getColor2();
      result = new GradientPaint(new Point2D.Double(bounds.getX(),
                                                    bounds.getY()),
                                 color1,
                                 new Point2D.Double(bounds.getX() +
                                                    bounds.getWidth(),
                                                    bounds.getY()),
                                 color2);
    }
    else if (graphColor.isVerticalGradient())
    {
      Color color1 = graphColor.getColor1();
      Color color2 = graphColor.getColor2();
      result = new GradientPaint(new Point2D.Double(bounds.getX(),
                                                    bounds.getY()),
                                 color1,
                                 new Point2D.Double(bounds.getX(),
                                                    bounds.getY() +
                                                    bounds.getHeight()),
                                 color2);
    }
    else
    {
      result = graphColor.getColor();
    }

    return (result);
  }


  private void setEditMode(int m) {
    _editMode = m;
  }


  /*
   * @return true if an edit mode is set
   */
  private boolean isEditModeSet() {
    return(_editMode != EDIT_NONE);
  }


  /*
   * @return true if the edit mode is set to slider.
   */
  private boolean isEditModeSlider() {
    return(_editMode == EDIT_SLIDER);
  }


  /*
   * Return a "control point" between two points.  We just choose a point
   * to make the line curve a bit.  We want to make edges that head up the
   * screen bend down like a U, and edges that head down the screen
   * bend up like a ^.
   *
   * @param tail the tail point
   * @param head the head point
   *
   * @return the control point (for the QuadCurve object).
   */
  private Point2D figureControlPoint(Point2D tail, Point2D head) {
    double dx = tail.getX() - head.getX();
    double dy = tail.getY() - head.getY();


    if (dx < 0) {
      return(new Point2D.Double(tail.getX() +
                                (Math.abs(dx) * 0.5), tail.getY()));
    }
    else {
      return(new Point2D.Double(tail.getX() -
                                (Math.abs(dx) * 0.5), tail.getY()));
    }
  }


  /**
   * Calculate the size of the slider along with the slider value
   * text.
   */
  private Size calcSliderSize(Graphics2D g, DrawableNode node, double scale) {
    Size s = new Size();

    if (_currentSliderResource != null) {
      MartiniIcon track =
        _currentSliderResource.getTrackImage(MSliderResource.VERTICAL, 0);
      MartiniIcon knob =
        _currentSliderResource.getKnobImage(MSliderResource.VERTICAL);
      s.setSize(scale * Math.max(track.getWidth(), knob.getWidth()),
                scale * track.getHeight());

      // Accomodate for the slider value region which goes directly
      // under the slider.
      String sliderValue = _nodeSliderValueFormat.format(0);
      Font font =
        _defaultFontCache.getFontBySize((int)(_prefs.getSliderValueFontSize() * scale));
      FontMetrics fm = g.getFontMetrics(font);

      int textHeight = fm.getHeight();
      int textWidth = fm.stringWidth(sliderValue);

      //      Rectangle2D textBounds = fm.getStringBounds(sliderValue, g);

      s.setSize(Math.max(s.getWidth(), textWidth),
                Math.max(s.getHeight(), (s.getHeight() + (scale * SLIDER_VALUE_OFFSET) + textHeight)));

    }

    return (s);
  }


  /**
   * Size data PLUS a text layout object.
   */
  public class NodeSize extends Size {

    private MultiLineTextLayout __layout;

    public NodeSize(double width,
                    double height,
                    MultiLineTextLayout layout)
    {
      super(width, height);
      __layout = layout;
    }

    public MultiLineTextLayout getTextLayout() {
      return (__layout);
    }

  }  // end class NodeSize"


  private interface SliderValueFormat {
    public String format(int sliderValue);
  }


  private class NodeSliderValueFormat implements SliderValueFormat{
    DecimalFormat f = new DecimalFormat("0%");

    public String format(int sliderValue) {
      return (f.format(sliderValue * 0.01));
    }
  }

  private class EdgeSliderValueFormat implements SliderValueFormat {

    public String format(int sliderValue) {
      return (String.valueOf(sliderValue));
    }
  }

} // end class "FaderUI"