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
 * FaderUI.java
 *
 * <p>This GraphUI generates events for the following UI decoration
 * identifiers:
 *
 * <ul>
 * <li>NODE (graph element = DrawableNode, context = DrawableNode)
 * <li>NODE_TEXT (graph element = DrawableNode,
 *                context = martinix.ui.TextScreenData)
 * <li>NODE_ICON (graph element = DrawableNode,
                  context = martinix.fader.FaderIconHitContext)
 * <li>EDGE_ICON (graph element = DrawableEdge,
                  context = martinix.fader.FaderIconHitContext)
 * <li>EDGE (graph element = DrawableEdge, context = DrawableEdge)
 * <li>SLIDER_KNOB (graph element = DrawableNode, context = MSlider)
 * </ul>
 *
 * Created: Mon Aug 26 11:07:38 2002
 *
 * @author <a href="mailto: daepark@apmindsf.com"</a>
 * @version
 */
public class FaderUI implements GraphUI {

  public static final String DECORATION_NODE            = "NODE";
  public static final String DECORATION_NODE_TEXT       = "NODE_TEXT";
  public static final String DECORATION_NODE_ICON       = "NODE_ICON";
  public static final String DECORATION_EDGE            = "EDGE";
  public static final String DECORATION_EDGE_ICON       = "EDGE_ICON";
  public static final String DECORATION_SLIDER_KNOB     = "SLIDER_KNOB";
  public static final String DECORATION_EDGE_BUBBLE     = "EDGE_BUBBLE";
  public static final String DECORATION_SLIDER_VALUE    = "SLIDER_VAL";

  private static final double NODE_ICON_Y_OFFSET = 6.0;
  private static final double SLIDER_OFFSET = 20.0;
  private static final double SLIDER_VALUE_OFFSET = 6.0;
  private static final double MANUAL_BACKGROUND_OFFSET = 15.0;

  private static final int EDIT_NONE = 0;
  private static final int EDIT_SLIDER = 2;

  private SliderValueFormat _edgeSliderValueFormat =
    new EdgeSliderValueFormat();
  private SliderValueFormat _nodeSliderValueFormat =
    new NodeSliderValueFormat();

  private GraphPanel _graphPanel;
  private HashMap _drawnObjects;
  private FaderUIPrefs _prefs;
  private FontCache _defaultFontCache;
  private IconLoader _iconLoader;

  private MSlider      _currentSlider;
  private DrawableGraphElement _currentSliderElement;

  private MSliderResource _currentSliderResource;

  //
  // This will be set to one of the EDIT_nnn constants defined above.
  //
  private int _editMode;

  /**
   * Initialized with a preferences object.
   *
   * @param prefs a FaderUIPrefs
   * @throws RuntimeException if it cannot load node icon type map.
   */
  public FaderUI(FaderUIPrefs prefs) {
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

    if (!edge.isVisible()) {
      return; // Don't draw invisible edges
    }

    DrawProps drawProps = getEdgeViewportDrawProps(edge, viewport);
    boolean curvedEdge = _prefs.getCurvedLines();

    Rectangle2D headNodeWorldBounds =
      getNodeWorldBounds(edge.getHead(), ctx, graphics, viewport);
    Rectangle2D tailNodeWorldBounds =
      getNodeWorldBounds(edge.getTail(), ctx, graphics, viewport);

    // Find closest canonical connection point, N, E, S, or W, etc.:
    EdgeGeometry egWorld =
      new EdgeGeometry(edge,
                       true,
                       headNodeWorldBounds,
                       tailNodeWorldBounds,
                       _prefs.getArrowHeadLength(),
                       _prefs.getArrowHeadFlair(),
                       _prefs.getArrowHeadOffset(),
                       true,
                       _prefs.getBorderEdgeAttachStrategy());

    Point2D headAttachPointW = egWorld.getHeadPoint();
    Point2D tailAttachPointW = egWorld.getTailPoint();

    EdgeGeometry egViewport = egWorld.mapWorldToViewport(_graphPanel);
    Point2D headAttachPointVP = egViewport.getHeadPoint();
    Point2D tailAttachPointVP = egViewport.getTailPoint();

    DrawnShape drawnShape = null;
    Shape path = null;
    Point2D labelCenter = null;
    ArrowHead arrow = null;

    if (curvedEdge) {
      //
      // Draw curved line
      //

      // this is the key for a curved edge
      Point2D controlPt = figureControlPoint(headAttachPointVP,
                                             tailAttachPointVP);
      Point2D controlPtWorld = viewport.mapViewportToWorld(controlPt);

      drawnShape = drawCurvedLine(graphics, drawProps,
                                  tailAttachPointVP,
                                  controlPt,
                                  headAttachPointVP);

      //
      // Calculate arrow points for the curved edge
      //
      Point2D arrowPtVP = null;
      Point2D arrowBottomPtVP = null;
      double arrowHeadOffsetVP =
        viewport.mapWorldToViewport(_prefs.getArrowHeadOffset());
      double arrowHeadLengthVP =
        viewport.mapWorldToViewport(_prefs.getArrowHeadLength());
      GeneralPath generalPath = new GeneralPath(drawnShape.getDrawnShape());

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


      //
      // Do some math to get a "pretty good" place for the edge label.
      //
      labelCenter = egViewport.getCenterPoint();
      double dy = (controlPt.getY() - egViewport.getCenterPoint().getY()) * 0.5;
      labelCenter.setLocation(labelCenter.getX(), labelCenter.getY() + dy);
    }
    else
    {
      //
      // Draw straight line
      //
      drawnShape =
        drawPolyline(graphics, drawProps, egViewport.getEdgePoints());

      labelCenter = egViewport.getCenterPoint();

      //
      // Calculate arrow points for the straight edge
      //
      arrow = new ArrowHead(headAttachPointW,
                            tailAttachPointW,
                            _prefs.getArrowHeadLength(),
                            _prefs.getArrowHeadFlair());
    }

    path = drawnShape.getDrawnShape();
    HitTester tester = new HitTester(path, DECORATION_EDGE, edge);
    hit.addHitTester(edge, tester);
    drawnList.add(drawnShape);

    // Draw the arrow
    //
    // For the purposes of hit testing, an edge arrow is as good as an
    // edge.

    if (arrow != null) {
      drawnShape = drawEdgeArrow(graphics,
                                 arrow,
                                 viewport,
                                 drawProps);
      path = drawnShape.getDrawnShape();
      tester = new HitTester(path, DECORATION_EDGE, edge);
      hit.addHitTester(edge, tester);
      drawnList.add(drawnShape);
    }


    //
    // Can draw either edge bubble (positive or negative) for sliders
    // or text label with edge icons (not both!).
    //
    if (_prefs.getSliders()) {
      //
      // draw edge bubble (positive or negative)
      //
      if (! (labelCenter == null || _iconLoader == null)) {
        MartiniIcon bubble = null;
        if (FaderUtil.isEdgePositive(edge)) {
          bubble = FaderUtil.isSliderVisible(edge) ? _iconLoader.getIconForType("positive_down") : _iconLoader.getIconForType("positive");
        }
        else {
          bubble = FaderUtil.isSliderVisible(edge) ? _iconLoader.getIconForType("negative_down") : _iconLoader.getIconForType("negative");
        }

        if (bubble != null) {
          double h = bubble.getHeight();
          double w = bubble.getWidth();
          double vh = viewport.mapWorldToViewport(h);
          double vw = viewport.mapWorldToViewport(w);
          double zfactor = vh / h;
          double factor = bubble.getScaleFactor(h) * zfactor;

          // calculate the position of the bubble
          double bx = labelCenter.getX() - vw * 0.5;
          double by = labelCenter.getY() - vh * 0.5;

          DrawnImage drawnImage = drawImage(graphics,
                                            bubble.getImage(),
                                            new Point2D.Double(bx, by),
                                            factor);
          drawnList.add(drawnImage);

          Shape newShape = drawnImage.getDrawnImageScreenData().getBounds();
          hit.addHitTester(edge, new HitTester(newShape,
                                               DECORATION_EDGE_BUBBLE,
                                               edge));
        }

        if (FaderUtil.isSliderVisible(edge))
        {
          //
          // draw edge sliders
          //
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
    }
    else
    {
      //
      // Draw edge label
      //

      // Don't bother drawing text with tiny fonts
      if (drawProps.getFontSize() < _prefs.getSmallestVisibleFontSize()) {
        return; // BAIL
      }

      String text = FaderUtil.getEdgeLabel(edge);
      if (! (text == null || labelCenter == null)) {
        // Don't draw text yet
        DrawnText drawnText =
          drawText(graphics, (int)drawProps.getFontSize(),
                   drawProps.getFontColor(), -1, text, labelCenter,
                   false, false);

        // First clear text background
        Rectangle clearRect =
          drawnText.getDrawnTextScreenData().getBounds().getBounds();
        float ascent = drawnText.getDrawnTextScreenData().getTextLayout().getAscent();
        clearRect.setSize(clearRect.width,
                          (int)(clearRect.height + ascent * 0.5));

        drawnShape = new DrawnShape(clearRect, graphics.getBackground(),
                                    null, null);
        drawnShape.redraw(graphics);
        drawnList.add(drawnShape);

        // finally draw the text
        drawnText.redraw(graphics);

        // edge label is just as good as the edge for hit testing
        TextScreenData tdata = drawnText.getDrawnTextScreenData();
        tester = new HitTester(tdata.getBounds(),
                               DECORATION_EDGE, tdata);
        hit.addHitTester(edge, tester);
        drawnList.add(drawnText);

        //
        // draw edge type icons
        //
        drawEdgeIcons(graphics, edge, viewport, clearRect, hit, drawnList);
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

    hit.removeAllHitTesters(node);

    if (!node.isVisible()) {
      return; // Don't need to invisible nodes
    }

    // node bounds must be the bounds of the main rectangle enclosing
    // the node text
    Rectangle2D nodeBoundsW =
      getNodeWorldBounds(node, ctx, graphics, viewport);
    Rectangle2D nodeBoundsVP =
      viewport.mapWorldToViewport(nodeBoundsW);

    Rectangle2D totalBoundsW = ctx.getBounds(node);
    Rectangle2D totalBoundsVP = viewport.mapWorldToViewport(totalBoundsW);

    if (nodeBoundsVP.getHeight() < _prefs.getSmallestVisibleNodeHeight()) {
      return; // Don't bother drawing excessively tiny nodes
    }

    // Get the node DrawProps, adjusted for the view transform
    DrawProps drawProps = getNodeViewportDrawProps(node, viewport);

    String text = FaderUtil.getNodeLabel(node);

    // if manual, draw manual background
    if (FaderUtil.isManual(node)) {
      Paint fillPaint = _prefs.getManualBackgroundColor();

      double offset = viewport.mapWorldToViewport(MANUAL_BACKGROUND_OFFSET);

      RoundRectangle2D manualBg =
        new RoundRectangle2D.Double(nodeBoundsVP.getX() - offset,
                                    nodeBoundsVP.getY() - offset,
                                    nodeBoundsVP.getWidth() + 2*offset,
                                    nodeBoundsVP.getHeight() + 2*offset,
                                    2*viewport.mapWorldToViewport(_prefs.getNodeRectangleArcWidth()),
                                    2*viewport.mapWorldToViewport(_prefs.getNodeRectangleArcHeight()));


      DrawnShape drawnShape = new DrawnShape(manualBg, fillPaint, null, null);
      drawnShape.redraw(graphics);
      drawnList.add(drawnShape);
    }


    //
    // Draw rectangle
    //
    RoundRectangle2D roundRect =
      new RoundRectangle2D.Double(nodeBoundsVP.getX(),
                                  nodeBoundsVP.getY(),
                                  nodeBoundsVP.getWidth(),
                                  nodeBoundsVP.getHeight(),
                                  viewport.mapWorldToViewport(_prefs.getNodeRectangleArcWidth()),
                                  viewport.mapWorldToViewport(_prefs.getNodeRectangleArcHeight()));


    Paint fillPaint = makePaint(drawProps.getFillColor(), nodeBoundsVP);
    Stroke stroke = new BasicStroke((float)drawProps.getStrokeWidth());
    Paint strokePaint = makePaint(drawProps.getStrokeColor(), nodeBoundsVP);
    DrawnShape drawnShape = new DrawnShape(roundRect, fillPaint,
                                           strokePaint, stroke);
    drawnShape.redraw(graphics);
    Shape shape = drawnShape.getDrawnShape();

    HitTester ht = new HitTester(shape, DECORATION_NODE, node);
    hit.addHitTester(node, ht);
    drawnList.add(drawnShape);


    //
    // Draw node label
    //

    // Don't bother drawing text with tiny fonts
    if (drawProps.getFontSize() < _prefs.getSmallestVisibleFontSize()) {
      return; // BAIL
    }

    if (text != null) {
      Point2D nodeCenter = new Point2D.Double(nodeBoundsVP.getCenterX(),
                                              nodeBoundsVP.getCenterY());

      DrawnText drawnText = drawText(graphics, (int)drawProps.getFontSize(),
                                     drawProps.getFontColor(),
                                     drawProps.getMaxLabelWidth(),
                                     text, nodeCenter, true, true);
      TextScreenData tdata = drawnText.getDrawnTextScreenData();
      HitTester tester =
        new HitTester(tdata.getBounds(), DECORATION_NODE_TEXT, tdata);
      hit.addHitTester(node, tester);
      drawnList.add(drawnText);
    }

    //
    // Draw type icon(s)
    //
    drawNodeIcons(graphics, node, viewport, nodeBoundsVP, hit, drawnList);

    //
    // Draw slider
    //
    if (_prefs.getSliders()) {
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
    int value = FaderUtil.getSliderValue(edge, _prefs.getEdgeSliderMaximum());

    MSlider slider =
      (MSlider)ctx.getContextData(edge, "slider");

    // if slider is null or the slider resource has changed, create a
    // new slider
    if (slider == null)
    {
      try {
        slider = new MSlider(MSliderResource.HORIZONTAL,
                             _prefs.getEdgeSliderMinimum(),
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
    int value = FaderUtil.getSliderValue(node, _prefs.getNodeSliderMaximum());

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
   * Decorate the edge with the little icons.
   *
   * @param viewport the Viewport that maps our coordinate system to
   * the screen coordinate
   * @param edgeLabelBounds the bounds of the edge label in VIEWPORT
   * coordinates.
   */
  private void drawEdgeIcons(Graphics2D g,
                             DrawableEdge edge,
                             Viewport viewport,
                             Rectangle2D edgeLabelBounds,
                             HitTesterMap hit,
                             List drawnList)
  {
    double iconSpacing =
      viewport.mapWorldToViewport(_prefs.getInterIconSpacing());
    double iconWidth =
      viewport.mapWorldToViewport(_prefs.getIconWidth());

    //
    // All icons are in a horizontal line at 'y'
    //
    // We position the center of the icons 6pts
    // below the lower edge of the edge label bounds.
    double y = edgeLabelBounds.getMaxY() +
      viewport.mapWorldToViewport(NODE_ICON_Y_OFFSET);


    // We place the icons one after another after the edge label
    //
    // |------------|
    // | edge label |
    // |------------1--2--3...
    //
    double x = edgeLabelBounds.getMaxX();
    double dx = iconWidth * 0.5;

    TypeInfoAdapter typeInfo = TypeInfoAdapter.getTypeInfoAdapter(edge);
    for (int i=0; i<typeInfo.getTypeCount(); i++) {
      x += dx;
      if (typeInfo.isTypeVisible(i)) {
        String type = typeInfo.getType(i);
        URL url = typeInfo.getTypeURL(i);

        Rectangle2D rect = drawTypeIcon(g, viewport, type,
                                        new Point2D.Double(x, y),
                                        drawnList);

        if (rect != null) {
          FaderIconHitContext hitContext =
            new FaderIconHitContext(type, url, i);

          // Add the icon to the hit context list
          hit.addHitTester(edge, new HitTester(rect,
                                               DECORATION_EDGE_ICON,
                                               hitContext));
        }
      }
      dx = iconWidth + iconSpacing;
    }
  }



  /**
   * Decorate the node with the little icons.
   *
   * @param viewport the Viewport that maps our coordinate system to the
   *  screen coordinate
   * @param nodeBounds the node bounding box in viewport coordinates
   */
  private void drawNodeIcons(Graphics2D g,
                             DrawableNode node,
                             Viewport viewport,
                             Rectangle2D nodeBounds,
                             HitTesterMap hit,
                             List drawnList)
  {
    double iconSpacing =
      viewport.mapWorldToViewport(_prefs.getInterIconSpacing());
    double iconWidth =
      viewport.mapWorldToViewport(_prefs.getIconWidth());


    //
    // All icons are in a horizontal line at 'y'
    //
    // We position the center of the icons 6pts
    // below the lower edge of the node.
    //
    double y = nodeBounds.getMaxY() +
      viewport.mapWorldToViewport(NODE_ICON_Y_OFFSET);

    //
    // We place the icons right to left starting from the east bounds of
    // the node box.
    //
    //    |-------------|
    //    |  some node  |
    //    |-----3--2--1-|
    //
    // Some other routine should have already made sure that the
    // node is wide enough for this to work.
    //
    double x = nodeBounds.getMaxX();
    double dx = -(iconWidth * 0.5);

    TypeInfoAdapter typeInfo = TypeInfoAdapter.getTypeInfoAdapter(node);
    for (int i=0; i<typeInfo.getTypeCount(); i++) {
      x += dx;
      if (typeInfo.isTypeVisible(i)) {
        String type = typeInfo.getType(i);
        URL url = typeInfo.getTypeURL(i);

        Rectangle2D rect = drawTypeIcon(g, viewport, type,
                                        new Point2D.Double(x, y),
                                        drawnList);
        if (rect != null) {
          FaderIconHitContext hitContext =
            new FaderIconHitContext(type, url, i);

          // Add the icon to the hit context list
          hit.addHitTester(node, new HitTester(rect,
                                               DECORATION_NODE_ICON,
                                               hitContext));
        }
      }

      dx = -(iconWidth + iconSpacing);
    }
  }


  /*
   * Draw a single node icon at the specified position.
   *
   * @param ntype the icon type.
   * @param pt the point at which to draw the icon (The icon is
   * centered on this point).
   *
   * @return the HitTest object or null if no object is drawn.
   */
  private Rectangle2D drawTypeIcon(Graphics2D g,
                                   Viewport viewport,
                                   String ntype,
                                   Point2D pt,
                                   List drawnList)
  {
    if (_iconLoader != null) {
      double h = _prefs.getIconHeight();
      double vh = viewport.mapWorldToViewport(h);
      double zfactor = vh / h;

      MartiniIcon ico = _iconLoader.getIconForType(ntype);

      if (ico != null) {
        //
        // We need to scale the icon two times: once to convert it
        // to the height specified in the metrics, and once more to
        // deal with our current zoom.
        //
        // Conveniently, scaling twice means multiply the two
        // factors.
        //
        double factor = ico.getScaleFactor(h) * zfactor;

        //
        // More hacks to calculate the point at which to place the
        // icon.
        //
        Point2D imgTopLeft =
          new Point2D.Double(pt.getX() - (ico.getCenterX() * factor),
                             pt.getY() - (ico.getCenterY() * factor));
        DrawnImage drawnImage =
          drawImage(g, ico.getImage(), imgTopLeft, factor);

        drawnList.add(drawnImage);

        return (drawnImage.getDrawnImageScreenData().getBounds());
      }
    }
    return (null);
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


  /**
   * Draw a curved line from the tail to head point where the arc is
   * determined by the specified control point.
   */
  private DrawnShape drawCurvedLine(Graphics2D graphics,
                                    DrawProps drawProps,
                                    Point2D tailPt,
                                    Point2D controlPt,
                                    Point2D headPt)
  {
    Stroke stroke = new BasicStroke((float)drawProps.getStrokeWidth(),
                                    BasicStroke.CAP_ROUND,
                                    BasicStroke.JOIN_ROUND);
    Paint strokePaint = makePaint(drawProps.getStrokeColor(), null);

    QuadCurve2D quad = new QuadCurve2D.Double();
    quad.setCurve(tailPt, controlPt, headPt);

    DrawnShape drawnShape = new DrawnShape(quad, null, strokePaint, stroke);
    drawnShape.redraw(graphics);

    return (drawnShape);
  }

  private DrawnShape drawEdgeArrow(Graphics2D graphics,
                                   ArrowHead arrow,
                                   Viewport viewport,
                                   DrawProps drawProps)
  {
    Point2D points[] = new Point2D[arrow.getPointCount()];
    for (int i=0; i<arrow.getPointCount(); i++) {
      points[i] = viewport.mapWorldToViewport(arrow.getPoint(i));
    }

    return (drawPolygon(graphics,drawProps, points));
  }


  private DrawnShape drawPolygon(Graphics2D graphics,
                                 DrawProps drawProps,
                                 Point2D points[])
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
    drawnShape.redraw(graphics);

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
    //
    // calculate rectangle to clear
    //
    Rectangle2D headNodeViewportBounds =
      calcNodeClearRect(edge.getHead(), ctx, viewport);

    Rectangle2D tailNodeViewportBounds =
      calcNodeClearRect(edge.getTail(), ctx, viewport);

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
   * including the type icons.
   */
  private Size calcTotalNodeWorldSize(Graphics2D graphics,
                                      DrawableNode node,
                                      String nodeLabel,
                                      double scale)
  {
    NodeSize nodeOnly =
      calcNodeWorldSize(graphics, node, nodeLabel, scale);

    // TODO: calculate icon bounds and add to node bounds
    double iconSpacing = (scale * _prefs.getInterIconSpacing());
    double iconWidth   = (scale * _prefs.getIconWidth());
    double iconHeight  = (scale * _prefs.getIconHeight());
    double yOffset     = (scale *  NODE_ICON_Y_OFFSET);


    TypeInfo typeInfo = TypeInfo.getTypeInfo(node);
    int typeCount = typeInfo.getTypeCount();

    // calculate the width for all icons
    iconWidth = (typeCount * iconWidth) + ((typeCount - 1) * iconSpacing);

    // calculate the height of the icons
    iconHeight = (iconHeight * 0.5) + yOffset;

    if (nodeOnly.getWidth() < iconWidth) {
      nodeOnly.setWidth(iconWidth);
    }

    if (iconHeight > 0) {
      nodeOnly.setHeight(iconHeight + nodeOnly.getHeight());
    }

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
   * multiline text.
   */
  private NodeSize calcNodeWorldSize(Graphics2D graphics,
                                     DrawableNode node,
                                     String nodeLabel,
                                     double scale)
  {
    // First calculate the width required to hold the text:
    DrawProps props =
      _prefs.getNodeDrawProps(false,
                              FaderUtil.getSliderValue(node,
                                               _prefs.getNodeSliderMaximum()));
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
    double offset = viewport.mapWorldToViewport(MANUAL_BACKGROUND_OFFSET);
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


  /*
   * Get the node DrawProps, adjusted for the view transform.
   */
  private DrawProps getNodeViewportDrawProps(DrawableNode node,
                                             Viewport viewport)
  {
    return (mapWorldToViewport
            (_prefs.getNodeDrawProps(FaderUtil.isSelected(node),
                                     FaderUtil.getSliderValue(node,
                                               _prefs.getNodeSliderMaximum())),
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
    DrawProps result =
      _prefs.getEdgeDrawProps(FaderUtil.getSliderValue(edge,
                                           _prefs.getEdgeSliderMaximum()),
                              FaderUtil.getSliderValue(edge.getTail(),
                                           _prefs.getNodeSliderMaximum()),
                              FaderUtil.isEdgePositive(edge));

    // Transform sizeal attributes
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
