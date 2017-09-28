package com.appliedminds.martinix.slider;

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
import java.awt.geom.GeneralPath;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.awt.geom.RoundRectangle2D;
import java.awt.geom.QuadCurve2D;
import java.text.AttributedCharacterIterator;
import java.text.AttributedString;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;


// tiles:
import java.awt.geom.AffineTransform;



/**
 * This is a concrete GraphUI that draws baysian networks with sliders
 * on nodes which allow the user to adjust probability settings on
 * them.
 *
 * <p>Its companion class, SliderUIPrefs can be used to customize
 * it to some extent.
 *
 * <p>This GraphUI requires that all the <i>getNNNPropertyName</i> methods
 * in the SliderUIPrefs return property names that exist in the graph.  In
 * addition, the following properties are required or used:
 *
 * <ul>
 *
 * <li>slidervalue - If present this is used for the value of the
 * slider.  If this value is not present then it is created and set to
 * "0.0".  This should be a double value encoded as a String.
 *
 * </ul>
 *
 *
 * <p>This GraphUI genereated events for the following UI Decoration
 * identifiers:
 *
 * <ul>
 * <li>NODE (returns a DrawableNode)
 * <li>NODE_TEXT (returns a martinix.ui.TextScreenData)
 * <li>EDGE (returns a DrawableEdge)
 * <li>SLIDER_LINE
 * <li>SLIDER_ARROW
 * </ul>
 *
 * @author will@apmindsf.com
 * @author mathias@apmindsf.com
 */
public class SliderUI implements GraphUI {

  private static final String DECORATION_NODE = "NODE";
  private static final String DECORATION_NODE_TEXT = "NODE_TEXT";
  private static final String DECORATION_EDGE = "EDGE";
  private static final String DECORATION_SLIDER_LINE = "SLIDER_LINE";
  private static final String DECORATION_SLIDER_ARROW = "SLIDER_ARROW";
  private static final String DECORATION_PROB_SWITCH = "PROB_SWITCH";

  private static final double SWITCH_WIDTH = 10.0;
  private static final double SWITCH_HEIGHT = 10.0;

  private static final int EDIT_NONE = 0;
  private static final int EDIT_TEXT = 1;
  private static final int EDIT_SLIDER = 2;

  private GraphPanel _graphPanel;
  private SliderUIPrefs _prefs;
  private FontCache _defaultFontCache;
  private HashMap _drawnObjects;

  private InPlaceTextEditor _editor;
  private Slider _currentSlider;
  private DrawableNode _currentSliderNode;

  private boolean _realTimeSliders = true;


  //
  // This will be set to one of the EDIT_nnn constants defined above.
  //
  private int _editMode;



  private static final double SLIDER_OFFSET = -8.0;


  private boolean _pixmap;
  private Tiles _tiles;



  /**
   * Create a new SliderUI.
   *
   * @throws MartiniError if there is a serious initialization error.
   */
  public SliderUI(SliderUIPrefs prefs) throws MartiniError {
    _prefs = prefs;
    _pixmap = "true".equals(System.getProperty("pixmap"));
    _drawnObjects = new HashMap();
    _editor = null;
    _editMode = EDIT_NONE;

    _defaultFontCache = new FontCache(_prefs.getNodeFontResourcePath());

    if (_pixmap) {
      /* Experimental pixmap code */
      try {
        _tiles = new Tiles();
      }
      catch(Exception e) {
        throw(new MartiniError("tiles init error"));
      }
    }
  }


  //
  // Begin GraphUI interface
  //

  public void setGraphPanel(GraphPanel graphPanel) {
    _graphPanel = graphPanel;
  }

  public Size getMinimumSize(Graphics2D g, DrawableNode node, double scale) {
    return(getPreferredSize(g, node, scale));
  }

  public Size getMaximumSize(Graphics2D g, DrawableNode node, double scale) {
    return(getPreferredSize(g, node, scale));
  }

  /*
   * The size that we return to the layout manager is the size of the
   * pill PLUS the size of the slider.  Later we will calcuate the
   * size of the pill by itself and cache that data in the context.
   */
  public Size getPreferredSize(Graphics2D g, DrawableNode node, double scale) {
    return(calcTotalNodeWorldSize(g, node, SliderUtil.getNodeLabel(node),
                                  scale));
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
      if ((!newBuffer) && erase) {
        eraseNode(graphics, node, viewport, hit, ctx);
      }
      drawNode(graphics, node, viewport, hit, ctx);
    }
  }


  public boolean setupEditSession(DrawableGraphElement el,
                                  Point loc,
                                  Object ctx)
  {
    if (!(el instanceof DrawableNode)) {
      return(false); // BAIL
    }

    /* User wants to do a text edit */
    if (ctx instanceof TextScreenData) {
      TextScreenData sdata = (TextScreenData) ctx;
      if (sdata == null) {
        return(false); // BAIL
      }

      DrawableNode node = (DrawableNode) el;

      _editor = new InPlaceTextEditor(node,
                                      SliderUtil.getNodeLabel(node),
                                      sdata,
                                      loc);

      setEditMode(EDIT_TEXT);

      // accept the edit mode request
      return(true); // BAIL
    }
    /* User wants to move a slider */
    else if (ctx instanceof Slider) {
      _currentSlider = (Slider) ctx;
      _currentSliderNode = (DrawableNode) el;
      setEditMode(EDIT_SLIDER);

      // accept the edit mode request.
      return(true); // BAIL
    }
    else {
      // deny the edit mode request
      return(false);
    }
  }


  public void teardownEditSession() {
    _editor = null;
    _currentSlider = null;
    _currentSliderNode = null;
    setEditMode(EDIT_NONE);
  }



  public byte mouseEvent(MouseEvent e)
  {
    if (!isEditModeSet()) {
      throw(new MartiniError("Call to mouseEvent but no edit session in place!"));
    }

    byte flags = 0;

    /* Text Edit */
    if (isEditModeText()) {

      if (_editor.mouseEvent(e) == InPlaceTextEditor.TEXTEDIT_CONTINUE) {
        flags |= FLAG_CONTINUE_EDIT;
      }

      if (_editor.isModified()) {
        flags |= FLAG_NEEDS_REPAINT;
      }
    }

    /* Slider move */
    else if (isEditModeSlider()) {

      Point pt = e.getPoint();
      double pv = _currentSlider.calculateValueFromYCoord(e.getY(), _currentSlider.getMax());
      _currentSlider.setValue(pv);
      SliderUtil.setSliderValue(_currentSliderNode,
                                _currentSlider.getValue());

      flags |= FLAG_CONTINUE_EDIT;
      flags |= FLAG_NEEDS_REPAINT;
    }

    return(flags);
  }



  public byte keyPressed(KeyEvent ke) {

    if ((!isEditModeSet()) || (_editor == null)) {
      throw(new MartiniError("Call to keyPressed but no edit session in place!"));
    }

    byte flags = 0;

    if (isEditModeText()) {

      int keyPressResult = _editor.keyPressed(ke);
      if (keyPressResult == InPlaceTextEditor.TEXTEDIT_CANCEL) {
        SliderUtil.setNodeLabel((DrawableNode) _editor.getTarget(),
                                _editor.getOriginalText());
        flags |= FLAG_NEEDS_REPAINT;
        return flags; // BAIL
      }
      else if (keyPressResult == InPlaceTextEditor.TEXTEDIT_CONTINUE) {
        flags |= (FLAG_CONTINUE_EDIT | FLAG_NEEDS_REPAINT);
      }

      DrawableNode node = (DrawableNode) _editor.getTarget();

      String prevLabel = SliderUtil.getNodeLabel(node);
      String newLabel = _editor.getText();

      if (! prevLabel.equals(newLabel)) {
        SliderUtil.setNodeLabel(node, newLabel);
        flags |= FLAG_NEEDS_REPAINT;
      }
    }

    return(flags);
  }


  public boolean getRealTimeSliderMode() {
    return _realTimeSliders;
  }

  public void setRealTimeSliderMode(boolean b) {
    _realTimeSliders = b;
  }


  /**
   * The SliderUI requires that a graph contains no cycle.
   *
   * @param g the graph to validate.
   * @return true if the graph is valid for the SliderUI.
   */
  public boolean validate(DrawableGraph g)
  {
    // cycle detection call here
    return true;
  }

  //
  // End GraphUI interface
  //



  /**
   * Get the list of DrawnObjects for the given graph element.
   */
  private List getDrawnObjectsForGraphElement(DrawableGraphElement element)
  {
    List drawnList = (List)_drawnObjects.get(element);
    if (drawnList == null) {
      drawnList = new ArrayList();
      _drawnObjects.put(element, drawnList);
    }

    return (drawnList);
  }


  private void drawNode(Graphics2D graphics,
                        DrawableNode node,
                        Viewport viewport,
                        HitTesterMap hit,
                        DrawableGraphContext ctx)
  {
    //
    // keep track of a list of DrawnObjects for this graph element
    //
    List drawnList = getDrawnObjectsForGraphElement(node);
    drawnList.clear();

    hit.removeAllHitTesters(node);

    if (!node.isVisible()) {
      return; // BAIL
    }

    //
    // nodeBounds must be the bounds of the pill-shaped node only.
    //
    Rectangle2D nodeBoundsW = getNodeWorldBounds(node, ctx,
                                                 graphics, viewport);
    Rectangle2D nodeBoundsVP = viewport.mapWorldToViewport(nodeBoundsW);

    // Don't bother drawing excessively tiny nodes
    if (nodeBoundsVP.getHeight() < _prefs.getSmallestVisibleNodeHeight())
    {
      return; // BAIL
    }

    // Get the node DrawProps, adjusted for the view transform
    DrawProps drawProps = getNodeViewportDrawProps(node, viewport);

    // See if we are in an ineractive edit session with this node.
    boolean editMe = (_editor != null) && isEditModeText() &&
      node.equals(_editor.getTarget());

    String text = SliderUtil.getNodeLabel(node);

    if (editMe) {
      // We may need to adjust the node width due to editing.
      NodeSize nodeSize =
        calcNodeWorldSize(graphics, node, text, viewport.getScale());

      if (ctx.getBounds(node).getWidth() != nodeSize.getWidth())
      {
        // we need to update the node's totalBounds in ctx
        double dw = nodeBoundsW.getWidth() - nodeSize.getWidth();

        Rectangle2D newBounds =
          new Rectangle2D.Double(nodeBoundsW.getX() + (dw / 2.0),
                                 nodeBoundsW.getY(),
                                 nodeSize.getWidth(),
                                 nodeSize.getHeight());

        // this call will set the bounds to include the slider
        setNodeWorldBounds(node, newBounds, ctx, viewport);

        // Redefine nodeBounds with the new bounds (including the slider)  *EEK*
        nodeBoundsVP =
          viewport.mapWorldToViewport(getNodeWorldBounds(node, ctx,
                                                         graphics, viewport));
      }
    }


    if (_pixmap) {
      /* Experimantal pixmap code */
      AffineTransform trans = graphics.getTransform();
      DrawnObject obj = drawPixmapNode(graphics, nodeBoundsVP, node);
      drawnList.add(obj);
      Rectangle2D shape = new Rectangle2D.Double();
      shape.setRect(nodeBoundsVP);
      HitTester ht = new HitTester(shape, DECORATION_NODE, node);
      hit.addHitTester(node, ht);
      graphics.setTransform(trans);
    }
    else {
      Point2D arcDimensions = new Point2D.Double(nodeBoundsVP.getHeight(),
                                                 nodeBoundsVP.getHeight());
      RoundRectangle2D rect =
        new RoundRectangle2D.Double(nodeBoundsVP.getX(),
                                    nodeBoundsVP.getY(),
                                    nodeBoundsVP.getWidth(),
                                    nodeBoundsVP.getHeight(),
                                    arcDimensions.getX(),
                                    arcDimensions.getY());

      Paint fillPaint = makePaint(drawProps.getFillColor(), nodeBoundsVP);
      Stroke stroke = new BasicStroke((float)drawProps.getStrokeWidth());
      Paint strokePaint = makePaint(drawProps.getStrokeColor(), nodeBoundsVP);
      DrawnShape drawnShape = new DrawnShape(rect, fillPaint,
                                             strokePaint, stroke);
      drawnShape.redraw(graphics);
      Shape shape = drawnShape.getDrawnShape();

      HitTester ht = new HitTester(shape, DECORATION_NODE, node);
      hit.addHitTester(node, ht);
      drawnList.add(drawnShape);
    }



    // Don't bother drawing text with tiny fonts
    if (drawProps.getFontSize() < _prefs.getSmallestVisibleFontSize()) {
      return; // BAIL
    }

    // Draw the label
    if (text != null)
    {
      Point2D nodeCenter = new Point2D.Double(nodeBoundsVP.getCenterX(),
                                              nodeBoundsVP.getCenterY());

      DrawnText drawnText = drawText(graphics, drawProps, text, nodeCenter);
      TextScreenData tdata = drawnText.getDrawnTextScreenData();
      HitTester tester =
        new HitTester(tdata.getBounds(), DECORATION_NODE_TEXT, tdata);
      hit.addHitTester(node, tester);
      drawnList.add(drawnText);

      if (editMe) {
        _editor.updateTextScreenData(tdata);

        // draw selection
        if (_editor.hasSelection()) {
          TextLayout layout = tdata.getTextLayout(_editor.getCaretIndex());
          Shape sel =
            tdata.getLogicalHighlightShape(_editor.getSelectionStartIndex(),
                                            _editor.getSelectionStopIndex());

          drawTextSelection(graphics,
                            tdata,
                            sel,
                            _prefs.getSelectedTextColor());
        }


        // draw cursor
        drawCaret(graphics, tdata);
      }
    }

    drawSlider(graphics, node, nodeBoundsVP, viewport, hit, ctx);

    if (_prefs.getDrawModeSwitchOnNode()) {
      drawModeSwitch(graphics, node, nodeBoundsVP, viewport, hit, ctx);
    }
  }



  /*
   * Draw a fancy pixmapped node.
   *
   * @param nodeBounds should be in VIEWPORT coords.  This must be the bounds
   * of the node only (no slider!).
   */
  private DrawnObject drawPixmapNode(Graphics2D g,
                                     Rectangle2D nodeBounds,
                                     DrawableNode node)
  {
    DrawnObject o = new DrawnPixmapNode(nodeBounds,
                                        SliderUtil.getSliderValue(node));
    o.redraw(g);
    return(o);
  }



  /**
   *
   *
   * Class that knows how to render a pixmap node.
   *
   *
   */
  private class DrawnPixmapNode extends DrawnObject {
    private Rectangle2D __nodeBounds;
    private AlphaComposite __lightener;


    /**
     * @param bounds the viewport bounds of the node (without the
     * slider).
     * @param sval the slider value (0 <= sval <= 100)
     */
    public DrawnPixmapNode(Rectangle2D bounds, double sval) {
      super();
      __nodeBounds = bounds;
      __lightener = AlphaComposite.getInstance(AlphaComposite.SRC_OVER,
                                               (float) (sval / 100.0));
    }

    public void redraw(Graphics2D g) {
      super.redraw(g);

      int x = (int) __nodeBounds.getX();
      int y = (int) __nodeBounds.getY();

      // bgArea is the area used by the background image for the node.
      Rectangle2D bgArea = Tiles.computeBGBoundsVP(__nodeBounds);

      //
      // BACKGROUND
      //
      Composite old = g.getComposite();
      g.setComposite(__lightener);
      g.setPaint(_tiles.bgPaint);
      g.fill(bgArea);
      g.setComposite(old);


      g.translate(x, y);


      AffineTransform trans = new AffineTransform();

      // I probably need to set the composite!


      //
      // CORNERS
      //

      // NW
      g.drawRenderedImage(_tiles.nwCorner.getImage(), trans);

      // NE
      g.translate(bgArea.getWidth() + 1.0, 0.0);
      g.drawRenderedImage(_tiles.neCorner.getImage(), trans);

      // SE
      g.translate(0.0, bgArea.getHeight() + 1.0);
      g.drawRenderedImage(_tiles.seCorner.getImage(), trans);

      // SW
      g.translate(- (bgArea.getWidth() + 1.0), 0.0);
      g.drawRenderedImage(_tiles.swCorner.getImage(), trans);


      g.translate(0.0, - (bgArea.getHeight() + 1.0)); // back to (x,y)



      //
      // N and S BORDERS
      //
      int span = ((int) bgArea.getWidth()) - (2 * Tiles.CORNER_OVERLAY);


      if (span > 0) {
        g.translate((double) Tiles.CORNER_TILE_DIM, 0.0);
        double dx = 0.0;
        for(int i=0; i < (span / _tiles.horizTileN.getWidth()); ++i) {
          // N
          if (i > 0) {
            g.translate((double) _tiles.horizTileN.getWidth(), 0.0);
            dx += _tiles.horizTileN.getWidth();
          }
          g.drawRenderedImage(_tiles.horizTileN.getImage(), trans);

          // S
          g.translate(0.0, bgArea.getHeight() + 1.0);
          g.drawRenderedImage(_tiles.horizTileS.getImage(), trans);
          g.translate(0.0, - (bgArea.getHeight() + 1.0));
        }
        g.translate( - dx, 0.0);
        g.translate( - ((double) Tiles.CORNER_TILE_DIM), 0.0);  // back to (x,y)
      }



      //
      // E and W BORDERS
      //

      span = ((int) bgArea.getHeight()) - (2 * Tiles.CORNER_OVERLAY);
      if (span > 0) {
        g.translate(0.0, (double) Tiles.CORNER_TILE_DIM);
        double dy = 0.0;
        for(int i=0; i < (span / _tiles.vertTileW.getHeight()); ++i) {
          // E
          if (i > 0) {
            g.translate(0.0, _tiles.vertTileW.getHeight());
            dy += _tiles.vertTileW.getHeight();
          }
          g.drawRenderedImage(_tiles.vertTileW.getImage(), trans);

          // W
          g.translate(bgArea.getWidth() + 1.0, 0.0);
          g.drawRenderedImage(_tiles.vertTileE.getImage(), trans);
          g.translate(- (bgArea.getWidth() + 1.0), 0.0);
        }

        g.translate(0.0, - dy);
        g.translate(0.0, - ((double) Tiles.CORNER_TILE_DIM)); // back to (x,y)
      }


      g.translate(-x, -y); // back to original
    }
  }// end "DrawnPixmapNode"





  private boolean isCurrentEditTarget(DrawableNode n) {
    return((_editor != null) && (_editor.getTarget() == n));
  }



  /**
   * @param nodeBounds this is in VIEWPORT COORDINATE!
   */
  private void drawSlider(Graphics2D graphics,
                          DrawableNode node,
                          Rectangle2D nodeBounds,
                          Viewport viewport,
                          HitTesterMap hit,
                          DrawableGraphContext ctx)
  {
    //
    // keep track of list of the DrawnObjects for this node's slider
    //
    List drawnList = getDrawnObjectsForGraphElement(node);

    double value = SliderUtil.getSliderValue(node);

    Slider slider = (Slider) ctx.getContextData(node, "slider");

    if (slider == null) {
      Slidee.SlideeState state = new Slidee.SlideeState(0.0, 100.0, value);
      Slidee.SlideeGUI gui = new Slidee.SlideeGUI(viewport, 0.0, 0.0);
      slider = new Slider(state, gui);
      ctx.putContextData(node, "slider", slider);
      if ((isEditModeSlider()) &&
          (!_realTimeSliders) &&
          (! isCurrentEditTarget(node)))
      {
        SliderUtil.setSliderVisible(node, false);
      }
      else {
        SliderUtil.setSliderVisible(node, true);
      }
    }
    else {
      slider.setValue(value);
    }

    // If slider is invisible don't do anything else.
    if (!SliderUtil.getSliderVisible(node)) {
      return; // BAIL
    }

    //
    // Now figure out where to draw the slider on the screen.  We want
    // it on top of the node and aligned with the right hand edge of
    // the node.
    //
    // This code must work together with the code in
    // calcSizeOfNodeAndSlider(): If we modify where we draw the
    // slider then we must also modify the way that the overall bounds
    // is computed.
    //
    double sx = nodeBounds.getX() + nodeBounds.getWidth() + (SLIDER_OFFSET * viewport.getScale());
    double sy = nodeBounds.getY() -
      ((slider.getHeightVP() - nodeBounds.getHeight()) / 2.0);

    slider.setLocation(sx, sy);

    DrawnObject[] drawnObjs =
      Slidee.drawSlidee(graphics, viewport, slider.getState(), slider.getGUI());

    for (int i = 0; i < drawnObjs.length; i++) {
      drawnList.add(drawnObjs[i]);
    }

    hit.addHitTester(node, new HitTester(slider.getGUI().getArrowPath(),
                                         DECORATION_SLIDER_ARROW,
                                         slider));

    hit.addHitTester(node, new HitTester(slider.getGUI().getLineBounds(),
                                         DECORATION_SLIDER_LINE,
                                         slider));
  }


  private void drawEdge(Graphics2D graphics,
                        DrawableEdge edge,
                        Viewport viewport,
                        HitTesterMap hit,
                        DrawableGraphContext ctx)
  {
    //
    // keep track of a list of DrawnObjects for this graph element
    //
    List drawnList = getDrawnObjectsForGraphElement(edge);
    drawnList.clear();

    hit.removeAllHitTesters(edge);

    if (!edge.isVisible()) {
      return; // BAIL
    }

    DrawProps drawProps = getEdgeViewportDrawProps(edge, viewport);
    boolean curvedEdge = _prefs.getCurvedLines();

    Rectangle2D headNodeWorldBounds =
      getNodeWorldBounds(edge.getHead(), ctx, graphics, viewport);
    Rectangle2D tailNodeWorldBounds =
      getNodeWorldBounds(edge.getTail(), ctx, graphics, viewport);

    if (_pixmap) {
      /* Experimental pixmap code */
      headNodeWorldBounds = Tiles.computeBGBoundsW(headNodeWorldBounds);
      tailNodeWorldBounds = Tiles.computeBGBoundsW(tailNodeWorldBounds);
    }

    // Find closest canonical connection point, N, E, S, or W, etc.:
    EdgeGeometry egWorld = new EdgeGeometry(edge,
                                            true,
                                            headNodeWorldBounds,
                                            tailNodeWorldBounds,
                                            _prefs.getArrowHeadLength(),
                                            _prefs.getArrowHeadFlair(),
                                            _prefs.getArrowHeadOffset(),
                                            true);
    Point2D headAttachPointW = egWorld.getHeadPoint();
    Point2D tailAttachPointW = egWorld.getTailPoint();

    EdgeGeometry egViewport = egWorld.mapWorldToViewport(_graphPanel);
    Point2D headAttachPointVP = egViewport.getHeadPoint();
    Point2D tailAttachPointVP = egViewport.getTailPoint();

    DrawnShape drawnShape = null;
    ArrowHead arrow = null;
    Point2D arrowTailPoint = tailAttachPointW;

    if (curvedEdge) {
      // draw curved line

      // this is the key for a curved edge
      Point2D controlPt = figureControlPoint(egViewport.getTailPoint(),
                                             egViewport.getHeadPoint());
      Point2D controlPtWorld = viewport.mapViewportToWorld(controlPt);

      drawnShape = drawCurvedLine(graphics, drawProps,
                                  egViewport.getTailPoint(),
                                  controlPt,
                                  egViewport.getHeadPoint());
      arrowTailPoint = controlPtWorld;
    }
    else {
      // draw straight line
      drawnShape =
        drawPolyline(graphics, drawProps, egViewport.getEdgePoints());
    }

    Shape path = drawnShape.getDrawnShape();
    HitTester tester = new HitTester(path, DECORATION_EDGE, edge);
    hit.addHitTester(edge, tester);
    drawnList.add(drawnShape);

    //
    // Calculate arrow points
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
      arrowBottomPtVP = arrowTailPoint;
    }

    arrow = new ArrowHead(viewport.mapViewportToWorld(arrowPtVP),
                          viewport.mapViewportToWorld(arrowBottomPtVP),
                          _prefs.getArrowHeadLength(),
                          _prefs.getArrowHeadFlair());

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
  }


  private void drawModeSwitch(Graphics2D graphics,
                              DrawableNode node,
                              Rectangle2D nodeBoundsVP,
                              Viewport viewport,
                              HitTesterMap hit,
                              DrawableGraphContext ctx)
  {
    List drawnList = getDrawnObjectsForGraphElement(node);
    boolean prMode = SliderUtil.getNodeInverseProbabilityMode(node);
    double switchWidthVP = viewport.mapWorldToViewport(SWITCH_WIDTH);
    double switchHeightVP = viewport.mapWorldToViewport(SWITCH_HEIGHT);

    Rectangle2D switchBounds = new Rectangle2D.Double(nodeBoundsVP.getX(),
                                                      nodeBoundsVP.getY(),
                                                      switchWidthVP,
                                                      switchHeightVP);

    DrawProps drawProps = getNodeViewportDrawProps(node, viewport);
    Paint fillPaint = makePaint(drawProps.getSwitchColor(prMode), switchBounds);
    Stroke stroke = new BasicStroke((float) drawProps.getStrokeWidth());
    Paint strokePaint = makePaint(drawProps.getStrokeColor(), nodeBoundsVP);

    DrawnShape drawnShape = new DrawnShape(switchBounds, fillPaint,
                                           strokePaint, stroke);
    drawnShape.redraw(graphics);
    HitTester ht = new HitTester(drawnShape.getDrawnShape(),
                                 DECORATION_PROB_SWITCH, node);
    hit.addHitTester(node, ht);
    drawnList.add(drawnShape);
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



  private Rectangle2D calcNodeClearRect(DrawableNode n,
                                        DrawableGraphContext ctx,
                                        Viewport viewport)
  {
    Rectangle2D nodeBounds = calcDrawnNodeViewportBounds(n, ctx, viewport);

    //
    // This seems to fix a strange off-by-on'ish error that was
    // occuring in "delayed response" mode.  Maybe it is due to
    // rounding errors or antialiasing.
    //
    nodeBounds.setFrame(nodeBounds.getX() - 1.0,
                        nodeBounds.getY(),
                        nodeBounds.getWidth() + 3.0,
                        nodeBounds.getHeight() + 2.0);
    return(nodeBounds);
  }



  /*
   * Return a rectangle that includes the node plus its slider.
   *
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
      List drawnList = (List)_drawnObjects.get(edge);
      for(Iterator itr2 = drawnList.iterator(); itr2.hasNext(); ) {
        ((DrawnObject)itr2.next()).redraw(graphics);
      }

      //      ctx.setNeedsRepaint(edge, false);
    }

    for(Iterator itr = nodes.iterator(); itr.hasNext(); ) {
      DrawableNode node = (DrawableNode) itr.next();
      List drawnList = (List)_drawnObjects.get(node);
      for(Iterator itr2 = drawnList.iterator(); itr2.hasNext(); ) {
        ((DrawnObject)itr2.next()).redraw(graphics);
      }
      //      ctx.setNeedsRepaint(node, false);
    }

    // Reset the clip area.
    graphics.setClip(null);
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


  /**
   * Get a width & height measurement that is large enough to hold the
   * node plus its slider.
   */
  private Size calcTotalNodeWorldSize(Graphics2D graphics,
                                      DrawableNode node,
                                      String nodeLabel,
                                      double scale)
  {
    NodeSize nodeOnly =
      calcNodeWorldSize(graphics, node, nodeLabel, scale);

    Size sliderSize = new Size(Slidee.getSize());

    return(calculateSizeOfNodeAndSlider(nodeOnly.getWidth(),
                                        nodeOnly.getHeight(),
                                        sliderSize.getWidth(),
                                        sliderSize.getHeight(),
                                        scale));
  }



  /*
   * Figure out the preferred size of a node without accounting for
   * the slider.
   *
   *
   * @param nodeLabel the actual label to use for the node.  If you
   * want to shorten it, do so before calling this method.
   * @return bounds in WORLD co-ordinates.
   *
   *
   * (Previous comment...)
   *
   * This is the method that calculates the bounds of the pill-shaped
   * rounded rectangle for a node.
   *
   * This calculation must be carried out in viewport coordinates,
   * since it depends on the text metrics.
   *
   * The node must be long enough to fit all the text and also to
   * display all the icons.
   *
   */
  private NodeSize calcNodeWorldSize(Graphics2D graphics,
                                     DrawableNode node,
                                     String nodeLabel,
                                     double scale)
  {
    //
    // First calculate the width required to hold the text:
    //
    DrawProps props = _prefs.getNodeDrawProps(false, false, 0);
    Font font =
      _defaultFontCache.getFontBySize((int)(props.getFontSize() * scale));
    MultiLineTextLayout layout =
      new MultiLineTextLayout((int)(_prefs.getMaxLabelWidth()*scale),
                              graphics, font, nodeLabel);

    double nodeWidth = layout.getWidth() + (2.0 * _prefs.getLabelHorizontalMargin());
    double nodeHeight = layout.getHeight() + (2.0 * _prefs.getLabelVerticalMargin());



    if (_pixmap) {
      /* Experimental pixmap code */
      nodeWidth = _tiles.roundWidthTotal(nodeWidth);
      nodeHeight = _tiles.roundHeightTotal(nodeHeight);
    }

    //
    // Perform a map from viewport --> world, and return in world
    // coordinates.
    //
    NodeSize sz = new NodeSize((nodeWidth / scale),
                               (nodeHeight / scale),
                               layout);
    return(sz);
  }



  /**
   * object.
   *
   * @param edge the edge
   */
  private DrawProps getEdgeViewportDrawProps(DrawableEdge edge,
                                             Viewport viewport)
  {
    DrawProps result = _prefs.getEdgeDrawProps(SliderUtil.getSliderValue(edge.getTail()));

    // Transform sizeal attributes
    result.setStrokeWidth(viewport.mapWorldToViewport(result.getStrokeWidth()));
    result.setFontSize(viewport.mapWorldToViewport(result.getFontSize()));
    result.setMaxLabelWidth((int)viewport.mapWorldToViewport(result.getMaxLabelWidth()));

    return result;
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
    return result;
  }


  /*
   *
   */
  private DrawProps getNodeViewportDrawProps(DrawableNode node,
                                             Viewport viewport)
  {
    return
      (mapWorldToViewport
       (_prefs.getNodeDrawProps(SliderUtil.isSelected(node),
                                isCurrentEditTarget(node),
                                SliderUtil.getSliderValue(node)),
        viewport.getScale()));
  }



  // cheesy hack to support color gradients
  private Paint makePaint(GraphColor graphColor, Rectangle2D bounds)
  {
    Paint result = null;

    if (graphColor.isHorizontalGradient())
    {
      Color color1 = graphColor.getColor1();
      Color color2 = graphColor.getColor2();
      result = new GradientPaint(
        new Point2D.Double(bounds.getX(), bounds.getY()),
        color1,
        new Point2D.Double(bounds.getX() + bounds.getWidth(), bounds.getY()),
        color2);
    }
    else if (graphColor.isVerticalGradient())
    {
      Color color1 = graphColor.getColor1();
      Color color2 = graphColor.getColor2();
      result = new GradientPaint(
        new Point2D.Double(bounds.getX(), bounds.getY()),
        color1,
        new Point2D.Double(bounds.getX(), bounds.getY() + bounds.getHeight()),
        color2);
    }
    else
    {
      result = graphColor.getColor();
    }

    return result;
  }


  /**
   * @return a valid TextScreenData object or NULL if the text is null.
   */
  private DrawnText drawText(Graphics2D graphics,
                             DrawProps drawProps,
                             String text,
                             Point2D center)
  {
    if (text == null)
      return(null); // BAIL

    Font font = _defaultFontCache.getFontBySize((int)drawProps.getFontSize());
    FontRenderContext frc = graphics.getFontRenderContext();

    Rectangle2D bounds;
    Rectangle2D drawnBounds;

    Paint fontPaint = null;
    MultiLineTextLayout layout = null;

    if (text.length() > 0)
    {
      layout = new MultiLineTextLayout(drawProps.getMaxLabelWidth(),
                                       graphics, font, text);

      drawnBounds =
        new Rectangle2D.Double(center.getX() - (layout.getWidth() / 2),
                               center.getY() - (layout.getHeight() / 2),
                               layout.getWidth(),
                               layout.getHeight());

      fontPaint = makePaint(drawProps.getFontColor(), drawnBounds);
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
      layout = new MultiLineTextLayout(drawProps.getMaxLabelWidth(),
                                       graphics, font, " ");

      // We don't actually draw anything.
      // Zero-width
      drawnBounds =
        new Rectangle2D.Double(center.getX(),
                               center.getY() - (layout.getHeight() / 2),
                               0,
                               layout.getHeight());
    }

    TextScreenData data = new TextScreenData(layout, drawnBounds, text);
    DrawnText drawnText = new DrawnMultiLineText(data, fontPaint, center);
    drawnText.redraw(graphics);

    return (drawnText);
  }



  /**
   * @param bounds the drawn bounds of the entire text block.
   */
  private void drawCaret(Graphics2D g,
                         TextScreenData tdata)
  {
    int y = tdata.getLineYByIndex(_editor.getCaretIndex());
    Shape caret = tdata.getCaretShape(_editor.getCaretIndex());

    double dx = tdata.getBounds().getX();
    double dy = tdata.getLineYByIndex(_editor.getCaretIndex()) +
      tdata.getLineHeight();

    g.translate(dx, dy);
    g.setXORMode(_prefs.getCaretColor());
    g.draw(caret);
    g.setPaintMode();
    g.translate(-dx, -dy);
  }


  /**
   * @param bounds the drawn bounds of the entire text block.
   */
  private void drawTextSelection(Graphics2D g,
                                 TextScreenData tdata,
                                 Shape selection,
                                 Color c)
  {
    double dx = tdata.getBounds().getX();
    double dy = tdata.getLineYByIndex(_editor.getCaretIndex()) +
      tdata.getLineHeight();

    g.translate(dx, dy);
    g.setXORMode(c);
    g.fill(selection);
    g.setPaintMode();
    g.translate(-dx, -dy);
  }


  private DrawnShape drawPolyline(Graphics2D graphics,
                                  DrawProps drawProps,
                                  Point2D points[])
  {
    Rectangle2D bounds = new Rectangle2D.Double();
    GeneralPath path = SliderUtil.makePath(points, bounds, false);

    Stroke stroke = new BasicStroke((float)drawProps.getStrokeWidth(),
                                    BasicStroke.CAP_ROUND,
                                    BasicStroke.JOIN_ROUND);
    Paint strokePaint = makePaint(drawProps.getStrokeColor(), bounds);

    DrawnShape drawnShape = new DrawnShape(path, null, strokePaint, stroke);
    drawnShape.redraw(graphics);

    return (drawnShape);
  }


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
    //graphics.draw(quad);
    //hit.addHitTester(edge, new HitTester(quad, DECORATION_EDGE, edge));

    DrawnShape drawnShape = new DrawnShape(quad, null, strokePaint, stroke);
    drawnShape.redraw(graphics);

    return (drawnShape);
  }


  private DrawnShape drawPolygon(Graphics2D graphics,
                                 DrawProps drawProps,
                                 Point2D points[])
  {

    Rectangle2D bounds = new Rectangle2D.Double();
    GeneralPath path = SliderUtil.makePath(points, bounds, true);

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
   * @return true if the edit mode is set to TEXT
   */
  private boolean isEditModeText() {
    return(_editMode == EDIT_TEXT);
  }


  /*
   * @return true if the edit mode is set to slider.
   */
  private boolean isEditModeSlider() {
    return(_editMode == EDIT_SLIDER);
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

    //
    // totalBounds = the bounds of the pill PLUS the bounds of the slider.
    //
    Rectangle2D totalBounds = ctx.getBounds(n);

    NodeSize pillSize = calcNodeWorldSize(graphics,
                                          n,
                                          SliderUtil.getNodeLabel(n),
                                          viewport.getScale());
    //
    // The x coordinate is the same as the one set by the layout.
    //
    double x = totalBounds.getX();

    //
    // Just put the pill Y coord in the middle of the totalBounds.
    //
    // Note that I have to subtract from the Y value since we are in
    // world coordinates.
    //
    double y = totalBounds.getY() -
      ((totalBounds.getHeight() - pillSize.getHeight()) / 2.0);

    Rectangle2D bounds = new Rectangle2D.Double(x,
                                                y,
                                                pillSize.getWidth(),
                                                pillSize.getHeight());

    ctx.putContextData(n, "bounds", bounds);
    ctx.putContextData(n, "nodeSize", pillSize);

    return(bounds);
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
  private Size calculateSizeOfNodeAndSlider(double nw,
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



  /*
   * Store/Set the world bounds of the pill-shape in the context.
   * This modifies the overall bounds also.
   *
   *
   * @param bounds the bounds for the pill-shape in WORLD coordinates.
   */
  private void setNodeWorldBounds(DrawableNode n,
                                  Rectangle2D bounds,
                                  DrawableGraphContext ctx,
                                  Viewport viewport)
  {

    Size sliderSize = new Size(Slidee.getSize());

    Size overallSize = calculateSizeOfNodeAndSlider(bounds.getWidth(),
                                                    bounds.getHeight(),
                                                    sliderSize.getWidth(),
                                                    sliderSize.getHeight(),
                                                    viewport.getScale());

    double w = overallSize.getWidth();
    double h = overallSize.getHeight();


    //
    // The x value does not change for the total bounds.
    //
    double x = bounds.getX();

    //
    // The y value is adjusted to center the node bounds inside the
    // total bounds.
    //
    // We have to add to the Y value since this is world coordinates.
    //
    double y = bounds.getY() + ((h - bounds.getHeight()) / 2.0);

    Rectangle2D totalBounds = new Rectangle2D.Double(x, y, w, h);

    ctx.putContextData(n, "bounds", bounds);
    NodeSize ns = (NodeSize) ctx.getContextData(n, "nodeSize");
    if (ns != null) {
      ns.setWidth(bounds.getWidth());
      ns.setHeight(bounds.getHeight());
    }
    ctx.setBounds(n, totalBounds);
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
  private static Point2D figureControlPoint(Point2D tail,
                                            Point2D head)
  {
    double dx = tail.getX() - head.getX();
    double dy = tail.getY() - head.getY();


    if (dx < 0) {
      return(new Point2D.Double(tail.getX() +
                                  (Math.abs(dx) / 2.0), tail.getY()));
    }
    else {
      return(new Point2D.Double(tail.getX() -
                                (Math.abs(dx) / 2.0), tail.getY()));
    }
  }



  /*
   * Size data PLUS a text layout object.
   */
  private class NodeSize extends Size
  {
    private MultiLineTextLayout __layout;

    public NodeSize(double width,
                    double height,
                    MultiLineTextLayout layout)
    {
      super(width, height);
      __layout = layout;
    }

    public MultiLineTextLayout getTextLayout() {
      return __layout;
    }
  }  // end class NodeSize


} // end class SliderUI





