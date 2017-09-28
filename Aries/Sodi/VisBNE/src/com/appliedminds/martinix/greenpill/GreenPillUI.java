package com.appliedminds.martinix.greenpill;

import com.appliedminds.martini.*;
import com.appliedminds.martinix.ui.*;
import java.awt.Color;
import java.awt.Shape;
import java.awt.Graphics2D;
import java.awt.Shape;
import java.awt.Point;
import java.awt.Composite;
import java.awt.AlphaComposite;
import java.awt.Dimension;
import java.awt.Stroke;
import java.awt.BasicStroke;
import java.awt.Paint;
import java.awt.GradientPaint;
import java.awt.geom.AffineTransform;
import java.awt.geom.Ellipse2D;
import java.awt.geom.GeneralPath;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.awt.geom.RoundRectangle2D;
import java.awt.geom.GeneralPath;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.HashSet;
import java.util.Set;
import java.util.StringTokenizer;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.net.URL;
import java.io.IOException;
import java.net.MalformedURLException;



/**
 * This is a concrete GraphUI that draws graphs in the style of the
 * original VGE.
 *
 * <p>Its companion class, GreenPillUIPrefs can be used to customize
 * it to some extent.
 *
 *
 * <p><i>This has been ported from the vge.lf.dlf.DefaultLookAndFeel
 * class so some code in here may be confusing.  At some point all the
 * old code will be fixed up and this message will be removed.</i>
 *
 * <p>Thie GraphUI requires the following graph properties:
 *
 * <ul>
 * <li>All the <i>getNNNPropertyName</i> methods in the GreenPillUIPrefs.
 * </ul>
 *
 * <p>This GraphUI genereated events for the following UI Decoration
 * identifiers:
 *
 * <ul>
 * <li>NODE (returns a DrawableNode)
 * <li>NODE_TEXT (returns a martinix.ui.TextScreenData)
 * <li>NODE_ICON (returns a DrawableNode)
 * <li>EDGE (returns a DrawableEdge)
 * <li>EDGE_BUBBLE (returns a DrawableEdge)
 * </ul>
 *
 *
 *
 * @author mathias@apmindsf.com
 * @author daepark@apmindsf.com
 * @author will@apmindsf.com
 */
public class GreenPillUI implements GraphUI {

  private static final String DECORATION_NODE = "NODE";
  private static final String DECORATION_NODE_TEXT = "NODE_TEXT";
  private static final String DECORATION_NODE_ICON = "NODE_ICON";
  private static final String DECORATION_EDGE = "EDGE";
  private static final String DECORATION_EDGE_BUBBLE = "EDGE_BUBBLE";

  private static final double NODE_TYPE_ICON_VERTICAL_OFFSET = 8.0;
  private static final double NODE_BOUNDS_FUDGE = 3.0;

  private GraphPanel _graphPanel;
  private J2DGraphicsRenderer _renderer;
  private GreenPillUIPrefs _prefs = GreenPillUIPrefs.getInstance();

  private IconLoader _iconLoader;

  // warning! this implementation is not thread safe
  private static HashMap _nodeTypeMap = new HashMap();

  private HashMap _drawnObjects;

  private InPlaceTextEditor _editor;





  /**
   * Create a new GreenPillUI.
   *
   * @throws GreenPillUIError if there is a serious initialization error.
   */
  public GreenPillUI() throws GreenPillUIError {

    try {
      _iconLoader = IconLoader.create(_prefs.getTypeMapURL());
    }
    catch(IOException e) {
      throw(new GreenPillUIError("GreenPill failed to init icon type map.  Check settings for getTypeMapURL.  Error = " + e));
    }
    _renderer = new J2DGraphicsRenderer(_prefs.getNodeFontResourcePath());
    _drawnObjects = new HashMap();
    _editor = null;
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


  public Size getPreferredSize(Graphics2D g, DrawableNode node, double scale) {
    return(calcNodeWorldSize(g, node, scale));
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
        //                              drawEdge(graphics, edge, viewport, hit, ctx, true);
        erase(graphics, edge, viewport, hit, ctx);
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
        //                              System.err.println("erasing drawn node");

        erase(graphics, node, viewport, hit, ctx);
      }

      drawNode(graphics, node, viewport, hit, ctx);
    }
  }





  public boolean setupEditSession(DrawableGraphElement el,
                                  Point loc,
                                  Object ctx)
  {
    if (!(el instanceof DrawableNode)) {
      return(false);
    }

    TextScreenData sdata = (TextScreenData) ctx;
    if (sdata == null) {
      return(false);
    }

    DrawableNode node = (DrawableNode) el;

    _editor = new InPlaceTextEditor(node,
                                    GreenPillUtil.getNodeLabel(node),
                                    sdata,
                                    loc);

    return(true);
  }



  public void teardownEditSession() {
    _editor = null;
  }



  public byte mouseEvent(MouseEvent e) {
    if (_editor == null) {
      throw(new GreenPillUIError("Call to mouseEvent but no edit session in place!"));
    }

    byte flags = 0;

    if (_editor.mouseEvent(e) == InPlaceTextEditor.TEXTEDIT_CONTINUE) {
      flags |= FLAG_CONTINUE_EDIT;
    }

    if (_editor.isModified()) {
      flags |= FLAG_NEEDS_REPAINT;
    }

    return(flags);
  }



  public byte keyPressed(KeyEvent ke) {

    if (_editor == null) {
      throw(new GreenPillUIError("Call to keyPressed but no edit session in place!"));
    }

    byte flags = 0;

    if (_editor.keyPressed(ke) == InPlaceTextEditor.TEXTEDIT_CONTINUE) {
      flags |= FLAG_CONTINUE_EDIT;
    }

    DrawableNode node = (DrawableNode) _editor.getTarget();

    String prevLabel = GreenPillUtil.getNodeLabel(node);
    String newLabel = _editor.getText();
    if (! prevLabel.equals(newLabel)) {
      GreenPillUtil.setNodeLabel(node, newLabel);
      flags |= FLAG_NEEDS_REPAINT;
    }

    return(flags);
  }



  /**
   * The GreenPillUI requires that a graph contains a root node, and that
   * all nodes in the graph have the rootdx property set (distance from root).
   * We check for these two properties and throw an error if we're unable to
   * deal with the graph given.
   *
   * @param g the graph to validate.
   * @return true if the graph is valid for the GreenPillUI.
   */
  public boolean validate(DrawableGraph g)
  {
    // first check if there is a root node.
    DrawableNode root = GreenPillUtil.getRootNode(g);
    if (root == null)
      return false;

    // check if we need to update the rootdx properties. here we only check
    // to see if one exists for each node. we leave it up to the user
    // to make sure that their values are actually correct. (since the app
    // will know when the graph is changed or updated to require the rootdx
    // properties to be updated.
    NodeIterator it = g.nodesIterator();
    boolean needsFix = false;
    while (it.hasNext()) {
      DrawableNode n = it.next();
      if (n.getProperty("rootdx") == null) {
        needsFix = true;
        break;
      }
    }

    if (needsFix) {
      GreenPillUtil.updateRootDistances(g);
    }

    return true;
  }

  //
  // End GraphUI interface
  //

  /**
   * Get the list of DrawnObjects for the given graph element.
   */
  private List getDrawnObjectsForGraphElement(DrawableGraphElement element) {
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

    Rectangle2D nodeBounds =
      viewport.mapWorldToViewport(ctx.getBounds(node));

    // Don't bother drawing excessively tiny nodes
    if (nodeBounds.getHeight() < _prefs.getSmallestVisibleNodeHeight())
    {
      return; // BAIL
    }

    // See if we are in an ineractive edit session with this node.
    boolean editMe = (_editor != null) && node.equals(_editor.getTarget());

    String text = null;
    if (editMe) {
      // Do not ellipsify IF editing.
      text = GreenPillUtil.getNodeLabel(node);
    }
    else {
      text = StringUtils.ellipsify(GreenPillUtil.getNodeLabel(node),
                                   _prefs.getMaxLabelLength());
    }

    if (editMe) {
      // We may need to adjust the node width due to editing.
      Size sz = calcNodeWorldSize(graphics, node, text, viewport.getScale());

      Rectangle2D oldbounds = ctx.getBounds(node);
      if (oldbounds.getWidth() != sz.getWidth()) {

        double dw = oldbounds.getWidth() - sz.getWidth();

        Rectangle2D newbounds =
          new Rectangle2D.Double(oldbounds.getX() + (dw / 2.0),
                                 oldbounds.getY(),
                                 sz.getWidth(),
                                 oldbounds.getHeight());

        ctx.setBounds(node, newbounds);

        // Redefine nodeBounds with the new bounds
        nodeBounds = viewport.mapWorldToViewport(newbounds);
      }
    }


    // Get the node DrawProps, adjusted for the view transform
    DrawProps drawProps = getNodeViewportDrawProps(node, viewport);

    DrawnShape drawnShape =
      _renderer.drawRoundedRectangle(graphics,
                                     drawProps,
                                     nodeBounds,
                                     new Point2D.Double
                                     (nodeBounds.getHeight(),
                                      nodeBounds.getHeight()));
    Shape shape = drawnShape.getDrawnShape();
    HitTester ht = new HitTester(shape, DECORATION_NODE, node);
    hit.addHitTester(node, ht);
    drawnList.add(drawnShape);

    // Don't bother drawing text with tiny fonts
    if (drawProps.getFontSize() < _prefs.getSmallestVisibleFontSize())
    {
      return; // BAIL
    }

    // Draw the label
    Point2D nodeCenter = new Point2D.Double(nodeBounds.getCenterX(),
                                            nodeBounds.getCenterY());

    if (text != null) {
      DrawnText drawnText =
        _renderer.drawText(graphics, drawProps, text, nodeCenter);
      TextScreenData tdata = drawnText.getDrawnTextScreenData();
      HitTester tester =
        new HitTester(tdata.getBounds(), DECORATION_NODE_TEXT, tdata);
      hit.addHitTester(node, tester);
      drawnList.add(drawnText);

      if (editMe) {
        _editor.updateTextScreenData(tdata);

        // draw selection
        if (_editor.hasSelection()) {
          Shape sel = tdata.getTextLayout().getLogicalHighlightShape(_editor.getSelectionStartIndex(), _editor.getSelectionStopIndex());
          _renderer.drawTextSelection(graphics,
                                      tdata.getBounds(),
                                      tdata.getTextLayout(),
                                      sel,
                                      _prefs.getSelectedTextColor());
        }


        // draw cursor
        Shape caret = tdata.getCaretShape(_editor.getCaretIndex());
        _renderer.drawCaret(graphics,
                            tdata.getBounds(),
                            tdata.getTextLayout(),
                            caret,
                            _prefs.getCaretColor());
      }
    }

    drawNodeIcons(graphics, node, viewport, nodeBounds, hit);
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

    CollapsedEdge collapsedEdge = CollapsedEdge.getCollapsedEdge(edge);

    hit.removeAllHitTesters(edge);

    if (!edge.isVisible()) {
      return;
    }

    DrawProps drawProps = getEdgeViewportDrawProps(edge, viewport);

    Rectangle2D headNodeWorldBounds = ctx.getBounds(edge.getHead());
    Rectangle2D tailNodeWorldBounds = ctx.getBounds(edge.getTail());

    // Find closest canonical connection point, N, E, S, or W, etc.:
    EdgeGeometry egWorld = new EdgeGeometry(edge,
                                            true,
                                            headNodeWorldBounds,
                                            tailNodeWorldBounds,
                                            _prefs.getArrowHeadLength(),
                                            _prefs.getArrowHeadFlair(),
                                            _prefs.getArrowHeadOffset(),
                                            false);

    EdgeGeometry egViewport = egWorld.mapWorldToViewport(_graphPanel);

    Point2D[] points = egViewport.getEdgePoints();


    // Draw the main part of the Edge
    DrawnShape drawnShape = _renderer.drawPolyline(graphics,
                                                   drawProps,
                                                   egViewport.getEdgePoints());
    Shape path = drawnShape.getDrawnShape();
    HitTester tester = new HitTester(path, DECORATION_EDGE, edge);
    hit.addHitTester(edge, tester);
    drawnList.add(drawnShape);


    // Draw the arrow
    //
    // For the purposes of hit testing, an edge arrow is as good as an
    // edge.

    boolean drawnHT = false;
    boolean drawnTH = false;
    for (int i = 0; i < collapsedEdge.getCardinality(); i++) {
      if (collapsedEdge.isEdgeVisible(i)) {
        if (collapsedEdge.isEdgeTailToHeadDirection(i) && (! drawnTH)) {
          path = drawEdgeArrow(graphics,
                               edge,
                               egViewport.getArrowHeadRightPts(),
                               getEdgeViewportDrawProps(edge, viewport));
          drawnTH = true;
          tester = new HitTester(path, DECORATION_EDGE, edge);
          hit.addHitTester(edge, tester);
        }

        if (collapsedEdge.isEdgeHeadToTailDirection(i) && (! drawnHT)) {
          path = drawEdgeArrow(graphics,
                               edge,
                               egViewport.getArrowHeadLeftPts(),
                               getEdgeViewportDrawProps(edge, viewport));
          drawnHT = true;
          tester = new HitTester(path, DECORATION_EDGE, edge);
          hit.addHitTester(edge, tester);
        }
      }
      if (drawnHT && drawnTH) {
        break;
      }
    }

    // Draw the edge bubble
    drawEdgeBubble(graphics, edge, egViewport.getBubblePoint(), viewport, hit);
  }


  /**
   * Erase the drawn element in the given graphics context. Currently,
   * this method simply clears the bounds of of the graph element and
   * redraws all intersecting elements.
   */
  private void erase(Graphics2D graphics,
                     DrawableNode node,
                     Viewport viewport,
                     HitTesterMap hit,
                     DrawableGraphContext ctx)
  {
    System.err.println("erasing node");

    Rectangle2D nodeBounds = calcDrawnNodeWorldBounds(node,
                                                      viewport.getScale(),
                                                      ctx);

    wipeAndRedrawEffected
      (graphics,
       viewport.mapWorldToViewport(nodeBounds),
       node,
       viewport,
       hit,
       ctx);
  }


  /*
   * HACK: account for the node icons since layout didn't and add
   * a little fudge to the laid out bounds
   */
  private Rectangle2D calcDrawnNodeWorldBounds(DrawableNode node,
                                               double scale,
                                               DrawableGraphContext ctx)
  {
    double fudge = NODE_BOUNDS_FUDGE * scale;

    Rectangle2D nodeWorldBounds = ctx.getBounds(node);

    //
    // HACK: account for the node icons since layout didn't
    //
    double nodeHeightWithIcon = nodeWorldBounds.getHeight();
    TypeInfo nodeTypeInfo = TypeInfo.getTypeInfo(node);
    if (nodeTypeInfo.getTypeCount() > 0) {
      nodeHeightWithIcon += (_prefs.getIconHeight()) * scale;
    }

    return (new Rectangle2D.Double(nodeWorldBounds.getX(),
                                   nodeWorldBounds.getY(),
                                   nodeWorldBounds.getWidth() + fudge,
                                   nodeHeightWithIcon + fudge));
  }


  /**
   * Erase the drawn element in the given graphics context. Currently,
   * this method simply clears the bounds of of the graph element and
   * redraws all intersecting elements.
   */
  private void erase(Graphics2D graphics,
                     DrawableEdge edge,
                     Viewport viewport,
                     HitTesterMap hit,
                     DrawableGraphContext ctx)
  {
    System.err.println("erasing edge");

    //
    // calculate rectangle to clear
    //
    Rectangle2D headNodeWorldBounds =
      calcDrawnNodeWorldBounds(edge.getHead(),
                               viewport.getScale(),
                               ctx);
    Rectangle2D tailNodeWorldBounds =
      calcDrawnNodeWorldBounds(edge.getTail(),
                               viewport.getScale(),
                               ctx);

    Rectangle2D headNodeViewportBounds =
      viewport.mapWorldToViewport(headNodeWorldBounds);
    Rectangle2D tailNodeViewportBounds =
      viewport.mapWorldToViewport(tailNodeWorldBounds);

    // create the union of the two rectangles...
    headNodeViewportBounds.add(tailNodeViewportBounds);

    wipeAndRedrawEffected(graphics, headNodeViewportBounds, edge, viewport, hit, ctx);
  }



  /*
   * Wipe out the given rectangular area, check to see what
   * was erased, and then draw those things that got erased back.
   *
   * Do not draw the element specified by the "me" parameter.
   *
   */
  private void wipeAndRedrawEffected(Graphics2D graphics,
                                     Rectangle2D clearRect,
                                     DrawableGraphElement me,
                                     Viewport viewport,
                                     HitTesterMap hit,
                                     DrawableGraphContext ctx)
  {
    graphics.clearRect((int)clearRect.getX(), (int)clearRect.getY(),
                       (int)clearRect.getWidth(), (int)clearRect.getHeight());

    DrawableGraph graph = me.getGraph();
    Set intersectingElements = graph.getIntersectingElements(hit, clearRect);

    Set nodes = new HashSet();
    Set edges = new HashSet();

    Iterator itr = intersectingElements.iterator();
    while (itr.hasNext()) {
      Object element = itr.next();
      if (element != me) {
        if (element instanceof DrawableNode) {
          nodes.add(element);
        }
        else if (element instanceof DrawableEdge) {
          DrawableEdge e = (DrawableEdge)element;
          edges.add(e);
        }
      }
    }

    itr = edges.iterator();
    while (itr.hasNext()) {
      DrawableEdge e = (DrawableEdge)itr.next();
      List drawnList = (List)_drawnObjects.get(e);
      Iterator itr2 = drawnList.iterator();
      while (itr2.hasNext()) {
        ((DrawnObject)itr2.next()).redraw(graphics);
      }
    }

    itr = nodes.iterator();
    while (itr.hasNext()) {
      DrawableNode n = (DrawableNode)itr.next();
      List drawnList = (List)_drawnObjects.get(n);
      Iterator itr2 = drawnList.iterator();
      while (itr2.hasNext()) {
        ((DrawnObject)itr2.next()).redraw(graphics);
      }
    }
  }


  /**
   * Decorate the node with the little icons.
   *
   * @param node that may contain zero or more type values
   * @param viewport the Viewport that maps our coordinate system to the
   *  screen coordinate
   * @param nodeBounds the node bounding box in viewport coordinates
   */
  private void drawNodeIcons(Graphics2D g, DrawableNode node, Viewport viewport,
                             Rectangle2D nodeBounds, HitTesterMap hit)
  {
    final double iconSpacing =
      viewport.mapWorldToViewport(_prefs.getInterIconSpacing());
    final double iconWidth =
      viewport.mapWorldToViewport(_prefs.getIconWidth());

    //
    // All icons are in a horizontal line at 'y'
    //
    // We position the center of the icons 8pts
    // below the lower edge of the node.
    //
    final double y = nodeBounds.getMaxY() +
      viewport.mapWorldToViewport(NODE_TYPE_ICON_VERTICAL_OFFSET);

    //
    // We place the icons around the center 'X' coordinate.  So
    // if there are four icons, they are placed like this:
    //
    //    /-------------\
    //    |  some node  |
    //    \-3--1---2--4-/
    //
    // Some other routine should have already made sure that the
    // node is wide enough for this to work.
    //
    double x = nodeBounds.getCenterX();
    double mult = -1.0;
    double dx = (mult) * ((iconWidth / 2.0) + iconSpacing);

    TypeInfo nodeTypeInfo = TypeInfo.getTypeInfo(node);
    for (int i = 0; i < nodeTypeInfo.getTypeCount(); i++) {
      x += dx;
      if (nodeTypeInfo.isTypeVisible(i)) {
        String ntype = nodeTypeInfo.getType(i);

        Rectangle2D rect = drawNodeIcon(g, node, viewport, ntype,
                                        new Point2D.Double(x, y));
        if (rect != null) {
          // Add the icon to the hit context list
          hit.addHitTester(node,
                           new HitTester(rect, DECORATION_NODE_ICON, node));
        }
      }

      mult = (-1.0 * mult);
      dx = ((mult) * ( ((i + 1.0) * iconWidth) + ((i + 2.0) * iconSpacing)));
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
  private Rectangle2D drawNodeIcon(Graphics2D g, DrawableNode node,
                                   Viewport viewport,
                                   String ntype, Point2D pt)
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
        // FIX: When Ken has finished mock up.
        //
        Point2D imgTopLeft =
          new Point2D.Double(pt.getX() - (ico.getCenterX() * factor),
                             pt.getY() - (ico.getCenterY() * factor));
        DrawnImage drawnImage =
          _renderer.drawImage(g, ico.getImage(), imgTopLeft, factor);

        List drawnList = getDrawnObjectsForGraphElement(node);
        drawnList.add(drawnImage);

        return (drawnImage.getDrawnImageScreenData().getBounds());
      }
    }
    return(null);
  }


  private Shape drawEdgeArrow(Graphics2D graphics,
                              DrawableEdge edge,
                              Point2D points[],
                              DrawProps drawProps)
  {
    DrawnShape drawnShape = _renderer.drawPolygon(graphics, drawProps, points);

    List drawnList = getDrawnObjectsForGraphElement(edge);
    drawnList.add(drawnShape);

    return (drawnShape.getDrawnShape());
  }




  private void drawEdgeBubble(Graphics2D graphics,
                              DrawableEdge edge,
                              Point2D bubblePoint,
                              Viewport viewport,
                              HitTesterMap hit)
  {
    List drawnList = getDrawnObjectsForGraphElement(edge);

    CollapsedEdge collapsedEdge = CollapsedEdge.getCollapsedEdge(edge);
    DrawProps drawProps = getEdgeBubbleViewportDrawProps(edge, viewport);

    double bubbleWidth =
      viewport.mapWorldToViewport(_prefs.getEdgeBubbleWidth());

    double bubbleHeight =
      viewport.mapWorldToViewport(_prefs.getEdgeBubbleHeight());

    Rectangle2D bubbleBounds =
      new Rectangle2D.Double(bubblePoint.getX() - (bubbleWidth / 2),
                             bubblePoint.getY() - (bubbleHeight / 2),
                             bubbleWidth,
                             bubbleHeight);

    // Draw the bubble
    DrawnShape drawnShape = _renderer.drawEllipse(graphics,
                                                  drawProps,
                                                  bubbleBounds);
    Shape path = drawnShape.getDrawnShape();
    HitTester ht = new HitTester(path, DECORATION_EDGE_BUBBLE, edge);
    hit.addHitTester(edge, ht);
                drawnList.add(drawnShape);


    // Don't bother drawing text with tiny fonts
    if (drawProps.getFontSize() < _prefs.getSmallestVisibleFontSize())
    {
      return; // BAIL
    }


    DrawnText drawnText = _renderer.drawText(graphics,
                                             drawProps,
                                             String.valueOf(collapsedEdge.getVisibleEdgeCount()),
                                             bubblePoint);
    drawnList.add(drawnText);

    // FIX: Need to use something from drawText for hit testing!
  }



  /*
   * Figure out the preferred size of a node.
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
  private Size calcNodeWorldSize(Graphics2D graphics,
                                 DrawableNode node,
                                 String nodeLabel,
                                 double scale)
  {
    //
    // First calculate the width required to hold the text:
    //

    DrawProps drawProps =
      mapWorldToViewport(getNodeWorldDrawProps(node), scale);

    double nodeHeight =_prefs.getNodeHeight() * scale;

    String text = nodeLabel;
    Rectangle2D textBounds = _renderer.getTextBounds(graphics,
                                                     drawProps,
                                                     text);

    double textWidth = textBounds.getWidth();

    double nodeWidthForText = textWidth + (2 * nodeHeight);

    //
    // Then calculate the node width and height required for the icons:
    //
    TypeInfo nodeTypeInfo = TypeInfo.getTypeInfo(node);
    int iconCount = nodeTypeInfo.getTypeCount();

    // HMM: Do I need to do a manual zoom calculation here?
    double iconWidth = _prefs.getIconWidth() * scale;
    double interIconSpacing = _prefs.getInterIconSpacing() * scale;
    double iconEdgeSpacing = _prefs.getIconEdgeSpacing() * scale;

    double nodeWidthForIcons =
      (iconCount * iconWidth) +
      ((iconCount - 1) * interIconSpacing) +
      ((iconCount == 0) ? 0.0 : (2 * iconEdgeSpacing));


    //
    // Finally, calculate node width with icons
    //
    double nodeWidth =
      (nodeWidthForIcons > nodeWidthForText) ? nodeWidthForIcons
      : nodeWidthForText;

    //
    // Perform a map from viewport --> world, and return in world
    // coordinates.
    //
    Size dim = new Size(nodeWidth  / scale, nodeHeight / scale);
    return(dim);
  }


  /*
   * Figure out the preferred size of a node based on the icons and
   * the ellipsified node label.
   *
   *
   * @return bounds in WORLD co-ordinates.
   */
  private Size calcNodeWorldSize(Graphics2D graphics,
                                 DrawableNode node,
                                 double scale)
  {
    return(calcNodeWorldSize(graphics, node, StringUtils.ellipsify(GreenPillUtil.getNodeLabel(node), _prefs.getMaxLabelLength()), scale));
  }



  private DrawProps getEdgeViewportDrawProps(DrawableEdge edge,
                                             Viewport viewport)
  {
    return(getEdgeViewportDrawProps(edge, false, viewport));
  }


  /**
   * Just like getEdgeViewportDrawProps (whatever that does!), execpt
   * that this version changes the draw props color to be whatever
   * is specified as the edge select color from the graph metrics
   * object.
   *
   * @param edge the edge
   * @param selected if true, get draw props for a "selected" edge.
   */
  private DrawProps getEdgeViewportDrawProps(DrawableEdge edge,
                                             boolean selected,
                                             Viewport viewport)
  {
    DrawProps result = getEdgeWorldDrawProps(edge);

    // Transform sizeal attributes
    result.setStrokeWidth(
      viewport.mapWorldToViewport(result.getStrokeWidth()));
    result.setFontSize(
      viewport.mapWorldToViewport(result.getFontSize()));

    if (selected) {
      result.setFillColor(_prefs.getSelectedEdgeArrowFillColor());
      result.setStrokeColor(_prefs.getSelectedEdgeArrowStrokeColor());
    }

    return result;
  }


  /*
   * Is this edge connected to the root node?
   */
  private boolean isConnectedToRoot(DrawableEdge e) {
    return(GreenPillUtil.isRootNode(e.getHead()) ||
                                         GreenPillUtil.isRootNode(e.getTail()));
  }


  /*
   *
   */
  private DrawProps getEdgeWorldDrawProps(DrawableEdge edge)
  {
    return(_prefs.getEdgeDrawProps(!isConnectedToRoot(edge),
                                   GreenPillUtil.isEdgeSelected(edge)));
  }


  /*
   *
   */
  private DrawProps getEdgeBubbleWorldDrawProps(DrawableEdge edge)
  {
    return(_prefs.getEdgeBubbleDrawProps(!isConnectedToRoot(edge)));
  }


  /*
   * Create new DrawProps based on the passed in, except that the
   * new one is mapped into viewport coords.
   */
  private DrawProps mapWorldToViewport(DrawProps props, double scale) {

    DrawProps result = new DrawProps(props);

    result.setStrokeWidth(result.getStrokeWidth() * scale);
    result.setFontSize(result.getFontSize() * scale);

    return result;
  }


  /*
   *
   */
  private DrawProps getEdgeBubbleViewportDrawProps(DrawableEdge edge,
                                                   Viewport viewport)
  {
    return(mapWorldToViewport(getEdgeBubbleWorldDrawProps(edge), viewport.getScale()));
  }



  /*
   *
   */
  private DrawProps getNodeViewportDrawProps(DrawableNode node,
                                             Viewport viewport)
  {
    return(mapWorldToViewport(getNodeWorldDrawProps(node), viewport.getScale()));
  }


  /*
   *
   */
  private DrawProps getNodeWorldDrawProps(DrawableNode node)
  {
    DrawProps result;

    if (GreenPillUtil.isNodeSelected(node))
    {
      if (GreenPillUtil.isRootNode(node))
      {
        result = _prefs.getSelectedRootNodeDrawProps();
      }
      else
      {
        result = _prefs.getSelectedNodeDrawProps();
      }
    }
    else if (GreenPillUtil.isRootNode(node))
    {
      result = _prefs.getRootNodeDrawProps();
    }
    else
    {
      //
      // is the node a neighbor of the root node?
      //
      result =
        _prefs.getNodeDrawProps(GreenPillUtil.getDistanceFromRoot(node) != 1);
    }

    return result;
  }

} // end class GreenPillUI
