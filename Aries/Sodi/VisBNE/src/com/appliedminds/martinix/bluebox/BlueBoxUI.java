package com.appliedminds.martinix.bluebox;

import com.appliedminds.martini.*;
import com.appliedminds.martinix.ui.ArrowHead;
import com.appliedminds.martinix.ui.BorderEdgeAttachStrategy;
import com.appliedminds.martinix.ui.DefaultBorderEdgeAttachStrategy;
import com.appliedminds.martinix.ui.FontCache;
import com.appliedminds.martinix.ui.TextScreenData;
import com.appliedminds.martinix.ui.StringUtils;
import java.awt.AlphaComposite;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.Paint;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.Stroke;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.font.FontRenderContext;
import java.awt.font.TextLayout;
import java.awt.geom.AffineTransform;
import java.awt.geom.Ellipse2D;
import java.awt.geom.GeneralPath;
import java.awt.geom.PathIterator;
import java.awt.geom.Point2D;
import java.awt.geom.QuadCurve2D;
import java.awt.geom.Rectangle2D;
import java.awt.geom.RoundRectangle2D;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;



/**
 *
 * <p>Thie GraphUI uses the following graph properties:
 *
 * <ul>
 *
 * <li> "label" - on Nodes - This is the text displayed in the node.
 * We take care of adding ellipses.
 *
 * <li> "byref" - on Edges - Can be true, false (not present is same
 * as false). This determines if we use a dashed line (when
 * byref=false) or a solid line for an edge.
 *
 * <li> "ntype" - on Nodes - Can be "single", "group", or "collapsed".
 * Each type is displayed in a different color.
 *
 * <li> "groupid" - on Nodes - an identifier for grouping.  All nodes
 * with the same groupid value are grouped together in the display.
 * <b>NOT YET IMPLEMENTED</b>.
 *
 * <li> "icon" - on Nodes or Edges - a symbolic name for an icon.
 * Valid default values are: "page", "site", or "search".  However,
 * any string that is present in the typemap.xml file will work.  The
 * location of the typemap.xml file is specfied by the
 * BlueBoxUIPrefs.getTypeMapURL() method.
 *
 * <li> "querystr" - on Edges - if this is present and non-empty, then
 * we will decorate the edge with a little icon (by default that is a
 * magnifying glass).  The icon identifier is set by the
 * BlueBoxUIPrefs.getSearchIconId() method (and this identifier must
 * map to an icon path in the typemap.xml file -- see notes for the
 * "icon" property above).
 *
 * <li> "pagecount" - on Nodes - the number of pages represented by
 * this node.  This must be an integer if it is present.
 *
 * </ul>
 *
 * <p>This GraphUI genereates events for the following UI Decoration
 * identifiers:
 *
 * <ul>
 * <li>NODE (returns a DrawableNode)
 * <li>NODE_TEXT (returns a martinix.ui.TextScreenData)
 * <li>EDGE (returns a DrawableEdge)
 * </ul>
 *
 * @author mathias@apmindsf.com
 */
public class BlueBoxUI implements GraphUI {

  // My event decoration id's
  private static final String DECORATION_NODE = "NODE";
  private static final String DECORATION_NODE_TEXT = "NODE_TEXT";
  private static final String DECORATION_EDGE = "EDGE";


  // Arcs for the rounded rectangles
  private static final double NODE_ARC_WIDTH = 2.0;
  private static final double NODE_ARC_HEIGHT = 2.0;
  private static final double EDGE_STROKE_WIDTH = 1.0;


  // Graph properties and values
  private static final String PROP_BYREF = "byref";
  private static final String VAL_BYREF_TRUE = "true";

  private static final String PROP_LABEL = "label";

  static final String PROP_NTYPE = "ntype";
  static final String VAL_NTYPE_GROUP = "group";
  static final String VAL_NTYPE_SINGLE = "single";
  static final String VAL_NTYPE_COLLAPSED = "collapsed";

  private static final String PROP_ICON = "icon";

  private static final String PROP_QUERYSTRING = "querystr";

  static final String PROP_VISIBLE = "visible";
  static final String PROP_SEQNUM = "seqnum";

  private static final String PROP_PAGE_COUNT = "pagecount";


  // Misc constants
  private static final String ELLIPSES = "...";

  private static final double FONT_WIDTH_HACK = 4.0;


  private GraphPanel _graphPanel;
  private BlueBoxUIPrefs _prefs;
  private FontCache _fontCache;
  private IconLoader _iconLoader;

  private BorderEdgeAttachStrategy _attachStrategy;

  /**
   * @throws MartinError if there is an initialization error.
   */
  public BlueBoxUI() {
    this(new BlueBoxUIPrefs());
  }


  /**
   * @throws MartiniError if there is an initialization error.
   */
  public BlueBoxUI(BlueBoxUIPrefs prefs) {
    _prefs = prefs;
    _fontCache = new FontCache(_prefs.getNodeFontResourcePath());
    try {
      _iconLoader = IconLoader.create(_prefs.getTypeMapURL());
    }
    catch(IOException e) {
      throw(new MartiniError("BlueBox failed to init icon type map.  Check settings for getTypeMapURL.  Error = " + e));
    }

    _attachStrategy = new DefaultBorderEdgeAttachStrategy();
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


  /*
   *   ==============
   *  ================
   * ==================
   *    Draw an Edge
   * ==================
   *  ================
   *   ==============
   */
  public void draw(Graphics2D graphics,
                   DrawableEdge edge,
                   Viewport viewport,
                   HitTesterMap hit,
                   DrawableGraphContext ctx,
                   boolean newBuffer,
                   boolean erase)
  {
    if ((edge.getProperty(PROP_VISIBLE) != null) &&
        edge.getProperty(PROP_VISIBLE).equals("false")) {
      return;
    }

    if (newBuffer || ctx.getNeedsRepaint(edge) || edge.hasChanged()) {
      hit.removeAllHitTesters(edge);
      double attachCircleRadius = _prefs.getEdgeAttachCircleRadius();

      Rectangle2D head = ctx.getBounds(edge.getHead());
      Rectangle2D bigHead =
        new Rectangle2D.Double(head.getX() - attachCircleRadius,
                               head.getY() + attachCircleRadius,
                               head.getWidth() + (2.0 * attachCircleRadius),
                               head.getHeight() + (2.0 * attachCircleRadius));

      Rectangle2D tail = ctx.getBounds(edge.getTail());
      Rectangle2D bigTail =
        new Rectangle2D.Double(tail.getX() - attachCircleRadius,
                               tail.getY() + attachCircleRadius,
                               tail.getWidth() + (2.0 * attachCircleRadius),
                               tail.getHeight() + (2.0 * attachCircleRadius));

      Point2D headPtInWorld =
        _attachStrategy.findAttachPoint
        (bigHead,
         new Point2D.Double(tail.getX(),
                            tail.getY()),
         _attachStrategy.RELATIVE);

      Point2D tailPtInWorld =
        _attachStrategy.findAttachPoint
        (bigTail,
         new Point2D.Double(head.getX(),
                            head.getY()),
         _attachStrategy.RELATIVE);

      Point2D headPtInViewport = viewport.mapWorldToViewport(headPtInWorld);
      Point2D tailPtInViewport = viewport.mapWorldToViewport(tailPtInWorld);


      // Figure out if we are doing a dashed line or not
      graphics.setPaint(_prefs.getEdgeColor());
      if (useDashedLine(edge)) {
        float dashValue =
          (float) viewport.mapWorldToViewport(_prefs.getEdgeDashSpacing());
        Stroke dash =
          new BasicStroke
          ((float) viewport.mapWorldToViewport(_prefs.getEdgeStrokeWidth()),
           BasicStroke.CAP_BUTT,
           BasicStroke.JOIN_MITER,
           (dashValue < 1.0f) ? 1.0f : dashValue ,
           new float[]{ dashValue },
           0.0f);

        graphics.setStroke(dash);
      }
      else {
        graphics.setStroke
          (new BasicStroke
           ((float) viewport.mapWorldToViewport(_prefs.getEdgeStrokeWidth())));
      }

      // Do the calculations for the curved line, if this returns NULL
      // then we should draw a straight line.
      Point2D controlPt = figureControlPoint(tailPtInViewport, headPtInViewport);

      //
      // Draw the edge either straight or curved.
      //
      if (controlPt == null) {
        /* Straight */
        GeneralPath gp = new GeneralPath();
        gp.moveTo((float) tailPtInViewport.getX(),
                  (float) tailPtInViewport.getY());
//         Point2D[] pts = edge.getIntermediateCoordinates();
//         for(int i=pts.length - 1; i >= 0; --i) {
//           Point2D vpt = viewport.mapWorldToViewport(pts[i]);
//           gp.lineTo((float) vpt.getX(), (float) vpt.getY());
//         }
        gp.lineTo((float) headPtInViewport.getX(),
                  (float) headPtInViewport.getY());
        gp.closePath();
        graphics.draw(gp);

        hit.addHitTester(edge, new HitTester(gp, DECORATION_EDGE, edge));
      }
      else {
        /* Curved */
        QuadCurve2D quad = new QuadCurve2D.Double();
        quad.setCurve(tailPtInViewport, controlPt, headPtInViewport);
        graphics.draw(quad);
        hit.addHitTester(edge, new HitTester(quad, DECORATION_EDGE, edge));
      }


      // Drawn the edge arrowhead
      //
      // The first point passed to ArrowHead is the tip of arrow head.
      // The second point is ANY point on a line shared with the tip
      // point around which the arrowhead should be symmetrical.
      //
      // If we are drawing a curved line (when controlPt != NULL) we
      // pass in the control point as that second point, otherwise we
      // just pass in the tail point.  We are assuming that if we are
      // not drawing a curved line, then the edge is a straight line
      // from tail to head.
      //
      ArrowHead arrow =
        new ArrowHead(headPtInWorld,
                      ((controlPt == null) ? tailPtInWorld
                       : viewport.mapViewportToWorld(controlPt)),
                      _prefs.getEdgeArrowHeadLength(),
                      _prefs.getEdgeArrowHeadFlair());

      GeneralPath path = new GeneralPath();
      for(int i=0; i < arrow.getPointCount(); ++i) {
        Point2D pt = viewport.mapWorldToViewport(arrow.getPoint(i));
        if (i == 0) {
          path.moveTo((float) pt.getX(), (float) pt.getY());
        }
        else {
          path.lineTo((float) pt.getX(), (float) pt.getY());
        }
      }
      path.closePath();
      graphics.setStroke
        (new BasicStroke((float) viewport.mapWorldToViewport
                         (_prefs.getUIStrokeWidth())));
      graphics.fill(path);

      // store the arrow head as part of the edge for hit testing
      hit.addHitTester(edge, new HitTester(path, DECORATION_EDGE, edge));


      //
      // Slap on an icon if necessary
      //
      if (edgeNeedsIcon(edge)) {
        MartiniIcon icon = getEdgeIcon();
        if (icon != null) {
          double factor =
            icon.getScaleFactor
            (viewport.mapWorldToViewport(_prefs.getEdgeIconHeight()));

          // iconPt will hold the center point for the icon (at first)
          Point2D iconPt = null;
          if (controlPt == null) {
            /* No control point so just choose the half way point */
            iconPt = new Point2D.Double((tailPtInViewport.getX() +
                                         headPtInViewport.getX()) / 2.0,
                                        (tailPtInViewport.getY() +
                                         headPtInViewport.getY()) / 2.0);
          }
          else {
            //
            // Do some black math hand waving to get a "pretty good"
            // place for the icon:
            //
            //
            //         T       C
            //                /
            //               P
            //              / \
            //                 1/2
            //
            //                 H
            //
            //
            //   H = head pt
            //   T = tail pt
            //   C = control pt
            //   P = icon point
            //
            // The icon point, P, is choosen to be half way down the
            // line between the control point, C, and the point at x =
            // 1/2(TC), and y = 1/2(CH).
            //
            iconPt = new Point2D.Double(tailPtInViewport.getX() + (0.75 * (controlPt.getX() - tailPtInViewport.getX())), controlPt.getY() + (0.25 * (headPtInViewport.getY() - controlPt.getY())));
          }

          // Adjust iconPt so that it sits at the top-left of the icon.
          iconPt.setLocation
            (iconPt.getX() - ((icon.getWidth() * factor) / 2.0),
             iconPt.getY() - ((icon.getHeight() * factor) / 2.0));

          Rectangle2D ibounds = drawImage(graphics,
                                          icon.getImage(),
                                          iconPt,
                                          factor);
          hit.addHitTester(edge,
                           new HitTester(ibounds, DECORATION_EDGE, edge));
        }
      }

    }// end if "needs to be drawn"
  }


  private static final void dump(Graphics2D g, String desc, double[] co, int n) {
    StringBuffer buf = new StringBuffer();
    buf.append(desc).append(":\n");
    for(int i=0; i < n; ++i) {
      buf.append("     (").append(co[i*2]).append(", ").append(co[(i*2) + 1]);
      buf.append(")\n");

      g.setPaint(Color.red);
      int x = (int) co[i*2];
      int y = (int) co[(i*2) + 1];
      g.drawLine(x - 4, y - 4, x + 4, y + 4);
      g.drawLine(x - 4, y + 4, x + 4, y - 4);
    }
    System.err.println(buf.toString());
  }


  /*
   *   ==============
   *  ================
   * ==================
   *    Draw a Node
   * ==================
   *  ================
   *   ==============
   */
  public void draw(Graphics2D graphics,
                   DrawableNode node,
                   Viewport viewport,
                   HitTesterMap hit,
                   DrawableGraphContext ctx,
                   boolean newBuffer,
                   boolean erase)
  {

    if ((node.getProperty(PROP_VISIBLE) != null) &&
        node.getProperty(PROP_VISIBLE).equals("false")) {
      return;
    }

    if (newBuffer || ctx.getNeedsRepaint(node) || node.hasChanged()) {

      hit.removeAllHitTesters(node);

      Stroke stroke =
        new BasicStroke((float) viewport.mapWorldToViewport
                        (_prefs.getUIStrokeWidth()));

      Rectangle2D nodeBounds = viewport.mapWorldToViewport(ctx.getBounds(node));

      RoundRectangle2D nodeRect =
        new RoundRectangle2D.Double
        (nodeBounds.getX(),
         nodeBounds.getY(),
         nodeBounds.getWidth(),
         nodeBounds.getHeight(),
         viewport.mapWorldToViewport(NODE_ARC_WIDTH),
         viewport.mapWorldToViewport(NODE_ARC_HEIGHT));

      // draw node box
      graphics.setPaint(getNodeFill(node));
      graphics.fill(nodeRect);
      graphics.setStroke(stroke);
      graphics.setPaint(getNodeStrokeColor(node));
      graphics.draw(nodeRect);

      // store the node rectangle for hit testing
      hit.addHitTester(node, new HitTester(nodeRect, DECORATION_NODE, node));

      // Draw the little attach point circles
      ArrayList attachPoints = new ArrayList();
      double attachRadius =
        viewport.mapWorldToViewport(_prefs.getEdgeAttachCircleRadius());
      for(NodeIterator i = node.getConnectedNodes(); i.hasNext(); ) {
        DrawableNode neighbor = i.next();
        Rectangle2D nbounds = ctx.getBounds(neighbor);
        Point2D other = new Point2D.Double(nbounds.getX(), nbounds.getY());
        Point2D aPt =
          viewport.mapWorldToViewport
          (_attachStrategy.findAttachPoint(ctx.getBounds(node), other, _attachStrategy.RELATIVE));

        if (containsPoint(attachPoints, aPt)) {
          continue;
        }

        attachPoints.add(aPt);

        Ellipse2D attachCircle =
          new Ellipse2D.Double(aPt.getX() - attachRadius,
                               aPt.getY() - attachRadius,
                               attachRadius * 2.0,
                               attachRadius * 2.0);
        graphics.setPaint(_prefs.getEdgeAttachCircleFill());
        graphics.fill(attachCircle);
        graphics.setStroke(stroke);
        graphics.setPaint(getNodeStrokeColor(node));
        graphics.draw(attachCircle);
      }
      attachPoints = null;

      // draw node icon
      MartiniIcon icon = getNodeIcon(node);
      //  ... we use this later for setting the text margin:
      Rectangle2D iconBounds = new Rectangle2D.Double(0.0, 0.0, 0.0, 0.0);
      if (icon != null) {
        double factor =
          icon.getScaleFactor
          (nodeRect.getHeight() -
           (2.0 * viewport.mapWorldToViewport(_prefs.getNodeIconMargin())));

        iconBounds = drawImage(graphics,
                               icon.getImage(),
                               (int) (nodeRect.getX() + 1),
                               (int) (nodeRect.getY() + 1),
                               factor);
        hit.addHitTester(node,
                         new HitTester(iconBounds, DECORATION_NODE, node));

        // Draw the page count pill if necessary
        if (needsPageCount(node)) {
          String countStr = Integer.toString(getPageCount(node));

          Font font = _fontCache.getFontBySize
            ((int) viewport.mapWorldToViewport(_prefs.getSiteCountFontSize()));

          TextLayout numLayout =
            new TextLayout(countStr, font, graphics.getFontRenderContext());

          Rectangle2D numBounds = numLayout.getBounds();

          // calculate the width/height for the pill-- all based on font size.
          double numPillHeight =
            numLayout.getAscent() + numLayout.getDescent() +
            (2.0 * viewport.mapWorldToViewport
             (_prefs.getSiteCountInternalTBMargin()));

          double numPillWidth = (numBounds.getWidth() - FONT_WIDTH_HACK) +
            (2.0 * viewport.mapWorldToViewport
             (_prefs.getSiteCountInternalLRMargin()));

          // Never allow the shape to be less than a circle.
          if (numPillWidth < numPillHeight) {
            numPillWidth = numPillHeight;
          }

          // calculate the top-left point for the pill
          int npx = (int) (iconBounds.getX() +
                           iconBounds.getWidth() - (numPillWidth / 2.0));

          int npy = (int) (iconBounds.getCenterY() - (numPillHeight / 2.0));

          RoundRectangle2D numPill =
            new RoundRectangle2D.Double(npx,
                                        npy,
                                        numPillWidth,
                                        numPillHeight,
                                        numPillHeight,
                                        numPillHeight);

          // Calculate where to position the font.  We want to center it
          // horizontally.
          int nfontY = (int) (numPill.getY() +
                              viewport.mapWorldToViewport(_prefs.getSiteCountInternalTBMargin()) +
                              numLayout.getAscent());
          int nfontX = npx + ((int) (0.5 * (numPillWidth - ((numBounds.getWidth() - FONT_WIDTH_HACK) + (2.0 * viewport.mapWorldToViewport(_prefs.getSiteCountInternalLRMargin()))))));


          // draw the pill
          graphics.setPaint(_prefs.getSiteCountFillColor());
          graphics.fill(numPill);
          graphics.setPaint(_prefs.getSiteCountBorderColor());
          graphics.setStroke
            (new BasicStroke((float) viewport.mapWorldToViewport
                             (_prefs.getUIStrokeWidth())));
          graphics.draw(numPill);

          // draw the font
          graphics.setPaint(_prefs.getSiteCountTextColor());
          numLayout.draw(graphics, nfontX, nfontY);
        }
      }


      // draw node text
      double leftMargin = nodeRect.getX() +
        viewport.mapWorldToViewport(_prefs.getNodeTextMargin());

      if (icon != null) {
        leftMargin += iconBounds.getWidth();
        leftMargin += viewport.mapWorldToViewport(_prefs.getNodeIconMargin());
      }

      String text = StringUtils.ellipsify(getNodeLabel(node),
                                          _prefs.getMaxNodeLabelLength());
      Font font = _fontCache.getFontBySize
        ((int) viewport.mapWorldToViewport(_prefs.getNodeFontSize()));

      TextLayout textLayout =
        new TextLayout(text, font, graphics.getFontRenderContext());

      Rectangle2D textBounds = textLayout.getBounds();

      graphics.setPaint(_prefs.getNodeFontColor());

      textLayout.draw
        (graphics,
         (float) leftMargin,
         (float)(nodeRect.getCenterY() + (textLayout.getAscent() / 2)));

      Rectangle2D drawnBounds =
        new Rectangle2D.Double
        (leftMargin,
         nodeRect.getCenterY() - (textLayout.getAscent() / 2),
         textBounds.getWidth(),
         textBounds.getHeight());

      // And store text for hit testing
      hit.addHitTester
        (node, new HitTester(drawnBounds,
                             DECORATION_NODE_TEXT,
                             new TextScreenData(textLayout, drawnBounds, text)));

    }// end if "has changed"
  }


  /**
   * <b>Not yet implemented</b>
   */
  public boolean validate(DrawableGraph g) {
    // FIX: Implement validation.
    return(true);
  }



  public boolean setupEditSession(DrawableGraphElement el,
                                  Point loc,
                                  Object ctx)
  {
    return(false);
  }


  public void teardownEditSession() { }


  public byte mouseEvent(MouseEvent e) {
    return(0x0);
  }

  public byte keyPressed(KeyEvent e) {
    return(0x0);
  }


  //
  // End GraphUI interface
  //



  /*
   * Draw an image.
   *
   * @return the bounds
   */
  private Rectangle2D drawImage(Graphics2D graphics,
                                BufferedImage img,
                                Point2D pt,
                                double scale)
  {
    return(drawImage(graphics, img, (int) pt.getX(), (int) pt.getY(), scale));
  }


  /*
   * Draw an image.
   *
   * @return the bounds
   */
  private Rectangle2D drawImage(Graphics2D graphics,
                                BufferedImage img,
                                int x,
                                int y,
                                double scale)
  {
    // Move the "pen"
    graphics.translate(x, y);

    // Compose the scaling transform
    AffineTransform trans = new AffineTransform();
    trans.setToScale(scale, scale);

    // Draw the picture
    graphics.drawRenderedImage(img, trans);

    // move the "pen" back
    graphics.translate(- x, - y);

    return (new Rectangle2D.Double(x,
                                   y,
                                   (img.getWidth() * scale),
                                   (img.getHeight() * scale)));
  }



  private Size calcNodeWorldSize(Graphics2D g, DrawableNode n, double scale) {

    //
    // Calculate the width required to hold the text:
    //
    String text = StringUtils.ellipsify(getNodeLabel(n),
                                        _prefs.getMaxNodeLabelLength());

    Font font = _fontCache.getFontBySize(_prefs.getNodeFontSize());
    FontMetrics fm = g.getFontMetrics(font);
    double textWidth = fm.stringWidth(text);

    double nodeWidth = textWidth + (2.0 * _prefs.getNodeTextMargin());

    //
    // And the width for the icon (if there is one):
    //
    // The icon has a large size so that scaling looks good.  We scale
    // the icon so that it fits with our world node height.
    //
    MartiniIcon icon = getNodeIcon(n);
    if (icon != null) {
      double factor = icon.getScaleFactor(_prefs.getNodeHeight() * scale);
      nodeWidth += icon.getWidth() * factor;
      nodeWidth += 2.0 * _prefs.getNodeIconMargin();
    }

    // This is in WORLD coordinates.
    return(new Size(nodeWidth, _prefs.getNodeHeight()));
  }



  /*
   * Return the node icon or null.
   */
  private MartiniIcon getNodeIcon(DrawableNode n) {
    String iconid = getIconId(n);
    if (iconid != null) {
      return(_iconLoader.getIconForType(iconid));
    }
    return(null);
  }


  /*
   * Return the edge decoration or null.
   */
  private MartiniIcon getEdgeIcon() {
    String id = _prefs.getSearchIconId();
    if (id != null) {
      return(_iconLoader.getIconForType(id));
    }
    return(null);
  }


  /*
   * @return true if the edge requires a icon.
   */
  private static final boolean edgeNeedsIcon(DrawableEdge e) {
    String v = e.getProperty(PROP_QUERYSTRING);
    return((v != null) && (v.length() > 0));
  }


  /**
   * Get the node label.
   */
  private static final String getNodeLabel(DrawableNode n) {
    return(n.getProperty(PROP_LABEL));
  }


  /**
   * Determine if the edge should be drawn with a dashed line.
   */
  private static final boolean useDashedLine(DrawableEdge e) {
    return(!isProperty(e, PROP_BYREF, VAL_BYREF_TRUE));
  }


  /**
   * Determine if a node is of "group" type.
   */
  private static final boolean isGroupType(DrawableNode n) {
    return(isProperty(n, PROP_NTYPE, VAL_NTYPE_GROUP));
  }


  /**
   * Determine if a node is of "collapsed" type.
   */
  private static final boolean isCollapsedType(DrawableNode n) {
    return(isProperty(n, PROP_NTYPE, VAL_NTYPE_COLLAPSED));
  }


  /**
   * Determine of a node is of "single" type.
   */
  private static final boolean isSingleType(DrawableNode n) {
    return(isProperty(n, PROP_NTYPE, VAL_NTYPE_SINGLE));
  }


  /**
   * Get the "icon" property from the element.  Could be null.
   */
  private static final String getIconId(DrawableGraphElement e) {
    return(e.getProperty(PROP_ICON));
  }


  /**
   * See if the given node has a 'page count' property.
   */
  private static final boolean needsPageCount(DrawableNode n) {
    String v = n.getProperty(PROP_PAGE_COUNT);
    return((v != null) && (v.length() > 0));
  }


  /**
   * Get the value of the "page count" property as an int.
   */
  private static final int getPageCount(DrawableNode n) {
    String v = n.getProperty(PROP_PAGE_COUNT);
    if (v != null) {
      try {
        return(Integer.parseInt(v));
      }
      catch(NumberFormatException e) {
        ;
      }
    }
    return(0);
  }


  /**
   * Get the node fill Paint based on the node type.
   * We default to SINGLE type.
   */
  private Paint getNodeFill(DrawableNode n) {
    if (isGroupType(n)) {
      return(_prefs.getGroupNodeFill());
    }
    else if (isCollapsedType(n)) {
      return(_prefs.getCollapsedNodeFill());
    }
    else {
      return(_prefs.getSingleNodeFill());
    }
  }


  /**
   * Get the node stroke Paint based on the node type.
   * We default to SINGLE type.
   */
  private Paint getNodeStrokeColor(DrawableNode n) {
    if (isGroupType(n)) {
      return(_prefs.getGroupNodeStrokeColor());
    }
    else if (isCollapsedType(n)) {
      return(_prefs.getCollapsedNodeStrokeColor());
    }
    else {
      return(_prefs.getSingleNodeStrokeColor());
    }
  }


  /*
   * Check if a property is set to a particular value (ignoreing case).
   *
   * @param e the node or edge
   * @param prop the property name
   * @param value the value to check for (ignoring case)
   * @return true if the named property has that value.
   */
  private static final boolean isProperty(DrawableGraphElement e,
                                          String prop,
                                          String value)
  {
    String v = e.getProperty(prop);
    return((v != null) && (value.equalsIgnoreCase(v)));
  }


  /*
   * Return the control point or null.  We just choose a point to make
   * the line bend a bit.  We want to make edges that head up the
   * screen bend down like a U, and edges that head down the screen
   * bend up like a ^.  If the line is vertical or horizontal with no
   * slope, then we return null.
   *
   * @param tail the tail point
   * @param head the head point
   *
   * @return the control point (for the QuadCurve object) or null if the
   * line should not bend.
   */
  private static Point2D figureControlPoint(Point2D tail, Point2D head) {
    double dx = tail.getX() - head.getX();
    double dy = tail.getY() - head.getY();
    if (dx != 0.0) {
      double m = dy / dx;
      if (m != 0) {
        if (dx < 0) {
          return(new Point2D.Double(tail.getX() + (Math.abs(dx) / 2.0), tail.getY()));
        }
        else {
          return(new Point2D.Double(tail.getX() - (Math.abs(dx) / 2.0), tail.getY()));
        }
      }
    }

    return null;
  }


  /*
   * @return true if the given list of points contains the given point.
   */
  private static boolean containsPoint(List list, Point2D pt) {
    for(Iterator i = list.iterator(); i.hasNext(); ) {
      Point2D q = (Point2D) i.next();
      if (pt.distance(q) < 0.0001) {
        return(true);
      }
    }
    return(false);
  }


} // end class BlueBoxUI
