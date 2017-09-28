package com.appliedminds.martinix.gapp;

import com.appliedminds.martini.*;
import com.appliedminds.martini.io.LayoutLoader;
import java.awt.geom.Rectangle2D;


/**
 * An implementation of the LayoutLoader interface. It uses a brand
 * new DrawableGraphContext to store the bounds of the graph
 * elements.
 *
 * @author daepark@apmindsf.com
 */
public class GLayoutLoader implements LayoutLoader {

  private DrawableGraphContext _ctx;
  private Rectangle2D _graphBounds;


  /**
   * @param graphPanel The GraphPanel object that will responsible for
   * displaying the saved graph.
   */
  public GLayoutLoader() {
    _ctx = new DrawableGraphContext();
    _graphBounds = null;
  }

  //
  // begin LayoutLoader interface
  //

  public void loadGraphBounds(DrawableGraph graph, Rectangle2D bounds) {
    if (_graphBounds == null) {
      _graphBounds = new Rectangle2D.Double();
    }
    _graphBounds.setRect(bounds.getX(), bounds.getY(),
                         bounds.getWidth(), bounds.getHeight());
  }

  public void loadNodeBounds(DrawableNode node, Rectangle2D bounds) {
    _ctx.setBounds(node, bounds);
    _ctx.setNeedsRepaint(node, true);
  }

  public void loadEdgeBounds(DrawableEdge edge, Rectangle2D bounds) {
    _ctx.setNeedsRepaint(edge, true);
  }

  public Rectangle2D getGraphBounds() {
    return (_graphBounds);
  }

  public Rectangle2D getBounds(DrawableNode node) {
    return (_ctx.getBounds(node));
  }

  public Rectangle2D getBounds(DrawableEdge edge) {
    return (_ctx.getBounds(edge));
  }

  //
  // end LayoutLoader interface
  //

} // end class "GLayoutLoader"
