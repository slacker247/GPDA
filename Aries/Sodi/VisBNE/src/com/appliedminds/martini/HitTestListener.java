package com.appliedminds.martini;


import java.awt.event.MouseEvent;
import java.awt.geom.Rectangle2D;


/**
 * The interface between an object that can perform hit testing and an
 * object that wants to know when a hit test has succeeded.  This is
 * used between the GraphPanel and the DrawableGraph.
 *
 * <p>This is a low-level interface designed to be used by the martini
 * framework.  Client code would not normally need to use this
 * interface.  Generally, client code will be more interedted in
 * DrawableGraphMouseEvents which are transmitted through the
 * DrawableGraphMouseEventListener interface.
 *
 *
 * @see com.appliedminds.martini.HitTester
 *
 *
 * @author mathias@apmindsf.com
 * @author will@apmindsf.com
 */
public interface HitTestListener {

  /** 
   * Do something interesting when a hit test has succeeded (that
   * means that a UI object has been hit).  Then return a value
   * idicating whether the hit test event was consumed by the
   * application logic.
   *
   * @param decorationId the UI decoration identifier.
   * @param el a graph element (could be null).
   * @param ctx some context object passed from the UI (could be null).
   * @param evt the raw awt event.
   *
   * @return TRUE if the hit is consumed by any of the potential
   * listeners.
   */
  boolean hitTestSuccess(String decorationId, 
                         DrawableGraphElement el, 
                         Object ctx, 
                         MouseEvent evt);



  /** 
   * Called when a hit-test determines that a hit has missed the
   * entire graph.
   *
   * @param evt the raw awt event.
   */
  void hitTestMissedGraph(MouseEvent evt);


  void multipleHitTestSuccess(Object[] hitElements, Rectangle2D rect);


  void multipleHitTestMissedGraph(Rectangle2D rect);
}
