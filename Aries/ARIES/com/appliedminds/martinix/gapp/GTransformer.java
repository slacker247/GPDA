package com.appliedminds.martinix.gapp;


import com.appliedminds.martini.DrawableGraph;


/**
 * Transforms a graph into another graph.
 *
 *
 * <p>Note that <code>transform(transform(graph)) == graph</code>.
 *
 *
 *
 * @author mathias@apmindsf.com
 */
public interface GTransformer {

  /**
   * Transform the given graph.  Remember that
   * <code>transform(transform(graph)) == graph</code>.  This is 
   * a reversable operation.
   *
   *
   * @param g the graph to transform (This graph is MODIFIED!).
   * @return the modified graph (same as the input)
   *
   * @throws MartiniError if a required graph property is missing.
   */
  DrawableGraph transform(DrawableGraph g);
    
}


