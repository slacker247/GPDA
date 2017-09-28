/*
 * ConvertToDG.java
 *
 * Created on March 31, 2003, 2:39 PM
 */

package com.appliedminds.jam;

/**
 *
 * @author  jeffmac
 */

import com.appliedminds.martini.io.*;
import com.appliedminds.martini.DrawableEdge;
import com.appliedminds.martini.DrawableGraph;
import com.appliedminds.martini.DrawableGraphElement;
import com.appliedminds.martini.DrawableNode;
import com.appliedminds.martini.MartiniError;
import com.appliedminds.martini.MartiniException;
import java.awt.geom.Rectangle2D;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Iterator;


// Royere classes are used for now.
// FIX: Use our own parser and go directly to DrawableGraph
import royere.cwi.input.*;
import royere.cwi.structure.Graph;
import royere.cwi.structure.Element;
import royere.cwi.structure.Node;
import royere.cwi.structure.Edge;



/**
 * This class knows how to create a DrawableGraph from a Graph.
 *
 */
public class ConvertToDG {

  public static DrawableGraph parseGraph(Graph in)
    throws IOException, Exception
  {
    return (parseGraph(in, null));
  }


  public static DrawableGraph parseGraph(Graph g,
                                       LayoutLoader loader)
    throws IOException, Exception
  {
    HashMap nodeMap = new HashMap();
    if (g == null) {
      throw(new IOException("No graph parsed."));
    }
    else {
      DrawableGraph dGraph = new DrawableGraph();

      copyRoyereToDrawable(g, dGraph);

      boolean loadLayout = (loader != null && isLayoutSaved(dGraph)) ?
        true : false;

      if (loadLayout) {
        loadLayout(dGraph, loader);
      }

      Element[] elements = g.toElementArray();

      // Pass 1: add all nodes
      for(int i=0; i < elements.length; ++i) {
        if (elements[i] instanceof Node) {
          DrawableNode dNode = dGraph.createNode();
          copyRoyereToDrawable((Node) elements[i], dNode);

          if (loadLayout) {
            loadLayout(dNode, loader);
          }

          nodeMap.put(elements[i], dNode);
        }
      }

      // Pass 2: add all edges
      for(int i=0; i < elements.length; ++i) {
        if (elements[i] instanceof Edge) {
          Edge rEdge = (Edge) elements[i];
          DrawableNode head = (DrawableNode) nodeMap.get(rEdge.getTarget());
          DrawableNode tail = (DrawableNode) nodeMap.get(rEdge.getSource());
          if ((head == null) || (tail == null)) {
            throw(new MartiniError("Royere parse routine failed: got an edge but nodes are not in map."));
          }
          DrawableEdge dEdge = dGraph.createEdge(head, tail);
          copyRoyereToDrawable(rEdge, dEdge);

          if (loadLayout) {
            loadLayout(dEdge, loader);
          }
        }
      }
      return(dGraph);
    }
  }


  /*
   * Copy relevant information from a royere node to a DrawableNode.
   */
  private static void copyRoyereToDrawable(Node rn, DrawableNode dn) {
    dn.setProperty("label", rn.getLabel());
    dn.setProperty("id", rn.getId());
    HashMap props = rn.getProperties().getProperties();
    for(Iterator i = props.keySet().iterator(); i.hasNext(); ) {
      String key = (String) i.next();
      dn.setProperty(key, rn.getProperty(key).toString());
    }
  }


  /*
   * Copy relevant information from a royere Edge to a DrawableEdge.
   */
  private static void copyRoyereToDrawable(Edge re, DrawableEdge de) {
    de.setProperty("label", re.getLabel());
    HashMap props = re.getProperties().getProperties();
    for(Iterator i = props.keySet().iterator(); i.hasNext(); ) {
      String key = (String) i.next();
      de.setProperty(key, re.getProperty(key).toString());
    }
  }


  /*
   * Copy relevant information from a royere Graph to a DrawableGraph.
   */
  private static void copyRoyereToDrawable(Graph rg, DrawableGraph dg) {
    dg.setProperty("label", rg.getLabel());
    dg.setProperty("id", rg.getId());
    HashMap props = rg.getProperties().getProperties();
    for(Iterator i = props.keySet().iterator(); i.hasNext(); ) {
      String key = (String) i.next();
      dg.setProperty(key, rg.getProperty(key).toString());
    }
  }


  /**
   * Is the layout saved property set to true (i.e., is there graphics
   * data containt the layout of the graph in dg).
   */
  public static boolean isLayoutSaved(DrawableGraph dg) {
    return false;
  }


  private static void loadLayout(DrawableGraph dg, LayoutLoader loader)
    throws MalformedGMLException
  {
    try {
      double x = Double.parseDouble(dg.getProperty("x"));
      double y = Double.parseDouble(dg.getProperty("y"));
      double w = Double.parseDouble(dg.getProperty("w"));
      double h = Double.parseDouble(dg.getProperty("h"));

      Rectangle2D bounds = new Rectangle2D.Double(x, y, w, h);

      loader.loadGraphBounds(dg, bounds);

      dg.removeProperty("x");
      dg.removeProperty("y");
      dg.removeProperty("w");
      dg.removeProperty("h");
    }
    catch (NumberFormatException e) {
      throw (new MalformedGMLException("GML Parse error! Expected graphics data: " + e.toString()));
    }
  }


  private static void loadLayout(DrawableNode node,
                                 LayoutLoader loader)
    throws MalformedGMLException
  {
    try {
      double x = Double.parseDouble(node.getProperty("x"));
      double y = Double.parseDouble(node.getProperty("y"));
      double w = Double.parseDouble(node.getProperty("w"));
      double h = Double.parseDouble(node.getProperty("h"));

      Rectangle2D bounds = new Rectangle2D.Double(x, y, w, h);

      loader.loadNodeBounds(node, bounds);

      node.removeProperty("x");
      node.removeProperty("y");
      node.removeProperty("w");
      node.removeProperty("h");
    }
    catch (NumberFormatException e) {
      throw (new MalformedGMLException("GML Parse error! Expected graphics data: " + e.toString()));
    }
  }


  private static void loadLayout(DrawableEdge edge,
                                 LayoutLoader loader)
    throws MalformedGMLException
  {
    loader.loadEdgeBounds(edge, new Rectangle2D.Double());
  }

} // end class ConvertToDG
