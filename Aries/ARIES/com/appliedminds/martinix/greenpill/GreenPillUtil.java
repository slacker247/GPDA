package com.appliedminds.martinix.greenpill;

import java.util.Arrays;
import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.StringTokenizer;

import com.appliedminds.martini.*;
import com.appliedminds.martinix.ui.TypeInfo;
import com.appliedminds.martinix.ui.StringUtils;


/**
 * This is a utility class that can help a user deal with a graph which
 * is missing the "rootdx" property required by the greenpill GraphUI.
 * This class will traverse a given DrawableGraph from the root node and
 * update the "rootdx" property of each node in the graph.
 *
 * @author will@apmindsf.com
 * @author daepark@apmindsf.com
 */
public class GreenPillUtil {

  /**
   * Start from the root node and traverse the graph, updating the "rootdx"
   * property of each node.
   *
   * @param g the drawable graph to update
   */
  public static void updateRootDistances(DrawableGraph g)
  {
    System.err.println("updating rootdx values");
    ArrayList list = new ArrayList();
    list.add(getRootNode(g));
    markDistanceFromRoot(g, list, 0);
  }


  /**
   * Retrieve the node in the graph whose "root" property is set to true.
   *
   * @return a DrawableNode that's marked as root in the graph. Returns
   * null if not found.
   */
  public static DrawableNode getRootNode(DrawableGraph g) {
    NodeIterator it = g.nodesIterator();
    while (it.hasNext()) {
      DrawableNode n = it.next();
      String isRoot = (String) n.getProperty("root");
      if ("true".equals(isRoot))
        return n;
    }

    return null;
  }


  /**
   * Change the "root" node to the specified node in graph g, assuming
   * the node is actually in graph g. If the node is not in graph g,
   * there will not be a "root" node specified in g. This temporarily
   * invalidates the "rootdx" property for each node.
   *
   * @param g the graph to update the "root" node.
   * @param newRoot the new "root" node.
   */
  public static void changeRootNode(DrawableGraph g, DrawableNode newRoot)
  {
    NodeIterator itr = g.nodesIterator();
    while (itr.hasNext()) {
      DrawableNode node = itr.next();
      if (newRoot == node) {
        setToRoot(node, true);
      }
      else {
        setToRoot(node, false);
      }

      node.setProperty("rootdx", Integer.toString(-1));
    }

    ArrayList tmp = new ArrayList();
    tmp.add(newRoot);
    markDistanceFromRoot(g, tmp, 0);
  }



  /*
   * Get the node lable via a property.
   *
   * Could return null.
   */
  public static String getNodeLabel(DrawableNode n) {
    return (n.getProperty(GreenPillUIPrefs.getNodeLabelPropertyName()));
  }


  /*
   * Set the node lable via a property.
   *
   */
  public static void setNodeLabel(DrawableNode n, String label) {
    n.setProperty(GreenPillUIPrefs.getNodeLabelPropertyName(), label);
  }

  /*
   * @return true if the "selected" property is set to "true".
   */
  public static boolean isNodeSelected(DrawableNode n) {
    return (isGraphElementSelected(n));
  }


  /*
   * @return true if the "selected" property is set to "true".
   */
  public static boolean isEdgeSelected(DrawableEdge e) {
    return (isGraphElementSelected(e));
  }


  /*
   * @return true if the "selected" property is set to "true".
   */
  private static boolean isGraphElementSelected(DrawableGraphElement elt) {
    String str = elt.getProperty(GreenPillUIPrefs.getSelectedPropertyName());
    return ("true".equals(str));
  }



  /*
   * Get the node type count via a property.  If the property is not
   * present, then throw.
   *
   * Also throw if the property is set but is not a valid int.
   */
  public static int getDistanceFromRoot(DrawableNode n) {
    String str =
      n.getProperty(GreenPillUIPrefs.getDistanceFromRootPropertyName());
    if (str != null) {
      try {
        return(Integer.parseInt(str));
      }
      catch(NumberFormatException e) {
        throw(new GreenPillUIError("Bad int value for distance-from-root property."));
      }
    }
    throw(new GreenPillUIError("Required node property missing: " +
                               GreenPillUIPrefs.getDistanceFromRootPropertyName()));
  }


  /*
   * @return true if the "root" property is set to "true".
   */
  public static boolean isRootNode(DrawableNode n) {
    String str = n.getProperty(GreenPillUIPrefs.getRootNodePropertyName());
    return ("true".equals(str));
  }


  /**
   * Get the list of all known edge types for the specified graph.
   *
   * @param graph the DrawableGraph context
   * @return all the known edge types (or relationships) of the given
   * graph.
   * @see GreenPillUIPrefs#getGraphEdgeTypesPrefix(int)
   */
  public static List getEdgeTypes(DrawableGraph graph) {
    String[] strList = null;
    String key = GreenPillUIPrefs.getGraphEdgeTypesPropertyName();
    String val = graph.getProperty(key);
    if (val != null) {
      strList = StringUtils.parseStringList(val);
    }
    else {
      strList = new String[0];
    }

    return (Arrays.asList(strList));
  }


  /**
   * Get the number of edges with the given type in the specified graph.
   *
   * @return the number of edges with the given type in the specified graph
   * that is currently visible.
   * @see #getEdgeTypes
   */
  public static int getEdgeTypeCount(DrawableGraph graph, String edgeType) {
    int count = 0;
    EdgeIterator itr = graph.edgesIterator();
    while (itr.hasNext()) {
      DrawableEdge edge = itr.next();
      CollapsedEdge ce = CollapsedEdge.getCollapsedEdge(edge);

      for (int i = 0; i < ce.getCardinality(); i++) {
        if (edgeType.equals(ce.getEdgeType(i))) {
          count++;
        }
      }
    }
    return (count);
  }


  /**
   * Get the list of all known node types for the specified graph.
   *
   * @param graph the DrawableGraph context
   * @return all the known node types of the given graph.
   * @see GreenPillUIPrefs#getGraphNodeTypesPrefix(int)
   */
  public static List getNodeTypes(DrawableGraph graph) {
    String[] strList = null;
    String key = GreenPillUIPrefs.getGraphNodeTypesPropertyName();
    String val = graph.getProperty(key);
    if (val != null) {
      strList = StringUtils.parseStringList(val);
    }
    else {
      strList = new String[0];
    }

    return (Arrays.asList(strList));
  }


  /**
   * Get the number of nodes with the given type in the specified graph.
   *
   * @return the number of nodes with the given type in the specified graph
   * @see #getNodeTypes
   */
  public static int getNodeTypeCount(DrawableGraph graph, String nodeType) {
    int count = 0;
    NodeIterator itr = graph.nodesIterator();
    while (itr.hasNext()) {
      DrawableNode node = itr.next();
      TypeInfo nodeTypeInfo = TypeInfo.getTypeInfo(node);

      for (int i=0; i<nodeTypeInfo.getTypeCount(); i++) {
        if (nodeTypeInfo.getType(i).equals(nodeType)) {
          count++;
        }
      }
    }
    return (count);
  }


  /**
   * Filter the graph so that only the edges with the given edgeTypes
   * will be displayed.
   *
   * @param graph the graph to filter. This graph will be modified.
   * @param edgeTypes a collection of edge types (Strings)
   */
  public static void filterEdges(DrawableGraph graph, List edgeTypes) {
    EdgeIterator itr = graph.edgesIterator();
    while (itr.hasNext()) {
      DrawableEdge edge = itr.next();

      CollapsedEdge ce = CollapsedEdge.getCollapsedEdge(edge);
      for (int i = 0; i < ce.getCardinality(); i++) {
        if (edgeTypes.contains(ce.getEdgeType(i))) {
          ce.setEdgeVisible(i, true);
        }
        else {
          ce.setEdgeVisible(i, false);
        }
      }
    }
  }


  /**
   * Filter the graph so that only the nodes with the given nodeTypes
   * will be displayed.
   *
   * @param graph the graph to filter. This graph will be modified.
   * @param nodeTypes a collection of node types (Strings)
   */
  public static void filterNodes(DrawableGraph graph, List nodeTypes) {
    NodeIterator itr = graph.nodesIterator();
    while (itr.hasNext()) {
      DrawableNode node = itr.next();
      TypeInfo nodeTypeInfo = TypeInfo.getTypeInfo(node);

      for (int i=0; i<nodeTypeInfo.getTypeCount(); i++) {
        if (nodeTypes.contains(nodeTypeInfo.getType(i))) {
          nodeTypeInfo.setTypeVisibility(i, true);
        }
        else {
          nodeTypeInfo.setTypeVisibility(i, false);
        }

        node.setVisible(nodeTypeInfo.getVisibleTypeCount() > 0 ||
                        nodeTypeInfo.getTypeCount() == 0);
      }
    }
  }


  /**
   * Recursive procedure to mark the link distance from root.
   * First time pass in a list containing just the root node
   * with a distance of zero.
   *
   * Go through all the nodes, starting from root and set the
   * DISTANCE_FROM_ROOT_PROP property.
   *
   * Note that nodes unconnected to root are left with
   * property "rootdx" value -1.
   *
   * @param graph the graph we want to update.
   * @param nodes the nodes to mark recursively.
   * @param distance the current distance.
   */
  private static void markDistanceFromRoot(DrawableGraph graph,
                                           List nodes,
                                           int distance)
  {
    ArrayList adjacentNodes = new ArrayList();

    for(Iterator i = nodes.iterator(); i.hasNext(); )
    {
      DrawableNode n = (DrawableNode) i.next();

      n.setProperty("rootdx", Integer.toString(distance));
      for(NodeIterator j = n.getConnectedNodes(); j.hasNext(); )
      {
        DrawableNode m = j.next();
        if (m.getProperty("rootdx") == null ||
            "-1".equals(m.getProperty("rootdx")))
        {
          adjacentNodes.add(m);
        }
        else
        {
          // Note that if there is more than one
          // path from m to root, m gets the
          // smallest applicable value for
          // DISTANCE_FROM_ROOT_PROP
        }
      }
    }

    if (adjacentNodes.size() > 0)
    {
      markDistanceFromRoot(graph, adjacentNodes, ++distance);
    }
  }


  private static void setToRoot(DrawableNode node, boolean isRoot) {
    node.setProperty("root", isRoot ? "true" : "false");
  }

} // end class NodeDistanceMarker
