package com.appliedminds.martini;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;


/**
 * Contains various algorithms for graph traversals.
 *
 * @author daepark@apmindsf.com
 */
public class GraphUtil {

  /**
   * All methods are static in this class.
   */
  private GraphUtil() { }


  /**
   * Check whether or not a graph has any cycles using dfs.
   *
   * <p>
   * <ol>
   *
   * <li>Run dfs on G.
   *
   * <li>If dfs follows an edge (u, v) and discovers that v is gray
   * (i.e. that s[v] > 0 but f[v] = 0), return true or "cyclic"
   *
   * <li>Otherwise, return false or "acyclic".
   *
   * </ol>
   *
   * @param graph the graph to test the cycle detection.
   * @return true if the graph contains a cycle, otherwise return false.
   * @see #dfs
   */
  public static boolean isCyclic(DrawableGraph graph) {

    NodeIterator itr = graph.nodesIterator();
    if (itr.hasNext()) {
      return (containsCycle(graph, itr.next(), null, null));
    }

    //              dfsVisitor.debug();
    return (false);
  }


  /**
   * Check if an edge going from the specified tail node to the head node
   * will create a cycle in the specified graph.
   *
   * @param graph the graph to test the cycle detection.
   * @param tail the source of the edge; the edge is pointing from this node.
   * @param head the target of the edge; the edge is pointing toward this node.
   * @see #isCyclic
   */
  public static boolean willCreateCyclicGraph(DrawableGraph graph,
                                              DrawableNode tail,
                                              DrawableNode head)
  {
    return (containsCycle(graph, tail, head, null));
  }


  /**
   * Perform a breadth first traversal beginning with the specified
   * node.
   *
   * <p>While dfs used an implicit stack, bfs uses an explicit queue
   * structure in determining the order in which vertices are
   * searched. Also, generally one does not restart bfs, because bfs
   * only makes sense in the context of exploring the part of the
   * graph that is reachable from a particular vertex, <i>s</i>.
   *
   * <p>bfs visits vertices in order of increasing distance from
   * <i>s</i>.  In fact, our bfs algorithm will keep track of the
   * distance or the number of edges in the shortest path from
   * <i>s</i> to each vertex.
   *
   * <p>Here is the pseudocode:
   *
   * <p>
   * <code>
   * <pre>
   * bfs(G, s) {
   *   // initialize
   *   s.distance 0
   *   s.parent null
   *   s.color gray
   *   foreach u V - {s} {
   *     u.distance
   *     u.parent null
   *     u.color white
   *   }
   *
   *   // do it
   *   Q.enqueue(s)
   *   while Q is not empty {
   *     u Q.dequeue()
   *     foreach v Adj[u] {
   *       if v.color = white {
   *         v.distance u.distance +1
   *         v.parent u
   *         v.color gray
   *         Q.enqueue(v)
   *       }
   *     }
   *     u.color black
   *   }
   * }
   * </pre>
   * </code>
   *
   * @param graph the graph to perform the traversal on.
   * @param startNode the node to start the traversal on.
   * @param visitor the visitor will be notified once per node during
   * the traversal.
   */
  public static void bfs(DrawableGraph graph,
                         DrawableNode startNode,
                         GraphVisitor visitor)
  {
    BFSVisitor bfsVisitor = new BFSVisitor();

    bfsVisitor.startVisit(startNode);
    bfsVisitor.setDistance(startNode, 0);
    bfsVisitor.setParent(startNode, null);


    LinkedList q = new LinkedList();
    q.addLast(startNode);

    while (q.size() > 0) {
      DrawableNode current = (DrawableNode)q.removeFirst();

      // Get the the adjacent nodes, nodes that are connected by all
      // outgoing edges from the current node.
      NodeIterator itr = current.getHeadNodes();

      while (itr.hasNext()) {
        DrawableNode adjNode = itr.next();
        if (bfsVisitor.isWhite(adjNode)) {
          bfsVisitor.startVisit(adjNode);
          bfsVisitor.setDistance(adjNode, bfsVisitor.getDistance(current) + 1);
          bfsVisitor.setParent(adjNode, current);
          q.addLast(adjNode);
        }
      }

      visitor.visit(current);
      bfsVisitor.finishVisit(current);
    }
  }


  /**
   * Perform a depth first traversal beginning with an arbitrary node
   * in the graph.
   *
   * <p>Depth-first search (dfs) traverses a directed graph
   * <i>G</i>. The defining characteristics of this traversal is that,
   * whenever it visits a node <i>u</i>, it recursively traverses the
   * graph starting at each <i>v in Adj[u]</i> before finishing
   * <i>u</i>. To put it another way, dfs replaces FIFO queue of bfs
   * by a LIFO stack.
   *
   * <p>dfs assigns each vertex <i>u</i> in <i>G</i> two values: a
   * starting time <i>s[u]</i> and a finishing time <i>f[u]</i>. When
   * the search first disovers <i>u</i>, it sets <i>u</i>'s start
   * time: when it finishes traversing all the edges out of <i>u</i>
   * or <i> Adj[u]</i>, it sets <i>u</i>'s finishing time. Time is a
   * global variable in the dfs algorithm that is incremented whenever
   * a vertex's starting or finishing time is set. To ensure that
   * every vertex in <i>G</i> is assigned starting and finishing time,
   * dfs loops over each vertex of <i>G</i> (in some arbitrary order),
   * starting the recursive traversal from every vertex that has not
   * yet been visited.
   *
   * <p>The dfs algorithm often uses colors to mark the visited
   * vertices. To see the mapping from times to colors, note first that
   * every vertex is initially assigned starting and finishing times
   * of 0, but the traversal always sets these times to values &gt; 0.
   *
   * <p>
   * <ul>
   *
   * <li>If a vertex <i>u</i> has <i>s[u] = f[u] = 0</i>, we say that
   * it is <b>white</b> (It has not yet been visited by the
   * algorithm).
   *
   * <li>If <i>s[u] &gt; 0</i> but <i>f[u] = 0</i>, we say that
   * <i>u</i> is <b>gray</b> (It has been discovered but not yet
   * finished).
   *
   * <li>If <i>f[u] &gt; 0</i>, we say that <i>u</i> is <b>black</b>
   * (It has been finished).
   *
   * </ul>
   *
   * <p>Here is the pseudocode:
   *
   * <p>
   * <code>
   * <pre>
   * dfs(G) {
   *   foreach u V {
   *     s[u] 0
   *     f[u] 0
   *     u.parent null
   *   }
   *
   *   time 1
   *   foreach u V {
   *     if s[u] = 0 { // not visited yet
   *       dfsVisit(G, u)
   *     }
   *   }
   * }
   *
   * dfsVisit(G, u) {
   *   s[u] time // start u
   *   time++
   *
   *   previsit(u); // for preorder
   *
   *   foreach v Adj[u] {
   *     if s[v] = 0 { // not visited yet
   *       v.parent u
   *       dfsVisit(G, v) // recur before continuing adj list
   *     }
   *   }
   *
   *   postvisit(u) // for postorder
   *
   *   f[u] time // finish u
   *   time++
   *  }
   * </pre>
   * </code>
   *
   * @param graph the graph to perform the traversal on.
   * @param visitor the visitor will be notified once per node during
   * the traversal.
   * @param postorder if true, run a postorder depth first traversal,
   * otherwise run preorder.
   */
  public static void dfs(DrawableGraph graph,
                         GraphVisitor visitor,
                         boolean postorder)
  {
    NodeIterator itr = graph.nodesIterator();
    if (itr.hasNext()) {
      DrawableNode startNode = itr.next();
      dfs(graph, startNode, visitor, postorder);
    }
  }


  /**
   * Perform a depth first traversal with the specified starting node
   * in the graph.
   *
   * @param graph the graph to perform the traversal on,
   * @param startNode the node to start the traversal on.
   * @param visitor the visitor will be notified once per node during
   * the traversal.
   * @param postorder if true, run a postorder depth first traversal,
   * otherwise run preorder.
   * @see #dfs(DrawableGraph, GraphUtil.GraphVisitor, boolean)
   */
  public static void dfs(DrawableGraph graph,
                         DrawableNode startNode,
                         GraphVisitor visitor,
                         boolean postorder)
  {
    DFSVisitor dfsVisitor = new DFSVisitor();
    dfs(graph, startNode, null, visitor, dfsVisitor, postorder);

    // Take care of possible unconnected components
    NodeIterator itr = graph.nodesIterator();
    while (itr.hasNext()) {
      DrawableNode current = itr.next();
      if (current != startNode) {
        dfs(graph, current, null, visitor, dfsVisitor, postorder);
      }
    }
  }


  /**
   * Run a postorder dfs.
   *
   * @see #dfs(DrawableGraph, GraphUtil.GraphVisitor, boolean)
   */
  public static void postorder(DrawableGraph graph, GraphVisitor visitor) {
    dfs(graph, visitor, true);
  }


  /**
   * Run a postorder dfs.
   *
   * @see #dfs(DrawableGraph, DrawableNode, GraphUtil.GraphVisitor, boolean)
   */

  public static void postorder(DrawableGraph graph,
                               DrawableNode startNode,
                               GraphVisitor visitor)
  {
    dfs(graph, startNode, visitor, true);
  }


  /**
   * Run a preorder dfs.
   *
   * @see #dfs(DrawableGraph, GraphUtil.GraphVisitor, boolean)
   */
  public static void preorder(DrawableGraph graph, GraphVisitor visitor) {
    dfs(graph, visitor, false);
  }


  /**
   * Run a preorder dfs.
   *
   * @see #dfs(DrawableGraph, DrawableNode, GraphUtil.GraphVisitor, boolean)
   */
  public static void preorder(DrawableGraph graph,
                              DrawableNode startNode,
                              GraphVisitor visitor)
  {
    dfs(graph, startNode, visitor, false);
  }


  /**
   * A topological sort takes a directed acyclic graph, or dag, and
   * returns an ordered list of the vertices such that if there is an
   * edge (v,u) in the graph, then v will appear before u in the list.
   *
   * <p>Specifically:
   *
   * <p>
   * <ol>
   *
   * <li>Run dfs on the graph,
   *
   * <li>If graph is cyclic then fail (graphs with directed cycles
   * have no topological ordering).
   *
   * <li>Return the nodes of the graph is descending order by their
   * finishing times <i>f[v]</i> or by decreasing postorder.
   *
   * </ol>
   *
   * @param graph an acyclic graph.
   * @return a sorted collection of all nodes in the graph
   * such that it meets the topological sort order constraints. If graph is
   * cyclic then return an empty collection.
   * @throws RuntimeException if graph is cyclic.
   * @see #dfs
   */
  public static Iterator topologicalSort(DrawableGraph graph)
  {
    // a GraphVisitor that puts the visited nodes by the dfs algorithm
    // on a stack which will ultimately contain the topological sort
    // order.
    final LinkedList retVal = new LinkedList();
    GraphVisitor myVisitor = new GraphVisitor() {
        public void visit(DrawableGraphElement node) {
          retVal.addFirst(node);
        }
      };

    NodeIterator itr = graph.nodesIterator();
    if (itr.hasNext()) {
      boolean containsCycle =
        containsCycle(graph, itr.next(), null, myVisitor);

      if (containsCycle) {
        throw (new RuntimeException("Can NOT topological sort on a cyclic graph!"));
      }
    }

    return (retVal.iterator());
  }


  /**
   * Get the <i>strongly connected components (SCC's)</i> of the given
   * graph.
   *
   * <P>Two vertices <i>u</i> and <i>v</i> of a directed graph <i>G =
   * (V, E)</i> are called <i>connected</i> if there is a path from
   * <i>u</i> to <i>v</i>, and one from <i>v</i> to <i>u</i>. This
   * relation between vertices partitions <i>V</i> into disjoint sets,
   * called the <i>strongly connected components (SCC's)</i> of the
   * graph. Within a strongly connected component, every pair of
   * vertices are connected.
   *
   * <P>If each SCC is shrunk into a vertex (supervertex) and draw an
   * edge (superedge) from SCC X to SCC Y (if there is at least one
   * edge from a vertex in X to a vertex in Y), the resulting directed
   * graph would have to be a directed acyclic graph or DAG - that is
   * to say, it can not have any cycles. The reason is simple: a cycle
   * containing several SCC's would merge to a single SCC, since there
   * would be a path between every pair of vertices in the SCC's of
   * the cycle. Hence, every directed graph is a DAG of its SCC's.
   *
   * <p>This can be used to evaluate the &quot;natural&quot; clusters
   * of a directed graph where a &quot;natural&quot; cluster is
   * represented by an SCC of the graph.
   *
   * @param graph the graph to find the SCC's on.
   * @return the set of SCC's found in the specified graph.
   */
//   public static SCC[] getSCCs(DrawableGraph graph) {

//     return (null);
//   }


//   public static class SCC {

//   } // end class SCC


  /**
   * Check for a cycle in the specified graph. This will detect cycles
   * using the dfs algorithm using postorder.
   *
   * @param graph the graph to test the cycle on
   * @param startNode the node to start the dfs alorithm
   * @param testNode if not null, act like if there is an edge from
   * the startNode to the testNode.
   * @param visitor used by topological sort and any other algorithms
   * that may want to test for cycle detection and run dfs, but do not
   * want to run dfs twice.
   */
  private static boolean containsCycle(DrawableGraph graph,
                                       DrawableNode startNode,
                                       DrawableNode testNode,
                                       GraphVisitor visitor)
  {
    if (visitor == null) {
      // create a default dumb graph visitor
      visitor = new GraphVisitor() {
          public void visit(DrawableGraphElement element) { }
        };
    }

    DFSVisitor dfsVisitor = new DFSVisitor();

    if (startNode != null) {
      dfs(graph, startNode, testNode, visitor, dfsVisitor, true);
    }

    // Take care of possible unconnected components
    NodeIterator itr = graph.nodesIterator();
    while (itr.hasNext()) {
      DrawableNode current = itr.next();
      if (current != startNode) {
        dfs(graph, current, null, visitor, dfsVisitor, true);
      }
    }

    //              dfsVisitor.debug();
    return (dfsVisitor.detectedCycle());
  }


  /**
   * dfs with the specified dfsVisitor
   *
   * @param testNode if not null, dfs will think as if their is an edge from
   * the startNode to the testNode.
   */
  private static void dfs(DrawableGraph graph,
                          DrawableNode startNode,
                          DrawableNode testNode,
                          GraphVisitor visitor,
                          DFSVisitor dfsVisitor,
                          boolean postorder)
  {
    if (dfsVisitor.isWhite(startNode)) {
      dfsVisit(graph, startNode, testNode, visitor, dfsVisitor, postorder);
    }
  }


  /**
   * The recursive method for dfs.
   *
   * @param testNode if not null, dfs will think as if their is an edge from
   * the current node to the testNode.
   */
  private static void dfsVisit(DrawableGraph graph,
                               DrawableNode node,
                               DrawableNode testNode,
                               GraphVisitor visitor,
                               DFSVisitor dfsVisitor,
                               boolean postorder)
  {
    // mark node as gray
    dfsVisitor.startVisit(node);

    if (!postorder) {
      visitor.visit(node);
    }

    NodeIterator itr = node.getHeadNodes();

    if (testNode != null) {
      // add the test node to the list of adjacent nodes
      //      List newList = new ArrayList();
      NodeList newList = new NodeList();
      newList.add(testNode);
      while (itr.hasNext()) {
        newList.add(itr.next());
      }
      itr = newList.iterator();
    }

    while (itr.hasNext()) {
      DrawableNode nextNode = (DrawableNode)itr.next();
      if (dfsVisitor.isWhite(nextNode)) {
        // keep track of the dfs path
        dfsVisitor.setParent(nextNode, node);
        dfsVisit(graph, nextNode, null, visitor, dfsVisitor, postorder);
      }
      else if (dfsVisitor.isGray(nextNode)) {
        // cycle detected
        dfsVisitor.addCycle(nextNode);
      }
    }

    if (postorder) {
      visitor.visit(node);
    }

    // mark node as black
    dfsVisitor.finishVisit(node);
  }


  /**
   * A visitor interface to the traversal algorithms.  When a
   * traversal algorithm &quot;visits&quot; a graph element (a node in
   * the case of dfs), the GraphVisitor will be notified by calling
   * the visitor's <code>visit</code> method. In most cases, the
   * GraphVisitor is assured that the <code>visit</code> method will
   * be called once per graph element during a traversal.
   *
   * @author daepark@apmindsf.com
   */
  public interface GraphVisitor {
    public void visit(DrawableGraphElement element);
  } // end interface GraphVisitor;



  /**
   * As commonly practiced, the dfs and bfs algorithms color the
   * verices white, gray, and black - white means the vertex has not
   * been visited by the algorithm, gray means the vertex has been
   * discovered and in the queue for processing, and black means the
   * algorithm has finished visiting the vertex (dequeued and
   * expanded).
   */
  private static class FSVisitor {

    protected Set __visitStarted = new HashSet();
    protected Set __visitFinished = new HashSet();

    public void startVisit(DrawableNode node) {
      __visitStarted.add(node);
    }

    public void finishVisit(DrawableNode node) {
      __visitFinished.add(node);
    }

    public boolean isWhite(DrawableNode node) {
      return (! (__visitStarted.contains(node) ||
                 __visitFinished.contains(node)));
    }

    public boolean isGray(DrawableNode node) {
      return (__visitStarted.contains(node) &&
              !__visitFinished.contains(node));
    }

    public boolean isBlack(DrawableNode node) {
      return (__visitStarted.contains(node) &&
              __visitFinished.contains(node));
    }

    public void clear() {
      __visitStarted = new HashSet();
      __visitFinished = new HashSet();
    }

  } // end class FSVisitor


  /**
   * An FSVisitor used by bfs. It can also keep track of the path of
   * the bfs algorithm and the distance of a node from the bfs origin,
   * <i>s</i>.
   *
   * @see #bfs
   */
  private static class BFSVisitor extends FSVisitor {

    private Map __distanceMap = new HashMap();
    private Map __parentMap = new HashMap();


    /**
     * Set the distance of the node from <i>s</i>.
     *
     * @param node the node to mark the distance.
     * @param distance the distance from <i>s</i>.
     */
    public void setDistance(DrawableNode node, int distance) {
      __distanceMap.put(node, new Integer(distance));
    }

    /**
     * Get the marked distance previously set by <code>setDistance</code>
     *
     * @param node the node to get the distance for.
     */
    public int getDistance(DrawableNode node) {
      Integer i = (Integer)__distanceMap.get(node);
      if (i == null) {
        return (-1);
      }
      return (i.intValue());
    }

    /**
     * Keep track of the bfs path
     *
     * @param node the bfs path from parentNode to node.
     * @param parentNode the bfs path from parentNode to node.
     */
    public void setParent(DrawableNode node, DrawableNode nodeParent) {
      __parentMap.put(node, nodeParent);
    }

    public void clear() {
      super.clear();
      __distanceMap.clear();
      __parentMap.clear();
    }

  } // end class BFSVisitor


  /**
   * An FSVisitor used by dfs. It can also keep track of the path of
   * the dfs algorithm and any cycled nodes the dfs may come across.
   *
   * @see #dfs
   */
  private static class DFSVisitor extends FSVisitor {

    private List __cycles = new ArrayList();
    private int __visitTime = 1;
    private Map __visitStartTimeMap = new HashMap();
    private Map __visitFinishTimeMap = new HashMap();
    private Map __parentMap = new HashMap();

    public void startVisit(DrawableNode node) {
      super.startVisit(node);
      __visitStartTimeMap.put(node, new Integer(__visitTime++));
    }

    public void finishVisit(DrawableNode node) {
      super.finishVisit(node);
      __visitFinishTimeMap.put(node, new Integer(__visitTime++));
    }

    /**
     * Keep track of the dfs path
     *
     * @param node the dfs path from parentNode to node.
     * @param parentNode the dfs path from parentNode to node.
     */
    public void setParent(DrawableNode node, DrawableNode nodeParent) {
      __parentMap.put(node, nodeParent);
    }

    /**
     * Keep track of any cycled nodes. A node is considered to be
     * cycled if the dfs follows an edge(u,v) and discovers that v is
     * &quot;gray&quot;.
     */
    public void addCycle(DrawableNode cycledNode) {
      __cycles.add(cycledNode);
    }

    /**
     * Did the dfs comes across any cycled nodes?
     */
    public boolean detectedCycle() {
      return (__cycles.size() > 0);
    }

    /**
     * Clear the this tracker to start fresh.
     */
    public void clear() {
      super.clear();
      __cycles.clear();
      __visitStartTimeMap.clear();
      __visitFinishTimeMap.clear();
      __parentMap.clear();
      __visitTime = 1;
    }

  } // end class DFSVisitor



} // end class GraphUtil
