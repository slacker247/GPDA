package com.appliedminds.martini.test;

import com.appliedminds.martini.*;
import junit.framework.*;
import java.util.Iterator;

/**
 * Test for GraphUtil
 *
 * @author daepark@apmindsf.com
 */
public class GraphUtilTest extends TestCase {

  public GraphUtilTest(String name) {
    super(name);
  }

  /**
   * test for GraphUtil.isCyclic
   */
  public void testIsCyclic() {
    //
    // An acyclic graph
    //
    DrawableGraph graph = new DrawableGraph();
    DrawableNode a = graph.createNode();
    a.setProperty("id", "a");
    DrawableNode b = graph.createNode();
    b.setProperty("id", "b");
    DrawableNode c = graph.createNode();
    c.setProperty("id", "c");
    graph.createEdge(b, a);
    graph.createEdge(c, a);
    graph.createEdge(b, c);

    System.err.println("testing acyclic graph");
    assertTrue("graph is acyclic!", !GraphUtil.isCyclic(graph));

    //
    // A cyclic graph
    //
    graph.createEdge(a, c);
    System.err.println("testing cyclic graph");
    assertTrue("graph is cyclic!", GraphUtil.isCyclic(graph));


    //
    // acyclic multiple edges
    //
    graph = new DrawableGraph();
    a = graph.createNode();
    a.setProperty("id", "A");
    b = graph.createNode();
    b.setProperty("id", "B");
    graph.createEdge(b, a);
    graph.createEdge(b, a);

    System.err.println("testing multiple edge acyclic graph");
    assertTrue("graph is acyclic!", !GraphUtil.isCyclic(graph));

    //
    // cyclic multiple edges
    //
    graph.createEdge(a, b);

    System.err.println("testing multiple edge cyclic graph");
    assertTrue("graph is cyclic!", GraphUtil.isCyclic(graph));
  }


  /**
   * Test for GraphUtil.willCreateCyclicGraph()
   */
  public void testWillCreateCyclicGraph() {
    DrawableGraph graph = new DrawableGraph();

    DrawableNode a = graph.createNode();
    DrawableNode b = graph.createNode();
    DrawableNode c = graph.createNode();
    DrawableNode d = graph.createNode();
    DrawableNode e = graph.createNode();
    DrawableNode f = graph.createNode();

    //
    // graph one
    //

    assertTrue("will not create cyclic graph a to b!",
               !GraphUtil.willCreateCyclicGraph(graph, a, b));

    graph.createEdge(b, a);

    assertTrue("will not create cyclic graph b to c!",
               !GraphUtil.willCreateCyclicGraph(graph, b, c));

    graph.createEdge(c, b);

    assertTrue("will create cyclic graph c to a!",
               GraphUtil.willCreateCyclicGraph(graph, c, a));


    //
    // graph two
    //
    graph.createEdge(d, c);
    graph.createEdge(e, c);
    graph.createEdge(f, c);
    graph.createEdge(e, d);
    graph.createEdge(e, f);

    assertTrue("graph is acyclic!", !GraphUtil.isCyclic(graph));

    assertTrue("will create cyclic graph f to a",
               GraphUtil.willCreateCyclicGraph(graph, f, a));

  }


  /**
   * Test for bfs
   */
  public void testBfs() {
    DrawableGraph graph = new DrawableGraph();
    DrawableNode a = graph.createNode();
    a.setProperty("id", "a");
    DrawableNode b = graph.createNode();
    b.setProperty("id", "b");
    DrawableNode c = graph.createNode();
    c.setProperty("id", "c");
    DrawableNode d = graph.createNode();
    d.setProperty("id", "d");
    DrawableNode e = graph.createNode();
    e.setProperty("id", "e");

    //
    // graph 1
    //
    graph.createEdge(c, a);
    graph.createEdge(b, a);

    final StringBuffer buf = new StringBuffer();
    GraphUtil.bfs(graph, a, new GraphUtil.GraphVisitor() {
        public void visit(DrawableGraphElement element) {
          buf.append(element.getProperty("id"));
        }
      });

    System.err.println("Breadth first search (BFS) 1: " + buf.toString());

    if (! ("abc".equals(buf.toString()) ||
           "acb".equals(buf.toString())))
    {
      fail("Bad breadth first search 1: " + buf.toString());
    }


    //
    // graph 2
    //
    graph.createEdge(c, b);
    graph.createEdge(c, d);
    graph.createEdge(d, b);

    final StringBuffer buf2 = new StringBuffer();
    GraphUtil.bfs(graph, a, new GraphUtil.GraphVisitor() {
        public void visit(DrawableGraphElement element) {
          buf2.append(element.getProperty("id"));
        }
      });

    System.err.println("Breadth first search (BFS) 2: " + buf2.toString());

    if (! ("abcd".equals(buf2.toString()) ||
           "acbd".equals(buf2.toString())))
    {
      fail("Bad breadth first search 2: " + buf2.toString());
    }

    //
    // graph 3
    //
    graph = new DrawableGraph();
    a = graph.createNode();
    a.setProperty("id", "a");
    b = graph.createNode();
    b.setProperty("id", "b");
    c = graph.createNode();
    c.setProperty("id", "c");
    d = graph.createNode();
    d.setProperty("id", "d");
    e = graph.createNode();
    e.setProperty("id", "e");
    graph.createEdge(a, b);
    graph.createEdge(a, c);
    graph.createEdge(b, d);
    graph.createEdge(b, e);

    final StringBuffer buf3 = new StringBuffer();
    GraphUtil.bfs(graph, c, new GraphUtil.GraphVisitor() {
        public void visit(DrawableGraphElement element) {
          buf3.append(element.getProperty("id"));
        }
      });

    System.err.println("Breadth first search (BFS) 3: " + buf3.toString());

    if (! ("ca".equals(buf3.toString())))
    {
      fail("Bad breadth first search 3: " + buf3.toString());
    }

  }


  /**
   * Test for dfs
   */
  public void testDfs() {
    DrawableGraph graph = new DrawableGraph();

    DrawableNode a = graph.createNode();
    a.setProperty("id", "a");
    DrawableNode b = graph.createNode();
    b.setProperty("id", "b");
    DrawableNode c = graph.createNode();
    c.setProperty("id", "c");

    //
    // graph 1
    //
    graph.createEdge(b, a);
    graph.createEdge(c, a);

    // graph 1 postorder test
    final StringBuffer buf = new StringBuffer();
    GraphUtil.dfs(graph, a, new GraphUtil.GraphVisitor() {
        public void visit(DrawableGraphElement element) {
          buf.append(element.getProperty("id"));
        }
      }, true);

    System.err.println("Depth first search (dfs postorder) 1: " +
                       buf.toString());

    if (! ("cba".equals(buf.toString()) ||
           "bca".equals(buf.toString())))
    {
      fail("Bad depth first search postorder 1: " + buf.toString());
    }

    // graph 2 preorder test
    final StringBuffer buf_1 = new StringBuffer();
    GraphUtil.dfs(graph, a, new GraphUtil.GraphVisitor() {
        public void visit(DrawableGraphElement element) {
          buf_1.append(element.getProperty("id"));
        }
      }, false);

    System.err.println("Depth first search (dfs preorder) 1: " +
                       buf_1.toString());

    if (! ("acb".equals(buf_1.toString()) ||
           "abc".equals(buf_1.toString())))
    {
      fail("Bad depth first search preorder 1: " + buf_1.toString());
    }




    DrawableNode d = graph.createNode();
    d.setProperty("id", "d");
    DrawableNode e = graph.createNode();
    e.setProperty("id", "e");

    //
    // graph 2
    //
    graph.createEdge(c, b);
    graph.createEdge(d, b);
    graph.createEdge(e, c);


    // graph 2 postorder
    final StringBuffer buf2 = new StringBuffer();
    GraphUtil.dfs(graph, a, new GraphUtil.GraphVisitor() {
        public void visit(DrawableGraphElement element) {
          buf2.append(element.getProperty("id"));
        }
      }, true);

    System.err.println("Depth first search (dfs postorder) 2: " +
                       buf2.toString());

    if (! ("ecdba".equals(buf2.toString()) ||
           "decba".equals(buf2.toString())))
    {
      fail("Bad depth first search postorder 2: " + buf2.toString());
    }

    // graph 2 preorder
    final StringBuffer buf2_1 = new StringBuffer();
    GraphUtil.dfs(graph, a, new GraphUtil.GraphVisitor() {
        public void visit(DrawableGraphElement element) {
          buf2_1.append(element.getProperty("id"));
        }
      }, false);

    System.err.println("Depth first search (dfs preorder) 2: " +
                       buf2_1.toString());

    if (! ("abdce".equals(buf2_1.toString()) ||
           "abced".equals(buf2_1.toString())))
    {
      fail("Bad depth first search preorder 2: " + buf2_1.toString());
    }

  }


  /**
   * Test for GraphUtil.topologicalSort
   */
  public void testTopologicalSort() {
    DrawableGraph graph = new DrawableGraph();

    DrawableNode a = graph.createNode();
    a.setProperty("id", "a");
    DrawableNode b = graph.createNode();
    b.setProperty("id", "b");
    DrawableNode c = graph.createNode();
    c.setProperty("id", "c");
    DrawableNode d = graph.createNode();
    d.setProperty("id", "d");
    DrawableNode e = graph.createNode();
    e.setProperty("id", "e");


    graph.createEdge(b, a);
    graph.createEdge(c, a);
    graph.createEdge(d, b);
    graph.createEdge(e, c);
    graph.createEdge(b, e);
    graph.createEdge(d, c);


    StringBuffer buf = new StringBuffer();
    Iterator sorted = GraphUtil.topologicalSort(graph);
    while (sorted.hasNext()) {
      buf.append(((DrawableNode)sorted.next()).getProperty("id"));
    }

    System.err.println("topological sort: " + buf.toString());

    if (! "acebd".equals(buf.toString()))
    {
      fail("Bad topological sort: " + buf.toString());
    }

  }

}
