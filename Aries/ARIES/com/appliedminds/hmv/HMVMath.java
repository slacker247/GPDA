package com.appliedminds.hmv;

import com.appliedminds.martini.*;
import com.appliedminds.martinix.fader.FaderUtil;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.HashSet;
import java.util.ArrayList;


/**
 * This is a utility class that collects the math methods needed for
 * this app.
 *
 * @author will@apmindsf.com
 */
public class HMVMath
{
  /*
   * Construct the path that is a list of nodes and all of its
   * incoming edges broken into records. Each record starts with a
   * single node followed by all of its supporting edges (i.e., edges
   * that regard the node as its head and is &quot;positive&quot;),
   * followed by all of its refuting edges (i.e., edges that regard
   * the node as its head and is &quot;negative&quot;). If a node is
   * in the list then it will always have a pair of supporting and
   * refuting edge list.
   *
   * This allows us to do the hard graph sorting and traversal stuff
   * up front so that we can recalculate slider values quickly later.
   *
   * @param sliderNode the node whose slider is being adjusted
   * @param path will be filled with the calculation path.
   */
  static void initCalculationPath2(DrawableGraph graph,
                                   DrawableNode sliderNode,
                                   List path,
                                   boolean recalculate)
  {
    path.clear();

    //
    // Collect only the nodes that are underneath the slider node.
    // Performing a BFS from the slider node should take care of this.
    //
    final Set childNodes = new HashSet();
    if (sliderNode != null) {
      GraphUtil.bfs(graph, sliderNode, new GraphUtil.GraphVisitor() {
          public void visit(DrawableGraphElement e) {
            childNodes.add(e);
          }
        });
    }
    else {
      // We are recalculating everything (for example, deleting a node
      // or edge).
      for (NodeIterator itr=graph.nodesIterator(); itr.hasNext();) {
        childNodes.add(itr.next());
      }
    }


    //    System.err.println("childNodes.size() : " + childNodes.size());


    // Do a topological sort on the graph to get
    // the ordering on what should be calculated first
    //
    // We only need to do a topologial sort
    // from the manual node and NOT on the entire graph
    //
    Iterator sorted = GraphUtil.topologicalSort(graph);

    List sortedList = new ArrayList();
    boolean haveSeenSliderNode = false;

    if (sliderNode != null) {
      while (sorted.hasNext()) {
        DrawableNode current = (DrawableNode)sorted.next();
        if (current == sliderNode) {
          haveSeenSliderNode = (haveSeenSliderNode || true);
        }

        if (haveSeenSliderNode) {
          sortedList.add(current);
        }
      }
    }
    else {
      while (sorted.hasNext()) {
        sortedList.add(sorted.next());
      }
    }

    //    System.err.println("sortedList.size() : " + sortedList.size());

    //
    // Now calculate all nodes from the manual node
    //
    sorted = sortedList.iterator();
    while (sorted.hasNext()) {
      DrawableNode current = (DrawableNode)sorted.next();
      // don't adjust the manual node
      if (!recalculate && current == sliderNode) {
        continue;
      }
      else if ((recalculate && current == sliderNode) ||
               current != sliderNode)
      {
        if (childNodes.contains(current)) {
          // set it to automatic
          FaderUtil.setManual(current, false);

          EdgeIterator itr = current.getIncomingEdges();

          // only update slider value if the node has parents
          if (itr.hasNext()) {

            List supporting = new ArrayList();
            List refuting = new ArrayList();

            int edgeCount = 0;
            path.add(current);

            while (itr.hasNext()) {

              DrawableEdge edge = (DrawableEdge)itr.next();
              if (FaderUtil.isEdgePositive(edge)) {
                supporting.add(edge);
              }
              else {
                refuting.add(edge);
              }
              ++edgeCount;
            }

            path.add(supporting);
            path.add(refuting);
          }
        }
      }
    }
  }



  /**
   * Use the pre-computed path to recalulate the probability of
   * affected nodes.
   *
   * @param sliderNode the node whose slider is being adjusted.
   * @param path the path
   */
  static void recalculateProbabilityUsingPath2(DrawableNode mnode,
                                               List path,
                                               GraphPanel graphPanel)
  {
    DrawableGraphContext ctx = graphPanel.getDrawableGraphContext();

    if (mnode != null) {
      setNeedsRepaintOnOutgoingEdges(mnode, ctx);
    }

    for(Iterator i = path.iterator(); i.hasNext(); ) {
      DrawableNode anode = (DrawableNode)i.next();

      int oldValue  = FaderUtil.getSliderValue(anode);

      // There is always a supporting and refuting edge list.
      List supportingOrig = (List)i.next();
      List refutingOrig = (List)i.next();

      List supporting = new ArrayList();
      List refuting = new ArrayList();

      // fix the lists in case an edge has changed from refuting to
      // supporting or vice versa.
      for (Iterator itr=supportingOrig.iterator(); itr.hasNext();)
      {
        DrawableEdge edge = (DrawableEdge) itr.next();
        if (FaderUtil.isEdgePositive(edge)) {
          supporting.add(edge);
        }
        else {
          refuting.add(edge);
        }
      }
      for (Iterator itr=refutingOrig.iterator(); itr.hasNext();)
      {
        DrawableEdge edge = (DrawableEdge) itr.next();
        if (FaderUtil.isEdgePositive(edge)) {
          supporting.add(edge);
        }
        else {
          refuting.add(edge);
        }
      }

      double newValue = unbias(supporting, refuting);

      //System.err.println("unbiased value: " + newValue);

      int newIntValue = (int)Math.round(newValue * 100.0);

      if (oldValue != newIntValue) {
        FaderUtil.setSliderValue(anode, newIntValue);
        setNeedsRepaintOnOutgoingEdges(anode, ctx);
      }
    }
  }


  private static void setNeedsRepaintOnOutgoingEdges(DrawableNode node,
                                                     DrawableGraphContext ctx)
  {
    // need to update the outgoing edges of this
    // node who's slider value is changing
    EdgeIterator edges = node.getOutgoingEdges();
    while (edges.hasNext()) {
      DrawableEdge edge = edges.next();
      ctx.setNeedsRepaint(edge, true);
    }
  }

  /**
   * Normalize (unbias) node calculation
   *
   * <Pre>
   * Public Calculate(Arraylist Sw, Arraylist, Sv, Arraylist Rw, Arraylist Rv)
   * {
   * Float Ssum=0;
   * Float A=0;
   * For(Int I=0; I <Sw.Size(); I++){
   *   Ssum+=Sw[I]*Sv[I];
   *   A+=Sw[I];
   * }
   * Float Rsum=0;
   * Float B=0;
   * For(Int I=0; I <Rw.Size(); I++){
   *  Rsum+=Rw[I]*Rv[I];
   *  B+=Rw[I];
   * }
   *
   * Float T=(Ssum-Rsum)/(A);
   *
   * Return Math.max(T, 0);
   * }
   * </Pre>
   *
   * @see Kurt Bollacker
   */
  private static double unbias(List supporting, List refuting)
  {
    double Ssum = 0;
    double A = 0;

    for (Iterator itr=supporting.iterator(); itr.hasNext();) {
      DrawableEdge edge = (DrawableEdge)itr.next();
      double tailValue = FaderUtil.getSliderValue(edge.getTail()) * 0.01;
      int edgeValue = FaderUtil.getSliderValue(edge);
      Ssum += (edgeValue * tailValue);
      A += edgeValue;
    }

    double Rsum = 0;
    double B = 0;
    for (Iterator itr=refuting.iterator(); itr.hasNext();)
    {
      DrawableEdge edge = (DrawableEdge)itr.next();
      double tailValue = FaderUtil.getSliderValue(edge.getTail())*0.01;
      int edgeValue = -1 * FaderUtil.getSliderValue(edge);
      Rsum += (edgeValue * tailValue);
      B += edgeValue;
    }

    if (A == 0) {
      return (0);
    }

    double T = (Ssum - Rsum) / A;
    return (Math.max(T, 0));

    //     System.err.println("Ssum = " + Ssum + ", A = " + A + ", Rsum = " + Rsum + ", B = " + B + ", T = " + T;
  }

} // end class HMVMath
