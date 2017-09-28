package com.appliedminds.martinix.greenpill;

import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.StringTokenizer;

import com.appliedminds.martini.DrawableEdge;
import com.appliedminds.martinix.ui.StringUtils;

/**
 * A CollapsedEdge encapsulates a collapsed edge familiar to the
 * GreenPillUI. A CollapsedEdge contains all the edges connecting two
 * nodes, thus there is a notion of a cardinality for a
 * CollapsedEdge. Each &quot;sub-edge&quot; within a collapsed edge
 * has an edge label (determined by it's type), a direction with
 * respect to the DrawableEdge connecting the two nodes, and a
 * visibility flag that that may be used for filtering certain edge
 * types.
 *
 * <p>The DrawableEdge must have the collapsed edge properties which
 * are the edgeDirection, edgeType and edgeVisible properties. These
 * properties should be represented as a list of strings as the
 * following:
 *
 * <p>
 * <pre>
 *   graph [
 *     ...
 *     edge [
 *       source 1
 *       target 2
 *       ...
 *       data [
 *         edgeTypeDirection    "(HEAD, TAIL)"
 *         edgeType             "(is related to, is near)"
 *         edgeTypeVisibility   "(true, false)"
 *         ...
 *       ]
 *       ...
 *     ]
 *     ...
 *   ]
 * </pre>
 *
 * <p>In the above example, the first &quot;sub-edge&quot; is of
 * type &quot;is related to&quot;, directed from source to target
 * and the second is of type &quot;is near&quot;, directed from
 * target to source. The collapsed edge has a cardinality of 2 but
 * only one of it's &quot;sub-edge&quot; is visible.
 *
 * @author daepark@apmindsf.com
 */
public class CollapsedEdge {

  public static final int HEAD_DIRECTION = 0;
  public static final int TAIL_DIRECTION = 1;

  private DrawableEdge _edge;
  private String[] _edgeDirections;
  private String[] _edgeTypes;
  private String[] _edgeVisibilities;


  /**
   * Factory method to obtain a collapsed edge.
   *
   * @param e the DrawableEdge with collapsed edge properties.
   * @return a CollapsedEdge encapsulating the edge's collapsed
   * edge properties.
   */
  public static CollapsedEdge getCollapsedEdge(DrawableEdge e) {
    //
    // FIX: we should cache the CollapsedEdges instead of making
    // a new one every time.
    //
    return (new CollapsedEdge(e));
  }

  /**
   * Create a CollapsedEdge from a DrawableEdge. Note that the
   * passed in edge may be modifed.
   *
   * @param edge A DrawableEdge with collapsed edge properties
   */
  private CollapsedEdge(DrawableEdge edge)
  {
    syncWithEdge(edge);
  }

  private CollapsedEdge() { }

  /**
   * Collect all sub-edge properties from the DrawableEdge
   */
  protected void syncWithEdge(DrawableEdge edge) {
    _edge = edge;

    String edgeDirection =
      _edge.getProperty(GreenPillUIPrefs.getEdgeTypeDirectionPropertyName());
    String edgeType =
      _edge.getProperty(GreenPillUIPrefs.getEdgeTypePropertyName());
    String edgeVisibility =
      _edge.getProperty(GreenPillUIPrefs.getEdgeTypeVisibilityPropertyName());

    if (edgeDirection == null || edgeType == null || edgeVisibility == null)
      {
        edgeDirection = "()";
        edgeType = "()";
        edgeVisibility = "()";
      }

    _edgeDirections = StringUtils.parseStringList(edgeDirection);
    _edgeTypes = StringUtils.parseStringList(edgeType);
    _edgeVisibilities = StringUtils.parseStringList(edgeVisibility);

    if (_edgeDirections.length != _edgeTypes.length ||
        _edgeTypes.length != _edgeVisibilities.length)
      {
        // if we have a valid greenpill gml this should not happen
        throw(new GreenPillUIError("A GreenPill edge must have the same number of \"edgedirection\", \"edgetype\", and \"edgevisibility\" values"));
      }

    // set the visibility of the edge depending on the visibility count
    // of the &quot;sub-edge(s)&quot;
    _edge.setVisible(getVisibleEdgeCount() > 0);
  }


  /**
   * Get the cardinality, which is the total number of
   * &quot;sub-edges&quot; this represents. Note that
   * this takes into account the non-visible &quot;sub-edge(s)&quot;
   *
   * @return the cardinality
   * @see #getVisibleEdgeCount
   */
  public int getCardinality() {
    return (_edgeDirections.length);
  }


  /**
   * Get the number of visible &quotsub-edge(s)&quot;
   *
   * @return the number of visible &quot;sub-edge(s)&quot;
   */
  public int getVisibleEdgeCount() {
    int count = 0;
    for (int i = 0; i < _edgeVisibilities.length; i++) {
      if ("true".equals(_edgeVisibilities[i])) {
        count++;
      }
    }
    return (count);
  }


  /**
   * Get the edge type of the &quot;sub-edge&quot; at the specified index.
   *
   * @param index the index of the &quot;sub-edge&quot; where 0 &lt;=
   * index &lt; cardinality.
   * @return the edge type of the &quot;sub-edge&quot; at the
   * specified index.
   */
  public String getEdgeType(int index) {
    return (_edgeTypes[index]);
  }


  /**
   * Set the visibility of a &quot;sub-edge&quot;. This call will modify
   * the underlying DrawableEdge.
   *
   * @param index the index of the &quot;sub-edge&quot; where 0 &lt;=
   * index &lt; cardinality.
   * @param visible true set to visible, false set to invisible.
   */
  public void setEdgeVisible(int index, boolean visible) {
    if (isEdgeVisible(index) != visible) {
      _edgeVisibilities[index] = (visible ? "true" : "false");

      //
      // modify edge edgeVisible property
      //
      String val = StringUtils.toStringList(_edgeVisibilities);
      _edge.setProperty(GreenPillUIPrefs.getEdgeTypeVisibilityPropertyName(),
                        val);

      // set the visibility of the edge depending on the visibility count
      // of the &quot;sub-edge(s)&quot;
      _edge.setVisible(getVisibleEdgeCount() > 0);
    }
  }


  /**
   * Is a &quot;sub-edge&quot; visible?
   *
   * @param index the index of the &quot;sub-edge&quot; where 0 &lt;=
   * index &lt; cardinality.
   * @return TRUE if visible, FALSE if invisible.
   */
  public boolean isEdgeVisible(int index) {
    return ("true".equals(_edgeVisibilities[index]));
  }


  /**
   * Is the &quot;sub-edge&quot; directed from tail to head?
   *
   * @param index the index of the &quot;sub-edge&quot; where 0 &lt;=
   * index &lt; cardinality.
   * @return TRUE if directed from tail to head, otherwise FALSE.
   */
  public boolean isEdgeTailToHeadDirection(int index) {
    return ("HEAD".equals(_edgeDirections[index]));
  }


  /**
   * Is the &quot;sub-edge&quot; directed from head to tail?
   *
   * @param index the index of the &quot;sub-edge&quot; where 0 &lt;=
   * index &lt; cardinality.
   * @return TRUE if directed from head to tail, otherwise FALSE.
   */
  public boolean isEdgeHeadToTailDirection(int index) {
    return (!isEdgeTailToHeadDirection(index));
  }


  /**
   * Add a &quot;sub-edge&quot;. Note that the underlying edge will be
   * modified.
   *
   * @param direction HEAD_DIRECTION or TAIL_DIRECTION
   * @param edgeType the edge type
   * @param visible whether or not this sub-edge should be visible
   */
  public void addSubEdge(int direction, String edgeType, boolean visible) {
    String[] oldDirections = _edgeDirections;
    String[] oldTypes = _edgeTypes;
    String[] oldVisibilities = _edgeVisibilities;

    _edgeDirections = new String[_edgeDirections.length + 1];
    _edgeTypes = new String[_edgeTypes.length + 1];
    _edgeVisibilities = new String[_edgeVisibilities.length + 1];

    for (int i = 0; i < _edgeDirections.length - 1; i++) {
      _edgeDirections[i] = oldDirections[i];
      _edgeTypes[i] = oldTypes[i];
      _edgeVisibilities[i] = oldVisibilities[i];
    }

    _edgeDirections[_edgeDirections.length - 1] =
      (direction == HEAD_DIRECTION ? "HEAD" : "TAIL");
    _edgeTypes[_edgeDirections.length - 1] = edgeType;
    _edgeVisibilities[_edgeDirections.length - 1] =
      (visible ? "true" : "false");

    //
    // modify edge properties
    //
    String val = StringUtils.toStringList(_edgeDirections);
    _edge.setProperty(GreenPillUIPrefs.getEdgeTypeDirectionPropertyName(),
                      val);
    val = StringUtils.toStringList(_edgeTypes);
    _edge.setProperty(GreenPillUIPrefs.getEdgeTypePropertyName(),
                      val);
    val = StringUtils.toStringList(_edgeVisibilities);
    _edge.setProperty(GreenPillUIPrefs.getEdgeTypeVisibilityPropertyName(),
                      val);

    // set the visibility of the edge depending on the visibility count
    // of the &quot;sub-edge(s)&quot;
    _edge.setVisible(getVisibleEdgeCount() > 0);
  }

} // end class CollapsedEdges

