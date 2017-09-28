package com.appliedminds.hmv.dsg;

import com.appliedminds.hmv.GraphElementProperties;
import com.appliedminds.martini.DrawableEdge;
import com.appliedminds.martinix.fader.FaderUtil;


/**
 * A bean for the various DSG edge properties.
 *
 * @author daepark@apmindsf.com
 */
public class DSGEdgeProperties extends GraphElementProperties {

  private static final String EMPTY_STRING = "";
  private static final String WHITE_SPACE  = " ";

  private String _relationship;

  public DSGEdgeProperties(DrawableEdge edge) {
    super(edge);

    _relationship = FaderUtil.getEdgeLabel(edge);
  }


  public String getRelationship() {
    return (_relationship);
  }


  public void setRelationship(String relationship) {
    if (relationship != null) {
      _relationship = relationship.replaceAll("[\n\r\f]", WHITE_SPACE).trim();
    }
    else {
      _relationship = EMPTY_STRING;
    }
  }


  protected GraphElementProperties replicate() {
    DSGEdgeProperties copy =
      new DSGEdgeProperties((DrawableEdge)getGraphElement());
    copy._relationship = this._relationship;

    return (copy);
  }


  protected void applyConfiguration() {
    DrawableEdge edge = (DrawableEdge)getGraphElement();

    // apply label change
    FaderUtil.setEdgeLabel(edge, _relationship);
  }

} // end class "DSGEdgeProperties"
