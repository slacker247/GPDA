package com.appliedminds.hmv.dsg;

import com.appliedminds.hmv.GraphElementProperties;
import com.appliedminds.martini.DrawableNode;
import com.appliedminds.martinix.fader.FaderUtil;


/**
 * A bean for the various DSG node properties.
 *
 * @author daepark@apmindsf.com
 */
public class DSGNodeProperties extends GraphElementProperties {

  private static final String EMPTY_STRING = "";
  private static final String WHITE_SPACE  = " ";

  private String _entityName;

  public DSGNodeProperties(DrawableNode node) {
    super(node);

    _entityName = FaderUtil.getNodeLabel(node);
  }


  public String getEntityName() {
    return (_entityName);
  }


  public void setEntityName(String entityName) {
    if (entityName != null) {
      _entityName = entityName.replaceAll("[\n\r\f]", WHITE_SPACE).trim();
    }
    else {
      _entityName = EMPTY_STRING;
    }
  }


  protected GraphElementProperties replicate() {
    DSGNodeProperties copy =
      new DSGNodeProperties((DrawableNode)getGraphElement());
    copy._entityName = this._entityName;

    return (copy);
  }


  protected void applyConfiguration() {
    DrawableNode node = (DrawableNode)getGraphElement();

    // apply label change
    FaderUtil.setNodeLabel(node, _entityName);
  }

} // end class "DSGNodeProperties"
