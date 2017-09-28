package com.appliedminds.martinix.bluebox;

import java.awt.Color;
import java.awt.Paint;
import java.awt.BasicStroke;
import java.awt.Stroke;
import java.net.URL;
import java.net.MalformedURLException;
import com.appliedminds.martini.MartiniError;



/**
 * All sizes and lengths here are in the abstract "world" coordinate
 * space.
 *
 * @author mathias@apmindsf.com
 */
public class BlueBoxUIPrefs {



  private static final double STROKE_WIDTH = 1.0;


  // Node characteristics
  private static final Color NODE_BORDER_COLOR = new Color(0xB6B6D5);
  private static final Color NODE_FONT_COLOR = new Color(0xFFFFFF);
  private static final Color COLLAPSED_NODE_COLOR = new Color(0x6969AE);
  private static final Color SINGLE_NODE_COLOR = new Color(0x9292CF);
  private static final Color GROUP_NODE_COLOR = new Color(0xA6A6EB);
  private static final Color SITE_COUNT_PILL_FILL = new Color(0xFE863B);
  private static final Color SITE_COUNT_PILL_BORDER = new Color(0xA4A4D8);
  private static final Color SITE_COUNT_PILL_TEXT = new Color(0xFFFFFF);

  // Edge characteristics
  private static final Color EDGE_COLOR = new Color(0x000000);
  private static final Color EDGE_ATTACH_CIRCLE_FILL = new Color(0xFFFFFF);
  private static final double EDGE_STROKE_WIDTH = 1.0;





  /**
   * Get the overall stroke width for the UI.  This is the stroke width
   * used by the majority of the UI elements.
   *
   * @return the overall stroke width.
   */
  public double getUIStrokeWidth() {
    return(STROKE_WIDTH);
  }


  /** 
   * The edge stroke widge.
   *
   * @return the edge stroke width.
   */
  public double getEdgeStrokeWidth() {
    return(EDGE_STROKE_WIDTH);
  }


  /** 
   * A resource path to the true-type font to use for the node text.
   *
   * @return the resource path to a font.
   */
  public String getNodeFontResourcePath() {
    return("com/appliedminds/martinix/bluebox/resources/InterstateRegular.ttf");
  }

  
  public URL getTypeMapURL() {
    try {
      URL dir = getClass().getClassLoader().getResource("com/appliedminds/martinix/bluebox/resources/");
      URL u = new URL(dir.toString() + "typemap.xml");
      return(u);
    }
    catch(MalformedURLException e) {
      throw(new MartiniError("BlueBoxUIPrefs: bad type map url.  Error = " + e));
    }
  }


  /**
   * Collapsed nodes are nodes that contain multiple nodes that
   * are not shown.
   *
   * @return the collapsed node fill color
   */
  public Paint getCollapsedNodeFill() { 
    return(COLLAPSED_NODE_COLOR);
  }


  /**
   * Collapsed nodes are nodes that contain multiple nodes that
   * are not shown.
   *
   * @return the collapsed node stroke color
   */
  public Paint getCollapsedNodeStrokeColor() { 
    return(NODE_BORDER_COLOR);
  }


  /** 
   * Single nodes are nodes that exist outside of any group.
   *
   * @return the single node fill color
   */
  public Paint getSingleNodeFill() { 
    return(SINGLE_NODE_COLOR);
  }

  /** 
   * Single nodes are nodes that exist outside of any group.
   *
   * @return the single node stroke color
   */
  public Paint getSingleNodeStrokeColor() { 
    return(NODE_BORDER_COLOR);
  }



  /**
   * Group nodes are nodes that appear inside a grouping.
   *
   * @return the group node fill color.
   */
  public Paint getGroupNodeFill() { 
    return(GROUP_NODE_COLOR);
  }


  /**
   * Group nodes are nodes that appear inside a grouping.
   *
   * @return the group node fill color.
   */
  public Paint getGroupNodeStrokeColor() { 
    return(NODE_BORDER_COLOR);
  }


  /** 
   * Edges that are drawn with a dashed line will use this spacing.
   *
   * @return the spacing for the dashed line.
   */
  public double getEdgeDashSpacing() {
    return(5.0);
  }


  /**
   * Get the color for all edge lines and arrow heads.
   *
   * @return the color for the edges.
   */
  public Paint getEdgeColor() {
    return(EDGE_COLOR);
  }


  /**
   * Get the default node font size in points.
   *
   * @return the node font size.
   */
  public int getNodeFontSize() {
    return(12);
  }


  /**
   * Get the max number of characters allowed in a node label after which
   * we use an ellipses.
   *
   * @return the max number of chars to display in a node label.
   */
  public int getMaxNodeLabelLength() {
    return(30);
  }


  /**
   * Get the node height.
   *
   * @return the node height.
   */
  public double getNodeHeight() {
    return(18.0);
  }


  /** 
   * Get the font color for text in nodes.
   */
  public Paint getNodeFontColor() {
    return(NODE_FONT_COLOR);
  }


  /**
   * Get the length value for the edge arrow heads.
   *
   * @return the length of the arrow head.
   */
  public double getEdgeArrowHeadLength() {
    return(10.0);
  }


  /** 
   * Get the flair value for edge arrowheads.
   *
   * @return the flair of the arrow head.
   */
  public double getEdgeArrowHeadFlair() {
    return(1.0);
  }


  /** 
   * Edges are attached to nodes by connecting to a little circle,
   * that is the "attach circle".  The radius of that circle is
   * returned here.
   *
   * @return the radius of the attach circle in (as usual) the
   * abstract world dimension.
   */
  public double getEdgeAttachCircleRadius() {
    return(3.0);
  }



  /** 
   * Get the fill used for the edge attach circle.  Edges are attached
   * to nodes by connecting to a little circle, that is the "attach
   * circle".
   *
   * @return the fill for the edge attach circle.
   */
  public Paint getEdgeAttachCircleFill() { 
    return(EDGE_ATTACH_CIRCLE_FILL);
  }


  /**
   * Get the amount of space to put on the left and right of the node
   * text.
   *
   * @return the node text margin value.
   */
  public double getNodeTextMargin() {
    return(10.0);
  }


  /** 
   * Get the amount of space for each side of the icon.
   */
  public double getNodeIconMargin() {
    return(1.0);
  }


  /**
   * Get the icon identification string that will be used for placing
   * a search icon on the edges that have a query string.  The value
   * returned here must be present in the 'typemap.xml' file.
   */
  public String getSearchIconId() {
    return("search");
  }


  /** 
   * Get the desired edge icon height in world coordinates.
   */
  public double getEdgeIconHeight() {
    return(18.0);
  }


  /** 
   * Get the internal left/right margin for the "site count" pills.
   */
  public double getSiteCountInternalLRMargin() {
    return(2.0);
  }



  /** 
   * Get the internal top/bottom margin for the "site count" pills.
   */
  public double getSiteCountInternalTBMargin() {
    return(1.0);
  }


  /** 
   * Get the fill color for the site count pills.
   */
  public Paint getSiteCountFillColor() {
    return(SITE_COUNT_PILL_FILL);
  }


  /** 
   * Get the border color for the site count pills.
   */
  public Paint getSiteCountBorderColor() {
    return(SITE_COUNT_PILL_BORDER);
  }


  /** 
   * Get the fill color for the site count pills.
   */
  public Paint getSiteCountTextColor() {
    return(SITE_COUNT_PILL_TEXT);
  }    


  /**
   * Get the default font size for the site count pills.
   *
   * @return the node font size.
   */
  public int getSiteCountFontSize() {
    return(9);
  }


	/// graph element properties used by bluebox ///
	/**
	 * @return the name of the node type property.
	 */
	public String getNodeTypePropertyName() {
		return BlueBoxUI.PROP_NTYPE;
	}

	/**
	 * @return the "collapsed" node type value.
	 */
	public String getNodeTypeValueCollapsed() {
		return BlueBoxUI.VAL_NTYPE_COLLAPSED;
	}

	/**
	 * @return the "group" node type value.
	 */
	public String getNodeTypeValueGroup() {
		return BlueBoxUI.VAL_NTYPE_GROUP;
	}

	/**
	 * @return the "single" node type value.
	 */
	public String getNodeTypeValueSingle() {
		return BlueBoxUI.VAL_NTYPE_SINGLE;
	}

	/**
	 * @return the name of the visible property.
	 */
	public String getVisiblePropertyName() {
		return BlueBoxUI.PROP_VISIBLE;
	}

	/**
	 * @return the name of the node sequence number property.
	 */
	public String getSequenceNumberPropertyName() {
		return BlueBoxUI.PROP_SEQNUM;
	}

	/**
	 * @return the name of the domain property.
	 */
	public String getDomainPropertyName() {
		return "domain";
	}

	/**
	 * @return the name of the path property.
	 */
	public String getPathPropertyName() {
		return "path";
	}

	public String getQueryStringPropertyName() {
		return "querystr";
	}

	public String getNumberOfKeyValuePairsPropertyName() {
		return "numkvpairs";
	}

	public String getIndexedQueryStringPropertyName(int i) {
		return "qs" + i;
	}

} // end class BlueBoxUIPrefs
