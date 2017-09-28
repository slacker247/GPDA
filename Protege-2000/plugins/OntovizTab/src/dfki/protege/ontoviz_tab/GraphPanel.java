package dfki.protege.ontoviz_tab;

import java.awt.*;
import java.awt.geom.*;
import java.awt.event.*;
import java.util.*;
import java.io.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.tree.*;
import dfki.util.*;
import att.grappa.*;


public class GraphPanel extends JScrollPane implements GrappaConstants {

  GrappaPanel itsGrappaPanel;
  Graph itsGrappaGraph;
  Vector itsSelection;

  public GraphPanel() {
    super(VERTICAL_SCROLLBAR_ALWAYS,
          HORIZONTAL_SCROLLBAR_ALWAYS);
    itsSelection = new Vector();
    itsGrappaGraph = null;
    itsGrappaPanel = null;
    setViewportView(null);
  }


  public void setGraph(Graph graph) {
    // graph may be null
    if (graphIsShowing()) {
      // remove listeners ... ???
    }
    itsGrappaGraph = graph;
    itsSelection.removeAllElements();
    if (itsGrappaGraph == null) {
      itsGrappaPanel = null;
      setViewportView(null);
    } else {
      //itsGrappaGraph.setEditable(true); // no
      //itsGrappaGraph.setMenuable(true); // ???
      itsGrappaPanel = new GrappaPanel(itsGrappaGraph);
      itsGrappaPanel.setScaleToFit(false);
      // itsGrappaPanel.setBackground(Color.white); ????
      setViewportView(itsGrappaPanel);
    }
  }


  public void addGrappaListener(GrappaListener listener) {
    // must be called for each new graph after setGraph!!!
    if (graphIsShowing())
      itsGrappaPanel.addGrappaListener(listener);
  }
    

  public boolean graphIsShowing() {
    return itsGrappaGraph != null;
  }

  public void selectMultiple(Collection ids) {
    if (graphIsShowing()) {
      deselectAll();
      Element someElement = null;
      for (Iterator idIterator = ids.iterator(); idIterator.hasNext();) {
        String id = (String)idIterator.next();
        Element element = selectNodeOrEdge(id);
        if (someElement == null && element != null)
          someElement = element;
      }
      if (someElement != null)
        scrollToCenterPoint(someElement);
      }
  }


  Element selectNodeOrEdge(String id) {
    // find node or edge
    Node node = itsGrappaGraph.findNodeByName(id);
    if (node != null) {
      select(node);
      return node;
    } else {
      Edge edge = itsGrappaGraph.findEdgeByName(id);
      if (edge != null) {
        select(edge);
        return edge;
      }
    }
    return null;
  }


  void scrollToCenterPoint(Element element) {
    GrappaNexus nexus = element.getGrappaNexus();
    Rectangle bounds = nexus.getBounds();
    double centerX = bounds.getCenterX();
    double centerY = bounds.getCenterY();
    Point2D center = new Point2D.Double(centerX, centerY);
    AffineTransform transform = itsGrappaPanel.getTransform();
    // variables with a T suffix denote transformed coordinates
    Point2D centerT = transform.transform(center, null);
    int centerXT = (int)centerT.getX();
    int centerYT = (int)centerT.getY();
    JViewport viewport = getViewport();
    Rectangle viewRect = viewport.getViewRect();
    if (!viewRect.contains(centerXT, centerYT)) {
      Dimension size = viewport.getExtentSize();
      int offsetX = (int)(size.getWidth() / 2);
      int offsetY = (int)(size.getHeight() / 2);
      int x = centerXT-offsetX;
      int y = centerYT-offsetY;
      if (x < 0) x = 0;
      if (y < 0) y = 0;
      viewport.setViewPosition(new Point(x, y));
      validate(); // this CORRECTS large coordinates !!!
    }
  }


  // basic selections

  public void select(Element element) {
    if (graphIsShowing() && !itsSelection.contains(element)) {
      itsSelection.addElement(element);
      GrappaSupport.setHighlight(element, SELECTION_MASK, HIGHLIGHT_ON);
      itsGrappaGraph.repaint();
    }
  }

  public void deselect(Element element) {
    if (graphIsShowing() && itsSelection.contains(element)) {
      itsSelection.removeElement(element);
      GrappaSupport.setHighlight(element, SELECTION_MASK, HIGHLIGHT_OFF);
      itsGrappaGraph.repaint();
    }
  }

  public void toggleSelection(Element element) {
    if (graphIsShowing() && itsSelection.contains(element)) {
      itsSelection.removeElement(element);
      GrappaSupport.setHighlight(element, SELECTION_MASK, HIGHLIGHT_ON);
    } else {
      itsSelection.addElement(element);
      GrappaSupport.setHighlight(element, SELECTION_MASK, HIGHLIGHT_OFF);
    }
    itsGrappaGraph.repaint();
  }

  public void deselectAll() {
    if (graphIsShowing()) {
      for (Iterator elementIterator = itsSelection.iterator();
	   elementIterator.hasNext();) {
        Element element = (Element)elementIterator.next();
        GrappaSupport.setHighlight(element, SELECTION_MASK, HIGHLIGHT_OFF);
      }
      itsSelection.removeAllElements();
    }
  }

}


