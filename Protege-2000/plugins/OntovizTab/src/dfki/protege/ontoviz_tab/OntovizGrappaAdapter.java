package dfki.protege.ontoviz_tab;

import java.awt.*;
import java.awt.geom.*;
import java.awt.event.*;
import javax.swing.*;
import java.util.*;
import att.grappa.*;


public class OntovizGrappaAdapter implements GrappaConstants, GrappaListener {

  OntovizTab itsTab;

  public OntovizGrappaAdapter(OntovizTab tab) {
    itsTab = tab;
  }


  /**
   * The method called when a single mouse click occurs on a displayed subgraph.
   *
   * @param subg displayed subgraph where action occurred
   * @param elem subgraph element in which action occurred
   * @param pt the point where the action occurred (graph coordinates)
   * @param modifiers mouse modifiers in effect
   * @param panel specific panel where the action occurred
   */
    public void grappaClicked(Subgraph subg, Element elem, GrappaPoint pt, int modifiers, GrappaPanel panel) {

      if (elem != null && elem.isNode()) {
	itsTab.makeClassVisible(elem.getName());
      }

    }


  /**
   * The method called when a mouse press occurs on a displayed subgraph.
   *
   * @param subg displayed subgraph where action occurred
   * @param elem subgraph element in which action occurred
   * @param pt the point where the action occurred (graph coordinates)
   * @param modifiers mouse modifiers in effect
   * @param panel specific panel where the action occurred
   */
  public void grappaPressed(Subgraph subg, Element elem, GrappaPoint pt, 
      int modifiers, GrappaPanel panel) {

  if ((modifiers&(InputEvent.BUTTON2_MASK|InputEvent.BUTTON3_MASK)) != 0 
      && (modifiers&(InputEvent.BUTTON2_MASK|InputEvent.BUTTON3_MASK)) 
	 == modifiers) {

	    // pop-up menu if button2 or button3
	    JPopupMenu popup = new JPopupMenu();
	    JMenuItem item = null;

	    popup.add(item = new JMenuItem("zoom in"));
	    item.addActionListener(new ActionListener() {
	      public void actionPerformed(ActionEvent ae) {
		GrappaPanel gp = itsTab.itsGraphPanel.itsGrappaPanel;
		gp.multiplyScaleFactor(1.25);
		gp.repaint();
	      }
	    });

	    popup.add(item = new JMenuItem("zoom out"));
	    item.addActionListener(new ActionListener() {
	      public void actionPerformed(ActionEvent ae) {
		GrappaPanel gp = itsTab.itsGraphPanel.itsGrappaPanel;
		gp.multiplyScaleFactor(0.8);
		gp.repaint();
	      }
	    });

	    popup.add(item = new JMenuItem("zoom in fast"));
	    item.addActionListener(new ActionListener() {
	      public void actionPerformed(ActionEvent ae) {
		GrappaPanel gp = itsTab.itsGraphPanel.itsGrappaPanel;
		gp.multiplyScaleFactor(2.0);
		gp.repaint();
	      }
	    });

	    popup.add(item = new JMenuItem("zoom out fast"));
	    item.addActionListener(new ActionListener() {
	      public void actionPerformed(ActionEvent ae) {
		GrappaPanel gp = itsTab.itsGraphPanel.itsGrappaPanel;
		gp.multiplyScaleFactor(0.5);
		gp.repaint();
	      }
	    });

	    popup.add(item = new JMenuItem("reset zoom"));
	    item.addActionListener(new ActionListener() {
	      public void actionPerformed(ActionEvent ae) {
		GrappaPanel gp = itsTab.itsGraphPanel.itsGrappaPanel;
		gp.resetZoom();
		gp.repaint();
	      }
	    });

	    /* does not work correctly (zooming disabled!)
	    popup.add(item = new JMenuItem("scale to fit"));
	    item.addActionListener(new ActionListener() {
	      public void actionPerformed(ActionEvent ae) {
		GraphPanel graphPanel = itsTab.itsGraphPanel;
		GrappaPanel gp = graphPanel.itsGrappaPanel;
		gp.setScaleToSize(graphPanel.getViewport().getSize());
		gp.repaint();
		// gp.setScaleToFit(false);
	      }
	    });
	    */

	    java.awt.geom.Point2D mpt = panel.getTransform().transform(pt,null);
	    popup.show(panel, (int)mpt.getX(), (int)mpt.getY());

	}

  }


  /**
   * The method called when a mouse release occurs on a displayed subgraph.
   *
   * @param subg displayed subgraph where action occurred
   * @param elem subgraph element in which action occurred
   * @param pt the point where the action occurred (graph coordinates)
   * @param modifiers mouse modifiers in effect
   * @param pressedElem subgraph element in which the most recent mouse press occurred
   * @param pressedPt the point where the most recent mouse press occurred (graph coordinates)
   * @param pressedModifiers mouse modifiers in effect when the most recent mouse press occurred
   * @param outline enclosing box specification from the previous drag position (for XOR reset purposes)
   * @param panel specific panel where the action occurred
   */
    public void grappaReleased(Subgraph subg, Element elem, GrappaPoint pt, int modifiers, Element pressedElem, GrappaPoint pressedPt, int pressedModifiers, GrappaBox outline, GrappaPanel panel) {
    }


  /**
   * The method called when a mouse drag occurs on a displayed subgraph.
   *
   * @param subg displayed subgraph where action occurred
   * @param currentPt the current drag point
   * @param currentModifiers the current drag mouse modifiers
   * @param pressedElem subgraph element in which the most recent mouse press occurred
   * @param pressedPt the point where the most recent mouse press occurred (graph coordinates)
   * @param pressedModifiers mouse modifiers in effect when the most recent mouse press occurred
   * @param outline enclosing box specification from the previous drag position (for XOR reset purposes)
   * @param panel specific panel where the action occurred
   */
    public void grappaDragged(Subgraph subg, GrappaPoint currentPt, int currentModifiers, Element pressedElem, GrappaPoint pressedPt, int pressedModifiers, GrappaBox outline, GrappaPanel panel) {
    }


  /**
   * The method called when a element tooltip is needed.
   *
   * @param subg displayed subgraph where action occurred
   * @param elem subgraph element in which action occurred
   * @param pt the point where the action occurred (graph coordinates)
   * @param modifiers mouse modifiers in effect
   * @param panel specific panel where the action occurred
   *
   * @return the tip to be displayed or null; in this implementation,
   * if the mouse is in a graph element that
   * has its <I>tip</I> attribute defined, then that text is returned.
   * If that attribute is not set, the element name is returned.
   * If the mouse is outside the graph bounds, then the text supplied
   * to the graph setToolTipText method is supplied.
   */
    public String grappaTip(Subgraph subg, Element elem, GrappaPoint pt, int modifiers, GrappaPanel panel) {

	return null;
    }


}


