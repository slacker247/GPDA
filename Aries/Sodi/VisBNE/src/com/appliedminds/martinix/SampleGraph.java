package com.appliedminds.martinix;


import com.appliedminds.martini.*;
import com.appliedminds.martini.layout.*;
import com.appliedminds.martinix.greenpill.*;
import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.Graphics;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.AdjustmentEvent;
import java.awt.event.AdjustmentListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLayeredPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JViewport;
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;



public class SampleGraph {

  private static JFrame _frame;
  private static GraphPanel _gpanel;
  private static JScrollPane _scrollPane;
  private static PanAndScanPanel _box;


  protected static DrawableGraph createTestGraph()
  {
    DrawableGraph dg = new DrawableGraph();

    DrawableNode node1 = dg.createNode();
    node1.setProperty(GreenPillUIPrefs.getNodeLabelPropertyName(), "node1");
    node1.setProperty(GreenPillUIPrefs.getSelectedPropertyName(), "false");
    node1.setProperty(GreenPillUIPrefs.getDistanceFromRootPropertyName(), "0");
    node1.setProperty(GreenPillUIPrefs.getRootNodePropertyName(), "true");

    DrawableNode node2 = dg.createNode();
    node2.setProperty(GreenPillUIPrefs.getNodeLabelPropertyName(), "node2sdfksdfds");
    node2.setProperty(GreenPillUIPrefs.getSelectedPropertyName(), "false");
    node2.setProperty(GreenPillUIPrefs.getDistanceFromRootPropertyName(), "1");
    node2.setProperty(GreenPillUIPrefs.getRootNodePropertyName(), "false");

    DrawableNode node3 = dg.createNode();
    node3.setProperty(GreenPillUIPrefs.getNodeLabelPropertyName(), "node 3");
    node3.setProperty(GreenPillUIPrefs.getSelectedPropertyName(), "false");
    node3.setProperty(GreenPillUIPrefs.getDistanceFromRootPropertyName(), "1");
    node3.setProperty(GreenPillUIPrefs.getRootNodePropertyName(), "false");

    DrawableEdge edge = dg.createEdge(node1, node2);
    CollapsedEdge ce = CollapsedEdge.getCollapsedEdge(edge);
    ce.addSubEdge(CollapsedEdge.HEAD_DIRECTION, "lalala", true);

    DrawableEdge edge2 = dg.createEdge(node1, node3);
    ce = CollapsedEdge.getCollapsedEdge(edge);
    ce.addSubEdge(CollapsedEdge.TAIL_DIRECTION, "blah", true);

    return dg;
  }


  public static void main(String args[])
  {
    // test the whole thing
    _gpanel = new GraphPanel(new GreenPillUI());

    DrawableGraph g = createTestGraph();
    _gpanel.setDrawableGraph(g);

    GraphLayout layout = new DefaultGraphLayout();
    _gpanel.setLayout(layout);
    _gpanel.setScale(5.0);
    _gpanel.setBackground(Color.red);
    _gpanel.setBorder(BorderFactory.createLineBorder(Color.green));

    _gpanel.addDrawableGraphMouseEventListener
      ("NODE",
       new DrawableGraphMouseEventListener() {
         public boolean handleDrawableGraphMouseEvent
           (DrawableGraphMouseEvent e)
         {
           System.out.println("Got node event: " + e.toString());
           return(false);
         }
       });

    _gpanel.addDrawableGraphMouseEventListener
      ("NODE_TEXT",
       new DrawableGraphMouseEventListener() {
         public boolean handleDrawableGraphMouseEvent
           (DrawableGraphMouseEvent e)
         {
           System.out.println("Got node-text event: " + e.toString());
           return(false);
         }
       });

    _gpanel.addDrawableGraphMouseEventListener
      ("EDGE",
       new DrawableGraphMouseEventListener() {
         public boolean handleDrawableGraphMouseEvent
           (DrawableGraphMouseEvent e)
         {
           System.out.println("Got edge event: " + e.toString());
           return(false);
         }
       });

    _gpanel.addDrawableGraphMouseEventListener
      ("EDGE_BUBBLE",
       new DrawableGraphMouseEventListener() {
         public boolean handleDrawableGraphMouseEvent
           (DrawableGraphMouseEvent e)
         {
           System.out.println("Got edge bubble event: " + e.toString());
           return(false);
         }
       });

    _gpanel.addDrawableGraphMouseEventListener
      (GraphPanel.OFFGRAPH_DECORATION_ID,
       new DrawableGraphMouseEventListener() {
         public boolean handleDrawableGraphMouseEvent
           (DrawableGraphMouseEvent e)
         {
           System.out.println("Got off-graph event: " + e.toString());
           return(false);
         }
       });


    _scrollPane = new JScrollPane(_gpanel);
    _scrollPane.setPreferredSize(new Dimension(400,400));
    _scrollPane.setViewportBorder(BorderFactory.createLineBorder(Color.orange));

    _scrollPane.getViewport().setBackground(Color.yellow);

    JRadioButton button = new JRadioButton("small");
    button.addActionListener(new ZoomListener(_gpanel));

    JSplitPane spane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
                                      _scrollPane, button);
    spane.setDividerLocation(300);

    WindowListener l = new WindowAdapter() {
        public void windowClosing(WindowEvent e) {System.exit(0);}
      };

    _box = new PanAndScanPanel(100,
                               100,
                               _gpanel,
                               new Color(206, 255, 238),
                               new Color(0xFE, 0x86, 0x3B));

    ViewportPanAndScanAdapter ada =
      new ViewportPanAndScanAdapter(_gpanel,
                                    _scrollPane.getViewport(),
                                    _box);
    _frame = new JFrame("MartiniTest");
    _frame.addWindowListener(l);
    _frame.getContentPane().add(spane);
    _frame.getLayeredPane().add(_box, JLayeredPane.PALETTE_LAYER);
    _frame.setSize(300,300);


    _scrollPane.addComponentListener
      (new ComponentListener() {
          public void componentMoved(ComponentEvent e) { }
          public void componentHidden(ComponentEvent e) { }
          public void componentShown(ComponentEvent e) {
            positionBox();
          }
          public void componentResized(ComponentEvent e) {
            positionBox();
          }
        });



    _frame.pack();
    _frame.setVisible(true);

    Point p = _scrollPane.getViewport().getLocation();
    System.err.println("viewport location: " +
                       SwingUtilities.convertPoint(_scrollPane.getViewport(),
                                                   p, _frame));

    p = _gpanel.getLocation();
    System.err.println("graph panel location: " +
                       SwingUtilities.convertPoint(_gpanel,p, _frame));

    positionBox();
  }



  /*
   * Try to keep the box in the lower left-hand corner.
   */
  private static void positionBox() {
    Insets insets = _frame.getContentPane().getInsets();

    _box.setBounds
      (insets.left + 5,
       (int) (_scrollPane.getViewportBorderBounds().getHeight() -
              _box.getHeight()),
       _box.getWidth(),
       _box.getHeight());
  }



  static class ZoomListener implements ActionListener {
    private GraphPanel _panel;

    public ZoomListener(GraphPanel p) {
      _panel = p;
    }

    public void actionPerformed(ActionEvent e) {
      if (((JRadioButton)e.getSource()).isSelected())
        _panel.setScale(1.0);
      else
        _panel.setScale(5.0);
      _frame.getContentPane().repaint();
    }
  }


  /*
   * A dummy transparent panel.
   */
  private static class TransP extends JPanel {
    private static final Color COLOR  = new Color(206,255,238);

    public TransP() {
      super();
      setOpaque(false);
    }

    public void paintComponent(Graphics g) {
      Graphics2D g2 = (Graphics2D) g;
      Dimension d = getSize();
      Rectangle2D rect =
        new Rectangle2D.Double(0,0,d.getWidth(), d.getHeight());
      g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER,
                                                 0.4f));
      g2.setColor(COLOR);
      g2.fill(rect);
    }
  }





} // end class SampleGraph
