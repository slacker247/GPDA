package com.appliedminds.martini.io;

import com.appliedminds.martini.DrawableEdge;
import com.appliedminds.martini.DrawableGraph;
import com.appliedminds.martini.DrawableGraphContext;
import com.appliedminds.martini.DrawableGraphElement;
import com.appliedminds.martini.DrawableNode;
import com.appliedminds.martini.EdgeIterator;
import com.appliedminds.martini.NodeIterator;
import java.awt.geom.Rectangle2D;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;


/**
 * Can convert a DrawableGraph to a GML form.  Most callers will just use
 * the static method, writeGML().
 *
 * <P>The GML format is described here:
 * http://infosun.fmi.uni-passau.de/Graphlet/GML/
 *
 * <p>In general, if key names or data values are found that are not
 * legal in GML, then they are entered into the output as commented
 * lines.
 *
 *
 *
 * @author mathias@apmindsf.com
 */
public class GMLOutput {

  protected static final String P_LAYOUT_SAVED = "layoutSaved";

  /**
   * The royere hack-- produces GML file that can be parsed by the
   * royere GML parser at the expense of mangleing the GML a bit.
   * This may result in some data being lost.
   *
   * <p>The Royere parser has at least these problems:
   *
   * <ul>
   * <li>It cannot parse comments (lines starting with #).
   * <li>No data field can start with "is".
   * </ul>
   */
  public static final byte ROYERE_HACK = 1;



  private static final byte NO_HACK = 0;


  private byte _hack;
  private int _indent;
  private Writer _out;



  /**
   * Main entry point into the GML generator.  Write a GML representation
   * of the given graph into the given output stream.
   *
   * @param graph the graph to convert to GML.
   * @param out the stream to write the GML to.
   *
   * @throws IOException if there is a stream error OR if the source
   * graph has some incompatability with GML.
   */
  public static void writeGML(DrawableGraph graph, OutputStream out)
    throws IOException
  {
    writeGML(graph, out, NO_HACK, null);
  }


  /**
   * In addition to generating the GML, write the coordinates of the
   * graph and all of its nodes as specified in the graph context
   * object. These coordinates are of course, in WORLD coordinates.
   *
   * @param graph the graph to convert to GML.
   * @param out the stream to write the GML to.
   * @param ctx the graph context that contains the
   */
  public static void writeGML(DrawableGraph graph,
                              OutputStream out,
                              DrawableGraphContext ctx)
    throws IOException
  {
    writeGML(graph, out, NO_HACK, ctx);
  }


  /**
   * Alternate entry point into the GML generator that allows you to
   * specify a hack type to apply.  Writes a GML representation of the
   * given graph into the given output stream using the given hack.
   *
   * @param graph the graph to convert to GML.
   * @param out the stream to write the GML to.
   * @param hack the hack to use (see the constants defined in this class).
   *
   * @throws IOException if there is a stream error OR if the source
   * graph has some incompatability with GML.
   */
  public static void writeGML(DrawableGraph graph,
                              OutputStream out,
                              byte hack)
    throws IOException
  {
    writeGML(graph, out, hack, null);
  }


  public static void writeGML(DrawableGraph graph,
                              OutputStream out,
                              byte hack,
                              DrawableGraphContext ctx)
    throws IOException
  {
    Writer w = new BufferedWriter(new OutputStreamWriter(out, "ASCII"));
    GMLOutput o = new GMLOutput(w);
    o.setHack(hack);
    o.openGraph();
    Date today = new Date();
    o.writeComment("Created by GMLOutput on " + today.toString());
    o.writeDirected(true);
    o.writeId(0);
    o.writeLabel(graph.toString());
    o.writeProps(graph);
    o.writeGraphics(graph, ctx);
    o.writeNodes(graph.nodesIterator(), ctx);
    o.writeEdges(graph.edgesIterator());
    o.closeGraph();
    w.flush();
    w = null;
    o = null;
  }




  /**
   * Constructor.  Callers will usually use the static writeGML method
   * instead of creating one of these directly.
   *
   * @param w the stream that this GMLOutput object will write to.
   */
  public GMLOutput(Writer w) {
    _out = w;
    _indent = 0;
    _hack = NO_HACK;
  }


  /**
   * Set the hack.  The default uses no hack, and no hack is required.
   * See the constants defined in this class.
   */
  public void setHack(byte hack) {
    _hack = hack;
  }


  public void writeNodes(NodeIterator nodes, DrawableGraphContext ctx)
    throws IOException
  {
    //
    // Ensure all nodes have at least an "id" property.
    // If not, assign one for them.
    //

    // find the max id
    List list = new ArrayList();
    int maxId = 1;
    while (nodes.hasNext()) {
      DrawableNode n = nodes.next();
      list.add(n);
      try {
        int id = getNodeId(n);
        if (id > maxId) {
          maxId = id;
        }
      }
      catch (Exception e) { }
    }

    Iterator itr = list.iterator();
    while(itr.hasNext()) {
      DrawableNode n = (DrawableNode) itr.next();
      openNode();

      // give it an id if it does not have one.
      int id = -1;
      try {
        id = getNodeId(n);
      }
      catch (Exception e) {
        id = ++maxId;
        n.setProperty("id", String.valueOf(id));
      }

      writeId(id);

      String lab = getLabel(n);
      if (lab != null) {
        writeLabel(lab);
      }
      writeProps(n);
      writeGraphics(n, ctx);
      closeNode();
    }
  }


  public void writeEdges(EdgeIterator edges) throws IOException {
    while(edges.hasNext()) {
      DrawableEdge e = edges.next();
      openEdge();
      writeSource(getNodeId(e.getTail()));
      writeTarget(getNodeId(e.getHead()));
      String lab = getLabel(e);
      if (lab != null) {
        writeLabel(lab);
      }
      writeProps(e);
      closeEdge();
    }
  }


  public void writeSource(int id) throws IOException {
    _out.write(indent("source "));
    _out.write(Integer.toString(id));
    cr();
  }


  public void writeTarget(int id) throws IOException {
    _out.write(indent("target "));
    _out.write(Integer.toString(id));
    cr();
  }


  public void writeComment(String c) throws IOException {
    _out.write(indent("comment "));
    _out.write(escapeValue(c));
    cr();
  }


  public void writeDirected(boolean isDirected) throws IOException {
    _out.write(indent("directed "));
    _out.write(isDirected ? "1" : "0");
    cr();
  }


  public void writeId( int id) throws IOException {
    _out.write(indent("id "));
    _out.write(Integer.toString(id));
    cr();
  }


  public void writeLayoutSaved(boolean saved) throws IOException {
    _out.write(indent(P_LAYOUT_SAVED + " "));
    _out.write(escapeValue((saved ? "true" : "false")));
    cr();
  }


  public void writeLabel( String lab) throws IOException {
    _out.write(indent("label "));
    _out.write(escapeValue(lab));
    cr();
  }


  public void writeProps( DrawableGraph g) throws IOException {
    List propsToWrite = new ArrayList();
    Iterator i = g.getPropertyKeys();
    while (i.hasNext()) {
      String k = (String) i.next();
      String v = g.getProperty(k);
      if (! (skipProperty(k) || v == null)) {
        if (royereHack()) {
          if (k.startsWith("is") || !checkName(k) || isGraphicsData(k)) {
            continue;
          }
        }
        propsToWrite.add(k);
      }
    }

    i = propsToWrite.iterator();
    if (i.hasNext()) {
      openData();
      while(i.hasNext()) {
        String k = (String) i.next();
        String v = g.getProperty(k);
        writeDataItem(k, v);
      }
      closeData();
    }
  }


  public void writeProps( DrawableGraphElement e) throws IOException {
    List propsToWrite = new ArrayList();
    Iterator i = e.getPropertyKeys();
    while (i.hasNext()) {
      String k = (String) i.next();
      String v = e.getProperty(k);
      if (! (skipProperty(k) || v == null)) {
        if (royereHack()) {
          if (k.startsWith("is") || !checkName(k) || isGraphicsData(k)) {
            continue;
          }
        }
        propsToWrite.add(k);
      }
    }

    i = propsToWrite.iterator();
    if (i.hasNext()) {
      openData();
      while(i.hasNext()) {
        String k = (String) i.next();
        String v = e.getProperty(k);
        writeDataItem(k, v);
      }
      closeData();
    }
  }


  public void writeGraphics(DrawableGraph graph, DrawableGraphContext ctx)
    throws IOException
  {
    if (ctx == null) {
      writeLayoutSaved(false);
      return;
    }

    Rectangle2D bounds = ctx.getOverallBounds(0);

    if (bounds == null) {
      writeLayoutSaved(false);
      return;
    }

    writeLayoutSaved(true);
    writeGraphics(bounds);
  }


  public void writeGraphics(DrawableNode node, DrawableGraphContext ctx)
    throws IOException
  {
    if (ctx == null) {
      return;
    }

    Rectangle2D bounds = ctx.getBounds(node);

    if (bounds == null) {
      return;
    }

    writeGraphics(bounds);
  }


  public void writeGraphics(Rectangle2D bounds) throws IOException {
    double x = bounds.getX();
    double y = bounds.getY();
    double w = bounds.getWidth();
    double h = bounds.getHeight();

    openGraphics();
    writeDataItem("x", String.valueOf(x));
    writeDataItem("y", String.valueOf(y));
    writeDataItem("w", String.valueOf(w));
    writeDataItem("h", String.valueOf(h));
    closeGraphics();
  }


  public void openGraph() throws IOException {
    openBlock("graph");
  }


  public void openData() throws IOException {
    openBlock("data");
  }


  public void openNode() throws IOException {
    openBlock("node");
  }


  public void openEdge() throws IOException {
    openBlock("edge");
  }


  public void openGraphics() throws IOException {
    openBlock("graphics");
  }


  public void writeDataItem(String key, String val) throws IOException {
    if (royereHack()) {
      if (key.startsWith("is")) {
        // Royere interprets isXXX methods as Boolean objects.
        // Don't write graphics related items (x, y, w, h)
        return;
      }
    }

    if (!checkName(key)) {
      if (royereHack()) {
        return; // bail
      }

      // The name is invalid so just comment it out in the output.
      _out.write("# ");
    }

    _out.write(indent(key));
    _out.write(" ");
    _out.write(escapeValue(val));
    cr();
  }


  public void closeGraph() throws IOException {
    closeBlock();
  }


  public void closeData() throws IOException {
    closeBlock();
  }


  public void closeEdge() throws IOException {
    closeBlock();
  }


  public void closeNode() throws IOException {
    closeBlock();
  }


  public void closeGraphics() throws IOException {
    closeBlock();
  }


  private boolean commentsSupported() {
    return(! royereHack());
  }


  private boolean royereHack() {
    return(_hack == ROYERE_HACK);
  }


  private void closeBlock() throws IOException {
    -- _indent;
    _out.write(indent("]"));
    cr();
  }


  private void cr() throws IOException {
    _out.write("\n");
  }


  private String indent(String str) {
    StringBuffer buf = new StringBuffer();
    for(int i=_indent; i > 0; --i) {
      buf.append("  ");
    }
    buf.append(str);
    return(buf.toString());
  }


  /*
   * Convert a value into a valid GML string.
   */
  private String escapeValue(String str) {
    // FIX: this should also check for pre-existing occurances of double-quote.
    return("\"" + str + "\"");
  }


  /*
   * Check to make sure that a GML field name is valid.
   */
  private boolean checkName(String str) throws IOException {
    // no spaces (' ') or '_' allowed
    return(str.trim().indexOf(" ") == -1 && str.indexOf("_") == -1);
  }


  private boolean isGraphicsData(String str) {
    return ("x".equals(str) || "y".equals(str) ||
            "w".equals(str) || "h".equals(str));
  }


  private int getNodeId(DrawableNode n) throws IOException {
    String idstr = n.getProperty("id");
    if (idstr == null) {
      throw(new IOException("Node without id."));
    }
    try {
      return(Integer.parseInt(idstr));
    }
    catch(NumberFormatException e) {
      throw(new IOException("Non-integer node id: " + idstr));
    }
  }


  private String getLabel(DrawableGraphElement el) {
    return(el.getProperty("label"));
  }


  private void openBlock( String name) throws IOException {
    _out.write(indent(name));
    _out.write(" [");
    cr();
    ++_indent;
  }


  /*
   * Some properties are not written to the output.
   */
  private boolean skipProperty(String prop) {
    //
    // Do not allow the id property to be written since giving it
    // a string value will break the royere parser.
    //
    return(prop.equals("id"));
  }


}
