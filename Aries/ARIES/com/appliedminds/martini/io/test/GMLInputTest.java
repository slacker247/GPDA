package com.appliedminds.martini.io.test;


import java.io.ByteArrayInputStream;
import junit.framework.*;
import com.appliedminds.martini.io.GMLInput;
import com.appliedminds.martini.*;
import java.util.Iterator;



/**
 * Basic tests for com.appliedminds.martini.io.GMLInput.
 *
 *
 * @author mathias@apmindsf.com
 */
public class GMLInputTest extends TestCase {


  private static final String GML =
    "graph [\n" +
    "    comment \"BAG GML\"\n" +
    "    directed 1\n" +
    "    id 1\n" +
    "    label \"Hello BAG\"\n" +
    "    data [\n" +
    "        nodetypes [\n" +
    "nodetype0 \"Producer\"\n" +
    "nodetype1 \"Writer\"\n" +
    "nodetype2 \"Director\"\n" +
    "nodetype3 \"Actor\"\n" +
    "nodetype4 \"Western\"\n" +
    "nodetype5 \"War\"\n" +
    "nodetype6 \"Thriller\"\n" +
    "nodetype7 \"Short\"\n" +
    "nodetype8 \"Sci-Fi\"\n" +
    "nodetype9 \"Romance\"\n" +
    "nodetype10 \"Mystery\"\n" +
    "nodetype11 \"Musical\"\n" +
    "nodetype12 \"Horror\"\n" +
    "nodetype13 \"Film-Noir\"\n" +
    "nodetype14 \"Family\"\n" +
    "nodetype15 \"Fantasy\"\n" +
    "nodetype16 \"Drama\"\n" +
    "nodetype17 \"Documentary\"\n" +
    "nodetype18 \"Crime\"\n" +
    "nodetype19 \"Comedy\"\n" +
    "nodetype20 \"Animation\"\n" +
    "nodetype21 \"Adult\"\n" +
    "nodetype22 \"Adventure\"\n" +
    "nodetype23 \"Action\"\n" +
    "nodetype24 \"place\"\n" +
    "nodetype25 \"thing\"\n" +
    "nodetype26 \"genre\"\n" +
    "nodetype27 \"entertainment industry worker\"\n" +
    "nodetype28 \"person\"\n" +
    "        ]\n" +
    "        edgetypes [\n" +
    "edgetype0 \"is related to\"\n" +
    "edgetype1 \"has business relationship with\"\n" +
    "edgetype2 \"is related paper\"\n" +
    "edgetype3 \"starring\"\n" +
    "edgetype4 \"written by\"\n" +
    "edgetype5 \"directed by\"\n" +
    "edgetype6 \"produced by\"\n" +
    "edgetype7 \"is a\"\n" +
    "edgetype8 \"aka\"\n" +
    "edgetype9 \"geographically related to\"\n" +
    "edgetype10 \"web page of\"\n" +
    "edgetype11 \"author\"\n" +
    "edgetype12 \"category\"\n" +
    "edgetype13 \"abstract of\"\n" +
    "edgetype14 \"works for\"\n" +
    "edgetype15 \"citation\"\n" +
    "edgetype16 \"same site\"\n" +
    "edgetype17 \"is near\"\n" +
    "        ]\n" +
    "\n" +
    "    ]\n" +
    "    node [\n" +
    "        id 11\n" +
    "        label \"Mynx, Tiffany\"\n" +
    "        data [\n" +
    "             horizon \"0\"\n" +
    "             rootdx \"1\"\n" +
    "             root \"false\"\n" +
    "             aaid \"reg://838838\"\n" +
    "             typeinfo \"(Director, Actor)\"\n" +
    "        ]\n" +
    "    ]\n" +
    "    node [\n" +
    "        id 9\n" +
    "        label \"Iroc\"\n" +
    "        data [\n" +
    "             horizon \"0\"\n" +
    "             rootdx \"1\"\n" +
    "             root \"false\"\n" +
    "             aaid \"reg://787111\"\n" +
    "             typeinfo \"(Actor)\"\n" +
    "        ]\n" +
    "    ]\n" +
    "    node [\n" +
    "        id 6\n" +
    "        label \"Stryc-9\"\n" +
    "        data [\n" +
    "             horizon \"0\"\n" +
    "             rootdx \"1\"\n" +
    "             root \"false\"\n" +
    "             aaid \"reg://890671\"\n" +
    "             typeinfo \"(Actor)\"\n" +
    "        ]\n" +
    "    ]\n" +
    "    node [\n" +
    "        id 8\n" +
    "        label \"Chandler (I)\"\n" +
    "        data [\n" +
    "             horizon \"0\"\n" +
    "             rootdx \"1\"\n" +
    "             root \"false\"\n" +
    "             aaid \"reg://725004\"\n" +
    "             typeinfo \"(Actor)\"\n" +
    "        ]\n" +
    "    ]\n" +
    "    node [\n" +
    "        id 12\n" +
    "        label \"Darlin, Jessica\"\n" +
    "        data [\n" +
    "             horizon \"0\"\n" +
    "             rootdx \"1\"\n" +
    "             root \"false\"\n" +
    "             aaid \"reg://737148\"\n" +
    "             typeinfo \"(Writer, Director, Actor)\"\n" +
    "        ]\n" +
    "    ]\n" +
    "    node [\n" +
    "        id 5\n" +
    "        label \"Long, Mike (IV)\"\n" +
    "        data [\n" +
    "             horizon \"0\"\n" +
    "             rootdx \"1\"\n" +
    "             root \"false\"\n" +
    "             aaid \"reg://511788\"\n" +
    "             typeinfo \"(Producer, Director, Actor)\"\n" +
    "        ]\n" +
    "    ]\n" +
    "    node [\n" +
    "        id 4\n" +
    "        label \"Damage, Van\"\n" +
    "        data [\n" +
    "             horizon \"0\"\n" +
    "             rootdx \"1\"\n" +
    "             root \"false\"\n" +
    "             aaid \"reg://383711\"\n" +
    "             typeinfo \"(Director, Actor)\"\n" +
    "        ]\n" +
    "    ]\n" +
    "    node [\n" +
    "        id 10\n" +
    "        label \"Hartley, Nina\"\n" +
    "        data [\n" +
    "             horizon \"0\"\n" +
    "             rootdx \"1\"\n" +
    "             root \"false\"\n" +
    "             aaid \"reg://777193\"\n" +
    "             typeinfo \"(Director, Actor)\"\n" +
    "        ]\n" +
    "    ]\n" +
    "    node [\n" +
    "        id 1\n" +
    "        label \"Asswoman in Wonderland (1998)\"\n" +
    "        data [\n" +
    "             horizon \"1\"\n" +
    "             rootdx \"0\"\n" +
    "             root \"true\"\n" +
    "             aaid \"reg://17248\"\n" +
    "             typeinfo \"(Adult)\"\n" +
    "        ]\n" +
    "    ]\n" +
    "    node [\n" +
    "        id 2\n" +
    "        label \"Black, Robert (II)\"\n" +
    "        data [\n" +
    "             horizon \"0\"\n" +
    "             rootdx \"1\"\n" +
    "             root \"false\"\n" +
    "             aaid \"reg://337846\"\n" +
    "             typeinfo \"(Producer, Writer, Director, Actor)\"\n" +
    "        ]\n" +
    "    ]\n" +
    "    node [\n" +
    "        id 7\n" +
    "        label \"Byron, Tom\"\n" +
    "        data [\n" +
    "             horizon \"0\"\n" +
    "             rootdx \"1\"\n" +
    "             root \"false\"\n" +
    "             aaid \"reg://354284\"\n" +
    "             typeinfo \"(Producer, Writer, Director, Actor)\"\n" +
    "        ]\n" +
    "    ]\n" +
    "    node [\n" +
    "        id 3\n" +
    "        label \"Nice, Alexandra\"\n" +
    "        data [\n" +
    "             horizon \"0\"\n" +
    "             rootdx \"1\"\n" +
    "             root \"false\"\n" +
    "             aaid \"reg://841790\"\n" +
    "             typeinfo \"(Actor)\"\n" +
    "        ]\n" +
    "    ]\n" +
    "    edge [\n" +
    "        source 1\n" +
    "        target 2\n" +
    "        label \"produced by\"\n" +
    "    ]\n" +
    "    edge [\n" +
    "        source 1\n" +
    "        target 3\n" +
    "        label \"starring\"\n" +
    "    ]\n" +
    "    edge [\n" +
    "        source 1\n" +
    "        target 4\n" +
    "        label \"starring\"\n" +
    "    ]\n" +
    "    edge [\n" +
    "        source 1\n" +
    "        target 5\n" +
    "        label \"starring\"\n" +
    "    ]\n" +
    "    edge [\n" +
    "        source 1\n" +
    "        target 6\n" +
    "        label \"starring\"\n" +
    "    ]\n" +
    "    edge [\n" +
    "        source 1\n" +
    "        target 7\n" +
    "        label \"starring\"\n" +
    "    ]\n" +
    "    edge [\n" +
    "        source 1\n" +
    "        target 8\n" +
    "        label \"starring\"\n" +
    "    ]\n" +
    "    edge [\n" +
    "        source 1\n" +
    "        target 9\n" +
    "        label \"starring\"\n" +
    "    ]\n" +
    "    edge [\n" +
    "        source 1\n" +
    "        target 10\n" +
    "        label \"starring\"\n" +
    "    ]\n" +
    "    edge [\n" +
    "        source 1\n" +
    "        target 11\n" +
    "        label \"starring\"\n" +
    "    ]\n" +
    "    edge [\n" +
    "        source 1\n" +
    "        target 2\n" +
    "        label \"starring\"\n" +
    "    ]\n" +
    "    edge [\n" +
    "        source 1\n" +
    "        target 11\n" +
    "        label \"directed by\"\n" +
    "    ]\n" +
    "    edge [\n" +
    "        source 1\n" +
    "        target 12\n" +
    "        label \"starring\"\n" +
    "    ]\n" +
    "]\n";



  public GMLInputTest(String name) {
    super(name);
  }



  /**
   * Test the parse routine.  Should be independant of actual
   * implementation of the GMLInput.
   */
  public void testParseGML() {
    try {
      ByteArrayInputStream in = new ByteArrayInputStream(GML.getBytes());
      DrawableGraph g = GMLInput.parseGML(in);

      // Check root
      boolean gotIt = false;
      for(NodeIterator i = g.nodesIterator(); i.hasNext(); ) {
        DrawableNode n = i.next();
        String id = n.getProperty("aaid");
        if ((id != null) && (id.equals("reg://17248"))) {
          assertTrue("Found the same node twice!", !gotIt);
          assertEquals("Failed to translate root property",
                       "true",
                       n.getProperty("root"));
          gotIt = true;
        }
      }
      assertTrue("Failed to locate root node", gotIt);

      // Check an edge.
      gotIt = false;
      for(EdgeIterator i = g.edgesIterator(); i.hasNext(); ) {
        DrawableEdge e = i.next();
        DrawableNode head = e.getHead();
        DrawableNode tail = e.getTail();

        if (head.getProperty("id").equals("8") &&
            tail.getProperty("id").equals("1"))
          {
            assertTrue("Edge found twice!", !gotIt);
            gotIt = true;
          }
      }
      assertTrue("Failed to locate known edge", gotIt);
    }
    catch(Exception e) {
      e.printStackTrace();
      fail("ERR: " + e);
    }
  }

}
