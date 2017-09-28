/*
 * AccessKB.java
 *
 * Created on March 26, 2003, 11:28 AM
 */

package com.appliedminds.jam;

/**
 *
 * @author  jeffmac
 */

import java.awt.*;
import javax.swing.*;
import java.awt.event.*;
import java.io.File;
import java.util.Vector;
import java.util.HashMap;
import java.util.Iterator;

import DBConnect.*;
import com.appliedminds.martini.*;
import royere.cwi.structure.*;

public class AccessKBDlg extends JDialog 
{

    private JPanel panel1 = new JPanel();
    private JButton jCancelBTN = new JButton();
    private JButton jOKBTN = new JButton();
    private JLabel jlb_Domains = new JLabel();
    private JLabel jlb_Stories = new JLabel();
    private JList jlst_Domains = new JList();
    private JScrollPane jscrl_Domains = new JScrollPane();
    private JList jlst_Stories = new JList();
    private JScrollPane jscrl_Stories = new JScrollPane();
    public DBConnect test = new DBConnect();
    
    private boolean debug = false;
    public boolean OK = false;

    public String Domain = "CND";
    public String StoryName = "";
    private Graph Network = new Graph();
    private Node A_Node[] = new Node[100];
    private Edge An_Edge[] = new Edge[100];

    /** Creates a new instance of AccessKB */
    public AccessKBDlg(Frame frame, String title, boolean modal) 
    {
        super(frame, title, modal);
        try 
        {
          jbInit();
          pack();
        }
        catch(Exception ex) 
        {
          ex.printStackTrace();
        }

        //Center the window
        this.setSize(400, 340);
        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        Dimension frameSize = this.getSize();
        if (frameSize.height > screenSize.height) {
          frameSize.height = screenSize.height;
        }
        if (frameSize.width > screenSize.width) {
          frameSize.width = screenSize.width;
        }
        this.setLocation((screenSize.width - frameSize.width) / 2, (screenSize.height - frameSize.height) / 2);
    }

    public AccessKBDlg() 
    {
      this(null, "KB Stories", false);
    }
    
    private void jbInit() throws Exception 
    {
      panel1.setLayout(null);
      jCancelBTN.setBounds(new Rectangle(210, 270, 95, 23));
      jCancelBTN.setText("Cancel");
      jCancelBTN.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(ActionEvent e) 
        {
            jCancelBTN_actionPerformed(e);
        }
      });
      jOKBTN.setBounds(new Rectangle(100, 270, 95, 23));
      jOKBTN.setText("OK");
      jOKBTN.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(ActionEvent e) 
        {
          jOKBTN_actionPerformed(e);
        }
      });
      jlst_Domains.addMouseListener(new java.awt.event.MouseAdapter() {
          public void mouseClicked(java.awt.event.MouseEvent evt) {
              jlst_DomainsMouseClicked(evt);
          }
      });
      jlb_Domains.setText("Domains:");
      jlb_Domains.setBounds(10, 10, 100, 28);
      jlb_Stories.setText("Stories:");
      jlb_Stories.setBounds(150, 10, 250, 28);
      jscrl_Stories.setBounds(150, 30, 230, 200);
      jscrl_Domains.setBounds(10, 30, 100, 200);
      getContentPane().add(panel1);
      panel1.add(jCancelBTN, null);
      panel1.add(jOKBTN, null);
      jscrl_Domains.setViewportView(jlst_Domains);
      jscrl_Stories.setViewportView(jlst_Stories);
      panel1.add(jscrl_Domains, null);
      panel1.add(jscrl_Stories, null);
      panel1.add(jlb_Domains, null);
      panel1.add(jlb_Stories, null);
    }

    private void jlst_DomainsMouseClicked(java.awt.event.MouseEvent evt)
    {
        jlst_Stories.removeAll();
        Domain = jlst_Domains.getSelectedValue().toString().replace('[', ' ').replace(']', ' ').trim();
        test.configuration.setMission(Domain);
        Vector v = test.runQuery("Show tables;");
        Vector v2 = (Vector)v.elementAt(0);
        jlst_Stories.setListData(v);

        this.setVisible(true);
    }

    void jOKBTN_actionPerformed(ActionEvent e)
    {
      //Globally set the story name
      StoryName = jlst_Stories.getSelectedValue().toString().replace('[', ' ').replace(']', ' ').trim();
      
      OK = true;
      this.setVisible(false);
    }

    void jCancelBTN_actionPerformed(ActionEvent e)
    {
      this.setVisible(false);
    }

    public void showKB()
    {
        
        Vector v = test.runQuery("Show databases;");
        Vector v2 = (Vector)v.elementAt(0);
        jlst_Domains.setListData(v);

        this.setVisible(true);
    }
    
    public DrawableGraph StoryToGMLFile()
    {
        test.configuration.setMission(Domain);
        Network.setLabel(StoryName);
        Network.setProperty("directed", "1");
        Network.setProperty("layoutSaved", "true");
        Network.setId(0);
        ConvertToDG GtoDG = null;
        Vector v2 = null;
        //Output the story structure to a file and return the file name 
        Vector v = test.runQuery("Select * from `" + StoryName + "`;");
        
        /* Take all the Hypothesies from the KB and convert them to nodes
         */
        int T_Nodes = 0;
        for(T_Nodes = 0; T_Nodes < v.size(); T_Nodes++)
        {
            v2 = (Vector)v.elementAt(T_Nodes);
            {
                A_Node[T_Nodes] = new Node();
                A_Node[T_Nodes].setId(T_Nodes + 1);
                A_Node[T_Nodes].setLabel(v2.elementAt(7).toString());
                A_Node[T_Nodes].setProperty("selected", "false");
                A_Node[T_Nodes].setProperty("inEdit", "false");
                A_Node[T_Nodes].setProperty("sliderToggleVisible", "false");
                A_Node[T_Nodes].setProperty("manual", "false");
                A_Node[T_Nodes].setProperty("sliderVisible", "false");
                String sztemp = "";
                try{
                    sztemp = String.valueOf((int)(Float.valueOf(v2.elementAt(5).toString()).floatValue() * (float)100));
                }catch(Exception e){e.printStackTrace();}
                
                java.util.Random r = new java.util.Random();
                r.setSeed(System.currentTimeMillis());
                int m = java.lang.Math.abs(r.nextInt());
                try{
                    m = Integer.valueOf(String.valueOf(m).substring((String.valueOf(m).length()-2),
                            String.valueOf(m).length())).intValue();
                }catch(Exception e){e.printStackTrace();}
                System.out.println("Value: " + m);
                if(m > 100 || m < 0)
                    m = 78;

                try{
                    if(Float.valueOf(sztemp).floatValue() == 0.0)
                        sztemp = String.valueOf(m);
                }catch(Exception e)
                {
                    sztemp = String.valueOf(m);
                    e.printStackTrace();
                }
                
                A_Node[T_Nodes].setProperty("sliderValue", sztemp);
                A_Node[T_Nodes].setProperty("connectorHubsVisible", "false");
                A_Node[T_Nodes].setProperty("label", v2.elementAt(7).toString());
                A_Node[T_Nodes].setProperty("SouthConnectorHubSelected", "false");
                A_Node[T_Nodes].setProperty("creatingEdge", "false");
                A_Node[T_Nodes].setProperty("level", v2.elementAt(8).toString());

/*                A_Node[T_Nodes].setProperty("Link_Labels", v2.elementAt(1).toString());
                A_Node[T_Nodes].setProperty("Parents", v2.elementAt(2).toString());
                A_Node[T_Nodes].setProperty("B_Weights", v2.elementAt(3).toString());
                A_Node[T_Nodes].setProperty("D_Weights", v2.elementAt(4).toString());
*/
                A_Node[T_Nodes].setProperty("Belief", v2.elementAt(5).toString());
                A_Node[T_Nodes].setProperty("Disbelief", v2.elementAt(6).toString());
                A_Node[T_Nodes].setProperty("Phrase", v2.elementAt(9).toString());
                A_Node[T_Nodes].setProperty("Threshold", v2.elementAt(10).toString());
                A_Node[T_Nodes].setProperty("Cutoff", v2.elementAt(11).toString());
                A_Node[T_Nodes].setProperty("y", "-88");
                A_Node[T_Nodes].setProperty("x", "34.5");
                A_Node[T_Nodes].setProperty("w", "225");
                A_Node[T_Nodes].setProperty("h", "144");
            }
        }
        if(!v.isEmpty())
            Network.setProperty("Levels", ((Vector)v.elementAt(0)).elementAt(0).toString());
        /* Create all the links between the nodes in the An_Edge Array
         */
        int EdgeCount = 0;
        for(int x = 0; x < v.size(); x++)
        {
            v2 = (Vector)v.elementAt(x);
            String Parents = (String)v2.elementAt(2);
            String Links = (String)v2.elementAt(3); //Belief link weights
            String Labels = (String)v2.elementAt(1);
            String DLinks = (String)v2.elementAt(4);
            if(Parents.equals((String)""))
            {
                continue;
            }
            //Breaks the Parents string into each node label
            String Parent[] = new String[100];
            String Link_Wieghts[] = new String[100];
            String Link_Labels[] = new String[100];
            String Link_D[] = new String[100];
            for(int t = 0; t < 100; t++)
                Link_D[t] = Link_Labels[t] = Link_Wieghts[t] = Parent[t] = "";
            
            int nextP = 0;
            for(int i = 0; i < Parents.length(); i++)
            {
                if(',' == Parents.charAt(i))
                {
                    nextP++;
                    continue;
                }
                Parent[nextP] += Parents.charAt(i);
            }

            int nextLabel = 0;
            for(int i = 0; i < Labels.length(); i++)
            {
                if(',' == Labels.charAt(i))
                {
                    nextLabel++;
                    continue;
                }
                Link_Labels[nextLabel] += Labels.charAt(i);
            }

            int nextDLink = 0;
            for(int i = 0; i < DLinks.length(); i++)
            {
                if(',' == DLinks.charAt(i))
                {
                    nextDLink++;
                    continue;
                }
                Link_D[nextDLink] += DLinks.charAt(i);
            }

            int nextL = 0;
            for(int i = 0; i < Links.length(); i++)
            {
                if(',' == Links.charAt(i))
                {
                    nextL++;
                    continue;
                }
                Link_Wieghts[nextL] += Links.charAt(i);
            }
            //Takes each node label and finds the node within the A_Node array
            // and creates an edge.
            for(int p = 0; p <= nextP; p++)
            {
                for(int row = 0; row < T_Nodes; row++)
                {
                    String NodeName = A_Node[row].getLabel();
                    if(NodeName.equals(Parent[p]) && x < T_Nodes)
                    {
                        An_Edge[EdgeCount] = new Edge(A_Node[row], A_Node[x]);
                        String sztemp = "";
                        try{
                            sztemp = String.valueOf((int)(Float.valueOf(Link_Wieghts[p]).floatValue() * (float)100));
                        }catch(Exception e){e.printStackTrace();}
                        An_Edge[EdgeCount].setProperty("sliderValue", sztemp);
                        An_Edge[EdgeCount].setProperty("sliderVisible", "false");
                        An_Edge[EdgeCount].setProperty("Link_Label", Link_Labels[p]);
                        An_Edge[EdgeCount].setProperty("B_Weight", Link_Wieghts[p]);
                        An_Edge[EdgeCount].setProperty("D_Weight", Link_D[p]);
                        EdgeCount++;
                        break;
                    }
                }
            }
                
        }
        Network.add(A_Node);
        Network.add(An_Edge);
        try
        {
            return GtoDG.parseGraph(Network);
        }catch(Exception ioe)
        {
            ioe.printStackTrace();
        }
        return null;
    }
    
    public void DrawableGraphToStory(DrawableGraph dg, String Story, String DomainName)
    {
        Domain = DomainName;
        test.configuration.setMission(DomainName);
        StoryName = Story;
        int LevelCount = 0;
        DrawableNode tempNode = new DrawableNode();
        DrawableNode HeadNode = new DrawableNode();
        NodeIterator NodeItr = dg.nodesIterator();
        NodeIterator HeadNodeItr = null;
        String Links[] = new String[4];
        String Levels[] = new String[20];
        for(int i = 0; i < 4; i++)
            Links[i] = "";
        for(int i = 0; i < 20; i++)
            Levels[i] = "";

        test.executeQuery("DELETE FROM " + StoryName + ";");

        while(NodeItr.hasNext())
        {
            tempNode = (DrawableNode)(NodeItr.next());

            // Search for the links that match to the Head Node values.
            EdgeIterator EdgeItr = dg.edgesIterator();
            while(EdgeItr.hasNext())
            {
                DrawableEdge tempEdge = (DrawableEdge)(EdgeItr.next());
                String head = ((DrawableNode)(tempEdge.getHead())).getProperty("label");
                if(debug) System.out.println("Head of Edge: " + head);
                if(head.equalsIgnoreCase(tempNode.getProperty("label")))
                {
                    String tail = ((DrawableNode)(tempEdge.getTail())).getProperty("label");
                    System.out.println("Tail of Edge: " + tail);
                    Links[0] += tempEdge.getProperty("Link_Label") + ",";
                    Links[1] += ((DrawableNode)(tempEdge.getTail())).getProperty("label") + ",";
                    Links[2] += tempEdge.getProperty("B_Weight") + ",";
                    Links[3] += tempEdge.getProperty("D_Weight") + ",";
                }
            }
            if(!Links[0].equalsIgnoreCase(""))
            {
                Links[0] = Links[0].substring(0, Links[0].length()-1);
                Links[1] = Links[1].substring(0, Links[1].length()-1);
                Links[2] = Links[2].substring(0, Links[2].length()-1);
                Links[3] = Links[3].substring(0, Links[3].length()-1);
            }

            String msg = "\'Temp\', \'" + Links[0] + "\', \'" +
                                         Links[1] + "\', \'" +
                                         Links[2] + "\', \'" +
                                         Links[3] + "\', \'" +
                                         tempNode.getProperty("Belief") + "\', \'" +
                                         tempNode.getProperty("Disbelief") + "\', \'" +
                                         tempNode.getProperty("label") + "\', \'" +
                                         tempNode.getProperty("level") + "\', \'" +
                                         tempNode.getProperty("Phrase") + "\', \'" +
                                         tempNode.getProperty("Threshold") + "\', \'" +
                                         tempNode.getProperty("Cutoff") + "\'";
            boolean LevelThere = false;
            for(int q = 0; q < LevelCount; q++)
            {
                if(Levels[q].equalsIgnoreCase(tempNode.getProperty("level")))
                {
                    LevelThere = true;
                    break;
                }
            }
            if(!LevelThere)
            {
                Levels[LevelCount] = tempNode.getProperty("level");
                LevelCount++;
            }
             System.out.println("Mysql Statement: " + msg);
            for(int i = 0; i < 4; i++)
                Links[i] = "";
            test.executeQuery("INSERT INTO " + StoryName + " Values(" + msg + ");");
        }
        String Level = "";
        for(int x = 0; x < LevelCount; x++)
        {
            Level += Levels[x] + ",";
        }
        System.out.println("Levels: " + Level);
        test.executeQuery("UPDATE " + StoryName + " SET LEVEL_LABELS=\'" + Level.substring(0, Level.length()-1) + "\';");
    }
    
    public void SubGraphToStory(DrawableGraph dg, String Story, String DomainName,
                                int startNode, int endNode)
    {
        Domain = DomainName;
        test.configuration.setMission(DomainName);
        StoryName = Story;
        int LevelCount = 0;
        int NodeNo = -1;
        DrawableNode tempNode = new DrawableNode();
        DrawableNode HeadNode = new DrawableNode();
        NodeIterator NodeItr = dg.nodesIterator();
        NodeIterator HeadNodeItr = null;
        String Links[] = new String[4];
        String Levels[] = new String[20];
        //
        for(int i = 0; i < 4; i++)
            Links[i] = "";
        for(int i = 0; i < 20; i++)
            Levels[i] = ""; //Integer.toString(i);

        test.executeQuery("DELETE FROM " + StoryName + ";");

        while(NodeItr.hasNext())
        {
          tempNode = (DrawableNode)(NodeItr.next());
          NodeNo++;
          //if ((NodeNo >= startNode) && (NodeNo <= endNode)) {

            // Search for the links that match to the Head Node values.
            EdgeIterator EdgeItr = dg.edgesIterator();
            while(EdgeItr.hasNext())
            {
                DrawableEdge tempEdge = (DrawableEdge)(EdgeItr.next());
                String head = ((DrawableNode)(tempEdge.getHead())).getProperty("label");
                if(debug) System.out.println("Head of Edge: " + head);
                if(head.equalsIgnoreCase(tempNode.getProperty("label")))
                {
                    String tail = ((DrawableNode)(tempEdge.getTail())).getProperty("label");
                    System.out.println("Tail of Edge: " + tail);
                    Links[0] += tempEdge.getProperty("Link_Label") + ",";
                    Links[1] += ((DrawableNode)(tempEdge.getTail())).getProperty("label") + ",";
                    Links[2] += tempEdge.getProperty("B_Weight") + ",";
                    Links[3] += tempEdge.getProperty("D_Weight") + ",";
                }
            }
            if(!Links[0].equalsIgnoreCase(""))
            {
                Links[0] = Links[0].substring(0, Links[0].length()-1);
                Links[1] = Links[1].substring(0, Links[1].length()-1);
                Links[2] = Links[2].substring(0, Links[2].length()-1);
                Links[3] = Links[3].substring(0, Links[3].length()-1);
            }

            String msg = "\'Temp\', \'" + Links[0] + "\', \'" +
                                         Links[1] + "\', \'" +
                                         Links[2] + "\', \'" +
                                         Links[3] + "\', \'" +
                                         tempNode.getProperty("Belief") + "\', \'" +
                                         tempNode.getProperty("Disbelief") + "\', \'" +
                                         tempNode.getProperty("label") + "\', \'" +
                                         tempNode.getProperty("level") + "\', \'" +
                                         tempNode.getProperty("Phrase") + "\', \'" +
                                         tempNode.getProperty("Threshold") + "\', \'" +
                                         tempNode.getProperty("Cutoff") + "\'";
            boolean LevelThere = false;
            for(int q = 0; q < LevelCount; q++)
            {
                if(Levels[q].equalsIgnoreCase(tempNode.getProperty("level")))
                {
                    LevelThere = true;
                    break;
                }
            }
            if(!LevelThere)
            {
                Levels[LevelCount] = tempNode.getProperty("level");
                LevelCount++;
            }
             System.out.println("Mysql Statement: " + msg);
            for(int i = 0; i < 4; i++)
                Links[i] = "";
            test.executeQuery("INSERT INTO " + StoryName + " Values(" + msg + ");");
          //}
        } // end while (Node)
        //
        String Level = "";
        for(int x = 0; x < LevelCount; x++)
        {
            Level += Levels[x] + ",";
        }
        System.out.println("Levels: " + Level);
        if (Level != "")
          test.executeQuery("UPDATE " + StoryName + " SET LEVEL_LABELS=\'" +
                            Level.substring(0, Level.length()-1) + "\';");
    }
}
