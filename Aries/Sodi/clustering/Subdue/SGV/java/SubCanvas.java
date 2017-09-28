import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.applet.*;

public class SubCanvas extends Canvas 
{
   private GsGraph graph;
   Vector datain;
   public static int subXPosition, subYPosition;
   Adjustable horiAdj,vertAdj;
             // constructor

/*
   public SubCanvas(GsGraph gph,Vector dat)
   {
      graph = gph;
      datain = dat;
   }
*/
   public void draw(GsGraph gph, Vector dat)
   {
     graph = gph;
     datain = dat;
     repaint();
   }   

  public void paint(Graphics g)
  {

    try {
      GsEdgeArray eray=null;
      GsComm subs=null;
      Enumeration cliobjects,objects;

        if(graph == null || datain == null)
	{
          return;    
        }
        cliobjects = datain.elements();
//        subXPosition = subYPosition = 4000;
        for(int i=0; i < datain.size(); i++)     
        {
             subs = (GsComm)datain.get(i);
             if(subs.typ == 'v') 
             {
                 objects = graph.vertexArray[subs.vert1 - 1].objlist.elements();
                 drawObjects(objects,g);
                 horiAdj = SubWindow.horizontal;
                 vertAdj = SubWindow.vertical;
                 horiAdj.setValue(subXPosition - 100);
                 vertAdj.setValue(subYPosition - 100);
             }
             else if(subs.typ == 'e' || subs.typ == 'd' || subs.typ == 'u') 
             {
                   // CHANGE :find the edge by comparing the input values to the values
                   // stored; not like this
                GsVertexArray vert = graph.vertexArray[subs.vert1 - 1];
  
                for(int j=0;j<vert.numberOfEdges;j++)
                {
                    eray = (GsEdgeArray)(vert.edgeArray.get(j));
                    // compare edge type (e,d or u), target vertex indx & label
                    int labindx = graph.labelArray.indexOf(subs.label);
                    if(eray != null)
                    {
                      if(subs.typ == eray.etype && subs.vert2 == eray.tgtVertexIndex && labindx == eray.elabelIndex)
                         break;
                    }
                      

                }
                if(eray != null)
                {
                  objects = eray.objlist.elements();
                  if(objects.hasMoreElements())
                  {
                    drawObjects(objects,g);
                    horiAdj = SubWindow.horizontal;
                    vertAdj = SubWindow.vertical;
                    horiAdj.setValue(subXPosition - 100);
                    vertAdj.setValue(subYPosition - 100);
                  }
                }
             }

        }
    }
    catch(NullPointerException np) {
       System.out.println("Null Pointer Exception");
       np.printStackTrace();
    }
    catch(ArrayIndexOutOfBoundsException aiob) {
        aiob.printStackTrace();
    }

  }

 
public void drawObjects(Enumeration objects,Graphics g)
{
    GsObject curObj;
    GsPoints curPt;
                      
    try {
      while(objects.hasMoreElements())
      {
        curObj = (GsObject)objects.nextElement();  
                     
        // set color
        g.setColor( new Color((float)curObj.crgb.cr,(float)curObj.crgb.cg,(float)curObj.crgb.cb));
        Enumeration points = curObj.pts.elements();
        switch(curObj.shp)
        {
          case 2:  // text
                    while(points.hasMoreElements())
                    {
                      curPt = (GsPoints)points.nextElement();
                      g.setFont( new Font(curObj.txt.fonttype,Font.PLAIN,curObj.txt.fontsiz));
                      g.drawString(curObj.txt.str,(int)curPt.px,(int)curPt.py);
                         subXPosition = getMin(subXPosition,(int)curPt.px);
                         subYPosition = getMin(subYPosition,(int)curPt.py);
                      
         
                    }
                    break;
          
          case 0:   // polygon

                    int[] xpoints, ypoints;
                    int ptcount = 0;
                    xpoints = new int[10];
                    ypoints = new int[10];
                    while(points.hasMoreElements())
                    {
                      curPt = (GsPoints)points.nextElement();
//                      plgn.addPoint((int)curPt.px,(int)curPt.py);
                      xpoints[ptcount] = (int)curPt.px;
                      ypoints[ptcount] = (int)curPt.py;
                         subXPosition = getMin(subXPosition,xpoints[ptcount]);
                         subYPosition = getMin(subYPosition,ypoints[ptcount]);
                      ptcount++;

                    }
                    xpoints[ptcount] = xpoints[0];
                    ypoints[ptcount] = ypoints[0];
                         subXPosition = getMin(subXPosition,xpoints[ptcount]);
                         subYPosition = getMin(subYPosition,ypoints[ptcount]);
                    ptcount++;
                   

                    if(curObj.crgb.fill)
                      g.fillPolygon(xpoints,ypoints,ptcount);
                    else
                      g.drawPolyline(xpoints,ypoints,ptcount);
                    break;

           case 1 :
                      while(points.hasMoreElements())
                      {
                        curPt = (GsPoints)points.nextElement();                        
                        if(curObj.crgb.fill)
                          g.fillOval((int)curPt.px,(int)curPt.py,(int)curObj.radius,(int)curObj.radius);
                        else
                          g.drawOval((int)curPt.px,(int)curPt.py,(int)curObj.radius,(int)curObj.radius);
                         subXPosition = getMin(subXPosition,(int)curPt.px);
                         subYPosition = getMin(subYPosition,(int)curPt.py);
                      }
                      break;

           default:  // shouldn't occur at all
                      break;
                      
        }
                     
      }  // end while
    }
    catch (NoSuchElementException ex) {}
    catch (NullPointerException ne) { System.out.println("Null Pointer Exception in SubCanvas.java ");
                                    }
}




  public void setCanvasWidth()
  {
//    width = w;
    repaint();
  }

  public void setCanvasHeight()
  {
//    height = h;
    repaint();
  }

  public int getMin(int a , int b)
  {
    if( a < b)
     return a;
    else
     return b;
  }
}
