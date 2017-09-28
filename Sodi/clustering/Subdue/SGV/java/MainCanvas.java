import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;

public class MainCanvas extends Canvas 
{
  private GsGraph graph = null;
  private boolean hilight;
  private GsClr hilightColor;
  private LinkedList instList = null;
  public float zoomvalue = 1;
  
                // constructor
  public MainCanvas(GsGraph gph)
  {
    graph = gph;
    hilight = false;
    hilightColor = new GsClr();
    hilightColor.cr = 0.0;
    hilightColor.cg = hilightColor.cb = 0.0;
    hilightColor.fill = false;

  }

  public void paint(Graphics g)
  {
     int i;
     int vertId;    
     int targetId;


	    try {
	      GsEdgeArray eray;
	      for(i=0;i<graph.vertexArrayLength;i++)
	      {
	        Enumeration objects = graph.vertexArray[i].objlist.elements();
                vertId = graph.vertexArray[i].vertexId;
                if(instList != null)
                   FindInList(vertId,-1);        
					 // check in the current instance list for this vertex
        		                 // if it is present, set hilight to true
        	drawObjects(objects,g);
    
	        for(int j=0;j<graph.vertexArray[i].edgeArray.size();j++)
        	{
	          eray = (GsEdgeArray)(graph.vertexArray[i].edgeArray.get(j));
                  targetId = eray.tgtVertexIndex;

                  if(instList != null)
                    FindInList(vertId,targetId);
        	  objects = eray.objlist.elements();
	          drawObjects(objects,g);
	        } // end for j
	      }  // end for i	
	  }catch(NullPointerException np) { }

  }

  public void drawObjects(Enumeration objects, Graphics g)
  {
    GsObject curObj;
    GsPoints curPt;


    try {
      while(objects.hasMoreElements())
      {
        curObj = (GsObject)objects.nextElement();

        if(hilight)
           // set highlight color
           g.setColor( new Color((float)hilightColor.cr,(float)hilightColor.cg,(float)hilightColor.cb));
        else
           // set color
           g.setColor( new Color((float)curObj.crgb.cr,(float)curObj.crgb.cg,(float)curObj.crgb.cb));

        Enumeration points = curObj.pts.elements();
        switch(curObj.shp)
        {
          case 2:  // text
                    while(points.hasMoreElements())
                    {
                      curPt = (GsPoints)points.nextElement();
                      g.setFont( new Font(curObj.txt.fonttype,Font.PLAIN, (int)(curObj.txt.fontsiz * zoomvalue) ));
//                      g.setXORMode(Color.black); 
                      g.drawString(curObj.txt.str,(int)(curPt.px * zoomvalue),(int)(curPt.py * zoomvalue));
                    }
                    break;
          case 1:   // circle
                    while(points.hasMoreElements())
                    {
                      curPt = (GsPoints)points.nextElement();
//                      g.setXORMode(Color.black); 
                      if(curObj.crgb.fill)
                       g.fillOval((int)(curPt.px * zoomvalue),(int)(curPt.py*zoomvalue),(int)(curObj.radius * zoomvalue),(int)(curObj.radius * zoomvalue) );
                      else
                        g.drawOval((int)(curPt.px*zoomvalue),(int)(curPt.py * zoomvalue),(int)(curObj.radius * zoomvalue), (int)(curObj.radius * zoomvalue) );
                    }
                    break;
          case 0:   // polygon
  //                  Polygon plgn;
				// instead of Polygon use an array of x and y points
				// so that its easier to draw a point and a
			        // line.
		    int[] xpoints , ypoints;
	
		    int ptcount = 0;
//                    plgn = new Polygon();
                    xpoints = new int[10];
                    ypoints = new int[10];
                    while(points.hasMoreElements())
                    {
                      curPt = (GsPoints)points.nextElement();
                      xpoints[ptcount] = (int)(curPt.px * zoomvalue);
                      ypoints[ptcount] = (int)(curPt.py * zoomvalue ) ;
		      ptcount++;          // to count the total number of points
                    }
                    xpoints[ptcount] = (int)(xpoints[0] ) ;
                    ypoints[ptcount] = (int)(ypoints[0] ) ;
                    ptcount++;

	                    if(curObj.crgb.fill)  // assuming only a closed polygon will be
						  // filled
        	              g.fillPolygon(xpoints,ypoints,ptcount);
                	    else
	                      g.drawPolyline(xpoints,ypoints,ptcount);
                    break;
          default:  // shouldn't occur at all
                   break;
   
        }

      }  // end while
    }
    catch (NoSuchElementException ex) {}
  } 


  public void FindInList(int vert1,int vert2)
  {
     Vector instance;
     GsComm currentObj;
     GsEdgeArray eray;
     Enumeration objects;
     boolean done = false;
   
     try {

    	for(int i = 0; (i < instList.size()) && (!done) ; i++)
    	{
    	       instance = (Vector)instList.get(i);
	       for(int j = 0; j< instance.size(); j++)
       	       {
	         currentObj = (GsComm)instance.get(j);
        	 if(vert2 < 0)
	         {
		 	// compare the vertex ids and label here
                        if(currentObj.typ == 'v')
			{
                                if(vert1 == currentObj.vert1)
				{
                            		done = true;
      	                                break;
 				}
			}
                 }
		 else {
			if(currentObj.typ == 'e' || currentObj.typ == 'd' || currentObj.typ == 'u')
			{
				if((vert1 == currentObj.vert1) && (vert2 == currentObj.vert2))
				{
					done = true;
					break;
				}
			}
		 }
               }
         }
     }catch(NullPointerException np) {
            np.printStackTrace();
     }catch(ArrayIndexOutOfBoundsException ab) {
       	    ab.printStackTrace();
     }
     if(done)
       hilight = true;
     else
       hilight = false;
     return;    
  }

  public void highlight(LinkedList list,String color)
  {
        hilight =  true;
        instList = list;

     	if(color.toLowerCase().equals("red"))
	{
        	hilightColor.cr = 1.0;
	        hilightColor.cg = 0.0;
	        hilightColor.cb = 0.0;

	}
        else if(color.toLowerCase().equals("green"))
        {
        	hilightColor.cr = 0.0;
	        hilightColor.cg = 1.0;
	        hilightColor.cb = 0.0;
        }
        else if(color.toLowerCase().equals("blue"))
        {
	        hilightColor.cr = 0.0;
	        hilightColor.cg = 0.0;
	        hilightColor.cb = 1.0;
        }
        repaint();
  }


  public void unHiLight()
  {
      hilight = false;
      repaint();
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

  public void zooomin()
  {
//    if(zoomvalue >= 2)
//      zoomvalue = 1;
//    else
      zoomvalue += 0.1;
    repaint(); 
 
  }

  public void zooomout()
  {
       zoomvalue -= 0.1;
       repaint();
  }
  

} 
