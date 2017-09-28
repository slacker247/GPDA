import java.applet.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;

public class MainWindow extends Frame
{
    private MainCanvas canvas;
    private ScrollPane cScrl;
    private Button zoomin,zoomout;
    private Panel zoompanel;
    private int min = 200;

    public MainWindow(GsGraph graph) {
      super("Main Graph");
      setSize(700,700);
      setVisible(true);
      setLayout(new BorderLayout());
      canvas = new MainCanvas(graph); 
      canvas.setSize(4000,4000);

      zoompanel = new Panel();

      zoomin = new Button("Zoom In");
      zoomin.addActionListener( 
	new ActionListener() {
            public void actionPerformed( ActionEvent e) 
	    {
                   canvas.zooomin();

	    }
        }
      );      
      zoompanel.add(zoomin);

      zoomout = new Button("Zoom Out");
      zoomout.addActionListener( 
	new ActionListener() {
            public void actionPerformed( ActionEvent e) 
	    {
                   canvas.zooomout();

	    }
        }
      );      
      zoompanel.add(zoomout);
      
      add(zoompanel,BorderLayout.NORTH);

      cScrl = new ScrollPane();
      cScrl.add(canvas,BorderLayout.CENTER);
      cScrl.setSize(500,500);
      add(cScrl,BorderLayout.CENTER);

      Adjustable horizontal = cScrl.getHAdjustable();
      horizontal.setUnitIncrement(2);
      Adjustable vertical = cScrl.getVAdjustable();
      vertical.setUnitIncrement(2);
      
      show();
    
   }

   public void refresh(Vector list, int subToDraw, String color)
   {
       canvas.unHiLight();
       LinkedList curSub = (LinkedList)list.get(subToDraw);
       canvas.highlight(curSub,color);  
   }

}
