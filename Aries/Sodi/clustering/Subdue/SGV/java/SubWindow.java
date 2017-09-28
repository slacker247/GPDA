import java.applet.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;

public class SubWindow extends Frame
{
  private SubCanvas canvas;
  private ScrollPane cScrl;
  private Panel choicePane;
  public static  Choice subsChoice;
  private GsGraph gph;
  private Vector sL;
  public static Adjustable horizontal;
  public static Adjustable vertical ;
  
  public SubWindow(GsGraph graph,Vector subsList, int s ) {
//    public SubWindow() {

    super("Substructure Window");
    setSize(600,500);
    setVisible(true);
    setLayout(new BorderLayout());
//    canvas = new SubCanvas(graph,datain);  
    canvas = new SubCanvas();
    canvas.setSize(4000,4000);
    gph = graph;
    sL = subsList;

    cScrl = new ScrollPane();
    cScrl.add(canvas,BorderLayout.CENTER);
//    cScrl.setSize(500,500);
    add(cScrl,BorderLayout.CENTER);

    horizontal= cScrl.getHAdjustable();
    horizontal.setUnitIncrement(2);
    vertical = cScrl.getVAdjustable();
    vertical.setUnitIncrement(2);

        
    choicePane = new Panel();
    subsChoice = new Choice();
    subsChoice.addItemListener( new ItemListener() {
	public void itemStateChanged(ItemEvent ie) {
	   int subNumber = subsChoice.getSelectedIndex();
	   
	   // Display the selected substructure here
           sL = Client.subsList;
	   refresh(gph,sL,subNumber);
           Client.highLight(subNumber);
        }
    });
    choicePane.add(subsChoice);
    subsChoice.setSize(20,20);
    subsChoice.setVisible(true);
    add(choicePane,BorderLayout.NORTH);

    show();   
 }

 public void refresh(GsGraph graph,Vector subsList, int s)
 {
   
   s++;
   double val = ((Double)Client.value.elementAt(s-1)).doubleValue();
   setTitle("Substructure " + s + "  with Compression " + val);
   LinkedList sList = (LinkedList)subsList.get(s-1);
   Vector subToDraw = (Vector)sList.getFirst();
   canvas.draw(graph,subToDraw);
   
//   cScrl.setScrollPosition(canvas.subXPosition-50,canvas.subYPosition-50);
//   System.out.println("Subx = " + canvas.subXPosition + "SubY = " + canvas.subYPosition);
//   horizontal.setValue(canvas.subXPosition - 100);
//   vertical.setValue(canvas.subYPosition - 100);
   canvas.subXPosition = canvas.subYPosition = 4000;
 }

}
