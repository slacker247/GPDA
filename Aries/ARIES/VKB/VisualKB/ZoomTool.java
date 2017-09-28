/*
 * ZoomTool.java
 *
 * Created on June 27, 2003, 10:52 AM
 */

package VisualKB;

import com.appliedminds.martinix.gapp.*;
import com.appliedminds.martini.*;

import java.awt.Cursor;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;
import java.awt.event.KeyEvent;
/**
 *
 * @author  s824685
 */
public class ZoomTool implements GTool {
    
    private Cursor zCursor = new Cursor(0);
    private int MouseY = 0;
    private double Scale = 0.0;
    private GraphPanel gp = null;
    private boolean debug = false;
    
    /** Creates a new instance of ZoomTool */
    public ZoomTool() {
    }
    
    /** Creates a new instance of ZoomTool */
    public ZoomTool(Cursor c) {
        zCursor = c;
    }
    
    public void setScaleValue(double in)
    {
        Scale = in;
    }
    
    public void setGraphPanel(GraphPanel in)
    {
        gp = in;
    }
    
    /** Notification that the tool has been activated.
     * Perferm any intialization steps before it's actually used.
     *
     * @args any arguments this tool need to be activated.
     */
    public void activate(Object[] args) {
        if(debug) System.out.println("ZoomTool.activate():");
    }
    
    /** Notification that the tool has been deactivated.
     * Do any clean up before another tool is activated.
     */
    public void deactivate() {
        if(debug) System.out.println("ZoomTool.deactivate():");
    }
    
    /** @return the Cursor that will represent this tool when this
     * tool is activated.
     */
    public Cursor getCursor() {
        if(debug) System.out.println("ZoomTool.getCursor():");
        return(zCursor);
    }
    
    /** Handle a Marquee Selection event.
     */
    public void handleMarqueeSelection(Rectangle marqueeBounds) {
        if(debug) System.out.println("ZoomTool.handleMarqueeSelection():");
    }
    
    /** Handle a key pressed event.
     */
    public void keyPressed(KeyEvent e) {
        if(debug) System.out.println("ZoomTool.keyPressed():");
    }
    
    /** Handle a key released event.
     */
    public void keyReleased(KeyEvent e) {
        if(debug) System.out.println("ZoomTool.keyReleased():");
    }
    
    /** Handle a key typed event.
     */
    public void keyTyped(KeyEvent e) {
        if(debug) System.out.println("ZoomTool.keyTyped():");
    }
    
    /** Handle a single mouse click event
     */
    public void mouseClicked(MouseEvent e) {
        if(debug) System.out.println("ZoomTool.mouseClicked():");
    }
    
    /** Handle a double mouse click event
     */
    public void mouseDoubleClicked(MouseEvent e) {
        if(debug) System.out.println("ZoomTool.mouseDoubleClicked():");
    }
    
    /** Handle a mouse dragged event.
     */
    public void mouseDragged(MouseEvent e) {
        if(debug) System.out.println("ZoomTool.mouseDragged():");
        if(e.getY() < MouseY)
        {
            Scale += 0.03;
            gp.setScale(Scale);
            gp.redraw();
        }
        if(e.getY() > MouseY)
        {
            Scale -= 0.03;
            gp.setScale(Scale);
            gp.redraw();
        }
        MouseY = e.getY();
    }
    
    /** Handle a mouse entered event.
     */
    public void mouseEntered(MouseEvent e) {
        if(debug) System.out.println("ZoomTool.mouseEntered():");
    }
    
    /** Handle a mouse exit event.
     */
    public void mouseExited(MouseEvent e) {
        if(debug) System.out.println("ZoomTool.mouseExited():");
    }
    
    /** Handle a mouse moved event.
     */
    public void mouseMoved(MouseEvent e) {
        if(debug) System.out.println("ZoomTool.mouseMoved():");
    }
    
    /** Handle a mouse pressed event.
     */
    public void mousePressed(MouseEvent e) {
        if(debug) System.out.println("ZoomTool.mousePressed():");
    }
    
    /** Handle a mouse released event.
     */
    public void mouseReleased(MouseEvent e) {
        if(debug) System.out.println("ZoomTool.mouseReleased():");
    }
    
}
