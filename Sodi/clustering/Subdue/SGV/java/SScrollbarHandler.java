import java.awt.event.*;

public class SScrollbarHandler implements AdjustmentListener {

  private SubCanvas ca;
  private int dimension;

  public SScrollbarHandler(SubCanvas c,int d)
  {
    ca = c;
    dimension = d;
  }


  public void adjustmentValueChanged(AdjustmentEvent e)
  {
    if(dimension ==  SubCanvas.WIDTH)
      ca.setCanvasWidth();
    else if(dimension == SubCanvas.HEIGHT)
      ca.setCanvasHeight();
  }

}
