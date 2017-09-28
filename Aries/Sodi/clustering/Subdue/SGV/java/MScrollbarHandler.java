import java.awt.event.*;

public class MScrollbarHandler implements AdjustmentListener {

  private MainCanvas ca;
  private int dimension;

  public MScrollbarHandler(MainCanvas c,int d)
  {
    ca = c;
    dimension = d;
  }


  public void adjustmentValueChanged(AdjustmentEvent e)
  {
    if(dimension ==  MainCanvas.WIDTH)
      ca.setCanvasWidth();
    else if(dimension == MainCanvas.HEIGHT)
      ca.setCanvasHeight();
  }

}
