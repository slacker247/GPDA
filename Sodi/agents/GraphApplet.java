
import java.awt.*;

public class GraphApplet extends Frame {
  static double MaxDelta[] = new double[800];
  static double MinDelta[] = new double[800];
  static double AvgDelta[] = new double[800];
  static double Scale = 1.0;
  static int    NowDelta = 1;
  static int    GrafType = 1;

    public static void putArray(double Max, double Min, double Mean) {

      if (NowDelta > 750) NowDelta = 1;
      MaxDelta[NowDelta] = Max*Scale;
      MinDelta[NowDelta] = Min*Scale;
      AvgDelta[NowDelta] = Mean*Scale;
      NowDelta = NowDelta + 1;
    }

    public static void setGraphics(int type, double scale) {

      Scale    = scale;
      GrafType = type;
    }

    public void paint(Graphics g) {
      double upper[] = new double[800];
      double lower[] = new double[800];
      int    i;

      if (GrafType == 0) {
        g.setColor(Color.black);
        g.drawLine(9, 110, 750, 110);
        g.drawLine(9,  20,   9, 200);

        g.setColor(Color.red);
        for (int x = 1 ; x < NowDelta ; x++) {
	    g.drawLine(9+x, 110-(int)MaxDelta[x-1], 10+(x), 110-(int)MaxDelta[x]);
        }

        g.setColor(Color.green);
        for (int x = 1 ; x < NowDelta ; x++) {
	    g.drawLine(9+x, 110+(int)MinDelta[x-1], 10+(x), 110+(int)MinDelta[x]);
        }
      } else {
        g.setColor(Color.yellow);
        for (int x = 1 ; x < NowDelta ; x++) {
	    g.drawLine(9+x, 190-(int)AvgDelta[x-1], 10+(x), 190-(int)AvgDelta[x]);
        }

        g.setColor(Color.red);
        for (int x = 1 ; x < NowDelta ; x++) {
	    g.drawLine(9+x, 190-(int)MaxDelta[x-1], 10+(x), 190-(int)MaxDelta[x]);
        }

        g.setColor(Color.green);
        for (int x = 1 ; x < NowDelta ; x++) {
	    g.drawLine(9+x, 190-(int)MinDelta[x-1], 10+(x), 190-(int)MinDelta[x]);
        }
      } /*else*/

   }
}
