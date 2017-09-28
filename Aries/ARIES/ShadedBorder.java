import java.awt.*;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.border.EmptyBorder.*;

public class ShadedBorder extends EmptyBorder
{
  protected Color  frameColor;
  protected Color  shadowColor;
  protected int    thickness;

  public ShadedBorder()
  {
    this(Color.LIGHT_GRAY, Color.DARK_GRAY, 7);
  }

  public ShadedBorder(Color frameColor, Color shadowColor, int thickness)
  {
    super(thickness, thickness, thickness, thickness);
    this.frameColor = frameColor;
    this.shadowColor = shadowColor;
    this.thickness = thickness;
  }

  public void paintBorder(Component component, 
                          Graphics g, int x, int y, int w, int h)
  {
    Graphics  copy = g.create();

    if (copy != null)
    {
      try
      {
        copy.translate(x, y);
        paintShadows(component, copy, w, h);
      }
      finally
      {
        copy.dispose();
      }
    }
  }

  public Insets getBorderInsets()
  {
    return new Insets(thickness, thickness, thickness, thickness);
  }

  protected void paintShadows(Component c, Graphics g, int w, int h)
  {
    int          thicknessX2 = thickness + thickness;
    int          thicknessX3 = thickness + thickness + thickness;
    Graphics2D  g2 = (Graphics2D) g;

    GradientPaint  sideColor = new GradientPaint
      (thickness-1, thickness, shadowColor, 0, thickness, frameColor);
    GradientPaint  bottomColor = new GradientPaint
      (thickness, h-thickness-1, shadowColor, thickness, h-1, frameColor);

    g2.setPaint(frameColor);
    g2.fillRect(0, 0, thickness, h);
    g2.fillRect(0, 0, w, thickness);
    g2.fillRect(0, h-thickness, w, thickness);
    g2.fillRect(w-thickness, 0, thickness, h);

    g2.setPaint(sideColor);
    g2.fillArc(0, thickness, thicknessX2, thicknessX2, 90, 90);
    g2.fillRect(0, thicknessX2, thickness, h-thicknessX3);
    g2.fillArc(0, h-thicknessX2, thicknessX2, thicknessX2, 180, 45);

    g2.setPaint(bottomColor);
    g2.fillArc(0, h-thicknessX2, thicknessX2, thicknessX2, 225, 45);
    g2.fillRect(thickness-1, h-thickness, w-thicknessX3, thickness);
    g2.fillArc(w-thicknessX3-1, h-thicknessX2,
               thicknessX2, thicknessX2, 270, 90);
  }
}

