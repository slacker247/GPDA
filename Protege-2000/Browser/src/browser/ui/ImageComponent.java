
/**
 * Title:        Browser<p>
 * Description:  This allows you to browse a library of knowledge bases.<p>
 * Copyright:    Copyright (c) Warren Shen<p>
 * Company:      Stanford Medical Informatics<p>
 * @author Warren Shen
 * @version 1.0
 */
package browser.ui;

import javax.swing.*;
import java.awt.*;

public class ImageComponent extends JPanel {

  private Image _image;
  private String _filename;
  private int _x, _y, _width, _height;

  public ImageComponent(int leftMargin, int topMargin, int width, int height) {
    setDimensions(leftMargin, topMargin, width, height);
    _filename = null;
  }

  public ImageComponent(String filename) {
    this.setBackground(Color.white);
    setDimensions(0, 0, -1, -1);
    _filename = filename;
    setImage(_filename);
  }

  public ImageComponent() {
    setDimensions(0, 0, -1, -1);
    _filename = null;
  }

  public void setDimensions(int leftMargin, int topMargin, int width, int height) {
    _x = leftMargin;
    _y = topMargin;
    _width = width;
    _height = height;
  }

  public void paintComponent(Graphics g) {
    super.paintComponent(g);
    if(_image == null) {
      g.drawString("No Image", 25, 25);
    } else {
      if(_width < 0 || _height < 0) {
        if(!g.drawImage(_image, _x, _y, this)) {
          g.drawString("Failed to load: " + _filename, 25, 25);
        }
      } else {
        if(!g.drawImage(_image, _x, _y, _width, _height, getBackground(), this)) {
          g.drawString("Failed to load: " + _filename, 25, 25);
        }
      }
    }
  }

  public void setImage(String filename) {
    if(filename != null) {
      if(_image != null) {
        _image.flush();
      }
      _image = Toolkit.getDefaultToolkit().getImage(filename);
      MediaTracker tracker = new MediaTracker(this);
      tracker.addImage(_image, 0);
      try {
        tracker.waitForID(0);
        _filename = filename;
      } catch (InterruptedException e) {
        e.printStackTrace();
      }
    } else {
      _image = null;
    }
  }
}