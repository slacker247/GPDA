package com.appliedminds.martinix.ui;

import com.appliedminds.martini.MartiniIcon;
import java.awt.Component;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Collections;
import java.util.Comparator;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;
import javax.swing.SwingConstants;



/**
 * <b>MSliderResource</b> is responsible for parsing the resources
 * required by an MSlider object (usually in URI form). MSliderResource
 * contains all the custom images of a slider including the slider track
 * images and the slider knob image.
 *
 * <p>Here is an example slider resource file:
 *
 * <pre>
 *   Please consult com/appliedminds/martinix/ui/resources/slider.xml
 * </pre>
 *
 * @see MSlider
 *
 * @author daepark@apmindsf.com
 */
public class MSliderResource implements SwingConstants {

  private String _id;
  private String _base;

  private static Hashtable _imageCache = new Hashtable();
  private ClassLoader _loader;

  //
  // slider resources
  //
  private URL[][] _knobImageURL = new URL[2][];
  private MartiniIcon[][] _knobImage = new MartiniIcon[2][];
  private URL[][] _trackImageURL = new URL[2][];
  private MartiniIcon[][] _trackImage = new MartiniIcon[2][];


  /**
   * Load the XML file given the URL to create MSliderResource.
   *
   * @param u the url pointing to the slider resource XML file. If
   * null, use default slider resource.
   * @param comp an optionall component to use for image loading, can
   * be null.
   * @throws IOException if there is a loading error.
   */
  public static MSliderResource create(URL u, Component comp)
    throws IOException
  {
    try {
      if (u == null) {
        URL dir = MSliderResource.class.getClassLoader().getResource("com/appliedminds/martinix/ui/resources/");
        u = new URL(dir.toString() + "slider.xml");
      }

      SAXBuilder builder = new SAXBuilder(false);
      Document doc = builder.build(u);
      MSliderResource resource = new MSliderResource(doc, comp);
      //      System.err.println("done loading slider resource");

      return (resource);
    }
    catch (JDOMException e) {
      throw(new IOException("MSliderResource.create() error: " +
                            e.toString()));
    }
    catch (MalformedURLException e) {
      throw (new IOException("Could not load slider resource: " +
                             e.toString()));
    }
  }


  /**
   * Load the XML file given the URL to create MSliderResource.
   *
   * @param u the url pointing to the slider resource XML file. If
   * null, use default slider resource.
   * @throws IOExcpetion if there is a loading error.
   */
  public static MSliderResource create(URL u) throws IOException {
    return(create(u, null));
  }


  /**
   * The "slider-resource" block contains an id value that is returned
   * here.
   *
   * @return the dataset id value.
   */
  public String getID() {
    return(_id);
  }


  /**
   * Get the default slider knob image for a specifid orientation.
   */
  public MartiniIcon getKnobImage(int orientation) {
    return (getKnobImage(orientation, false));
  }


  /**
   * Get the slider knob image for the specified knob pressed state.
   *
   * @param down if TRUE get the mouse pressed down knob image,
   * otherwise get the default knob image.
   */
  public MartiniIcon getKnobImage(int orientation, boolean down) {
    checkOrientation(orientation);
    return (down ? _knobImage[mapOrientationToIndex(orientation)][1] :_knobImage[mapOrientationToIndex(orientation)][0]);
  }


  /**
   * Get the total number of slider track images for a specific orientation.
   */
  public int getTrackImageCount(int orientation) {
    checkOrientation(orientation);
    return (_trackImage[mapOrientationToIndex(orientation)].length);
  }


  /**
   * Get n-th track image for the specific orientation.
   */
  public MartiniIcon getTrackImage(int orientation, int index) {
    checkOrientation(orientation);
    return (_trackImage[mapOrientationToIndex(orientation)][index]);
  }


  /**
   * Get the track image that best matches the given slider tracks.
   *
   * <p>For example, if the value if equal to the minimum slider
   * value, than this will return the first track image of the
   * specified orientation. If the value is equal to the maximum, then
   * it will return the last track image. Everything else in between
   * the min/max values will be determined by the number of track
   * images available for the given orientation and how each track
   * image maps to a range of values between the min/max.
   *
   * @param orientation the orientation of the slider.
   * @param min the minimum value of the slider.
   * @param max the maximum value of the slider.
   * @param value the value of the slider.
   */
  public MartiniIcon getTrackImage(int orientation, int min, int max,
                                   int value)
  {
    checkOrientation(orientation);

    MartiniIcon[] trackImage =
      _trackImage[mapOrientationToIndex(orientation)];

    // max must be greater than min
    if (max <= min) {
      return (trackImage[0]);
    }

    // return min/max image if value is not in the specified range.
    if (value < min) {
      return (trackImage[0]);
    }

    int length = trackImage.length;

    if (value > max) {
      return (trackImage[length - 1]);
    }

    int range = max - min;
    int index = length - 1;

    if (length > range) {
      double tracksPerValue = length / range;
      index -= Math.round(tracksPerValue * ((double)max - value));
    }
    else {
      double valuePerTrack = range /length;
      index -= Math.round(((double)max - value) / valuePerTrack);
    }

    index = Math.max(0, index);
    index = Math.min(length - 1, index);

    return (trackImage[index]);
  }


  /**
   * Construct a new MSliderResource using the given XML document.
   * This will also load up the variouse slider images specified in
   * the XML document.  If there is an error loading the images, that
   * will throw a RuntimeException (for now).
   *
   * @param dom the XML document that specifies the slider resource.
   * @param comp An optional component to use for its MediaTracker, can be
   * null.
   */
  private MSliderResource(Document dom, Component comp)
  {
    _loader = getClass().getClassLoader();

    Element root = dom.getRootElement();
    _id = root.getAttributeValue("id");
    _base = root.getChildText("icon-base");

    List sliderList = root.getChildren("slider");
    for (Iterator itr=sliderList.iterator(); itr.hasNext();) {
      Element slider = (Element)itr.next();

      int orientation =
        checkOrientation(slider.getAttributeValue("orientation"));

      List knobs = slider.getChildren("knob");
      Element tracks = slider.getChild("tracks");

      //
      // knob image(s)
      //
      _knobImageURL[mapOrientationToIndex(orientation)] = new URL[2];

      if (knobs.size() == 1) {
        Element knob = (Element)knobs.get(0);
        try {
          URL u = createImageURL(_base, knob.getAttributeValue("image"));
          _knobImageURL[mapOrientationToIndex(orientation)][0] = u;
          _knobImageURL[mapOrientationToIndex(orientation)][1] = u;
        }
        catch (MalformedURLException e) {
          throw (new RuntimeException("Bad slider knob image URL: " + e));
        }
      }
      else {
        for (Iterator itr2=knobs.iterator(); itr2.hasNext();) {
          Element knob = (Element)itr2.next();
          try {
            URL u = createImageURL(_base, knob.getAttributeValue("image"));
            String state = knob.getAttributeValue("state");
            if (state == null || "default".equals(state)) {
              _knobImageURL[mapOrientationToIndex(orientation)][0] = u;
            }
            else if ("down".equals(state)) {
              _knobImageURL[mapOrientationToIndex(orientation)][1] = u;
            }
          }
          catch (MalformedURLException e) {
            throw (new RuntimeException("Bad slider knob image URL: " + e));
          }
        }
      }

      if (_knobImageURL[mapOrientationToIndex(orientation)][0] == null ||
          _knobImageURL[mapOrientationToIndex(orientation)][1] == null)
      {
        throw (new IllegalStateException("Please check knob resource description"));
      }
      else if (_knobImageURL[mapOrientationToIndex(orientation)][1] == null) {
        _knobImageURL[mapOrientationToIndex(orientation)][1] =
          _knobImageURL[mapOrientationToIndex(orientation)][0];
      }

      //
      // slider track images
      //
      int count = Integer.parseInt(tracks.getAttributeValue("value"));

      List trackList = tracks.getChildren("track");
      if (count < 1 || count != trackList.size()) {
        throw (new RuntimeException("MSliderResource: incosistent number of slider track images"));
      }
      Collections.sort(trackList, new Comparator() {
          public int compare(Object o1, Object o2) {
            Element e1 = (Element)o1;
            Element e2 = (Element)o2;

            Integer i = Integer.valueOf(e1.getAttributeValue("value"));
            Integer j = Integer.valueOf(e2.getAttributeValue("value"));
            return (i.compareTo(j));
          }
        });

      _trackImageURL[mapOrientationToIndex(orientation)] = new URL[count];
      int i = 0;
      for (Iterator itr2=trackList.iterator(); itr2.hasNext(); i++) {
        Element track = (Element)itr2.next();
        try {
          _trackImageURL[mapOrientationToIndex(orientation)][i] =
            createImageURL(_base, track.getAttributeValue("image"));

//           System.err.println("track image url[" +  (orientation == MSliderResource.VERTICAL ? "VERTICAL" : "HORIZONTAL") + "][" + i + "]: " + _trackImageURL[mapOrientationToIndex(orientation)][i]);
        }
        catch (MalformedURLException e) {
          throw (new RuntimeException("Bad slider track image URL: " + e));
        }
      }
    }

    batchLoadImages(comp);
  }


  private void batchLoadImages(Component comp) {
    // load slider knob images
    for (int i=0; i<_knobImageURL.length; i++) {
      _knobImage[i] = new MartiniIcon[_knobImageURL[i].length];
      for (int j=0; j<_knobImageURL[i].length; j++) {
        MartiniIcon icon = (MartiniIcon)_imageCache.get(_knobImageURL[i][j]);

        if (icon == null) {
          try {
            icon = MartiniIcon.load(_knobImageURL[i][j], comp);
            _imageCache.put(_knobImageURL[i][j], icon);
          }
          catch (IOException e) {
            throw (new RuntimeException("MSliderResource: I/O error loading image: " + e));
          }
        }
        _knobImage[i][j] = icon;
      }
    }


    // load slider track images
    for (int i=0; i<_trackImageURL.length; i++) {
      _trackImage[i] = new MartiniIcon[_trackImageURL[i].length];
      for (int j=0; j<_trackImageURL[i].length; j++) {
        MartiniIcon icon = (MartiniIcon)_imageCache.get(_trackImageURL[i][j]);
        if (icon == null) {
          try {
            icon = MartiniIcon.load(_trackImageURL[i][j], comp);
            _imageCache.put(_trackImageURL[i][j], icon);
          }
          catch (IOException e) {
            throw (new RuntimeException("MSliderResource: I/O error loading image: " + e));
          }
        }
        _trackImage[i][j] = icon;
      }
    }
  }


  /*
   * Construct a URL from its parts.
   *
   * @param the base part of the url
   * @param image the non-base part.
   *
   * @return some concatenation of the base and the image.
   */
  private URL createImageURL(String base, String image)
    throws MalformedURLException
  {
    if (base == null) {
      return(new URL(image));
    }

    String uri;
    if ((!base.endsWith("/")) && (!image.startsWith("/"))) {
      uri = base + "/" + image;
    }
    else {
      uri = base + image;
    }

    //
    // If the base starts with a "resource:" then treat it like a
    // resource.
    //
    if (base.startsWith("resource:")) {
      URL r = _loader.getResource(uri.substring(9));
      return(r);
    }
    else {
      return(new URL(uri));
    }
  }


  /**
   * @throws IllegalArgumentException if unrecognized orientation.
   */
  protected static void checkOrientation(int orientation) {
    switch (orientation) {
    case VERTICAL:
    case HORIZONTAL:
      break;
    default:
      throw new IllegalArgumentException("orientation must be one of: VERTICAL, HORIZONTAL");
    }
  }

  private static int checkOrientation(String orientation) {
    if ("vertical".equalsIgnoreCase(orientation)) {
      return (VERTICAL);
    }

    if ("horizontal".equalsIgnoreCase(orientation)) {
      return (HORIZONTAL);
    }

    throw new IllegalArgumentException("orientation must be one of: \"vertical\", \"horizontal\"");
  }

  private int mapOrientationToIndex(int orientation) {
    switch (orientation) {
    case VERTICAL:
      return 0;
    case HORIZONTAL:
      return 1;
    default:
      return 0;
    }
  }

} // end class "MSliderResource"
