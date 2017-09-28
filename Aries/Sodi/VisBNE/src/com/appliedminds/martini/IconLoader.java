package com.appliedminds.martini;

import java.awt.Component;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;


/**
 * The IconLoader is responsible for parsing a list of icon resources (usually
 * in URI form), loads the icons specified, and maps them to the keys specified
 * in the resource file.
 *
 * <P>Here is an exmaple resource file:
 *
 * <pre>
 * &lt;vge-dataset id="imdb"&gt;
 *  &lt;icon-base&gt;file://path/to/icon/dir&lt;/icon-base&gt;
 *  &lt;!-- OR: &lt;icon-base&gt;http://server/path/to/icon/dir&lt;/icon-base&gt; --&gt;
 *
 *  &lt;type-map&gt;
 *   &lt;type&gt;
 *     &lt;value&gt;Director&lt;/value&gt;
 *     &lt;!-- The icon value is relative to the icon-source defined elsewhere --&gt;
 *     &lt;icon&gt;director.ico&lt;/icon&gt;
 *   &lt;/type&gt;
 *   &lt;type&gt;
 *     &lt;value&gt;Actor&lt;/value&gt;
 *     &lt;icon&gt;actor.ico&lt;/icon&gt;
 *   &lt;/type&gt;
 *   &lt;type&gt;
 *     &lt;value&gt;*&lt;/value&gt;
 *     &lt;icon&gt;generic.ico&lt;/icon&gt;
 *   &lt;/type&gt;
 *  &lt;/type-map&gt;
 * &lt;/vge-dataset&gt;
 * </pre>
 * <p>
 *
 * IconLoader combines code from vge's GraphDataset and GraphMetaData to
 * create a generic ImageIcon loader.
 *
 * @see com.appliedminds.vge.core.GraphDataset
 * @see com.appliedminds.vge.core.GraphMetaData
 *
 * @author mathias@apmindsf.com
 * @author will@apmindsf.com
 */
public class IconLoader {

  private String _id;
  private String _base;
  private HashMap _typeMap;
  //private GraphLoaderPreferences _glPrefs;
  private ClassLoader _loader;
  private HashMap _iconCache;


  /**
   * Load the XML file given the URL to create an IconLoader.
   *
   * @param u the url pointing to the dataset XML file.
   * @param comp an optionall component to use for image loading, can
   * be null.
   * @throws IOExcpetion if there is a loading error.
   */
  public static IconLoader create(URL u, Component comp) throws IOException {
    try {
      SAXBuilder builder = new SAXBuilder(false);
      Document doc = builder.build(u);
      return(new IconLoader(doc, comp));
    }
    catch(JDOMException e) {
      throw(new IOException("IconLoader.load() error: " + e.toString()));
    }
  }

  /**
   * Load the XML file given the URL to create an IconLoader.
   *
   * @param u the url pointing to the dataset XML file.
   * @throws IOExcpetion if there is a loading error.
   */
  public static IconLoader create(URL u) throws IOException {
    return(create(u, null));
  }


  /**
   * Construct a new IconLoader using the given XML document.  This will
   * also load up the graph icons.  If there is an error loading the
   * icons, that will not cause this to fail.
   *
   * @param dom the XML document that specifies icon-base and type-map
   * @param comp An optional component to use for its MediaTracker, can be
   * null.
   */
  private IconLoader(Document dom, Component comp)
  {
    _loader = getClass().getClassLoader();
    _typeMap = new HashMap();
    Element root = dom.getRootElement();
    _id = root.getAttributeValue("id");
    _base = root.getChildText("icon-base");

    Element map = root.getChild("type-map");

    if (map != null) {
      List types = map.getChildren("type");

      for(Iterator i = types.iterator(); i.hasNext(); ) {
        Element type = (Element) i.next();
        String value = type.getChildText("value").toLowerCase();
        String icon = type.getChildText("icon");
        try {
          URL u = createIconURL(_base, icon);
          _typeMap.put(value, u);
        }
        catch(MalformedURLException err) {
          System.err.println("Bad icon URL: " + icon +
                             " (" + err.toString() + ")");
        }
      }
    }

    batchLoadIcons(comp);

    /* [commented out during port]
    Element servlet = root.getChild("servlets");
    if (servlet == null) {
      // FIX: Create a MalformedGraphDataSetException
      throw(new RuntimeException("DataSet definition is missing a <servlets> section."));
    }

    initGraphLoaderPreferences(servlet);
    */
  }


  /**
   * The "vge-dataset" block contains an id value that is returned
   * here.
   *
   * @return the dataset id value.
   */
  public String getID() {
    return(_id);
  }


  /**
   * A graph might have meta data that prescribes an icon for a given
   * type value.  This method returns an icon given a type.  It should
   * return null if there is no icon available.
   *
   * @param typeval the type value
   * @return the icon, or null if there is none available.
   */
  public MartiniIcon getIconForType(String typeval) {
    URL u = (URL) _typeMap.get(typeval.toLowerCase());
    if (u != null) {
      return (MartiniIcon) _iconCache.get(u);
    }

    return (null);
  }


  /**
   * Get the icon URL for the specified type.
   *
   *
   * @param type the type value (case insensitive).
   * @return the URL or null if there is no URL associated with the
   * type.
   */
  private URL getIconURLForType(String type) {
    String k = type.toLowerCase();
    if (_typeMap.containsKey(k)) {
      return((URL) _typeMap.get(k));
    }
    return((URL) _typeMap.get("*"));
  }


  /**
   * Get the list of all URLs that specify icon files.
   *
   * @return a list of URL objects.
   */
  private Iterator listIconURLs() {
    return(_typeMap.values().iterator());
  }


  private void batchLoadIcons(Component comp) {
    if (_iconCache == null) {
      _iconCache = new HashMap();
    }

    for(Iterator i = listIconURLs(); i.hasNext(); )
    {
      URL u = (URL) i.next();
      if (! _iconCache.containsKey(u)) {
        try {
          MartiniIcon icon = MartiniIcon.load(u, comp);
          _iconCache.put(u, icon);
        }
        catch(IOException err) {
          System.err.println("IconLoader: i/o err loading icon: " +
                             u +
                             " (" + err.toString() + ")");
        }
      }
    }

  }


  /**
   * The preferences are loaded once from the source data, and a
   * reference to that object is returned every time.  If a caller
   * modifies the preferences, they are modified for all other users
   * of it.
   */
  /* [commented out during port]
  public GraphLoaderPreferences getGraphLoaderPreferences() {
    return(_glPrefs);
  }
  */

  /*
   * Construct a URL from its parts.
   *
   * @param the base part of the url
   * @param icon the non-base part.
   *
   * @return some concatenation of the base and the icon.
   */
  private URL createIconURL(String base, String icon)
    throws MalformedURLException
  {
    if (base == null) {
      return(new URL(icon));
    }

    String uri;
    if ((!base.endsWith("/")) && (!icon.startsWith("/"))) {
      uri = base + "/" + icon;
    }
    else {
      uri = base + icon;
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


  /*
   * Configure the GraphLoaderPreferences object.
   *
   * This modifies the _glPrefs member.
   */
  /* [commented out during port]
  private void initGraphLoaderPreferences(Element servlet) {
    String defProto = servlet.getChildText("protocol");
    String defHost = servlet.getChildText("host");

    String gmlURL = parseServletURL("s-gml", servlet, defProto, defHost);
    String topicURL = parseServletURL("s-topic", servlet, defProto, defHost);
    String taskURL = parseServletURL("s-task", servlet, defProto, defHost);

    String taskPath = servlet.getChildText("s-task");

    _glPrefs = new GraphLoaderPreferences(gmlURL, topicURL, taskURL, taskPath);
  }
  */


  /*
   * Parse a servlet descriptor from the XML element.
   * The servlet descriptor can have a -host part, a -proto part,
   * and a plain part (the path).
   *
   * So if you pass in "s-gml" as the name, this looks for:
   *
   *   <s-gml-host>
   *   <s-gml-proto>
   *   <s-gml>
   *
   * If the -host part is not found then the value of defHost is used.
   * If the -proto part is not found then the vale of defProtocol is used.
   *
   */
  /* [commented out during port]
  private String parseServletURL(String name,
                                 Element servlet,
                                 String defProtocol,
                                 String defHost)
  {
    String host = servlet.getChildText(name + "-host");
    if ((host == null) || (host.length() == 0)) {
      host = defHost;
    }
    String proto = servlet.getChildText(name + "-protocol");
    if ((proto == null) || (proto.length() == 0)) {
      proto = defProtocol;
    }
    String path = servlet.getChildText(name);
    if (path == null) {
      // FIX: Use MalformedGraphDataSetException
      throw(new RuntimeException("DataSet servlets section is missing the <name> tag."));
    }
    return(proto + "://" + host + path);
  }
  */

} // end IconLoader
