
package mil.navy.nps.dis;

import java.util.*;

/**
 * BehaviorStreamBufferInfo is an object that contains configuration data
 * about a BehaviorStreamBuffer. This data includes information such as
 * whether the data that is coming in is RTP enabled, sources of further
 * information, such as web sites, etc.<p>
 *
 * The object uses a dual scheme for creating objects. In effect this is
 * an object with a unique serialization scheme, made unique because we
 * want the serialized format to comply with our wishes. ("Oh, 
 * Behave!" --Austin Powers). We write out our various state variables
 * as attribute-value pairs, delimited by spaces. we can create a new
 * object from such a String object. Or we can create the string object
 * from the data in the object.<p>
 *
 * Significant attribute-value pairs as of this writing:<p>
 *
 * rtpEnabled=true|false. If true, the PDUs in this stream have RTP turned on,
 * and so therefore have a bunch of binary data prepended to them.<p>
 *
 * worldSite=url. There can be zero or more worldSite attribute-value pairs.
 * This describes a URL that contains information about the virtual world
 * used to create this PDU stream.<p>
 *
 * @author DMcG
 */

public class BehaviorStreamBufferInfo extends Object
{
  public static final int CURRENT_VERSION = 1;

  // List of all the attributes we can have in the system

  public static final String VERSION        = "version";
  public static final String URL            = "url";
  public static final String RTP_ENABLED    = "rtpEnabled";

  /**
   * version of the info object
   */

  private int version = CURRENT_VERSION;

  /** 
   * Whether RTP is turned on for this PDU source/sink
   */

  private boolean rtpEnabled = false;

  /**
   * A list of URLs, in string format, that contain information about the
   * virtual world associated with this PDU stream.
   */

  private Vector urls = new Vector();

  /**
   * Plain, no-args constructor
   */

  public BehaviorStreamBufferInfo()
  {
  }

  public BehaviorStreamBufferInfo(boolean pRtpEnabled)
  {
    rtpEnabled = pRtpEnabled;
  }

  /**
   * Constructor, takes the RTP status and an array of strings that
   * describe the world we got this PDU stream from.
   *
   * @param pRtpEnabled whether RTP is enabled for this stream
   * @param pURLs array of strings that describe URLS about this pdu stream
   */

  public BehaviorStreamBufferInfo(boolean pRtpEnabled, String pURLs[])
  {
    // whether this is enabled or not
    rtpEnabled = pRtpEnabled;

    // No URLs, punt (quick-kick?)
    if(pURLs == null)
      return;

    // Add the array of urls to our list
    for(int idx = 0; idx < pURLs.length; idx++)
    {
      urls.add(pURLs[idx]);
    }
  }

  /**
   * Convert a string of attribute-value pairs into a filled out info object.
   *
   */

  public BehaviorStreamBufferInfo(String pConfigString)
  {
    StringTokenizer tokenizer;

    // The strings are laid out as attribute-value pairs separated by spaces,
    // eg "rtpEnabled=true url=http://foo.com", etc.

    tokenizer = new StringTokenizer(pConfigString);

    // Loop through each attribute=value entry
    while(tokenizer.hasMoreElements())
    {
      String          pair = tokenizer.nextToken();
      StringTokenizer splitter = new StringTokenizer(pair, "=");
      String          attribute, value;

      // Sanity check; we should have two and only two tokens.
      if(splitter.countTokens() != 2)
      {
        System.out.println("Invalid format for config data");
        return;
      }

      attribute = splitter.nextToken();
      value     = splitter.nextToken();

      // find one of our known attributes

      // Version number (in text format for easy debugging)
      if(attribute.compareToIgnoreCase(VERSION) == 0)
      {
        try
        {
          version = Integer.parseInt(value);

          if(version != CURRENT_VERSION)
          {
            System.out.println("Mismatch between version read from stream, " + version + 
              " and current version " + CURRENT_VERSION);
          }
        }
        catch(NumberFormatException nfe)
        {
          System.out.println("Version number for PDU stream in incorrect format " + nfe);
        }
      }

      // URL string
      if(attribute.compareToIgnoreCase(URL) == 0)
      {
        urls.add(value);
      }

      // rtpEnabled
      if(attribute.compareToIgnoreCase(RTP_ENABLED) == 0)
      {
        if(value.compareToIgnoreCase("true") == 0)
        {
          rtpEnabled = true;
        }
        else                  // if it's not true, it's false. Linear dead-white-male thinking.
        {
          rtpEnabled = false;
        }
      }
    } // end of while
  } // end of constructor

  /**
   * Convert the object to a string. The string will be of the form
   * "attribute=value attribute=value attribute=value", etc.
   */

  public String toString()
  {
    String result = VERSION + "=" + version;

    result = result + " " + RTP_ENABLED + "=" + rtpEnabled;

    // All the URLs about this stream
    for(int idx = 0; idx < urls.size(); idx++)
    {
      result = result + " " + URL + "=" + (String)urls.elementAt(idx);
    }

    return result;
  }

  /**
   * is the rtp capability enabled or not?
   */
  public void setRtpEnabled(boolean pEnabled)
  { rtpEnabled = pEnabled;
  }

  /**
   * Get status of RTP for this stream
   */

  public boolean getRtpEnabled()
  { return rtpEnabled;
  }

  /**
   * Return the version number, which has an effect on what attributes are
   * valid. Note that we don't set versions--that's handled in the constructor,
   * and we can't really modify it after the fact.
   */

  public int getVersion()
  { return version;
  }

  /**
   * Add a URL to our list of URLs about this stream
   */

  public void addUrl(String pURL)
  { urls.addElement(pURL);
  }

  /**
   * Returns an array filled with all the URLs we have.
   */

  public String[] getUrls()
  { return (String[])urls.toArray();  // Nifty obscure method that converts a vector to an array
  }

} // End of class






