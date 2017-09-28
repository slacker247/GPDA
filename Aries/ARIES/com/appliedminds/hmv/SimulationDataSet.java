package com.appliedminds.hmv;

import java.io.File;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.BufferedReader;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.text.SimpleDateFormat;
import java.text.ParseException;



/**
 * An instance of this class represents a set of time/probability
 * comma-separated pairs. The time format is expected to be:
 * mm/dd/yyyy hh:mm:ss am (or pm).
 * And the probability should be specified between 0 and 100.
 * A Simulation Data set input file may also contain comments which are
 * lines starting with semi-colons.<p>
 *
 * An example of an input file may look like this:
 * <pre>
 * ;;;
 * ;; creation of the song 'baby got back' by sir mix-a-lot
 * ;;
 * 8/12/1963 8:21:00am,	33
 * 5/5/1992  4:12:41pm	,	90
 * </pre>
 *
 * @author will@apmindsf.com
 */
public class SimulationDataSet
{
  private static final String COMMENT_CHAR = ";";
  private static final String DATE_FORMAT = "MM/dd/yyyy hh:mm:ssaa";

  private static SimpleDateFormat _formatter =
    new SimpleDateFormat(DATE_FORMAT);

  private String _fileName;
  private ArrayList _dataList = new ArrayList();

  private SimulationData _firstEvent = null;
  private SimulationData _lastEvent = null;


  /**
   * Create a SimulationDataSet from an input file. Out of range
   * probabilities will be rounded off to the maximum or minimum values
   * (100 to 0).
   *
   * @param file a File object representing the input file.
   * @throws InvalidSimFileException if the input file contain invalid
   *  dates.
   */
  public static SimulationDataSet createSimulationDataSet(File file)
    throws InvalidSimFileException, IOException
  {
    return (new SimulationDataSet(file));
  }


  /**
   * Parse a simulation data file.
   */
  private SimulationDataSet(File file)
    throws InvalidSimFileException, IOException
  {
    _fileName = file.getAbsolutePath();
    BufferedReader reader = new BufferedReader(new FileReader(file));

    String line;
    int lineNum = 0;
    while ((line = reader.readLine()) != null)
    {
      lineNum++;
      if (line.startsWith(COMMENT_CHAR)) {
        continue;
      }
      line = line.trim();
      String dateStr = null;
      int i=0;
      while (i < line.length())
      {
        char c = line.charAt(i);
        if (c == ',') {
          dateStr = line.substring(0, i).trim();
          break;
        }
        i++;
      }

      if (i >= (line.length() - 1)) {
        throw (new InvalidSimFileException("Missing probability on line " +
                                           lineNum));
      }

      String probStr = line.substring(i+1).trim();

      try {
        Date d = _formatter.parse(dateStr);
        int prob = Integer.parseInt(probStr);
        if (prob > 100) {
          prob = 100;
        }
        else if (prob < 0) {
          prob = 0;
        }
        SimulationData data = new SimulationData(d, prob);
        _dataList.add(data);
        if ((_firstEvent == null) || _firstEvent._date.after(data._date)) {
          _firstEvent = data;
        }

        if ((_lastEvent == null) || _lastEvent._date.before(data._date)) {
          _lastEvent = data;
        }
      }
      catch (ParseException e)
      {
        throw (new InvalidSimFileException("Invalid timestamp on line " +
                                           lineNum));
      }
      catch (NumberFormatException e)
      {
        throw (new InvalidSimFileException("Invalid probability on line " +
                                           lineNum));
      }
    }

    reader.close();
  }


  public Iterator iterator()
  {
    return (_dataList.iterator());
  }

  public SimulationData getFirstEventData() {
    return _firstEvent;
  }

  public SimulationData getLastEventData() {
    return _lastEvent;
  }

  public long getFirstEventTime()
  {
    if (_firstEvent == null) {
      throw (new RuntimeException("Empty simulation data set"));
    }

    return (_firstEvent._date.getTime());
  }


  public long getLastEventTime()
  {
    if (_lastEvent == null) {
      throw (new RuntimeException("Empty simulation data set"));
    }

    return (_lastEvent._date.getTime());
  }


  /**
   * Overwrite this to spit out the name of the data file when this
   * is saved as a property.
   */
  public String toString()
  {
    try {
      return URLEncoder.encode(_fileName, "ISO-8859-1");
    }
    catch (UnsupportedEncodingException e) {
      e.printStackTrace();
      return (_fileName);
    }
  }


  public class SimulationData
  {
    Date _date;
    int _prob;

    public SimulationData(Date d, int prob)
    {
      _date = d;
      _prob = prob;
    }

    public String toString() {
      return (_formatter.format(_date) + ", " + _prob + "%");
    }
  }

} // end class SimulationDataSet
