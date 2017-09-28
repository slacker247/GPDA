/*
 *  StandAloneAnnie.java
 *
 *
 * Copyright (c) 2000-2001, The University of Sheffield.
 *
 * This file is part of GATE (see http://gate.ac.uk/), and is free
 * software, licenced under the GNU Library General Public License,
 * Version 2, June1991.
 *
 * A copy of this licence is included in the distribution in the file
 * licence.html, and is also available at http://gate.ac.uk/gate/licence.html.
 *
 *  hamish, 29/1/2002
 *
 *  $Id: StandAloneAnnie.java,v 1.2 2002/02/06 15:23:49 nasso Exp $
 */
package SAA;

import java.util.*;
import java.io.*;
import java.net.*;

import gate.*;
import gate.creole.*;
import gate.util.*;
import gate.gui.*;
import gate.corpora.RepositioningInfo;

//import creole.templite.*;


/**
 * This class illustrates how to use ANNIE as a sausage machine
 * in another application - put ingredients in one end (URLs pointing
 * to documents) and get sausages (e.g. Named Entities) out the
 * other end.
 * <P><B>NOTE:</B><BR>
 * For simplicity's sake, we don't do any exception handling.
 */
public class StandAloneAnnie{

  /** The Corpus Pipeline application to contain ANNIE */
    private gate.creole.SerialAnalyserController annieController;
    private boolean debug = false;//true;
    private String TxtFile = null;
    public String WorkingDir = "/home/jeffmac";
    public String CurrentRules = "Test";

    public StandAloneAnnie()
    {
    }
    public StandAloneAnnie(String inFile)
    {
        if(debug) Out.prln(inFile);
        char val = ' ';
        int x = 0;
        String Temp = "";
        while(val != '.')
        {
            val = inFile.charAt(x);
            if(debug) Out.prln(x);
            x++;
        }
        x -= 1;
        int y = x;
        while(val != '/')
        {
            val = inFile.charAt(y);
            if(debug) Out.prln(y + " " + val);
            y--;
        }
        y += 2;
        Temp = inFile.substring(0, x);
        TxtFile = Temp;
    }
  /**
   * Initialise the ANNIE system. This creates a "corpus pipeline"
   * application that can be used to run sets of documents through
   * the extraction system.
   */
  public void initAnnie() throws gate.util.GateException {
    Out.prln("Initialising ANNIE...");

    // create a serial analyser controller to run ANNIE with
    annieController =
      (gate.creole.SerialAnalyserController) Factory.createResource(
        "gate.creole.SerialAnalyserController", Factory.newFeatureMap(),
        Factory.newFeatureMap(), "ANNIE_" + gate.Gate.genSym()
      );

    // load each PR as defined in ANNIEConstants
    gate.FeatureMap params = null;
    for(int i = 0; i < gate.creole.ANNIEConstants.PR_NAMES.length; i++) {
      Out.prln("Adding PR " + gate.creole.ANNIEConstants.PR_NAMES[i]);
      if(gate.creole.ANNIEConstants.PR_NAMES[i].equalsIgnoreCase("gate.creole.gazetteer.DefaultGazetteer"))
      {
          params = Factory.newFeatureMap();
          params.put("encoding", "ISO-8859-1");

          try{
              URL gazetteerURL = new URL("file:" + WorkingDir + "/AssistRQ/Rules_Words/" + CurrentRules + "/Lists/lists.def");
              params.put("listsURL", gazetteerURL);
      }catch(MalformedURLException e)
          {
              e.printStackTrace();
          }
      }
      else
        params = Factory.newFeatureMap(); // use default parameters

////////////////////////Need to change params here//////////////////////////////

      gate.ProcessingResource pr = (gate.ProcessingResource)
        Factory.createResource(gate.creole.ANNIEConstants.PR_NAMES[i], params);

      // add the PR to the pipeline controller
      annieController.add(pr);
    } // for each ANNIE PR

    CreoleRegister cr = Gate.getCreoleRegister();
    URL UrlTemp = null;
    String TempName = TxtFile + ".tags";
    try
    {
        UrlTemp = new URL("file:" + WorkingDir + "/AssistRQ/Modules/Template");
    } catch (MalformedURLException e)
    {
        Out.prln("Bad URL");
    }
      params = Factory.newFeatureMap();

      try{
          URL TemplateURL = new URL("file:" + WorkingDir + "/AssistRQ/");
          params.put("workingdir", TemplateURL);
          params.put("outputfile", TempName);
          params.put("currentrules", CurrentRules);
      }catch(MalformedURLException e)
      {
          e.printStackTrace();
      }
      Out.prln(UrlTemp);
    cr.registerDirectories(UrlTemp);
    Set resourcesCr = cr.getPrTypes();
      Out.prln("Adding PR Template ... ");
      gate.ProcessingResource pr = (gate.ProcessingResource)
        Factory.createResource("Templite", params);
      annieController.add(pr);

      params = Factory.newFeatureMap();
    try
    {
        UrlTemp = new URL("file:" + WorkingDir + "/AssistRQ/Modules/Hedger");
    } catch (MalformedURLException e)
    {
        Out.prln("Bad URL");
    }
      try{
          params.put("outputname", TempName);
          Out.prln(TempName);
          URL TemplateURL = new URL("file:" + WorkingDir + "/AssistRQ/");
          params.put("workingdir", TemplateURL);
      }catch(MalformedURLException e)
      {
          e.printStackTrace();
      }
    cr.registerDirectories(UrlTemp);
    resourcesCr = cr.getPrTypes();
      Out.prln("Adding PR Hedger ... ");
      pr = (gate.ProcessingResource)
        Factory.createResource("Hedger", params);
      annieController.add(pr);

      Out.prln("...ANNIE loaded");
  } // initAnnie()

  /** Tell ANNIE's controller about the corpus you want to run on */
  public void setCorpus(gate.Corpus corpus) {
    annieController.setCorpus(corpus);
  } // setCorpus

  /** Run ANNIE */
  public void execute() throws gate.util.GateException {
    Out.prln("Running ANNIE...");
    annieController.execute();
    Out.prln("...ANNIE complete");
  } // execute()

  static public class SortedAnnotationList extends java.util.Vector {
    public SortedAnnotationList() {
      super();
    } // SortedAnnotationList

    public boolean addSortedExclusive(Annotation annot) {
      Annotation currAnot = null;

      // overlapping check
      for (int i=0; i<size(); ++i) {
        currAnot = (Annotation) get(i);
        if(annot.overlaps(currAnot)) {
          return false;
        } // if
      } // for

      long annotStart = annot.getStartNode().getOffset().longValue();
      long currStart;
      // insert
      for (int i=0; i < size(); ++i) {
        currAnot = (Annotation) get(i);
        currStart = currAnot.getStartNode().getOffset().longValue();
        if(annotStart < currStart) {
          insertElementAt(annot, i);
  /*
  Out.prln("Insert start: "+annotStart+" at position: "+i+" size="+size());
  Out.prln("Current start: "+currStart);
  */
          return true;
        } // if
      } // for

      int size = size();
      insertElementAt(annot, size);
//Out.prln("Insert start: "+annotStart+" at size position: "+size);
      return true;
    } // addSorted
  } // SortedAnnotationList
} // class StandAloneAnnie

/**
 *
 */
