import java.awt.*;
import java.io.*;
import java.lang.*;
import java.net.*;

import java.applet.Applet;
import java.util.Vector;
import java.util.Enumeration;
import java.util.StringTokenizer;
import java.util.GregorianCalendar;
import java.util.Calendar;
import java.util.Properties;

import com.objectspace.voyager.*;
import com.objectspace.voyager.agent.*;
//import ParkAgent;
//import ParkAgent;
//import RoamAgent;
//import RoamAgent;

public class Launch extends Frame {
  private       Thread life = null;
  Panel         panel1;
  Graphics 	g;
  Frame		aFrame;
  static final int	VERTICAL = 0;
  static final int 	HORIZONTAL = 1;

  int 		appWidth = 640;			//Our width
  int 		appHeight = 480;		//Our height
  Thread 	engine = null;			//The thread animating the images.
  Thread	killme = null;
  int 		xPos = 0;			//The current X position 
  int 		yPos = 0;			//The current Y position 
  int 		defaultPause = 2000;		//Default PAUSE time
  int 		globalPause = defaultPause;	//The global delay between refreshs
  boolean 	userPause = false;		//User paused?
  boolean	threadSuspended = false;	//
  boolean 	repeat;				//Continuous updates?
  boolean 	loaded = false;			//Can we paint yet?
  boolean 	error = false;			//Was there an initialization error?

  double	UpperLoad   = 0.55;
  double	LowerLoad   = 0.45;
  double	TotalLoad   = 0.0;
  double	AverageLoad = 0.0;
  double	MaxOutlier  = 0.0;
  double	MinOutlier  = 0.0;
  double	MaxPerturb  = 18.0;
  double        AverageTime = 0.0;
  long          CurrentTime = 0;
  double        DeltaTime   = 0.0;
  double        TotalTime   = 0.0;
  double        LoadCount   = 0.0;
  int           NowDelta    = 0;
  int		FoundLower  = -1;
  int		FoundUpper  = -1;
  int		DeltaWork   = 5;
  boolean       Balanced    = true;
  double	ProcessorCPU[]  = new double[64];    
  IParkAgent	agent[] = new IParkAgent[64];
  IRoamAgent    roamagent;
  int		loads[] = new int[64];
  String	host[]  = new String[64];
  String	port[]  = new String[64];
  String        name[]  = new String[64];

  Panel         aNewPanel1 = new Panel();
  Panel         aNewPanel2 = new Panel();
  Panel         aNewPanel3 = new Panel();
  Panel         aNewPanel4 = new Panel();
  Panel         aTravelWin;
  Panel         aGraphWin;
  Panel         aOptionWin;
  Panel         aBarWin;
  Frame		aStatsWin;
  Frame         aMessageWin;
  static TextArea	aTextArea, aMessageArea;
  Panel         aGraphArea;
  String	aTextString;
  Button        runButton = new Button("Run");
  Button        pawsButton = new Button("Pause");
  Button        rsetButton = new Button("Restart");
  Button        optsButton = new Button("Options");
  Button        quitButton = new Button("Quit");
  Menu          fileMenu = new Menu("File");
  Menu          editMenu = new Menu("Edit");
  Menu          viewMenu = new Menu("View");
  Menu          optsMenu = new Menu("Options");
  Menu          helpMenu = new Menu("Help");
  CheckboxMenuItem      iStatItem, iMessItem, iNetwItem, iHistItem, iLoadItem;

  Panel         aHomePanel[]  = new Panel[2];
  Panel         aParkPanel[]  = new Panel[6];
  Panel         aHorzPanel[]  = new Panel[6];
  Panel         aVertPanel[]  = new Panel[6];
  Panel         bVertPanel[]  = new Panel[6];
  Label         aNodeLabel[]  = new Label[6];

  static int	nagents = 4;
  static double SavePerturb = 18.0;
  static int    SaveDelta = 5;
  static int    SavePause = 2000;
  static int    SaveHosts = 16, nhosts = 16;
  static int    SaveGType = 0;
  static double SaveScale = 2.0;
  boolean       lLoadShow, lNetwShow, lStatShow, lMessShow, lHistShow;

  StatsReader   serv = null;
  static int    sockport = 1037;
  double        StartTime = 0.0;

  public void Launch(int nprocs) {
    int		i, Deposit;
    int         PerturbCount = 4;
    StringBuffer buf = new StringBuffer();
    String	s = new String();
    char	c, cc;


    try {
	  //UnixTimers t = new UnixTimers();
	  StartTime = 1000.0; //t.elapsed;
	  System.out.println( "Starting elapsed time " + StartTime );
//
//  Define Main window and some graphics Panels
//
	  Font statsFont = new java.awt.Font("Courier", Font.BOLD, 14);
	  Font netFont = new java.awt.Font("Courier", Font.BOLD, 12);

	  setLayout(new GridLayout(2,2,0,0));
	  setTitle("Software Agents");
	  resize(insets().left+insets().right+820, insets().top+insets().bottom+660);
	  add(aNewPanel1);
	  add(aNewPanel2);
	  add(aNewPanel3);
	  add(aNewPanel4);
//
//  Build and add Menus to Main window
//
	  MenuBar aMenu = new MenuBar();
	  aMenu.add(fileMenu);
	  fileMenu.add(new MenuItem("Run"));
	  fileMenu.add(new MenuItem("Pause"));
	  fileMenu.addSeparator();
	  fileMenu.add(new MenuItem("Quit"));
          aMenu.add(editMenu);
	  editMenu.add(new MenuItem("Cut"));
	  editMenu.add(new MenuItem("Copy"));
	  editMenu.add(new MenuItem("Paste"));
	  aMenu.add(viewMenu);
	  iLoadItem = new CheckboxMenuItem("Packet Load"); 
	  viewMenu.add(iLoadItem); lLoadShow = true; iLoadItem.setState(lLoadShow);
          iNetwItem = new CheckboxMenuItem("Network Comm"); 
	  viewMenu.add(iNetwItem); lNetwShow = true; iNetwItem.setState(lNetwShow);
	  iStatItem = new CheckboxMenuItem("Statistics"); 
	  viewMenu.add(iStatItem); lStatShow = false; iStatItem.setState(lStatShow);
	  iMessItem = new CheckboxMenuItem("Messages"); 
	  viewMenu.add(iMessItem); lMessShow = false; iMessItem.setState(lMessShow);
	  iHistItem = new CheckboxMenuItem("History"); 
	  viewMenu.add(iHistItem); lHistShow = true; iHistItem.setState(lHistShow);
	  aMenu.add(optsMenu);
	  optsMenu.add(new MenuItem("Set"));
	  optsMenu.add(new MenuItem("Reset"));
	  optsMenu.addSeparator();
	  optsMenu.add(new MenuItem("AbsTime"));
	  optsMenu.add(new MenuItem("DifTime"));
	  optsMenu.addSeparator();
	  optsMenu.add(new MenuItem("Q Loads"));
	  aMenu.add(helpMenu);
	  helpMenu.add(new MenuItem("Overview..."));
	  helpMenu.add(new MenuItem("Topics..."));
	  helpMenu.add(new MenuItem("About..."));
	  aMenu.setHelpMenu(helpMenu);
	  setMenuBar(aMenu);
	  show();
//
//  Map the Performance graph to the Main window
//
	  aGraphWin = new GraphApplet();
	  aGraphWin.setFont(statsFont);
	  aGraphWin.resize(410, 300);
          GraphApplet.setGraphics(SaveGType, SaveScale);
	  aNewPanel2.add(aGraphWin);
	  if (lHistShow) aGraphWin.show();
	  aGraphWin.repaint();
//
//  Map the Timing Bar graphs to the Main window
//
	  aBarWin = new BarGraph();
	  aBarWin.setFont(statsFont);
	  aBarWin.resize(410, 300);
	  aNewPanel4.add(aBarWin);
	  if (lLoadShow) aBarWin.show();
	  aBarWin.repaint();
//
//  Map the Network graph to the Main window
//
	  aTravelWin = new TravelView();
	  TravelView.travelInit(aTravelWin);
	  aTravelWin.setFont(netFont);
	  aTravelWin.resize(410, 300);
	  aNewPanel1.add(aTravelWin);
	  if (lNetwShow) aTravelWin.show();
	  aTravelWin.repaint();
//
//  Map the Options window to the Main window
//
	  aOptionWin = new OptionsDialog();
	  OptionsDialog.optionInit(aOptionWin);
	  aOptionWin.setFont(statsFont);
	  aOptionWin.resize(410, 300);
	  aNewPanel3.add(aOptionWin);
	  aOptionWin.show();
	  aOptionWin.repaint();
//
//  Open other user interface windows for Statistics and Messages
//
	  aStatsWin = new Frame("Statistics View");
	  aStatsWin.setFont(statsFont);
	  aStatsWin.resize(300, 240);
	  aTextArea = new TextArea();
	  aStatsWin.add(aTextArea);
	  aStatsWin.move(50, 50);
	  if (lStatShow) aStatsWin.show();

	  aMessageWin = new Frame("Message View");
	  aMessageWin.setFont(statsFont);
	  aMessageWin.resize(680, 220);
	  aMessageArea = new TextArea();
	  aMessageWin.add(aMessageArea);
	  aMessageWin.move(360,800);
	  if (lMessShow) aMessageWin.show();
//
//  Listen on SPEEDES socket output port for GVT messages
//
	  /*
	  try {
	    serv = new StatsReader (sockport);
	    serv.start();
	    //serv.join();
	  }
	  catch (IOException e) { System.err.println("Error starting reader: " + e); return; }
	  */
//
//  Build all the agents
//
	  host[0] =  Agent.of( this ).getHome();	// Save Home address
	  RandomAccessFile in = new RandomAccessFile("Ragents", "r"); 
	  s = in.readLine();
	  StringTokenizer sn = new StringTokenizer(s);
	  s = sn.nextToken();				// Skip 'Rem'
	  s = sn.nextToken();				// Skip comment
	  s = sn.nextToken();				// Get agent count
          Integer Ij = new Integer(1);
          nagents = Ij.parseInt(s);
	  nhosts = nagents;
//
//  Build Parking Agents and send to appropriate node
//
	  for( i = 1; i <= nagents; i++ ) {
	  	s = in.readLine();
		//System.out.println( "Processing "+s );
		StringTokenizer st = new StringTokenizer(s);
		s = st.nextToken();			// Skip 'rsh'
		s = st.nextToken();			// Skip '-l'
		s = st.nextToken();			// Skip userid
		host[i] = st.nextToken();		// Get host address
		s = st.nextToken();			// Skip 'voyager'
		port[i] = st.nextToken();		// Get port number
		name[i] = "//" + host[i] + ":" + port[i];
		Voyager.startup(name[i]);
		String parkClass = ParkAgent.class.getName();
		agent[i] = (IParkAgent)Factory.create(parkClass, name[i] );
		agent[i].addToItinerary( name[i], i );
		agent[i].SetPolling( defaultPause );
		agent[i].SetUpper( (int)(UpperLoad*100.0) );
		agent[i].SetLower( (int)(LowerLoad*100.0) );
		agent[i].SetLoad( (int)(Math.random() * 90.0) );
		System.out.println( "Sending agent to "+name[i] );
		agent[i].launch();			// send the agent on its way
	  }
//
//  Build a stationary Home Agent
//
          Voyager.startup("//localhost:8001");
	  String homeClass = HomeAgent.class.getName();
          IHomeAgent homeagent = (IHomeAgent)Factory.create(homeClass,
                                  "//localhost:8001");
	  //homeagent.liveForever(); 
	  //homeagent.saveNow();
	  homeagent.arrived();
//
//  Build Roaming Agent and start it on its way
//
	  //int roamHome  = 1;
	  int roamIndex = 1;
	  int roamCount = 4;
          Voyager.startup("//localhost:8002");
	  String roamClass = RoamAgent.class.getName();
	  roamagent = (IRoamAgent)Factory.create(roamClass, "//localhost:8002" );
	  for( i = 1; i <= 4; i++ ) {
		roamagent.addToItinerary( "//" + host[i] + ":" + port[i], i );
	  }
	  System.out.println( "Start Roaming Agent ");
	  //roamagent.liveForever();
	  roamagent.launch();			       // send the agent on its way
//
//  Popup the System Load view
//
	  /*
	  try
	    {
	      Process ProcVu = Runtime.getRuntime().exec("xsysinfo");
	    }
	  catch (IOException e) { System.out.println(e); }
	  */
	  System.out.println( "Initialization time " + (/*t.Getelapsed()-*/StartTime) );
//
//		Do Load Balancing until user terminated.
//
	  BarGraph.setLoaded(true);
	  loaded = true;
	  repaint();
	  while (true) {
          while (userPause == false) {
	    //	    nhosts = SaveHosts;
	    DeltaWork = SaveDelta;
	    MaxPerturb = SavePerturb;
	    defaultPause = SavePause;
    		try
		  {
	            Thread.sleep( defaultPause );
		    PerturbCount = PerturbCount - 1;
		    if (PerturbCount <= 0) {
			PerturbCount = (int)(Math.random()*6.0);
			i = (int) (Math.random()*(nhosts) + 1);
			Deposit = (int)(Math.random() * MaxPerturb); 
			if ( (Math.random()*2.0 - 1.0) <= 0.0) {
			    agent[i].withdraw(Deposit);	// Take work
			    //System.out.println( "Withdraw " + Deposit + " from " + i );
			  } else {
			    agent[i].deposit(Deposit);	// Add work
                            //System.out.println( "Deposit " + Deposit + " in " + i );
			  }
		    } // End Perturbation
//
//	Get work load for all processors.
//	Find Average work load.
//	Find upper and lower extremes (outliers).
//	Find a Node that needs more work (if any)
//
		    MaxOutlier = -1000.0;
		    MinOutlier = +1000.0;
		    TotalLoad = 0.0;
		    FoundLower = -1;
	  	    for( i = 1; i <= nhosts; i++ ) {
			ProcessorCPU[i] = agent[i].getBalance();
			TotalLoad = TotalLoad + ProcessorCPU[i];
			if (ProcessorCPU[i] > MaxOutlier) MaxOutlier = ProcessorCPU[i];
                        if (ProcessorCPU[i] < MinOutlier) MinOutlier = ProcessorCPU[i];
                        if (ProcessorCPU[i] < LowerLoad) FoundLower = i;
		    }
		    AverageLoad = TotalLoad/nhosts;
		    LowerLoad  = AverageLoad - 0.05;
		    GraphApplet.putArray(MaxOutlier*100.0,MinOutlier*100.0,AverageLoad*100.0);
		    Balanced = true;
		    if (MinOutlier < (AverageLoad - 0.05)) Balanced = false;
		    if (MaxOutlier > (AverageLoad + 0.05)) Balanced = false;
//
//	If there is a node which needs more work, find one that needs less work (if any)
//	If one found, transfer some work from one to the other.
//
		    if (FoundLower > 0) {
			FoundUpper = -1;
                        for( i = 1; i <= nhosts; i++ ) {
                           if (ProcessorCPU[i] > UpperLoad) FoundUpper = i;
                        }
			if (FoundUpper > 0) {
			  if (DeltaWork > ProcessorCPU[FoundUpper]*100.0) 
			      DeltaWork = (int)(ProcessorCPU[FoundUpper]*100.0);
			    CurrentTime = java.lang.System.currentTimeMillis();
		            agent[FoundUpper].withdraw(DeltaWork);
		            agent[FoundLower].deposit(DeltaWork);
			    DeltaTime = (double)(java.lang.System.currentTimeMillis() - CurrentTime);
			    LoadCount = LoadCount+1.0;
			    TotalTime = TotalTime+DeltaTime;
			    AverageTime = TotalTime/LoadCount;
			    if (ProcessorCPU[FoundUpper]*100.0 > 80.0) {
			      //
			      //   Notify testbed
			      //
			      int gpdaport = 8127;
			      String gpdahost = new String("158.114.52.140");
			      String sRecord = "158.114.52.140";
                              String byteStream = "";
			      InetAddress inet;
			      try {
                                byteStream = "NetSim GPDA IOW 3 " + sRecord.length() + " 0 " + sRecord;
				System.out.println("Socket created to " + gpdahost + "  " + gpdaport);
			        DatagramSocket outSock = new DatagramSocket();
				inet = InetAddress.getByName(gpdahost);
			        byte obuf[] = byteStream.getBytes();

				DatagramPacket datagram = new DatagramPacket(obuf, byteStream.length());
				datagram.setPort(gpdaport);
				datagram.setAddress(inet);
				datagram.setLength(byteStream.length());
				outSock.send(datagram);
				outSock.close();
			      } catch (Exception e) {
				e.printStackTrace();
			      }
			    }
			}
		    }
		    BarGraph.putCPUTimes(ProcessorCPU, nhosts);
	            aBarWin.repaint();
		    updateStats();
		    aGraphWin.repaint();
		  }
    		catch( InterruptedException exception ) {}
//
//     Move roaming agent to next location on itinerary
//
	  roamCount = roamCount - 1;
	  if (roamCount <= 0) {
	      roamCount = 4;

	      TravelView.travelLeave(roamIndex);
	      roamIndex = roamIndex+1;
	      if (roamIndex > 4) roamIndex = 1;
	      try
	      	{
		    //roamagent.Agent.of( this ).getHome().moveTo(host[roamIndex]+":"+port[roamIndex]);
	        }
	      catch( Exception exception ) { System.err.println( exception ); }
	      //aParkPanel[roamIndex].setBackground(Color.red);
	      TravelView.travelArrive(roamIndex);
	      aTravelWin.repaint();
	      putMessage(roamagent.getUptime()+"\n");
	    }

	  } // end while
	  try {
	    Thread.sleep(2000);
	  }
	  catch( InterruptedException exception ) {}
          } // end infinite while
    } catch( Exception exception )
	{ System.err.println( exception ); }
    /*
    catch( FileNotFoundException exception)
	{ System.err.println( exception ); }
    catch( IOException exception )
	{ System.err.println( exception ); }
    */
  }			// ***** End Launch ***** \\

  public static void setOptions(int nhost, int dwork, int perturb, int polling) {

    SaveHosts = nhost;
    SaveDelta = dwork;
    SavePerturb = perturb;
    SavePause = polling;
  }			// ***** End setOptions ***** \\

  public static void putMessage(String msg) {
                                      
    //          System.out.println(msg);
	  aMessageArea.append(msg);
  }			// ***** End putMessage ***** \\

  public static void newAddress(String agentaddr, int index) {

    System.out.println( "New agent address" + agentaddr + " with index " + index );
  }			// ***** End newAddress ***** \\

  public synchronized void updateStats() {

	  StringBuffer aBuffer = new StringBuffer();
	  String aTextString   = new String();
          aBuffer.append("Available Agents      " + "\t" +
		 String.valueOf(nagents) + "\n");
          aBuffer.append("Nodes Load Balanced   " + "\t" +
		 String.valueOf(nhosts) + "\n");
	  aBuffer.append("Average Load          " + "\t" +
		 String.valueOf((int)(AverageLoad*100.0)) + "\n");
	  aBuffer.append("Upper Outlier         " + "\t" +
		 String.valueOf((int)(MaxOutlier*100.0)) + "\n");
	  aBuffer.append("Lower Outlier         " + "\t" +
		 String.valueOf((int)(MinOutlier*100.0)) + "\n");
	  aBuffer.append("Load Tolerance        " + "\t" +
		 String.valueOf(5) + "\n");
	  aBuffer.append("Delta Work Unit       " + "\t" +
		 String.valueOf(DeltaWork) + "\n");
	  aBuffer.append("Max. Perturbation     " + "\t" +
		 String.valueOf((int)(MaxPerturb)) + "\n");
	  aBuffer.append("Polling Rate (mS)     " + "\t" +
		 String.valueOf(defaultPause) + "\n");
	  aBuffer.append("Load Balanced         " + "\t" +
		 String.valueOf(Balanced) + "\n");
	  aBuffer.append("Avg Comm Overhead (mS)" + "\t" +
		 String.valueOf((int)(AverageTime)) + "\n");
	  aTextString = aBuffer.toString();
	  aTextArea.setText(aTextString);
  }			// ***** End updateStats ***** \\

  public boolean handleEvent(Event event) {
    int        i;
    Integer    Ij = new Integer(1);
    ParkAgent  person;

    try {
	
    	if (event.id == Event.WINDOW_DESTROY) {
/*
            for( i = 1; i <= nagents; i++ ) {
	         person = (ParkAgent) Object.forObjectAt(host[i]+":"+port[i]+
					       "/Fred"+String.valueOf(i) );
                 person.dismiss();              // Destroy Parking agent
	    }
            person = (IParkAgent) Object.forObjectAt( "localhost:8001/HOMEAGENT1" );
            person.dismiss();                   // Destroy Home agent
            person = (IParkAgent) Object.forObjectAt( "localhost/Wilma" );
	    person.dismiss();                   // Destroy Roaming agent
*/
            hide();         			// hide the Frame
            dispose();      			// free the system resources
	    Voyager.shutdown();			// shutdown Voyager
            System.exit(0); 			// close the application
            return true;
    	}

	if (event.target instanceof MenuItem) {
	  //System.out.println( "MenuItem: " + lStatShow );
	}

	if (event.target instanceof CheckboxMenuItem) {
	  CheckboxMenuItem checkitem = (CheckboxMenuItem)event.target;
	  if (checkitem == iStatItem) { 
	      lStatShow = !lStatShow;
	      if (lStatShow) aStatsWin.show();
	      else           aStatsWin.hide();
	  }
	  if (checkitem == iMessItem) {
	      lMessShow = !lMessShow;
	      if (lMessShow) aMessageWin.show();
	      else           aMessageWin.hide();
	  }
	  if (checkitem == iHistItem) {
	      lHistShow = !lHistShow;
	      if (lHistShow) aGraphWin.show();
	      else           aGraphWin.hide();
	  }
	  if (checkitem == iLoadItem) {
	      lLoadShow = !lLoadShow;
	      if (lLoadShow) aBarWin.show();
	      else           aBarWin.hide();
	  }
	  if (checkitem == iNetwItem) {
	      lNetwShow = !lNetwShow;
	      if (lNetwShow) aTravelWin.show();
	      else           aTravelWin.hide();
	  }
	}

	if (event.id == Event.ACTION_EVENT) {
	  if ("Run".equals(event.arg)) {
	      userPause = false;
	  }
	  if ("Pause".equals(event.arg)) {
	      userPause = true;
	  }
	  if ("Exit".equals(event.arg)) {
            for( i = 1; i <= nagents; i++ ) {
	         System.out.println( "Dismissing: " + name[i] );
	         //person = (VParkAgent) VObject.forObjectAt(name[i]);
                 //person.dismiss();              // Destroy Parking agent
	    }
            //person = (VParkAgent) VObject.forObjectAt( "localhost/HOMEAGENT1" );
            //person.dismiss();                   // Destroy Home agent
            //person = (VParkAgent) VObject.forObjectAt( "localhost/Wilma" );
	    //person.dismiss();                   // Destroy Roaming agent
            hide();         			// hide the Frame
            dispose();      			// free the system resources
	    Voyager.shutdown();		        // shutdown Voyager
            System.exit(0); 			// close the application
          }

	  if ("Set".equals(event.arg)) {
	    //new OptionsDialog(this);
	  }
	  if ("Reset".equals(event.arg)) {
	      SavePerturb = 18.0;
	      SaveDelta   = 5;
	      SavePause   = 2000;
	      SaveHosts   = 16;
	      SaveGType   = 0;
	      SaveScale   = 2.0;
	      GraphApplet.setGraphics(SaveGType, SaveScale);
          }
	  if ("AbsTime".equals(event.arg)) {
	      SaveGType = 1;
	      GraphApplet.setGraphics(SaveGType, -1.0);
          }
	  if ("DifTime".equals(event.arg)) {
	      SaveGType = 0;
	      GraphApplet.setGraphics(SaveGType, -1.0);
          }
	  if ("Q Loads".equals(event.arg)) {
	      SaveGType = 2;
	      GraphApplet.setGraphics(SaveGType, -1.0);
          }
	  if ("About...".equals(event.arg)) {
	      new AboutDialog(this);
	  }
       	}
      }
    catch( Exception exception )
        { System.err.println( exception ); }

    return super.handleEvent(event);
  }                     // ***** End handleEvent ***** \\

  public static void main(String args[]) {
    Integer Ii = new Integer(1);

      int i = 16;
//    int i = Ii.parseInt(args[0]);
      (new Launch()).Launch(i);
  }                     // ***** End main ***** \\
 /***************************************************************************/
 /************************* End     Launch    Class *************************/
 /***************************************************************************/
}

class OptionsDialog extends Panel {
  static TextField     field1 = new TextField("4", 10);
  static TextField     field2 = new TextField("5", 10);
  static TextField     field3 = new TextField("18", 10);
  static TextField     field4 = new TextField("2000", 10);
  static TextField     field5 = new TextField("2", 10);
  static Button        okButton = new Button("Apply");
  static Button        canButton = new Button("Cancel");

  public static void optionInit(Panel aDialog) {

    Font dialogFont = new java.awt.Font("Courier", Font.BOLD, 12);

    aDialog.setFont(dialogFont);
    aDialog.setLayout(new GridLayout(6,2,10,10));
    
    Label label1 = new Label("Nodes To Monitor", Label.LEFT);
    aDialog.add(label1);
    aDialog.add(field1);
    label1.reshape(15, 30, 200, 30);
    field1.reshape(205, 30, 100, 30);

    Label label2 = new Label("Delta Load    ", Label.LEFT);
    aDialog.add(label2);
    aDialog.add(field2);
    label2.reshape(15, 60, 200, 30);
    field2.reshape(205, 60, 100, 30);

    Label label3 = new Label("Perturbation  ", Label.LEFT);
    aDialog.add(label3);
    aDialog.add(field3);
    label3.reshape(15, 90, 200, 30);
    field3.reshape(205, 90, 100, 30);

    Label label4 = new Label("Polling Rate  ", Label.LEFT);
    aDialog.add(label4);
    aDialog.add(field4);
    label4.reshape(15, 120, 200, 30);
    field4.reshape(205, 120, 100, 30);

    Label label5 = new Label("Graph Scaling ", Label.LEFT);
    aDialog.add(label5);
    aDialog.add(field5);
    label5.reshape(15, 150, 200, 30);
    field5.reshape(205, 150, 100, 30);

    aDialog.add(okButton);
    okButton.reshape(80, 230, 80, 22);
    aDialog.add(canButton);
    canButton.reshape(200, 230, 80, 22);
  }

  public Insets insets() {
    return new Insets(3,3,3,3);
  }

  public void paint(Graphics g) {
    Dimension  mySize = size();
    Insets     myInsets = insets();

    g.setColor(Color.white);
    g.fillRect(0,0,mySize.width,myInsets.top);
    g.fillRect(0,0,myInsets.left,mySize.height);
    g.setColor(Color.black);
    g.fillRect(mySize.width-myInsets.right,0,myInsets.right,mySize.height);
    g.fillRect(0,mySize.height-myInsets.bottom,mySize.width,mySize.height);
  }

  public boolean handleEvent(Event event) {
    int i;
    int nhosts, dwork, perturb, polling;
    double scale;
    Integer Ij = new Integer(1);

    if (event.id == Event.ACTION_EVENT) {
        if (event.target == okButton) {
            nhosts = Ij.parseInt(field1.getText());
            dwork = Ij.parseInt(field2.getText());
            perturb = Ij.parseInt(field3.getText());
            polling = Ij.parseInt(field4.getText());
	    Launch.setOptions(nhosts, dwork, perturb, polling);
	    scale = (double)Ij.parseInt(field5.getText());
	    GraphApplet.setGraphics(-1, scale);
            System.out.println( "Changing Options to: " + nhosts + "  " + dwork +
				"  " + perturb + "  " + polling + "  " + scale);
	    //dispose();
	    return true;
        }
        if (event.target == canButton) {
            System.out.println( "Dialog Cancelled with NO Changes" );
	    //dispose();
	    return true;
        }
    }
    
    if (event.id == Event.WINDOW_DESTROY) {
      //dispose();      			// free the system resources
       return true;
    }
    
    return false; //super.handleEvent(event);
  }
 /***************************************************************************/
 /************************* End OptionsDialog Class *************************/
 /***************************************************************************/
}

class AboutDialog extends Dialog {
  Button        okButton = new Button("Ok");
  Button        canButton = new Button("Cancel");

  AboutDialog(Frame parent) {

          super(parent, true);
	  setTitle("About Launch");
	  Font dialogFont = new java.awt.Font("Courier", Font.BOLD, 12);

	  Panel aDialog = new Panel();
	  aDialog.setFont(dialogFont);
	  aDialog.setLayout(new GridLayout(9,2,2,2));
	  add(aDialog);

	  Properties props = System.getProperties();
	  //props.list(System.out);
	  Calendar cal = new GregorianCalendar();
	  int iYear = cal.get(Calendar.YEAR);
	  int iMon  = cal.get(Calendar.MONTH)+1;
	  int iDay  = cal.get(Calendar.DAY_OF_MONTH);
	  int iHour = cal.get(Calendar.HOUR);
	  int iMin  = cal.get(Calendar.MINUTE);
	  int iSec  = cal.get(Calendar.SECOND);

	  Label label1 = new Label("Application Name", Label.LEFT);
	  Label field1 = new Label("Launch", Label.RIGHT);
	  aDialog.add(label1);
	  aDialog.add(field1);
	  Label label2 = new Label("Organization", Label.LEFT);
	  Label field2 = new Label("TRW/IAP", Label.RIGHT);
	  aDialog.add(label2);
	  aDialog.add(field2);
	  Label label3 = new Label("Current Time", Label.LEFT);
	  Label field3 = new Label(iHour+":"+iMin+":"+iSec, Label.RIGHT);
	  aDialog.add(label3);
	  aDialog.add(field3);
	  Label label4 = new Label("Current Date", Label.LEFT);
	  Label field4 = new Label(iMon+"/"+iDay+"/"+iYear, Label.RIGHT);
	  aDialog.add(label4);
	  aDialog.add(field4);
	  Label label5 = new Label("Architecture", Label.LEFT);
	  Label field5 = new Label(System.getProperty("os.arch")+"/"+
                                   System.getProperty("os.name"), Label.RIGHT);
	  aDialog.add(label5);
	  aDialog.add(field5);
	  String Tmem = String.valueOf((int)(Runtime.getRuntime().totalMemory()/1024));
	  String Amem = String.valueOf((int)(Runtime.getRuntime().freeMemory()/1024));
	  Label label6 = new Label("Memory (Total/Avail)", Label.LEFT);
	  Label field6 = new Label(Tmem+"KB/"+Amem+"KB", Label.RIGHT);
	  aDialog.add(label6);
	  aDialog.add(field6);
	  Label label7 = new Label("Java Version", Label.LEFT);
	  Label field7 = new Label(System.getProperty("java.version"), Label.RIGHT);
	  aDialog.add(label7);
	  aDialog.add(field7);
	  Label label8 = new Label("User's Name", Label.LEFT);
	  Label field8 = new Label(System.getProperty("user.name"), Label.RIGHT);
	  aDialog.add(label8);
	  aDialog.add(field8);

       	  aDialog.add(okButton);
	  aDialog.add(canButton);

	  resize(300, 240);
	  move(50,80);
	  show();
  }

  public boolean handleEvent(Event event) {
    if (event.id == Event.ACTION_EVENT) {
        if (event.target == okButton) {
	    dispose();
	    return true;
        }
        if (event.target == canButton) {
	    dispose();
	    return true;
        }
    }

    if (event.id == Event.WINDOW_DESTROY) {
       dispose();      			// free the system resources
       return true;
    }

    return false; //super.handleEvent(event);
  }
 /***************************************************************************/
 /************************* End  AboutDialog  Class *************************/
 /***************************************************************************/
}

class TravelView extends Panel {
  static Panel   aParkPanel[]  = new Panel[6];
  static Panel   aHorzPanel[]  = new Panel[6];
  static Panel   aVertPanel[]  = new Panel[6];
  static Panel   bVertPanel[]  = new Panel[6];
  static Label   aNodeLabel[]  = new Label[6];
  static Color   backColor;

  public static void travelInit(Panel parent)  {

    java.awt.Button button1;
    java.awt.TextArea textArea1;
    java.awt.Label label1;
    java.awt.Label label2;
    java.awt.Label label3;
    java.awt.Label label4;

      backColor = parent.getBackground();

      aParkPanel[0] = new java.awt.Panel();    // Home Node
      aParkPanel[0].setLayout(null);
      aParkPanel[0].reshape(150,170,50,30);
      aParkPanel[0].setBackground(Color.blue);
      parent.add(aParkPanel[0]);

      aParkPanel[1] = new java.awt.Panel();    // Node 1
      aParkPanel[1].setLayout(null);
      aParkPanel[1].reshape(15,75,50,30);
      aParkPanel[1].setBackground(Color.blue);
      parent.add(aParkPanel[1]);
      aParkPanel[2] = new java.awt.Panel();    // Node 2
      aParkPanel[2].setLayout(null);
      aParkPanel[2].reshape(105,75,50,30);
      aParkPanel[2].setBackground(Color.blue);
      parent.add(aParkPanel[2]);
      aParkPanel[3] = new java.awt.Panel();    // Node 3
      aParkPanel[3].setLayout(null);
      aParkPanel[3].reshape(195,75,50,30);
      aParkPanel[3].setBackground(Color.blue);
      parent.add(aParkPanel[3]);
      aParkPanel[4] = new java.awt.Panel();    // Node 4
      aParkPanel[4].setLayout(null);
      aParkPanel[4].reshape(285,75,50,30);
      aParkPanel[4].setBackground(Color.blue);
      parent.add(aParkPanel[4]);

      aHorzPanel[0] = new java.awt.Panel();    // Horizontal Travel Home to Node 1
      aHorzPanel[0].setLayout(null);
      aHorzPanel[0].reshape(40,185,110,2);
      aHorzPanel[0].setBackground(Color.blue);
      parent.add(aHorzPanel[0]);
      aVertPanel[0] = new java.awt.Panel();    // Vertical Travel Home to Node 1
      aVertPanel[0].setLayout(null);
      aVertPanel[0].reshape(40,105,2,80);
      aVertPanel[0].setBackground(Color.blue);
      parent.add(aVertPanel[0]);

      aHorzPanel[1] = new java.awt.Panel();    // Horizontal Travel Node 1 to 2
      aHorzPanel[1].setLayout(null);
      aHorzPanel[1].reshape(65,90,40,2);
      aHorzPanel[1].setBackground(Color.blue);
      parent.add(aHorzPanel[1]);

      aHorzPanel[2] = new java.awt.Panel();    // Horizontal Travel Node 2 to 3
      aHorzPanel[2].setLayout(null);
      aHorzPanel[2].reshape(155,90,40,2);
      aHorzPanel[2].setBackground(Color.blue);
      parent.add(aHorzPanel[2]);

      aHorzPanel[3] = new java.awt.Panel();    // Horizontal Travel Node 3 to 4
      aHorzPanel[3].setLayout(null);
      aHorzPanel[3].reshape(245,90,40,2);
      aHorzPanel[3].setBackground(Color.blue);
      parent.add(aHorzPanel[3]);

      aHorzPanel[4] = new java.awt.Panel();    // Horizontal Travel Node 4 to 1
      aHorzPanel[4].setLayout(null);
      aHorzPanel[4].reshape(40,60,270,2);
      aHorzPanel[4].setBackground(Color.blue);
      parent.add(aHorzPanel[4]);
      aVertPanel[4] = new java.awt.Panel();    // Vertical line 4A
      aVertPanel[4].setLayout(null);
      aVertPanel[4].reshape(40,60,2,30);
      aVertPanel[4].setBackground(Color.yellow);
      parent.add(aVertPanel[4]);
      bVertPanel[4] = new java.awt.Panel();    // Vertical line 4B
      bVertPanel[4].setLayout(null);
      bVertPanel[4].reshape(310,60,2,30);
      bVertPanel[4].setBackground(Color.yellow);
      parent.add(bVertPanel[4]);

      label1 = new java.awt.Label("HOME");
      label1.reshape(160,196,138,33);
      parent.add(label1);
      label2 = new java.awt.Label("Node 1");
      label2.reshape(15,12,60,24);
      parent.add(label2);
      label3 = new java.awt.Label("Node 2");
      label3.reshape(105,12,60,24);
      parent.add(label3);
      label4 = new java.awt.Label("Node 3");
      label4.reshape(195,12,60,24);
      parent.add(label4);
      label4 = new java.awt.Label("Node 4");
      label4.reshape(285,12,60,24);
      parent.add(label4);

      aParkPanel[0].setBackground(Color.blue);
      aHorzPanel[0].setBackground(Color.red);
      aVertPanel[0].setBackground(Color.red);
      try{Thread.sleep(1000);} catch(InterruptedException Exception){}
      aHorzPanel[0].setBackground(Color.blue);
      aVertPanel[0].setBackground(Color.blue);
      aParkPanel[0].setBackground(Color.red);
  }			// ***** End TravelView ***** \\

  public static void travelLeave(int roamIndex) {
  int i;

    aParkPanel[roamIndex].setBackground(Color.green);
    boolean lOn = true;
    Color travelColor = Color.red;
    for (i=0; i<10; i++) {
      if (lOn) travelColor = Color.red;
      else     travelColor = backColor;
      aHorzPanel[roamIndex].setBackground(travelColor);
      if (roamIndex == 4) {
	aVertPanel[roamIndex].setBackground(travelColor);
	bVertPanel[roamIndex].setBackground(travelColor);
      }
      //aTravelWin.repaint();
      try{Thread.sleep(100);} catch(InterruptedException Exception){}
      lOn = !lOn;
    }
    aHorzPanel[roamIndex].setBackground(Color.blue);
    if (roamIndex == 4) {
      aVertPanel[roamIndex].setBackground(Color.blue);
      bVertPanel[roamIndex].setBackground(Color.blue);
    }
  }

  public static void travelArrive(int roamIndex) {

    aParkPanel[roamIndex].setBackground(Color.red);
  }


  public Insets insets() {
    return new Insets(3,3,3,3);
  }

  public void paint(Graphics g) {
    Dimension   mySize = size();
    Insets      myInsets = insets();
    int         i;
    Font        titleFont, valueFont;
    FontMetrics titleFontMetrics;
    String      title  = "Network Comm View";

    g.setColor(Color.white);
    g.fillRect(0,0,mySize.width,myInsets.top);
    g.fillRect(0,0,myInsets.left,mySize.height);
    g.setColor(Color.black);
    g.fillRect(mySize.width-myInsets.right,0,myInsets.right,mySize.height);
    g.fillRect(0,mySize.height-myInsets.bottom,mySize.width,mySize.height);

    valueFont = new java.awt.Font("Courier", Font.BOLD, 12);
    titleFont = new java.awt.Font("Courier", Font.BOLD, 16);
    titleFontMetrics = getFontMetrics(titleFont);

    g.setColor(Color.black);
    g.setFont(titleFont);

    // draw the title centered at the bottom of the graph
    i = titleFontMetrics.stringWidth(title);
    g.drawString(title, Math.max((size().width - i)/2, 0),
		     size().height - 10 - titleFontMetrics.getDescent()*2);
  }
 /***************************************************************************/
 /************************* End  TravelView   Class *************************/
 /***************************************************************************/
}

class GraphApplet extends Panel {
  static double MaxDelta[] = new double[800];
  static double MinDelta[] = new double[800];
  static double AvgDelta[] = new double[800];
  static double MaxQueue[] = new double[800];
  static double MinQueue[] = new double[800];
  static double AvgQueue[] = new double[800];
  static double Scale = 1.0;
  static int    NowDelta = 1;
  static int    NowQueue = 1;
  static int    GrafType = 1;

    public static void putArray(double Max, double Min, double Mean) {

      if (NowDelta > 380) NowDelta = 1;
      MaxDelta[NowDelta] = Max*Scale;
      MinDelta[NowDelta] = Min*Scale;
      AvgDelta[NowDelta] = Mean*Scale;
      NowDelta = NowDelta + 1;
    }

    public static void putQueue(double Max, double Min, double Mean) {

      if (NowQueue > 380) NowQueue = 1;
      MaxQueue[NowQueue] = Max*Scale;
      MinQueue[NowQueue] = Min*Scale;
      AvgQueue[NowQueue] = Mean*Scale;
      NowQueue = NowQueue + 1;
    }

    public static void setGraphics(int type, double scale) {

      if (scale > 0.0) {
	if (scale != Scale) NowDelta = 1;
	Scale = scale;
      }
      if (type >= 0) GrafType = type;
    }

    public Insets insets() {
      return new Insets(3,3,3,3);
    }

    public void paint(Graphics g) {
      double        upper[] = new double[800];
      double        lower[] = new double[800];
      int           i;
      Dimension     mySize = size();
      Insets        myInsets = insets();

      Font	    titleFont, valueFont;
      FontMetrics   titleFontMetrics;
      String	    title  = "History View";

      g.setColor(Color.white);
      g.fillRect(0,0,mySize.width,myInsets.top);
      g.fillRect(0,0,myInsets.left,mySize.height);
      g.setColor(Color.black);
      g.fillRect(mySize.width-myInsets.right,0,myInsets.right,mySize.height);
      g.fillRect(0,mySize.height-myInsets.bottom,mySize.width,mySize.height);

      valueFont = new java.awt.Font("Courier", Font.BOLD, 12);
      titleFont = new java.awt.Font("Courier", Font.BOLD, 16);
      titleFontMetrics = getFontMetrics(titleFont);
      g.setColor(Color.black);
      g.setFont(titleFont);
      // draw the title centered at the bottom of the graph
      i = titleFontMetrics.stringWidth(title);
      g.drawString(title, Math.max((size().width - i)/2, 0),
		     size().height - 10 - titleFontMetrics.getDescent()*2);

      g.setFont(valueFont);

      switch (GrafType) {
      case 0:  // **** Differential CPU Time ****
        g.drawLine(9,  20,   9, 200);                 // Draw vertical axis
	g.drawString("Packet Load", 12, 30);
	g.drawString("Time", 340, 100);
	g.setColor(Color.blue);
        g.drawLine(9, 110, 380, 110);                 // Draw horizontal axis
        g.setColor(Color.red);
        for (int x = 1 ; x < NowDelta ; x++) {
	    g.drawLine( 9+x, 110-(int)(MaxDelta[x-1]-AvgDelta[x-1]),
		       10+x, 110-(int)(MaxDelta[x]-AvgDelta[x]));
        }
        g.setColor(Color.green);
        for (int x = 1 ; x < NowDelta ; x++) {
	    g.drawLine( 9+x, 110+(int)(AvgDelta[x-1]-MinDelta[x-1]),
		       10+x, 110+(int)(AvgDelta[x]-MinDelta[x]));
        }
	break;
      case 1:  // **** Absolute CPU Time ****
        g.drawLine(9, 200, 380, 200);                 // Draw horizontal axis
        g.drawLine(9,  20,   9, 200);                 // Draw vertical axis
	g.drawString("Time", 340, 180);
	g.drawString("Packet Load", 12, 30);
        g.setColor(Color.blue);
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
	break;
      case 2:  // **** Queue Load ****
        g.drawLine(9, 200, 380, 200);                 // Draw horizontal axis
        g.drawLine(9,  20,   9, 200);                 // Draw vertical axis
	g.drawString("Time", 340, 180);
	g.drawString("Q Load", 12, 30);
        g.setColor(Color.cyan);
        for (int x = 1 ; x < NowDelta ; x++) {
	    g.drawLine(9+x, 190-(int)AvgDelta[x-1], 10+(x), 190-(int)AvgDelta[x]);
        }
	break;
      default:
	System.out.println( "Illegal graph type specified." );
      } /*switch*/

   }
 /***************************************************************************/
 /************************* End  GraphApplet  Class *************************/
 /***************************************************************************/
}

class BarGraph extends Panel {
  private static double   ProcessorCPU[] = new double[64];
  private static double   average = 0.0;
  private static int      nhosts;
  private static boolean  loaded = false;

  public static void putCPUTimes(double times[], int n) {
    int i;

    average = 0.0;
    nhosts = n;
    for (i=0; i < n; i++) {
      ProcessorCPU[i] = times[i+1];
      average = average + ProcessorCPU[i];
    }
    average = average/(double)n;
  }

  public static void setLoaded(boolean type) {
    loaded = type;
  }

  public Insets insets() {
    return new Insets(3,3,3,3);
  }

  public synchronized void paint(Graphics g) {

  int		i, j;
  int		cx = 0, cy = 0, barHeight;
  double	percentuser, percentsys, percentidle;
  char		l[] = new char[1];
  Font		titleFont, valueFont;
  FontMetrics	titleFontMetrics;
  String	title  = "Packet Load View";
  String        label1 = "(Red = Idle Time, Yellow = System Time, Green = User Time)";
  String        label2 = "(System is Load Balanced when all Green bars are same size)";
  int		titleHeight = 15;
  int		value = 180;			//Bar height
  int		scale = 10;			//Scaling factor
  int		maxLabelWidth = 2;
  int		barWidth = 20;
  int		barSpacing = 10;
  int		max = 0;
  int	        orientation;
  int		columns;
  int		values[];
  Object	colors[];
  Object	labels[];
  Color		backgroundColor = null;
  int		styles[];
  Dimension     mySize = size();
  Insets        myInsets = insets();

      g.setColor(Color.white);
      g.fillRect(0,0,mySize.width,myInsets.top);
      g.fillRect(0,0,myInsets.left,mySize.height);
      g.setColor(Color.black);
      g.fillRect(mySize.width-myInsets.right,0,myInsets.right,mySize.height);
      g.fillRect(0,mySize.height-myInsets.bottom,mySize.width,mySize.height);

     if (loaded == false) return;

        valueFont = new java.awt.Font("Courier", Font.BOLD, 12);
	titleFont = new java.awt.Font("Courier", Font.BOLD, 16);
	titleFontMetrics = getFontMetrics(titleFont);
	g.setColor(Color.black);
	g.setFont(titleFont);

	// draw the title centered at the bottom of the bar graph
	i = titleFontMetrics.stringWidth(title);
	g.drawString(title, Math.max((size().width - i)/2, 0),
		     size().height - 10 - titleFontMetrics.getDescent()*2);
	// draw the labels
	//i = titleFontMetrics.stringWidth(label1);	
	//g.drawString(label1, Math.max((size().width - i)/2, 0), 6*titleFont.getSize());
	//i = titleFontMetrics.stringWidth(label2);	
	//g.drawString(label2, Math.max((size().width - i)/2, 0), 8*titleFont.getSize());

	g.setFont(valueFont);
	columns = nhosts;
	for (i=0; i < columns; i++) {
		// set the next X coordinate to account for the label
		// being wider than the bar size().width.
		cx = (Math.max((barWidth+barSpacing), maxLabelWidth) * i) +
		    barSpacing;

		// center the bar chart
		//cx += Math.max((size().width - (columns *
		//			 (barWidth + (1 * barSpacing))))/2, 0);
		
		// set the next Y coordinate to account for the size().height
		// of the bar as well as the title and labels painted
		// at the bottom of the chart.
		cy = size().height - (value) - 40; // - (2 * titleFont.getSize());

		// draw the label
		g.setColor(Color.black);		
//		g.drawString((String)labels[i], cx,
//		 size().height - titleFont.getSize() - titleFontMetrics.getDescent());

		// draw the bar with the specified colors
		percentuser = ProcessorCPU[i];
		percentsys  = (Math.random()*0.10);
		percentidle = 1.0-percentsys-percentuser;

		g.setColor(Color.red);			// Percent IDLE time
		barHeight = (int)((double)(value) * percentidle);
		g.fill3DRect(cx, cy, barWidth, barHeight, true);

		g.setColor(Color.yellow);		// Percent SYSTEM time
		cy = cy + barHeight;
		barHeight = (int)((double)(value) * percentsys);
		g.fill3DRect(cx, cy, barWidth, barHeight, true);

		g.setColor(Color.green);		// Percent CPU time
		cy = cy + barHeight;
		barHeight = (int)((double)(value) * percentuser);
		g.fill3DRect(cx, cy, barWidth, barHeight, true);

		g.setColor(Color.black);                // Label the bar with the % CPU
		cy = cy + barHeight;
		g.drawString(String.valueOf((int)(percentuser*100.0)),
		             cx+2, cy - titleFontMetrics.getDescent() );
     }
     g.setColor(Color.blue);
     cy = size().height - 40 - (int)((double)(value) * average); 
     g.drawLine(barSpacing, cy, cx+barWidth, cy);
  }
 /***************************************************************************/
 /************************* End   BarGraph    Class *************************/
 /***************************************************************************/
}
