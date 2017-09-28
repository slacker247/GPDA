
import java.util.Vector;
import java.io.*;
import com.objectspace.voyager.*;
import com.objectspace.voyager.agent.*;


public class RoamAgent extends Agent
  {
  Vector itinerary = new Vector(); 	        // Vector of nodes to visit
  static int index = 0; 			// Index of current node
  int myid = 1, nNodes = 5;
  String homeaddress = "localhost:8001/HOMEAGENT1";
  String nowaddress  = "localhost";

  public void addToItinerary( String address, int id )
    {
      myid = id;
      itinerary.addElement( address );
      System.out.println( "We will visit "+address+" at index "+itinerary.size());
    }

  public void launch()
    {
      String destination = "localhost";
     
      try
        {
	  homeaddress = Agent.of( this ).getHome() ;	 // Save Home address
	  destination = (String) itinerary.elementAt( index );
	  System.out.println( "Starting: from "+homeaddress+" to "+destination ); 
          Agent.of( this ).moveTo( destination, "atProgram" );
        }
      catch( Exception exception ) { System.out.println(exception); }
    }

  public void atProgram()
    {
      String destination = "localhost";

      //System.out.println( "Arrived at "+Voyager.getAddress()+" index is "+index );
      System.out.println(getUptime() );
      /*      
      try
	{
          //VParkAgent agenty = (VParkAgent)VObject.forObjectAt(updated_agent_address);  
          VHomeAgent home = (VHomeAgent)VObject.forObjectAt(homeaddress);
          String msg = "Agent is at " + nowaddress;
          home.putMessage(msg);
	}
      catch( Exception exception ) { System.out.println(exception); }
      */      
      //System.out.println( "Sleep for awhile " + Voyager.getAddress() );
      try
	{
	  Thread.sleep( 8000 );
	}
      catch( InterruptedException exception ) { System.out.println(exception); }
      /*
      index = index + 1;
      if (index > itinerary.size()) index = 0;
      destination = (String)itinerary.elementAt(index);
      System.out.println( "Try moving to "+destination+" index "+index ); 
      try
        {
	  Agent.of( this ).moveTo(destination, "atProgram");
        }
      catch( Exception exception ) { System.out.println(exception); }
      */
    }

  public String getUptime()
    {
      String         uptime_str = "initial";
      String         saved_line = "initial";
      BufferedReader uptime_in;

      try
	{
	  Process ls_proc = Runtime.getRuntime().exec("uptime");
	  uptime_in = new BufferedReader(new InputStreamReader(ls_proc.getInputStream()));
	  while ((uptime_str = uptime_in.readLine()) != null) {
//	    System.out.println(uptime_str);
	    //saved_line = "Uptime: " + Voyager.getAddress() + uptime_str;
	  }
        }
      catch (IOException e) { System.out.println(e); }
      return saved_line;
    }

  public void dismiss()
    {
      //System.out.println( "Dismiss " + Voyager.getAddress() );
      //dieNow();                             // kill myself and all my forwarders
      //System.exit(0);
    }
  }
