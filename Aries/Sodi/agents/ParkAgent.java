
import java.util.Vector;
import java.io.*;
import com.objectspace.voyager.*;
import com.objectspace.voyager.agent.*;


public class ParkAgent extends Agent
  {
  Vector itinerary = new Vector(); 	        // Vector of nodes to visit
  int index; 				// index of current application
  int myid;
  String homeaddress = "localhost";
  int pollingrate;
  int lowerbound;
  int upperbound;
  int balance;

  public void addToItinerary( String address, int id )
  {
    myid = id;
    itinerary.addElement( address );
  }

  public void SetPolling( int rate ) { pollingrate = rate; }

  public void SetUpper( int rate ) { upperbound = rate; }

  public void SetLower( int rate ) { lowerbound = rate; }

  public void SetLoad( int rate ) { balance = rate; }

  public synchronized int deposit( int amount )
  {
      if( amount < 0 )
        throw new IllegalArgumentException( "Cannot deposit " + amount );
      balance += amount;
      if (balance > 90) balance = 90;
      return balance;
  }

  public synchronized int withdraw( int amount ) 
      throws Exception
  {
      balance -= amount;
      if (balance < 0) balance = 0;
      if( balance < 0 )
	throw new Exception( "Cannot have negative balance " + balance );
      return balance;
  }

  public synchronized double getBalance()
  {
      return ((double)balance/100.0);
  }

  public void launch()
  {
      String destination = (String) itinerary.elementAt( index++ );      
      try
        {
	//homeaddress = Agent.of( this ).getHome() ;		// Save Home address
        Agent.of( this ).moveTo( destination, "atProgram" );
        }
      catch( Exception exception ) { System.out.println(exception); }
  }

  public void atProgram()
  {
      //System.out.println( "Parking at " + Agent.of( this ).getAddress() + " from " + homeaddress );

//	  VParkAgent agenty = (VParkAgent) VObject.forObjectAt(updated_agent_address);    
//	  VLaunch home = (VLaunch) VObject.forObjectAt(homeaddress);
	  //String msg = "Agent has arrived at " + Agent.of( this ).getAddress();
//	  home.putMessage(msg);
   
    try
	{
	  Thread.sleep( 3000 );
	}
    catch( InterruptedException exception ) {}   
  }

  public void dismiss() throws Exception
  {
//    System.out.println( "Dismiss " + Agent.of( this ).getAddress() );
//    dieNow(); // kill myself and all my forwarders
//    System.exit(0);
  }
}
