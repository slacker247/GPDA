import com.objectspace.voyager.*;
import com.objectspace.voyager.agent.*;
import java.io.*;

public class HomeAgent extends Agent {

  public void putMessage(String msg)  
  {                                      
     System.out.println(msg);            
  }

 public void arrived()
 {
   System.out.println("Home agent 1 has arrived");
 }

  public void dismiss()
    {
      //System.out.println( "Dismiss " + Voyager.getAddress() );
      //dieNow();                             // kill myself and all my forwarders
      //System.exit(0);
    }

}
