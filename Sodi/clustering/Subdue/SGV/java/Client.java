import java.io.*;
import java.net.*;
import java.awt.*;
import java.util.*;
import java.applet.*;
import java.lang.*;

public class Client extends Frame {
   
  private TextArea display;
  boolean conn_open = true;
  public static MainWindow mainWindow;
  private ReadFile gphfl;
  private SubWindow subWindow;
  public static Vector subsList;	 // a vector which contains all the substructures 
					 // discovered by Subdue and sent over the socket
  public static Vector value;

  public static boolean endNow;
  public Client() {

    super("Client");
    display = new TextArea("",0,0,TextArea.SCROLLBARS_BOTH);
    add(display,BorderLayout.CENTER);
    setSize(300,150);
    setVisible(true);
  }

  public void runClient()
  {
        Socket client;
        PrintWriter output;       // for socket output
        BufferedReader input;     // for socket input
        GsComm subs;
        String filename ;         // the input filename
        String buffer;      
        Vector datain ;           // one instance to display in the SubWindow
        Vector instance;          // each instance
        LinkedList instanceList;  // all the instances of one substructure
                                  // to be highlighted in the MainWindow
        String outbuf;
        int subcount = 0;
        String hicolor;


        try {
          client  = new Socket(InetAddress.getLocalHost(),5872);   // port no is hard coded 

          display.append("Connected to " + client.getInetAddress().getHostName());

          input = new BufferedReader(new InputStreamReader(client.getInputStream()));
          output = new PrintWriter(client.getOutputStream(),true);
          display.append("\nGot I/O Streams\n");
 
          /* Read the input graph filename over the socket from the server */
          
          filename =  input.readLine();

          if(filename == null)
          {
             System.out.println("Input file does not exist\n");
             client.close();
             System.exit(1);   
          }
          outbuf = "k\n";
          output.write(outbuf);

          display.append("filename is " + filename + "\n");

          gphfl = new ReadFile(filename);
	  
          if(gphfl == null)
          {
            System.out.println("Unable to parse file " + filename);
            client.close();
            System.exit(1);
          }
  
          value = new Vector();

          mainWindow = new MainWindow(gphfl.curGraph);        
          mainWindow.addWindowListener(new CloseWindowAndExit());
          
	  subsList = new Vector();
          subWindow = new SubWindow(gphfl.curGraph,subsList,-1);
          subWindow.addWindowListener(new CloseWindowAndExit());
          endNow = false;

          while(true)
          {    
                  
            buffer = input.readLine();     // has the compression value
            if(buffer == null)
                continue;
            display.append(buffer+"\n");
            Double v = new Double(buffer);
            double val = v.doubleValue();
            int pos = findPositionInList(val);
//            value.add(pos,v);
            value.insertElementAt(v,pos);
            buffer = input.readLine(); // has the substructure instances
            display.append(buffer+"\n");
            StringTokenizer st = new StringTokenizer(buffer);

            instanceList = new LinkedList();
            instance = new Vector();

            while(st.hasMoreTokens())
            {
                 instance = new Vector();
 		 loadSubs(st,instance);
                 if(instance != null)
		 {
                   //System.out.println("client : adding instance ");
                   instanceList.add(instance);
		 }
              
            }
            outbuf = "ok";
            output.write(outbuf);
//            subsList.add(pos,instanceList);
            subsList.insertElementAt(instanceList,pos);                 
            if(endNow)
              break;
  

            datain = (Vector)instanceList.getLast();
//            double val =(Double.valueOf(value.lastElement().toString())).doubleValue();
            
            subWindow.subsChoice.add("Substructure " + (subcount+1));
            subWindow.refresh(gphfl.curGraph,subsList,subcount);

            hicolor = "red";
            mainWindow.refresh(subsList,subsList.size()-1,hicolor);  
            subcount++;
             
          }

          display.append("\nTransmission complete "+ "closing connection\n\n");
          client.close();
        }  
    
        catch(IOException e) {
           e.printStackTrace();
        }
        catch(NullPointerException np) {
           np.printStackTrace();
        }
        catch(NoSuchElementException nse) {
           nse.printStackTrace();
        }
  }


  public void loadSubs(StringTokenizer st,Vector instance)
  {
              while(st.hasMoreTokens())
              {
	              GsComm subs = new GsComm();

	              subs.typ = st.nextToken().charAt(0);
                      if(subs.typ == '$')
			return;
                      if(subs.typ == '%')
                      {
			endNow = true;
                        return;
                      }			
        	      Integer toconv = Integer.valueOf(st.nextToken());

	              subs.vert1 = toconv.intValue();
        	      if(subs.typ == 'e' || subs.typ == 'd' || subs.typ == 'u')
              	      {
	                toconv = Integer.valueOf(st.nextToken());
        	        subs.vert2 = toconv.intValue();
	              }
        	      subs.label = st.nextToken();          
		
	              instance.addElement(subs);     
		}
		return;        
  }

  public Vector getSubsList()
  {
    return subsList;
  }

  public static void highLight(int subToDraw)
  {

     String hicolor = "red";
     Client.mainWindow.refresh(Client.subsList,subToDraw,hicolor);  
    
  }
  public int findPositionInList(double v)
  {
    int n = value.size();
    if(n==0)
      return 0;
    int i;

    for(i=0;i<n;i++)
    {
      if(v > ((Double)value.elementAt(i)).doubleValue() )
          return i;
    }
    return i;
  }

  public static void main(String args[])
  {
    Client c = new Client();
    c.addWindowListener(new CloseWindowAndExit());
    c.runClient();

  }



}

