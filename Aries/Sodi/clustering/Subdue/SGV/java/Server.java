import java.io.*;
import java.net.*;
import java.awt.*;

public class Server extends Frame {
   
  private TextArea display;

  public Server() {
    super("Server");
    display = new TextArea("",0,0,TextArea.SCROLLBARS_VERTICAL_ONLY);
    add(display,BorderLayout.CENTER);
    setSize(300,150);
    setVisible(true);
    display.append("SGV Server loaded.\n");
  }


  public void runServer()
  {
    ServerSocket server;
    Socket connection;
    DataOutputStream output;
    DataInputStream input;
    int counter = 1;

    try {
      server = new ServerSocket(5872,100);
      display.append("SGV waiting for connection...\n");
      while(true) {
        connection = server.accept();
        display.append("Connection " + counter+" received from: " + connection.getInetAddress().getHostName());
        input = new DataInputStream(connection.getInputStream());
        output = new DataOutputStream(connection.getOutputStream());
        display.append("\nGot I/O Streams\n");
        display.append("Sending Message \n");
        output.writeChar('e');
        display.append("Sending Message \n");
        output.writeInt(1);
        display.append("Sending Message \n");
        output.writeInt(11);
        display.append("Sending Message \n");
        output.writeUTF("shape");

        if(input.readInt() == 1)
        {       
         display.append("\nTransmission complete \n"+ "closing socket\n\n");
         connection.close();
         ++counter;
        }
      }
    }
    catch(IOException e) {
       e.printStackTrace();
    }
  }
 
  public static void main(String args[])
  {
    Server s = new Server();
//    s.addWindowListener(new CloseWindowAndExit());
    s.runServer();

  }
}



