import java.applet.Applet;
import java.awt.*;
import java.io.*;

public class TCPServerApplet extends Applet {
  StatsReader serv = null;

  public void init() {
    int port = 1039;

    try {
      serv = new StatsReader (port);
    }
    catch (IOException e) { System.err.println("Starting server: " + e); return; }

    serv.start();

    try {
      serv.join();
    }
    catch (InterruptedException e) { System.err.println("Interrupted: " + e); }
    serv = null;
  }

  public static void main(String args[]) {
    
    (new TCPServerApplet() ).init();
    System.exit(0);
  }

}


