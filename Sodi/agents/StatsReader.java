import java.io.*;
import java.net.*;

public class StatsReader implements Runnable {
  Thread       serverThread = null;
  ServerSocket s = null;
  int          port = 1039;

  public StatsReader(int p) throws IOException {
    port = p;

    try {
      s = new ServerSocket(port);
    }
    catch( IOException e ) { System.err.println("Socket allocation error: " + e); throw e; }
  }

  public synchronized void start() {
    if(serverThread == null) {
      serverThread = new Thread(this);
      serverThread.setPriority(Thread.MAX_PRIORITY/4);
      serverThread.start();
    }
    if(s == null) {
      try {
	s = new ServerSocket(port);
      }
      catch (IOException e) { System.err.println("Socket allocation error: " + e); return; }
    }
  }

  public synchronized void stop() {
    if(serverThread != null) {
      serverThread.stop();
      serverThread = null;
    }
    if(s != null) {
      try {
	s.close();
	s = null;
      }
      catch (IOException e) { System.err.println("Socket close error: " + e); return; }
    }
  }

  public synchronized void join() throws InterruptedException {
    if(serverThread != null) {
      serverThread.join();
    }
    return;
  }

  public void run() {
    InputStream in = null;
    PrintStream out = null;
    Socket      con = null;

    while (serverThread != null) {
      try {
	con = s.accept();
      }
      catch (IOException e) { System.err.println("Socket accept error: " + e); return; }
      System.err.println("Got connection from " + con.getInetAddress() + ":" +
                         con.getPort() );

      try {
	out = new PrintStream(con.getOutputStream() );
	in = con.getInputStream();
      }
      catch (Exception e)  { System.err.println("Stream build error: " + e); }

      try {
	int nbytes;
	boolean done = false;
	byte b[] = new byte[1024];

	while (!done && ((nbytes = in.read(b,0,1024)) != -1) ) {
	  String str = new String(b,0,0,nbytes);
	  //System.err.println("Received:\n" + str);
	  Launch.putMessage("Socket: "+str);
	}
      }
      catch (Exception e)  { System.err.println("Stream read: " + e); }

    }

    try {
      con.close();
    }
    catch (Exception e)  { System.err.println("Stream close error: " + e); }
  }

}
