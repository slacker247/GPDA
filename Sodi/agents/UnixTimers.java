//
//	This Java class provides an interface to the Unix "times" function
//	to get smaller granularity times. It provides five values:
//		Elapsed wall time
//		Process CPU user time
//		Process System time
//		Child CPU user time
//		Child System time
//
//	It may be used in either of two ways:
//
//	1) Make one Unix system call to get all five times and use any of the
//	   five values returned as globally readable variables.
//
//			UnixTimers t = new UnixTimers();
//			t.getTimes();
//			double elapsed = t.elapsed;
//			double system  = t.pstime;
//			double usrcpu  = t.putime;
//			double childu  = t.cutime;
//			double childs  = t.cstime;
//
//	2) Make a Unix system call to get the requested time.
//
//			UnixTimers t = new UnixTimers();
//			double elapsed = t.Getelapsed();
//                      double system  = t.Getpstime();
//                      double usrcpu  = t.Getputime();
//                      double childu  = t.Getcutime();
//                      double childs  = t.Getcstime();
//
import java.io.*;

public class UnixTimers {
  public double  putime = 0.0;
  public double  pstime = 0.0;
  public double  cutime = 0.0;
  public double  cstime = 0.0;
  public double  elapsed = 0.0;

  public UnixTimers() {
    getTimes();
  }

  public double Getputime() {
    getTimes();
    return (double)putime;
  }

  public double Getpstime() {
    getTimes();
    return (double)pstime;
  }

  public double Getcutime() {
    getTimes();
    return (double)cutime;
  }

  public double Getcstime() {
    getTimes();
    return (double)cstime;
  }

  public double Getelapsed() {
    getTimes();
    return (double)elapsed;
  }

  public native void getTimes();

  static { System.loadLibrary("timers"); }

}
