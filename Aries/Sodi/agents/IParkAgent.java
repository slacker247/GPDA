/**
 * IParkAgent.java
 * <p>
 * @version 1.0
 * @author generated by igen 3.3 at Wed Nov 22 15:57:55 MST 2000
 */

public interface IParkAgent
  {
  void SetLoad( int arg1 );
  void SetLower( int arg1 );
  void SetPolling( int arg1 );
  void SetUpper( int arg1 );
  void addToItinerary( java.lang.String arg1, int arg2 );
  void atProgram();
  int deposit( int arg1 );
  void dismiss() throws java.lang.Exception;
  double getBalance();
  void launch();
  int withdraw( int arg1 ) throws java.lang.Exception;
  }
