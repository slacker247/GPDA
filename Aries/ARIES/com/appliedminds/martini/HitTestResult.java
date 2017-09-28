package com.appliedminds.martini;




/**
 * The result of a hit test.  This keeps track of the two boolean
 * conditions that could result from a hit test which combine to make
 * just three potential states:
 *
 * <ul>
 * <li>The hit test succeeded.
 * <li>The hit test succeeded and the hit was consumed by some listener.
 * <li>The hit test failed.
 * </ul>
 *
 *
 * @author mathias@apmindsf.com
 */
public class HitTestResult {


  private HitTester _tester;
  private boolean _hit;
  private boolean _consumed;


  /** 
   * Factory method creates a new HitTestResult that indicates that
   * the hit was successful and it was consumed.
   *
   * @param ht the hit tester object.
   */
  public static HitTestResult hitTestConsumed(HitTester ht) {
    if (ht == null) { 
      throw new RuntimeException("Assert Failed: HitTestResult.hitTestConsumed requires a non null argument.");
    }
    return(new HitTestResult(ht, true, true));
  }


  /** 
   * Factory method creates a new HitTestResult that indicates that
   * the hit was successful but was not consumed.
   *
   * @param ht the hit tester object.
   */
  public static HitTestResult hitTestHitNotConsumed(HitTester ht) {
    if (ht == null) { 
      throw new RuntimeException("Assert Failed: HitTestResult.hitTestHitNotConsumed requires a non null argument.");
    }
    return(new HitTestResult(ht, true, false));
  }


  /** 
   * Factory method creates a new HitTestResult that indicates that
   * the hit was not successful.
   *
   * @param ht the hit tester object.
   */
  public static HitTestResult hitTestMissed() {
    return(new HitTestResult(null, false, false));
  }



  /*
   * Keep this private so that users don't get all confused about the
   * param value restrictions.  Main things is that if consumed is
   * true then hit MUST be true also.
   */
  private HitTestResult(HitTester ht, boolean hit, boolean consumed) {
    _tester = ht;
    _hit = hit;
    _consumed = consumed;
  }



  /** 
   * See if the hit test resulted in a hit on a UI decoration.
   */
  public boolean wasHit() {
    return(_hit);
  }


  /** 
   * See if the hit was consumed by a listener.
   */
  public boolean wasConsumed() {
    return(_consumed);
  }


  /**
   * Get the hit tester object associated with the hit.  This will
   * only be valid if wasHit() is true.
   *
   * @return the hit tester object, or null if there was no hit.
   */
  public HitTester getHitTester() {
    return(_tester);
  }


  public String toString() {
    return("HitTestResult{ HIT=" + _hit + ", CONSUMED=" + _consumed + " }");
  }

}
