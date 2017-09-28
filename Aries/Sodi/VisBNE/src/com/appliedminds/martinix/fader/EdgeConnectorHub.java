package com.appliedminds.martinix.fader;

import javax.swing.SwingConstants;
import java.awt.geom.Point2D;

public class EdgeConnectorHub {

  /** predefined NORTH connection hub **/
  public static final EdgeConnectorHub NORTH =
    new EdgeConnectorHub(null, SwingConstants.NORTH);

  /** predefined WEST connection hub **/
  public static final EdgeConnectorHub WEST =
    new EdgeConnectorHub(null, SwingConstants.WEST);

  /** predefined SOUTH connection hub **/
  public static final EdgeConnectorHub SOUTH =
    new EdgeConnectorHub(null, SwingConstants.SOUTH);

  /** predefined EAST connection hub **/
  public static final EdgeConnectorHub EAST =
    new EdgeConnectorHub(null, SwingConstants.EAST);


  private Point2D _point = new Point2D.Double();
  private int _connection = -1;


  /**
   * An EdgeConnectorHub's connection is guaranteed to be one of
   * the SwingConstants, NORTH, WEST, SOUTH or EAST.
   *
   * @param connection can be one of the SwingConstants, NORTH,
   * WEST, SOUTH or EAST.
   * @throws RuntimeException if invalid connection point.
   */
  public EdgeConnectorHub(Point2D point, int connection) {
    setPoint(_point);
    setConnection(connection);
  }
  public EdgeConnectorHub(double x, double y, int connection) {
    setPoint(x, y);
    setConnection(connection);
  }

  public boolean isNorth() {
    return (SwingConstants.NORTH == _connection);
  }


  public boolean isWest() {
    return (SwingConstants.WEST == _connection);
  }


  public boolean isSouth() {
    return (SwingConstants.SOUTH == _connection);
  }


  public boolean isEast() {
    return (SwingConstants.EAST == _connection);
  }


  public int getConnection() {
    return (_connection);
  }


  public void setConnection(int connection) {
    checkConnection(connection);
    _connection = connection;
  }

  public Point2D getPoint() {
    return (_point);
  }

  public void setPoint(Point2D point) {
    if (point != null) {
      _point.setLocation(point.getX(), point.getY());
    }
    else {
      _point.setLocation(0, 0);
    }
  }

  public void setPoint(double x, double y) {
    _point.setLocation(x, y);
  }


  public static void checkConnection(int connection) {
    switch(connection) {
    case SwingConstants.NORTH:
      return;
    case SwingConstants.WEST:
      return;
    case SwingConstants.SOUTH:
      return;
    case SwingConstants.EAST:
      return;
    }
    throw (new RuntimeException("Invalid connection: " +
                                connection));
  }

} // end class "EdgeConnectorHub"
