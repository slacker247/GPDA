package com.appliedminds.hmv;

import java.util.Hashtable;


/**
 * <b>SupportHandlerManager</b> can be used to locate a handler for
 * any given support type name. This support handler must support the
 * com.appliedminds.hmv.SupportHandler interface for handling a given
 * URI.
 *
 * <p>The SupportHandlerManager locates a handler for a given support
 * type by providing a registerSupportHandler method to allow a
 * handler to be specifically registered for a given support type.
 *
 * @author daepark@apmindsf.com
 */
public class SupportHandlerManager {

  private static Hashtable _registry;

  static {
    _registry = new Hashtable();
  }


  /**
   * Register a handler to be used to handle the the given support type.
   *
   * @param supportType The support type associated with the given
   * Class object.
   * @param supportHandlerClass The Class object of the handler
   * class. If this is null, then any existing definition will be
   * removed.
   */
  public static void registerSupportHandler(String supportType,
                                            Class supportHandlerClass)
  {
    if (supportHandlerClass == null) {
      Class c = (Class)_registry.remove(supportType);
    }
    else {
      _registry.put(supportType, supportHandlerClass);
    }
  }


  /**
   * Locate a handler for a given support type.
   *
   * @param supportType The support type to be handled.
   * @return A handler object for the given support type. The result
   * is null if no suitable handler can be found.
   */
  public static synchronized SupportHandler findSupportHandler(String supportType)
  {
    Class supportHandlerClass = (Class)_registry.get(supportType);
    if (supportHandlerClass != null) {
      try {
        Object o = supportHandlerClass.newInstance();
        return ((SupportHandler)o);
      }
      catch (Exception e) {
        System.err.println("Couldn't instantiate support type handler \"" +
                           supportHandlerClass.getName() + "\" : " + e);
        e.printStackTrace();
      }
    }

    // We couldn't find a suitable SupportHandler
    return (null);
  }

} // end class "SupportHandlerManager"
