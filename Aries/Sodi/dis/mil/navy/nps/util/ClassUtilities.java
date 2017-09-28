/*
 File:		ClassUtilities.java
 CVS Info:	$Id: ClassUtilities.java,v 1.1.1.1 1998/01/27 22:48:51 mcgredo Exp $
 Compiler:	jdk1.3
 */


/**
   A class that does some useful stuff on determing class
   membership. This is intended to be similar to the Smalltalk
   utilities such as isKindOf, isMemberOf, and so on.

  If this were a just world, this would be added as an Objective-C category 
  at the Object level.

  @author Don McGregor (http://www.npsnet.org/~mcgredo)

*/

package mil.navy.nps.util;

public class ClassUtilities extends Object
{

/**
  * For autonumbering names. This returns a monotonicially increasing, unique
  * number each time it is called, for the duration of the class. Note that
  * we might have some problems with the class being unloaded during runtime,
  * which would upset the count.
  */
 private static int serialNumber;

 public static synchronized int nextSerialNum() 
 { return serialNumber++;
 }

 /**
  * This displays the names of all the active threads in the current
  * threadgroup. It seems that, by default, all the threads in an
  * applet/application are gathered together into one threadgroup;
  * this displays them all.
  *
  * The activeCount may be off by a bit; bad the array below by a few
  * entries so we don't get an arrayOutOfBounds error.
  */

 public static void showActiveThreads()
 {
   Thread    threadList[] = new Thread[Thread.currentThread().activeCount() + 5]; // Fudge factor
   System.out.println("active threads in threadgroup number " + Thread.currentThread().enumerate(threadList));

   for(int idx=0; idx < threadList.length; idx++)
   {
     System.out.println("Thread name: " + threadList[idx].getName());
   }
 }

  /**
    Returns true if the object passed in is a subclass of the class passed
    in. Returns false if the object is exactly the same class as the object
    passed in, or if the object is part of a different class hierarchy.
  */
public static boolean objectIsSubclassOfClass(Object pObject, Class pClass)
{
  Class currentClass = pClass;

  while(currentClass != null)
  {
    currentClass = currentClass.getSuperclass();
    if(currentClass == pObject.getClass())
      return true;
  }

  // fall through; didn't find it anywhere in the inheritance 
  // hierarchy, so it must not be a subclass.
  return false;
}

/**
  Returns true if the object passed in is an instance of the class
  passed in. The object must EXACTLY be the same class as the
  class passed in.
*/

public static boolean objectIsMemberOfClass(Object pObject, Class pClass)
{
  if(pObject.getClass() == pClass)
    return true;

  return false;
}

/**
 Return  true  if class of object, or any superclass thereof,
 is the same as the class argument.
*/

public static boolean objectIsKindOfClass(Object pObject, Class pClass)
{
  // removed static reference ClassUtilities.objectIsMemberOfClass,
  // now simply an unreferenced call to objectIsMemberOfClass
  if(objectIsMemberOfClass(pObject, pClass))
    return true;
  if(objectIsSubclassOfClass(pObject, pClass))
    return true;

  return false;
}

}
