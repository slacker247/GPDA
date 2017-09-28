package browser.util;

import browser.Browser;
import java.util.jar.*;
import java.net.URL;
import java.net.URLClassLoader;
import java.net.JarURLConnection;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.InvocationTargetException;
import java.util.jar.Attributes;
import java.io.IOException;
import browser.model.*;
import java.io.*;

/**
 * A class loader for loading jar files, both local and remote.
 */
public class JarClassLoader extends URLClassLoader {
    private URL url;

    /**
     * Creates a new JarClassLoader for the specified url.
     *
     * @param url the url of the jar file
     */
    public JarClassLoader(URL url) {
	super(new URL[] { url });
	this.url = url;
    }

    /**
     * Returns the name of the jar file main class, or null if
     * no "Main-Class" manifest attributes was defined.
     */
    public String getMainClassName() throws IOException {
	URL u = new URL("jar", "", url + "!/");
	JarURLConnection uc = (JarURLConnection)u.openConnection();
	Attributes attr = uc.getMainAttributes();
	return attr != null ? attr.getValue(Attributes.Name.MAIN_CLASS) : null;
    }

    /**
     * Invokes the application in this jar file given the name of the
     * main class and an array of arguments. The class must define a
     * static method "main" which takes an array of String arguemtns
     * and is of return type "void".
     *
     * @param name the name of the main class
     * @param args the arguments for the application
     * @exception ClassNotFoundException if the specified class could not
     *            be found
     * @exception NoSuchMethodException if the specified class does not
     *            contain a "main" method
     * @exception InvocationTargetException if the application raised an
     *            exception
     */
    public void invokeClass(String name, String[] args)
	throws ClassNotFoundException,
	       NoSuchMethodException,
	       InvocationTargetException
    {
	Class c = loadClass(name);
        try {
          Object o = c.newInstance();

          Method[] ms = c.getMethods();
          for(int i = 0; i < ms.length; i++) {

            if(ms[i].getName().equals("test")) {
              ms[i].invoke(o, new Object[]{});
            }
          }
        } catch (Exception e) {
          e.printStackTrace();
        }

	//Method m = c.getMethod("test", new Class[] { args.getClass() });
	//m.setAccessible(true);
	//int mods = m.getModifiers();
	//if (m.getReturnType() != void.class || !Modifier.isStatic(mods) ||
	  //  !Modifier.isPublic(mods)) {
	    //throw new NoSuchMethodException("test");
	//}
	//try {
	  //  m.invoke(null, new Object[] { args });
	//} catch (IllegalAccessException e) {
          //  System.out.println("Illegal access");
	    // This should not happen, as we have disabled access checks
	//}
    }


    public Object getNewLibraryItem(String name)
	throws ClassNotFoundException,
	       NoSuchMethodException,
	       InvocationTargetException
    {
      try {
        Class c = loadClass(name, true);

        Object o = c.newInstance();

        return o;
      } catch (Exception e) {
        e.printStackTrace();

      }
      return null;
    }

    public static File[] getJarsInDirectory(String pathname) {
      if(pathname == null) {
        return new File[]{};
      }
      File dir = new File(pathname);
      if(!dir.isDirectory()) {
        return null;
      }
      File jar_files[] = dir.listFiles(new FileFilter()  {
            public boolean accept(File pathname) {
                return pathname.getName().endsWith(".jar");
            }
      });
      return jar_files;
    }

    public static String getItemTypeName(File jarFile) {
      try {
        JarFile jar = new JarFile(jarFile);
        Manifest manifest = jar.getManifest();
        Attributes att = manifest.getMainAttributes();
        String className = att.getValue(Browser.LIBRARY_ITEM_ENTRY_PROPERTY);
        return className;
      } catch (IOException ioe) {
        ioe.printStackTrace();
      }
      return null;
    }

}

