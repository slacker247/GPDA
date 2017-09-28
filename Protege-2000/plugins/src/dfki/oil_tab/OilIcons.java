package dfki.protege.oil_tab;

import java.awt.*;
import java.io.*;
import java.net.*;
import java.util.*;
import javax.swing.*;
import javax.swing.*;
import edu.stanford.smi.protege.util.*;


/** Adapted from Protege's Icons and ComponentUtilities classes */
public class OilIcons {

    static Map theOilIcons = new HashMap();


    static Icon getViewSHIQIcon() {
        return getImageIcon("ViewSHIQ");
    }

    static Icon getViewXMLIcon() {
        return getImageIcon("ViewXML");
    }


    static ImageIcon getImageIcon(String name) {
        ImageIcon icon = (ImageIcon) theOilIcons.get(name);
        if (icon == null || icon.getIconWidth() == -1) {
            icon = loadImageIcon(OilIcons.class, "image/" + name + ".gif");
            if (icon == null && !name.equals("Ugly")) {
                icon = getImageIcon("Ugly");
            }
            theOilIcons.put(name, icon);
        }
        return icon;
    }

    static ImageIcon loadImageIcon(Class cls, String name) {
        ImageIcon icon = null;
        URL url = cls.getResource(name);
        if (url != null) {
            icon = new ImageIcon(url);
            if (icon.getIconWidth() == -1) {
                Log.error("failed to load", 
		  OilIcons.class, "loadImageIcon", cls, name);
            }
        }
        return icon;
    }

}


