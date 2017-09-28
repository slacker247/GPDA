package dfki.protege.ontoviz_tab;

import java.awt.*;
import java.io.*;
import java.net.*;
import java.util.*;
import javax.swing.*;
import javax.swing.*;
import edu.stanford.smi.protege.util.*;


/** Adapted from Protege's Icons and ComponentUtilities classes */
public class OntovizIcons {

    static Map theOntovizIcons = new HashMap();


    static Icon getAddClsIcon() {
        return getImageIcon("AddCls");
    }

    static Icon getAddInstanceIcon() {
        return getImageIcon("AddInstance");
    }

    static Icon getOptionsIcon() {
        return getImageIcon("Options");
    }

    static Icon getConfigureSlotsIcon() {
        return getImageIcon("ConfigureSlots");
    }


    static ImageIcon getImageIcon(String name) {
        ImageIcon icon = (ImageIcon) theOntovizIcons.get(name);
        if (icon == null || icon.getIconWidth() == -1) {
            icon = loadImageIcon(OntovizIcons.class, "image/" + name + ".gif");
            if (icon == null && !name.equals("Ugly")) {
                icon = getImageIcon("Ugly");
            }
            theOntovizIcons.put(name, icon);
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
		  OntovizIcons.class, "loadImageIcon", cls, name);
            }
        }
        return icon;
    }

}


