/*
 * The contents of this file are subject to the Mozilla Public License
 * Version 1.1 (the "License");  you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * The Original Code is Protege-2000.
 *
 * The Initial Developer of the Original Code is Stanford University. Portions
 * created by Stanford University are Copyright (C) 2001.  All Rights Reserved.
 *
 * Protege-2000 was developed by Stanford Medical Informatics
 * (http://www.smi.stanford.edu) at the Stanford University School of Medicine
 * with support from the National Library of Medicine, the National Science
 * Foundation, and the Defense Advanced Research Projects Agency.  Current
 * information about Protege can be obtained at http://protege.stanford.edu
 *
 * Contributor(s):
 */

package edu.stanford.smi.RemoteKBTab.toolbox;

import edu.stanford.smi.RemoteKBTab.*;
import javax.swing.tree.*;
import java.awt.*;
import javax.swing.*;
import edu.stanford.smi.protege.model.*;

/** An implementation of a JTree component.  It displays the search term as
 *  the root of the tree and the search results as the children of that root.
 *  The tree only has the two levels, the root and its children. To use this
 *  class, simply override it and create a new search function. */
public abstract class TreeRelationDisplay extends AbstractRelationDisplay {

    protected JTree itsTree;
    protected JPanel itsComp;
    protected DefaultTreeModel itsModel;
    protected String lastSearch;

    public TreeRelationDisplay() {
        createComponents(null, null);
    }

    public TreeRelationDisplay(String label, String slot,
                               AbstractRemoteKBAction[] actions) {
        setSlotName(slot);
        Helper.setRelationsDisplay(actions, this);
        createComponents(label, actions);
        this.label = label;
    }

    /** Create one tree. */
    private JTree createTree() {
        DefaultMutableTreeNode newroot =
        new DefaultMutableTreeNode("root");
        itsModel = new DefaultTreeModel(newroot);
        JTree tree = new JTree(itsModel);
        tree.setCellRenderer(new TreeStringRenderer());
        return tree;
    }

    /** Create components in this relation display. */
    private void createComponents(String label,
                                  AbstractRemoteKBAction[] actions) {
        itsTree = createTree();
        JScrollPane scroll = new JScrollPane(itsTree);
        itsTree.putClientProperty("JTree.lineStyle", "Angled");
        itsComp = new JPanel();
        if(getNorthComponent() != null) {
            itsComp.add(getNorthComponent());
        }
        Helper.createLabeledComponentInPanel(label, scroll, actions, itsComp);
    }

    /** Return the relation display component. */
    public JComponent getComponent() {
        return itsComp;
    }

    /** Return the embedded widget in the relation display, the JTree. */
    public JComponent getWidget() {
        return itsTree;
    }

    /** Set the specified string array to the tree. */
    public void setData(String[] data) {
        DefaultMutableTreeNode newroot = new DefaultMutableTreeNode(data[0]);
	if (data!= null) {
            for (int i = 1; i< data.length;i++) {
                newroot.add(new DefaultMutableTreeNode(data[i]));
            }
	}
	itsModel.setRoot(newroot);
    }

    /** Set the last Search string. */
    public void setLastSearch(String str) {
        lastSearch = str;
    }

    /** Get the data from tree and return it as a string array. */
    public String[] getData() {
        return null;
    }

    /** Get the selected leaves in the tree and return them as a string
     *  array. */
    public String[] getSelectedItems() {
        return getSelectStrs();
    }

    /** Get the selected leaves in the tree and return them as a string array. */
    private String[] getSelectStrs() {
        String[] selectNodeStrs;
        DefaultMutableTreeNode selectNodes[];
        DefaultMutableTreeNode selectNode;

        if (itsTree.getSelectionCount()>0 ) {
            TreePath[] paths = new TreePath[itsTree.getSelectionCount()];
            selectNodes = new DefaultMutableTreeNode[paths.length];
            paths = itsTree.getSelectionPaths();
            selectNodeStrs = new String[paths.length];
            for (int j = 0; j<paths.length; j++) {
                selectNodeStrs[j] = new String();
                Object elements[] = paths[j].getPath();
                selectNodeStrs[j] = elements[elements.length - 1].toString();
                selectNode = (DefaultMutableTreeNode)paths[j].getPathComponent(elements.length - 1);
                selectNodes[j] = selectNode;
            }
        } else {
            selectNodeStrs = new String[0];
        }
        return selectNodeStrs;
    }

    /** Create one string array include all leaves in the tree. */
    protected String[] createArray(String str, String[] strArray) {
        if( str != null) {
            String[] newArray = new String[ strArray.length +1];
            newArray[0] = new String(str);
            for (int i=0; i<strArray.length;i++) {
                newArray[i+1] = new String(strArray[i]);
            }
            return newArray;
        } else {
            return strArray;
        }
    }

    /** Specified the display of the tree and associate action. */
    class TreeStringRenderer extends JLabel implements TreeCellRenderer {
        TreeStringRenderer () {
            super.setHorizontalAlignment(SwingConstants.LEFT);
	    setOpaque(true);
            setPreferredSize(new Dimension(400, 15));
	}

	public Component getTreeCellRendererComponent(JTree tree, Object value,
                                                      boolean selected, boolean expanded,
                                                      boolean leaf, int row, boolean hasFocus) {
            StringBuffer text = new StringBuffer();
	    text.append (row + "->");
            text.append (value.toString());
            setBackground(selected ? Color.lightGray : new Color(16777215));
	    setForeground(new Color(0));
	    setText(text.toString());
	    return this;
        }
    }

    /** Set the slot not a single value slot. */
    public boolean isSlotSingleValued(){
        return false;
    }
}