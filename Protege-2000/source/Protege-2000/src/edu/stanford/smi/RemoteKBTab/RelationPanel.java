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

/** The RelationPanel is the central panel that appears on the tab, which
	 contains all of the RelationDisplay's.
 */


package edu.stanford.smi.RemoteKBTab;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;


public class RelationPanel extends JPanel {
  RemoteKBTab tab;
  JCheckBoxMenuItem items[];
  String[] widgetTitles;
  JTabbedPane jTabbedPane1;
	JPanel[] innerPanels;

  /** Constructor. Setup the components will be shown in the panel and
      their respective titles. */
	public RelationPanel(JComponent[] comps, String[] titles, RemoteKBTab tab) {
    if ( titles!=null) {
       this.widgetTitles = new String[titles.length];

       for (int i=0;i<titles.length;i++)
         widgetTitles[i] = new String(titles[i]);
    }

    this.tab = tab;
		//JComponent tabbedPane = createTabbedPane(comps, titles);
    JComponent tabbedPane = new JScrollPane(createTabbedPane(comps, titles));
		addPopupMenu(tabbedPane);
    add(tabbedPane);
		setLayout(new BoxLayout(this,BoxLayout.Y_AXIS));
	}

  /** CreateTabbedPane positions each component into one tabbed pane with their
      respective title. */
  private JComponent createTabbedPane(JComponent[] comps, String[] titles) {
     jTabbedPane1 = new JTabbedPane();
     innerPanels = new JPanel[comps.length];

     for(int i=0; i< comps.length; i++){
        innerPanels[i] = new JPanel();
        ((JPanel)comps[i]).setLayout(new BoxLayout(((JPanel)comps[i]),BoxLayout.Y_AXIS));
        innerPanels[i].setLayout(new BoxLayout(innerPanels[i],BoxLayout.Y_AXIS));

        innerPanels[i].add((JComponent)comps[i]);
        innerPanels[i].setPreferredSize(new Dimension(450,300));
        jTabbedPane1.add(titles[i], innerPanels[i]);

     }
     return jTabbedPane1;
  }

  /**  CreateDisplay positions the RelationDisplay's in a 2 by N grid where
   2 * N is the largest whole number greater than or equal to the number of
   RelationDisplay's to show. */
	private JPanel createDisplay(JComponent[] comps){
		JPanel panel = new JPanel();
		if (comps == null) return panel;

		int width = 2, height = 0;
		int length = comps.length;

		height = length / 2 + 1;

		panel.setLayout(new GridLayout(height, width));

		for(int i = 0; i < length; i++){
			panel.add((JComponent)comps[i]);
		}
		return panel;

	}

  /** Add Popup Menu to the panel. */
  private void addPopupMenu(JComponent jc) {

     final JPopupMenu popupMenu = new JPopupMenu();
      ItemHandler handler = new ItemHandler();
      items = new JCheckBoxMenuItem[ widgetTitles.length ];

      // construct each menu item and add to popup menu; also
      // enable event handling for each menu item
      for ( int i = 0; i < items.length; i++ ) {
         items[ i ] = new JCheckBoxMenuItem( widgetTitles[ i ] );
         items[i].setSelected(true);
         popupMenu.add( items[ i ] );
         items[ i ].addActionListener( handler );

      }

      jc.addMouseListener(
         new MouseAdapter() {
            public void mousePressed( MouseEvent e )
               { checkForTriggerEvent( e );

               }

            public void mouseReleased( MouseEvent e )
               { checkForTriggerEvent( e ); }

            private void checkForTriggerEvent( MouseEvent e )
            {
               if ( e.isPopupTrigger() )
                  popupMenu.show( e.getComponent(),
                                  e.getX(), e.getY() );

            }
         }
      );
      setSize( 300, 200 );
  }

  public JTabbedPane getTabbedPane() {
     return jTabbedPane1;
  }

  /** Listener for the popup menu. */
  private class ItemHandler implements ActionListener {
    public void actionPerformed( ActionEvent e ) {
         // determine which menu item was selected
        for ( int i = 0; i < items.length; i++ )  {
            if ( e.getSource() == items[ i ] ) {

               if ( items[i].isSelected() )  {
                      //int index = ((JTabbedPane)jTabbedPane1).indexOfTab(widgetTitles[i]);
                      //jTabbedPane1.setEnabledAt(index, true);
											jTabbedPane1.add(widgetTitles[i], innerPanels[i]);
                      (tab.getDisplays())[i].setVisible(true);

							 }

							 else {
                      //int index = ((JTabbedPane)jTabbedPane1).indexOfTab(widgetTitles[i]);
                      //jTabbedPane1.setEnabledAt(index, false);
                      int index = ((JTabbedPane)jTabbedPane1).indexOfTab(widgetTitles[i]);
											jTabbedPane1.removeTabAt(index);
                      (tab.getDisplays())[i].setVisible(false);

               }
               return;
            }

         }    // end of for
    }         // end of actionPerformed
  }           // end of ItemHandler

}         // end of RelationPanel