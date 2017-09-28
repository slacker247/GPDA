import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.Vector;
import javax.swing.event.*;

/*
	displays all the records of reporters questions
		used twice, one to display the reporters questions that have belief assigned to them
		and the second to display the reporters questions that do not have belief assigned.
*/

class EvidencePane extends JPanel
{

	private JPanel mainPanel, listPanel, displayPanel;
	private JList evidenceListBox;
	private JButton submitDB;
	private DefaultListModel listData;
	private ReportersQuestionsPanel questionsPanel;
	private InterfaceMain myParent;
	private Vector rqFieldNames, evidenceList, evidenceBelief;
	int currentRQTemplate = -1;									//holds the index for the previous belief value (combo box) 
																//  (basically an ischanged value)

	
	//evidenceList contains the caseIDs of the reporters questions
	//caller is so that this pane can call the database functions to get the reporters questions

	public EvidencePane(Vector fieldNames, Vector evidenceList, InterfaceMain caller) { 
		
		evidenceBelief = new Vector();

		myParent = caller;
		rqFieldNames = fieldNames;
		this.evidenceList = evidenceList;
		try
		{
			System.out.println("Initializing the Evidence Frame");
			initializeFrame();
		}
		catch (Exception e)
		{
			System.out.println("EEE -- Error during Frame Initialization: ");
			e.printStackTrace();
		}

		//set up the evidence/belief conversion.....
		for (int i=0; i < evidenceList.size(); i++)	{
			evidenceBelief.add(
				new EvidenceBelief( 
					((Integer)evidenceList.elementAt(i)).intValue(), 
					myParent.getBelief( ((Integer)evidenceList.elementAt(i)).intValue() ) 
				)
			);
		}

	}

	public void initializeFrame () throws Exception {


		mainPanel =  this;
		
		mainPanel.setLayout(new BorderLayout());


	//List Box on the left side of the screen
		listData = new DefaultListModel();

		evidenceListBox = new JList(listData);


		evidenceListBox.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

		evidenceListBox.addListSelectionListener(new ListSelectionListener()
		{
			public void valueChanged(ListSelectionEvent lse) {
				//retrieve the values for the ReportersPanel and 
				//draw it on the appropriate place.
				

				if (!lse.getValueIsAdjusting() && !((JList)lse.getSource()).isSelectionEmpty()) {

					if (currentRQTemplate != -1) {
						//have to save the value of the combo box....
						for (int i = 0; i < evidenceBelief.size(); i++) {
							if ( ((EvidenceBelief)evidenceBelief.elementAt(i)).getEvidence() == currentRQTemplate) {
								((EvidenceBelief)evidenceBelief.elementAt(i)).setBelief(questionsPanel.getBeliefValue());
								break;
							}
						}
					}

					int rqTemplate = Integer.parseInt(((String)((JList)lse.getSource()).getSelectedValue()).substring(9));

					System.out.println("Retrieving the record for: " + rqTemplate);

					Vector entries = myParent.getFieldValues (rqTemplate);
						questionsPanel.setVisible(true);
						questionsPanel.updateTextFields(entries);
						for (int i = 0; i < evidenceBelief.size(); i++) {
							if (((EvidenceBelief)evidenceBelief.elementAt(i)).getEvidence() == rqTemplate) {
								questionsPanel.setBeliefComboBox(((EvidenceBelief)evidenceBelief.elementAt(i)).getBelief());
								break;
							}
						}
						currentRQTemplate = rqTemplate;
					System.out.println("The values should be placed on the display");
				}
				
			}
		});


		JScrollPane scrollPane = new JScrollPane(evidenceListBox);
		
		listPanel = new JPanel(new BorderLayout());

		listPanel.add(scrollPane,BorderLayout.CENTER);

	//Display Pane on the right side of the screen
		displayPanel = new JPanel(new BorderLayout());
		questionsPanel = new ReportersQuestionsPanel(rqFieldNames,null);
		questionsPanel.setVisible(false);
		displayPanel.add(questionsPanel,BorderLayout.CENTER);

	//adding the panels onto everything
		mainPanel.add(listPanel, BorderLayout.WEST);
		mainPanel.add(displayPanel, BorderLayout.CENTER);
		

	//inserting data into the list....
		if (evidenceList.size() == 0) {	//no evidence found
			listData.addElement("None");
			evidenceListBox.setEnabled(false);
		}
		else {
			for (int i=0; i < evidenceList.size(); i++) {
				listData.addElement("Case ID #" + (Integer)evidenceList.elementAt(i));
			}
		}

	}

	private void actionPerformedSubmitDB(ActionEvent e) {
		//updates the belief values for the reporters questions
		if (currentRQTemplate != -1) {
		//have to save the value of the combo box....
			for (int i = 0; i < evidenceBelief.size(); i++) {
				if ( ((EvidenceBelief)evidenceBelief.elementAt(i)).getEvidence() == currentRQTemplate) {
					((EvidenceBelief)evidenceBelief.elementAt(i)).setBelief(questionsPanel.getBeliefValue());
					break;
				}
			}
		}

		for (int i = 0; i < evidenceBelief.size(); i++) {
			myParent.updateBelief( (EvidenceBelief) evidenceBelief.elementAt(i));
		}
		Vector entries = myParent.getFieldValues (currentRQTemplate);
		questionsPanel.updateTextFields(entries);
		//System.out.println("||| Submit to Database");
	}

	public void missionChanged(Vector rqFieldNames, Vector evidenceList) {
		//when the mission is changed this is called to update the field names in the reporters questions object
		// as well as updates the list box that displays the evidence found
		questionsPanel.setVisible(false);
		evidenceListBox.setEnabled(true);


		questionsPanel.updateFieldNames(rqFieldNames);

		if (!listData.isEmpty())
			listData.clear();

		if (evidenceList.size() == 0) {
			listData.addElement("None");
			evidenceListBox.setEnabled(false);
		}
		else {
			for (int i=0; i < evidenceList.size(); i++) {
				listData.addElement("Case ID #" + (Integer)evidenceList.elementAt(i));
			}
		}
	}

	public Vector getEvidenceBelief() {
		//saves the current changes to the reporters questions object
		// and then returns the evidence/belief objects back
		if (currentRQTemplate != -1) {
		//have to save the value of the combo box....
			for (int i = 0; i < evidenceBelief.size(); i++) {
				if ( ((EvidenceBelief)evidenceBelief.elementAt(i)).getEvidence() == currentRQTemplate) {
					((EvidenceBelief)evidenceBelief.elementAt(i)).setBelief(questionsPanel.getBeliefValue());
					break;
				}
			}
		}

		return evidenceBelief;
	}

	public void setEvidenceBelief(int evidenceIndex, int beliefValue) {
		//sets the belief value of a piece of evidence based on the caseID
		for (int i = 0; i < evidenceBelief.size(); i++) {
			if ( ((EvidenceBelief)evidenceBelief.elementAt(i)).getEvidence() == evidenceIndex) {
					((EvidenceBelief)evidenceBelief.elementAt(i)).setBelief(beliefValue);
					break;
			}
		}
	}

	public void setEvidenceBeliefIndex(int index, String value) {
		//sets the belief value of a piece of evidence based on the order displayed in the combo box
		((EvidenceBelief)evidenceBelief.elementAt(index)).setBelief(value);
	}

	public void fix() {
		//minor fix so that the combo box would not overwrite itself after being run through weka.
		currentRQTemplate = -1;
		evidenceListBox.clearSelection();
	}
	
}
