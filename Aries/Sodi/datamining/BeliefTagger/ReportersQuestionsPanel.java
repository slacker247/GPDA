import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import java.util.Vector;

//displays the reporters questions

class ReportersQuestionsPanel extends JPanel
{
	
	private	final int FIELDS = 51;
	private final int HEADERS = 7;

	private final int FIELD_LABEL_SETTING_WIDTH = 100;
	private final int FIELD_LABEL_SETTING_HEIGHT = 16;
	private final int FIELD_TEXT_SETTING_WIDTH = 130;
	private final int FIELD_TEXT_SETTING_HEIGHT = 20;
	private final int HEADER_WIDTH = 170;
	private final int HEADER_HEIGHT = 16;

	JLabel fieldLabel[] = new JLabel[FIELDS];
	JLabel header[] = new JLabel[HEADERS];
	JTextField fieldTextBox[] = new JTextField[FIELDS];
	JComboBox belief;

	public ReportersQuestionsPanel (Vector fieldNames, Vector entries) {
		initPanel();
		updateFieldNames(fieldNames);
	}

	private void initPanel() {
		int fieldSettings[][] = {
		{ 40,  50}, 		{ 40,  80}, 		{ 40, 110}, 		{ 40, 140}, 		{ 40, 200},
		{ 40, 230}, 		{ 40, 260}, 		{ 40, 290}, 		{ 40, 320}, 		{ 40, 350},
		{ 40, 380}, 		{ 40, 410}, 		{ 40, 440}, 		{ 40, 470}, 		{ 40, 500},
		{ 40, 530}, 		{ 40, 580}, 		{ 40, 610}, 		{ 40, 640}, 		{ 40, 670},
		{320,  40}, 		{320,  70}, 		{320, 100}, 		{320, 130}, 		{320, 160},
		{320, 190}, 		{320, 220}, 		{320, 250}, 		{320, 310}, 		{320, 340},
		{320, 370}, 		{320, 430}, 		{320, 460},  		{320, 490}, 		{320, 520},
		{570,  40}, 		{570,  70}, 		{570, 100}, 		{570, 130}, 		{570, 160},
		{570, 190}, 		{570, 220}, 		{570, 250}, 		{570, 310}, 		{570, 340},
		{570, 370}, 		{570, 400}, 		{570, 450}, 		{570, 480}, 		{570, 510},
		{570, 540}};

		int headerSettings[][] = {
			{ 60, 170 }, 			{ 70, 560 }, 			{340,  20}, 			{340, 280},
			{340, 410}, 			{590, 20  }, 			{590, 280} }; 

		this.setLayout(null);
		

		for (int i = 0;i < FIELDS ; i++)
		{
			fieldLabel[i] = new JLabel();
			fieldTextBox[i] = new JTextField();
			fieldLabel[i].setText("Dummy Label " + i);

			fieldLabel[i].setHorizontalAlignment(SwingConstants.TRAILING);
			fieldLabel[i].setBounds(fieldSettings[i][0], fieldSettings[i][1], FIELD_LABEL_SETTING_WIDTH, FIELD_LABEL_SETTING_HEIGHT);
			this.add(fieldLabel[i]);

			fieldTextBox[i].setBounds(fieldSettings[i][0]+110,fieldSettings[i][1], FIELD_TEXT_SETTING_WIDTH, FIELD_TEXT_SETTING_HEIGHT);
			fieldTextBox[i].setEditable(false);
			this.add(fieldTextBox[i]);
		}

		for (int i = 0; i < HEADERS ; i++)
		{
			header[i] = new JLabel();
			header[i].setText("Dummy Header");
			header[i].setHorizontalAlignment(SwingConstants.CENTER);
			add(header[i]);
			header[i].setBounds(headerSettings[i][0],headerSettings[i][1],HEADER_WIDTH,HEADER_HEIGHT);
		}

		//add a combo box that will be set to the current belief of the record
		JLabel beliefLabel = new JLabel("Set Belief");
		beliefLabel.setHorizontalAlignment(SwingConstants.TRAILING);
		beliefLabel.setBounds(570,600,100,16);
		this.add(beliefLabel);

		DefaultComboBoxModel comboList = new DefaultComboBoxModel(EvidenceBelief.getBeliefs());
		belief = new JComboBox(comboList);
		belief.setBounds(680,600,130,20);
		this.add(belief);


		this.setSize(new Dimension(900,750));
		this.setVisible(true);
	}

	public int getBeliefValue() 
	{
		//returns the belief value held in the combo box that can be manipulated by the user
		return belief.getSelectedIndex();
	}
	
	public void updateFieldNames(Vector fieldNames) {
		int count = 0;

            fieldLabel[0].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
            fieldLabel[1].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
            fieldLabel[2].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[3].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));

			header[0].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_', ' '));

			fieldLabel[4].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[5].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[6].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[7].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[8].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[9].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[10].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[11].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[12].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[13].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[14].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[15].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));


			header [1].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_', ' '));

			fieldLabel[16].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[17].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[18].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[19].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));

			header [2].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_', ' '));

			fieldLabel[20].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[21].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[22].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[23].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[24].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[25].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[26].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[27].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));

			header [3].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_', ' '));

			fieldLabel[28].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[29].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[30].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));

			header [4].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_', ' '));

			fieldLabel[31].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[32].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[33].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[34].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));

			header [5].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_', ' '));

			fieldLabel[35].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[36].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[37].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[38].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[39].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[40].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[41].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[42].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));

   			header [6].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_', ' '));

			fieldLabel[43].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[44].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[45].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[46].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[47].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[48].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[49].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
			fieldLabel[50].setText(((String)fieldNames.elementAt(count++)).substring(1).replace('_',  ' '));
	}

	public void updateTextFields(Vector fieldValues) {
		fieldTextBox[0].setText(((Integer)fieldValues.elementAt(0)).toString());
		for (int i = 1; i < FIELDS; i++) {
			if (i == 2) {
				System.out.println( (String) fieldValues.elementAt(i));
				if ( ((String)fieldValues.elementAt(i)).compareTo("?") == 0)
					fieldTextBox[i].setText( EvidenceBelief.getBeliefString( (double) -1) );
				else
					fieldTextBox[i].setText( EvidenceBelief.getBeliefString( Double.parseDouble( (String) fieldValues.elementAt(i)) ) );
			}
			else 
				fieldTextBox[i].setText( (String) fieldValues.elementAt(i));
		}
	}

	public void setBeliefComboBox (int belief) {
		//sets the combobox (used mostly to set the default value)
		this.belief.setSelectedIndex(belief);
	}

	public void setBeliefComboBox (double belief) {
		//sets the combobox based on the double value from the database
		this.belief.setSelectedIndex(EvidenceBelief.getBeliefInt(belief));
	}

}
