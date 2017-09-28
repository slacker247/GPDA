import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import java.sql.*;
import java.util.Vector;
import java.io.*;

//the main window also holds all the functions that make calls to the database.

class InterfaceMain extends JFrame
{

public static ConfigData configuration;
private JTextField configPort, configDBDriver, configAddress, configName, configPassword, configHost;
private JDialog jDiagConfigHost;
private EvidencePane validated, unvalidated;
private JButton validatedApply, unvalidatedApply;
private	JTabbedPane tabs;

public InterfaceMain() {

    //MainFrame frame = new MainFrame(this,retrieveMissions(),getFieldNames(),getEvidenceList());
    try {
	initFrame();
    }
    catch (Exception e) {
	e.printStackTrace();
    }

    this.setSize(1000,800);
    this.setVisible(true);

    //frame.setVisible(true);
}

public void initFrame() throws Exception {

    this.addWindowListener(new WindowAdapter() {
	    public void windowClosing(WindowEvent e) {System.exit(0);}
	});

    setTitle("Belief Tagger");

    JMenuBar menuBar = new JMenuBar();

    JMenu menuFile    = new JMenu("File",false);
    JMenu menuHelp    = new JMenu("Help",false);
    JMenu menuOptions = new JMenu("Options",false);
    JMenu menuMission = new JMenu("Mission",false);

    JMenuItem updateDatabase = new JMenuItem("Update Database");
    updateDatabase.addActionListener(new ActionListener()
	{
	    public void actionPerformed(ActionEvent e)
	    {
		Vector evidenceList = validated.getEvidenceBelief();
	
		for (int i = 0; i < evidenceList.size(); i ++ ) 
		    updateBelief( (EvidenceBelief) evidenceList.elementAt(i) );

		evidenceList = unvalidated.getEvidenceBelief();

		for (int i=0; i < evidenceList.size(); i++ ) 
		    updateBelief( (EvidenceBelief) evidenceList.elementAt(i) );

				//now to update the tabs....
		reinit();
	    }
	});

    JMenuItem exitItem = new JMenuItem("Exit");
    exitItem.addActionListener(new ActionListener()
	{
	    public void actionPerformed(ActionEvent e)
	    {
		configuration.saveFile();
		System.exit(0);
	    }
	});

    JMenuItem aboutItem = new JMenuItem("About");
/*		aboutItem.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				actionPerformedAbout(e);
			}
		});
*/

    JMenuItem configDatabase = new JMenuItem("Config Database");
    configDatabase.addActionListener(new ActionListener()
	{
	    public void actionPerformed(ActionEvent e)
	    {
		if (jDiagConfigHost == null) initConfigurationBox();
		try
		    {
			String[] values = configuration.getValues();

			configPort.setText(values[configuration.PORT]);
			configDBDriver.setText(values[configuration.DRIVER]);
			configAddress.setText(values[configuration.ADDRESS]);
			configName.setText(values[configuration.USERNAME]);
			configHost.setText(values[configuration.HOST]);
			configPassword.setText(values[configuration.PASSWORD]);
		    }
		catch (Exception ee)
		    {
			ee.printStackTrace();
		    }
		jDiagConfigHost.setSize(540, 340);
		jDiagConfigHost.setVisible(true);
	    }
	});

    JMenuItem runWeka = new JMenuItem("Run Weka Classifier");
    //want it to pop up a frame that will have an out put display of what is going on....
    runWeka.addActionListener(new ActionListener() 
	{
	    // ALL OF THE WEKA CODE IS IN HERE!
	    private MyInstances readCSVFile(String fileName) throws IOException {
	        //reads the CSV file and creates the objects required by Weka Classifiers
		weka.core.converters.CSVLoader firstLoader = new weka.core.converters.CSVLoader();
		firstLoader.setSource(new File(fileName));

		weka.core.Instances firstInstance = firstLoader.getDataSet();

		return new MyInstances(firstInstance);

	    }

	    private void createCSVFile (String query, String fileName) throws IOException {
		// Creates the CSV Files based on the data pulled from the knowledge base
		FileWriter oOutStream = new FileWriter(fileName, false);
				
	        //have to get the labels that we want to use for the top line and write them comma seperated.
		Vector labels = getFieldNames();

		for (int i = 5; i < 49; i ++) {
		    if (i != 17 && i != 22 && i != 31 && i != 35 && i != 40) 
			oOutStream.write((String)labels.elementAt(i) + ",");
		}

		oOutStream.write("Belief");
		oOutStream.write("\n");

		Vector evidence = runQuery(query);//"Select * from Evidence;"

		for (int i=0; i < evidence.size(); i++) {
		    Vector row = (Vector) evidence.elementAt(i);

		    for (int j=4; j < 43; j++) {
			if ( ((String)row.elementAt(j)).compareTo("?") == 0) 
			    oOutStream.write("None,");
			else
			    oOutStream.write ((String)row.elementAt(j)+ ",");
		    }
		    if ( ((String)row.elementAt(2)).compareTo("?") == 0) 
			oOutStream.write(EvidenceBelief.getBeliefString( (double) -1));
		    else
			oOutStream.write(EvidenceBelief.getBeliefString(Double.parseDouble( (String) row.elementAt(2))));

		    oOutStream.write("\n");
		}

		oOutStream.flush();
		oOutStream.close();
	    }

	    public void actionPerformed(ActionEvent e)
	    {
		validated.fix();
		unvalidated.fix();
		JFrame logWindow = new JFrame("Weka Log");
		JTextArea logArea = new JTextArea();
		JScrollPane scroller = new JScrollPane(logArea);

		MyInstances largeSet, trainingData, testData;
		weka.core.FastVector header;

		logArea.setEditable(false);
		logWindow.getContentPane().add(scroller);
		logWindow.setSize(500,700);				
		logWindow.setVisible(true);
		logArea.append("-- Initializing Weka\n");

		//getting the header for the entire data set
	        //put the first set into a comma seperated file and then import it into Weka that way.
		try {
		    createCSVFile ("Select * from Evidence;", "firstFile");
		} catch (IOException ioe) {
		    logArea.append("EEE Can't write the CSV file (exiting...)\n"); return;
		}

		logArea.append("--- Attempting to capture the Header Information\n");

		try {
		    largeSet = readCSVFile("firstFile");
		}
		catch (IOException ioe) {
		    logArea.append("EEE Can't open data file (exiting...)\n"); return;
		}
		header = largeSet.getHeader();
		logArea.append("--- Obtained the header object\n");


		try {
		    createCSVFile ("Select * from Evidence where Belief != '?';","secondFile");
		} catch (IOException ioe) {
		    logArea.append("EEE Can't write training Data File (exiting...)\n"); return;
		}

		try {
		    trainingData = readCSVFile("secondFile");		
		} catch (IOException ioe) {
		    logArea.append("EEE Can't open Training Data File (exiting...)\n"); return;
		}

		logArea.append("--- Training datafile created and loaded\n");

		try {
		    createCSVFile("Select * from Evidence where Belief = '?';","thirdFile");
		} catch (IOException ioe) {
		    logArea.append("EEE Can't write test Data File (exiting...)\n"); return;
		}

		try {
		    testData = readCSVFile("thirdFile");
		} catch (IOException ioe) {
		    logArea.append("EEE Can't open Test Data File (exiting...)\n"); return;
		}

		logArea.append("--- Test Dataset created and loaded\n");
		logArea.append("\n--- Adjusting Headers on the training and test Data\n");
				
		testData.setHeader(header);
		trainingData.setHeader(header);

		testData.setClassIndex(testData.numAttributes()-1);
		trainingData.setClassIndex(trainingData.numAttributes()-1);

		largeSet = null;
		header = null;

		System.gc();			// run the garbage collecter

		weka.classifiers.j48.J48 j48Classifier;
		try {
		    //The Classifier that we're using....
		    j48Classifier = new weka.classifiers.j48.J48();
		    j48Classifier.setMinNumObj(1);

		    j48Classifier.buildClassifier((weka.core.Instances)trainingData);
		    logArea.append("--- Classifier built successfully\n");
		} catch (Exception exc) {
		    logArea.append("EEE cannot build the classifier (Exiting...)\n"); 
		    logArea.append(exc.toString());
		    return;
		}

		//attempting to classify one instance of the test data	
		logArea.append("--- Classifying TestData\n");

		try { 
		    for (int i=0; i < testData.numInstances(); i ++) {
			int classification = (int) j48Classifier.classifyInstance(testData.instance(i));
			logArea.append("Attempting to classify an instance: " +
				       classification +
				       " (" +
				       testData.classAttribute().value(classification) +
				       ")\n");
			unvalidated.setEvidenceBeliefIndex(i,testData.classAttribute().value(classification));
		    }
		} catch (Exception exc) {
		    logArea.append("EEE cannot classify instance (Exiting...)\n"); 
		    return;
		}

		logArea.append("--- Done!\n");
	    }
	});

    // gets the list of missions and populates the menu list submenu.
    ButtonGroup missionGroup = new ButtonGroup();
    Vector missions = retrieveMissions();
    for (int i=0;i < missions.size() ; i++)
	{
	    JRadioButtonMenuItem mission = new JRadioButtonMenuItem((String)missions.elementAt(i));
	    mission.addActionListener(new ActionListener() 
		{
		    public void actionPerformed(ActionEvent e)
		    {
			configuration.setMission(((JRadioButtonMenuItem)(e.getSource())).getText());
			Vector rqFieldNames = getFieldNames();
			Vector evidenceList = getEvidenceList(true);
			Vector unevidenceList = getEvidenceList(false);
			validated.missionChanged(rqFieldNames,evidenceList);
			unvalidated.missionChanged(rqFieldNames,unevidenceList);

			System.out.println("Mission Changed to: " + configuration.getMission());
		    }
		});

	    if (((String)missions.elementAt(i)).compareToIgnoreCase(configuration.getMission())==0)
				mission.setSelected(true);

	    missionGroup.add(mission);
	    menuMission.add(mission);
	}
	
    menuFile.add(updateDatabase);
    menuFile.add(runWeka);
    menuFile.add(exitItem);
			
    menuHelp.add(aboutItem);
    menuOptions.add(configDatabase);
    menuOptions.add(menuMission);

    menuBar.add(menuFile);
    menuBar.add(menuOptions);
    menuBar.add(menuHelp);

    setJMenuBar(menuBar);

    // Evidence Panes
    tabs = new JTabbedPane();

    validated = new EvidencePane(getFieldNames(),getEvidenceList(true),this);
    unvalidated = new EvidencePane(getFieldNames(),getEvidenceList(false),this);

    tabs.add("Validated Beliefs",validated);
    tabs.add("Unvalidated Beliefs",unvalidated);

    getContentPane().add(tabs);

}

    protected void reinit() {
	tabs.removeAll();
	validated = new EvidencePane(getFieldNames(),getEvidenceList(true),this);
	unvalidated = new EvidencePane(getFieldNames(),getEvidenceList(false),this);

	tabs.add("Validated Beliefs",validated);
	tabs.add("Unvalidated Beliefs",unvalidated);
    }

    public static void main(String[] args) {
	Vector missions = new Vector();
	try {
	    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());

	    configuration = new ConfigData();
	    //missions = retrieveMissions();

	}
	catch(Exception e) {
	    e.printStackTrace();
	}
		
	new InterfaceMain();
    }

    //DATABASE FUNCTIONS
    public static Vector getEvidenceList (boolean validated) {
	Vector evidenceList = new Vector();
	Vector evidenceListRaw;
	if (validated) 
	    evidenceListRaw = runQuery ("select Case_ID from Evidence where Belief != '?';");
	else
	    evidenceListRaw = runQuery ("select Case_ID from Evidence where Belief = '?';");

	for (int i = 0; i < evidenceListRaw.size(); i++)
	    {
		Vector column = (Vector)evidenceListRaw.elementAt(i);
		for (int j=0; j < column.size(); j++) {
		    evidenceList.add(column.elementAt(j));
		}
	    }

	return evidenceList;
    }

    public static Vector getFieldValues (int caseIDValue) {
	Vector fieldValues = new Vector();
	Vector fieldValuesRaw = runQuery("select * from Evidence where Case_ID=" +
					 caseIDValue +
					 " limit 1;");

	for (int i=0; i < fieldValuesRaw.size(); i++)
	    {
		Vector column = (Vector)fieldValuesRaw.elementAt(i);
		for (int j=0; j< column.size(); j++)
		    {
			fieldValues.add(column.elementAt(j));
		    }
	    }
	return fieldValues;
    }

    public static Vector getFieldNames () {
	Vector fieldNamesRaw = runQuery("Select * from Labels;");
	Vector fieldNames = new Vector();

	for (int i=0;i<fieldNamesRaw.size() ;i++ )
	    {
		Vector column = (Vector)fieldNamesRaw.elementAt(i);
		//System.out.print((i+1) + ": ");
		for (int j=0;j<column.size() ;j++ )
		    {
			String fieldName = (String)column.elementAt(j);
				//System.out.println(fieldName);
			fieldNames.add(fieldName);
		    }
		//System.out.println();
	    }
	
	return fieldNames;
    }

    public static double getBelief (int caseID) {
	Vector beliefRaw = runQuery("Select Belief from Evidence where Case_ID=" +
				    caseID +
				    " limit 1;");
	try {
	    return Double.parseDouble((String) ((Vector)beliefRaw.elementAt(0)).elementAt(0) );
	}
	catch (Exception e) {
	    return (double) -1;
	}
    }

    private static Vector retrieveMissions () {
	Vector returnValue = new Vector();
        Vector dbs = null;

        dbs = runQuery("Show databases;");
		
	for (int i = 0; i < dbs.size(); i++ )
	    {
		Vector row = (Vector) dbs.elementAt(i);
		for (int j = 0; j < row.size() ; j++ )
		    {
			String missionName = (String) row.elementAt(j);
			if (!(missionName.trim().equalsIgnoreCase("mysql")) &&
			    !(missionName.trim().equalsIgnoreCase("test")))
			    returnValue.add(missionName);
		    }
	    }
        return returnValue;
    }
	
    public static Vector runQuery (String query) {
	String dbURL = configuration.getDBURL() + "//" +
	    configuration.getHost() + "/" +
	    configuration.getMission();
	ResultSet dbs = null;
	Connection conn = null;
	Statement queryStatement = null;
	Vector rows = new Vector();

	try
	    {
		Class.forName(configuration.getDBDriver()).newInstance();
		conn = DriverManager.getConnection(dbURL, configuration.getUsername(),
						   configuration.getPassword());
		queryStatement = conn.createStatement();

		//System.out.println("Established a connection with the DB: " + dbURL);

		dbs = queryStatement.executeQuery(query);
		ResultSetMetaData rsmd = dbs.getMetaData();
		int numberOfColumns = rsmd.getColumnCount();
		while (dbs.next())
		    {
			Vector columns = new Vector();
			for (int column = 1;column <= numberOfColumns ; column++)
			    {
				columns.add(dbs.getObject(column));
				//System.out.println(column);
				}
			rows.add(columns);
		    }
	    }
	catch (Exception e)
	    {
		System.out.println("EEE - Error while running query: ");
		System.out.println("        " + query);
		e.printStackTrace();
		System.out.println("EEE - ---------------------------");
	    }
	finally {
	    if (queryStatement != null)
		try	{ queryStatement.close(); }
	    catch (SQLException SQLEx)  { }
	    if (conn != null)
		try	{ conn.close(); }
	    catch (SQLException SQLEx) { }
	}

	return rows;
    }

	
    public int updateBelief (EvidenceBelief update) {
	//updates the belief values in the database
	String dbURL = configuration.getDBURL() + "//" +
	    configuration.getHost() + "/" +
	    configuration.getMission();
	int dbs = -1;
	Connection conn = null;
	Statement queryStatement = null;

	try
	    {
		Class.forName(configuration.getDBDriver()).newInstance();
		conn = DriverManager.getConnection(dbURL, configuration.getUsername(),
						   configuration.getPassword());
		queryStatement = conn.createStatement();

		System.out.println("Established a connection with the DB: " + dbURL);

		dbs = queryStatement.executeUpdate("Update Evidence set Belief='" +
						   update.getDBValue() +
						   "' where Case_ID=" +
						   update.getEvidence() +
						   ";");
	    }
	catch (Exception e)
	    {
		System.out.println("EEE - Error while running update query: ");
		e.printStackTrace();
		System.out.println("EEE - ---------------------------");
	    }
	finally {
	    if (queryStatement != null)
		try	{ queryStatement.close(); }
	    catch (SQLException SQLEx)  { }
	    if (conn != null)
		try	{ conn.close(); }
	    catch (SQLException SQLEx) { }
	}

	return dbs;
    }

    //CONFIGURATION OF THE DATABSE
    public void initConfigurationBox() {

	jDiagConfigHost = new JDialog();
        jDiagConfigHost.getContentPane().setLayout(null);
        jDiagConfigHost.setTitle("Configure Host");

	//Labels
	JLabel label[] = new JLabel[6];
	int labelXY[][] = {
	    { 30,  20, 200,  16},
	    { 30,  70,  28,  16},
	    {310,  20, 110,  16},
	    {310,  70, 120,  16},
	    {310, 120,  70,  16},
	    {310, 170,  70,  16} };
		
	String labelText[] = {
	    "IP Address of Computer Name:",
	    "Port:",
	    "Database Driver:",
	    "Database Address:",
	    "User Name:",
	    "Password:" };

	for (int i = 0;i < 6 ; i++ )
	    {
		label[i] = new JLabel(labelText[i]);
		jDiagConfigHost.getContentPane().add(label[i]);
		label[i].setBounds(labelXY[i][0],labelXY[i][1],labelXY[i][2],labelXY[i][3]);
	    }

	//Text Fields
	configHost = new JTextField();
        jDiagConfigHost.getContentPane().add(configHost);
        configHost.setBounds(30, 40, 190, 20);

        configPort = new JTextField();
	jDiagConfigHost.getContentPane().add(configPort);
        configPort.setBounds(30, 90, 70, 20);

        configDBDriver = new JTextField();
	jDiagConfigHost.getContentPane().add(configDBDriver);
        configDBDriver.setBounds(310, 40, 140, 20);

        configAddress = new JTextField();
	jDiagConfigHost.getContentPane().add(configAddress);
        configAddress.setBounds(310, 90, 140, 20);

        configName = new JTextField();
	jDiagConfigHost.getContentPane().add(configName);
        configName.setBounds(310, 140, 140, 20);

        configPassword = new JTextField();
	jDiagConfigHost.getContentPane().add(configPassword);
        configPassword.setBounds(310, 190, 140, 20);
	
	//OK and Cancel Buttons
	JButton okButton = new JButton("OK");
        okButton.addActionListener(new java.awt.event.ActionListener() {
		public void actionPerformed(java.awt.event.ActionEvent evt) {
		    try
			{
			    configuration.updateValues(configName.getText(),
						       configPassword.getText(),
						       configHost.getText(),
						       configPort.getText(),
						       configAddress.getText(),
						       configDBDriver.getText(),
						       configuration.getMission());
			}
		    catch (Exception e)
			{
			    e.printStackTrace();
			}
		    jDiagConfigHost.setVisible(false);
		}
	    });

        jDiagConfigHost.getContentPane().add(okButton);
        okButton.setBounds(140, 230, 70, 20);

        JButton cancelButton = new JButton("Cancel");
        cancelButton.addActionListener(new java.awt.event.ActionListener() {

            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jDiagConfigHost.setVisible(false);
            }
        });

        jDiagConfigHost.getContentPane().add(cancelButton);
        cancelButton.setBounds(290, 230, 75, 20);
	}
}
