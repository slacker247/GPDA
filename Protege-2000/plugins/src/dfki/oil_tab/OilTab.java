package dfki.protege.oil_tab;

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import edu.stanford.smi.protege.widget.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protege.util.*;


public class OilTab extends AbstractWidget implements OilConstants {

    KnowledgeBase itsKB;
    Fact itsFact;
    JTextArea itsFactReportTextArea;
    JTextArea itsMessageTextArea;
    OilFrames frames;
    String itsTells = null;
    boolean itsLatexExported = false;

    String itsPDFLatexCommand;
    String itsAcroreadCommand;
    final static String itsLatexFileName = "fact-kb";


    public void initialize() {
        // setIcon(Icons.getInstancesIcon()); // icon!!!
        setLabel("Oil");
	itsPDFLatexCommand = 
	  ApplicationProperties.getString("oil.latex.pdflatex", null);
	itsAcroreadCommand = 
	  ApplicationProperties.getString("oil.latex.acroread", null);
	itsKB = getKnowledgeBase();
	itsMessageTextArea = new JTextArea();
	frames = new OilFrames(itsKB, itsMessageTextArea);
	itsFact = new Fact();
	itsFact.setDefaultNSHost("localhost");
	itsFact.setDefaultNSPort("8000");
	itsFact.setDefaultServerName("FaCTServer");
        itsFactReportTextArea = new JTextArea();
	itsFact.setReportArea(itsFactReportTextArea);
        add(createOilPanel());
    }


    JComponent createOilPanel() {
      JSplitPane splitter = createTopBottomSplitPane("OilTab.top_bottom", 200);
      LabeledComponent oilComponent = new LabeledComponent("FaCT Interaction", 
	new JScrollPane(itsFactReportTextArea));
      oilComponent.addHeaderButton(
         new AbstractAction("view XML KB", OilIcons.getViewXMLIcon()) {
           public void actionPerformed(ActionEvent event) { 
	    if (itsTells != null) {
	      viewXML();
	    }
	  }
        }
      );
      if (itsPDFLatexCommand != null && itsAcroreadCommand != null) {
        oilComponent.addHeaderButton(
	  new AbstractAction("view SHIQ KB", OilIcons.getViewSHIQIcon()) {
	    public void actionPerformed(ActionEvent event) { 
	      if (itsLatexExported) {
	        viewSHIQ(); 
	      }
	    }
	  }
        );
      }
      oilComponent.addHeaderButton(
	new AbstractAction("classify", Icons.getCreateIcon()) {
	  public void actionPerformed(ActionEvent event) { classify(); }
	}
      );
      splitter.setTopComponent(oilComponent);
      LabeledComponent messages = new LabeledComponent("Messages", 
	new JScrollPane(itsMessageTextArea));
      splitter.setBottomComponent(messages);
      return splitter;
    }


  void classify() {

    itsFactReportTextArea.setText("");
    itsMessageTextArea.setText("");
    itsTells = null;
    itsLatexExported = false;

    // export
    itsFact.latexOutput(itsLatexFileName + ".tex");
    itsFact.connect();
    itsFact.beginTransaction();
    itsFact.clear(); // throw away the old KB
    exportOilProperties();
    exportOilClasses();
    exportOilAxioms();
    itsFact.endTransaction();
    itsLatexExported = itsFact.endLatexOutput();

    // import
    if (itsFact.connected()) {
      showUnsatisfiableClasses();
      getDirectSuperClasses();
      itsTells = itsFact.getAllTells();
    }

    itsFact.release();

  }


  // import

  void showUnsatisfiableClasses() {
    Collection oilClasses = frames.getInstances(CLASS);
    for (Iterator ocIterator = oilClasses.iterator();
         ocIterator.hasNext();) {
      Cls oilClass = (Cls)ocIterator.next();
      if (!frames.equals(oilClass, BOTTOM)) {
        FactConcept concept = getFactConcept(oilClass);
        if (!itsFact.satisfiable(concept))
	  report("class " + oilClass.getName() + " is not satisfiable");
      }
    }
  }


  void getDirectSuperClasses() {

    Collection oilClasses = frames.getInstances(CLASS);

    // hack needed since LISP symbols are used by FaCT !!!
    Hashtable classes = new Hashtable(); // lowercase -> cls
    for (Iterator ocIterator = oilClasses.iterator();
         ocIterator.hasNext();) {
      Cls oilClass = (Cls)ocIterator.next();
      String lowercaseName = oilClass.getName().toLowerCase();
      Cls cls = (Cls)classes.get(lowercaseName);
      if (cls != null)
	report("classes identical wrt. FaCT: " + oilClass.getName() + " & "
	  + cls.getName());
      else
	classes.put(lowercaseName, oilClass);
    }

    for (Iterator ocIterator = oilClasses.iterator();
         ocIterator.hasNext();) {

      Cls oilClass = (Cls)ocIterator.next();
      if (frames.equals(oilClass, TOP))
	continue;

      FactConcept concept = getFactConcept(oilClass);

      // the following is not finished!!
	Collection superConcepts =
	  itsFact.getDirectSuperConcepts(concept);

	if (superConcepts != null && !superConcepts.isEmpty()) {
	  Collection oldSuperClasses = oilClass.getDirectSuperclasses();
	  for (Iterator iter = oldSuperClasses.iterator();
	       iter.hasNext();) {
	    oilClass.removeDirectSuperclass((Cls)iter.next());
	  }
	  for (Iterator iterator = superConcepts.iterator();
	       iterator.hasNext();) {
	    FactConcept superConcept = (FactConcept)iterator.next();
	    String superConceptName;
	    if (superConcept instanceof FactTOP)
	      superConceptName = TOP;
	    else if (superConcept instanceof FactBOTTOM)
	      // should never happen!
	      superConceptName = BOTTOM;
	    else if (superConcept instanceof FactPrimitiveConcept)
	      superConceptName = 
		((FactPrimitiveConcept)superConcept).getName();
	    else { // anonymous class
	      System.out.println("anonymous classes not yet handled");
	      superConceptName = TOP; // hack ... !!!
	    }
	    Cls superClass;
	    superClass = (Cls)classes.get(superConceptName.toLowerCase());
	    if (superClass == null)
	      superClass = itsKB.getCls(superConceptName); // ???
	    if (superClass == null)
	      superClass = itsKB.getCls(superConceptName.toLowerCase()); // ???
	    if (superClass != null)
	      oilClass.addDirectSuperclass(superClass);
	    else
	      report("superclass not found: " + superConcept);
	  }
	} else {
	  report("no superconcepts for: " + oilClass);
	}

    }
  }


  // export properties

  void exportOilProperties() {
    Collection oilProperties = frames.getInstances(PROPERTY);
    for (Iterator opIterator = oilProperties.iterator();
         opIterator.hasNext();) {
      Slot oilProperty = (Slot)opIterator.next();
      exportOilProperty(oilProperty);
    }
  }

  void exportOilProperty(Slot oilProperty) {
    String propertyName = oilProperty.getName();
    FactPrimitiveRole factRole = new FactPrimitiveRole(propertyName);
    // defrole
    itsFact.defrole(propertyName);
    // subrole
    Collection superProperties = 
      frames.getOwnSlotValues(oilProperty, SUBPROPERTYOF);
    for (Iterator spIterator = superProperties.iterator();
	 spIterator.hasNext();) {
      Instance superProperty = (Instance)spIterator.next();
      String superPropertyName = superProperty.getName();
      itsFact.impliesR(propertyName, superPropertyName);
    }
    // inverse role (from Protege :SLOT-INVERSE)
    // (should we also use oil:inverseRelationOf?)
    // the OIL defintion allows more than one slot inverse!
    // the following acts as if this were also possible in Protege:
    Collection inverseProperties = 
      frames.getOwnSlotValues(oilProperty, INVERSEPROPERTY);
    for (Iterator ipIterator = inverseProperties.iterator();
	 ipIterator.hasNext();) {
      Instance inverse = (Instance)ipIterator.next();
      FactInvRole factInverse = new FactInvRole(inverse.getName());
      itsFact.equalR(factRole, factInverse);
    }
    // domain
    Collection domains = 
      frames.getOwnSlotValues(oilProperty, DOMAIN);
    for (Iterator dIterator = domains.iterator();
	 dIterator.hasNext();) {
      Instance domain = (Instance)dIterator.next();
      exportDomain(factRole, domain);
    }
    // range & cardinalities
    // 1. use range definitions from Protege!!!
    FactConcept range;
    ValueType valueType = oilProperty.getValueType();
    if (valueType.equals(ValueType.INSTANCE)) {
      Collection allowedClses = oilProperty.getAllowedClses();
      Collection factConcepts = getFactConcepts(allowedClses);
      if (factConcepts.isEmpty())
	range = Fact.BOTTOM;
      else if (factConcepts.size() == 1)
	range = (FactConcept)factConcepts.iterator().next();
      else // implicit or in Protege!!
	range = new FactOrConcept(factConcepts);
    } else {
      String rangeName = valueType.toString(); // ...
      report("warning: using (undefined) class " + rangeName +
	" as range for global property " + oilProperty);
      range = new FactPrimitiveConcept(rangeName);
    }
    exportRange(factRole, range);
    // cardinalities
    int min = oilProperty.getMinimumCardinality();
    int max = oilProperty.getMaximumCardinality();
    exportCardinalities(factRole, min, max);
    // 2. use range definitions from Oil
    Collection ranges = 
      frames.getOwnSlotValues(oilProperty, RANGE);
    for (Iterator rIterator = ranges.iterator();
	 rIterator.hasNext();) {
      Instance oilRange = (Instance)rIterator.next();
      exportRange(factRole, oilRange);
    }
    // properties (transitive, symmetric, functional)
    Collection properties = 
      frames.getOwnSlotValues(oilProperty, SLOTPROPERTIES);
    for (Iterator pIterator = properties.iterator();
	 pIterator.hasNext();) {
      String prop = (String)pIterator.next();
      if (prop.equals(TRANSITIVE)) {
	itsFact.transitive(factRole);
      } else if (prop.equals(SYMMETRIC)) {
	// (equalR role (inverse role))
	FactInvRole inverseRole = new FactInvRole(factRole);
	itsFact.equalR(factRole, inverseRole);
      } else if (prop.equals(FUNCTIONAL)) {
	itsFact.functional(factRole);
      } else
	report("unknown slot property: " + prop);
    }
  }

  void exportDomain(FactPrimitiveRole role, Instance domainExpression) {
    // (impliesC (some role TOP) domainConcept)
    FactSomeConcept some = new FactSomeConcept(role, Fact.TOP);
    FactConcept domainConcept = getFactConcept(domainExpression);
    itsFact.impliesC(some, domainConcept);
  }

  void exportRange(FactPrimitiveRole role, Instance rangeExpression) {
    // (impliesC TOP (all role rangeConcept))
    FactConcept rangeConcept = getFactConcept(rangeExpression);
    exportRange(role, rangeConcept);
  }

  void exportRange(FactPrimitiveRole role, FactConcept range) {
    // (impliesC TOP (all role range))
    if (!range.equals(Fact.TOP)) {
      FactAllConcept all = new FactAllConcept(role, range);
      itsFact.impliesC(Fact.TOP, all);
    }
  }

  void exportCardinalities(FactPrimitiveRole role, int min, int max) {
    // (impliesC TOP (atleast min role TOP))
    // (impliesC TOP (atmost max role TOP))
    if (min > 0) {
      report("global minimum cardinality for " + role.getName() + " ignored");
      /* we must use the domain instead of TOP !!
      FactAtleastConcept atleast = new FactAtleastConcept(min, role, Fact.TOP);
      itsFact.impliesC(Fact.TOP, atleast);
      */
    }
    if (max > 0) {
      report("warning: global maximum cardinality for " + role.getName());
      FactAtmostConcept atmost = new FactAtmostConcept(max, role, Fact.TOP);
      itsFact.impliesC(Fact.TOP, atmost);
    }
  }


  // export classes

  void exportOilClasses() {
    Collection oilClasses = frames.getInstances(CLASS);
    for (Iterator ocIterator = oilClasses.iterator();
         ocIterator.hasNext();) {
      Cls oilClass = (Cls)ocIterator.next();
      exportOilClass(oilClass);
    }
  }

  void exportOilClass(Cls oilClass) {
    String className = oilClass.getName();
    /* we use type instead
    boolean defined = 
      frames.equals(oilClass.getDirectType(), DEFINEDCLASS);
    */
    String type = (String)frames.getOwnSlotValue(oilClass, TYPE);
    boolean defined = (type != null && type.equals("defined"));

    Vector factConcepts = new Vector(); // these become one big AND

    // 1. superclasses from Protege
    Vector superClasses =
      new Vector(frames.getOwnSlotValues(oilClass, DIRECTSUPERCLASSES));
    superClasses.remove(frames.getCls(COLONTHING));
    factConcepts.addAll(getFactConcepts(superClasses));

    // 2. superclasses from OIL expressions
    Collection superClassExpressions =
      frames.getOwnSlotValues(oilClass, SUBCLASSOF);
    factConcepts.addAll(getFactConcepts(superClassExpressions));

    // 3. template slots from Protege
    //Collection templateSlots = oilClass.getDirectTemplateSlots();
    Vector templateSlots = new Vector(oilClass.getDirectTemplateSlots());
    // find directly overriden template slots
    Collection allTemplateSlots = oilClass.getTemplateSlots();
    for (Iterator atsIterator = allTemplateSlots.iterator();
	 atsIterator.hasNext();) {
      Slot ts = (Slot)atsIterator.next();
      if (oilClass.hasDirectlyOverriddenTemplateSlot(ts)
	  && !templateSlots.contains(ts))
	templateSlots.add(ts);
    }

    for (Iterator tsIterator = templateSlots.iterator();
	 tsIterator.hasNext();) {
      Slot templateSlot = (Slot)tsIterator.next();
      FactPrimitiveRole role = new FactPrimitiveRole(templateSlot.getName());
      // Cls tsType = templateSlot.getDirectType(); ... TODO
      ValueType valueType = oilClass.getTemplateSlotValueType(templateSlot);
      FactConcept range;
      if (valueType == ValueType.INSTANCE) {
	// allowed classes:
	Collection allowedClses = 
	  oilClass.getTemplateSlotAllowedClses(templateSlot);
	Collection acFactConcepts = getFactConcepts(allowedClses);
	if (acFactConcepts.isEmpty())
	  range = Fact.BOTTOM;
	else if (acFactConcepts.size() == 1)
	  range = (FactConcept)acFactConcepts.iterator().next();
	else
	  range = new FactOrConcept(acFactConcepts);
      } else {
	String rangeName = valueType.toString();
	report("warning: using undefined class " + rangeName + 
	  " as range for template slot " + templateSlot + " on class " +
	  oilClass);
	range = new FactPrimitiveConcept(rangeName);
      }
      // (all role (or allowedClses)) or (all role class)
      FactAllConcept allConcept = new FactAllConcept(role, range);
      factConcepts.add(allConcept);
      // cardinalities:
      int min = oilClass.getTemplateSlotMinimumCardinality(templateSlot);
      if (min > 0) { // (atleast min role T)
        FactAtleastConcept atleastConcept =
	  new FactAtleastConcept(min, role, Fact.TOP);
        factConcepts.add(atleastConcept);
      }
      int max = oilClass.getTemplateSlotMaximumCardinality(templateSlot);
      if (max > 0 ) { // (atmost max role T)
	FactAtmostConcept atmostConcept =
	  new FactAtmostConcept(max, role, Fact.TOP);
	factConcepts.add(atmostConcept);
      }
    }

    // 4. property restrictions from OIL
    Collection propertyRestrictions =
      frames.getOwnSlotValues(oilClass, HASPROPERTYRESTRICTION);
    factConcepts.addAll(getFactConcepts(propertyRestrictions));

    // export
    if (frames.equals(oilClass, TOP) || frames.equals(oilClass, BOTTOM)) {
      // special treatment???
      // do nothing for right now--FaCT knows these classes ... !!!
    } else if (factConcepts.isEmpty())
      itsFact.defconcept(className);
    else {
      itsFact.defconcept(className); // not really needed
      FactConcept concept;
      if (factConcepts.size() == 1)
	concept = (FactConcept)factConcepts.iterator().next();
      else
	concept = new FactAndConcept(factConcepts);
      if (defined)
        itsFact.equalC(className, concept);
      else // primitive
        itsFact.impliesC(className, concept);
    }

  }


  // export axioms

  void exportOilAxioms() {
    if (frames.getCls(AXIOM) != null) {
      exportDisjointAxioms();
      exportEquivalenceAxioms();
      exportCoverAxioms();
      // disjoint cover axioms handled by disjoint and cover!
    }
  }

  void exportDisjointAxioms()  {
    Vector disjointAxioms = new Vector(frames.getInstances(DISJOINT));
    disjointAxioms.addAll(frames.getInstances(DISJOINTCOVER));
    for (Iterator daIterator = disjointAxioms.iterator();
	 daIterator.hasNext();) {
      Instance disjointAxiom = (Instance)daIterator.next();
      Vector concepts = 
	getFactConcepts(frames.getOwnSlotValues(disjointAxiom, HASOBJECT));
      int l = concepts.size();
      if (l < 2) { // 1 for DISJOINTCOVER (but who would it use then) !!!
	report("the disjoint axiom " + disjointAxiom 
	  + " has less than two class expressions");
      } else {
	for (int i = 0; i < l-1; i++) {
	  FactConcept c1 = (FactConcept)concepts.elementAt(i);
	  for (int j = i+1; j < l; j++) {
	    FactConcept c2 = (FactConcept)concepts.elementAt(j);
	    // (impliesC c1 (not c2))
	    FactConcept notC2Concept = new FactNotConcept(c2);
	    itsFact.impliesC(c1, notC2Concept);
	  }
	}
      }
    }
  }

  void exportEquivalenceAxioms() {
    Collection equivalenceAxioms = frames.getInstances(EQUIVALENCE);
    for (Iterator eaIterator = equivalenceAxioms.iterator();
	 eaIterator.hasNext();) {
      Instance equivalenceAxiom = (Instance)eaIterator.next();
      Vector concepts = 
	getFactConcepts(frames.getOwnSlotValues(equivalenceAxiom, HASOBJECT));
      int l = concepts.size();
      if (l < 2) {
	report("the equivalence axiom " + equivalenceAxiom 
	  + " has less than two class expressions");
      } else {
	for (int i = 0; i < l-1; i++) {
	  FactConcept c1 = (FactConcept)concepts.elementAt(i);
	  FactConcept c2 = (FactConcept)concepts.elementAt(i+1);
	  // (equalC c1 c2)
	  itsFact.equalC(c1, c2);
	}
      }
    }
  }

  void exportCoverAxioms() {
    Vector coverAxioms = new Vector(frames.getInstances(COVER));
    coverAxioms.addAll(frames.getInstances(DISJOINTCOVER));
    for (Iterator caIterator = coverAxioms.iterator();
	 caIterator.hasNext();) {
      Instance coverAxiom = (Instance)caIterator.next();
      Instance subjectExpression = (Instance)frames
	.getOwnSlotRequiredValue(coverAxiom, HASSUBJECT);
      if (subjectExpression != null) {
        FactConcept subject = getFactConcept(subjectExpression);
        Collection concepts = 
	  getFactConcepts(
	    frames.getOwnSlotValuesNotEmpty(coverAxiom, HASOBJECT));
	// (impliesC subject (or c1 ... cN))
	if (concepts.size() == 1)
	  itsFact.impliesC(subject, (FactConcept)concepts.iterator().next());
	else if (concepts.size() > 1)
	  itsFact.impliesC(subject, new FactOrConcept(concepts));
      }
    }
  }


  // getting fact concepts (FactConcept) from Protege expressions

  Vector getFactConcepts(Collection oilClassExpressions) {
    Vector factConcepts = new Vector();
    for (Iterator iterator = oilClassExpressions.iterator();
	 iterator.hasNext();) {
      factConcepts.add(getFactConcept((Instance)iterator.next()));
    }
    return factConcepts;
  }

  FactConcept getFactConcept(Instance oilClassExpression) {
    // System.out.println("getFactConcept: " + oilClassExpression);
    Cls type = oilClassExpression.getDirectType();
    /* we use only CLASS (with TYPE)
    if (frames.equals(type, DEFINEDCLASS)
	|| frames.equals(type, PRIMITIVECLASS)) {
    */
    if (frames.equals(type, CLASS)) {

      if (frames.equals((Cls)oilClassExpression, TOP))
	return Fact.TOP;
      else if (frames.equals((Cls)oilClassExpression, BOTTOM))
	return Fact.BOTTOM;
      else
        return new FactPrimitiveConcept(oilClassExpression.getName());

    } else if (frames.equals(type, AND)) {

      Collection subOilClassExpressions =
	frames.getOwnSlotValues(oilClassExpression, HASOPERAND);
      return new FactAndConcept(getFactConcepts(subOilClassExpressions));

    } else if (frames.equals(type, OR)) {

      Collection subOilClassExpressions =
	frames.getOwnSlotValues(oilClassExpression, HASOPERAND);
      return new FactOrConcept(getFactConcepts(subOilClassExpressions));

    } else if (frames.equals(type, NOT)) {

      Instance subOilClassExpression =
	(Instance)frames.getOwnSlotRequiredValue(oilClassExpression, 
	  HASOPERAND, frames.getCls(TOP));
      return new FactNotConcept(getFactConcept(subOilClassExpression));

    } else if (frames.equals(type, VALUETYPE) 
	       || frames.equals(type, HASVALUE)) {

      // build conjunction of (all R C) or (some R C)
      boolean some = frames.equals(type, HASVALUE); // all or some mode
      String propertyName = 
	frames.getOwnSlotRequiredValueName(oilClassExpression, 
	  ONPROPERTY, "error");
      FactPrimitiveRole role = new FactPrimitiveRole(propertyName);
      Collection toClasses = 
	frames.getOwnSlotValuesNotEmpty(oilClassExpression, TOCLASS);
      Collection factConcepts = getFactConcepts(toClasses);
      Vector allSomeRCList = new Vector();
      for (Iterator cIterator = factConcepts.iterator();
	   cIterator.hasNext();) {
	FactConcept someAllConcept;
	if (some)
	  someAllConcept = 
	    new FactSomeConcept(role, (FactConcept)cIterator.next());
	else
	  someAllConcept = 
	    new FactAllConcept(role, (FactConcept)cIterator.next());
	allSomeRCList.add(someAllConcept);
      }
      if (allSomeRCList.size() > 1)
        return new FactAndConcept(allSomeRCList);
      else if (allSomeRCList.size() == 1)
	return (FactConcept)allSomeRCList.iterator().next();
      else
	return Fact.BOTTOM;

    } else if (frames.equals(type, MAXCARDINALITY) || 
	       frames.equals(type, MINCARDINALITY) || 
	       frames.equals(type, CARDINALITY)) {

      String propertyName = 
	frames.getOwnSlotRequiredValueName(
	  oilClassExpression, ONPROPERTY, "error");
      FactPrimitiveRole role = new FactPrimitiveRole(propertyName);
      Instance toClass = 
	(Instance)frames.getOwnSlotRequiredValue(
	  oilClassExpression, TOCLASS, frames.getCls(TOP));
      FactConcept concept = getFactConcept(toClass);
      int num = 
	frames.getOwnSlotRequiredIntValue(oilClassExpression, NUMBER, 1);
      if (frames.equals(type, MINCARDINALITY))
	return new FactAtleastConcept(num, role, concept);
      else if (frames.equals(type, MAXCARDINALITY))
        return new FactAtmostConcept(num, role, concept);
      else { // min&max
	Vector concepts = new Vector();
	concepts.add(new FactAtleastConcept(num, role, concept));
	concepts.add(new FactAtmostConcept(num, role, concept));
	return new FactAndConcept(concepts);
      }

    } else { // ... (other expressions) !!!

      report("class expression not handled: " + oilClassExpression);
      return Fact.TOP;

    }
  }


  // view tells (XML document)

  void viewXML() {
    if (itsTells != null) {
      JFrame frame = new JFrame("classifier tells");
      JTextArea textArea = new JTextArea(itsTells, 30, 80);
      textArea.setCaretPosition(0);
      textArea.setEditable(false);
      frame.getContentPane().add(new JScrollPane(textArea));
      frame.pack();
      frame.show();
    }
  }


  // view SHIQ kb via LaTeX/acroread

  void viewSHIQ() {
    if (itsPDFLatexCommand != null && itsAcroreadCommand != null) {
      try {
        String[] command1 =
	  new String[] {itsPDFLatexCommand, "--interaction=batchmode",
	    itsLatexFileName};
        String[] command2 =
	  new String[] {itsAcroreadCommand, itsLatexFileName + ".pdf"};
	if (Runtime.getRuntime().exec(command1).waitFor() == 0)
	  Runtime.getRuntime().exec(command2);
	else
	  System.out.println("pdflatex failed; possible cause: oil.latex.pdflatex not correct or the pdf file is locked by acro reader");
      } catch (Exception e) {
	System.out.println(e);
      }
    }
  }


  // messages (errors, warnings etc.)

  void report(String string) {
    itsMessageTextArea.append(string);
    itsMessageTextArea.append("\n");
  }


}


