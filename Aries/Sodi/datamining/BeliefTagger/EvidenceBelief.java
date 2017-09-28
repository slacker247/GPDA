/*
	does the translation between nominal and numerical belief
		as well as holds the users changes of belief based on the reporters questions
*/
class EvidenceBelief {

	protected int evidence;
	protected int belief;

	protected static final int NONE				= 0;	 
	protected static final int IMPOSSIBLE		= 1;
	protected static final int VERY_UNLIKELY	= 2;
	protected static final int LOW_CHANCE		= 3;
	protected static final int SMALL_CHANCE		= 4;
	protected static final int MAY				= 5;
	protected static final int SOME_CHANCE		= 6;
	protected static final int GOOD_CHANCE		= 7;
	protected static final int VERY_LIKELY		= 8;
	protected static final int CERTAIN			= 9;

	protected static final double[] beliefDoubles = {
	   -1.0,	// -1.0	 none
		0.00,	// 0.00  impossible
		0.01,	// 0.05  extremely_unlikely
		0.05,	// 0.20  very_low_chance
		0.20,	// 0.38  small_chance
		0.38,	// 0.60  may
		0.60,	// 0.79  meaningful_chance
		0.79,	// 0.95  most_likely
		0.95,	// 0.99  extremely_likely
		1.00,	// 1.00  certain
	};
	


	protected static final String[] beliefs = {
		"?", "Impossible", "Extremely_Unlikely", "Very_Low_Chance", "Small_Chance", "May", "Meaningful_Chance", 
			"Most_Likely", "Extremely_Likely", "Certain" };
			
	
	public EvidenceBelief (int evidence, int belief) {
		this.evidence = evidence;
		this.belief = belief;
	}

	public EvidenceBelief (int evidence, String belief) {
		this.evidence = evidence;
		if (belief.compareTo("?") == 0) setBelief(NONE);
		else {
			setBelief( Double.parseDouble(belief) );
		}
	}

	public EvidenceBelief (int evidence, double belief) {
		this.evidence = evidence;
		System.out.println("aDouble: " + belief);
		setBelief(belief);
	}

	public void setEvidence(int evidence)	{ this.evidence = evidence; }
	public void setBelief  (int belief)		{ this.belief = belief; }
	public void setBelief  (double belief)  { 
		//have to find out which int to pass on
		for (int i = NONE; i < CERTAIN; i++) {
			if (belief < beliefDoubles[i+1]) { setBelief(i); return; }
		}
		setBelief(CERTAIN);
		return;
	}
	public void setBelief (String beliefName) {
		for (int i = 0; i <= CERTAIN; i++) 
			if (beliefs[i].compareTo(beliefName) == 0) {
				setBelief(i);
				return;
			}

		setBelief(0);
	}


	public int getEvidence()				{ return evidence; }
	public int getBelief()					{ return belief; }

	public static int getBeliefInt (double belief) {
		for (int i=0; i < CERTAIN; i++) 
			if (belief < beliefDoubles[i+1]) return i;
		return CERTAIN;
	}

	public static String getBeliefString(double belief) {
		for (int i=0; i < CERTAIN; i++) 
			if (belief < beliefDoubles[i+1]) return beliefs[i];
		return beliefs[CERTAIN];
	}

	
	public String getStringBelief()			{ return beliefs[belief]; }

	public String getDBValue()				{ 
		if (beliefDoubles[belief] == -1) return "?";
		
		return new Double(beliefDoubles[belief]).toString(); 
	}

	public static String[] getBeliefs()		{ return beliefs; }

	public static double getBelief(String beliefValue) {
		for (int i = 0; i <= CERTAIN; i++) 
			if (beliefs[i].compareTo(beliefValue) == 0)
				return beliefDoubles[i];

		return 0;
	}

	public String toString()				{ return "Evidence: " + getEvidence() + " Belief: " + getStringBelief() + " (" + getBelief() + ")"; }



}
