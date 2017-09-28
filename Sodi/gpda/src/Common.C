
typedef struct LEXICAL {
  float         lower;
  float         upper;
  char          desc[16];
};

/* --------------------------------------------------------------------- */

int             LexCount = 9;
LEXICAL         LexTable[9] =  { { 0.00,  0.01,  "Impossible"  },
                                 { 0.01,  0.05,  "V.Unlikely" },
                                 { 0.05,  0.20,  "Low Chance"  },
                                 { 0.20,  0.38,  "Sm. Chance"  },
                                 { 0.38,  0.60,  "May"         },
                                 { 0.60,  0.79,  "Some Chance" },
                                 { 0.79,  0.95,  "Good Chance" },
                                 { 0.95,  0.99,  "Very Likely" },
                                 { 0.99,  1.01,  "Certain"     } };

void Alarm(int alarm, char *label, int priority, char *arg)
{
}

extern "C" {

void SC_update_def(int newdef)
{
}

void SC_update_rp(int newrp)
{
}

void SC_update_roe(char *newroe)
{
}

void SC_update_dea(char *newdea)
{
}

void SC_update_plan(char *str)
{
}

}
