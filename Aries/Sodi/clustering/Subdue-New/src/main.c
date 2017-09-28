//---------------------------------------------------------------------------
// main.c
//
// Subdue 5
//---------------------------------------------------------------------------

#include "subdue.h"
#include "time.h"

// Function prototypes

int main (int, char **);
Parameters *GetParameters (int, char **);
void PrintParameters (Parameters *);
void FreeParameters (Parameters *);


//---------------------------------------------------------------------------
// NAME:    main
//
// INPUTS:  (int argc) - number of arguments to program
//          (char **argv) - array of strings of arguments to program
//
// RETURN:  (int) - 0 if all is well
//
// PURPOSE: Main Subdue function that processes command-line arguments
// and initiates discovery.
//---------------------------------------------------------------------------

int main (int argc, char **argv)
{
  time_t startTime;
  time_t endTime;
  time_t iterationStartTime;
  time_t iterationEndTime;
  SubList *subList;
  Parameters *parameters;
  FILE *outputFile;
  ULONG iteration;
  BOOLEAN done;

  startTime = time (NULL);
  printf ("Subdue %s\n\n", SUBDUE_VERSION);
  parameters = GetParameters (argc, argv);
  PrintParameters (parameters);

  // compress pos and neg graphs with predefined subs, if given
  if (parameters->numPreSubs > 0)
    CompressWithPredefinedSubs (parameters);

  if (parameters->iterations == 0)
    parameters->iterations = MAX_UNSIGNED_LONG; // infinity

  if (parameters->iterations > 1)
    printf ("----- Iteration 1 -----\n\n");

  iteration = 1;
  done = FALSE;
  while ((iteration <= parameters->iterations) && (! done)) {
    iterationStartTime = time (NULL);
    if (iteration > 1)
      printf ("----- Iteration %lu -----\n\n", iteration);
    printf ("%lu positive graphs: %lu vertices, %lu edges, %.0f bits\n",
	    parameters->numPosEgs,
	    parameters->posGraph->numVertices,
	    parameters->posGraph->numEdges, parameters->posGraphDL);
    if (parameters->negGraph != NULL) {
      printf ("%lu negative graphs: %lu vertices, %lu edges, %.0f bits\n",
	      parameters->numNegEgs,
	      parameters->negGraph->numVertices,
	      parameters->negGraph->numEdges, parameters->negGraphDL);
    }
    printf ("\n");

    subList = DiscoverSubs (parameters);

    if (subList->head == NULL) {
      done = TRUE;
      printf ("No substructures found.\n\n");
    } else {
      // write output to stdout
      if (parameters->outputLevel > 1) {
	printf ("\nBest %lu substructures:\n\n", parameters->numBestSubs);
	PrintSubList (subList, parameters);
      } else {
	printf ("\nBest substructure:\n\n");
	PrintSub (subList->head->sub, parameters);
      }

      // write machine-readable output to file, if given
      if (parameters->outputToFile) {
	outputFile = fopen (parameters->outFileName, "a");
	if (outputFile == NULL) {
	  printf ("WARNING: unable to write to output file %s, disabling\n",
		  parameters->outFileName);
	  parameters->outputToFile = FALSE;
	}
	WriteGraphToFile (outputFile, subList->head->sub->definition,
			  parameters->labelList);
	
	fclose (outputFile);
      }

      if (iteration < parameters->iterations) { // another iteration?

	if (parameters->evalMethod == EVAL_SETCOVER) {
	  printf ("Removing positive examples covered by");
	  printf (" best substructure.\n\n");
	  RemovePosEgsCovered (subList->head->sub, parameters);
	} else {
	  printf ("Compressing graph with best substructure.\n\n");
	  CompressFinalGraphs (subList->head->sub, parameters, iteration,
			       FALSE);
	}

	// check for stopping condition
	// if set-covering, then no more positive examples
	// if MDL or size, then positive graph contains no edges
	if (parameters->evalMethod == EVAL_SETCOVER) {
	  if (parameters->numPosEgs == 0) {
	    done = TRUE;
	    printf ("Ending iterations - all positive examples covered.\n\n");
	  }
	} else {
	  if (parameters->posGraph->numEdges == 0) {
	    done = TRUE;
	    printf ("Ending iterations - graph fully compressed.\n\n");
	  }
	}
      }
    }
    FreeSubList (subList);
    if (parameters->iterations > 1) {
      iterationEndTime = time (NULL);
      printf ("Elapsed time for iteration %lu = %lu seconds.\n\n",
	      iteration, (iterationEndTime - iterationStartTime));
    }
    iteration++;
  }
  
  FreeParameters (parameters);
  endTime = time (NULL);
  printf ("\nSubdue done (elapsed time = %lu seconds).\n",
	  (endTime - startTime));
  return 0;
}


//---------------------------------------------------------------------------
// NAME: GetParameters
//
// INPUTS: (int argc) - number of command-line arguments
//         (char *argv[]) - array of command-line argument strings
//
// RETURN: (Parameters *)
//
// PURPOSE: Initialize parameters structure and process command-line
// options.
//---------------------------------------------------------------------------

Parameters *GetParameters (int argc, char *argv[])
{
  Parameters *parameters;
  int i;
  double doubleArg;
  ULONG ulongArg;
  BOOLEAN limitSet = FALSE;
  FILE *outputFile;

  parameters = (Parameters *) malloc (sizeof (Parameters));
  if (parameters == NULL)
    OutOfMemoryError ("parameters");

  // initialize default parameter settings
  parameters->directed = TRUE;
  parameters->limit = 0;
  parameters->numBestSubs = 3;
  parameters->beamWidth = 4;
  parameters->valueBased = FALSE;
  parameters->prune = FALSE;
  strcpy (parameters->outFileName, "none");
  parameters->outputToFile = FALSE;
  parameters->outputLevel = 2;
  parameters->allowInstanceOverlap = FALSE;
  parameters->threshold = 0.0;
  parameters->evalMethod = EVAL_MDL;
  parameters->iterations = 1;
  strcpy (parameters->psInputFileName, "none");
  parameters->predefinedSubs = FALSE;
  parameters->minVertices = 1;
  parameters->maxVertices = 0; // i.e., infinity

  // process command-line options
  i = 1;
  while (i < (argc - 1)) {
    if (strcmp (argv[i], "-beam") == 0) {
      i++;
      sscanf (argv[i], "%lu", &ulongArg);
      if (ulongArg == 0) {
	fprintf (stderr, "%s: beam must be greater than zero\n", argv[0]);
	exit (1);
      }
      parameters->beamWidth = ulongArg;
    } else if (strcmp (argv[i], "-eval") == 0) {
      i++;
      sscanf (argv[i], "%lu", &ulongArg);
      if ((ulongArg < 1) || (ulongArg > 3)) {
	fprintf (stderr, "%s: eval must be 1-3\n", argv[0]);
	exit (1);
      }
      parameters->evalMethod = ulongArg;
    } else if (strcmp (argv[i], "-iterations") == 0) {
      i++;
      sscanf (argv[i], "%lu", &ulongArg);
      parameters->iterations = ulongArg;
    } else if (strcmp (argv[i], "-limit") == 0) {
      i++;
      sscanf (argv[i], "%lu", &ulongArg);
      if (ulongArg == 0) {
	fprintf (stderr, "%s: limit must be greater than zero\n", argv[0]);
	exit (1);
      }
      parameters->limit = ulongArg;
      limitSet = TRUE;
    } else if (strcmp (argv[i], "-maxsize") == 0) {
      i++;
      sscanf (argv[i], "%lu", &ulongArg);
      if (ulongArg == 0) {
	fprintf (stderr, "%s: maxsize must be greater than zero\n", argv[0]);
	exit (1);
      }
      parameters->maxVertices = ulongArg;
    } else if (strcmp (argv[i], "-minsize") == 0) {
      i++;
      sscanf (argv[i], "%lu", &ulongArg);
      if (ulongArg == 0) {
	fprintf (stderr, "%s: minsize must be greater than zero\n", argv[0]);
	exit (1);
      }
      parameters->minVertices = ulongArg;
    } else if (strcmp (argv[i], "-nsubs") == 0) {
      i++;
      sscanf (argv[i], "%lu", &ulongArg);
      if (ulongArg == 0) {
	fprintf (stderr, "%s: nsubs must be greater than zero\n", argv[0]);
	exit (1);
      }
      parameters->numBestSubs = ulongArg;
    } else if (strcmp (argv[i], "-out") == 0) {
      i++;
      strcpy (parameters->outFileName, argv[i]);
      parameters->outputToFile = TRUE;
    } else if (strcmp (argv[i], "-output") == 0) {
      i++;
      sscanf (argv[i], "%lu", &ulongArg);
      if ((ulongArg < 1) || (ulongArg > 5)) {
	fprintf (stderr, "%s: output must be 1-5\n", argv[0]);
	exit (1);
      }
      parameters->outputLevel = ulongArg;
    } else if (strcmp (argv[i], "-overlap") == 0) {
      parameters->allowInstanceOverlap = TRUE;
    } else if (strcmp (argv[i], "-prune") == 0) {
      parameters->prune = TRUE;
    } else if (strcmp (argv[i], "-ps") == 0) {
      i++;
      strcpy (parameters->psInputFileName, argv[i]);
      parameters->predefinedSubs = TRUE;
    } else if (strcmp (argv[i], "-threshold") == 0) {
      i++;
      sscanf (argv[i], "%lf", &doubleArg);
      if ((doubleArg < 0.0) || (doubleArg > 1.0)) {
	fprintf (stderr, "%s: threshold must be 0.0-1.0\n", argv[0]);
	exit (1);
      }
      parameters->threshold = doubleArg;
    } else if (strcmp (argv[i], "-undirected") == 0) {
      parameters->directed = FALSE;
    } else if (strcmp (argv[i], "-valuebased") == 0) {
      parameters->valueBased = TRUE;
    } else {
      fprintf (stderr, "%s: unknown option %s\n", argv[0], argv[i]);
      exit (1);
    }
    i++;
  }

  // initialize log2Factorial[0..1]
  parameters->log2Factorial = (double *) malloc (2 * sizeof(double));
  if (parameters->log2Factorial == NULL)
    OutOfMemoryError ("GetParameters:parameters->log2Factorial");
  parameters->log2FactorialSize = 2;
  parameters->log2Factorial[0] = 0; // lg(0!)
  parameters->log2Factorial[1] = 0; // lg(1!)

  // read graphs from input file
  strcpy(parameters->inputFileName, argv[argc - 1]);
  parameters->labelList = AllocateLabelList ();
  parameters->posGraph = NULL;
  parameters->negGraph = NULL;
  parameters->numPosEgs = 0;
  parameters->numNegEgs = 0;
  parameters->posEgsVertexIndices = NULL;
  parameters->negEgsVertexIndices = NULL;
  ReadInputFile (parameters);
  if (parameters->numPosEgs == 0) {
    fprintf (stderr, "ERROR: no positive graphs defined\n");
    exit (1);
  }
  // check bounds on discovered substructures' number of vertices
  if (parameters->maxVertices == 0)
    parameters->maxVertices = parameters->posGraph->numVertices;
  if (parameters->maxVertices < parameters->minVertices) {
    fprintf (stderr, "ERROR: minsize exceeds maxsize\n");
    exit (1);
  }

  // read predefined substructures
  parameters->numPreSubs = 0;
  if (parameters->predefinedSubs)
    ReadPredefinedSubsFile (parameters);

  parameters->posGraphDL = MDL (parameters->posGraph,
				parameters->labelList->numLabels,
				parameters);
  if (parameters->negGraph != NULL) {
    parameters->negGraphDL = MDL (parameters->negGraph,
				  parameters->labelList->numLabels,
				  parameters);
  }
  
  // set limit accordingly
  if (! limitSet)
    parameters->limit = GraphSize (parameters->posGraph) / 2;

  // create output file, if given
  if (parameters->outputToFile) {
    outputFile = fopen (parameters->outFileName, "w");
    if (outputFile == NULL) {
      printf ("ERROR: unable to write to output file %s\n",
	      parameters->outFileName);
      exit (1);
    }
    fclose (outputFile);
  }  

  return parameters;
}


//---------------------------------------------------------------------------
// NAME: PrintParameters
//
// INPUTS: (Parameters *parameters)
//
// RETURN: (void)
//
// PURPOSE: Print selected parameters.
//---------------------------------------------------------------------------

void PrintParameters (Parameters *parameters)
{
  printf ("Parameters:\n");
  printf ("  Input file..................... %s\n", parameters->inputFileName);
  printf ("  Predefined substructure file... %s\n",
	  parameters->psInputFileName);
  printf ("  Output file.................... %s\n", parameters->outFileName);
  printf ("  Beam width..................... %lu\n", parameters->beamWidth);
  printf ("  Evaluation method.............. ");
  switch (parameters->evalMethod) {
  case 1: printf ("MDL\n"); break;
  case 2: printf ("size\n"); break;
  case 3: printf ("setcover\n"); break;
  }
  printf ("  'e' edges directed............. ");
  PrintBoolean (parameters->directed);
  printf ("  Iterations..................... ");
  if (parameters->iterations == 0)
    printf ("infinite\n");
  else printf ("%lu\n", parameters->iterations);
  printf ("  Limit.......................... %lu\n", parameters->limit);
  printf ("  Minimum size of substructures.. %lu\n", parameters->minVertices);
  printf ("  Maximum size of substructures.. %lu\n", parameters->maxVertices);
  printf ("  Number of best substructures... %lu\n", parameters->numBestSubs);
  printf ("  Output level................... %lu\n", parameters->outputLevel);
  printf ("  Allow overlapping instances.... ");
  PrintBoolean (parameters->allowInstanceOverlap);
  printf ("  Prune.......................... ");
  PrintBoolean (parameters->prune);
  printf ("  Threshold...................... %lf\n", parameters->threshold);
  printf ("  Value-based queue.............. ");
  PrintBoolean (parameters->valueBased);
  printf ("\n");

  printf ("Read %lu positive graphs\n", parameters->numPosEgs);
  if (parameters->numNegEgs > 0) {
    printf ("Read %lu negative graphs\n", parameters->numNegEgs);
  }
  if (parameters->numPreSubs > 0)
    printf ("Read %lu predefined substructures\n", parameters->numPreSubs);
  printf ("\n");
}


//---------------------------------------------------------------------------
// NAME: FreeParameters
//
// INPUTS: (Parameters *parameters)
//
// RETURN: (void)
//
// PURPOSE: Free memory allocated for parameters.  Note that the
// predefined substructures are de-allocated as soon as they are
// processed, and not here.
//---------------------------------------------------------------------------

void FreeParameters (Parameters *parameters)
{
  FreeGraph (parameters->posGraph);
  FreeGraph (parameters->negGraph);
  FreeLabelList (parameters->labelList);
  free (parameters->posEgsVertexIndices);
  free (parameters->negEgsVertexIndices);
  free (parameters->log2Factorial);
  free (parameters);
}
