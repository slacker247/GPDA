///////////////////////////////////////////////////////////////
// Table to Graph converter
//
// for SUBDUE
//
// by Istvan Jonyer         jonyer@cse.uta.edu
///////////////////////////////////////////////////////////////
// File format:       | Example:
//                    |
// <item name>        | book
// <list of features> | title author  ISBN   category
// <list of items>    | LGM   King    I-345  thriller
// ...                | JFK   Costner I-698  documentary
///////////////////////////////////////////////////////////////

#include <stdio.h>
#include <iostream.h>
#include <fstream.h>
//#include <strstrea.h>
#include <string.h>

////////////////////
// defines
#define VERTEX						 "v"
#define EDGE						 "e"
#define GRAPH_FILE_EXT				".g"
#define SEPARATORS					 " \t"
#define MAX_COLUMNS			1000
#define WORD_SIZE			  50
#define LINE_SIZE		   55000
#define FN_LEN				 256

///////////////////
// Function prototypes
void Tokenize(char *str, char *separators, char *tokens[], int &tokenCount);

///////////////////
// Function declarations
int main(int argc, char *argv[])
{
	char graphFileName[FN_LEN];
	char edgeFileName[FN_LEN];
	char line[LINE_SIZE];
	char *columns[MAX_COLUMNS];
	int	 columnCount;
	char generalItemName[WORD_SIZE];
	char featureList[LINE_SIZE];
	char *features[MAX_COLUMNS];
	int  featureCount;
	char itemProperty[WORD_SIZE];
	int  itemVertexNumber;
	int  itemPropertyVertexNumber;
	int  discrepancy;
	int  lineCount;

	if (argc < 2 || argc > 3) {
		cout << "Usage" << endl
			 << "table2graph <table file> [<graph file>]" << endl;
		return 1;
	}

	if (argc == 2) {				// if graph file name is not specified
		strcpy(graphFileName, argv[1]);		// get table file name for it
		strcat(graphFileName, GRAPH_FILE_EXT);	// and append it with .graph
	} else {
		strcpy(graphFileName, argv[2]);
	}
	strcpy(edgeFileName, graphFileName);		// make edge file name
	strcat(edgeFileName, ".edge");

	// opening files
	ifstream tableFile(argv[1], ios::in | ios::nocreate);	// opening table file
	if (!tableFile.is_open()) {
		cout << "Error opening table file " << argv[1] << endl;
		return 1;
	}

	ofstream graphFile(graphFileName, ios::out);		// opening graph file
	if (!graphFile.is_open()) {									// this is vertex first
		cout << "Error opening graph file " << graphFileName << endl;
		return 1;
	}

	fstream edgeFile(edgeFileName, ios::out|ios::in);	// opening graph file
	if (!edgeFile.is_open()) {
		cout << "Error opening temporary edge file " << edgeFileName << endl;
		return 1;
	}

	// build graph
	tableFile >> generalItemName;					// get general item name
	tableFile.getline(featureList, LINE_SIZE);
	tableFile.getline(featureList, LINE_SIZE);

	Tokenize(featureList, SEPARATORS, features, featureCount);

	graphFile << "% Graph based on table " << argv[1] << endl
			  << "%" << endl
			  << "% vertices" << endl
			  << "%" << endl;

	// process items
	discrepancy = 0;
	lineCount = 1;
	itemVertexNumber = 1;
	itemPropertyVertexNumber = 1;
	while (!tableFile.eof()) {
		tableFile.getline(line, LINE_SIZE);					// get line from file
		if (strlen(line) <= 1)
			break;

		graphFile << VERTEX << " " << itemVertexNumber << " " << generalItemName << endl;	// vertex
		itemPropertyVertexNumber = itemVertexNumber;
		itemVertexNumber++;
//		istrstream item(line);								// make string stream out of it

		Tokenize(line, SEPARATORS, columns, columnCount);
		if (columnCount < featureCount) {
			discrepancy = 1;
			cout << "Discrepancy in line " << lineCount 
				 << " : Number of proberties is " << columnCount << endl;
		}

		if (columnCount > featureCount) {
			discrepancy = 1;
			cout << "Fatal discrepancy in line " << lineCount 
				 << " : Number of properties is " << columnCount << endl;
			break;
		}

		
		for (int i = 0; i < columnCount; i++) {		//while (!item.eof()) {
			//item >> itemProperty;
			graphFile << VERTEX << " " << itemVertexNumber << " " << columns[i] << endl;	// vertex
			edgeFile << EDGE << " " << itemPropertyVertexNumber << " " << itemVertexNumber	// edge
				<< " " << features[i] << endl;
			itemVertexNumber++;
		}

		lineCount++;
	}

	graphFile << "%" << endl
			  << "% edges" << endl
			  << "%" << endl;

	edgeFile.seekg(0);										// rewind edge file
	while (!edgeFile.eof()) {
		edgeFile.getline(line, LINE_SIZE);					// get line from file
		graphFile << line << endl;
	}

	// cleaning up
	graphFile.close();
	edgeFile.close();
	remove(edgeFileName);								// delete edge file

	if (discrepancy) {									// if there is a discrepancy,
		cout << endl << "Press enter..." << endl;
		gets(line);											// stop
	}

	return 0;
}

//////////////////////
// Tokenizing a string
void Tokenize(char *str, char *separators, char *tokens[], int &tokenCount)
{
	tokenCount = 0;
	
	tokens[tokenCount] = strtok(str, separators);			// get first token
	
	while (tokens[tokenCount] != NULL) {
		tokenCount++;										// count number of tokens
		tokens[tokenCount] = strtok(NULL, separators);			// get rest of the tokens
		if (tokens[tokenCount] == NULL)
			break;
	}
}

