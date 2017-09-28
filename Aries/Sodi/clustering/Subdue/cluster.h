/********************************************************************
*
* SUBDUE
*
* FILE NAME: cluster.h
* VERSION:   4.2.d.2
*
********************************************************************/

#ifndef __CLUSTER_H__
#define __CLUSTER_H__

/*
* Definitions used in cluster analysis
*/

// AT&T Bell Lab's graphviz stuff
#define ATT_COLOR_COUNT			16			/* There are many more!!! */
const char *ATT_COLOR[] = {"white", "green1", "ivory2", "orange1", "lightblue", 
						   "ivory4", "red", "yellow", "green", "violet", "brown", 
						   "azure3", "gold", "maroon", "orchid", "magenta"};

const char *ATT_SHAPE[] = {"ellipse", "box", "circle", "doublecircle", 
						   "diamond", "plaintext", "record", "polygon"};

#endif // __CLUSTER_H__