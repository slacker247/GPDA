#include "subdue.h"

/*
int main(int argc , char *argv[])
{
  StartServer(5872);       // port number as parameter
}
*/


BOOLEAN StartServer(PGRAPH_VARIABLES GV)
{
   CHAR buffer[BUF_LENGTH];
   CHAR inbuf[BUF_LENGTH];

   INT n;
   PSOCKET_VAR sockVar = GV->sockVar;

   gethostname(sockVar->host,200);
   printf("connecting to host %s\n",sockVar->host);
   if((sockVar->serv_fd = socket(AF_INET,SOCK_STREAM,0)) < 0)
   {
      printf("Server : cannot open stream socket\n");
      return FALSE;
   }
   bzero((CHAR *)&(sockVar->serverAddr),sizeof(sockVar->serverAddr));
   sockVar->serverAddr.sin_family = AF_INET;
   sockVar->serverAddr.sin_addr.s_addr = htonl(INADDR_ANY);
   sockVar->serverAddr.sin_port = htons(5872);                

   printf("socket created\n");
  
   //sockVar->portNo);

   if(bind(sockVar->serv_fd,(struct sockaddr*) &(sockVar->serverAddr),sizeof(sockVar->serverAddr)) < 0)
   {
      printf("Server : Cannot bind local address [%s]\n", strerror(errno));
      return FALSE;
   }
  
   printf("Listening on socket\n");
   listen(sockVar->serv_fd,5);    /* listens to 5 connections and queues
                                   * them - 5 not necessary. just one is
                                   * enough for this application.
                                   */

   sockVar->client_len = sizeof(sockVar->clientAddr);
   sockVar->client_fd = accept(sockVar->serv_fd,(struct sockaddr*)&(sockVar->clientAddr), &(sockVar->client_len));
   if(sockVar->client_fd < 0)
   {
      printf("Server: Client could not connect to server\n");
      return FALSE;
//      break;
   }
   printf("Server: Client connected to server\n");

   strcpy(buffer,GV->graphFileName);
   strcat(buffer,"\n");
   printf("client fd = %d\n",sockVar->client_fd);
   write(sockVar->client_fd,buffer,strlen(buffer));
   printf("Server : Written to client\n");   
   n = ReadSkt(sockVar->serv_fd,inbuf,BUF_LENGTH);
//   printf("buffer is %s\n",inbuf);
//   printf("n=%d\n",n);
   
}

void StopServer(PGRAPH_VARIABLES GV)
{
   close(GV->sockVar->serv_fd);
   close(GV->sockVar->client_fd);
}

ULONG WriteSkt(ULONG fd, CHAR *ptr, ULONG n)
{
   ULONG nleft, nwritten;
  
   nleft = n;
   while(nleft > 0)
   {
      nwritten = write(fd,ptr,nleft);
      if(nwritten <= 0)
         return nwritten;
      nleft -= nwritten;
      ptr += nwritten;
   }
   return(n - nleft);
}


ULONG ReadSkt(ULONG fd, CHAR *ptr, ULONG max)
{
   ULONG n, rc;
   CHAR c;

   for(n=1;n<max;n++)
   {
     if( (rc=read(fd,&c,1)) == 1)
     {
       *ptr++ = c;
       if(c== '\n')
         break;
   
     }
     else if(rc == 0)
     {
       if(n==1)
         return 0;
       break;
     }
     else return -1;
   }
   *ptr = 0;
   return n;
}



void GraphDisplay(PGRAPH_VARIABLES GV,PSUB newSub)
{
   CHAR buffer[BUF_LENGTH];
   CHAR inbuf[BUF_LENGTH];
   PSOCKET_VAR sockVar = GV->sockVar;
   SHORT n;
   USHORT noOfChar;   
   CHAR *valueToStr;

   strcpy(buffer," ");

   valueToStr = (CHAR *)Malloc(sizeof(CHAR) * 10);
   noOfChar = sprintf(valueToStr,"%f",newSub->value);   

   strcpy(buffer, valueToStr);
   strcat(buffer,"\n");

   write(sockVar->client_fd,buffer,strlen(buffer));
   n = read(sockVar->serv_fd,inbuf,BUF_LENGTH);


   strcpy(buffer," ");

   GetSub( GV, newSub, buffer);
        

   strcat(buffer,"\n");      
                   // the last char should be \n so that the java
                   // client can read one line
//   printf("buffer is %s",buffer);
 

   write(sockVar->client_fd,buffer,strlen(buffer)); 
   n = read(sockVar->serv_fd,inbuf,BUF_LENGTH);
//   printf("after getting sub, n= %d and buffer = %s", n,inbuf);

}




/*******************************************************************************
FUNCTION NAME:  GetVertex 
INPUTS:		PGRAPH_VERTEX vertex, BOOLEAN negativeSub
RETURNS:	none
PURPOSE: 
CALLED BY:	prntstct.c: GetSubGraph()
*******************************************************************************/

void GetVertex( PGRAPH_VARIABLES GV, PGRAPH_VERTEX vertex, CHAR *buffer)
{ 
	LABEL label;
        CHAR *vertToStr;        
        USHORT noOfChar;

#ifdef DEBUG_TRACE
	printf( "%s: GetVertex()\n", __FILE__ );
#endif
	
	strcat(buffer,"v ");


        vertToStr = (CHAR *)Malloc(sizeof(CHAR) * 10); 
                         // assuming 10 digits

        noOfChar = sprintf(vertToStr,"%d",vertex->ID);
        strcat(buffer,vertToStr);
        strcat(buffer," ");

	label = GV->labelList[vertex->labelIndex];

	if ( label.labelType == 's' )     
        {
                          /* string label */

	
//                strcat(buffer," ");
                strcat(buffer,label.content.stringValue );
                strcat(buffer," ");
                
        }
	else		/* numeric label */
        {
//                strcat(buffer," ");
              noOfChar = sprintf(vertToStr, "%d", label.content.numericValue); 
              strcat(buffer,vertToStr);
              strcat(buffer," ");
        }
	return;
}


/*******************************************************************************
FUNCTION NAME:  GetEdge
INPUTS:		PGRAPH_EDGE edge, 
			PGRAPH_VERTEX vertices, 
			ULONG sourceVertexID,
			BOOLEAN negativeSub
RETURNS:	void
PURPOSE: 
CALLED BY:	prntstct.c: GetSubGraph()
*******************************************************************************/

void GetEdge( PGRAPH_VARIABLES GV, PGRAPH_EDGE edge, PGRAPH_VERTEX vertices,ULONG sourceVertexID, CHAR *buffer)
{
	ULONG targetVertexID;
	LABEL label;
        CHAR *vertToStr;
        USHORT noOfChar;

#ifdef DEBUG_TRACE
	printf( "%s: GetEdge()\n", __FILE__ );
#endif
	
        vertToStr = (CHAR *)Malloc(sizeof(CHAR) * 10); 
                         // assuming 10 digits

	targetVertexID = vertices[edge->targetVertexIndex].ID;
	
	/* do not print undirected edges for a vertex with higher ID than target */
//{U}
//	if ( edge->directed == FALSE && targetVertexID < sourceVertexID )
//		return;
	
        if ( edge->directed )
		strcat(buffer,"e ");  // change this to d
	else
		strcat(buffer,"e ");
  
        noOfChar = sprintf(vertToStr,"%d",sourceVertexID);
        strcat(buffer,vertToStr);


        strcat(buffer," ");
         
        noOfChar = sprintf(vertToStr,"%d",targetVertexID);
        strcat(buffer,vertToStr);

	
	label = GV->labelList[edge->labelIndex];


	if ( label.labelType == 's' )     
        {
                          /* string label */
                strcat(buffer," ");
                strcat(buffer,label.content.stringValue );
                strcat(buffer," ");
                
        }
	else		/* numeric label */
        {
                strcat(buffer," ");
                noOfChar = sprintf(vertToStr,"%d",label.content.numericValue);
                strcat(buffer,vertToStr);

                strcat(buffer," ");
        }

	
	return;
}



/*******************************************************************************
FUNCTION NAME: GetSubGraph
INPUTS:		PGRAPH_VARIABLES GV, 
			PSUB_GRAPH subGraph, 
			PRINT_MODE mode
RETURNS:	void
PURPOSE: 
CALLED BY:
     			server.c: GetSub()
*******************************************************************************/

void GetSubGraph( PGRAPH_VARIABLES GV, PSUB_GRAPH subGraph, PRINT_MODE mode, CHAR *buffer)
{
	ULONG vertexIndex;
	ULONG edgeIndex;
	PSUB_GRAPH_VERTEX vertices;
	PSUB_GRAPH_VERTEX currentVertex;
	PSUB_GRAPH_EDGE currentSubEdges;
	PGRAPH_VERTEX graphVertices;
	PGRAPH_VERTEX currentGraphVertex;
	char margin[8];
	
#ifdef DEBUG_TRACE
	printf( "%s: GetSubGraph()\n", __FILE__ );
#endif
	

	vertices = subGraph->vertices;
	graphVertices = GV->graph->vertices;

	for ( vertexIndex = 0; vertexIndex < subGraph->numberOfVertices; vertexIndex++ )
	{
		currentVertex = &vertices[vertexIndex];
		currentGraphVertex = &graphVertices[currentVertex->indexInGraph];
		currentSubEdges = &subGraph->edges[currentVertex->edgesIndex];
		if ( mode == VERTICES )                              /* printing vertices */
			GetVertex( GV, currentGraphVertex, buffer );
		else			                            /* printing edges */
			for ( edgeIndex = 0; edgeIndex < currentVertex->numberOfEdges; edgeIndex++ )
				GetEdge( GV, &currentGraphVertex->edges[currentSubEdges[edgeIndex].indexInGraphVertex],graphVertices, currentGraphVertex->ID, buffer);
	}
}



/*******************************************************************************
FUNCTION NAME: GetSub
INPUTS:		PGRAPH_VARIABLES GV, 
			PSUB sub
RETURNS:	void
PURPOSE: 
CALLED BY:	subdue.c: Discover()
*******************************************************************************/

void GetSub( PGRAPH_VARIABLES GV, PSUB sub, CHAR *buffer)
{
	
        USHORT i;
        PLIST_OF_SUB_GRAPHS_NODE instance;
       
#ifdef DEBUG_TRACE
	printf( "%s: GetSub()\n", __FILE__ );
#endif
	
//	GetSubGraph( GV, sub->definition, VERTICES, buffer);
// 	GetSubGraph( GV, sub->definition, EDGES, buffer );
        
        // put a termination character here to indicate one instance

//        strcat(buffer," $ ");

        instance = sub->instances->head;

        while(instance != NULL)
        {

            GetSubGraph(GV, instance->subGraph, VERTICES, buffer);
            GetSubGraph(GV, instance->subGraph, EDGES, buffer);
        
            // put a termination character here to indicate one instance
            
            strcat(buffer," $ ");
  
            instance = instance->next;
        }
       
	fflush( stdout );
}
