import java.util.*;


class GsVertexArray {
 
  int vertexId;            // unique id
  int vlabelIndex;         // index of the label for this vertex
  int numberOfEdges;       // number of edges for this vertex
  LinkedList edgeArray;    // since the number of edges is not known
                           // beforehand
  Vector objlist;          // to store objects of type GsObject

  void GsVertexArray() {
    vertexId = 0;
    vlabelIndex = 0;
    numberOfEdges = 0;
//    objlist = new Vector();    
  }
}

