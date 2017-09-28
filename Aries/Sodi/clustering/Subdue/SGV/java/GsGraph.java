import java.util.*;

 class GsGraph {

  int numberOfVertices;
  int numberOfEdges;        // no need for this??
  int numberOfUndirectedEdges;
  int vertexArrayLength;
  GsVertexArray vertexArray[];
  Vector labelArray;

  void GsGraph() {
    numberOfVertices = 0;
    numberOfEdges = 0;
    numberOfUndirectedEdges = 0;
    vertexArrayLength = 0;
//    labelArray = new Vector();
  }


  public void createVertexArray() {
//     System.out.println("Length is "+vertexArrayLength);
     vertexArray = new GsVertexArray[vertexArrayLength];
//       vertexArray = new Vector();

  }
} 

