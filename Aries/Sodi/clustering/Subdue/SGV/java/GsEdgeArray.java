import java.util.*;


class GsEdgeArray {

  char etype;          // indicates e,d or u as edge type  
  int elabelIndex;     // index of the label for this edge
  int tgtVertexIndex;
  Vector objlist;      // of type GsObject

  void GsEdgeArray() {
    elabelIndex = 0;
    tgtVertexIndex = 0;
//    objlist  = new Vector();
  }

}
