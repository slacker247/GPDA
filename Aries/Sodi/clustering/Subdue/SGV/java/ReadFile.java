/********************** ReadFile.java ***************************/

import java.io.*;
import java.awt.*;
import java.awt.event.*;
import java.applet.*;
import java.util.*;


public class ReadFile {

  private int line_no = 0;
  private DataInputStream  inpfile,ifile;
  private int toktype;
  private StreamTokenizer tok;
  GsGraph curGraph;    // should be public


 // Constructor
  public ReadFile(String filename) 
  {
    curGraph = new GsGraph();      // create new Graph with vertex array & edge
                                   // array
    curGraph.labelArray = new Vector();
    CountVertices(filename);       // count the number of vertices & create 
                                   // label array
    try {
 
      inpfile = new DataInputStream(new FileInputStream(filename));
    }
    catch(FileNotFoundException e) { 
      System.out.println("file not found"); 
      System.exit(1);
    }
    
    Reader rd2 = new BufferedReader(new InputStreamReader(inpfile));
    tok = new StreamTokenizer(rd2);

    if(ProcessFile())
    {
      System.out.println("Parsing Successful");
      //return true;
    }

    else
    {
      System.out.println("Parsing unSuccessful");
      //return false;
    }
    //System.exit(1);

  }
    

/************************ CLOSEFILE ***********************************/

  private boolean closefile(DataInputStream fl)
  {
    try {
        fl.close();
        return true;
    }
    catch (IOException e)  
    {
          System.err.println("Error closing file \n" + e.toString());
          return false;
    }

  }


/**************************** COUNTVERTICES *********************************/
  // to count the number of vertices in the file and to create label array
  public void CountVertices(String filename)
  {

    StreamTokenizer tokn;
    try {
 
      ifile = new DataInputStream(new FileInputStream(filename));
    }
    catch(FileNotFoundException e) { 
      System.out.println("file not found"); 
      System.exit(1);
    }

    Reader rd1 = new BufferedReader(new InputStreamReader(ifile));

    tokn = new StreamTokenizer(rd1);
                            // initializing defaults
    tokn.commentChar((int)'%');
    tokn.quoteChar((int)'"');          
    tokn.parseNumbers();
    tokn.eolIsSignificant(true);
    tokn.slashStarComments(false);
    tokn.slashSlashComments(false);
    tokn.lowerCaseMode(false);
    tokn.ordinaryChar((int) '(');
    tokn.ordinaryChar((int) ')');

    int vertcount = 0;
    int edgecount = 0;
    int i;
  
    try {
      while(true)
      {
        toktype = tokn.nextToken();

        if(toktype == tokn.TT_WORD)
        {
          if(tokn.sval.toLowerCase().equals("v"))
            vertcount++;
          if(tokn.sval.toLowerCase().equals("e") || tokn.sval.toLowerCase().equals("d") || tokn.sval.toLowerCase().equals("u"))
          {
            edgecount++;
            toktype = tokn.nextToken();
          }
          toktype = tokn.nextToken();
          toktype = tokn.nextToken();
          if(toktype == tokn.TT_WORD)
          {
            if( curGraph.labelArray.contains(tokn.sval)==false)
            {  
              //System.out.println("The word is "+tokn.sval);
              curGraph.labelArray.addElement(tokn.sval);
            }

          }
          else if(toktype == tokn.TT_NUMBER)
          {
            if(!curGraph.labelArray.contains(String.valueOf(tokn.nval)))
            {
              //System.out.println("The word is "+tokn.nval);
              curGraph.labelArray.addElement(String.valueOf(tokn.nval));
            }

          }
            
    //      toktype = tok.nextToken();
        }
        while(toktype != tokn.TT_EOL && toktype != tokn.TT_EOF)
            toktype = tokn.nextToken();
          
        if(toktype == tokn.TT_EOL) 
          continue;
        if(toktype == tokn.TT_EOF)
        {
          curGraph.vertexArrayLength = vertcount;
          closefile(ifile);
          break;
        }
      }
      //System.out.println("Number of vertices = " +vertcount);
      curGraph.vertexArrayLength = vertcount;
    }
    catch(IOException em) {
      em.printStackTrace();          
      closefile(ifile);
      return;
    }

    catch(NullPointerException np) { 
      System.out.println("Error while parsing in CountVertices: Null Pointer Exception"); 
      np.printStackTrace();
      closefile(ifile);
      return;
    }
    closefile(ifile);
    return;
  }


/********************************* PROCESSFILE *******************************/
  public boolean ProcessFile()
  {

    int curIndex = 0;
    int labindx = 0;
    boolean vertex = true;  // indicates whether current read line is a vertex
                            // or edge
    GsVertexArray varray;
//    GsVertexArray vx;
    GsEdgeArray earray;
    GsObject obj;
    GsPoints pt;


        boolean endofline;
        boolean endoffile = false;
        boolean first, processed ;


                                       // set defaults   
    tok.commentChar((int)'%');         
    tok.quoteChar((int)'"');          
    tok.parseNumbers();
    tok.eolIsSignificant(true);
    tok.slashStarComments(false);
    tok.slashSlashComments(false);
    tok.lowerCaseMode(false);
    tok.ordinaryChar((int) '(');
    tok.ordinaryChar((int) ')');

    pt = new GsPoints();
    curGraph.createVertexArray();

    while(true)
    {
      endofline = false;
      first = true;
      processed = true;

      try {
        //System.out.println();
        varray = new GsVertexArray();
        earray = new GsEdgeArray();
        toktype = tok.nextToken();
        //while(  (toktype == tok.TT_WORD) || (toktype == tok.TT_EOL) || (toktype == tok.TT_NUMBER))

                                              /**** vertex processing ****/

        if(toktype == tok.TT_WORD && tok.sval.toLowerCase().equals("v"))
        {
          vertex = true;
          varray.objlist = new Vector();
          varray.edgeArray = new LinkedList();
          toktype = tok.nextToken();
          varray.vertexId = (int)tok.nval;    // vertex ids correspond to array
                                              // index + 1
          toktype = tok.nextToken();

          if(toktype == tok.TT_WORD)
            labindx = curGraph.labelArray.indexOf(tok.sval);
          else
            labindx = curGraph.labelArray.indexOf(String.valueOf(tok.nval));
          //System.out.print("index is " +labindx+" ");
          varray.vlabelIndex = labindx;
        }

                                            /**** edge processing ****/

        else if(toktype == tok.TT_WORD && (tok.sval.toLowerCase().equals("e") || tok.sval.toLowerCase().equals("d") || tok.sval.toLowerCase().equals("u")) )
        {
          vertex = false;       // indicates current line is an edge

          earray.etype = tok.sval.toLowerCase().charAt(0);
                       // indicates 'e', 'd' or 'u'
                                     
          toktype = tok.nextToken();
          if(toktype != tok.TT_NUMBER)
          {
            System.out.println("Invalid vertex Id in line no "+tok.lineno());
            closefile(inpfile);
            return false;
          }
          int v1 = (int)tok.nval;
          varray = curGraph.vertexArray[v1-1];
          toktype = tok.nextToken();  // second vertex
          int v2 = (int)tok.nval;
          toktype = tok.nextToken();  // the label
          if(toktype == tok.TT_WORD)
            labindx = curGraph.labelArray.indexOf(tok.sval);
          else
            labindx = curGraph.labelArray.indexOf(String.valueOf(tok.nval));
          earray.objlist = new Vector();
          earray.elabelIndex = labindx;
          earray.tgtVertexIndex = v2;
                       
        }
        else processed = false;
        
        while(endofline == false) {
        
                      // if there are multiple objects
        while(toktype != ((int)'(')  && toktype != tok.TT_EOL && toktype != tok.TT_EOF)
          toktype = tok.nextToken();

                                   // end of file
        if(toktype == tok.TT_EOF)
        {
          endoffile = true;
          closefile(inpfile);
          return true;
        }
               
                             // end of line
        if(toktype == tok.TT_EOL)
        {
          endofline= true;
          
//          break;
          continue;
        }
//        if(toktype == (int) ')')
//             break;

        obj = new GsObject();
        obj.pts = new Vector();
        obj.crgb = new GsClr();
        obj.txt = new GsText();

//        pt = new GsPoints();
                                          // begin objlist
        if(first)
        {
          first = false;
          toktype = tok.nextToken();
        }
        if(toktype != (int) '(' )    // begin object
        {
          System.out.println("ProcessFile : Missing paranthesis in line number " + tok.lineno());
          closefile(inpfile);
          return false;
        }
        else 
          toktype = tok.nextToken();

        if(toktype == tok.TT_WORD)
        {
          if(tok.sval.toLowerCase().equals("polygon"))
          {
            if(!parsePoly(obj,pt))
            {
              closefile(inpfile);
              return false;
            }
            toktype = tok.nextToken();

          }
          else if(tok.sval.toLowerCase().equals("circle"))
          {
            if(!parseCircle(obj,pt))
            {
              closefile(inpfile);
              return false;
            }
            toktype = tok.nextToken();

          }
          else if(tok.sval.toLowerCase().equals("text"))
          {
            if(!parseText(obj,pt))
            {
              closefile(inpfile);
              return false;
            }
            toktype = tok.nextToken();
            //System.out.println("here it is " +(char)toktype);
          }
          else 
          {
            System.out.println("Unrecognized word in line no "+tok.lineno());
            closefile(inpfile);
            return false;
          } 
        }
               // not a word
        if((char)toktype == '(')
        {
          // could be font,color,size or fill
          while(toktype !=  (int) ')' )
          {
            toktype = tok.nextToken();
            if(toktype == tok.TT_WORD)
            {
                               /****** font ******/

              if(tok.sval.toLowerCase().equals("font"))
              {
                toktype = tok.nextToken();
                // parse font type
                if(toktype == tok.TT_WORD)
                  obj.txt.fonttype = tok.sval;
                else
                {
                  System.out.println("Font type not recognized in line no " + tok.lineno());
                  closefile(inpfile);
                  return false;
                }
                toktype = tok.nextToken();   // could be ) or size
                                
              }
                         // if token is "size"
              if(toktype == tok.TT_WORD && tok.sval.toLowerCase().equals("size"))
              {
                toktype = tok.nextToken();
                // parse size
                obj.txt.fontsiz = (int)tok.nval;
                toktype = tok.nextToken();
              }

              if(toktype != tok.TT_WORD && toktype != (int)')')
              {
                System.out.println("Error4 in line no "+tok.lineno());
                closefile(inpfile);
                return false;
              }
              else  if(toktype == (int) ')')
                toktype = tok.nextToken();

              if(toktype != tok.TT_WORD )
              {
                if(toktype != (int)'(' )
                {
                  if( toktype != (int) ')' )
                  {
                     // error
                    System.out.println("Error5 in line no "+tok.lineno());
                    closefile(inpfile);
                    return false;
                  }
                  else
                  {
                    // finished one object
                       break;      // from inner while loop
                  }
                }
                else    // token is (
                  toktype = tok.nextToken();
              }
              
                                          /****** color ******/
              if(tok.sval.toLowerCase().equals("color"))
              {
                toktype = tok.nextToken();
                // parse color
                parseColor(obj);
                toktype = tok.nextToken();
              }
                    // could be fill or )

              if(toktype == tok.TT_WORD && tok.sval.toLowerCase().equals("fill"))
              {
                toktype = tok.nextToken();
                // parse fill
                if(toktype == tok.TT_WORD)
                {
                  if(tok.sval.toLowerCase().equals("yes"))
                    obj.crgb.fill = true;
                  else if(tok.sval.toLowerCase().equals("no"))
                    obj.crgb.fill = false;
                  // else // error
                }
                // else   //error
                toktype = tok.nextToken();
              }

              if(toktype != tok.TT_WORD && toktype != (int)')')
              {
                System.out.println("Error6 in line no "+tok.lineno());
                closefile(inpfile);
                return false;
              }
              else    
                toktype = tok.nextToken();

              if(toktype != tok.TT_WORD )
              {
                if(toktype != (int)'(' )
                {
                  if( toktype != (int) ')' )
                  {
                     // error
                   // System.out.println((char)toktype);
                    System.out.println("Error7 in line no "+tok.lineno());
                    closefile(inpfile);
                    return false;
                  }
                  else
                  {
                    // finished one object
                       break;      // from inner while loop
                  }
                }
                else    // token is (
                {
              
                     // error
                    System.out.println("Error8 in line no "+tok.lineno());
                    closefile(inpfile);
                    return false;
                }
              }
                    // end

            }
            else    // not a word
            {
                     // error
                    System.out.println("Error9 in line no "+tok.lineno());
                    closefile(inpfile);
                    return false;
            }
          } // end while

        }// end if
        else if( (char)toktype != ')'){
            System.out.println("Error10 in line no " + tok.lineno());
            closefile(inpfile);
            return false;
        }
        if(vertex)
	{
          if(varray.objlist == null)
            varray.objlist = new Vector();
          varray.objlist.addElement(obj);    // add this object to this vertex
 	}
        else
        {
          earray.objlist.addElement(obj);
          varray.edgeArray.add(earray);
          varray.numberOfEdges++;
        }
      }           // while(endofline == false)
      
      }  // end try
 
      catch(IOException em) {          
        em.printStackTrace();
        closefile(inpfile);
        return false;
      }

      catch(NullPointerException np) { 
        System.out.println("Error while parsing in ProcessLine: Null Pointer Exception"); 
        np.printStackTrace();
        closefile(inpfile);
        return false;
      }
      if(vertex && endofline && processed)
      {
       // System.out.println("The current index is " + curIndex);
        curGraph.vertexArray[curIndex]=varray;
        curIndex++;
      }
    }       // end while
 
  }  // end function ProcessLine2


/******************************** PARSEPOLY *************************/

  private boolean parsePoly(GsObject obj,    GsPoints pt )
  {

    int ptcount=0;
    int pttype = 0;

    try {
      
      toktype = tok.nextToken();
      if(toktype == (int)'(')
      {
        while(true)
        {
          toktype = tok.nextToken();
          if(toktype == (int)')')
            break;
          if(toktype == (int)'(')
          {
            toktype = tok.nextToken();  // reading each point
            pttype = 1;
            pt = new GsPoints();
            pt.px = pt.py = pt.pz = 0;
            while(toktype != (int)')')
            {
              //System.out.print(tok.nval+" ");       
              switch(pttype)
              {
                case 1: pt.px = tok.nval;
                        break;
                case 2: pt.py = tok.nval;
                        break;
                case 3: pt.pz = tok.nval;
                        break;
              }
              pttype++;
              toktype = tok.nextToken();
            }
             obj.pts.addElement(pt);
             ptcount++;
          }

        } // end outer while 
      }
      else 
      {
        System.out.println("ParsePoly :Missing paranthesis in line no "+ tok.lineno());
        return false;
      }
      obj.no_of_pts = ptcount;
      obj.shp = 0;
    }
    catch(IOException em) {          
        em.printStackTrace();
        closefile(inpfile);
        return false;
    }

    catch(NullPointerException np) { 
      np.printStackTrace();
      closefile(inpfile);
      return false;
    }
    return true;
  }


/******************************** PARSECIRCLE *************************/

  private boolean parseCircle(GsObject obj,GsPoints pt)
  {
    int pttype = 0;

    try {
      
      toktype = tok.nextToken();
      if(toktype == (int)'(')
      {
        toktype = tok.nextToken();
        pttype = 1;
        pt = new GsPoints();
        while(toktype != (int)')')
        {
          //System.out.print(tok.nval+" ");       
          switch(pttype)
          {
            case 1: pt.px = tok.nval;
                    break;
            case 2: pt.py = tok.nval;
                    break;
            case 3: pt.pz = tok.nval;
                    break;
            
          }
          pttype++;
          toktype = tok.nextToken();
        }
        obj.pts.addElement(pt);
        obj.no_of_pts = 1;
        obj.shp = 1;
      }
      else 
      {
        System.out.println("ParseCircle: Missing paranthesis in line no "+ tok.lineno());
        return false;
      }
      toktype = tok.nextToken();  // radius
      if(toktype == tok.TT_NUMBER)
      {
        //System.out.print(tok.nval+" ");
        obj.radius = tok.nval;
      }
      else
      {
        System.out.println("Missing radius info in line no "+ tok.lineno());
        return false;
      } 
    }
    catch(IOException em) {
        em.printStackTrace();          
        closefile(inpfile);
        return false;
    }

    catch(NullPointerException np) { 
      np.printStackTrace();          
      closefile(inpfile);
      return false;
    }
    return true;
  }

/******************************** PARSETEXT *************************/

  private boolean parseText(GsObject obj,GsPoints pt)
  {
    int pttype = 0;

    try {
     // System.out.println("read text");
      toktype = tok.nextToken();
      // toktype contains text to be written
//      System.out.print(tok.sval+" ");

      obj.txt.str = tok.sval;

      toktype = tok.nextToken();
//      System.out.println((char)toktype);
      if(toktype == (int)'(')
      {
        toktype = tok.nextToken();
        pttype = 1;
        pt = new GsPoints();
        while(toktype != (int)')')
        {
          //System.out.print(tok.nval+" ");       
          switch(pttype)
          {
            case 1: pt.px = tok.nval;
                    break;
            case 2: pt.py = tok.nval;
                    break;
            case 3: pt.pz = tok.nval;
                    break;
            
          }
          pttype++;
          toktype = tok.nextToken();
        }
        obj.pts.addElement(pt);
        obj.no_of_pts = 1;
        obj.shp = 2;
      }
      else 
      {
        System.out.println("ParseText:Missing paranthesis in line no "+ tok.lineno());
        return false;
      }
    }
    catch(IOException em) {          
        em.printStackTrace();          

        closefile(inpfile);
        return false;
    }

    catch(NullPointerException np) { 
      np.printStackTrace();          
      closefile(inpfile);
      return false;
    }
  
    return true;
  }


/****************** PARSECOLOR ********************************/
 
public void parseColor(GsObject obj)
{
    obj.crgb.cr = 0.0;
    obj.crgb.cg = 0.0;
    obj.crgb.cb = 0.0;
  if(tok.sval.toLowerCase().equals("red"))
    obj.crgb.cr = 1.0;
  else   if(tok.sval.toLowerCase().equals("green"))
    obj.crgb.cg = 1.0;
  else   if(tok.sval.toLowerCase().equals("blue"))
    obj.crgb.cb = 1.0;

// add more colors like black, white, ...

}

/*
// ****************************** MAIN **************************** /

  public static void main(String args[])
  {
      new ReadFile("points");
  }
    
*/

} // end class
