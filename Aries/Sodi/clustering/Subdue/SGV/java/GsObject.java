import java.util.*;

class GsObject {

 int shp;
 Vector pts;      // GsPoints
 int no_of_pts;
 GsClr crgb;
 double radius;
 GsText txt;


public void GsObject() {
                  shp = -1;
                  no_of_pts = 0;
                  radius = 0;
}


// set functions

public void setshp(int s)
{
  shp = s;
}

/*
public void setpoints(double x,double y, double z,int i)
{
  pts[i].px = x;
  pts[i].py = y;
  pts[i].pz = z;
}
*/

public void setnum(int n)
{
  no_of_pts = n;
}

public void setclr(boolean f,double x,double y,double z)
{
  crgb.fill = f;
  crgb.cr = x;
  crgb.cg = y;
  crgb.cb = z;
}


public void setradius(double rad)
{
  radius = rad;
}

public void settxt(String t) //,String ft,int fs)
{
  txt.str = t;
//  txt.fonttype = ft;
//  txt.fontsiz = fs;
}


// get functions

public int getshp() { return shp;}
//public float getx(int i) { return (pts[i].px);}
//public float gety(int i) { return (pts[i].py);}
//public float getz(int i) { return (pts[i].pz);}
public int getnum() { return no_of_pts;}
public boolean getfill() { return crgb.fill;}
public double getcr() { return crgb.cr;}
public double getcg() { return crgb.cg;}
public double getcb() { return crgb.cb;}
public double getradius() { return radius;}
public String getstr() { return txt.str;}
public String gettype() { return txt.fonttype;}
public int getsiz() { return txt.fontsiz;}

}   // end class

