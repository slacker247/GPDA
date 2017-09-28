/*
 * Entity.java
 *
 * Created on November 10, 2003, 9:32 AM
 */

/**
 *
 * @author  s824685
 */
public class Entity {
    
    private int count = 0;
    private String MissionID = "";
    private float Latitude = 0.0f;
    private float Longitude = 0.0f;
    
    /** Creates a new instance of Entity */
    public Entity() {
    }
    
    public int getCount()
    {
        return count;
    }
    
    public String getMissionID()
    {
        return MissionID;
    }
    
    public float getLatitude()
    {
        return Latitude;
    }
    
    public float getLongitude()
    {
        return Longitude;
    }

    public void incrementCount()
    {
        count++;
    }
    
    public void setMissionID(String in)
    {
        MissionID = in;
    }
    
    public void setLatitude(float in)
    {
        Latitude = in;
    }
    
    public void setLongitude(float in)
    {
        Longitude = in;
    }
}
