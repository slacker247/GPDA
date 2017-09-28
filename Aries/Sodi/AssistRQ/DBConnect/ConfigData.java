/*
 * DBConnect.java
 *
 * Created on March 4, 2003, 10:08 AM
 */

package DBConnect;

import java.io.*;

public class ConfigData implements Serializable {

    public String User = null;

    public String Password = null;

    public String Host = null;

    public String Port = null;

    public String DB_Url = null;

    public String DB_Driver = null;

    public String Mission = null;

    public boolean Local = true;

    public ConfigData() {
        User = null;                   ///access to db
        Password = null;               ///access to db
        Host = null;                   ///Location of testbed
        Port = null;                   ///Port on Testbed
        DB_Url = null;                 ///jdbc:mysql:
        DB_Driver = null;              ///com.mysql.jdbc.Driver
        Mission = null;
        Local = true;
    }

    public ConfigData(String User, String Password, String Host, String Port, String DB_Url, String DB_Driver, String Mission, boolean Local) {
        this.User = User;                   ///access to db
        this.Password = Password;           ///access to db
        this.Host = Host;                   ///Location of testbed
        this.Port = Port;                   ///Port on Testbed
        this.DB_Url = DB_Url;
        this.DB_Driver = DB_Driver;
        this.Mission = Mission;
        this.Local = Local;
    }

    public void setUser(String User) { this.User = User;}

    public void setPassword(String Password) { this.Password = Password;}

    public void setHost(String Host) { this.Host = Host;}

    public void setPort(String Port) { this.Port = Port;}

    public void setDB_Url(String DB_Url) { this.DB_Url = DB_Url;}

    public void setDB_Driver(String DB_Driver) { this.DB_Driver = DB_Driver;}

    public void setMission(String Mission) { this.Mission = Mission;}

    public void setLocal(boolean Local) { this.Local = Local;}

    public String getUser() { return User;}

    public String getPassword() { return Password;}

    public String getHost() { return Host;}

    public String getPort() { return Port;}

    public String getDB_Url() { return DB_Url;}

    public String getDB_Driver() { return DB_Driver;}

    public String getMission() { return Mission;}

    public boolean getLocal() { return Local;}

    /** Outputs the configuration data that is located in the
     * Configuration class
     */
    public void writeConfigFile() throws IOException
    {
        FileWriter oOutStream = new FileWriter("Config.dat", false);
        /////////////////
        {
            oOutStream.write(DB_Driver + "\n");
            oOutStream.write(DB_Url + "\n");
            oOutStream.write(Host + "\n");
            oOutStream.write(Mission + "\n");
            Boolean tempb = new Boolean(Local);
            oOutStream.write(tempb.toString() + "\n");
            System.out.println(tempb.toString());
            String Pass = "";
            for(int i = 0; i < Password.length(); i++)
            {
                Pass += (char)((Password.toCharArray())[i] + '3');
            }
            oOutStream.write(Pass + "\n");
            oOutStream.write(Port + "\n");
            oOutStream.write(User + "\n");
        }
        oOutStream.flush();
        oOutStream.close();
    }

    /** Read in the Stored configuration data from the last session
     */
    public void readConfigFile() throws Exception
    {
        FileInputStream iInStream = new FileInputStream("Config.dat");
        BufferedReader br = new BufferedReader(
                            new InputStreamReader(iInStream));
        /////////////////
        {
            DB_Driver = br.readLine();
            DB_Url = br.readLine();
            Host = br.readLine();
            Mission = br.readLine();
            Boolean tempb = new Boolean(br.readLine());
            System.out.println(tempb.toString());
            Local = tempb.booleanValue();
            String Pass = "";
            Password = "";
            char Temp[] = br.readLine().toCharArray();
            for(int i = 0; i < Temp.length; i++)
            {
                Password += (char)(Temp[i] - '3');
            }
            Port = br.readLine();
            User = br.readLine();
        }
        iInStream.close();
    }
}

