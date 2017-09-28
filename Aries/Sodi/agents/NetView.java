
import java.awt.*;
import COM.objectspace.voyager.*;
import java.io.*;
import AgentX;
import VAgentX;
import homeagent;
import Vhomeagent;




public class agentgui extends Frame {
//{{DECLARE_CONTROLS
	java.awt.Panel panel1;
   java.awt.Panel panel2;
   java.awt.Panel panel3;
   java.awt.Panel panel4;
   java.awt.Panel panel5;
   java.awt.Panel horiz1;  //horizontal line #1
   java.awt.Panel horiz2;  //horizontal line #2
   java.awt.Panel horiz3;  //horizontal line #3
   java.awt.Panel horiz4;  //horizontal line #4
   java.awt.Panel horiz5;  //horizontal line #4
   java.awt.Panel vert1;  //vertical line #1
   java.awt.Panel vert2;  //vertical line #2
   java.awt.Panel vert3;  //vertical line #3
   java.awt.Panel vert4;  //vertical line #4
	java.awt.Button button1;
	java.awt.TextArea textArea1;
	java.awt.Label label1;
	java.awt.Label label2;
	java.awt.Label label3;
	java.awt.Label label4;
   int waittime = 0;
   int shortsleep = 500;
   Graphics g;
	//}}	
	public agentgui() {

		//{{INIT_CONTROLS
		setLayout(null);
		addNotify();
		resize(insets().left + insets().right + 640,insets().top + insets().bottom + 555);
		panel1 = new java.awt.Panel();
		panel1.setLayout(null);
		panel1.reshape(insets().left + 12,insets().top + 12,616,528);
		panel1.setBackground(new Color(12632256));
		add(panel1);
		button1 = new java.awt.Button("Start Agent");
		button1.reshape(216,456,214,31);
      panel1.add(button1);

      panel2 = new java.awt.Panel();                      
      panel2.setLayout(null);                             
      panel2.reshape(240,252,151,41);                     
      panel2.setBackground(Color.blue);                   
      panel1.add(panel2);                                 
		
      panel3 = new java.awt.Panel();         
      panel3.setLayout(null);                
      panel3.reshape(36,36,151,41);        
      panel3.setBackground(Color.blue);      
      panel1.add(panel3);                    

      panel4 = new java.awt.Panel();         
      panel4.setLayout(null);                
      panel4.reshape(240,36,151,41);          
      panel4.setBackground(Color.blue);      
      panel1.add(panel4);                    

      panel5 = new java.awt.Panel();          
      panel5.setLayout(null);                 
      panel5.reshape(444,36,151,41);          
      panel5.setBackground(Color.blue);       
      panel1.add(panel5);                     

		textArea1 = new java.awt.TextArea();
		textArea1.reshape(12,324,602,124);
		panel1.add(textArea1);

      horiz1 = new java.awt.Panel();  //horizontal line 1      
      horiz1.setLayout(null);             
      horiz1.reshape(108,280,144,2);      
      horiz1.setBackground(Color.blue);  
      panel1.add(horiz1);                 

      horiz4 = new java.awt.Panel();  //horizontal line 1         
      horiz4.setLayout(null);                                     
      horiz4.reshape(130,262,144,2);                              
      horiz4.setBackground(Color.white);                          
      panel1.add(horiz4);                                         

      horiz5 = new java.awt.Panel();  //horizontal line 1     
      horiz5.setLayout(null);                                 
      horiz5.reshape(391,261,130,2);                          
      horiz5.setBackground(Color.white);                      
      panel1.add(horiz5);                                     




		//horizontalLine1 = new symantec.itools.awt.shape.HorizontalLine();
		//horizontalLine1.reshape(108,264,144,2);
		//panel1.add(horizontalLine1);
      horiz2 = new java.awt.Panel();  //horizontal line 2             
      horiz2.setLayout(null);                                         
      horiz2.reshape(180,60,72,2);                                  
      horiz2.setBackground(Color.blue);                              
      panel1.add(horiz2);
      
		//horizontalLine2 = new symantec.itools.awt.shape.HorizontalLine();
		//horizontalLine2.reshape(180,60,72,2);
		//panel1.add(horizontalLine2);
      horiz3 = new java.awt.Panel();  //horizontal line 3                   
      horiz3.setLayout(null);                                               
      horiz3.reshape(384,60,128,2);                                          
      horiz3.setBackground(Color.blue);                                    
      panel1.add(horiz3);                                                   



		//horizontalLine3 = new symantec.itools.awt.shape.HorizontalLine();
		//horizontalLine3.reshape(384,60,72,2);
		//panel1.add(horizontalLine3);
       vert1 = new java.awt.Panel();  //vertical line 1       
       vert1.setLayout(null);                                   
       vert1.reshape(108,72,2,210);                              
       vert1.setBackground(Color.blue);                        
       panel1.add(vert1);                                       

       vert2 = new java.awt.Panel();  //vertical line 1           
       vert2.setLayout(null);                                     
       vert2.reshape(130,72,2,190);                               
       vert2.setBackground(Color.white);                          
       panel1.add(vert2);                                         

       vert3 = new java.awt.Panel();  //vertical line 1                    
       vert3.setLayout(null);                                              
       vert3.reshape(316,72,2,179);                                        
       vert3.setBackground(Color.white);                                   
       panel1.add(vert3);                                                  

       vert4 = new java.awt.Panel();  //vertical line 1       
       vert4.setLayout(null);                                 
       vert4.reshape(519,77,2,184);                           
       vert4.setBackground(Color.white);                      
       panel1.add(vert4);                                     





		//verticalLine1 = new symantec.itools.awt.shape.VerticalLine();
		//verticalLine1.reshape(108,72,2,192);
		//panel1.add(verticalLine1);


		label1 = new java.awt.Label("HOME (TISA3A)");
		label1.reshape(262,296,138,33);
		panel1.add(label1);
		label2 = new java.awt.Label("TISA1A");
		label2.reshape(36,12,60,24);
		panel1.add(label2);
		label3 = new java.awt.Label("TISA2A");
		label3.reshape(252,12,60,24);
		panel1.add(label3);
		label4 = new java.awt.Label("TISA2B");
		label4.reshape(456,12,60,24);
		panel1.add(label4);
		setTitle("Agent Display");
		//}}

		//{{INIT_MENUS
		//}}
	}

	public agentgui(String title) {
	    this();
	    setTitle(title);
	}

    public synchronized void show() {
    	move(50, 50);
    	super.show();
    }

	public boolean handleEvent(Event event) {
    	if (event.id == Event.WINDOW_DESTROY) {
            hide();         // hide the Frame
            dispose();      // free the system resources
            System.exit(0); // close the application
            return true;
    	}
		return super.handleEvent(event);
	}

	

	static public void main(String args[]) {
	    (new agentgui()).show();
	}
	


                                                                                          
 void button1_Clicked(Event event) {
  
  //textArea1.text = "test";


	//initialize();                           	
}


public boolean action(Event e, Object o)
{
   //check to see if a button triggered the event
   if (e.target instanceof Button)
      {
         if (e.target == button1)
            textArea1.append("Initializing Agent 1 on TISA3A, port 8000.\n");
            initialize();
            
            return true;

      }
   return true;       
}



   public void initialize()                                                      
   {                                                                                           
    try                                                                                        
    {                   
      //graph = rect1.getGraphics();
      //create a traveling agent in tisa3a at port 8000                                        
      VAgentX agent1;
      
      panel2.setBackground(Color.red);
      repaint();
      //rect1.paint(graph);     
          
      agent1 = new VAgentX("127.0.0.1:8000/AGENT1");
      agent1.liveForever();                                                                    
      agent1.saveNow();
      //horizontalLine1.setForeground(Color.red);
      //horizontalLine1.repaint();            
      textArea1.append("Traveling agent 'Agent1' has been built at local server \n");
  
      
      

      //Voyager.shutdown();                                                                    
      //create a stationary agent in tisa3a at port 8001                                       
      Vhomeagent homeagent1;                                                                   
      homeagent1 = new Vhomeagent("127.0.0.1:8001/HOMEAGENT1");
      homeagent1.liveForever();                                                                
      homeagent1.saveNow();                                                                    
      homeagent1.arrived();                                                                    
      System.out.println("Home Agent1 has been built at local server");
      travel();
      Voyager.shutdown();                                                                      
    }                                                                                          
                                                                                               
    catch(Exception exception)                                                                 
    {                                                                                          
      System.err.println(exception);                                                           
    }                                                                                          
                                                                                               
                                                                                               
                                                                                               
                                                                                               
  }                                                                                            
                                                                                               

   public void travel()                                                  
   {                                                                                       
   String reply = "junk";                                                                  
   int sleeptime = 5000;                                                                   
   try                                                                                     
      {                                                                                    
       VAgentX agent1 = (VAgentX) VObject.forObjectAt("127.0.0.1:8000/AGENT1");         

       //xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
       panel2.setBackground(Color.blue);
       repaint();
       horiz1.setBackground(Color.red);          
       repaint();                                
       vert1.setBackground(Color.red);           
       repaint();                                
       agent1.moveTo("tisa1a:7001");

       try{Thread.sleep(waittime);} catch(InterruptedException Exception){}
       horiz1.setBackground(Color.blue); 
       repaint();                       
       vert1.setBackground(Color.blue);  
       repaint();
       
       panel3.setBackground(Color.red);
       repaint();
      
      
       agent1.update_address();
        vert2.setBackground(Color.red);          
        horiz4.setBackground(Color.red);         
        repaint();
        try{Thread.sleep(shortsleep);} catch(InterruptedException Exception){} 
        vert2.setBackground(Color.white);                    
        horiz4.setBackground(Color.white);           
        repaint();
        try{Thread.sleep(shortsleep);} catch(InterruptedException Exception){}

       textArea1.append(agent1.arrived());
        vert2.setBackground(Color.red);                                                   
        horiz4.setBackground(Color.red);                                                  
        repaint();                                                                        
        try{Thread.sleep(shortsleep);} catch(InterruptedException Exception){}            
        vert2.setBackground(Color.white);                                                 
        horiz4.setBackground(Color.white);                                                
        repaint();                                                                        
        try{Thread.sleep(shortsleep);} catch(InterruptedException Exception){}            

    
       textArea1.append(agent1.starting());
        vert2.setBackground(Color.red);                          
        horiz4.setBackground(Color.red);                         
        repaint();
        try{Thread.sleep(shortsleep);} catch(InterruptedException Exception){}
        vert2.setBackground(Color.white);                        
        horiz4.setBackground(Color.white);                       
        repaint();
        try{Thread.sleep(shortsleep);} catch(InterruptedException Exception){}
       textArea1.append(agent1.get_info() + "\n");
        vert2.setBackground(Color.red);                          
        horiz4.setBackground(Color.red);                         
        repaint();
        try{Thread.sleep(shortsleep);} catch(InterruptedException Exception){}
        vert2.setBackground(Color.white);                        
        horiz4.setBackground(Color.white);                       
        repaint();
        try{Thread.sleep(shortsleep);} catch(InterruptedException Exception){}

       textArea1.append(agent1.done());
        vert2.setBackground(Color.red);                          
        horiz4.setBackground(Color.red);                         
        repaint();
        try{Thread.sleep(shortsleep);} catch(InterruptedException Exception){}
        vert2.setBackground(Color.white);                        
        horiz4.setBackground(Color.white);                       
        repaint();
        try{Thread.sleep(shortsleep);} catch(InterruptedException Exception){}


       textArea1.append(agent1.leaving() + "\n");
        vert2.setBackground(Color.red);                          
        horiz4.setBackground(Color.red);                         
        repaint();
        try{Thread.sleep(shortsleep);} catch(InterruptedException Exception){}
        vert2.setBackground(Color.white);                        
        horiz4.setBackground(Color.white);                       
        repaint();
        try{Thread.sleep(shortsleep);} catch(InterruptedException Exception){}

       panel3.setBackground(Color.blue);
       repaint();
       horiz2.setBackground(Color.red);            
       repaint();
       agent1.moveTo("tisa2a:7002");  
       try{Thread.sleep(waittime);} catch(InterruptedException Exception){}
       horiz2.setBackground(Color.blue);    
       repaint();                          
       panel4.setBackground(Color.red);
       repaint();
       //xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx                                                                                    

       agent1.update_address();
       textArea1.append(agent1.arrived());
        vert3.setBackground(Color.red);                                         
        repaint();                                                              
        try{Thread.sleep(shortsleep);} catch(InterruptedException Exception){}  
        vert3.setBackground(Color.white);                                       
        repaint();                                                              
        try{Thread.sleep(shortsleep);} catch(InterruptedException Exception){}  
       textArea1.append(agent1.starting());
       vert3.setBackground(Color.red);                                             
       repaint();                                                                  
       try{Thread.sleep(shortsleep);} catch(InterruptedException Exception){}      
       vert3.setBackground(Color.white);                                           
       repaint();                                                                  
       try{Thread.sleep(shortsleep);} catch(InterruptedException Exception){}      

       textArea1.append(agent1.get_info() + "\n");
       vert3.setBackground(Color.red);                                             
       repaint();                                                                  
       try{Thread.sleep(shortsleep);} catch(InterruptedException Exception){}      
       vert3.setBackground(Color.white);                                           
       repaint();                                                                  
       try{Thread.sleep(shortsleep);} catch(InterruptedException Exception){}      

       textArea1.append(agent1.done());
       vert3.setBackground(Color.red);                                             
       repaint();                                                                  
       try{Thread.sleep(shortsleep);} catch(InterruptedException Exception){}      
       vert3.setBackground(Color.white);                                           
       repaint();                                                                  
       try{Thread.sleep(shortsleep);} catch(InterruptedException Exception){}      

       textArea1.append(agent1.leaving() + "\n");
       vert3.setBackground(Color.red);                                             
       repaint();                                                                  
       try{Thread.sleep(shortsleep);} catch(InterruptedException Exception){}      
       vert3.setBackground(Color.white);                                           
       repaint();                                                                  
       try{Thread.sleep(shortsleep);} catch(InterruptedException Exception){}      

       panel4.setBackground(Color.blue);
       horiz3.setBackground(Color.red);          
       repaint();
       agent1.moveTo("tisa2b:7003");     
       try{Thread.sleep(waittime);} catch(InterruptedException Exception){}   
       horiz3.setBackground(Color.blue);       
       repaint();                             


       //xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx                                                                                    
       panel5.setBackground(Color.red);
       repaint();

       agent1.update_address();
       vert4.setBackground(Color.red);                                          
       horiz5.setBackground(Color.red);                                         
       repaint();                                                               
       try{Thread.sleep(shortsleep);} catch(InterruptedException Exception){}   
       vert4.setBackground(Color.white);                                        
       horiz5.setBackground(Color.white);                                       
       repaint();                                                               
       try{Thread.sleep(shortsleep);} catch(InterruptedException Exception){}   


       textArea1.append(agent1.arrived());
       vert4.setBackground(Color.red);                                              
       horiz5.setBackground(Color.red);                                             
       repaint();                                                                   
       try{Thread.sleep(shortsleep);} catch(InterruptedException Exception){}       
       vert4.setBackground(Color.white);                                            
       horiz5.setBackground(Color.white);                                           
       repaint();                                                                   
       try{Thread.sleep(shortsleep);} catch(InterruptedException Exception){}       

       textArea1.append(agent1.starting());
       vert4.setBackground(Color.red);                                              
       horiz5.setBackground(Color.red);                                             
       repaint();                                                                   
       try{Thread.sleep(shortsleep);} catch(InterruptedException Exception){}       
       vert4.setBackground(Color.white);                                            
       horiz5.setBackground(Color.white);                                           
       repaint();                                                                   
       try{Thread.sleep(shortsleep);} catch(InterruptedException Exception){}       

       textArea1.append(agent1.get_info() + "\n");
       vert4.setBackground(Color.red);                                              
       horiz5.setBackground(Color.red);                                             
       repaint();                                                                   
       try{Thread.sleep(shortsleep);} catch(InterruptedException Exception){}       
       vert4.setBackground(Color.white);                                            
       horiz5.setBackground(Color.white);                                           
       repaint();                                                                   
       try{Thread.sleep(shortsleep);} catch(InterruptedException Exception){}       

       textArea1.append(agent1.done() + "\n");
       vert4.setBackground(Color.red);                                              
       horiz5.setBackground(Color.red);                                             
       repaint();                                                                   
       try{Thread.sleep(shortsleep);} catch(InterruptedException Exception){}       
       vert4.setBackground(Color.white);                                            
       horiz5.setBackground(Color.white);                                           
       repaint();                                                                   
       try{Thread.sleep(shortsleep);} catch(InterruptedException Exception){}       


       textArea1.append("FINISHED....TIME TO DIE \n");
        vert4.setBackground(Color.red);                                           
        horiz5.setBackground(Color.red);                                          
        repaint();                                                                
        try{Thread.sleep(shortsleep);} catch(InterruptedException Exception){}    
        vert4.setBackground(Color.white);                                         
        horiz5.setBackground(Color.white);                                        
        repaint();                                                                
        try{Thread.sleep(shortsleep);} catch(InterruptedException Exception){}    



       textArea1.append("DYING NOW \n");
        vert4.setBackground(Color.red);                                           
        horiz5.setBackground(Color.red);                                          
        repaint();                                                                
        try{Thread.sleep(shortsleep);} catch(InterruptedException Exception){}    
        vert4.setBackground(Color.white);                                         
        horiz5.setBackground(Color.white);                                        
        repaint();                                                                
        try{Thread.sleep(shortsleep);} catch(InterruptedException Exception){}    

       panel5.setBackground(Color.black);
       repaint();
       agent1.dieNow();                                                                    
       Voyager.shutdown();                                                                 

      }                                                                                    
                                                                                           
      catch(Exception exception)                                                           
      {                                                                                    
       System.err.println(exception);                                                      
      }                                                                                    
                                                                                           
                                                                                           
                                                                                           
   }                                                                                       
                                                                                           












































































































































}  //final closing brace

	
	
	
	

	

	
	






