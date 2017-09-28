package com.appliedminds.martinix.gapp;


import java.awt.Component;
import java.awt.Dimension;
import com.appliedminds.martini.GraphPanel;




public interface GFilterTable {

  Component getComponent();
  void setPreferredScrollableViewportSize(Dimension d);
  void syncWithGraphPanel(GraphPanel p);
  


}
