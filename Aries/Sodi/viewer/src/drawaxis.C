   GR_color(255, 0, 0);
   glBegin(GL_LINES);
     glVertex3f(0.0, 0.0, 0.0);
     glVertex3f(2.0*RE, 0.0, 0.0);
   glEnd();

   GR_color(0, 255, 0);
   glBegin(GL_LINES);
     glVertex3f(0.0, 0.0, 0.0);
     glVertex3f(0.0, 2.0*RE, 0.0);
   glEnd();

   GR_color(0, 0, 255);
   glBegin(GL_LINES);
     glVertex3f(0.0, 0.0, 0.0);
     glVertex3f(0.0, 0.0, 2.0*RE);
   glEnd();

