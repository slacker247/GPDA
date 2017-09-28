// Main.C for prox

#include "Speedes.H"

//...... global object:
#include "freeobjs.H"
C_FREEOBJS freeobjs;

void PlugInProx();

int main (int argc, char **argv)
{
   PlugInProx();
   ExecuteSpeedes(argc, argv);
   return 0;
}
