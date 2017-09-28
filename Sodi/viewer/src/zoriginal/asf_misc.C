#include "asf_misc.H"

extern char *get_type (long id);      // in /home1/.../rsdgw93/get_type.C
extern char *get_graphics_type_string (long type);  // in sim_interface.C

char*
get_type_string (long id, long type)
{
   char *type_string;

   type_string = get_type (id);
   if (!type_string)   // i.e., if the "type" in <type,id> pair is not there:
      type_string = get_graphics_type_string (type);
   
   return type_string;
}  


