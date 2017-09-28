/*		%W%		%G%		*/
#include		"GR_DispList.H"

GR_DispList::GR_DispList ()
{
	p_total_objects = 0;
	p_enumidx = -1;
	p_object_list = new GR_DispObj* [BLOCKSIZE];
	p_array_size = BLOCKSIZE;
}

void  GR_DispList::add_object (GR_DispObj* object)
{
	GR_DispObj		**new_list;
	int						i;

	if (p_total_objects + 1 == p_array_size) 
	{   
		//	Allocate a new array
		p_array_size += BLOCKSIZE;
		new_list = new GR_DispObj* [p_array_size];

		//	Copy the old elements into the new array
		for (i=0; i<p_total_objects; i++)
			new_list[i] = p_object_list[i];

		//	Delete the old array
		delete p_object_list;

		//	Set the object list to the new array
		p_object_list = new_list;
	} 
	p_object_list [p_total_objects] = object; 
	p_total_objects++;
}

void  
GR_DispList::drawobjs ()
{
	int     i;

	for (i=0; i < p_total_objects; i++)
        {
           if ( (GR_pickflag == 0) ||
                (GR_pickflag == 1 && 
                  p_object_list [i] -> get_register_flag ())
              )
			p_object_list [i]->draw();
        }
}

void		
GR_DispList::delete_object (GR_DispObj* object)
{
	int   i, j;

	for (i=0; i<p_total_objects; i++)
	{
		if (p_object_list[i] == object)
		{
			for (j = i+1; j < p_total_objects; j++)
				p_object_list [j-1] = p_object_list [j];

			p_total_objects--;
                        p_object_list[p_total_objects] = NULL; // added 12/29/92
			break;
		}
	}
}

void            
GR_DispList::delete_object (long id)
{
        int   i, j;

        for (i=0; i<p_total_objects; i++)
        {
                if (p_object_list[i]->get_id () == id)
                {
                        for (j = i+1; j < p_total_objects; j++)
                                p_object_list [j-1] = p_object_list [j];

                        p_total_objects--;
                        p_object_list[p_total_objects] = NULL; // added 12/29/92
                        break;
                }
        }
}

void
GR_DispList::delete_object_by_type (long type)
{
        int   i, j;

        for (i=0; i<p_total_objects; i++)
        {
                if (p_object_list[i]->get_type () == type)
                {
                        for (j = i+1; j < p_total_objects; j++)
                                p_object_list [j-1] = p_object_list [j];

                        p_total_objects--;
                        p_object_list[p_total_objects] = NULL; // added 12/29/92
                        break;
                }
        }
}

GR_DispObj*
GR_DispList::delete_cur ()
{
	GR_DispObj*	objectptr;

	if (p_enumidx == -1 || p_total_objects == 0)
		return 0;

	if (p_enumidx > 0)
		p_enumidx--;

	objectptr = (GR_DispObj *) p_object_list [p_enumidx];

	if (p_total_objects > 0)
	{
		p_total_objects--;
		p_object_list [p_enumidx] = p_object_list [p_total_objects];
	}

	return objectptr;
}

GR_DispObj*
GR_DispList::retrieve_object (long id)
{
   int   i;
   GR_DispObj*     objptr;

   objptr = NULL;
   for (i=0; i<p_total_objects; i++)
   {
       if (p_object_list[i]->get_id () == id)
       {
           objptr = p_object_list[i];
           break;
       }
   }
   return objptr;
}


GR_DispObj*
GR_DispList::retrieve_delete_object (long id)
{
   int         i;
   GR_DispObj  *objptr, *lastptr;

   objptr = NULL;
   if (p_total_objects>0)
   {
     lastptr = p_object_list[p_total_objects-1]; 
     for (i=0; i<p_total_objects; i++)
     {
        if (p_object_list[i]->get_id () == id)
        {
           objptr = p_object_list[i];  
           p_object_list[i] = lastptr;
           p_total_objects--;
           break;
        }
     }
   }
   return objptr;
}


GR_DispObj*
GR_DispList::enumerate (long flag)
{
	GR_DispObj* 	objectptr;

	if (flag)
		p_enumidx = 0;

	if (p_enumidx == -1)
		return 0;
		
	if (p_enumidx >= p_total_objects)
	{
		p_enumidx = -1;
		return	0;
	}

	objectptr =	p_object_list [p_enumidx];
	p_enumidx++;

	return objectptr;
}

void
GR_DispList::pop_object (GR_DispObj* object)
{
	int						i;
	GR_DispObj	*tmpobj;

	tmpobj = p_object_list [p_total_objects - 1];
	for (i=0; i<p_total_objects; i++)
	{
		if (p_object_list[i] == object)
		{
			p_object_list [p_total_objects - 1] = p_object_list [i];
			p_object_list [i] = tmpobj;
			return;
		}
	}
}

Boolean
GR_DispList::inlist (long id)
{
  register int i;
  for (i = 0; i< p_total_objects; i++)
  {
    if (p_object_list[i]->get_id() == id)
       return TRUE;
  }
  return FALSE;
}

Boolean
GR_DispList::inlist_by_type (long type)
{
  register int i;
  for (i = 0; i< p_total_objects; i++)
  {
    if (p_object_list[i]->get_type () == type)
       return TRUE;
  }
  return FALSE;
}

