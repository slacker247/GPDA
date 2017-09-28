/*		%W%		%G%		*/
/********************************************************

  modified many times by Y.Tung, 10/23/92 and earlier.

*********************************************************/

#ifndef		GR_DISPLIST
#define		GR_DISPLIST

#define INLINE inline

#include		"GR_DispObj.H"
#define			BLOCKSIZE		512

class GR_DispObj;

class	GR_DispList
{
  protected:
	GR_DispObj		**p_object_list;
	long			p_total_objects;
	long			p_array_size;
	long			p_enumidx;
  public:
	GR_DispList ();

	void	add_object (GR_DispObj& object);
	void	add_object (GR_DispObj* object);
	void	delete_object (GR_DispObj* object);
	void	delete_object (long id);
	void	delete_object_by_type (long type);
	void    delete_track (long trackid);
	void	drawobjs ();
	void	delete_objects ();
        GR_DispObj* retrieve_object (long id);
        GR_DispObj* retrieve_delete_object (long id);
	GR_DispObj* enumerate (long flag = 0);
	GR_DispObj* delete_cur ();
	void pop_object (GR_DispObj* object);

        long    length () { return p_total_objects;}
        GR_DispObj* getobj (long id) { return p_object_list[id];}
        Boolean inlist (long id);
	Boolean inlist_by_type (long type);

        long    get_array_size () { return p_array_size;}
        long    get_total_objects () { return p_total_objects;}
};

INLINE void
GR_DispList::add_object (GR_DispObj &object)
{
	this->add_object (&object);
}

INLINE void	
GR_DispList::delete_objects ()
{
        for (int i=0; i<p_total_objects; i++) p_object_list[i] = NULL;
	p_total_objects = 0;
}

#endif
