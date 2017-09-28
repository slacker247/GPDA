#ifndef GR_VIEWTRANS
#define GR_VIEWTRANS

#include "GR_Interface.H"

#define		GR_ORTHO		1
#define		GR_ORTHO2		2
#define		GR_PERSPECTIVE	3

union ViewParams
{
	struct
	{
		float left;
		float right;
		float bottom;
		float top;
		float near;
		float far;
	}orthoView;

	struct 
	{
		long fov;
		float aspect;
		float near;
		float far;
	}perspectiveView;

};

class GR_ViewTrans
{
	private:
		long		p_viewmode;
		ViewParams	p_viewparams;
		GR_Point	p_viewpos;
		GR_Point	p_lookvec;
		long		p_twist;
		void p_set_default_ortho ();
		void p_set_default_perspective ();
	public:
		GR_ViewTrans (long viewmode = GR_ORTHO2);
		void draw ();
		void set_viewmode (long viewmode);
		long get_viewmode ();
		void field_of_view (float value);
		void aspect (float value);
		void near (float value);
		void far (float value);
		void left (float value);
		void right (float value);
		void bottom (float value);
		void top (float value);
		void twist (float value);
		void view_position (GR_Point position);
		void view_position (float x, float y, float z);
		void look_vector (GR_Point vector);
		void look_vector (float x, float y, float z);
};

#endif
