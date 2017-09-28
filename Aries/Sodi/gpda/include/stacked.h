
#define FL_STACKED       FL_USER_CLASS_START+1
#define FL_VERT_STACKED  0
#define FL_HOR_STACKED   1

extern FL_OBJECT *fl_create_stacked(int, FL_Coord, FL_Coord, FL_Coord, FL_Coord,
				    const char *);
extern FL_OBJECT *fl_add_stacked(int, FL_Coord, FL_Coord, FL_Coord, FL_Coord,
				 const char *);
extern void fl_clear_stacked( FL_OBJECT *obj);
extern void fl_add_stacked_value(FL_OBJECT *obj, double val, const char *text, int col);
extern void fl_replace_stacked_value(FL_OBJECT *obj, int index,
				     double val, const char *text, int col);
