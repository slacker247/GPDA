
const int MAX1 = 100;    //for node and edge arrays
const int MAX2 = 10;    //for location array of logistics info

/***
Index values struct.  These 15 variables hold the user entered
value for what is wanted in a case.  These varibles are similar
to the Case_Index_data below.  The following are the weighted values
for user entered data while Case_Index_data holds weighted
values of case data from the case base.
***/

struct WeightCriteria
{
      float max_threatsW;
      float terrainW;
      float slopeW;
      float timingW;
      float strengthW;
      float timeW;
      float commoditiesW;
      float equipageW;
      float fuelW;
      float subsidiariesW;
      float expected_down_commW;
      float unexpected_down_commW;
      float natural_elementsW;
      float mechanicalW;
      float distanceW;
};


/**Terrain data in an enumerated type to make
   program more understandable
**/

enum TerrainE {urban, range_land, barren_land, tundra,
              ice_snow, wetland, forest};

/***
Supposed to be part of an array that holds information
about each enemies strength along the route
***/

struct TroopStrength
{
   //percentage of fire unit's power

   float manpower;
   float firepower;
};


/**Holds information about threats in route.
Where each threat is and the number at that
locations
**/

struct ThreatS
{
   char threat_locations[MAX2];
   int num_at_location[MAX2];
};


/***
"Real" data stored in a case for this program.  Needs
to be developed to be much more realistic, but hey, it works for now.
These values are the route information and are simplified
and indexed by the values in the Case_Index_data struct
***/

struct Case_Real_data
{
   char edges[MAX1];
   char nodes[MAX1];
   int departure_time;
   int  ETA;
   float travel_time;
   TerrainE terrain_type;
   float slope;
   ThreatS threats;
   int num_of_threats;
   TroopStrength strength_info;     //needs to be array for all threat
   float distance;                   //locations, couldn't get read in
   char commodity_locations[MAX2];   //working right
   char fuel_locations[MAX2];
   char equipage_locations[MAX2];
   char subsidiary_locations[MAX2];
   float chance_expec_down_comm;
   float chance_unexpec_down_comm;
   float chance_natural_elements;
   float chance_mechanical;
};


/***
Index values that hold the weighted values for each case
from the case base.  Compared to weighted values user
entered to determine best matching cases.
***/

struct Case_Index_data
{
   int max_threatsV,
       terrainV,
       slopeV,
       timingV,
       strengthV,
       timeV,
       commoditiesV,
       equipageV,
       fuelV,
       subsidiariesV,
       expec_down_commV,
       unexpec_down_commV,
       natural_elementsV,
       mechanicalV,
       distanceV;
};


/***
Struct of a character array.  I couldn't get my arrays
to return correctly so I put it in a struct and returned that
***/
struct char_return
{
   char char_temp[10];
};

/***
Also a struct or arrays to allow them to be returned
***/
struct point_return
{
   float total_points[MAX1];
   float match_points[MAX1];

};


/***
Node struct for the linked list that holds the cases
from the case base
It is doubly linked so it contains prev and next to
link the list and all the index values of the case,
all the real case data from the case, the total points
found by index values matching user entered values, and the
total points a case has based only on its index values
***/
struct Node
{
   Node *prev;
   Node *next;

   Case_Index_data index_data;
   Case_Real_data mission_data;
   float case_total_match_points;
   float case_total_points;
};



#ifndef CASECLASS_H
#define CASECLASS_H

class CASECLASS
{
   private:
           Node *head;
           Node *tail;
           Node *head_best;
           Node *tail_best;
           void build_cases();
           void add_cases(Case_Real_data info, Case_Index_data new_case);
           void add_best_cases(Case_Real_data info, Case_Index_data new_case);

   public:
          CASECLASS();
          ~CASECLASS();
          point_return original_points;
          point_return modified_points;
          char_return returnme;
          void set_case_values(WeightCriteria weight_data, int i);
          void rate_cases(float score);
          Case_Real_data return_case_data();
          void repair_cases();
          void modify_case_int(int i, int input1, int change_this_data);
          void modify_case_float(int i, float input1, const char *get_str,
                                 int change_this_data);
          char_return modify_case_add_char(int i, const char *input1,
                             char input2, int change_this_data);
          char_return modify_case_rem_char(int i, const char *input1,
                             char input2, int change_this_data);
          void modify_case_terrain(int i, int choice);
          void fix_index(int i);
          Case_Real_data score_final(float socre);
          void save_cases();
          point_return plotme();
          point_return plotmetoo();
};

#endif

