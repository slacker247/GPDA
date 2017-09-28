#include<iostream.h>
#include<fstream.h>
#include<string.h>
#include<stdio.h>

//member functions for caseclass


//----------------------------------------------------------------

#include "cases.h"

//------------------------Constructor------------------------------

//builds case base from file
//sets up linked lists

CASECLASS::CASECLASS()
{
   head = new Node;
   tail = new Node;
   
   head_best = new Node;
   tail_best = new Node;

   head->prev = NULL;
   head->next = tail;

   tail->prev = head;
   tail->next = NULL;

   head_best->prev = NULL;
   head_best->next = tail_best;
  
   tail_best->prev = head_best;
   tail_best->next = NULL;

//get the cases from the file/case base and add them to linked list
   build_cases(); 
//Set the indexes of new cases to make all follow same metrics
   fix_index(1);
 
}

//--------------------------Build Cases--------------------------

//reads cases from file one at a time and calls an add case
//function to add them to a node in the list
//when inputting strings, care is taken to pad the array
//if string does not fill it up and remove garbage
//from the end of previous data and after the string

void CASECLASS::build_cases()
{ 
  Case_Index_data temp;
  Case_Real_data temp2;

  char tempcrap;

  int terrain_type_num;
  int i = 0;  //for string padding
  ifstream in;

  in.open("coas.dat");

  in >> temp.max_threatsV;

//input case data

   while ( !(in.eof()) )
   {

       in >> temp.terrainV;
       in >> temp.slopeV;
       in >> temp.timingV;
       in >> temp.strengthV;
       in >> temp.timeV;
       in >> temp.commoditiesV;
       in >> temp.equipageV;
       in >> temp.fuelV;
       in >> temp.subsidiariesV;
       in >> temp.expec_down_commV;
       in >> temp.unexpec_down_commV;
       in >> temp.natural_elementsV;
       in >> temp.mechanicalV;
       in >> temp.distanceV;
      
//node string input
       i = 0;  
      
       in.get(tempcrap);

       if ((tempcrap >= 'A') && (tempcrap <= 'Z'))
          temp2.nodes[i] = tempcrap;
       else
          in.get(temp2.nodes[i]);

       while (temp2.nodes[i] != '\n')
       {
          i++; 
          in.get(temp2.nodes[i]);
       }
 
       while (i < MAX1)
       {
           temp2.nodes[i] = '\0';
           i++;
       }

//edges value inputs
       i = 0;  
       
       in.get(temp2.edges[i]);
      
       while (temp2.edges[i] != '\n')
       {
          i++; 
          in.get(temp2.edges[i]);
       }
      
       while (i < MAX1)
       {
           temp2.edges[i] = '\0';
           i++;
       }

       in >> temp2.departure_time;
       in >> temp2.ETA;
       in >> temp2.travel_time;
       in >> terrain_type_num;

       switch (terrain_type_num)
       {
          case 0: temp2.terrain_type = urban; break;
          case 1: temp2.terrain_type = range_land; break;
          case 2: temp2.terrain_type = barren_land; break;
          case 3: temp2.terrain_type = tundra; break;
          case 4: temp2.terrain_type = ice_snow; break;
          case 5: temp2.terrain_type = wetland; break;
          case 6: temp2.terrain_type = forest; break;
       };

       in >> temp2.slope;
     
       i = 0; 
       in.get(tempcrap);

       in.get(temp2.threats.threat_locations[i]);
       
       while (temp2.threats.threat_locations[i] != '\n')
       {
          i++;
          in.get(temp2.threats.threat_locations[i]);
       }
       
       while (i <= MAX2)
       {
           temp2.threats.threat_locations[i] = '\0';
           i++;
       }
      

       in >> temp2.num_of_threats;
       in >> temp2.strength_info.manpower;
       in >> temp2.strength_info.firepower;
       in >> temp2.distance;

//commodity string input 
       i = 0; 
       in.get(tempcrap);

       in.get(temp2.commodity_locations[i]);
       
       while (temp2.commodity_locations[i] != '\n')
       {
          i++;
          in.get(temp2.commodity_locations[i]);
       }

       while (i <= MAX2)
       {
           temp2.commodity_locations[i] = '\0';
           i++;
       }
      
//fuel string input 
       i = 0;

       in.get(temp2.fuel_locations[i]);
       
       while (temp2.fuel_locations[i] != '\n')
       {
          i++;
          in.get(temp2.fuel_locations[i]);
       }

       while (i <= MAX2)
       {
           temp2.fuel_locations[i] = '\0';
           i++;
       }
      
//equipage string input 
       i = 0;
      
       in.get(temp2.equipage_locations[i]);
       
       while (temp2.equipage_locations[i] != '\n')
       {
          i++;
          in.get(temp2.equipage_locations[i]);
       }

       while (i <= MAX2)
       {
           temp2.equipage_locations[i] = '\0';
           i++;
       }
     
//subsidiary input
       i = 0;
      
       in.get(temp2.subsidiary_locations[i]);
       
       while (temp2.subsidiary_locations[i] != '\n')
       {
          i++;
          in.get(temp2.subsidiary_locations[i]);
       }

       while (i <= MAX2)
       {
           temp2.subsidiary_locations[i] = '\0';
           i++;
       }
     

       in >> temp2.chance_expec_down_comm;
       in >> temp2.chance_unexpec_down_comm;
       in >> temp2.chance_natural_elements;
       in >> temp2.chance_mechanical;
       
       add_cases(temp2, temp);

       in >> temp.max_threatsV;
   }
}

//-----------------------Add Cases---------------------------------

//after one case's information is read in, this fuction
//creates a new node, inserts the new information/case
//and adds it to the end of the list

void CASECLASS::add_cases(Case_Real_data info, Case_Index_data case_data)
{
   Node *temp;
   Node *addnode;
   
   addnode = new Node;

   addnode->index_data = case_data;
   addnode->mission_data = info;
 
   if (tail->prev == head)
   {
      addnode->next = tail;
      addnode->prev = head;
      head->next = addnode;
      tail->prev = addnode;
   }

   else
   {
      temp = tail->prev;

      tail->prev = addnode;
      addnode->prev = temp;
      temp->next = addnode;
      addnode->next = tail;
   }

}

//------------------Add Best Cases to list to repair-----------

//Different add function to add the best cases to their own
//separate linked list

void CASECLASS::add_best_cases(Case_Real_data info, Case_Index_data case_data)
{
   Node *temp;
   Node *addnode;
   
   addnode = new Node;
   
   addnode->index_data = case_data;
   addnode->mission_data = info;

   if (tail_best->prev == head_best)
   {
      addnode->next = tail_best;
      addnode->prev = head_best;
      head_best->next = addnode;
      tail_best->prev = addnode;
   }

   else
   {
      temp = tail_best->prev;

      tail_best->prev = addnode;
      addnode->prev = temp;
      temp->next = addnode;
      addnode->next = tail_best;
   }
}


//----------------------Set Case Values--------------------------------
  
//Based on index values of a case, calculates a generic score
//to be compared to the value of the specified weights and to 
//other cases to decide the best
 
void CASECLASS::set_case_values(WeightCriteria Weights, int i)
{
   float add_points;
   float total_points = 0;
   float total_match_points = 0;
   Node *pointer;
   Node *end;
   int j = 0;

   if ( i == 1)
   {
      pointer = head->next;
      end = tail;
   }
   if (i == 2)
   {
      pointer = head_best->next;
      end = tail_best;
   }

   j = 0;

   while (pointer != end)
   {

//*****max_threats switch*/
     
     switch (pointer->index_data.max_threatsV)
     {
        case 0: add_points = 1; break;
        case 1: add_points = .75; break;
        case 2: add_points = .50; break;
        case 3: add_points = .25; break;
        case 4: add_points = 0; break;
     };

     total_points += add_points;

     if(Weights.max_threatsW == add_points)
        total_match_points += add_points;

//*****terrain switch*/
     
     switch (pointer->index_data.terrainV)
     {
        case 0: add_points = 1; break;
        case 1: add_points = .86; break;
        case 2: add_points = .72; break;
        case 3: add_points = .58; break;
        case 4: add_points = .44; break;
        case 5: add_points = .3; break;
        case 6: add_points = .16; break;
     };

     total_points += add_points;

     if(Weights.terrainW == add_points)
       total_match_points += add_points;

//*****slope switch*/

     switch (pointer->index_data.slopeV)
     {
        case 0: add_points = 1; break;
        case 1: add_points = .8; break;
        case 2: add_points = .6; break;
        case 3: add_points = .4; break;
        case 4: add_points = .2; break;
        case 5: add_points = 0; break;
     };

     total_points += add_points;

     if(Weights.slopeW == add_points)
       total_match_points += add_points;

//*****strength switch*/

     switch (pointer->index_data.strengthV)
     {
        case 0: add_points = 1; break;
        case 1: add_points = .75; break;
        case 2: add_points = .5; break;
        case 3: add_points = .25; break;
        case 4: add_points = .0; break;
     };

     total_points += add_points;

     if(Weights.strengthW == add_points)
       total_match_points += add_points;

//*****time switch*/

     switch (pointer->index_data.timeV)
     {
        case 0: add_points = 1; break;
        case 1: add_points = .5; break;
        case 2: add_points = .0; break;
     };

     total_points += add_points;

     if(Weights.timeW == add_points)
       total_match_points += add_points;

//*****commodities switch*/

     switch (pointer->index_data.commoditiesV)
     {
        case 0: add_points = 1; break;
        case 1: add_points = .7; break;
        case 2: add_points = .4; break;
        case 3: add_points = 0; break;
     };

     total_points += add_points;

     if(Weights.commoditiesW == add_points)
       total_match_points += add_points;

//*****fuel switch*/

     switch (pointer->index_data.fuelV)
     {
        case 0: add_points = 1; break;
        case 1: add_points = .7; break;
        case 2: add_points = .4; break;
        case 3: add_points = 0; break;
     };

     total_points += add_points;

     if(Weights.fuelW == add_points)
       total_match_points += add_points;

//*****subsidiaries switch*/

     switch (pointer->index_data.subsidiariesV)
     {
        case 0: add_points = 1; break;
        case 1: add_points = .7; break;
        case 2: add_points = .4; break;
        case 3: add_points = 0; break;
     };

     total_points += add_points;

     if(Weights.subsidiariesW == add_points)
       total_match_points += add_points;

//*****equipage switch*/

     switch (pointer->index_data.equipageV)
     {
        case 0: add_points = 1; break;
        case 1: add_points = .7; break;
        case 2: add_points = .4; break;
        case 3: add_points = 0; break;
     };

     total_points += add_points;

     if(Weights.equipageW == add_points)
       total_match_points += add_points;


/*****expected_down_comm switch*/

     switch (pointer->index_data.expec_down_commV)
     {
        case 0: add_points = 1; break;
        case 1: add_points = .8; break;
        case 2: add_points = .6; break;
        case 3: add_points = .4; break;
        case 4: add_points = .2; break;
        case 5: add_points = .1; break;
        case 6: add_points = .05; break;
        case 7: add_points = 0; break;
     };

     total_points += add_points;

     if(Weights.expected_down_commW == add_points)
       total_match_points += add_points;
   

//*****unexpected_down_comm switch*/

     switch (pointer->index_data.unexpec_down_commV)
     {
        case 0: add_points = 1; break;
        case 1: add_points = .8; break;
        case 2: add_points = .6; break;
        case 3: add_points = .4; break;
        case 4: add_points = .2; break;
        case 5: add_points = .1; break;
        case 6: add_points = .05; break;
        case 7: add_points = 0; break;
     };
     
     total_points += add_points;

     if(Weights.unexpected_down_commW == add_points)
       total_match_points += add_points;

//*****natural_elements switch*/

     switch (pointer->index_data.natural_elementsV)
     {
        case 0: add_points = 1; break;
        case 1: add_points = .8; break;
        case 2: add_points = .6; break;
        case 3: add_points = .4; break;
        case 4: add_points = .2; break;
        case 5: add_points = .1; break;
        case 6: add_points = .05; break;
        case 7: add_points = .0; break;
     };

     total_points += add_points;

     if(Weights.natural_elementsW == add_points)
       total_match_points += add_points;

//*****mechanical switch*/
     
     switch (pointer->index_data.mechanicalV)
     {
        case 0: add_points = 1; break;
        case 1: add_points = .8; break;
        case 2: add_points = .6; break;
        case 3: add_points = .4; break;
        case 4: add_points = .2; break;
        case 5: add_points = .1; break;
        case 6: add_points = .05; break;
        case 7: add_points = 0; break;
        default: add_points = 0;
     };
     
     total_points += add_points;

     if(Weights.mechanicalW == add_points)
       total_match_points += add_points;

//*****distance switch*/

     switch (pointer->index_data.distanceV)
     {
        case 0: add_points = 1; break;
        case 1: add_points = .5; break;
        case 3: add_points = 0; break;
     };

     total_points += add_points;

     if(Weights.distanceW == add_points)
       total_match_points += add_points;

//*****timing*/

     //if (pointer->index_data.timingV == Weights.timingW)
      //  total_match_points++;

     pointer->case_total_points = total_points;
     pointer->case_total_match_points = total_match_points;

     if (i == 1)
     {
       original_points.total_points[j] = total_points;
       original_points.match_points[j] = total_match_points;
     }

     pointer = pointer->next;
     total_points = 0;
     total_match_points = 0;
     j++;
   }
}

//-----------------------------Rate Cases--------------------------

//This function traverses the list of cases to find the three best
//matches for inspection and selection

//While three cases have not yet been selected, the function
//finds the case with a score closest to that of the given
//criteria that has not yet been selected

void CASECLASS::rate_cases(float score)
{
   float case_comparison_score = score;  //total of user entered weights
   Node *case_pointer;                   //points to current best case
   Node *pointer;                        //traverses through specified list
   Node *end;                            //set to tail of specified list 
   float prev_difference = 100;         //used in comparison to find best
    int choosen = 0;                     //total best cases found (max 3)
    int place = 0;                       //location in linked list
    int case_place = 0;                  //location best match was found in list
    int case_place1 = -1;                //location of first best match found 
    int case_place2 = -1;                //location of second best match found
   Case_Index_data best_case;            //Real data stored in best case
   float difference;                     //difference between case match score
                                             //and user entered criteria
    int new_case = 0;                    //tells if case to add is new
                                             //in case there are less than
                                             //three to choose from

/***
    While three cases haven't been found yet, continue to travers
    the linked list of cases from the case base to find the best.
    The first found is the best case based on user criteria in the
    repository, the second is second best etc.  The three cases, or
    less if three aren't available from the base, are placed in
    a separate linked list of best cases to be modified, graphed,
    saved etc.
***/

   while (choosen < 3)
   {
      pointer = head->next;
 
      while (pointer != tail)
      {
         difference = pointer->case_total_match_points - case_comparison_score;
         if (difference < 0)
             difference = difference * -1;

         if ((difference < prev_difference) && (case_place1 != place) &&
             (case_place2 != place))
         {
            prev_difference = difference;
            case_pointer = pointer; 
            case_place = place; 
            new_case = 1;
         }
 
         pointer = pointer ->next;
         place++;
      }
     
      if (choosen == 0)
         case_place1 = case_place;
      if (choosen == 1)
         case_place2 = case_place;

      if (new_case == 1)  
         add_best_cases(case_pointer->mission_data, case_pointer->index_data);
      choosen++;
      prev_difference = 100; 
      place = 0;      
      new_case = 0;
   }
}

//----------------------------score final----------------------

//Once the best three cases have been selected, output, and 
//manipulated as the user wants, the score final function
//finds the case of those three that best matches criteria
//This will be the selected case for it is closest to the
//original criteria and to what the user may later
//decide he wants

Case_Real_data CASECLASS::score_final(float score)
{
   Node *pointer;                        //pointer to traverse list
   Node *best_pointer = new Node;        //node holds best case
   Case_Real_data final_case;            //holds data of best case
   int j = 0;                            //loop control variable
   float difference;                     //difference between case match score
                                             //and user entered criteria
   float prev_difference = 100;          //previous differnece
   float case_comparison_score = score;  //added total of user entered
                                             //weighted values

   pointer = head_best->next;

//while not at the end of the list, find the case closest to the 
//entered criteria and select it as best case

   while (pointer != tail_best)
   {
 
      difference = pointer->case_total_match_points - case_comparison_score;

      if (difference < 0)
          difference = difference * -1;

      if (difference < prev_difference)
      {
         prev_difference = difference;
         best_pointer = pointer; 
         final_case = pointer->mission_data;
      }

      modified_points.match_points[j] = pointer->case_total_match_points;

      pointer =  pointer->next;
      j++;
   }

//make the node with the best case the only entry in the best linked list

   head_best->next = best_pointer;
   best_pointer->prev = head_best;
   best_pointer->next = tail_best;
   tail_best->prev = best_pointer;

//return the information of the best case to output
   return final_case;

}
//----------------------------repair cases----------------------

//Repair Cases provides the information to set up the Test form
//allowing the viewing of the three best case selection
//and their modification

//If three good cases were found, pertenant information is
//sent to the form initialization code,
//if less than three are found, the form is notified so
//NONE can be output rather than case information

 
void CASECLASS::repair_cases()
{
   Node *pointer;
   int i = 0;

   void initialize_test_form(Case_Real_data mission, int i);
   void empty_coa(int i); 

   pointer = head_best->next;

   while ((pointer != tail_best) && (i < 3))
   {
      initialize_test_form(pointer->mission_data, i); 

      pointer = pointer->next; 
      i++;
   }

   if (i < 3)
   {
      empty_coa(i);
   }

}

//------------------------------Modify Cases int------------------------

//This function allows for the modification of case information in
//the linked list of integer type.
//The appropriate form (time changer) sends the field to be modified
//and the value to change it to. This function then updates the correct
//field in the Case Real data

void CASECLASS::modify_case_int(int i, int input1, int change_this_data)
{
   int location = i;
   int place = 1;

   Node *pointer;
   
   pointer = head_best->next;

   while ((place < location) && (pointer != tail_best))
   {
      pointer = pointer->next;
      place++;
   }

   switch (change_this_data)
   {
      case 1: pointer->mission_data.departure_time = input1; break;
      case 2: if (pointer->mission_data.ETA > 
                  pointer->mission_data.departure_time)
                  pointer->mission_data.ETA = input1; break;


      case 7: pointer->mission_data.num_of_threats = input1; break;
   };
}

//--------------------------Modify Case Float--------------------------

//This function allows for the modification of case information in
//the linked list of float type.
//The appropriate form (float changer) sends the field to be modified
//and the value to change it to. This function then updates the correct
//field in the Case Real data


void CASECLASS::modify_case_float(int i, float input1, const char *get_str, int change_this_data)
{
   int location = i;
   int place = 1;
   int hours = 0;
   int minutes = 0;
   float float_hours = 0;
   float float_mins = 0;

   Node *pointer;
   
   pointer = head_best->next;

   while ((place < location) && (pointer != tail_best))
   {
      pointer = pointer->next;
      place++;
   }

   switch (change_this_data)
   {
      case 3:  pointer->mission_data.travel_time = input1; break;
      case 5:  pointer->mission_data.slope = input1; break;
      case 8:  pointer->mission_data.strength_info.manpower = input1; break;
      case 9:  pointer->mission_data.strength_info.firepower= input1; break;
      case 10: pointer->mission_data.distance = input1; break;
      case 15: pointer->mission_data.chance_unexpec_down_comm = input1; break;
      case 16: pointer->mission_data.chance_expec_down_comm = input1; break;
      case 17: pointer->mission_data.chance_natural_elements = input1; break;
      case 18: pointer->mission_data.chance_mechanical = input1; break;
   };

   if (change_this_data == 3)
   {

   }
}

//--------------------------Modify Case Add Char-------------------------

//This function allows for the modification of case information in
//the linked list of char/string type.
//The appropriate form (add changer) sends the field to be modified
//and the value to change it to. This function then updates the correct
//field in the Case Real data

//This function deals with adding a char to a character array
//of locations along the route

char_return CASECLASS::modify_case_add_char(int i, const char *input1, char input2, int change_this_data)
{
   int location = i;
   int place = 1;
   char chtemp[MAX2];
   int copy = 0;

   Node *pointer;
   
   pointer = head_best->next;

   for (int u=0; u < MAX2; u++)
       chtemp[u] = '\0';

   while ((place < location) && (pointer != tail_best))
   {
      pointer = pointer->next;
      place++;
   }
   
   switch (change_this_data)
   {
      case 6:  for (int x = 0; x < MAX2; x++)
               if (input2 == pointer->mission_data.threats.threat_locations[x])
                  copy = 1;
               
               if (copy == 1)
               {                 
                  sprintf(chtemp, "%s", pointer->mission_data.threats.
                  threat_locations); 
                  pointer->mission_data.threats.threat_locations = chtemp;
               }
  
               else
               {
                  sprintf(chtemp, "%s%s", pointer->mission_data.threats.
                  threat_locations, input1); 
                  pointer->mission_data.threats.threat_locations = chtemp;
                  pointer->mission_data.num_of_threats++;
               } 
               break;

      case 11: copy = 0; 
               for (int z = 0; z < MAX2; z++)
               if (input2 == pointer->mission_data.commodity_locations[z])
                  copy = 1; 

               if (copy == 1)
               {
                  sprintf(chtemp,"%s",pointer->mission_data.
                  commodity_locations);
                  pointer->mission_data.commodity_locations = chtemp;
               }
 
               else 
               {
                  sprintf(chtemp,"%s%s",pointer->mission_data.
                  commodity_locations, input1);
                  pointer->mission_data.commodity_locations = chtemp;
               }
               break;

      case 12: copy = 0;
               for (int x = 0; x < MAX2; x++)
               if (input2 == pointer->mission_data.fuel_locations[x])
                  copy = 1; 

               if (copy == 1)
               {
                  sprintf(chtemp, "%s", pointer->mission_data.fuel_locations);
                  pointer->mission_data.fuel_locations = chtemp;
               }
 
               else
               {
                  sprintf(chtemp, "%s%s", pointer->mission_data.fuel_locations,
                  input1); 
                  pointer->mission_data.fuel_locations = chtemp;
               } 
               break;

      case 13: copy = 0;
               for (int x = 0; x < MAX2; x++)
               if (input2 == pointer->mission_data.equipage_locations[x])
                  copy = 1; 

               if (copy == 1)
               {
                  sprintf(chtemp,"%s",pointer->mission_data.
                  equipage_locations);
                  pointer->mission_data.equipage_locations = chtemp;
               }
 
               else 
               { 
                  sprintf(chtemp,"%s%s",pointer->mission_data.
                  equipage_locations, input1);
                  pointer->mission_data.equipage_locations = chtemp;
               } 
               break;

      case 14: copy = 0; 
               for (int x = 0; x < MAX2; x++)
               if (input2 == pointer->mission_data.subsidiary_locations[x])
                  copy = 1; 

               if (copy == 1)
               {
                  sprintf(chtemp, "%s", pointer->mission_data.
                  subsidiary_locations);
                  pointer->mission_data.subsidiary_locations = chtemp;
               }

               else 
               {
                  sprintf(chtemp, "%s%s", pointer->mission_data.
                  subsidiary_locations, input1);
                  pointer->mission_data.subsidiary_locations = chtemp;
               }
               break;
   };
  
   returnme.char_temp = chtemp;

   return returnme;
}

//----------------------Modify Case Rem Char-----------------------------

//This function allows for the modification of case information in
//the linked list of char/string type.
//The appropriate form (rem changer) sends the field to be modified
//and the value to change it to. This function then updates the correct
//field in the Case Real data

//This function deals with removing a char from a character array
//of locations along the route

char_return CASECLASS::modify_case_rem_char(int i, const char *input1, char input2, int change_this_data)
{
   int location = i;
   int place = 1;
   char chtemp[MAX2];
   char temp_char;
   char_return returnme;
    int copy = 0;

   Node *pointer;
 
   pointer = head_best->next;

   for (int u=0; u < MAX2; u++)
       chtemp[u] = '\0';
   
   while ((place < location) && (pointer != tail_best))
   {
      pointer = pointer->next;
      place++;
   }
   
   switch (change_this_data)
   {
      case 6: copy = 0; 
              for (int z = 0; z < MAX2; z++)
              if (input2 == pointer->mission_data.threats.threat_locations[z])
                 copy = 1; 

              if (copy != 1)
              {
                 sprintf(chtemp,"%s",pointer->mission_data.
                    threats.threat_locations);
                 pointer->mission_data.threats.threat_locations = chtemp;
              }
 
              else 
              {
              for(int lcv = 0; lcv < MAX2; lcv++)
               {
                  if (pointer->mission_data.threats.threat_locations[lcv] ==
                      input2)
                  {
                     pointer->mission_data.threats.threat_locations[lcv] = ' ';
                  }
               }
               sprintf(chtemp, "%s", pointer->mission_data.threats.
                       threat_locations);
               pointer->mission_data.threats.threat_locations = chtemp;
               }
               break;
 
      
      case 11: copy = 0; 
               for (int z = 0; z < MAX2; z++)
               if (input2 == pointer->mission_data.commodity_locations[z])
                  copy = 1; 

               if (copy != 1)
               {
                  sprintf(chtemp,"%s",pointer->mission_data.
                  commodity_locations);
                  pointer->mission_data.commodity_locations = chtemp;
               }
 
               else 
               {
                  for(int lcv = 0; lcv < MAX2; lcv++)
                  {
                  if (pointer->mission_data.commodity_locations[lcv] ==
                      input2)
                  {
                   pointer->mission_data.commodity_locations[lcv] = ' ';
                  }
                  }
                  sprintf(chtemp, "%s", pointer->mission_data.
                          commodity_locations);
                  pointer->mission_data.commodity_locations = chtemp;
               }
               break;

     case 12: copy = 0; 
               for (int z = 0; z < MAX2; z++)
               if (input2 == pointer->mission_data.fuel_locations[z])
                  copy = 1; 

               if (copy != 1)
               {
                  sprintf(chtemp,"%s",pointer->mission_data.
                  fuel_locations);
                  pointer->mission_data.fuel_locations = chtemp;
               }
 
               else 
               {
                  for(int lcv = 0; lcv < MAX2; lcv++)
                  {
                  if (pointer->mission_data.fuel_locations[lcv] ==
                      input2)
                  {
                   pointer->mission_data.fuel_locations[lcv] = ' ';
                  }
                  }
                  sprintf(chtemp, "%s", pointer->mission_data.
                          fuel_locations);
                  pointer->mission_data.fuel_locations = chtemp;
               }
               break;

     case 13: copy = 0; 
               for (int z = 0; z < MAX2; z++)
               if (input2 == pointer->mission_data.equipage_locations[z])
                  copy = 1; 

               if (copy != 1)
               {
                  sprintf(chtemp,"%s",pointer->mission_data.
                  equipage_locations);
                  pointer->mission_data.equipage_locations = chtemp;
               }
 
               else 
               {
                  for(int lcv = 0; lcv < MAX2; lcv++)
                  {
                  if (pointer->mission_data.equipage_locations[lcv] ==
                      input2)
                  {
                   pointer->mission_data.equipage_locations[lcv] = ' ';
                  }
                  }
                  sprintf(chtemp, "%s", pointer->mission_data.
                          equipage_locations);
                  pointer->mission_data.equipage_locations = chtemp;
               }
               break;

     case 14: copy = 0; 
               for (int z = 0; z < MAX2; z++)
               if (input2 == pointer->mission_data.subsidiary_locations[z])
                  copy = 1; 

               if (copy != 1)
               {
                  sprintf(chtemp,"%s",pointer->mission_data.
                  subsidiary_locations);
                  pointer->mission_data.subsidiary_locations = chtemp;
               }
 
               else 
               {
                  for(int lcv = 0; lcv < MAX2; lcv++)
                  {
                  if (pointer->mission_data.subsidiary_locations[lcv] ==
                      input2)
                  {
                   pointer->mission_data.subsidiary_locations[lcv] = ' ';
                  }
                  }
                  sprintf(chtemp, "%s", pointer->mission_data.
                          subsidiary_locations);
                  pointer->mission_data.subsidiary_locations = chtemp;
               }
               break;
   }; 
   
   returnme.char_temp = chtemp;
  
   return returnme;

}

//--------------------------Modify Case Terrain-----------------------

//This function allows for the modification of case information in
//the linked list of TerrainE type (the enumerated type of
//terrain values.
//The appropriate form (terrain changer) sends the field to be modified
//and the value to change it to. This function then updates the correct
//field in the Case Real data

void CASECLASS::modify_case_terrain(int i, int choice)
{
   int location = i;
   int place = 1;
   int terrain_choice = choice;
   Node *pointer = head_best->next;

   while ((place < location) && (pointer != tail_best))
   {
      pointer = pointer->next;
      place++;
   }
   
   switch (terrain_choice)
   {
      case 1:  pointer->mission_data.terrain_type = urban; break ;
      case 2:  pointer->mission_data.terrain_type = range_land; break;
      case 3:  pointer->mission_data.terrain_type = barren_land; break;
      case 4:  pointer->mission_data.terrain_type = tundra; break;
      case 5:  pointer->mission_data.terrain_type = ice_snow; break;
      case 6:  pointer->mission_data.terrain_type = wetland; break;
      case 7:  pointer->mission_data.terrain_type = forest; break;
   };

}

//---------------------------Fix Index----------------------------

//Fix Index uses Case Real Data values to calculate the
//general index value for each of the defined fields.

//This is done to all original cases once they are read in and after
//cases are modified.  Calculation here allows for consistency in
//case index values that could be mis-created by a case creator or user.
//Having consistent values allows for better indexing of the cases
//and better selection of the best cases.

void CASECLASS::fix_index(int i)
{ 
   int total_nodes = 0;       //total nodes along route
   int needed_nodes = 0;      //total nodes of a type of location
   int ratio;                 //ration of total nodes to needed nodes
   float float_temp;          //temporary float variable
   float strength_average;    //average of manpower and firepower variables
   int int_temp;              //temporary integer variable
   Node *pointer;             //pointer to traverse given list
   Node *end;                 //end pointer of given list
   Node *temp;                //temporary pointer
   Node *previous;            //pointer to previous node of specified node
   Node *nextone;             //ponter to next node of specified node
   int deleteme = 0;          //flag variable to tell whether to delete node


//specifies case base linked list or best cases linked list
   if (i == 1)
   {
      pointer = head->next;
      end = tail;
      temp = head;
   }
   if (i == 2)
   {
      pointer = head_best->next;
      end = tail_best;
      temp = head_best;
   }

//finds total nodes is route
   for (int y = 0; y < MAX1; y++)
   {
      if ((pointer->mission_data.nodes[y] >= 'A') &&
          (pointer->mission_data.nodes[y] <= 'Z'))
      total_nodes++;
   } 

   while (pointer != end)
   {

//Conversion of number of threats to index value
      
      switch (pointer->mission_data.num_of_threats)
      {
         case 0: pointer->index_data.max_threatsV = 0; break;
         case 1:
         case 2: pointer->index_data .max_threatsV = 1; break;
         case 3:
         case 4: pointer->index_data.max_threatsV = 2; break;
         case 5:
         case 6: pointer->index_data.max_threatsV = 3; break;
         default: pointer->index_data.max_threatsV = 4; break;
      }

//Conversion of terrain to index value 

      switch (pointer->mission_data.terrain_type)
      {
         case 0: pointer->index_data.terrainV = 0; break;
         case 1: pointer->index_data.terrainV = 1; break;
         case 2: pointer->index_data.terrainV = 2; break;
         case 3: pointer->index_data.terrainV = 3; break;
         case 4: pointer->index_data.terrainV = 4; break;
         case 5: pointer->index_data.terrainV = 5; break;
         case 6: pointer->index_data.terrainV = 6; break;
      }

//Conversion of slope to index value

      float_temp = pointer->mission_data.slope;
     
      if (float_temp <= 2)
          pointer->index_data.slopeV = 0;
      else if (float_temp <=5)    
          pointer->index_data.slopeV = 1;
      else if (float_temp <= 8)
          pointer->index_data.slopeV = 2;
      else if (float_temp <= 11)
          pointer->index_data.slopeV = 3;
      else if (float_temp <= 14)
          pointer->index_data.slopeV = 4;
      else if (float_temp >= 15)
          pointer->index_data.slopeV = 5;

//Conversion of timing to index value

//Conversion of strength to index value

      strength_average = (pointer->mission_data.strength_info.manpower +
         pointer->mission_data.strength_info.firepower) / 2;

      if (strength_average <= 40)
         pointer->index_data.strengthV = 0;
      else if (strength_average <= 80)
         pointer->index_data.strengthV = 1;
      else if (strength_average <= 100)
         pointer->index_data.strengthV = 2;
      else if (strength_average > 100)
         pointer->index_data.strengthV = 3;
      else pointer->index_data.strengthV = 4;
       

//Conversion of travel time to index

      float_temp = pointer->mission_data.travel_time;

      if (float_temp <= 24)
         pointer->index_data.timeV = 0;
      else if (float_temp <=48)   
         pointer->index_data.timeV = 1;
      else 
         pointer->index_data.timeV = 2;

//Conversion of Commodities to an index value

      needed_nodes = 0;

      for (int w = 0; w < MAX2; w++)
      {  
          if ((pointer->mission_data.commodity_locations[w] >= 'A') &&
              (pointer->mission_data.commodity_locations[w] <= 'Z'))
          needed_nodes = needed_nodes + 1;
      }
      
      ratio = total_nodes / needed_nodes; 
      
      switch (ratio)
      {
         case 0:
         case 1: pointer->index_data.commoditiesV = 0; break;
         case 2:
         case 3:
         case 4: pointer->index_data.commoditiesV = 1; break;
         case 5:
         case 6:
         case 7: pointer->index_data.commoditiesV = 2; break;
         default: pointer->index_data.commoditiesV = 3; break;
      }; 

//Conversion of Equipage

      needed_nodes = 0;

      for (int w = 0; w < MAX2; w++)
      {  
          if ((pointer->mission_data.equipage_locations[w] >= 'A') &&
              (pointer->mission_data.equipage_locations[w] <= 'Z'))
          needed_nodes = needed_nodes + 1;
      }

      ratio = total_nodes / needed_nodes; 

      switch (ratio)
      {  
         case 0:
         case 1: pointer->index_data.equipageV = 0; break;
         case 2:
         case 3:
         case 4: pointer->index_data.equipageV = 1; break;
         case 5:
         case 6:
         case 7: pointer->index_data.equipageV = 2; break;
         default: pointer->index_data.equipageV = 3; break;
      }; 

//Conversion of Fuel

      needed_nodes = 0;
      ratio = 0;

      for (int w = 0; w < MAX2; w++)
      {  
          if ((pointer->mission_data.fuel_locations[w] >= 'A') &&
              (pointer->mission_data.fuel_locations[w] <= 'Z'))
          needed_nodes = needed_nodes + 1;
      }

      ratio = total_nodes / needed_nodes; 

      switch (ratio)
      {
         case 0:
         case 1: pointer->index_data.fuelV = 0; break;
         case 2:
         case 3:
         case 4: pointer->index_data.fuelV = 1; break;
         case 5:
         case 6:
         case 7: pointer->index_data.fuelV = 2; break;
         default: pointer->index_data.fuelV = 3; break;
      };

//Subsidiary locations

      needed_nodes = 0;

      for (int z = 0; z < MAX2; z++)
      {  
          if ((pointer->mission_data.subsidiary_locations[z] >= 'A') &&
              (pointer->mission_data.subsidiary_locations[z] <= 'Z'))
          needed_nodes = needed_nodes + 1;
      }

      ratio = total_nodes / needed_nodes;
 
      switch (ratio)
      {
         case 0:
         case 1: pointer->index_data.subsidiariesV = 0; break;
         case 2:
         case 3: 
         case 4: pointer->index_data.subsidiariesV = 1; break;
         case 5:
         case 6:
         case 7: pointer->index_data.subsidiariesV = 2; break;
         default: pointer->index_data.subsidiariesV = 3; break;
      };

//Conversion of Expected Down Comm

      float_temp = pointer->mission_data.chance_expec_down_comm;

      if (float_temp == 0)
         pointer->index_data.expec_down_commV = 0;
      else if (float_temp <= 20)
         pointer->index_data.expec_down_commV = 1;
      else if (float_temp <= 40)
         pointer->index_data.expec_down_commV = 2;
      else if (float_temp <= 60)
         pointer->index_data.expec_down_commV = 3;
      else if (float_temp <= 80)
         pointer->index_data.expec_down_commV = 4;
      else if (float_temp < 100)
         pointer->index_data.expec_down_commV = 5;
      else if (float_temp == 100)
         deleteme = 1;

//Conversion of Unexpected Down Comm

      float_temp = pointer->mission_data.chance_unexpec_down_comm;

      if (float_temp == 0)
         pointer->index_data.unexpec_down_commV = 0;
      else if (float_temp <= 20)
         pointer->index_data.unexpec_down_commV = 1;
      else if (float_temp <= 40)
         pointer->index_data.unexpec_down_commV = 2;
      else if (float_temp <= 60)
         pointer->index_data.unexpec_down_commV = 3;
      else if (float_temp <= 80)
         pointer->index_data.unexpec_down_commV = 4;
      else if (float_temp < 100)
         pointer->index_data.unexpec_down_commV = 5;
      else if (float_temp == 100)
         deleteme = 1;

//Conversion of Natural Elements to index values

      float_temp = pointer->mission_data.chance_natural_elements;

      if (float_temp == 0)
         pointer->index_data.natural_elementsV = 0;
      else if (float_temp <= 20)
         pointer->index_data.natural_elementsV = 1;
      else if (float_temp <= 40)
         pointer->index_data.natural_elementsV = 2;
      else if (float_temp <= 60)
         pointer->index_data.natural_elementsV = 3;
      else if (float_temp <= 80)
         pointer->index_data.natural_elementsV = 4;
      else if (float_temp < 100)
         pointer->index_data.natural_elementsV = 5;
      else if (float_temp == 100)
         deleteme = 1;

//Conversion of Mechanical to index value

      float_temp = pointer->mission_data.chance_mechanical;

      if (float_temp == 0)
         pointer->index_data.mechanicalV = 0;
      else if (float_temp <= 20)
         pointer->index_data.mechanicalV = 1;
      else if (float_temp <= 40)
         pointer->index_data.mechanicalV = 2;
      else if (float_temp <= 60)
         pointer->index_data.mechanicalV = 3;
      else if (float_temp <= 80)
         pointer->index_data.mechanicalV = 4;
      else if (float_temp < 100)
         pointer->index_data.mechanicalV = 5;
      else if (float_temp == 100)
         deleteme = 1;
      else pointer->index_data.mechanicalV = 5;


/***
     This big mess was supposed to eliminate any cases with a failure 
     rate of 100%, but it is not working right, typical
***/

      if (deleteme == 1)
      {
         if (temp == head)
         if (pointer->prev == head)
            {
               head->next = pointer->next;
               pointer->next->prev = head;
               pointer = pointer->prev;
            }
         else
            {
              previous = pointer->prev;
              nextone = pointer->next;
              temp = pointer;

              pointer = previous;
              pointer->next = nextone;
              nextone->prev = previous;
       
              temp->next = NULL;
              temp->prev = NULL; 
              delete(temp);
            }

         if (temp == head_best)
         if (pointer->prev == head_best)
            {
               head_best->next = pointer->next;
               pointer->next->prev = head_best;
               pointer = pointer->prev;
            }
         else
            {
              previous = pointer->prev;
              nextone = pointer->next;
              temp = pointer;

              pointer = previous;
              pointer->next = nextone;
              nextone->prev = previous;
       
              temp->next = NULL;
              temp->prev = NULL; 
              delete(temp);
            }

      } 
 
    pointer = pointer->next;
   }

}

//--------------------------Save Cases----------------------------

//Save cases will write all cases in linked list back to the file they
//came from for future use.  In addition, if the case choosen as the
//final option has been modified from its original form, it too
//will be added to the case base to allow for even more options in the future

void CASECLASS::save_cases()
{
   ofstream out;
   Node *pointer;
   Node *best_pointer;

//match_points and match used to determine if the best case (one selected as
//best COA) has been modified (i.e. not identical to any original cases)
//If it has, it will be added

    int match_points = 0;
    int match = 0;

//The following variables allow for the comparison of char arrays
//The string compare function could be implemented for this

    int j = 0;
    int lcv1 = 0;
    int lcv2 = 0;
    int length = 0;
    int match_count = 0;
 
   pointer = head->next;
   best_pointer = head_best->next;

   out.open("coas");

   while (pointer != tail)
   {
      match_points = 0;
      
      out << pointer->index_data.max_threatsV;
      out << endl; 
      out << pointer->index_data.terrainV;
      out << endl;
      out << pointer->index_data.slopeV;
      out << endl;
      out << pointer->index_data.timingV;
      out << endl;
      out << pointer->index_data.strengthV;
      out << endl;
      out << pointer->index_data.timeV;
      out << endl;
      out << pointer->index_data.commoditiesV;
      out << endl;
      out << pointer->index_data.equipageV;
      out << endl;
      out << pointer->index_data.fuelV;
      out << endl;
      out << pointer->index_data.subsidiariesV;
      out << endl;
      out << pointer->index_data.expec_down_commV;
      out << endl;
      out << pointer->index_data.unexpec_down_commV;
      out << endl;
      out << pointer->index_data.natural_elementsV;
      out << endl;
      out << pointer->index_data.mechanicalV;
      out << endl;
      out << pointer->index_data.distanceV;
      out << endl;

//Nodes
      for (int i = 0; i < MAX1; i++)
      {
         if ((pointer->mission_data.nodes[i] >= 'A') && 
            (pointer->mission_data.nodes[i] <= 'Z')) 
         { 
             out << pointer->mission_data.nodes[i];
         }
      } 

//checking match to chosen best case, if it matches, the best case
//isn't a modification and shouldn't be written to the file twice
      j = 0;
      match_count = 0;
      length = 0;
 
      while ( j < MAX2) 
      {
         if ((pointer->mission_data.nodes[j] ==
             best_pointer->mission_data.nodes[j]) &&
            (pointer->mission_data.nodes[j] >= 'A') &&
             (pointer->mission_data.nodes[j] <= 'Z') &&
            (best_pointer->mission_data.nodes[j] >= 'A') &&
             (best_pointer->mission_data.nodes[j] <= 'Z'))

            match_count++;

         if ((pointer->mission_data.nodes[j] >= 'A') &&
             (pointer->mission_data.nodes[j] <= 'Z'))
            length++;
    

         j++;
      }//end while

      if (match_count == length)
          match_points++;
      out << endl;

//Edges
      for (int i = 0; i < MAX1; i++)
      {
         if ((pointer->mission_data.edges[i] >= 'A') && 
            (pointer->mission_data.edges[i] <= 'Z')) 
         { 
             out << pointer->mission_data.edges[i];
         }
      } 

//checking match to chosen best case, if it matches, the best case
//isn't a modification and shouldn't be written to the file twice
      j = 0;
      match_count = 0;
      length = 0;

      while ( j < MAX2) 
      {
         if ((pointer->mission_data.edges[j] ==
             best_pointer->mission_data.edges[j]) &&
            (pointer->mission_data.edges[j] >= 'A') &&
             (pointer->mission_data.edges[j] <= 'Z') &&
            (best_pointer->mission_data.edges[j] >= 'A') &&
             (best_pointer->mission_data.edges[j] <= 'Z'))

            match_count++;

         if ((pointer->mission_data.edges[j] >= 'A') &&
             (pointer->mission_data.edges[j] <= 'Z'))
            length++;

         j++;
      }//end while

      if (match_count == length)
          match_points++;
      out << endl;
      
//Departure Time

      out << pointer->mission_data.departure_time;
      if (pointer->mission_data.departure_time == 
          best_pointer->mission_data.departure_time)
          match_points++;
      out << endl;

//ETA     
      out << pointer->mission_data.ETA;
      if (pointer->mission_data.ETA == best_pointer->mission_data.ETA)
          match_points++;
      out << endl;
    
//Travel Time 
      out << pointer->mission_data.travel_time;
      if (pointer->mission_data.travel_time == 
          best_pointer->mission_data.travel_time)
          match_points++;
      out << endl;

//Terrain Type  
      switch (pointer->mission_data.terrain_type)
      {
         case 0:  out << 0;break;
         case 1:  out << 1;break;
         case 2:  out << 2;break;
         case 3:  out << 3;break;
         case 4:  out << 4;break;
         case 5:  out << 5;break;
         case 6:  out << 6;break;
      }; 
      if (pointer->mission_data.terrain_type == 
          best_pointer->mission_data.terrain_type)
          match_points++;
      out << endl;

//Slope
      out << pointer->mission_data.slope;
      if (pointer->mission_data.slope == best_pointer->mission_data.slope)
          match_points++;
      out << endl;

//Threat Locations write to file
      for (int i = 0; i < MAX2; i++)
      {
         if ((pointer->mission_data.threats.threat_locations[i] >= 'A') && 
            (pointer->mission_data.threats.threat_locations[i] <= 'Z')) 
         { 
             out << pointer->mission_data.threats.threat_locations[i];
         }
      } 

//checking match to chosen best case, if it matches, the best case
//isn't a modification and shouldn't be written to the file twice
      j = 0;
      match_count = 0;
      length = 0;
 
      while ( j < MAX2) 
      {
         if ((pointer->mission_data.threats.threat_locations[j] ==
             best_pointer->mission_data.threats.threat_locations[j]) &&
            (pointer->mission_data.threats.threat_locations[j] >= 'A') &&
             (pointer->mission_data.threats.threat_locations[j] <= 'Z') &&
            (best_pointer->mission_data.threats.threat_locations[j] >= 'A') &&
             (best_pointer->mission_data.threats.threat_locations[j] <= 'Z'))

            match_count++;

         if ((pointer->mission_data.threats.threat_locations[j] >= 'A') &&
             (pointer->mission_data.threats.threat_locations[j] <= 'Z'))
            length++;
    

         j++;
      }//end while

      if (match_count == length)
          match_points++;
      out << endl;

//Num of Threats
      out << pointer->mission_data.num_of_threats;
      if (pointer->mission_data.num_of_threats == 
          best_pointer->mission_data.num_of_threats)
          match_points++;
      out << endl;

//Manpower
      out << pointer->mission_data.strength_info.manpower;
      if (pointer->mission_data.strength_info.manpower == 
          best_pointer->mission_data.strength_info.manpower)
          match_points++;
      out << endl;

//Firepower
      out << pointer->mission_data.strength_info.firepower;
      if (pointer->mission_data.strength_info.firepower == 
          best_pointer->mission_data.strength_info.firepower)
          match_points++;
      out << endl;

//Distance
      out << pointer->mission_data.distance;
      if (pointer->mission_data.distance == best_pointer->mission_data.distance)
          match_points++;
      out << endl;

//Commodity Locations
      for (int i = 0; i < MAX2; i++)
      {
         if ((pointer->mission_data.commodity_locations[i] >= 'A') && 
            (pointer->mission_data.commodity_locations[i] <= 'Z')) 
         { 
             out << pointer->mission_data.commodity_locations[i];
         }
      } 
      if (pointer->mission_data.commodity_locations == 
          best_pointer->mission_data.commodity_locations)
          match_points++;

//checking match to chosen best case, if it matches, the best case
//isn't a modification and shouldn't be written to the file twice
      j = 0;
      match_count = 0;
      length = 0;
 
      while ( j < MAX2) 
      {
         if ((pointer->mission_data.commodity_locations[j] ==
             best_pointer->mission_data.commodity_locations[j]) &&
            (pointer->mission_data.commodity_locations[j] >= 'A') &&
             (pointer->mission_data.commodity_locations[j] <= 'Z') &&
            (best_pointer->mission_data.commodity_locations[j] >= 'A') &&
             (best_pointer->mission_data.commodity_locations[j] <= 'Z'))

            match_count++;

         if ((pointer->mission_data.commodity_locations[j] >= 'A') &&
             (pointer->mission_data.commodity_locations[j] <= 'Z'))
            length++;
    

         j++;
      }//end while

      if (match_count == length)
          match_points++;
      out << endl;

//Fuel Locations
      for (int i = 0; i < MAX2; i++)
      {
         if ((pointer->mission_data.fuel_locations[i] >= 'A') && 
            (pointer->mission_data.fuel_locations[i] <= 'Z')) 
         { 
             out << pointer->mission_data.fuel_locations[i];
         }
      } 
      if (pointer->mission_data.fuel_locations == 
          best_pointer->mission_data.fuel_locations)
          match_points++;

//checking match to chosen best case, if it matches, the best case
//isn't a modification and shouldn't be written to the file twice
      j = 0;
      match_count = 0;
      length = 0;
 
      while ( j < MAX2) 
      {
         if ((pointer->mission_data.fuel_locations[j] ==
             best_pointer->mission_data.fuel_locations[j]) &&
            (pointer->mission_data.fuel_locations[j] >= 'A') &&
             (pointer->mission_data.fuel_locations[j] <= 'Z') &&
            (best_pointer->mission_data.fuel_locations[j] >= 'A') &&
             (best_pointer->mission_data.fuel_locations[j] <= 'Z'))

            match_count++;

         if ((pointer->mission_data.fuel_locations[j] >= 'A') &&
             (pointer->mission_data.fuel_locations[j] <= 'Z'))
            length++;
    

         j++;
      }//end while

      if (match_count == length)
          match_points++;
      out << endl;

//Equipage Locations
      for (int i = 0; i < MAX2; i++)
      {
         if ((pointer->mission_data.equipage_locations[i] >= 'A') && 
            (pointer->mission_data.equipage_locations[i] <= 'Z')) 
         { 
             out << pointer->mission_data.equipage_locations[i];
         }
      } 
      if (pointer->mission_data.equipage_locations == 
          best_pointer->mission_data.equipage_locations)
          match_points++;
      
//checking match to chosen best case, if it matches, the best case
//isn't a modification and shouldn't be written to the file twice
      j = 0;
      match_count = 0;
      length = 0;
 
      while ( j < MAX2) 
      {
         if ((pointer->mission_data.equipage_locations[j] ==
             best_pointer->mission_data.equipage_locations[j]) &&
            (pointer->mission_data.equipage_locations[j] >= 'A') &&
             (pointer->mission_data.equipage_locations[j] <= 'Z') &&
            (best_pointer->mission_data.equipage_locations[j] >= 'A') &&
             (best_pointer->mission_data.equipage_locations[j] <= 'Z'))

            match_count++;

         if ((pointer->mission_data.equipage_locations[j] >= 'A') &&
             (pointer->mission_data.equipage_locations[j] <= 'Z'))
            length++;

         j++;
      }//end while

      if (match_count == length)
          match_points++;
      out << endl;
      
//Subsidiary Locations
      for (int i = 0; i < MAX2; i++)
      {
         if ((pointer->mission_data.subsidiary_locations[i] >= 'A') && 
            (pointer->mission_data.subsidiary_locations[i] <= 'Z')) 
         { 
             out << pointer->mission_data.subsidiary_locations[i];
         }
      } 
      if (pointer->mission_data.subsidiary_locations ==
          best_pointer->mission_data.subsidiary_locations)
          match_points++;

//checking match to chosen best case, if it matches, the best case
//isn't a modification and shouldn't be written to the file twice
      j = 0;
      match_count = 0;
      length = 0;
 
      while ( j < MAX2) 
      {
         if ((pointer->mission_data.subsidiary_locations[j] ==
             best_pointer->mission_data.subsidiary_locations[j]) &&
            (pointer->mission_data.subsidiary_locations[j] >= 'A') &&
             (pointer->mission_data.subsidiary_locations[j] <= 'Z') &&
            (best_pointer->mission_data.subsidiary_locations[j] >= 'A') &&
             (best_pointer->mission_data.subsidiary_locations[j] <= 'Z'))

            match_count++;

         if ((pointer->mission_data.subsidiary_locations[j] >= 'A') &&
             (pointer->mission_data.subsidiary_locations[j] <= 'Z'))
            length++;
    
         j++;
      }//end while

      if (match_count == length)
          match_points++;
      out << endl;

//Chance of Expected
      out << pointer->mission_data.chance_expec_down_comm;
      if (pointer->mission_data.chance_expec_down_comm == 
          best_pointer->mission_data.chance_expec_down_comm)
          match_points++;
      out << endl;

//Chance of Unexpected
      out << pointer->mission_data.chance_unexpec_down_comm;
      if (pointer->mission_data.chance_unexpec_down_comm == 
          best_pointer->mission_data.chance_unexpec_down_comm)
          match_points++;
      out << endl;

//Chance of Natural Elements
      out << pointer->mission_data.chance_natural_elements;
      if (pointer->mission_data.chance_natural_elements == 
          best_pointer->mission_data.chance_natural_elements)
          match_points++; 
      out << endl;

//Chance of Mechanical
      
      out << pointer->mission_data.chance_mechanical;
      if (pointer->mission_data.chance_mechanical == 
          best_pointer->mission_data.chance_mechanical)
          match_points++;
      out << endl;
      out << endl;

      pointer = pointer ->next;
 
      if (match_points == 20)
          match = 1;

   }


   pointer = head_best->next;

//If best case (selected course of action) is new, not an original case
//it too is added

   if (match != 1)
   {
   while (pointer != tail_best)
   {

      fix_index(2);
   
      pointer->index_data.timingV = 2;
 
      out << pointer->index_data.max_threatsV;
      out << endl; 
      out << pointer->index_data.terrainV;
      out << endl;
      out << pointer->index_data.slopeV;
      out << endl;
      out << pointer->index_data.timingV;
      out << endl;
      out << pointer->index_data.strengthV;
      out << endl;
      out << pointer->index_data.timeV;
      out << endl;
      out << pointer->index_data.commoditiesV;
      out << endl;
      out << pointer->index_data.equipageV;
      out << endl;
      out << pointer->index_data.fuelV;
      out << endl;
      out << pointer->index_data.subsidiariesV;
      out << endl;
      out << pointer->index_data.expec_down_commV;
      out << endl;
      out << pointer->index_data.unexpec_down_commV;
      out << endl;
      out << pointer->index_data.natural_elementsV;
      out << endl;
      out << pointer->index_data.mechanicalV;
      out << endl;
      out << pointer->index_data.distanceV;
      out << endl;


      for (int i = 0; i < MAX1; i++)
      {
         if ((pointer->mission_data.nodes[i] >= 'A') && 
            (pointer->mission_data.nodes[i] <= 'Z')) 
         { 
             out << pointer->mission_data.nodes[i];
         }
      } 
      out << endl;

      for (int i = 0; i < MAX1; i++)
      {
         if ((pointer->mission_data.edges[i] >= 'A') && 
            (pointer->mission_data.edges[i] <= 'Z')) 
         { 
             out << pointer->mission_data.edges[i];
         }
      } 
      out << endl;
      
      out << pointer->mission_data.departure_time;
      out << endl;
      out << pointer->mission_data.ETA;
      out << endl;
      out << pointer->mission_data.travel_time;
      out << endl;

   
      switch (pointer->mission_data.terrain_type)
      {
         case 0:  out << 0;break;
         case 1:  out << 1;break;
         case 2:  out << 2;break;
         case 3:  out << 3;break;
         case 4:  out << 4;break;
         case 5:  out << 5;break;
         case 6:  out << 6;break;
      }; 
      out << endl;

      out << pointer->mission_data.slope;
      out << endl;

      for (int i = 0; i < MAX2; i++)
      {
         if ((pointer->mission_data.threats.threat_locations[i] >= 'A') && 
            (pointer->mission_data.threats.threat_locations[i] <= 'Z')) 
         { 
             out << pointer->mission_data.threats.threat_locations[i];
         }
      } 
      out << endl;

      out << pointer->mission_data.num_of_threats;
      out << endl;
      out << pointer->mission_data.strength_info.manpower;
      out << endl;
      out << pointer->mission_data.strength_info.firepower;
      out << endl;
      out << pointer->mission_data.distance;
      out << endl;

      for (int i = 0; i < MAX2; i++)
      {
         if ((pointer->mission_data.commodity_locations[i] >= 'A') && 
            (pointer->mission_data.commodity_locations[i] <= 'Z')) 
         { 
             out << pointer->mission_data.commodity_locations[i];
         }
      } 
      out << endl;

      for (int i = 0; i < MAX2; i++)
      {
         if ((pointer->mission_data.fuel_locations[i] >= 'A') && 
            (pointer->mission_data.fuel_locations[i] <= 'Z')) 
         { 
             out << pointer->mission_data.fuel_locations[i];
         }
      } 
      out << endl;

      for (int i = 0; i < MAX2; i++)
      {
         if ((pointer->mission_data.equipage_locations[i] >= 'A') && 
            (pointer->mission_data.equipage_locations[i] <= 'Z')) 
         { 
             out << pointer->mission_data.equipage_locations[i];
         }
      } 
      out << endl;
      
      for (int i = 0; i < MAX2; i++)
      {
         if ((pointer->mission_data.subsidiary_locations[i] >= 'A') && 
            (pointer->mission_data.subsidiary_locations[i] <= 'Z')) 
         { 
             out << pointer->mission_data.subsidiary_locations[i];
         }
      } 
      out << endl;

      out << pointer->mission_data.chance_expec_down_comm;
      out << endl;
      out << pointer->mission_data.chance_unexpec_down_comm;
      out << endl;
      out << pointer->mission_data.chance_natural_elements;
      out << endl;
      out << pointer->mission_data.chance_mechanical;
      out << endl;
      pointer = pointer->next;
   } //end while
   } //end if

   out.close();

   head_best->next = tail_best;
   tail_best->prev = head_best;
   head_best->prev = NULL;
   tail_best->next = NULL;
}

//-----------------------------Plotme------------------------------

//Called by final form to get total points a case is worth returned
//to graph

point_return CASECLASS::plotme()
{
   Node *pointer = head->next;
   int i = 0;

   while (pointer != tail)
   {
      i++;
      pointer = pointer->next;
   }

   return original_points;

}

//---------------------------Plotmetoo-----------------------------

//called to return total match points the three best cases found
//were worth to graph their comparision

point_return CASECLASS::plotmetoo()
{
   Node *pointer = head->next;
   int i = 0;

   while (pointer != tail)
   {
      i++;
      pointer = pointer->next;
   }

   return modified_points;
}

//------------------------------Deconstructor-----------------------

CASECLASS::~CASECLASS()
{
}

