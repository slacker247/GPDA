   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*                                                     */
   /*             CLIPS Version 6.00  05/12/93            */
   /*                                                     */
   /*             FUZZY COMMANDS HEADER FILE              */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Donnell                                     */
/*      Bob Orchard (NRCC - Nat'l Research Council of Canada)*/
/*                  (Fuzzy reasoning extensions)             */
/*                  (certainty factors for facts and rules)  */
/*                  (extensions to run command)              */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/



#ifndef _H_fuzzycom
#define _H_fuzzycom




#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _FUZZYCOM_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

#if ANSI_COMPILER
    LOCALE VOID           DeffuzzyCommands();
    LOCALE VOID          *getu();
    LOCALE double         getu_from();
    LOCALE double         getu_to();
    LOCALE VOID          *getu_units();
    LOCALE VOID          *get_fs();
    LOCALE VOID          *get_fs_template();
    LOCALE VOID          *get_fs_lv();
    LOCALE int            get_fs_length(); 
    LOCALE double         get_fs_value();
    LOCALE double         get_fs_x();
    LOCALE double         get_fs_y();
    LOCALE double         moment_defuzzify();
    LOCALE double         maximum_defuzzify();
    LOCALE VOID           add_fuzzy_modifier();
    LOCALE VOID           remove_fuzzy_modifier();
    LOCALE VOID           set_fuzzy_inference_type();
    LOCALE VOID          *get_fuzzy_inference_type();
    LOCALE VOID           set_fuzzy_display_precision();
    LOCALE long int       get_fuzzy_display_precision();
    LOCALE VOID           set_alpha_value();
    LOCALE double         get_alpha_value();
    LOCALE VOID           plot_fuzzy_value();
    LOCALE struct fuzzy_value *get_fuzzy_slot();
    LOCALE struct fuzzy_value *fuzzy_union();
    LOCALE struct fuzzy_value *fuzzy_intersection();
    LOCALE struct fuzzy_value *fuzzy_modify();
    LOCALE struct fuzzy_value *create_fuzzy_value();

#else
    LOCALE VOID           DeffuzzyCommands();
    LOCALE VOID          *getu();
    LOCALE double         getu_from();
    LOCALE double         getu_to();
    LOCALE VOID          *getu_units();
    LOCALE VOID          *get_fs_lv();
    LOCALE VOID          *get_fs();
    LOCALE VOID          *get_fs_template();
    LOCALE int            get_fs_length(); 
    LOCALE double         get_fs_value();
    LOCALE double         get_fs_x();
    LOCALE double         get_fs_y();
    LOCALE double         moment_defuzzify();
    LOCALE double         maximum_defuzzify();
    LOCALE VOID           add_fuzzy_modifier();
    LOCALE VOID           remove_fuzzy_modifier();
    LOCALE VOID           set_fuzzy_inference_type();
    LOCALE VOID          *get_fuzzy_inference_type();
    LOCALE VOID           set_fuzzy_display_precision();
    LOCALE long int       get_fuzzy_display_precision();
    LOCALE VOID           set_alpha_value();
    LOCALE double         get_alpha_value();
    LOCALE VOID           plot_fuzzy_value();
    LOCALE struct fuzzy_value *get_fuzzy_slot();
    LOCALE struct fuzzy_value *fuzzy_union();
    LOCALE struct fuzzy_value *fuzzy_intersection();
    LOCALE struct fuzzy_value *fuzzy_modify();
    LOCALE struct fuzzy_value *create_fuzzy_value();

#endif 



#endif

