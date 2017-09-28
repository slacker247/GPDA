//  messager resources for the graphics
//
//	RSDAIR     -- receives messages from the airsim
//  RSDPLAT    -- receives messages about space based platforms
//  RSDMISSILE -- receives messages about ballistic missiles
//  RSDSITE    -- receives messages about site objects ( non-moving )
//
//
//
#define ATMOSPHERE_ALT         1.07  // normalized to one

#define CMND_EXIT              -1    // not used
#define CMND_DEFINE             1    // create a graphical object
#define CMND_ADVANCE            2    // create/move a graphical object
#define CMND_LAUNCH             3    // not used
#define CMND_BURNOUT            4    // not used
#define CMND_IMPACT             5    // not used 
#define CMND_UNDEFINE           6    // delete graphical objects from the system
#define CMND_VIEWPOINT          7    // change the camera viewpoint
#define CMND_REFRPOINT          8    // change what the camera is looking at
#define CMND_FOV                9    // change the cameras field of view
#define CMND_TRAJ_ADV           10   // not used
#define CMND_CV_IMPACT          11   // not used
#define CMND_UPDATE_RATE        12   // not used
#define CMND_DESTROY            13   // not used
#define CMND_CV_KILLED			14   // used to update the score board
#define CMND_MISSILE_KILLED		15   // used to update the score board
#define CMND_MISSILE_LAUNCH		16   // used to update the score board
#define CMND_LINK				17   // link two objects
#define CMND_UNLINK				18   // unlink two linked objects
#define CMND_LINK_COLOR         19   // change a links color
#define CMND_LINK_POINTS        20   // link two points
#define CMND_RESET_TRAJECTORIES 21   // clear trajectory arrays during a pause 
#define CMND_OTHB_SECTORS       22   // show special interrogation mode sectors


// these are the object types that correspond to the icons that are
// draw on the screen

#define TYPE_NULL               0 
#define TYPE_SITE               1
#define TYPE_STAGE1             2
#define TYPE_BSTS               3
#define TYPE_CV                 4
#define TYPE_SILO               5
#define TYPE_KKV                6

#define TYPE_SITE_SIM           7
#define TYPE_STAGE1_SIM         8
#define TYPE_BSTS_SIM           9
#define TYPE_CV_SIM            10
#define TYPE_SILO_SIM          11
#define TYPE_KKV_SIM           12

#define TYPE_ERRBASKET         13
#define TYPE_COVARIANCE        14
#define TYPE_VIEWER            15
#define TYPE_LOOKAT			   16

#define TYPE_STAGE2            17
#define TYPE_BUS               18
#define TYPE_WRHD              19
#define TYPE_SSTS              20

#define TYPE_STAGE2_SIM        21
#define TYPE_BUS_SIM           22
#define TYPE_WRHD_SIM          23
#define TYPE_SSTS_SIM          24

#define TYPE_ASAT              25
#define TYPE_ASAT_SIM          26

#define TYPE_ROL               27
#define TYPE_ROL_SIM           28
#define TYPE_BUS_BURN          29

#define TYPE_SHELL				30
#define TYPE_IMPACT             31
#define TYPE_CV_IMPACT          32
#define TYPE_EARTH				33
#define TYPE_POLAR				34
#define TYPE_PICKED_CV			35
#define TYPE_PICKED_SITE		36
#define TYPE_PIP				37
#define TYPE_SS18_WRHD			38
#define TYPE_ASAT_WRHD			39
#define TYPE_TARGET_PLAT		40
#define TYPE_PLAT_IN_BATTLE		41
#define TYPE_PLAT_TARGET_IN_BATTLE		42
#define TYPE_STAGE1_ENGAGED		43
#define TYPE_STAGE2_ENGAGED		44
#define TYPE_BUS_ENGAGED		45

#define TYPE_TRAJECTORY        	46
#define TYPE_DEAD_SITE			47
#define	TYPE_RV					48
#define	TYPE_SMALL_EARTH		49

#define	TYPE_DONUT				50
#define	TYPE_AIRCRAFT			51
#define	TYPE_BLUE_FIGHTER		51
#define	TYPE_BLUE_BOMBER		52
#define	TYPE_BLUE_TANKER		53
#define	TYPE_RED_FIGHTER		54
#define	TYPE_RED_BOMBER			55
#define	TYPE_RED_TANKER			56
#define TYPE_BLUE_ALCM			57
#define TYPE_RED_ALCM			58



#define	TYPE_BLUE_FIGHTER_HIGHLIGHT		59

#define	TYPE_BLUE_BOMBER_HIGHLIGHT		60
#define	TYPE_BLUE_TANKER_HIGHLIGHT		61
#define	TYPE_RED_FIGHTER_HIGHLIGHT		62
#define	TYPE_RED_BOMBER_HIGHLIGHT		63
#define	TYPE_RED_TANKER_HIGHLIGHT		64

#define TYPE_F14						65
#define TYPE_F15						66
#define TYPE_F16						67
#define TYPE_AWAC						68
#define TYPE_747						69

#define	TYPE_BLUE_ALCM_HIGHLIGHT		80
#define	TYPE_RED_ALCM_HIGHLIGHT		    81

#define TYPE_RADAR						65
#define TYPE_LINK						66


#define TYPE_NUDET0						90
#define TYPE_NUDET1						91
#define TYPE_NUDET2						92
#define TYPE_NUDET3						93



#define TYPE_BLUE_NAVALBASE				72
#define TYPE_RED_NAVALBASE				73
#define TYPE_BLUE_AIRBASE				74
#define TYPE_RED_AIRBASE				75
#define TYPE_BLUE_HQ					76
#define TYPE_RED_HQ						77
#define TYPE_BLUE_SUB					78
#define TYPE_RED_SUB					79

#define TYPE_F15_HIGHLIGHT				101
#define TYPE_AWAC_HIGHLIGHT			    102
#define TYPE_747_HIGHLIGHT				104

#define TYPE_OTHB						105
#define TYPE_BLUE_AIR_MISSION			106
#define TYPE_BLUE_AIR_MISSION_HIGHLIGHT 107
#define TYPE_RED_AIR_MISSION			108
#define TYPE_RED_AIR_MISSION_HIGHLIGHT  109
#define TYPE_STARS						110
#define TYPE_POLITICAL_BOUNDRIES		125

#define TYPE_SIGHTING                   126
#define TYPE_GROUND_RADAR				127

#define LINK_OFFSET 					5000

#define SHOW_OTHB						105
#define SHOW_G_RADAR					127
#define SHOW_OTHB_AND_G_RADAR			232
