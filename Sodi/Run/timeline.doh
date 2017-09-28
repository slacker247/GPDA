   21   1   00:00:00				# No. of entries, AutoAccept, Clock adjustment
# 1) Time of event
# 2) Units of time (S=sec, M=min, H=hour, D=day, W=week, N=month, Y=year
# 3) Event priority
# 4) Type of event
# 5) Event Action
# 6) Action value
# 7) Event description
# 8) Associated filename (if any)
#        1         2         3         4         5         6         7         8         9         
#234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
-1440.0   M 2  ALARM     JCSORDER        WARN     JCS_Warning_Order.                      filename
-1440.0   M 1  ALARM     INTEL              0     Intelligence_Reports.                   filename
-1440.0   M 1  UPDATE    READI              0     Force_Status_Reports.
-60.0     M 3  COMM      BRIEFING           0     Situation_Briefing.
0.0       M 3  COMM      CONFERENCE         0     Threat_Conference.
4.0       M 2  ALARM     INTEL              0     DPRK_Ultimatum.                         filename
9.0       M 4  ALARM     POTEVENT           0     DPRK_Missile_Launch.
13.0      M 4  ALARM     POTEVENT           0     DSP_Launch_Detection.
16.0      M 3  COMM      CONFERENCE         0     Missile_Warning_Conference.
18.0      M 3  COMM      CONFERENCE         0     NCA_Conference.
19.0      M 3  UPDATE    DEFCON             1     DEFCON_Change.
20.0      M 3  UPDATE    ROE            CONUS     NMD_ROE_Change.
21.0      M 3  COMM      JCSORDER         GEN     Force_Generation_Orders.                filename
22.0      M 4  UPDATE    DEA             Free     NMD_Weapons_Free.
22.0      M 2  SIM       GBI                0     GBI_Launches.
43.0      M 3  COMM      CONFERENCE         0     SIOP_Option_Selected.
47.0      M 2  SIM       Intercepts         0     GBI_Intercepts_Begin.
51.0      M 4  ALARM     POTEVENT           0     PRC_Launch_Detection.
55.0      M 3  COMM      JCSORDER        EXEC     JCS_Execute_Order.                      filename
58.8      M 2  SIM       Intercepts         0     PRC_Missile_Intercepts_Begin.
61.0      M 3  COMM      JCSORDER        TERM     JCS_Terminate_Order.                    filename