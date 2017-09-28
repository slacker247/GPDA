// sensobj.C method file

#include <stdio.h>
#include <math.h>
#include "def.h"
#include "sensobj.H"
#include "gridid.H"
#include "track.H"

/************************************************************************
* C_SENSOBJ : construct a sensobj object				*
************************************************************************/
C_SENSOBJ::C_SENSOBJ() {

  on_off = 0;
  scanning = 0;
  scan_type = -1;
  com_rate = INFINITY;
  last_tcom = -INFINITY;
  trackup_time = -INFINITY;
  trackup_node = NODE;

  ring = -1;
  id_in_ring = -1;
  n_per_ring = -1;

  sensor_model = NULL;

  coverage = NULL;
  movers = NULL;
  tracks = NULL;
  links = NULL;

}

/************************************************************************
* set_sensor_model : set sensor model					*
************************************************************************/
void C_SENSOBJ::set_sensor_model(C_SENSOR_MODEL *sm) {

  sensor_model = sm;

  if (sm != NULL) {
    coverage = new C_XQUEUE();
    movers = new C_XQUEUE();
    tracks = new C_XQUEUE();
    links = new C_XQUEUE();
    icon = sensor_model->get_icon();
  }

}

/************************************************************************
* get_scan_time : get the scan time for the sensor			*
************************************************************************/
double C_SENSOBJ::get_scan_time() {

  if (sensor_model) {
    return sensor_model->get_scan_time();
  }else{
    return -1.0;
  }

}

/************************************************************************
* get_rmax : get rmax for the sensor					*
************************************************************************/
double C_SENSOBJ::get_rmax() {

  if (sensor_model) {
    return sensor_model->get_rmax();
  }else{
    return -1.0;
  }

}

/************************************************************************
* get_rmaxcoverage : get rmaxcoverage for the sensor			*
************************************************************************/
double C_SENSOBJ::get_rmaxcoverage() {

  if (sensor_model) {
    return sensor_model->get_rmaxcoverage();
  }else{
    return -1.0;
  }

}


