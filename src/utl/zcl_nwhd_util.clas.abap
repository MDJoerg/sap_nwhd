class ZCL_NWHD_UTIL definition
  public
  create public .

public section.

  interfaces ZIF_NWHD_UTIL .
protected section.
private section.
ENDCLASS.



CLASS ZCL_NWHD_UTIL IMPLEMENTATION.


  METHOD zif_nwhd_util~get_all_mod_col.
    SELECT clsname
      FROM zv_nwhd_mod_col
      INTO TABLE rt_col.
  ENDMETHOD.


  method ZIF_NWHD_UTIL~GET_ALL_MOD_JOB.
    SELECT clsname
      FROM zv_nwhd_mod_job
      INTO TABLE rt_job.
  endmethod.


  METHOD zif_nwhd_util~get_all_mod_pub.
    SELECT clsname
      FROM zv_nwhd_mod_pub
      INTO TABLE rt_pub.
  ENDMETHOD.
ENDCLASS.
