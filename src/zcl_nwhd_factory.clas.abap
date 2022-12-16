class ZCL_NWHD_FACTORY definition
  public
  create public .

public section.

  interfaces ZIF_NWHD_C .

  class-methods CREATE_COLLECTOR
    importing
      !IV_TYPE type DATA
    returning
      value(RR_COLLECTOR) type ref to ZIF_NWHD_COL .
  class-methods CREATE_GUID
    returning
      value(RV_GUID) type STRING .
  class-methods CREATE_JOB
    importing
      !IV_TYPE type DATA
    returning
      value(RR_JOB) type ref to ZIF_NWHD_JOB .
  class-methods CREATE_LOGGER
    returning
      value(RR_INSTANCE) type ref to ZIF_NWHD_LOGGER .
  class-methods CREATE_PUBLISHER
    importing
      !IV_TYPE type DATA
    returning
      value(RR_PUBLISHER) type ref to ZIF_NWHD_PUB .
  class-methods CREATE_UTIL
    returning
      value(RR_INSTANCE) type ref to ZIF_NWHD_UTIL .
  class-methods CREATE_INSTANCE
    importing
      !IV_TYPE type DATA
    returning
      value(RR_INSTANCE) type ref to OBJECT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_NWHD_FACTORY IMPLEMENTATION.


  METHOD create_collector.
    rr_collector ?= create_instance( iv_type ).
  ENDMETHOD.


  METHOD create_guid.
    rv_guid = cl_uuid_factory=>create_system_uuid( )->create_uuid_c32( ).
  ENDMETHOD.


  METHOD create_instance.

    DATA(lv_type) = CONV seoclsname( iv_type ).
    IF lv_type CP 'ZIF*'.
      REPLACE 'ZIF' IN lv_type WITH 'ZCL'.
    ENDIF.

    CREATE OBJECT rr_instance TYPE (lv_type).

  ENDMETHOD.


  METHOD CREATE_JOB.
    rr_job ?= create_instance( iv_type ).
  ENDMETHOD.


  METHOD CREATE_LOGGER.
    rr_instance ?= create_instance( 'ZIF_NWHD_LOGGER' ).
  ENDMETHOD.


  METHOD create_publisher.
    rr_publisher ?= create_instance( iv_type ).
  ENDMETHOD.


  METHOD create_util.
    rr_instance ?= create_instance( 'ZIF_NWHD_UTIL' ).
  ENDMETHOD.
ENDCLASS.
