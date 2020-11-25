INTERFACE zif_do_clob
  PUBLIC .

  TYPES :
    BEGIN OF  ts_objects,
      id      TYPE zdo_object_id,
      ptr_obj TYPE REF TO object,
    END OF  ts_objects,
    tt_objects TYPE STANDARD TABLE OF ts_objects WITH DEFAULT KEY,
    BEGIN OF  ts_obj_relationship,
      id           TYPE zdo_object_id,
      relationship TYPE zdo_object_relationship,
      ptr_obj      TYPE REF TO object,
    END OF  ts_obj_relationship,
    tt_obj_relationship
      TYPE STANDARD TABLE OF ts_obj_relationship WITH DEFAULT KEY.
ENDINTERFACE.
