INTERFACE zif_do_se
  PUBLIC .

  TYPES:
    BEGIN OF ts_look_up,
      ucomm                  TYPE syucomm,
      obj_data               TYPE REF TO zcl_do_it,  "was zcl_data_object
      conv_method(40)        TYPE c,
      conv_filter_method(40) TYPE c,
      program                TYPE progname,
      dynnr                  TYPE sydynnr,
      title(30)              TYPE c,
    END OF ts_look_up,
    tt_look_up        TYPE STANDARD TABLE OF ts_look_up WITH DEFAULT KEY,
    tt_sorted_look_up TYPE SORTED TABLE OF ts_look_up
      WITH UNIQUE KEY ucomm.
ENDINTERFACE.
