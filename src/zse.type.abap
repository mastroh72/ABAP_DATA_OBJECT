TYPE-POOL ZSE .
TYPES:
  BEGIN OF zse_st_do_look_up,
    ucomm TYPE syucomm,
    obj_data TYPE REF TO zcl_do_it,  "was zcl_data_object
    conv_method(40) TYPE c,
    conv_filter_method(40) type c,
    program type PROGNAME,
    dynnr TYPE sydynnr,
    title(30) TYPE c,
  END OF zse_st_do_look_up,
  zse_tt_do_look_up TYPE STANDARD TABLE OF zse_st_do_look_up WITH DEFAULT KEY,
  zse_tt_do_sorted_look_up TYPE SORTED TABLE OF zse_st_do_look_up
    WITH UNIQUE KEY ucomm.
