TYPE-POOL zslot .

TYPES:
  zslot_do_hex_seperator TYPE x,
  BEGIN OF zslot_st_do_filter,
    fieldname TYPE fieldname,
    low       TYPE char72,
    high      TYPE char72,
    filter_on TYPE char1,
    pb(80),
    include   TYPE zst_do_rowobject,
  END OF zslot_st_do_filter,

  BEGIN OF zslot_st_do_field_values,
    fieldname  TYPE fieldname,
    fieldvalue TYPE REF TO data,
  END OF zslot_st_do_field_values,

  BEGIN OF zslot_st_do_selection_options,
    fieldname TYPE fieldname,
    sign(1)   TYPE c,
    option(2) TYPE c,
    ptrlow    TYPE REF TO data,
    ptrhigh   TYPE REF TO data,
  END OF zslot_st_do_selection_options,

  BEGIN OF zslot_st_do_so_rowobj,
    fieldname TYPE fieldname,
    sign(1)   TYPE c,
    option(2) TYPE c,
    ptrlow    TYPE REF TO data,
    ptrhigh   TYPE REF TO data,
    include   TYPE zst_do_rowobject,
  END OF zslot_st_do_so_rowobj,

  zslot_tt_do_field_values
    TYPE STANDARD TABLE OF zslot_st_do_field_values WITH DEFAULT KEY,
  zslot_tt_do_so_rowobj
    TYPE STANDARD TABLE OF zslot_st_do_so_rowobj WITH DEFAULT KEY,
  zslot_tt_do_selection_options
    TYPE STANDARD TABLE OF zslot_st_do_selection_options WITH DEFAULT KEY,
  zslot_tt_do_filter
    TYPE STANDARD TABLE OF zslot_st_do_filter WITH DEFAULT KEY.
