INTERFACE zif_do_seo
  PUBLIC .

  TYPES:
    td_hex_seperator(1) TYPE x,
    BEGIN OF ts_filter,
      fieldname TYPE fieldname,
      low       TYPE char72,
      high      TYPE char72,
      filter_on TYPE char1,
      pb(80),
      include   TYPE zst_do_rowobject,
    END OF ts_filter,

    BEGIN OF ts_field_values,
      fieldname  TYPE fieldname,
      fieldvalue TYPE REF TO data,
    END OF ts_field_values,

    BEGIN OF ts_selection_options,
      fieldname TYPE fieldname,
      sign(1)   TYPE c,
      option(2) TYPE c,
      ptrlow    TYPE REF TO data,
      ptrhigh   TYPE REF TO data,
    END OF ts_selection_options,

    BEGIN OF ts_so_rowobj,
      fieldname TYPE fieldname,
      sign(1)   TYPE c,
      option(2) TYPE c,
      ptrlow    TYPE REF TO data,
      ptrhigh   TYPE REF TO data,
      include   TYPE zst_do_rowobject,
    END OF ts_so_rowobj,

    tt_field_values
      TYPE STANDARD TABLE OF ts_field_values WITH DEFAULT KEY,
    tt_so_rowobj
      TYPE STANDARD TABLE OF ts_so_rowobj WITH DEFAULT KEY,
    tt_selection_options
      TYPE STANDARD TABLE OF ts_selection_options WITH DEFAULT KEY,
    tt_filter
      TYPE STANDARD TABLE OF ts_filter WITH DEFAULT KEY.
ENDINTERFACE.
