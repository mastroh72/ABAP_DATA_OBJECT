INTERFACE zif_do
  PUBLIC .


  TYPES:
    td_hex_seperator(1) TYPE x .
  TYPES:
    BEGIN OF ts_function_codes,
      fcode          TYPE sy-ucomm,
      update_only    TYPE char1,
      use_method(70) TYPE c,
    END OF ts_function_codes .
  TYPES:
    BEGIN OF ts_marked_rows,
      rowid LIKE sy-tabix,
    END OF ts_marked_rows .
  TYPES:
    tt_function_codes TYPE HASHED TABLE OF ts_function_codes
       WITH UNIQUE KEY fcode .
  TYPES:
    tt_markedrows     TYPE STANDARD TABLE OF ts_marked_rows WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ts_field_mapping,
      relationship_seq TYPE sydbcnt,
      published_field  TYPE fieldname,
      subscribed_field TYPE fieldname,
      foreign_key      TYPE char1,
      fieldvalue       TYPE REF TO data,
    END OF ts_field_mapping .
  TYPES:
    tt_field_mapping
      TYPE STANDARD TABLE OF ts_field_mapping
      WITH NON-UNIQUE DEFAULT KEY .
  TYPES:
    BEGIN OF ts_id_field_mapping,
      relationship TYPE zdo_object_relationship,
      id           TYPE zdo_object_id,
      active       TYPE char1,
      map          TYPE  tt_field_mapping,
    END OF ts_id_field_mapping .
  TYPES:
    tt_id_field_mapping
      TYPE STANDARD TABLE OF ts_id_field_mapping WITH DEFAULT KEY .
ENDINTERFACE.
