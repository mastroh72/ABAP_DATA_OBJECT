TYPE-POOL zdo .
TYPES:
  zdo_do_hex_seperator TYPE x,
  BEGIN OF zdo_st_do_function_codes,
    fcode          TYPE sy-ucomm,
    update_only    TYPE char1,
    use_method(70) TYPE c,
  END OF zdo_st_do_function_codes,
  BEGIN OF zdo_st_do_marked_rows,
    rowid LIKE sy-tabix,
  END OF zdo_st_do_marked_rows,
  zdo_tt_do_function_codes TYPE HASHED TABLE OF zdo_st_do_function_codes
     WITH UNIQUE KEY fcode,
  zdo_tt_do_markedrows     TYPE STANDARD TABLE OF zdo_st_do_marked_rows WITH DEFAULT KEY,
  BEGIN OF zdo_st_do_field_mapping,
    relationship_seq TYPE sydbcnt,
    published_field  TYPE fieldname,
    subscribed_field TYPE fieldname,
    foreign_key      TYPE char1,
    fieldvalue       TYPE REF TO data,
  END OF zdo_st_do_field_mapping,
  zdo_tt_do_field_mapping
    TYPE STANDARD TABLE OF zdo_st_do_field_mapping
    WITH NON-UNIQUE DEFAULT KEY,
  BEGIN OF zdo_st_do_id_field_mapping,
    relationship TYPE zdo_object_relationship,
    id           TYPE zdo_object_id,
    active       TYPE char1,
    map          TYPE  zdo_tt_do_field_mapping,
  END OF zdo_st_do_id_field_mapping,
  zdo_tt_do_id_field_mapping
    TYPE STANDARD TABLE OF zdo_st_do_id_field_mapping WITH DEFAULT KEY.
