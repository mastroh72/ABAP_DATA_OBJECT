FUNCTION ZTB_TOOLS_01_SEARCH_MAPPING.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(CXTAB_CONTROL) TYPE  CXTAB_CONTROL OPTIONAL
*"     REFERENCE(SELECTEDCOLUMNS) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(ALLFIELDS) TYPE  STRUKNAME OPTIONAL
*"  TABLES
*"      CXTAB_COLUMN TYPE  TABLE OPTIONAL
*"      FIELDS_TO_SEARCH TYPE  ZTT_DO_TB_FIELD_TO_SEARCH
*"----------------------------------------------------------------------

  FIELD-SYMBOLS:
    <cols>  TYPE ANY TABLE,
    <field> TYPE ANY,
    <screen> TYPE ANY,
    <selected> TYPE ANY,
    <wa>    TYPE ANY,
    <invisible> TYPE ANY,
    <input> TYPE ANY,
    <table> TYPE ANY TABLE.


  DATA :
      order TYPE numc4,
      fieldname(132) TYPE c,
      temp      LIKE fieldname,
      it_fieldtab TYPE STANDARD TABLE OF dfies
           WITH HEADER LINE.

  DEFINE get_fieldname.
    assign component 'SCREEN' of structure <wa> to <screen>.
    assign component 'NAME' of structure <screen> to <field>.
    assign component 'INPUT' of structure <screen> to <input>.
    assign component 'INVISIBLE' of structure <screen> to <invisible>.
    fieldname = <field>.
    while fieldname cs '-'.
      split fieldname at '-' into temp fieldname.
    endwhile.
    if <invisible> = 0 and <input> = 1.
      fields_to_search-replaceallowed = 'X'.
    else.
      clear : fields_to_search-replaceallowed.
    endif.
    fields_to_search-fieldname = fieldname.
    fields_to_search-searchorder = order.
    order = order + 1.
    append fields_to_search.
  END-OF-DEFINITION.

  DEFINE build_from_cols.
    if selectedcolumns is initial.
      loop at <cols> assigning <wa>.
        get_fieldname.
      endloop.
    else.
      loop at <cols> assigning <wa>.
        assign component 'SELECTED' of structure <wa> to <selected>.
        if <selected> = 'X'.
          get_fieldname.
        endif.
      endloop.
      read table fields_to_search index 1 transporting no fields .
      if sy-subrc ne 0.
        loop at <cols> assigning <wa>.
          get_fieldname.
        endloop.
      endif.
    endif.
  END-OF-DEFINITION.



  order = 1.
  IF cxtab_control IS INITIAL.
    READ TABLE cxtab_column INDEX 1 TRANSPORTING NO FIELDS .
    IF sy-subrc = 0.
      ASSIGN :
        cxtab_column[] TO <cols>.
      build_from_cols.
    ELSEIF allfields IS INITIAL.

    ELSE.
      CALL FUNCTION 'GET_FIELDTAB'
       EXPORTING
         langu                     = sy-langu
         only                      = ' '
         tabname                   = allfields
         withtext                  = space
*       IMPORTING
*         HEADER                    =
*         RC                        =
        TABLES
          fieldtab                  = it_fieldtab
       EXCEPTIONS
         internal_error            = 1
         no_texts_found            = 2
         table_has_no_fields       = 3
         table_not_activ           = 4
         OTHERS                    = 5
                .
      LOOP AT it_fieldtab.
        fields_to_search-fieldname =  it_fieldtab-fieldname.
        fields_to_search-searchorder = order.
        order = order + 1.
        APPEND fields_to_search.
      ENDLOOP.

    ENDIF.
  ELSE.
    ASSIGN :
      cxtab_control-cols[] TO <cols>.
    build_from_cols.
  ENDIF.

  SORT fields_to_search BY searchorder.
ENDFUNCTION.
