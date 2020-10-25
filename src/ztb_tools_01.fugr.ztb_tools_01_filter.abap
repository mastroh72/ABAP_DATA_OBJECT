FUNCTION ztb_tools_01_filter.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(CXTAB_CONTROL) TYPE  CXTAB_CONTROL OPTIONAL
*"     REFERENCE(IM_ASK_FILTER_ON) TYPE  CHAR1 DEFAULT SPACE
*"  TABLES
*"      IT_SELECTION TYPE  ZSLOT_TT_DO_SELECTION_OPTIONS
*"      IT_FIELD_LIST TYPE  SPAR_DFIES
*"  CHANGING
*"     VALUE(CH_FILTER_ON) TYPE  CHAR1
*"  EXCEPTIONS
*"      CANCEL
*"      NO_CHANGES
*"----------------------------------------------------------------------
  DATA:
    no_change  TYPE char1,
    ptr        TYPE REF TO data,
    prev_field TYPE fieldname.


  FIELD-SYMBOLS:
    <from_low>  TYPE any,
    <from_high> TYPE any,
    <low>       TYPE any,
    <high>      TYPE any.

  g_filter_on = ch_filter_on.
  g_ask_filter_on = im_ask_filter_on.
  REFRESH git_filter.
  LOOP AT it_field_list.
    IF prev_field NE it_field_list-fieldname.
      git_filter-fieldlabel = it_field_list-scrtext_m.
      git_filter-tablename = it_field_list-tabname.
      git_filter-rollname = it_field_list-rollname.
      git_filter-inttype = it_field_list-inttype.
      git_filter-content = it_field_list-scrtext_m.
      git_filter-fieldname = it_field_list-fieldname.
      APPEND git_filter.
      prev_field = git_filter-fieldname.
    ENDIF.
  ENDLOOP.

  REFRESH git_selection.
  LOOP AT it_selection.
    git_selection = it_selection.
    READ TABLE it_field_list
      WITH KEY fieldname = it_selection-fieldname.
    IF sy-subrc = 0.
      IF it_field_list-rollname IS INITIAL.
        CASE it_field_list-inttype.  "See Domain INTTYPE.
          WHEN 'I'.             "I  Integer number (4-byte integer with sign)
            CREATE DATA:
               git_selection-ptrlow  TYPE i,
               git_selection-ptrhigh TYPE i.
          WHEN 'g'.             "g  Character string with variable length (ABAP type STRING)
            CREATE DATA:
               git_selection-ptrlow  TYPE string,
               git_selection-ptrhigh TYPE string.
        ENDCASE.
      ELSE.
        CREATE DATA:
          git_selection-ptrlow  TYPE (it_field_list-rollname),
          git_selection-ptrhigh TYPE (it_field_list-rollname).
      ENDIF.

*      CREATE DATA git_selection-ptrlow TYPE (it_field_list-rollname).
*      CREATE DATA git_selection-ptrhigh TYPE (it_field_list-rollname).
      ASSIGN:
        git_selection-ptrlow->*  TO <low>,
        git_selection-ptrhigh->* TO <high>,
        it_selection-ptrlow->*   TO <from_low>,
        it_selection-ptrhigh->*  TO <from_high>.

      <low> = <from_low>.
      <high> = <from_high>.
    ENDIF.
    APPEND git_selection.
  ENDLOOP.

  READ TABLE git_selection INDEX 1 TRANSPORTING NO FIELDS.
  IF sy-subrc NE 0.
    g_filter_on  = 'X'.
  ENDIF.

  GET REFERENCE OF git_selection[] INTO ptr.
  CALL METHOD :
    gobj_filter->set_it_field_list
      EXPORTING
        im_field_list = it_field_list[],
    gobj_filter->set_selection
      EXPORTING
         im_selection = ptr,
    gobj_filter->it_force_integrity,
    gobj_filter->tc_set_displayed_lines
      EXPORTING
        im_lines = 1,
    gobj_filter->set_content
      EXPORTING
        im_option = cl_filter=>co_field_label.

  CLEAR:
    fcode,
    gfcode.

  CALL SCREEN  220 STARTING AT 10 05
                   ENDING   AT 102 18.


  IF gfcode = 'ENTER'.
    no_change = space.

    READ TABLE git_selection INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
      CLEAR  g_filter_on.
    ENDIF.
*    LOOP AT git_selection.
*      READ TABLE it_sort_columns
*        WITH KEY sortorder = git_sort_cols-rowseq.
*      IF sy-subrc = 0.
*        IF it_sort_columns-fieldname NE git_sort_cols-fieldname.
*          CLEAR no_change.
*          EXIT.
*        ENDIF.
*        IF it_sort_columns-sortorder NE git_sort_cols-rowseq.
*          CLEAR no_change.
*          EXIT.
*        ENDIF.
*        IF NOT ( git_sort_cols-sort_up = 'X' AND
*                ( it_sort_columns-direction = 'A' OR
*                  it_sort_columns-direction = 'a' ) ) .
*          CLEAR no_change.
*          EXIT.
*        ENDIF.
*      ELSE.
*        CLEAR no_change.
*        EXIT.
*      ENDIF.
*    ENDLOOP.
    ch_filter_on = g_filter_on .
    IF no_change = 'X'.
      RAISE no_changes.
    ELSE.
      REFRESH it_selection.
      LOOP AT git_selection.
        it_selection = git_selection.
        READ TABLE it_field_list
          WITH KEY fieldname = git_selection-fieldname.
        IF sy-subrc = 0.
          IF it_field_list-rollname IS INITIAL.
            CASE it_field_list-inttype.  "See Domain INTTYPE.
              WHEN 'I'.             "I  Integer number (4-byte integer with sign)
                CREATE DATA:
                   it_selection-ptrlow  TYPE i,
                   it_selection-ptrhigh TYPE i.
              WHEN 'g'.             "g  Character string with variable length (ABAP type STRING)
                CREATE DATA:
                   it_selection-ptrlow  TYPE string,
                   it_selection-ptrhigh TYPE string.
            ENDCASE.
          ELSE.
            CREATE DATA:
              it_selection-ptrlow  TYPE (it_field_list-rollname),
              it_selection-ptrhigh TYPE (it_field_list-rollname).
          ENDIF.

*          CREATE DATA:
*            it_selection-ptrlow  TYPE (it_field_list-rollname),
*            it_selection-ptrhigh TYPE (it_field_list-rollname).
          ASSIGN:
            it_selection-ptrlow->*  TO <low>,
            it_selection-ptrhigh->* TO <high>,
            git_selection-ptrlow->*   TO <from_low>,
            git_selection-ptrhigh->*  TO <from_high>.

          <low> = <from_low>.
          <high> = <from_high>.
        ENDIF.
        APPEND it_selection.
      ENDLOOP.
    ENDIF.
  ELSEIF gfcode = 'CANCEL'.
    RAISE cancel.
  ENDIF.






ENDFUNCTION.
