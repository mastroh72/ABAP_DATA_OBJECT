FUNCTION Z_DO_TB_TOOLS_01_SELECTION.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      IT_SELECTION TYPE  ZDO01_TT_SELECTION_OPTIONS
*"      IT_FIELD_LIST TYPE  SPAR_DFIES
*"----------------------------------------------------------------------





  DATA:
*    it_field_desc   TYPE STANDARD TABLE OF fldconvert WITH HEADER LINE,
    lt_field_desc          TYPE STANDARD TABLE OF fldconvert,
    ls_field_desc          TYPE fldconvert,
    it_field_text          TYPE STANDARD TABLE OF rsdstexts WITH HEADER LINE,
    it_expressions         TYPE rsds_texpr,
    it_field_ranges        TYPE  rsds_trange,

    restrict               TYPE sscr_restrict_ds,
    c_title                LIKE sy-title,
    selection_id           LIKE rsdynsel-selid,
    it_field_tab           TYPE STANDARD TABLE OF rsdsfields
      WITH HEADER LINE,
    it_fields_not_selected TYPE STANDARD TABLE OF rsdsfields
      WITH HEADER LINE,
*    ctabname    TYPE tabname,
*    cfieldname  TYPE fieldname,
    current_field          TYPE fieldname,
    wa_selopt              TYPE rsdsselopt,
    wa_frange              TYPE rsds_frange,
    wa_expr_tab            TYPE LINE OF rsds_expr_tab,
    prev_table             TYPE tabname,
    lg_ddic_table          TYPE abap_bool,
    wa_field_range         LIKE LINE OF it_field_ranges.








  FIELD-SYMBOLS:
    <wa_expr_tab>     TYPE LINE OF rsds_texpr,
    <wa_selopt>       TYPE LINE OF rsds_selopt_t,
    <wa_frange>       TYPE LINE OF rsds_frange_t,
    <wa_field_ranges> LIKE LINE OF it_field_ranges,
    <wa_expressions>  LIKE LINE OF  it_expressions,
    <low>             TYPE any,
    <high>            TYPE any.




  c_title = text-001.




  SORT it_selection BY fieldname.
  LOOP AT it_selection WHERE NOT sign IS INITIAL.
    READ TABLE it_field_list
      WITH KEY fieldname = it_selection-fieldname.
    IF sy-subrc = 0.
*  Look for Table Row if not available add
      READ TABLE  it_field_ranges ASSIGNING  <wa_field_ranges>
        WITH KEY tablename = it_field_list-tabname.
      IF sy-subrc NE 0.
        wa_field_range-tablename = it_field_list-tabname.
        APPEND    wa_field_range TO it_field_ranges.
        READ TABLE it_field_ranges ASSIGNING  <wa_field_ranges>
          INDEX sy-tabix.
      ENDIF.
*  Look for Fieldname Row if not Availalbe add
      READ TABLE <wa_field_ranges>-frange_t ASSIGNING <wa_frange>
        WITH KEY fieldname = it_selection-fieldname        .
      IF sy-subrc NE 0.
        wa_frange-fieldname = it_selection-fieldname.
        APPEND wa_frange TO <wa_field_ranges>-frange_t.
        READ TABLE <wa_field_ranges>-frange_t ASSIGNING <wa_frange>
          INDEX sy-tabix.
      ENDIF.
*  Add Record to selopts
      wa_selopt-sign = it_selection-sign.
      wa_selopt-option = it_selection-option.

      ASSIGN:
        it_selection-ptrlow->*  TO <low>.
      IF sy-subrc = 0.
        wa_selopt-low = <low>.
      ENDIF.
      ASSIGN :
        it_selection-ptrhigh->* TO <high>.
      IF sy-subrc = 0.
        wa_selopt-high = <high>.
      ENDIF.
      CASE it_field_list-inttype.
        WHEN 'I' OR 'P'.
          CONDENSE:
             wa_selopt-low,
             wa_selopt-high.
      ENDCASE.
      CASE it_field_list-convexit.
        WHEN 'ALPHA'.
          wa_selopt-low = |{ wa_selopt-low ALPHA = OUT }|.
          wa_selopt-high = |{ wa_selopt-high ALPHA = OUT }|.
        WHEN OTHERS.
      ENDCASE.
      APPEND wa_selopt TO <wa_frange>-selopt_t.
    ENDIF.
  ENDLOOP.
*  fieldname like rsdstabs-prim_fname,
*  selopt_t type rsds_selopt_t,

  CALL FUNCTION 'FREE_SELECTIONS_RANGE_2_EX'
    EXPORTING
      field_ranges = it_field_ranges
    IMPORTING
      expressions  = it_expressions.






  lg_ddic_table = abap_false.

  REFRESH:
     it_field_text.


  LOOP AT it_field_list.
    IF prev_table NE  it_field_list-tabname.
      SELECT SINGLE typename INTO prev_table
          FROM ddtypes
          WHERE typename = it_field_list-tabname.
      IF sy-subrc = 0.
        lg_ddic_table = abap_true.
      ELSE.
        lg_ddic_table = abap_false.
        prev_table = it_field_list-tabname.
      ENDIF.
    ENDIF.

    IF it_field_list-langu IS INITIAL OR lg_ddic_table = abap_false.
* Since Not from abap dictionary clear table name.
      LOOP AT it_expressions ASSIGNING <wa_expressions> WHERE tablename = it_field_list-tabname.
        CLEAR <wa_expressions>-tablename.
      ENDLOOP.

      CLEAR:
         it_field_tab-tablename,
         it_field_text-tablename.
      it_field_tab-type = it_field_list-inttype.
      it_field_tab-where_leng = it_field_list-outputlen.

      CLEAR ls_field_desc.

      ls_field_desc-fieldname   = it_field_list-fieldname.
      ls_field_desc-type        = it_field_list-inttype.
* This multiple is needed becuase FORM HANDLE_NON_DD_FIELD IN LSSELFXX divideds length by charsize
* If this does not happen vbeln = 10 character fields a user
* enters is 1234567890 and only 12345 would return with out the code.
      CASE  ls_field_desc-type.
        WHEN 'C'.
          ls_field_desc-length      = it_field_list-outputlen *  cl_abap_char_utilities=>charsize.
        WHEN OTHERS.
          ls_field_desc-length      = it_field_list-outputlen.
      ENDCASE.
      ls_field_desc-clength     = it_field_list-outputlen.
      ls_field_desc-olength     = it_field_list-outputlen.
      ls_field_desc-where_leng  = it_field_list-outputlen.
      ls_field_desc-ddic_leng   = it_field_list-outputlen.
* Free has issue with g and a sign.  the maxium size is 45
* for selection fields.  when g is used and a sign
* it makes it 46 which causes dumps.
      CASE  ls_field_desc-type.
        WHEN 'g'.
          ls_field_desc-sign        = abap_false.
        WHEN OTHERS.
          ls_field_desc-sign        = it_field_list-sign.
      ENDCASE.
      ls_field_desc-decimals    = it_field_list-decimals.
      ls_field_desc-dddecimals  = it_field_list-decimals.
      ls_field_desc-lower       = it_field_list-lowercase.
      ls_field_desc-convexit    = it_field_list-convexit.
      ls_field_desc-outputstyle = it_field_list-outputstyle.
      APPEND ls_field_desc TO lt_field_desc.

    ELSE.
      it_field_tab-tablename = it_field_list-tabname.
      it_field_text-tablename = it_field_list-tabname.

    ENDIF.


    it_field_text-fieldname = it_field_list-fieldname.
    DATA(ilonglenth) = strlen( it_field_list-scrtext_l ).
    IF ilonglenth GT 0 AND ilonglenth < 31.
      it_field_text-text = it_field_list-scrtext_l.
    ELSEIF it_field_list-scrtext_m IS NOT INITIAL.
      it_field_text-text = it_field_list-scrtext_m.
    ELSEIF it_field_list-scrtext_s IS NOT INITIAL.
      it_field_text-text = it_field_list-scrtext_s.
    ELSE.
      it_field_text-text = it_field_text-fieldname.
    ENDIF.
    APPEND it_field_text.

    it_field_tab-fieldname = it_field_list-fieldname.
    CASE it_field_list-inttype.
      WHEN 'P' OR 'I'.
        it_field_tab-sign = 'X'.
        it_field_tab-decimals = it_field_list-decimals.
      WHEN OTHERS.
        CLEAR:
            it_field_tab-decimals,
            it_field_tab-sign.
    ENDCASE.
    APPEND it_field_tab.
  ENDLOOP.

  it_fields_not_selected[] = it_field_tab[].

  LOOP AT it_fields_not_selected.
    IF line_exists( it_selection[ fieldname =  it_fields_not_selected-fieldname ] ) .
      DELETE it_fields_not_selected.
    ENDIF.
  ENDLOOP.


*     it_field_desc.
*  LOOP AT it_selection.
*    IF current_field NE it_selection-fieldname.
*      READ TABLE it_field_list
*          WITH KEY fieldname = it_selection-fieldname.
*      it_field_tab-tablename = it_field_list-tabname.
*      it_field_tab-fieldname = it_selection-fieldname.
*
*      IF prev_table NE  it_field_tab-tablename.
*        SELECT SINGLE typename INTO prev_table
*            FROM ddtypes
*            WHERE typename = it_field_tab-tablename.
*        IF sy-subrc = 0.
*          lg_ddic_table = abap_true.
*        ELSE.
*          lg_ddic_table = abap_false.
*          prev_table = it_field_tab-tablename.
*        ENDIF.
*      ENDIF.
*
*
*      IF it_field_list-langu IS INITIAL OR lg_ddic_table = abap_false.
** Since Not from abap dictionary clear table name.
*        LOOP AT it_expressions ASSIGNING <wa_expressions> WHERE tablename = it_field_tab-tablename.
*          CLEAR <wa_expressions>-tablename.
*        ENDLOOP.
*
*        CLEAR it_field_tab-tablename.
*        it_field_tab-type = it_field_list-inttype.
*        it_field_tab-where_leng = it_field_list-outputlen.
**        it_field_desc-fieldname = it_selection-fieldname.*
**        APPEND it_field_desc.
*        it_field_text-fieldname = it_selection-fieldname.
*        IF it_field_list-scrtext_s IS NOT INITIAL.
*          it_field_text-text = it_field_list-scrtext_s.
*        ELSEIF it_field_list-scrtext_m IS NOT INITIAL.
*          it_field_text-text = it_field_list-scrtext_m.
*        ELSE.
*          it_field_text-text = it_field_text-fieldname.
*        ENDIF.
*        APPEND it_field_text.
*
*
*
*      ENDIF.
*
*      APPEND it_field_tab.
*      current_field = it_selection-fieldname.
*    ENDIF.
*  ENDLOOP.







  CALL FUNCTION 'FREE_SELECTIONS_INIT'
    EXPORTING
      kind                     = 'F'
      expressions              = it_expressions
      restriction              = restrict
    IMPORTING
      selection_id             = selection_id
    TABLES
      fields_tab               = it_field_tab
      field_desc               = lt_field_desc
      field_texts              = it_field_text
      fields_not_selected      = it_fields_not_selected
    EXCEPTIONS
      fields_incomplete        = 1
      fields_no_join           = 2
      field_not_found          = 3
      no_tables                = 4
      table_not_found          = 5
      expression_not_supported = 6
      incorrect_expression     = 7
      illegal_kind             = 8
      area_not_found           = 9
      inconsistent_area        = 10
      kind_f_no_fields_left    = 11
      kind_f_no_fields         = 12
      too_many_fields          = 13
      dup_field                = 14
      field_no_type            = 15
      field_ill_type           = 16
      dup_event_field          = 17
      node_not_in_ldb          = 18
      area_no_field            = 19
      OTHERS                   = 20.

  IF sy-subrc = 0.
    CALL FUNCTION 'FREE_SELECTIONS_DIALOG'
      EXPORTING
        selection_id        = selection_id
        title               = c_title
        frame_text          = text-002
        status              = 1
        as_window           = 'X'
        no_intervals        = ' '
        just_display        = ' '
        tree_visible        = ' '
        diag_text_2         = text-002
      IMPORTING
        field_ranges        = it_field_ranges
      TABLES
        fields_tab          = it_field_tab
        fields_not_selected = it_fields_not_selected
      EXCEPTIONS
        internal_error      = 1
        no_action           = 2
        selid_not_found     = 3
        illegal_status      = 4
        OTHERS              = 5.



    IF sy-subrc = 0.
      LOOP AT it_field_ranges ASSIGNING  <wa_field_ranges>.
        LOOP AT <wa_field_ranges>-frange_t ASSIGNING <wa_frange>.
          DELETE it_selection WHERE fieldname = <wa_frange>-fieldname.
          it_selection-fieldname =  <wa_frange>-fieldname.
          READ TABLE it_field_list
              WITH KEY fieldname = it_selection-fieldname.
          IF sy-subrc = 0.
            LOOP AT <wa_frange>-selopt_t ASSIGNING <wa_selopt>.
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
                  WHEN OTHERS.
                    CREATE DATA:
                       it_selection-ptrlow  LIKE <wa_selopt>-low,
                       it_selection-ptrhigh LIKE <wa_selopt>-low.
                ENDCASE.
              ELSE.
                CREATE DATA:
                  it_selection-ptrlow  TYPE (it_field_list-rollname),
                  it_selection-ptrhigh TYPE (it_field_list-rollname).
              ENDIF.
              ASSIGN:
                it_selection-ptrlow->*  TO <low>,
                it_selection-ptrhigh->*  TO <high>.
              it_selection-option =  <wa_selopt>-option.
              it_selection-sign   =  <wa_selopt>-sign.
              <low> =  <wa_selopt>-low.
              <high> =  <wa_selopt>-high.
              CASE it_field_list-convexit.
                WHEN 'ALPHA'.
                  <low> = |{ <low> ALPHA = IN }|.
                  <high> = |{ <high> ALPHA = IN }|.
                WHEN OTHERS.
              ENDCASE.
              APPEND it_selection.
            ENDLOOP.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
  ENDIF.

  DELETE it_selection WHERE sign IS INITIAL.
ENDFUNCTION.
