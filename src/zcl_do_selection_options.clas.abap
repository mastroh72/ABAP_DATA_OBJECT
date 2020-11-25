class ZCL_DO_SELECTION_OPTIONS definition
  public
  create public .

public section.

  constants CO_QUOTE type CHAR1 value '''' ##NO_TEXT.
  constants CO_CHAR_0 type ZIF_DO_SEO=>td_hex_seperator value '00' ##NO_TEXT.
  class-data C_QUOTE_CHAR_0 type CHAR2 read-only .
  class-data C_DOUBLE_QUOTE type CHAR2 read-only .
  data FILTER_ON type CHAR1 read-only .

  class-methods CLASS_CONSTRUCTOR .
  class-methods CONVERT_TO_CHARACTER .
  methods ADD_CONDITION
    importing
      !IM_FIELD_NAME type FIELDNAME
      value(IM_SIGN) type CHAR1 default 'I'
      value(IM_OPTION) type CHAR2 default 'EQ'
      !IM_LOW type ANY
      value(IM_HIGH) type ANY optional
    exceptions
      INVALID_FIELD_NAME
      INVALID_SIGN
      INVALID_OPTION
      HIGH_POPULATED
      HIGH_MISSING .
  methods ADD_CONDITION_BY_SELECTION
    importing
      !IM_IT_SELECTION type ZIF_DO_SEO=>tt_selection_options
      !IM_REFRESH_FIELDNAMES type CHAR1 .
  methods ADD_CONDITION_BY_SEL_FILTER
    importing
      !IM_IT_SELECTION type ZIF_DO_SEO=>tt_selection_options
      !IM_REFRESH_FIELDNAMES type CHAR1 .
  methods ASK_SELECTION_OPTIONS
    importing
      !IM_IT_FIELDNAME type ZTT_DO_FIELDNAME optional
    exceptions
      NO_CHANGES .
  methods ASSIGN_VALUE
    importing
      !IM_FIELD_NAME type FIELDNAME
      !IM_VALUE type ANY .
  methods CLEAR_VALUE
    importing
      !IM_FIELD_NAME type FIELDNAME optional .
  methods CONSTRUCTOR
    importing
      !IM_IT_VALID_FIELDS type SPAR_DFIES .
  methods DELETE_CONDITION
    importing
      !IM_FIELD_NAME type FIELDNAME optional .
  methods DELETE_CONDITION_EXCLUDE
    importing
      !IM_IT_FIELD_NAME type ZTT_DO_FIELDNAME optional .
  methods GET_ASK_FILTER_ON
    exporting
      value(EX_ASK_FILTER_ON) type CHAR1 .
  methods GET_CONDITION
    importing
      !IM_FIELD_NAME type FIELDNAME optional
    returning
      value(RE_RANGE) type ZIF_DO_SEO=>tt_selection_options .
  methods GET_DATABASE_WHERE
    exporting
      !EX_IT_WHERE_CLAUSE type ZTT_DO_WHERE_CLAUSE .
  methods GET_DATA_TYPE
    importing
      value(IM_FIELD_NAME) type FIELDNAME
    exporting
      !EX_DATA_TYPE type ROLLNAME
    exceptions
      NO_FIELD_FOUND .
  methods GET_FIELD_VALUE
    exporting
      !EX_IT_FIELD_VALUES type ZIF_DO_SEO=>tt_field_values .
  methods GET_FILTER_ON
    exporting
      value(EX_FILTER_ON) type CHAR1 .
  methods IS_CONDITION
    importing
      !IM_FIELD_NAME type FIELDNAME
      value(IM_SIGN) type CHAR1 default 'I'
      value(IM_OPTION) type CHAR2 default 'EQ'
      value(IM_LOW) type ANY optional
      value(IM_HIGH) type ANY optional
    exporting
      !EX_EXIST type CHAR1
    exceptions
      INVALID_FIELD_NAME
      INVALID_SIGN
      INVALID_OPTION
      HIGH_POPULATED
      HIGH_MISSING .
  methods IS_MATCH
    exporting
      value(EX_MATCH) type CHAR1 .
  methods MAKE_WHERE_VALUE
    importing
      !IM_FIELDNAME type FIELDNAME
      !IM_VALUE type ANY
    exporting
      !EX_VALUE type ZST_DO_WHERE_CLAUSE .
  methods RENAME_CONDITION
    importing
      value(IM_FIELD_NAME_FROM) type FIELDNAME optional
      value(IM_FIELD_NAME_TO) type FIELDNAME optional .
  methods SET_ASK_FILTER_ON
    importing
      value(IM_ASK_FILTER_ON) type CHAR1 .
  methods SET_FILTER_ON
    importing
      value(IM_FILTER_ON) type CHAR1 .
  methods SET_VALID_FIELDS
    importing
      !IM_IT_VALID_FIELDS type SPAR_DFIES .
  methods SET_VALID_FIELDS_DESCRIPTIONS
    importing
      !IM_IT_VALID_FIELDS type SPAR_DFIES .
protected section.

  data IT_FIELD_VALUES type ZIF_DO_SEO=>tt_field_values .
  data IT_SELECTION type ZIF_DO_SEO=>tt_selection_options .
  data IT_VALID_FIELDS type SPAR_DFIES .
  data ASK_FILTER_ON type CHAR1 .

  methods BUILD_FIELD_VALUES .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DO_SELECTION_OPTIONS IMPLEMENTATION.


  METHOD add_condition.
* ...

    DATA :
      wa_selection   TYPE ZIF_DO_SEO=>ts_selection_options,
      wa_fieldvalues TYPE ZIF_DO_SEO=>ts_field_values.
    FIELD-SYMBOLS :
      <high>            TYPE any,
      <low>             TYPE any,
      <wa_valid_fields> TYPE  dfies.

*  IF im_sign IS INITIAL.
*    im_sign = 'I'.
*  ENDIF.
*  IF im_option IS INITIAL.
*    im_option = 'EQ'.
*  ENDIF.

    CASE im_sign.
      WHEN 'I' OR 'E'.
        READ TABLE it_valid_fields ASSIGNING <wa_valid_fields>
          WITH KEY fieldname = im_field_name.
        IF sy-subrc = 0.
          IF <wa_valid_fields>-rollname IS INITIAL.
            CASE <wa_valid_fields>-inttype.  "See Domain INTTYPE.
                "C  Character String
                "N  Character String with Digits Only
                "D  Date (Date: YYYYMMDD)
                "T  Time (Time: HHMMSS)
                "X  Byte Seq. (heXadecimal), in DDIC metadata also for INT1/2/4
              WHEN 'I'.             "I  Integer number (4-byte integer with sign)
                CREATE DATA:
                    wa_selection-ptrlow  TYPE i,
                    wa_selection-ptrhigh  TYPE i.
                "b  1-byte integer, integer number <= 254
                "s  2-byte integer, only for length field before LCHR or LRAW
                "P  Packed number
                "F  Floating point number to accuracy of 8 bytes
              WHEN 'g'.             "g  Character string with variable length (ABAP type STRING)
                CREATE DATA:
                    wa_selection-ptrlow  TYPE string,
                    wa_selection-ptrhigh TYPE string.
                "y  Byte sequence with variable length (ABAP type XSTRING)
                "u  Structured type, flat
                "v  Structured type, deep
                "h  Table type
                "V  Character string (old Dictionary type VARC)
                "r  Reference to class/interface
                "l  Reference to data object
                "a  Decimal Floating Point Number, 16 Digits
                "e  Decimal Floating Point Number, 34 Digits
                "j  Static Boxed Components
                "k  Generic Boxed Components
              WHEN OTHERS.
                CREATE DATA:
                   wa_selection-ptrlow  LIKE im_low,
                   wa_selection-ptrhigh  LIKE im_low.
            ENDCASE.
          ELSE.
            CREATE DATA:
              wa_selection-ptrlow  TYPE (<wa_valid_fields>-rollname),
              wa_selection-ptrhigh TYPE (<wa_valid_fields>-rollname) .
          ENDIF.
          ASSIGN:
            wa_selection-ptrlow->* TO <low>,
            wa_selection-ptrhigh->* TO <high>.
          <low> = im_low.
          <high> = im_high.
          wa_selection-fieldname = im_field_name.
          wa_selection-sign = im_sign.
          wa_selection-option = im_option.
          CASE wa_selection-option.
            WHEN 'EQ' OR 'NE' OR
                 '='  OR '<>' OR
                 'GT' OR 'GE' OR
                 '>'  OR '>=' OR
                 'LT' OR 'LE' OR
                 '<'  OR '<=' OR
                 'CP' OR 'NP'.
              IF NOT <high> IS INITIAL.
                RAISE high_populated.
              ENDIF.
            WHEN 'BT' OR 'NB'.
              IF <high> IS INITIAL.
                RAISE high_missing.
              ENDIF.
            WHEN OTHERS.
              RAISE invalid_option.
          ENDCASE.
          APPEND wa_selection TO it_selection.
          READ TABLE it_field_values WITH KEY fieldname = im_field_name
            TRANSPORTING NO FIELDS.
          IF sy-subrc NE 0.
            wa_fieldvalues-fieldname = im_field_name.
            IF <wa_valid_fields>-rollname IS INITIAL.
              CASE <wa_valid_fields>-inttype.  "See Domain INTTYPE.
                WHEN 'I'.             "I  Integer number (4-byte integer with sign)
                  CREATE DATA:
                      wa_fieldvalues-fieldvalue  TYPE i.
                WHEN 'g'.             "g  Character string with variable length (ABAP type STRING)
                  CREATE DATA:
                      wa_fieldvalues-fieldvalue  TYPE string.
                WHEN OTHERS.
                  CREATE DATA:
                      wa_fieldvalues-fieldvalue  LIKE im_low.
              ENDCASE.
            ELSE.
              CREATE DATA:
                wa_fieldvalues-fieldvalue
                  TYPE (<wa_valid_fields>-rollname).
            ENDIF.
            APPEND wa_fieldvalues TO it_field_values.
          ENDIF.
        ELSE.
          RAISE invalid_field_name.
        ENDIF.
      WHEN OTHERS.
        RAISE invalid_sign.
    ENDCASE.

  ENDMETHOD.


  METHOD add_condition_by_selection.
* ...

    DATA :
      prev_fieldname TYPE fieldname.

    FIELD-SYMBOLS:
      <wa_selection> LIKE LINE OF im_it_selection.

    IF im_refresh_fieldnames = 'X'.
      LOOP AT im_it_selection ASSIGNING <wa_selection>.
        IF prev_fieldname NE <wa_selection>-fieldname.
          prev_fieldname =  <wa_selection>-fieldname.
          CALL METHOD me->delete_condition
            EXPORTING
              im_field_name = <wa_selection>-fieldname.
        ENDIF.
      ENDLOOP.
    ENDIF.

    APPEND LINES OF im_it_selection TO it_selection.

  ENDMETHOD.


  METHOD add_condition_by_sel_filter.
* ...

    CALL METHOD:
      me->add_condition_by_selection
      EXPORTING
        im_it_selection        = im_it_selection
        im_refresh_fieldnames = im_refresh_fieldnames,
      me->build_field_values,
      me->set_filter_on
          EXPORTING
               im_filter_on = 'A'.

  ENDMETHOD.


  METHOD ask_selection_options.
* ...

    DATA :
      changes      TYPE c,
      ilines       TYPE i,
      wa_selection LIKE LINE OF it_selection.

    FIELD-SYMBOLS:
      <wa_fieldname>    LIKE LINE OF im_it_fieldname.

*    READ TABLE im_it_fieldname INDEX 1 TRANSPORTING NO FIELDS.
*    IF sy-subrc = 0.
      LOOP AT im_it_fieldname ASSIGNING <wa_fieldname>.
        READ TABLE it_selection
          WITH KEY fieldname = <wa_fieldname>-fieldname
          TRANSPORTING NO FIELDS.
        IF sy-subrc NE 0.
          wa_selection-fieldname = <wa_fieldname>-fieldname.
          APPEND wa_selection TO it_selection.
        ENDIF.
      ENDLOOP.
      CALL FUNCTION 'Z_DO_TB_TOOLS_01_SELECTION'
        TABLES
          it_selection  = it_selection
          it_field_list = it_valid_fields
        EXCEPTIONS
          OTHERS        = 1.
      changes = 'X'.
      DESCRIBE TABLE it_selection LINES ilines.
      IF ilines = 0.
        CLEAR filter_on.
      ELSE.
        filter_on = 'X'.
      ENDIF.

*    ELSE.
*      CALL FUNCTION 'Z_DO_TB_TOOLS_01_FILTER'
*        EXPORTING
*          im_ask_filter_on = ask_filter_on
*        TABLES
*          it_field_list    = it_valid_fields
*          it_selection     = it_selection
*        CHANGING
*          ch_filter_on     = filter_on
*        EXCEPTIONS
*          cancel           = 1
*          no_changes       = 2
*          OTHERS           = 3.
*
*      IF sy-subrc = 1 OR sy-subrc = 2.
*        RAISE no_changes.
*      ELSE.
*        changes = 'X'.
*      ENDIF.
*
*    ENDIF.

    IF changes = 'X'.
      CALL METHOD me->build_field_values.
    ENDIF.

  ENDMETHOD.


  METHOD assign_value.
* ...

    FIELD-SYMBOLS   :
      <fieldvalue>     TYPE any,
      <wa_fieldvalues> TYPE ZIF_DO_SEO=>ts_field_values.

    READ TABLE it_field_values ASSIGNING <wa_fieldvalues>
      WITH KEY fieldname = im_field_name.
    IF sy-subrc = 0.
      ASSIGN
        <wa_fieldvalues>-fieldvalue->* TO <fieldvalue>.
      <fieldvalue> = im_value.
    ENDIF.

  ENDMETHOD.


  METHOD build_field_values.
    DATA:
      wa_field_values LIKE LINE OF it_field_values,
      prev_field      TYPE fieldname.

    FIELD-SYMBOLS:
      <wa_valid_fields> LIKE LINE OF it_valid_fields,
      <wa_selection>    LIKE LINE OF it_selection.

    REFRESH  it_field_values.
    SORT it_selection BY fieldname.
    LOOP AT it_selection ASSIGNING <wa_selection>.
      IF prev_field NE <wa_selection>-fieldname.
        wa_field_values-fieldname = <wa_selection>-fieldname.
        READ TABLE it_valid_fields ASSIGNING <wa_valid_fields>
          WITH KEY fieldname = <wa_selection>-fieldname.
        IF sy-subrc = 0.
          IF <wa_valid_fields>-rollname IS INITIAL.
            CASE <wa_valid_fields>-inttype.  "See Domain INTTYPE.
              WHEN 'I'.             "I  Integer number (4-byte integer with sign)
                CREATE DATA:
                    wa_field_values-fieldvalue TYPE i.
              WHEN 'g'.             "g  Character string with variable length (ABAP type STRING)
                CREATE DATA:
                   wa_field_values-fieldvalue TYPE string.
              WHEN 'C'.
                CREATE DATA:
                   wa_field_values-fieldvalue TYPE C LENGTH <wa_valid_fields>-OUTPUTLEN.
            ENDCASE.
          ELSE.
            CREATE DATA:
           wa_field_values-fieldvalue TYPE (<wa_valid_fields>-rollname).
          ENDIF.
        ENDIF.
        APPEND wa_field_values TO it_field_values.
        prev_field = <wa_selection>-fieldname.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD class_constructor.
* ...

    CONCATENATE :
       ZCL_DO_SELECTION_OPTIONS=>co_quote
       ZCL_DO_SELECTION_OPTIONS=>co_quote
       INTO c_double_quote,
       ZCL_DO_SELECTION_OPTIONS=>co_quote
       cl_abap_char_utilities=>vertical_tab INTO
       c_quote_char_0.

  ENDMETHOD.


  METHOD clear_value .
* ...

    FIELD-SYMBOLS   :
      <fieldvalue>     TYPE any,
      <wa_fieldvalues> TYPE ZIF_DO_SEO=>ts_field_values.

    IF im_field_name IS INITIAL.
      LOOP AT it_field_values ASSIGNING <wa_fieldvalues>.
        ASSIGN
          <wa_fieldvalues>-fieldvalue->* TO <fieldvalue>.
        CLEAR <fieldvalue>.
      ENDLOOP.
    ELSE.
      LOOP AT it_field_values ASSIGNING <wa_fieldvalues>
        WHERE fieldname = im_field_name.
        ASSIGN
          <wa_fieldvalues>-fieldvalue->* TO <fieldvalue>.
        CLEAR <fieldvalue>.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
* ...

    it_valid_fields[] = im_it_valid_fields[].

*    APPEND LINES OF im_it_valid_fields TO it_valid_fields.

  ENDMETHOD.


  METHOD convert_to_character.
* ...
  ENDMETHOD.


  METHOD delete_condition.
* ...

    IF im_field_name IS INITIAL.
      REFRESH :
        it_selection,
        it_field_values.
    ELSE.
      DELETE it_selection WHERE fieldname = im_field_name.
      DELETE it_field_values WHERE fieldname = im_field_name.
    ENDIF.
  ENDMETHOD.


  METHOD delete_condition_exclude.
* ...

    LOOP AT it_selection ASSIGNING FIELD-SYMBOL(<wa_selection>).
      IF NOT line_exists( im_it_field_name[ fieldname = <wa_selection>-fieldname ] ).
        DELETE it_field_values WHERE fieldname = <wa_selection>-fieldname.
        DELETE it_selection WHERE fieldname = <wa_selection>-fieldname.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_ask_filter_on .
* ...

    ex_ask_filter_on = ask_filter_on.
  ENDMETHOD.


  METHOD get_condition.

    IF im_field_name IS INITIAL.
      re_range = it_selection[].
    ELSE.
      LOOP AT it_selection ASSIGNING FIELD-SYMBOL(<wa_selection>) WHERE fieldname = im_field_name.
        APPEND <wa_selection> TO re_range.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD get_database_where.
* ...

    DATA :
      ilines          TYPE i,
      c_low_value     TYPE Zst_do_WHERE_CLAUSE,
      c_high_value    TYPE Zst_do_WHERE_CLAUSE,
      prev_field      TYPE fieldname,
      prev_sign       TYPE c,
      wa_where_clause TYPE Zst_do_WHERE_CLAUSE.
    FIELD-SYMBOLS :
      <high>         TYPE any,
      <low>          TYPE any,
      <wa_selection> TYPE ZIF_DO_SEO=>ts_selection_options.

    SORT it_selection BY fieldname sign DESCENDING option.

    REFRESH ex_it_where_clause.

    LOOP AT it_selection ASSIGNING <wa_selection>.
      IF prev_field NE <wa_selection>-fieldname OR
         prev_sign  NE <wa_selection>-sign.

        IF NOT prev_field IS INITIAL OR
           NOT prev_sign IS INITIAL.
          wa_where_clause-where_condition = ')'.
          APPEND wa_where_clause TO ex_it_where_clause.
          wa_where_clause-where_condition = 'AND'.
          APPEND wa_where_clause TO ex_it_where_clause.
        ENDIF.

        CASE <wa_selection>-sign.
          WHEN 'I'.
            wa_where_clause-where_condition  = '('.
            APPEND wa_where_clause TO ex_it_where_clause.
          WHEN 'E'.
            wa_where_clause-where_condition  = 'NOT ('.
            APPEND wa_where_clause TO ex_it_where_clause.
          WHEN OTHERS.
            CLEAR : wa_where_clause.
        ENDCASE.

        prev_field = <wa_selection>-fieldname.
        prev_sign =  <wa_selection>-sign.
      ELSE.
        wa_where_clause-where_condition  = 'OR'.
        APPEND wa_where_clause TO ex_it_where_clause.
      ENDIF.
      ASSIGN:
        <wa_selection>-ptrlow->* TO <low>,
        <wa_selection>-ptrhigh->* TO <high>.

      CLEAR:
        c_low_value,
        c_high_value.

      CALL METHOD me->make_where_value
        EXPORTING
          im_fieldname = <wa_selection>-fieldname
          im_value     = <low>
        IMPORTING
          ex_value     = c_low_value.

      CASE  <wa_selection>-option.
        WHEN 'EQ' OR 'NE' OR
             '='  OR '<>' OR
             'GT' OR 'GE' OR
             '>'  OR '>=' OR
             'LT' OR 'LE' OR
             '<'  OR '<=' .

          CONCATENATE
            <wa_selection>-fieldname
            <wa_selection>-option
            c_low_value
            INTO wa_where_clause-where_condition SEPARATED BY space.
          APPEND wa_where_clause TO ex_it_where_clause.
        WHEN 'CP'.
          TRANSLATE c_low_value USING '*%+_'.
*        REPLACE '*' WITH '%' INTO c_low_value.

          CONCATENATE
           <wa_selection>-fieldname
           'LIKE'
            c_low_value
           INTO wa_where_clause-where_condition SEPARATED BY space.

          APPEND wa_where_clause TO ex_it_where_clause.

        WHEN 'NP'.

          TRANSLATE c_low_value USING '*%+_'.

*        REPLACE '*' WITH '%' INTO c_low_value.

          CONCATENATE
           <wa_selection>-fieldname
           'NOT LIKE'
            c_low_value
           INTO wa_where_clause-where_condition SEPARATED BY space.

          APPEND wa_where_clause TO ex_it_where_clause.

        WHEN 'BT'.
          CALL METHOD me->make_where_value
            EXPORTING
              im_fieldname = <wa_selection>-fieldname
              im_value     = <high>
            IMPORTING
              ex_value     = c_high_value.
          CONCATENATE
            <wa_selection>-fieldname
           'BETWEEN'
              c_low_value
            'AND'
            c_high_value
            INTO wa_where_clause-where_condition SEPARATED BY space.
          APPEND wa_where_clause TO ex_it_where_clause.
        WHEN 'NB'.
          CALL METHOD me->make_where_value
            EXPORTING
              im_fieldname = <wa_selection>-fieldname
              im_value     = <high>
            IMPORTING
              ex_value     = c_high_value.
          CONCATENATE
            <wa_selection>-fieldname
            'NOT BETWEEN'
            c_low_value
            'AND'
            c_high_value
            INTO wa_where_clause-where_condition SEPARATED BY space.
          APPEND wa_where_clause TO ex_it_where_clause.
      ENDCASE.
    ENDLOOP.
    DESCRIBE TABLE it_selection LINES ilines.
    IF ilines > 0.
      wa_where_clause-where_condition = ')'.
      APPEND wa_where_clause TO ex_it_where_clause.
    ENDIF.
*        READ TABLE it_valid_fields ASSIGNING <wa_valid_fields>
*          WITH KEY fieldname = <wa_selection>-fieldname
*.
*        IF sy-subrc = 0.
*          CASE <wa_valid_fields>-inttype.
*            WHEN 'C'.
*              CONCATENATE
*                ''''
*                <low>
*                ''''
*                INTO cvalue.
*              CONCATENATE
*                <wa_selection>-fieldname
*                <wa_selection>-option
*                cvalue
*                INTO wa_where_clause-where_condition SEPARATED BY space
    .
*            WHEN OTHERS.
*              CONCATENATE
*                <wa_selection>-fieldname
*                <wa_selection>-option
*                <low>
*                INTO wa_where_clause-where_condition SEPARATED BY space
    .
*          ENDCASE.
*        ELSE.
*          CONCATENATE
*            <wa_selection>-fieldname
*            <wa_selection>-option
*            <low>
*            INTO wa_where_clause-where_condition SEPARATED BY space.
*        ENDIF.

  ENDMETHOD.


  METHOD get_data_type .
* ...
    FIELD-SYMBOLS :
      <wa_valid_fields> LIKE LINE OF it_valid_fields.

    READ TABLE it_valid_fields ASSIGNING <wa_valid_fields>
      WITH KEY fieldname = im_field_name.
    IF sy-subrc = 0.
      IF <wa_valid_fields>-rollname IS INITIAL.
        ex_data_type = <wa_valid_fields>-datatype.
      ELSE.
        ex_data_type = <wa_valid_fields>-rollname.
      ENDIF.
    ELSE.
      RAISE no_field_found.
    ENDIF.

  ENDMETHOD.


  METHOD get_field_value.

    ex_it_field_values[]  = it_field_values[].

  ENDMETHOD.


  METHOD get_filter_on .
* ...

    ex_filter_on = filter_on.
  ENDMETHOD.


  METHOD is_condition .
* ...

*  DATA :
*        wa_selection TYPE ZIF_DO_SEO=>ZSLOT_st_do_selection_options,
*        wa_fieldvalues TYPE ZIF_DO_SEO=>ZSLOT_st_do_field_values.
*  FIELD-SYMBOLS :
*    <high> TYPE ANY,
*    <low> TYPE ANY,
*    <wa_valid_fields> TYPE  dfies.

*  IF im_sign IS INITIAL.
*    im_sign = 'I'.
*  ENDIF.
*  IF im_option IS INITIAL.
*    im_option = 'EQ'.
*  ENDIF.

    READ TABLE it_selection WITH KEY fieldname = im_field_name
            TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ex_exist = 'X'.
    ENDIF.

*  CASE im_sign.
*    WHEN 'I' OR 'E'.
*      READ TABLE it_valid_fields ASSIGNING <wa_valid_fields>
*        WITH KEY fieldname = im_field_name.
*      IF sy-subrc = 0.
*        CREATE DATA:
*          wa_selection-ptrlow  TYPE (<wa_valid_fields>-rollname),
*          wa_selection-ptrhigh TYPE (<wa_valid_fields>-rollname) .
*        ASSIGN:
*          wa_selection-ptrlow->* TO <low>,
*          wa_selection-ptrhigh->* TO <high>.
*        <low> = im_low.
*        <high> = im_high.
*        wa_selection-fieldname = im_field_name.
*        wa_selection-sign = im_sign.
*        wa_selection-option = im_option.
*        CASE wa_selection-option.
*          WHEN 'EQ' OR 'NE' OR
*               '='  OR '<>' OR
*               'GT' OR 'GE' OR
*               '>'  OR '>=' OR
*               'LT' OR 'LE' OR
*               '<'  OR '<=' OR
*               'CP' OR 'NP'.
*            IF NOT <high> IS INITIAL.
*              RAISE high_populated.
*            ENDIF.
*          WHEN 'BT' OR 'NB'.
*            IF <high> IS INITIAL.
*              RAISE high_missing.
*            ENDIF.
*          WHEN OTHERS.
*            RAISE invalid_option.
*        ENDCASE.
*        APPEND wa_selection TO it_selection.
*        READ TABLE it_field_values WITH KEY fieldname = im_field_name
*          TRANSPORTING NO FIELDS.
*        IF sy-subrc NE 0.
*          wa_fieldvalues-fieldname = im_field_name.
*          CREATE DATA:
*            wa_fieldvalues-fieldvalue
*              TYPE (<wa_valid_fields>-rollname).
*          APPEND wa_fieldvalues TO it_field_values.
*        ENDIF.
*      ELSE.
*        RAISE invalid_field_name.
*      ENDIF.
*    WHEN OTHERS.
*      RAISE invalid_sign.
*  ENDCASE.

  ENDMETHOD.


  METHOD is_match.
* ...

*EX_IT_WHERE_CLAUSE
*EX_MATCH
*IT_FIELD_VALUES
*IT_SELECTION
*IT_VALID_FIELDS
*
*ZIF_DO_SEO=>ZSLOT_tt_do_field_values
*ZIF_DO_SEO=>ZSLOT_tt_do_selection_options
*SPAR_DFIES

    FIELD-SYMBOLS   :
      <istrue>         TYPE c,
      <fieldvalue>     TYPE any,
      <high>           TYPE any,
      <low>            TYPE any,
      <wa_selection>   TYPE ZIF_DO_SEO=>ts_selection_options,
      <wa_fieldvalues> TYPE ZIF_DO_SEO=>ts_field_values.

    DATA :
      istrue_e TYPE c,
      istrue_i TYPE c.

    ex_match = 'X'.

*    SORT it_selection BY fieldname.

    LOOP AT it_field_values ASSIGNING <wa_fieldvalues>.
      ASSIGN
        <wa_fieldvalues>-fieldvalue->* TO <fieldvalue>.

      istrue_i = 'F'.
      istrue_e = 'F'.
      LOOP AT it_selection ASSIGNING <wa_selection>
          WHERE fieldname = <wa_fieldvalues>-fieldname.
        ASSIGN :
            <wa_selection>-ptrlow->* TO <low>,
            <wa_selection>-ptrhigh->* TO <high>.

        CASE <wa_selection>-sign.
          WHEN 'I'.
            ASSIGN
              istrue_i TO <istrue>.
          WHEN 'E'.
            ASSIGN
              istrue_e TO <istrue>.
        ENDCASE.

        CASE <wa_selection>-option.
          WHEN 'EQ' OR '='.
            IF  <fieldvalue> = <low>.
              <istrue> = 'T'.
            ENDIF.
          WHEN 'GT' OR '>'.
            IF <fieldvalue> GT <low>.
              <istrue> = 'T'.
            ENDIF.
          WHEN 'GE' OR '>='.
            IF <fieldvalue> GE <low>.
              <istrue> = 'T'.
            ENDIF.
          WHEN 'LT' OR '<'.
            IF <fieldvalue> LT <low>.
              <istrue> = 'T'.
            ENDIF.
          WHEN 'LE' OR '<='.
            IF <fieldvalue> LE <low>.
              <istrue> = 'T'.
            ENDIF.
          WHEN 'NE' OR '<>'.
            IF <fieldvalue> NE <low>.
              <istrue> = 'T'.
            ENDIF.
          WHEN 'CP'.
            IF <fieldvalue> CP <low>.
              <istrue> = 'T'.
            ENDIF.
          WHEN 'NP'.
            IF <fieldvalue> NP <low>.
              <istrue> = 'T'.
            ENDIF.
          WHEN 'BT'.
            IF <fieldvalue> BETWEEN <low> AND <high>.
              <istrue> = 'T'.
            ENDIF.
          WHEN 'NB'.
            IF NOT ( <fieldvalue> BETWEEN <low> AND <high> ).
              <istrue> = 'T'.
            ENDIF.
        ENDCASE.

        CASE  <wa_selection>-sign.
*        WHEN 'I'.
*          IF <istrue> = 'F'.
*            CLEAR ex_match.
*            EXIT.
*          ENDIF.
          WHEN 'E'.
            IF <istrue> = 'T'.
              EXIT.
            ENDIF.
        ENDCASE.
      ENDLOOP.

      CASE <wa_selection>-sign.
        WHEN 'I'.
          IF istrue_i = 'F'.
            CLEAR ex_match.
            EXIT.
          ENDIF.
        WHEN 'E'.
          IF istrue_e = 'T'.
            CLEAR ex_match.
            EXIT.
          ENDIF.
      ENDCASE.

*    IF istrue_i = 'F' OR istrue_e = 'T'.
*      CLEAR ex_match.
*      EXIT.
*    ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD make_where_value.
* ...

*  DATA :
*  cdouble_quote(2) TYPE c,
*  char0 TYPE x VALUE '00',
*  cquote TYPE c VALUE '''',
*  quote_to_char_0(2) TYPE c .

    FIELD-SYMBOLS:
          <wa_valid_fields>  LIKE LINE OF it_valid_fields.

    READ TABLE it_valid_fields ASSIGNING <wa_valid_fields>
      WITH KEY fieldname = im_fieldname.

    IF sy-subrc = 0.
      ex_value = im_value.

      CASE <wa_valid_fields>-inttype.
        WHEN 'C' OR 'D'.
          IF ex_value CS ZCL_DO_SELECTION_OPTIONS=>co_quote.
            TRANSLATE ex_value USING me->c_quote_char_0.
            WHILE ex_value CS cl_abap_char_utilities=>vertical_tab.
              REPLACE cl_abap_char_utilities=>vertical_tab
                WITH me->c_double_quote INTO ex_value.
*            REPLACE ZCL_DO_SELECTION_OPTIONS=>co_char_0
*              WITH me->c_double_quote INTO ex_value.
            ENDWHILE.
          ENDIF.
          CONCATENATE
            ''''
            ex_value
            ''''
            INTO ex_value.
        WHEN 'P'.
          IF im_value IS INITIAL.
            ex_value = '0'.
          ELSE.
            ex_value = ex_value.
          ENDIF.
        WHEN OTHERS.
          ex_value = ex_value.
      ENDCASE.
    ELSE.
      ex_value = ex_value.
    ENDIF.
  ENDMETHOD.


  METHOD rename_condition .
* ...

    FIELD-SYMBOLS:
      <wa_selection>    LIKE LINE OF it_selection,
      <wa_field_values> LIKE LINE OF it_field_values.

    LOOP AT it_selection ASSIGNING <wa_selection>
      WHERE fieldname = im_field_name_from.
      <wa_selection>-fieldname = im_field_name_to.
    ENDLOOP.

    LOOP AT it_field_values ASSIGNING <wa_field_values>
      WHERE fieldname = im_field_name_from.
      <wa_field_values>-fieldname = im_field_name_to.
    ENDLOOP.

*  IF not im_field_name_from IS INITIAL.
*    REFRESH :
*      it_selection,
*      it_field_values.
*  ELSE.
*    DELETE it_selection WHERE fieldname = im_field_name.
*    DELETE it_field_values WHERE fieldname = im_field_name.
*  ENDIF.
  ENDMETHOD.


  METHOD set_ask_filter_on.
* ...

    ask_filter_on = im_ask_filter_on.
  ENDMETHOD.


  METHOD set_filter_on.
* ...
    CASE im_filter_on.
      WHEN 'A'.
        READ TABLE it_selection INDEX 1
              TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          filter_on = 'X'.
        ELSE.
          filter_on = space.
        ENDIF.

      WHEN OTHERS.
        filter_on = im_filter_on.
    ENDCASE.
  ENDMETHOD.


  METHOD set_valid_fields .
* ...

    REFRESH it_valid_fields.

    APPEND LINES OF im_it_valid_fields TO it_valid_fields.

  ENDMETHOD.


  METHOD set_valid_fields_descriptions .
* ...


    LOOP AT it_valid_fields ASSIGNING FIELD-SYMBOL(<wa_valid_fields>).
      READ TABLE im_it_valid_fields ASSIGNING FIELD-SYMBOL(<wa_im_valid_fields>)
           WITH KEY fieldname = <wa_valid_fields>-fieldname.
      IF sy-subrc = 0.
        <wa_valid_fields>-scrtext_s = <wa_im_valid_fields>-scrtext_s.
        <wa_valid_fields>-scrtext_m = <wa_im_valid_fields>-scrtext_m.
        <wa_valid_fields>-scrtext_l = <wa_im_valid_fields>-scrtext_l.
        <wa_valid_fields>-sign = <wa_im_valid_fields>-sign.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
