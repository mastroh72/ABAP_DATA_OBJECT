FUNCTION Z_DO_TB_TOOLS_01_RANGE_MAP_SO.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(FIELDNAME) TYPE  FIELDNAME
*"     REFERENCE(DATA_TYPE) TYPE  ROLLNAME
*"     REFERENCE(RESET_SELECTION) TYPE  CHAR1 DEFAULT 'X'
*"  TABLES
*"      IT_RANGE TYPE  STANDARD TABLE
*"      IT_SELECTION_OPTIONS TYPE  ZDO01_TT_SELECTION_OPTIONS
*"  EXCEPTIONS
*"      NO_RANGE_TAB
*"----------------------------------------------------------------------
*  DELETE it_selection_options

  DATA:
    wa_selection_options LIKE LINE OF it_selection_options.

  DATA l_type LIKE rsscr-type.
  DATA l_length TYPE i.
  DATA l_decimals TYPE i.
  DATA l_convert LIKE rsconvert.
  DATA l_emask(7).

  FIELD-SYMBOLS:
    <value_low>,
    <value_high>,
    <l_sign>,
    <l_option>,
    <l_low>,
    <l_high>,
    <l_low_e>,
    <l_high_e>.

  ASSIGN COMPONENT 1 OF STRUCTURE it_range TO <l_sign>.
  IF sy-subrc NE 0.
    RAISE no_range_tab.
  ENDIF.
  DESCRIBE FIELD <l_sign> TYPE l_type LENGTH l_length IN CHARACTER MODE.
  IF l_type NE 'C' OR l_length NE 1.
    RAISE no_range_tab.
  ENDIF.

  ASSIGN COMPONENT 2 OF STRUCTURE it_range TO <l_option>.
  IF sy-subrc NE 0.
    RAISE no_range_tab.
  ENDIF.
  DESCRIBE FIELD <l_option> TYPE l_type LENGTH l_length IN CHARACTER MODE.
  IF l_type NE 'C' OR l_length NE 2.
    RAISE no_range_tab.
  ENDIF.

  ASSIGN COMPONENT 3 OF STRUCTURE it_range TO <l_low>.
  IF sy-subrc NE 0.
    RAISE no_range_tab.
  ENDIF.
  DESCRIBE FIELD <l_low>
                TYPE l_convert-type LENGTH l_convert-length IN CHARACTER MODE
                DECIMALS l_convert-decimals
                OUTPUT-LENGTH l_convert-olength
                EDIT MASK l_emask.

  ASSIGN COMPONENT 4 OF STRUCTURE it_range TO <l_high>.
  IF sy-subrc NE 0.
    RAISE no_range_tab.
  ENDIF.
  DESCRIBE FIELD <l_high>
               TYPE l_type LENGTH l_length IN CHARACTER MODE DECIMALS l_decimals.
  IF l_convert-type NE l_type OR l_convert-length NE l_length OR
     l_convert-decimals NE l_decimals.
    RAISE no_range_tab.
  ENDIF.

  IF reset_selection = 'X'.
    REFRESH it_selection_options.
  ENDIF.
*  FIELD-SYMBOLS:
*    <wa_range> LIKE LINE OF it_range.

*  create
  wa_selection_options-fieldname = fieldname.

  LOOP AT it_range.
    CREATE DATA:
      wa_selection_options-ptrlow  TYPE (data_type),
      wa_selection_options-ptrhigh TYPE (data_type).
    ASSIGN :
      wa_selection_options-ptrhigh->* TO <value_high>,
      wa_selection_options-ptrlow->* TO <value_low>.
    wa_selection_options-sign = <l_sign>.
    wa_selection_options-option = <l_option>.
    <value_high> = <l_high>.
    <value_low> = <l_low>.
    APPEND wa_selection_options TO it_selection_options.
  ENDLOOP.

  READ TABLE it_range INDEX 1.




*         SIGN(1),
*         OPTION(2),
*         LOW  LIKE f,
*         HIGH LIKE f,
*  begin of zslot_st_do_selection_options,
*    fieldname type fieldname,
*    sign(1) type c,
*    option(2) type c,
*    ptrlow  type ref to data,
*    ptrhigh type ref to data,
*  end of zslot_st_do_selection_options,





ENDFUNCTION.
