FUNCTION Z_DO_TB_TOOLS_01_RANGE_WA.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(WA_RANGE) TYPE  ANY
*"  TABLES
*"      IT_RANGE
*"--------------------------------------------------------------------
 DATA :
    ilines       TYPE i.

  FIELD-SYMBOLS:
    <wa_low>,
    <wa_high>,
    <wa_option>,
    <wa_sign>.

  ASSIGN :
    COMPONENT 1 OF STRUCTURE wa_range TO <wa_sign>,
    COMPONENT 2 OF STRUCTURE wa_range TO <wa_option>,
    COMPONENT 3 OF STRUCTURE wa_range TO <wa_low>,
    COMPONENT 4 OF STRUCTURE wa_range TO <wa_high>.

  DESCRIBE TABLE it_range LINES ilines.

  CASE ilines.
    WHEN 0.
      IF <wa_high> IS INITIAL.
        IF NOT <wa_low> IS INITIAL.
          IF <wa_low> CS '*'.
            <wa_option> = 'CP'.
          ELSEIF <wa_low> CS '+'.
            <wa_option> = 'CP'.
          ELSE.
            <wa_option> = 'EQ'.
          ENDIF.
          <wa_sign> = 'I'.
          APPEND wa_range TO it_range.
        ENDIF.
      ELSE.
        <wa_sign> = 'I'.
        <wa_option> = 'BT'.
        APPEND wa_range TO it_range.
      ENDIF.
    WHEN OTHERS.
      IF <wa_high> IS INITIAL.
        IF <wa_low> IS INITIAL.
          DELETE it_range INDEX 1.
        ELSE.
          IF <wa_low> CS '*'.
            <wa_option> = 'CP'.
          ELSEIF <wa_low> CS '+'.
            <wa_option> = 'CP'.
          ELSE.
            <wa_option> = 'EQ'.
          ENDIF.
          <wa_sign> = 'I'.
          MODIFY it_range FROM wa_range INDEX 1.
        ENDIF.
      ELSE.
        <wa_sign> = 'I'.
        <wa_option> = 'BT'.
        MODIFY it_range FROM wa_range INDEX 1.
      ENDIF.
  ENDCASE.




ENDFUNCTION.
