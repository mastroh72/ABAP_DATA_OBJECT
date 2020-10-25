CLASS zcl_do_tabstrip DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ts_tab_strip,
        o_tabstrip   TYPE REF TO cxtab_tabstrip,
        subscreen    LIKE sy-dynnr,
        prog         LIKE sy-repid,
        pressed_tab  LIKE sy-ucomm,
        t_tabs_index LIKE sy-tabix,
      END OF ts_tab_strip,
      BEGIN OF ts_tab,
        function_code LIKE sy-ucomm,
        subscreen     LIKE sy-dynnr,
      END OF ts_tab,
      tt_tabs TYPE STANDARD TABLE OF ts_tab WITH KEY function_code.
    METHODS:
      constructor
        IMPORTING
          is_tab_strip TYPE ts_tab_strip
          it_tabs      TYPE tt_tabs,
      get_screen_for_pressed_tab,
      process_function_code
        IMPORTING
          iv_ucomm LIKE sy-ucomm ,
      get_program
        RETURNING VALUE(rv_program) LIKE sy-repid,
      get_screen
        RETURNING VALUE(rv_subscreen) LIKE sy-dynnr.

    DATA:
            s_tab_strip TYPE ts_tab_strip READ-ONLY.


  PROTECTED SECTION.
    DATA:
      t_tabs      TYPE tt_tabs.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_do_tabstrip IMPLEMENTATION.
  METHOD constructor.
    me->s_tab_strip = is_tab_strip.
    me->t_tabs = it_tabs.


    IF  me->s_tab_strip-t_tabs_index = 0.
      IF s_tab_strip-pressed_tab IS INITIAL.
        me->s_tab_strip-t_tabs_index = 1.
      ELSE.
        process_function_code( s_tab_strip-pressed_tab ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_screen_for_pressed_tab.
    FIELD-SYMBOLS:
       <lc_tab_strip> TYPE cxtab_tabstrip.

    ASSIGN
      me->s_tab_strip-o_tabstrip->* TO <lc_tab_strip>.
    <lc_tab_strip>-activetab  =  me->s_tab_strip-pressed_tab.
    READ TABLE me->t_tabs  INDEX me->s_tab_strip-t_tabs_index
        ASSIGNING FIELD-SYMBOL(<ls_tab>).
    IF sy-subrc = 0.
      <lc_tab_strip>-activetab = me->s_tab_strip-pressed_tab.
      me->s_tab_strip-subscreen = <ls_tab>-subscreen.
    ENDIF.
  ENDMETHOD.

  METHOD process_function_code.
    READ TABLE me->t_tabs ASSIGNING FIELD-SYMBOL(<ls_tab>)
       WITH KEY function_code = iv_ucomm.
    IF sy-subrc = 0.
      me->s_tab_strip-pressed_tab = <ls_tab>-function_code.
      me->s_tab_strip-t_tabs_index = sy-tabix.
    ENDIF.
  ENDMETHOD.

  METHOD get_program.
    rv_program = s_tab_strip-prog.
  ENDMETHOD.

  METHOD get_screen.
    rv_subscreen = s_tab_strip-subscreen.
  ENDMETHOD.

ENDCLASS.
