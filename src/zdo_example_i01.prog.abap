*----------------------------------------------------------------------*
***INCLUDE ZDO_EXAMPLE_I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

MODULE user_command_0100 INPUT.


  "selection screen check for Enter F8.
  CASE gok_code.
    WHEN 'ENTER'.
      CALL METHOD gobj_execute->apply_selection
        EXPORTING
          im_ucomm = 'BLUE'
*         im_layout    = p_layout
*         im_maxhits   = pmaxhits
*         im_go_to_alv = pgoalv
        IMPORTING
          ex_dynnr = gc_1000_tc.

      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'CANCEL'.
      SET SCREEN 0.
      LEAVE SCREEN.
  ENDCASE.


ENDMODULE.


MODULE user_command_1000 INPUT.
  CASE gok_code.
    WHEN 'SELCHG'.
      CLEAR gok_code.
      CALL SCREEN 0100
        STARTING AT c_gts_0100-start_column  c_gts_0100-start_line
        ENDING   AT c_gts_0100-end_column    c_gts_0100-end_line.
      CLEAR gok_code.
  ENDCASE.
ENDMODULE.


MODULE moveokcode INPUT.
  CLEAR gok_code.
  gok_code  = ok_code.
  CLEAR ok_code.
ENDMODULE.


MODULE user_function_e INPUT.
  PERFORM back_exit_cancel_input.
ENDMODULE.

*&SPWIZARD: INPUT MODUL FOR TC 'GTC_T001W'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE gtc_t001w_mark INPUT.
*  DATA: g_GTC_T001W_wa2 like line of GIT_T001W.
*    if GTC_T001W-line_sel_mode = 1
*    and GIT_T001W-ROWMARKED = 'X'.
*     loop at GIT_T001W into g_GTC_T001W_wa2
*       where ROWMARKED = 'X'.
*       g_GTC_T001W_wa2-ROWMARKED = ''.
*       modify GIT_T001W
*         from g_GTC_T001W_wa2
*         transporting ROWMARKED.
*     endloop.
*  endif.
  MODIFY git_t001w
    INDEX gtc_t001w-current_line.
*    TRANSPORTING ROWMARKED.


*   TRANSPORTING ROWMARKED.
  CALL METHOD gobj_t001w->it_update
    EXPORTING
      im_db_cnt = git_t001w-rowdbcnt.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GTC_T001W_CHANGE_TC_ATTR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE gtc_t001w_change_tc_attr INPUT.
  CALL METHOD :
    gobj_t001w->it_function_code
      CHANGING
        ch_function_code = gok_code,
   gobj_t001w->it_cursor
      CHANGING
        ch_set_position = gwa_set_pos-set_position
        ch_field = gwa_set_pos-field
        ch_offset = gwa_set_pos-fieldoffset
        ch_line = gwa_set_pos-line.
ENDMODULE.

*&SPWIZARD: INPUT MODUL FOR TC 'GTC_T320'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE gtc_t320_mark INPUT.
*  DATA: g_gtc_t320_wa2 LIKE LINE OF git_t320.
*  IF gtc_t320-line_sel_mode = 1
*  AND git_t320-rowmarked = 'X'.
*    LOOP AT git_t320 INTO g_gtc_t320_wa2
*      WHERE rowmarked = 'X'.
*      g_gtc_t320_wa2-rowmarked = ''.
*      MODIFY git_t320
*        FROM g_gtc_t320_wa2
*        TRANSPORTING rowmarked.
*    ENDLOOP.
*  ENDIF.
*  MODIFY git_t320
*    INDEX gtc_t320-current_line
*    TRANSPORTING rowmarked.

  MODIFY git_t320
    INDEX gtc_t320-current_line.
*    TRANSPORTING ROWMARKED.


*   TRANSPORTING ROWMARKED.
  CALL METHOD gobj_t320->it_update
    EXPORTING
      im_db_cnt = git_t320-rowdbcnt.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GTC_T320_CHANGE_TC_ATTR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE gtc_t320_change_tc_attr INPUT.
  CALL METHOD :
    gobj_t320->it_function_code
      CHANGING
        ch_function_code = gok_code,
   gobj_t320->it_cursor
      CHANGING
        ch_set_position = gwa_set_pos-set_position
        ch_field = gwa_set_pos-field
        ch_offset = gwa_set_pos-fieldoffset
        ch_line = gwa_set_pos-line.
ENDMODULE.
