*----------------------------------------------------------------------*
***INCLUDE ZDO_EXAMPLE_O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_1000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'GS_0100'.
  SET TITLEBAR 'GT_0100'.
ENDMODULE.

MODULE status_1000 OUTPUT.
  SET PF-STATUS 'ZDO_EXAMPLE_S001'.
*  SET TITLEBAR 'xxx'.
ENDMODULE.


*&SPWIZARD: OUTPUT MODULE FOR TC 'GTC_T001W'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE gtc_t001w_change_tc_attr OUTPUT.
*  DESCRIBE TABLE GIT_T001W LINES GTC_T001W-lines.
  CALL METHOD gobj_t001w->tc_change_attribute.
ENDMODULE.


MODULE set_cursor OUTPUT.

  IF GWA_SET_POS-set_position = 'X'.
    SET CURSOR FIELD  GWA_SET_POS-field
               OFFSET GWA_SET_POS-fieldoffset
               LINE   GWA_SET_POS-line.
  ENDIF.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GTC_T001W_CHANGE_FIELD_ATTR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE gtc_t001w_change_field_attr OUTPUT.
  CALL METHOD gobj_t001w->tc_set_displayed_lines
    EXPORTING
      im_lines  = sy-loopc
      im_db_cnt = git_t001w-rowdbcnt.

ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'GTC_T320'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE gtc_t320_change_tc_attr OUTPUT.
* DESCRIBE TABLE GIT_T320 LINES GTC_T320-lines.
  CALL METHOD gobj_t320->tc_change_attribute.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GTC_T320_CHANGE_FIELD_ATTR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE gtc_t320_change_field_attr OUTPUT.
  CALL METHOD gobj_t320->tc_set_displayed_lines
    EXPORTING
      im_lines  = sy-loopc
      im_db_cnt = git_t320-rowdbcnt.
ENDMODULE.
