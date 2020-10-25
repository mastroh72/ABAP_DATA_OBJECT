*----------------------------------------------------------------------*
***INCLUDE LZTB_TOOLS_01O01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  D240_PFSTATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE d240_pfstatus OUTPUT.
  SET PF-STATUS 'DPOP'.
  SET TITLEBAR 'SRC'.
  <g_searchparms>-start_at_top = 'X'.  "turn on with popup
*  CLEAR USE_UPPER_LOWER.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0280  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0280 OUTPUT.
  SET PF-STATUS 'SORT'.
  SET TITLEBAR 'SORT'.

ENDMODULE.                 " STATUS_0280  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GTC_COLS_LIST_CHANGE_TC_ATTR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE gtc_cols_list_change_tc_attr OUTPUT.
  CALL METHOD gobj_cols_list->tc_change_attribute.
ENDMODULE.                 " GTC_COLS_LIST_CHANGE_TC_ATTR  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GTC_SORT_COLS_CHANGE_TC_ATTR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE gtc_sort_cols_change_tc_attr OUTPUT.
  DESCRIBE TABLE git_sort_cols LINES gilines.
  IF gilines = 0.
    CALL METHOD gobj_sort_cols->tc_set_displayed_lines
      EXPORTING
        im_lines = 1.
  ENDIF.
  CALL METHOD gobj_sort_cols->tc_change_attribute.
ENDMODULE.                 " GTC_SORT_COLS_CHANGE_TC_ATTR  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GTC_COLS_LIST_CHANGE_FIELD_ATT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE gtc_cols_list_pbo OUTPUT.
  CALL METHOD gobj_cols_list->tc_set_displayed_lines
    EXPORTING
      im_lines = sy-loopc.
ENDMODULE.                 " GTC_COLS_LIST_CHANGE_FIELD_ATT  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GTC_SORT_COLS_CHANGE_FIELD_ATT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE gtc_sort_cols_pbo OUTPUT.
  CALL METHOD gobj_sort_cols->tc_set_displayed_lines
    EXPORTING
      im_lines = sy-loopc.
ENDMODULE.                 " GTC_SORT_COLS_CHANGE_FIELD_ATT  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0220  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0220 OUTPUT.
  SET PF-STATUS 'FILTER'.
  SET TITLEBAR 'FILTER'.

*  GET CURSOR FIELD  g_field  LINE g_line.

  IF g_field = 'GIT_FILTER-PB'.
    SET CURSOR FIELD  'GIT_FILTER-CONTENT'
               LINE g_line.
  ENDIF.
  LOOP AT SCREEN.
    IF screen-name = 'G_FILTER_ON'.
      IF g_ask_filter_on = 'X'.
        screen-invisible = 0.
      ELSE.
        screen-invisible = 1.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.                 " STATUS_0220  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GTC_FILTER_CHANGE_FIELD_ATTR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE gtc_filter_change_field_attr OUTPUT.
  CALL METHOD gobj_filter->tc_set_displayed_lines
    EXPORTING
      im_lines = sy-loopc.
ENDMODULE.                 " GTC_FILTER_CHANGE_FIELD_ATTR  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GTC_FILTER_CHANGE_TC_ATTR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE gtc_filter_change_tc_attr OUTPUT.
  DESCRIBE TABLE git_filter LINES gilines.
  IF gilines = 0.
    CALL METHOD gobj_filter->tc_set_displayed_lines
      EXPORTING
        im_lines = 1.
  ENDIF.
  CALL METHOD gobj_filter->tc_change_attribute.
ENDMODULE.                 " GTC_FILTER_CHANGE_TC_ATTR  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0501  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0501 OUTPUT.
  SET PF-STATUS 'FILTER'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0501  OUTPUT
