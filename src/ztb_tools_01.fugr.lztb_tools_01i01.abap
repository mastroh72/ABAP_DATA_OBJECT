*----------------------------------------------------------------------*
***INCLUDE LZTB_TOOLS_01I01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  D240_GET_ANSWER  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE d240_get_answer INPUT.
  DATA tempfcode LIKE sy-ucomm.

  <g_searchparms>-newtxt = <g_searchparms>-searchtxt.
  CASE fcode.
    WHEN 'SEARCH_LOCAL'.
      CLEAR: fcode, <g_searchparms>-repltxt.
      IF <g_searchparms>-newtxt = space.
        <g_searchparms>-answer = space.
      ELSE.
        <g_searchparms>-answer = 'X'.
        <g_searchparms>-selectedmode = 'FIND'.
*      length = strlen( <g_searchparms>-newtxt ).
*      ASSIGN <g_searchparms>-newtxt(length) TO <pos>.
      ENDIF.
    WHEN 'REPLACE_LOCAL' OR 'REPLACE_ALL_LOCAL'.
      tempfcode = fcode.
      CLEAR: fcode, sy-ucomm.
      IF <g_mode> = 'D'.
        MESSAGE s303(eu).
        IF <g_searchparms>-searchtxt = space.
          SET SCREEN 0.
          LEAVE SCREEN.
        ENDIF.
      ELSEIF <g_searchparms>-searchtxt = space OR
             <g_searchparms>-repltxt = space.
        MESSAGE i020(ed).
        EXIT.
      ENDIF.
      <g_searchparms>-answer = 'X'.
      CASE tempfcode.
        WHEN 'REPLACE_LOCAL'.
          <g_searchparms>-selectedmode = 'REPL'.
        WHEN 'REPLACE_ALL_LOCAL'.
          <g_searchparms>-selectedmode = 'REPLALL'.
      ENDCASE.
      <g_searchparms>-rsearch = 'R'.
*    length = strlen( <g_searchparms>-searchtxt ).
*    ASSIGN <g_searchparms>-searchtxt(length) TO <pos>.
      IF <g_searchparms>-searchtxt CP '*# '.
      ENDIF.
      <g_searchparms>-searchlength = sy-fdpos.
      IF <g_searchparms>-repltxt CP '*# '.
      ENDIF.
      <g_searchparms>-repllength = sy-fdpos.
    WHEN OTHERS.
      EXIT.
  ENDCASE.
  SET SCREEN 0.
  LEAVE SCREEN.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  DYNPEXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE dynpexit INPUT.
  CLEAR: <g_searchparms>-answer,
         <g_searchparms>-conflag.
  CASE fcode.
    WHEN 'EESC'.
      SET SCREEN 0.
      LEAVE SCREEN.
  ENDCASE.
ENDMODULE.

* INPUT MODULE FOR TABLECONTROL 'GTC_SORT_COLS': MODIFY TABLE
MODULE gtc_sort_cols_modify INPUT.
  MODIFY git_sort_cols
    INDEX gtc_sort_cols-current_line.
  CALL METHOD gobj_sort_cols->it_update
    EXPORTING
      im_db_cnt = git_sort_cols-rowdbcnt.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  gtc_cols_list_modify  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE gtc_cols_list_modify INPUT.
  MODIFY git_cols_list
    INDEX gtc_cols_list-current_line.
  CALL METHOD gobj_cols_list->it_update
    EXPORTING
      im_db_cnt = git_cols_list-rowdbcnt.

ENDMODULE.                 " gtc_cols_list_modify  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0280  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0280 INPUT.
  CALL METHOD :
     gobj_sort_cols->it_function_code
       CHANGING
         ch_function_code = fcode,
     gobj_cols_list->it_function_code
       CHANGING
         ch_function_code = fcode.

*        gobj_sort_cols->it_cursor
*      CHANGING
*        ch_set_position = set_position
*        ch_field = field
*        ch_offset = fieldoffset
*        ch_line = line.
*  IF set_position = 'X'.
*    CALL METHOD g2400->set_cursor
*      EXPORTING
*        im_field  = field
*        im_offset = fieldoffset
*        im_line   = line.
*  ENDIF.


  CASE fcode.
    WHEN 'TECH'.
      CALL METHOD :
        gobj_sort_cols->toggle_content,
        gobj_cols_list->toggle_content.
      CLEAR fcode.
    WHEN 'CANCEL'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'ENTER'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN OTHERS.
      CLEAR fcode.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0280  INPUT

* INPUT MODULE FOR TABLECONTROL 'GTC_FILTER': MODIFY TABLE
MODULE gtc_filter_modify INPUT.
  MODIFY git_filter
    FROM git_filter
    INDEX gtc_filter-current_line.
  CALL METHOD gobj_filter->it_update
    EXPORTING
      im_db_cnt = git_filter-rowdbcnt.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0220  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0220 INPUT.

*  CALL METHOD :
*     gobj_sort_cols->it_function_code
*       CHANGING
*         ch_function_code = fcode,
*     gobj_cols_list->it_function_code
*       CHANGING
*         ch_function_code = fcode.
*
*        gobj_sort_cols->it_cursor
*      CHANGING
*        ch_set_position = set_position
*        ch_field = field
*        ch_offset = fieldoffset
*        ch_line = line.
*  IF set_position = 'X'.
*    CALL METHOD g2400->set_cursor
*      EXPORTING
*        im_field  = field
*        im_offset = fieldoffset
*        im_line   = line.
*  ENDIF.


  CASE gfcode.
    WHEN 'FLTR_ASK'.
      CALL METHOD gobj_filter->it_selection_dialog.
    WHEN 'TECH'.
      CALL METHOD gobj_filter->toggle_content.
    WHEN 'CANCEL'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'ENTER'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN OTHERS.
      CLEAR gfcode.
      CLEAR sy-ucomm.
      CLEAR fcode.
  ENDCASE.
  GET CURSOR FIELD  g_field  LINE g_line.

ENDMODULE.                 " USER_COMMAND_0220  INPUT
*&---------------------------------------------------------------------*
*&      Module  GTC_FILTER_CHANGE_TC_ATTR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE gtc_filter_change_tc_attr INPUT.
  CASE gfcode.
    WHEN 'FIND' OR 'FIND+'
      OR 'COL_U' OR 'COL_D'.

      CONCATENATE
        'GTC_FILTER_'
        gfcode
        INTO gfcode.
  ENDCASE.

  CALL METHOD gobj_filter->it_function_code
    CHANGING
      ch_function_code = gfcode.
ENDMODULE.                 " GTC_FILTER_CHANGE_TC_ATTR  INPUT
*&---------------------------------------------------------------------*
*&      Module  move_fcode  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE move_fcode INPUT.
  gfcode = fcode.
  CLEAR fcode.
ENDMODULE.                 " move_fcode  INPUT
