FUNCTION ztb_tools_01_sort.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      IT_SORT_COLUMNS TYPE  ZTT_DO_SORT_COLUMNS
*"      IT_FIELD_LIST TYPE  ZTT_DO_FIELD_LIST
*"  EXCEPTIONS
*"      CANCEL
*"      NO_CHANGES
*"----------------------------------------------------------------------
  DATA:
    no_change TYPE c.

  REFRESH :
    git_sort_cols,
    git_cols_list.

  LOOP AT it_sort_columns.
    CLEAR git_sort_cols.
    CASE it_sort_columns-direction.
      WHEN 'a' OR 'A'.
        git_sort_cols-sort_up = 'X'.
      WHEN OTHERS.
        git_sort_cols-sort_down = 'X'.
    ENDCASE.

    git_sort_cols-fieldlabel = it_sort_columns-fieldname.
    git_sort_cols-fieldname  = it_sort_columns-fieldname.
    git_sort_cols-rowseq     = it_sort_columns-sortorder.

    READ TABLE it_field_list
      WITH KEY fieldname = it_sort_columns-fieldname.
    IF sy-subrc = 0.
      git_sort_cols-fieldlabel = it_field_list-fieldlabel.
    ENDIF.
    APPEND git_sort_cols TO git_sort_cols.
  ENDLOOP.

  SORT git_sort_cols BY rowseq.

  LOOP AT it_field_list.
    READ TABLE it_sort_columns
      WITH KEY fieldname = it_field_list-fieldname.
    IF sy-subrc NE 0.
      git_cols_list-fieldlabel = it_field_list-fieldlabel.
      git_cols_list-fieldname  = it_field_list-fieldname.
      APPEND git_cols_list TO git_cols_list[].
    ENDIF.
  ENDLOOP.

  CALL METHOD :
    gobj_cols_list->it_force_integrity,
    gobj_cols_list->tc_set_displayed_lines
      EXPORTING
        im_lines = 1,
    gobj_sort_cols->it_force_integrity,
    gobj_sort_cols->tc_set_displayed_lines
      EXPORTING
        im_lines = 1,
    gobj_sort_cols->set_content
      EXPORTING
        im_option = cl_sort=>co_field_label,
    gobj_cols_list->set_content
      EXPORTING
        im_option = cl_sort=>co_field_label.


  CLEAR fcode.
  CALL SCREEN  280 STARTING AT 10 05
                   ENDING   AT 95 22.


  IF fcode = 'ENTER'.
    no_change = 'X'.
    LOOP AT git_sort_cols.
      READ TABLE it_sort_columns
        WITH KEY sortorder = git_sort_cols-rowseq.
      IF sy-subrc = 0.
        IF it_sort_columns-fieldname NE git_sort_cols-fieldname.
          CLEAR no_change.
          EXIT.
        ENDIF.
        IF it_sort_columns-sortorder NE git_sort_cols-rowseq.
          CLEAR no_change.
          EXIT.
        ENDIF.
        IF NOT ( git_sort_cols-sort_up = 'X' AND
                ( it_sort_columns-direction = 'A' OR
                  it_sort_columns-direction = 'a' ) ) .
          CLEAR no_change.
          EXIT.
        ENDIF.
      ELSE.
        CLEAR no_change.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF no_change = 'X'.
      RAISE no_changes.
    ELSE.
      REFRESH it_sort_columns.
      LOOP AT git_sort_cols.
        it_sort_columns-fieldname = git_sort_cols-fieldname.
        it_sort_columns-sortorder = git_sort_cols-rowseq.
        IF git_sort_cols-sort_up = 'X'.
          it_sort_columns-direction = 'A'.
        ELSE.
          it_sort_columns-direction = 'D'.
        ENDIF.
        APPEND it_sort_columns.
      ENDLOOP.
    ENDIF.
  ELSEIF fcode = 'CANCEL'.
    RAISE cancel.
  ENDIF.
ENDFUNCTION.
