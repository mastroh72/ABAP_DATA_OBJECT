FUNCTION Z_DO_TB_TOOLS_01_FIND.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      FINDIN TYPE  TABLE
*"      FIELDS TYPE  ZTT_DO_TB_FIELD_TO_SEARCH
*"  CHANGING
*"     REFERENCE(SEARCHPARMS) TYPE  ZST_DO_SEARCHPARMS
*"--------------------------------------------------------------------
* logic compied from function group wbmessages


  ASSIGN :
      <g_searchparms>-markfield TO <g_markfield>,
      fields[] TO <git_fields>,
      findin[] TO <git_lookin>,
      searchparms TO <g_searchparms>,
      <g_searchparms>-zcursor   TO <g_cursor>.


  IF <g_searchparms>-answer = 'X'.
    length = strlen( <g_searchparms>-newtxt ).
    ASSIGN <g_searchparms>-newtxt(length) TO <g_pos>.
    PERFORM  find_set_up.
    CASE <g_searchparms>-selectedmode.
      WHEN 'FIND' OR 'REPL' OR 'REPLALL'.
        IF <g_searchparms>-start_at_top = 'X'.
          <g_searchparms>-top_line = 1.   "start at top of data browser
        ENDIF.
    ENDCASE.
    CASE <g_searchparms>-selectedmode.
      WHEN 'FIND+'.
        PERFORM msg_src_next_local
          USING <g_searchparms>-top_line.
      WHEN 'FIND'.
        PERFORM msg_src_repl_local
          USING <g_searchparms>-top_line.
      WHEN 'REPL' OR 'REPL+'.
        PERFORM replace_local
          USING <g_searchparms>-top_line.
      WHEN 'REPLALL' OR 'REPLALL+'.
        PERFORM replace_all_local
          USING <g_searchparms>-top_line.
    ENDCASE.
    IF <g_searchparms>-setcursor = 'X'.
      CONCATENATE
        <g_searchparms>-tablename
        '-'
        <g_cursor>-field
        INTO <g_cursor>-field.
    ENDIF.
  ENDIF.








ENDFUNCTION.
