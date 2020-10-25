FUNCTION ZTB_TOOLS_01_FIND_ASK.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(METHOD) TYPE  SYUCOMM DEFAULT 'REPL'
*"     REFERENCE(MODE) TYPE  CHAR1 DEFAULT 'U'
*"  CHANGING
*"     REFERENCE(SEARCHPARMS) TYPE  ZST_DO_SEARCHPARMS
*"----------------------------------------------------------------------
  ASSIGN :
    mode TO <g_mode>,
    searchparms TO <g_searchparms>.

  <g_searchparms>-method = method.

  CASE method.
    WHEN 'REPLACE' OR 'REPL'.
      CASE mode.
        WHEN 'U'.
          CALL SCREEN  240 STARTING AT 10 05
                           ENDING   AT 60 09.
        WHEN 'D'.
          CALL SCREEN  260 STARTING AT 10 05
                           ENDING   AT 60 09.
      ENDCASE.
    WHEN 'FIND'.
      CALL SCREEN  260 STARTING AT 10 05
                       ENDING   AT 60 09.
    WHEN 'REPLACE+' OR 'REPL+'.
      IF <g_searchparms>-searchtxt = space.
        <g_searchparms>-answer = space.
        <g_searchparms>-method = 'REPL'.
        CASE mode.
          WHEN 'U'.
            CALL SCREEN  240 STARTING AT 10 05
                             ENDING   AT 60 09.
          WHEN 'D'.
            CALL SCREEN  260 STARTING AT 10 05
                             ENDING   AT 60 09.
        ENDCASE.
      ELSE.
        IF <g_searchparms>-selectedmode NS '+'.
          CONCATENATE <g_searchparms>-selectedmode '+'
              INTO <g_searchparms>-selectedmode.
        ENDIF.
      ENDIF.
    WHEN 'FIND+' .
      IF <g_searchparms>-searchtxt = space.
        <g_searchparms>-answer = space.
        <g_searchparms>-method = 'FIND'.
        CALL SCREEN  260 STARTING AT 10 05
                         ENDING   AT 60 09.
      ELSE.
        IF <g_searchparms>-selectedmode NS '+'.
          CONCATENATE <g_searchparms>-selectedmode '+'
              INTO <g_searchparms>-selectedmode.
        ENDIF.
      ENDIF.
  ENDCASE.




ENDFUNCTION.
