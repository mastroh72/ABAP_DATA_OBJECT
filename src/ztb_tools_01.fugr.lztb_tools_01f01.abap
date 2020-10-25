*----------------------------------------------------------------------*
***INCLUDE LZTB_TOOLS_01F01 .
*----------------------------------------------------------------------*




DEFINE check_if_in_table.
  read table <git_fields> with key
     fieldname = <g_cursor>-field transporting no fields.
END-OF-DEFINITION.

*DEFINE check_field_macro.
*  while offs > sy-fdpos.
*    shift &1 left by length places.
*    shift &1 right by length places.
*    if &1 cs <g_searchparms>-newtxt.
*      length = strlen( <g_searchparms>-newtxt ) + sy-fdpos.
*    else.
*      exit.
*    endif.
*  endwhile.
*END-OF-DEFINITION.



FORM build_ddfields TABLES it_components TYPE abap_component_tab
                     USING VALUE(structure_name) TYPE string.

  DATA:
    class_name     TYPE string,
    wa_struct_name TYPE string,
    wa_dfies       TYPE dfies,
    lit_components TYPE abap_component_tab,
    l_elem         TYPE REF TO cl_abap_elemdescr,
    l_struct       TYPE REF TO cl_abap_structdescr.

*    name       type string,
*    type       type ref to cl_abap_datadescr,
*    as_include type abap_bool,
*    suffix     type string,



  LOOP AT it_components ASSIGNING FIELD-SYMBOL(<wa_components>).
    IF <wa_components>-as_include = abap_true.
      l_struct ?= <wa_components>-type.
      lit_components = l_struct->get_components( ).
      wa_struct_name = l_struct->get_relative_name( ).
      PERFORM build_ddfields
         TABLES
            lit_components
         USING wa_struct_name.

    ELSE.
      class_name  = cl_abap_classdescr=>get_class_name( <wa_components>-type ).
      CASE class_name.
        WHEN '\CLASS=CL_ABAP_ELEMDESCR'.
          CLEAR wa_dfies.
          l_elem ?=  <wa_components>-type.
          IF l_elem->is_ddic_type( ) = abap_true.
            wa_dfies = l_elem->get_ddic_field( p_langu = sy-langu ).
          ELSE.
            wa_dfies-outputlen = l_elem->output_length.
            wa_dfies-inttype = l_elem->type_kind.

          ENDIF.
          wa_dfies-fieldname =  <wa_components>-name.
          wa_dfies-tabname = structure_name.
          APPEND wa_dfies TO git_ddfields.
      ENDCASE.
    ENDIF.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  REPLACE_LOCAL
*&---------------------------------------------------------------------*
*       lokales Ersetzen
*----------------------------------------------------------------------*

FORM find_set_up.
  READ TABLE <git_fields> INDEX 1 ASSIGNING <gwa_fields>.
  CONCATENATE <g_searchparms>-tablename '-' <gwa_fields>-fieldname
    INTO g_firstfield.
ENDFORM.                    "find_set_up



*---------------------------------------------------------------------*
*       FORM replace_local                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  TOP_LINE                                                      *
*---------------------------------------------------------------------*
FORM replace_local USING top_line.
  DATA :
     text_length TYPE i.
  <g_searchparms>-setcursor = 'X'.
  IF <g_cursor>-line NE search_line.       "akt. pos <> repl.pos.
    check_if_in_table.
    IF sy-subrc NE 0.
      <g_cursor>-field = g_firstfield.
      startline = top_line.
    ELSE.                              "innerhalb
      IF <g_cursor>-field NE g_firstfield.
        offs = 0.
        <g_cursor>-field = g_firstfield.
        startline = top_line + <g_cursor>-line - 1.
      ELSE.
        startline = top_line + <g_cursor>-line - 1.
        offs = <g_cursor>-fieldoffset.
      ENDIF.
    ENDIF.
    topcur = startline.
  ELSE.
    IF search_line = <g_cursor>-line AND offs = <g_cursor>-fieldoffset.
      current_line_save = top_line + <g_cursor>-line - 1.
      IF startline NE current_line_save.
        check_if_in_table.
        IF sy-subrc = 0.
          <g_cursor>-field = g_firstfield.
          startline = top_line + <g_cursor>-line - 1.
        ENDIF.
      ENDIF.
    ELSEIF search_line = <g_cursor>-line AND
           offs NE <g_cursor>-fieldoffset.
      current_line_save = top_line + <g_cursor>-line - 1.
      IF startline = current_line_save.
        offs = <g_cursor>-fieldoffset.
      ELSE.
        check_if_in_table.
        IF sy-subrc = 0.
          <g_cursor>-field = g_firstfield.
          startline = top_line + <g_cursor>-line - 1.
          offs = <g_cursor>-fieldoffset.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
*  READ TABLE <git_lookin> ASSIGNING <gwa_lookin> INDEX startline.
*  IF  <gwa_lookin>-PFLEGE_FLAG NE 'S' AND
*      <gwa_lookin>-PFLEGE_FLAG NE 'E' and
*       MARK_COUNT ne LISTTAB_LINES.                 "091299
**     AND MARK_COUNT NE NUMBER_OF_LINES.
*    MESSAGE s303(eu).
**   Lokales Ersetzen ist im Anzeigemodus nicht möglich
*    offs = offs + 1.
*    PERFORM nextfield USING top_line.
*  ELSE.

  PERFORM nextfield USING top_line.
  IF <g_searchparms>-repllength GT <g_searchparms>-searchlength.
    text_length = strlen( <g_currfield> ) +
                 ( <g_searchparms>-repllength -
                   <g_searchparms>-searchlength ).
    IF text_length GT g_fieldsize.
      PERFORM nextfield USING top_line.
      MESSAGE s344(eu).
*   Kein ausreichender Platz für die Ersetzung
      EXIT.
    ENDIF.
  ENDIF.
  PERFORM text_replace USING top_line.
*ENDIF.
ENDFORM.                               " REPLACE_LOCAL
*&---------------------------------------------------------------------*
*&      Form  REPLACE_ALL_LOCAL
*&---------------------------------------------------------------------*
*       komplettes Ersetzen ohne Abfrage
*----------------------------------------------------------------------*
FORM replace_all_local USING top_line.
  <g_searchparms>-setcursor = 'X'.
  search_line = 1.
*  IF <g_cursor>-field EQ g_firstfield
*    offs = 0.
*    startline = top_line = top_line + <g_cursor>-line - 1.
*  ELSE
  IF <g_cursor>-field EQ g_firstfield.
    offs = <g_cursor>-fieldoffset.
    startline = top_line = top_line + <g_cursor>-line - 1.
  ELSE.
    check_if_in_table.
    IF sy-subrc NE 0.
      offs = 0.
      <g_cursor>-field = g_firstfield.
      startline = top_line.
    ENDIF.
  ENDIF.
  PERFORM replace_all.
  PERFORM alle_entmarkieren.
ENDFORM.                               " REPLACE_ALL_LOCAL

*---------------------------------------------------------------------*
*       FORM MSG_SRC_NEXT_LOCAL                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  TOP_LINE                                                      *
*---------------------------------------------------------------------*
FORM msg_src_next_local USING top_line.

  <g_searchparms>-setcursor = 'X'.
  IF <g_searchparms>-searchtxt EQ space.         "noch kein suchtext
    check_if_in_table.
    IF sy-subrc NE 0.
      offs = 0.
      startline = top_line.
      last_field_index = 1.
      <g_cursor>-field = g_firstfield.
    ELSE.                              "innerhalb
      IF <g_cursor>-field NE g_firstfield.
        offs = 0.
        <g_cursor>-field = g_firstfield.
        last_field_index = 1.
        startline = top_line +  <g_cursor>-line - 1.
      ELSE.
        startline = top_line +  <g_cursor>-line - 1.
        offs = <g_cursor>-fieldoffset.
      ENDIF.
    ENDIF.
    topcur = startline.
  ELSEIF <g_cursor>-line NE search_line.   "akt. pos <> suchpos.
    check_if_in_table.
    IF sy-subrc NE 0.
      offs = 0.
      startline = top_line.
      last_field_index = 1.
      <g_cursor>-field = g_firstfield.
    ELSE.                              "innerhalb
      IF <g_cursor>-field NE g_firstfield.  "nicht auf ~
        offs = 0.
        last_field_index = 1.
        <g_cursor>-field = g_firstfield.
        startline = top_line + <g_cursor>-line - 1.
      ELSE.
        startline = top_line + <g_cursor>-line - 1.
        offs = <g_cursor>-fieldoffset.
      ENDIF.
    ENDIF.
    topcur = startline.
  ELSE.
    IF search_line = <g_cursor>-line AND offs = <g_cursor>-fieldoffset.
      current_line_save = top_line + <g_cursor>-line - 1.
      IF startline = current_line_save.
        offs = offs + 1.
      ELSE.
        check_if_in_table.
        IF sy-subrc NE 0.
          startline = top_line + <g_cursor>-line - 1.
        ELSE.
          offs = offs + 1.
        ENDIF.
      ENDIF.
    ELSEIF search_line = <g_cursor>-line AND offs NE
      <g_cursor>-fieldoffset.
      current_line_save = top_line + <g_cursor>-line - 1.
      IF startline = current_line_save.
        offs = <g_cursor>-fieldoffset.
      ELSE.
        check_if_in_table.
        IF sy-subrc = 0.
          startline = top_line + <g_cursor>-line - 1.
        ENDIF.
        offs = <g_cursor>-fieldoffset.
      ENDIF.
    ENDIF.
  ENDIF.
  PERFORM nextfield USING top_line.
*  IF <g_searchparms>-searchtxt = space.
*    answer = space.
*    CALL SCREEN  240 STARTING AT 10 05
*                     ENDING   AT 60 09.
*    IF answer <> space.
*      PERFORM nextfield USING top_line.
*    ENDIF.
*  ELSE.
*    PERFORM nextfield USING top_line.
*  ENDIF.
ENDFORM.                               " MSG_SRC_NEXT_LOCAL

*---------------------------------------------------------------------*
*       FORM NEXTFIELD                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_TOP_LINE                                                    *
*---------------------------------------------------------------------*
FORM nextfield USING p_top_line.
  DATA: length           LIKE sy-fdpos,
        no_more_matches,
        found,
        pos              TYPE i,
        firstfield       LIKE sy-tabix,
        curr_field_index TYPE sy-tabix,
        tabix_lookin     LIKE sy-tabix.

  FIELD-SYMBOLS : <field> TYPE any.

  firstfield = pos = 0.
  firstflag = 'X'.
  foundafter = space.
  PERFORM alle_entmarkieren.
*  CLEAR <gwa_lookin>.

  LOOP AT <git_lookin> ASSIGNING <gwa_lookin> FROM startline.
    tabix_lookin = sy-tabix.

* I has issue find same string on same row when the string is 2 different fields on the same
* row.  Since it starts over at the first component it will keep looking.
* I think this should be changed to do the search once on each line finding all
* locations and save it intneral table. then each time it call
* it will pop the top entry off and leave the remaining.
* but that would have issue if the user moves the cursor between finds hmm
    LOOP AT <git_fields> ASSIGNING <gwa_fields> FROM last_field_index.
      ASSIGN COMPONENT <gwa_fields>-fieldname
          OF STRUCTURE <gwa_lookin> TO <g_currfield>.

      curr_field_index = sy-tabix.

      temp_currfield = <g_currfield>.

      DESCRIBE FIELD temp_currfield LENGTH g_fieldsize IN CHARACTER MODE.
      IF tabix_lookin = startline.
        IF <g_searchparms>-newtxt CA '*+'.
          CHECK temp_currfield CP <g_searchparms>-newtxt.
        ELSE.
          CHECK temp_currfield CS <g_searchparms>-newtxt.
        ENDIF.


        length = strlen( <g_searchparms>-newtxt ) + sy-fdpos.
        PERFORM check_field USING <g_currfield> CHANGING length.

        length = length - strlen( <g_searchparms>-newtxt ).
        IF offs > length.
          CLEAR offs.
          CONTINUE.
        ELSE.
          offs = length.
        ENDIF.
      ELSE.
        IF <g_searchparms>-newtxt CA '*+'.
          CHECK  temp_currfield CP <g_searchparms>-newtxt.
        ELSE.
          CHECK  temp_currfield CS <g_searchparms>-newtxt.
        ENDIF.
        length = strlen( <g_searchparms>-newtxt ) + sy-fdpos.
        offs = sy-fdpos.
      ENDIF.
      IF <g_searchparms>-newtxt NA '*+'.
        IF <g_searchparms>-use_upper_lower EQ 'X'.
          DO.
            REPLACE <g_pos> WITH 'XXXX' INTO <g_currfield>.
            IF sy-subrc NE 0.
              no_more_matches = 'X'.
              found = 'X'.
              EXIT.
            ENDIF.
            IF <g_currfield>+offs(4) NE 'XXXX'.
*   Suchstring vorhanden, aber nicht an aktueller Stelle
              READ TABLE <git_lookin> ASSIGNING <gwa_lookin> INDEX
  sy-tabix.
              offs = offs + 1.
              CHECK <g_currfield> CS <g_searchparms>-newtxt.
              length = strlen( <g_searchparms>-newtxt ) + sy-fdpos.
              PERFORM check_field USING <g_currfield> CHANGING length.
*              WHILE offs > sy-fdpos.
*                SHIFT <g_currfield> LEFT BY length PLACES.
*                SHIFT <g_currfield> RIGHT BY length PLACES.
*                IF <g_currfield> CS <g_searchparms>-newtxt.
*                  length = strlen( <g_searchparms>-newtxt ) + sy-fdpos.
*                ELSE.
*                  found = 'X'.
*                  EXIT.
*                ENDIF.
*              ENDWHILE.
              length = length - strlen( <g_searchparms>-newtxt ).
              IF offs > length.
                no_more_matches = 'X'.
                found = 'X'.
                EXIT.
              ELSE.
                offs = length.
              ENDIF.
            ELSE.
              found = 'X'.
              EXIT.
            ENDIF.
          ENDDO.
          IF no_more_matches = 'X'.
            no_more_matches = ' '.
            CONTINUE.
          ENDIF.
        ENDIF.
      ELSE.
        CHECK <g_currfield> CP <g_searchparms>-newtxt.
        length = strlen( <g_searchparms>-newtxt ) + sy-fdpos.
        offs = sy-fdpos.
      ENDIF.
      IF firstflag = 'X'.
        CLEAR firstflag. firstfield = topcur.
      ENDIF.
      foundafter = 'X'.
      found = 'X'.
      EXIT.
    ENDLOOP.

    IF found = 'X'.
      EXIT.
    ELSE.
      last_field_index = 1.
      CLEAR offs.
    ENDIF.
  ENDLOOP.

  IF found = space.
    DESCRIBE TABLE <git_lookin> LINES DATA(lookin_lines).
    IF tabix_lookin = lookin_lines.
      startline = 1.
      p_top_line = 1.
      last_field_index = 1.
      MESSAGE s005(zdo).
      <g_searchparms>-setcursor = 'X'.
      <g_searchparms>-top_line = 1.
      <g_cursor>-line = 1.
      CLEAR <g_cursor>-fieldoffset.
      READ TABLE <git_fields> ASSIGNING <gwa_fields> INDEX 1.
      IF sy-subrc = 0.
        <g_cursor>-field = <gwa_fields>-fieldname.
      ENDIF.
* Did not find any more in table
    ENDIF.
  ENDIF.

  last_field_index = curr_field_index.
  IF found = 'X'.
    IF foundafter <> 'X'.
      IF firstfield = 0.
*      MESSAGE s022 WITH <g_searchparms>-newtxt.
*      Text & wurde nicht gefunden


        <g_cursor>-fieldoffset = offs.
        <g_cursor>-field = <gwa_fields>-fieldname.

        offs = 0.
        search_line = <g_cursor>-line.
        <g_searchparms>-setcursor = 'X'.
*        <g_searchparms>-setcursor = 'X'.
      ELSE.
        topcur = firstfield.
      ENDIF.
    ELSE.
      READ TABLE <git_lookin> INDEX tabix_lookin ASSIGNING <gwa_lookin>.
      ASSIGN COMPONENT <g_markfield> OF STRUCTURE <gwa_lookin> TO <field>.
      <field> = 'X'.
      startline = sy-tabix.
      p_top_line = sy-tabix.
      <g_searchparms>-top_line = sy-tabix.
      <g_cursor>-fieldoffset = offs.
      <g_cursor>-field = <gwa_fields>-fieldname.
      <g_searchparms>-setcursor = 'X'.
      search_line = 1.
      <g_cursor>-line = 1.
    ENDIF.
  ENDIF.
ENDFORM.                               " NEXTFIELD


*---------------------------------------------------------------------*
*       FORM TEXT_REPLACE                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_TOP_LINE                                                    *
*---------------------------------------------------------------------*
FORM text_replace USING p_top_line.
  DATA length LIKE sy-fdpos.
  DATA: rfound(1).
  DATA: no_more_matches,
        found,
        tabix_lookin LIKE sy-tabix.
  FIELD-SYMBOLS: <search>, <repl>.

  PERFORM alle_entmarkieren.
  IF g_firstsearch = 'X'.
    CLEAR g_firstsearch.                 " TOPCUR = 1.
  ENDIF.
  READ TABLE <git_lookin> ASSIGNING <gwa_lookin> INDEX startline.
  IF <g_currfield> CS <g_searchparms>-searchtxt.
    IF <g_searchparms>-rsearch = 'R'.
      IF offs <= sy-fdpos AND <g_searchparms>-use_upper_lower = 'X'.
        ASSIGN <g_searchparms>-searchtxt(<g_searchparms>-searchlength)
            TO <search>.
        ASSIGN <g_searchparms>-repltxt(<g_searchparms>-repllength)
            TO <repl>.
        REPLACE <search> WITH <repl> INTO <g_currfield>.
        IF sy-subrc = 0.
          offs = sy-fdpos.
*          TEST_TAB-CHANGE_FLAG = 'X'.
*          TEST_TAB-MESS_CHANGE = 'X'.
*          MODIFY <git_lookin> INDEX STARTLINE.
          modifiziert = 'X'.
          mess_change = 'X'.
          IF <g_currfield> CS <g_searchparms>-searchtxt.
            <g_searchparms>-rsearch = 'S'.
          ELSE.
            CLEAR <g_searchparms>-rsearch.
          ENDIF.
        ENDIF.
      ELSE.
        IF offs > sy-fdpos.
          PERFORM check_field USING <g_currfield> CHANGING length.

*          WHILE offs > sy-fdpos.
*            SHIFT <g_currfield> LEFT BY length PLACES.
*            SHIFT <g_currfield> RIGHT BY length PLACES.
*            IF <g_currfield> CS <g_searchparms>-newtxt.
*              length = strlen( <g_searchparms>-newtxt ) + sy-fdpos.
*            ELSE.
*              EXIT.
*            ENDIF.
*          ENDWHILE.



          length = length - strlen( <g_searchparms>-newtxt ).
          IF offs > length.
          ELSE.
            offs = length.
            READ TABLE <git_lookin> ASSIGNING <gwa_lookin> INDEX
startline.
            PERFORM replace_text.
            offs = sy-fdpos.
*            TEST_TAB-CHANGE_FLAG = 'X'.
*            TEST_TAB-MESS_CHANGE = 'X'.
*            MODIFY TEST_TAB INDEX STARTLINE.
            modifiziert = 'X'.
            mess_change = 'X'.
            IF <g_currfield> CS <g_searchparms>-searchtxt.
              <g_searchparms>-rsearch = 'S'.
            ELSE.
              CLEAR <g_searchparms>-rsearch.
            ENDIF.
          ENDIF.
        ELSE.
          PERFORM replace_text.
          offs = sy-fdpos.
*          TEST_TAB-CHANGE_FLAG = 'X'.
*          TEST_TAB-MESS_CHANGE = 'X'.
*          MODIFY TEST_TAB INDEX STARTLINE.
          modifiziert = 'X'.
          mess_change = 'X'.
          IF <g_currfield> CS <g_searchparms>-searchtxt.
            <g_searchparms>-rsearch = 'S'.
          ELSE.
            CLEAR <g_searchparms>-rsearch.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
*  CLEAR <gwa_lookin>.
  LOOP AT <git_lookin> ASSIGNING <gwa_lookin> FROM startline.
    tabix_lookin = sy-tabix.
    LOOP AT <git_fields> ASSIGNING <gwa_fields>.
      ASSIGN COMPONENT <gwa_fields>-fieldname
          OF STRUCTURE <gwa_lookin> TO <g_currfield>.
      DESCRIBE FIELD <g_currfield> LENGTH g_fieldsize IN CHARACTER MODE.
      IF sy-tabix = startline.
        CHECK <g_currfield> CS <g_searchparms>-searchtxt.
        CLEAR <g_searchparms>-rsearch.
        length = strlen( <g_searchparms>-newtxt ) + sy-fdpos.
        PERFORM check_field USING <g_currfield> CHANGING length.
*      WHILE offs > sy-fdpos.
*        SHIFT <g_currfield> LEFT BY length PLACES.
*        SHIFT <g_currfield> RIGHT BY length PLACES.
*        IF <g_currfield> CS <g_searchparms>-newtxt.
*          length = strlen( <g_searchparms>-newtxt ) + sy-fdpos.
*        ELSE.
*          EXIT.
*        ENDIF.
*    ENDWHILE.
        length = length - strlen( <g_searchparms>-newtxt ).
        IF offs > length.
          CONTINUE.
        ELSE.
          offs = length.
        ENDIF.
      ELSE.
        CHECK <g_currfield> CS <g_searchparms>-searchtxt.
        offs = sy-fdpos.
      ENDIF.
      IF <g_searchparms>-use_upper_lower EQ 'X'.
        DO.
          REPLACE <g_pos> WITH 'XXXX' INTO <g_currfield>.
          IF sy-subrc NE 0.
            no_more_matches = 'X'.
            found = 'X'.
            EXIT.
          ENDIF.
          IF <g_currfield>+offs(4) NE 'XXXX'.
*   Suchstring vorhanden, aber nicht an aktueller Stelle
            READ TABLE <git_lookin> ASSIGNING <gwa_lookin> INDEX sy-tabix.
            offs = offs + 1.
            CHECK <g_currfield> CS <g_searchparms>-newtxt.
            length = strlen( <g_searchparms>-newtxt ) + sy-fdpos.
            PERFORM check_field USING <g_currfield> CHANGING length.
*          WHILE offs > sy-fdpos.
*            SHIFT <g_currfield> LEFT BY length PLACES.
*            SHIFT <g_currfield> RIGHT BY length PLACES.
*            IF <g_currfield> CS <g_searchparms>-newtxt.
*              length = strlen( <g_searchparms>-newtxt ) + sy-fdpos.
*            ELSE.
*              EXIT.
*            ENDIF.
*          ENDWHILE.
            length = length - strlen( <g_searchparms>-newtxt ).
            IF offs > length.
              no_more_matches = 'X'.
              found = 'X'.
              EXIT.
            ELSE.
              offs = length.
            ENDIF.
          ELSE.
            found = 'X'.
            EXIT.
          ENDIF.
        ENDDO.
        IF no_more_matches = 'X'.
          no_more_matches = ' '.
          CONTINUE.
        ENDIF.
      ENDIF.
      IF <g_searchparms>-rsearch = 'S'.
        CLEAR <g_searchparms>-rsearch.
      ELSE.
        <g_searchparms>-rsearch = 'R'.
        rfound = 'X'.
        startline = sy-tabix.
        p_top_line = sy-tabix.
        search_line = 1.
        found = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF found = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.
  IF rfound = space.
    CLEAR <g_searchparms>-rsearch.
    g_firstsearch = 'X'.
*    MESSAGE s022 WITH <g_searchparms>-searchtxt.
* Text & wurde nicht gefunden
    offs = <g_cursor>-fieldoffset.
    search_line = <g_cursor>-line.
  ENDIF.
ENDFORM.                    "text_replace


*---------------------------------------------------------------------*
*       FORM REPLACE_ALL                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM replace_all.
  DATA: no_space,
        text_length  TYPE i,
        shiftlength  TYPE i,
        textlen      TYPE i,
        rest         TYPE i,
        text(73),
        textreplaced.
  FIELD-SYMBOLS: <search>, <repl>.

  CLEAR: repcounter.
  LOOP AT <git_lookin> ASSIGNING <gwa_lookin>
     FROM startline . "WHERE <g_currfield> CS <g_searchparms>-searchtxt.
    IF "test_tab-pflege_flag EQ 'S' OR test_tab-pflege_flag EQ 'E' OR
                                      mark_count EQ listtab_lines.
*                            MARK_COUNT EQ NUMBER_OF_LINES.    "091299
      IF <g_searchparms>-use_upper_lower = ' '.
        PERFORM repl_text USING <g_currfield> offs 'X'.
        IF sy-subrc NE 0.
          no_space = 'X'.
          CONTINUE.
        ENDIF.
*        test_tab-change_flag = 'X'.
*        test_tab-mess_change = 'X'.
*        MODIFY test_tab INDEX sy-tabix.
        IF modifiziert = space.
          modifiziert = 'X'.
          mess_change = 'X'.
        ENDIF.
        ADD 1 TO repcounter.
      ELSE.
        ASSIGN <g_searchparms>-searchtxt(<g_searchparms>-searchlength)
             TO <search>.
        ASSIGN <g_searchparms>-repltxt(<g_searchparms>-repllength)
             TO <repl>.
        WHILE <g_currfield> CS <g_searchparms>-searchtxt.
          IF <g_searchparms>-repllength GT <g_searchparms>-searchlength.
            text_length = strlen( <g_currfield> ) +
                          ( <g_searchparms>-repllength -
                            <g_searchparms>-searchlength ).
            IF text_length GT g_fieldsize.
              no_space = 'X'.
              EXIT.
            ENDIF.
          ENDIF.
          shiftlength = strlen( <g_searchparms>-searchtxt ) + sy-fdpos.
          REPLACE <search> WITH <repl> INTO <g_currfield>.
          IF sy-subrc = 0.
            textreplaced = 'X'.
          ELSE.
            textlen = strlen( text ).
            rest = shiftlength - textlen.
            text+textlen = <g_currfield>+textlen(rest).
            SHIFT <g_currfield> LEFT BY shiftlength PLACES.
            SHIFT <g_currfield> RIGHT BY shiftlength PLACES.
          ENDIF.
        ENDWHILE.
        IF textreplaced = 'X'.
          CLEAR textreplaced.
          textlen = strlen( text ).
          text+textlen = <g_currfield>+textlen.
          <g_currfield> = text.
          ADD 1 TO repcounter.
*          test_tab-change_flag = 'X'.
*          test_tab-mess_change = 'X'.
*          MODIFY test_tab INDEX sy-tabix.
          modifiziert = 'X'.
          mess_change = 'X'.
        ENDIF.
        CLEAR: text, textlen.
        IF no_space = 'X'.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF no_space = 'X'.
    MESSAGE i344(eu).
*   Kein ausreichender Platz für die Ersetzung
    CLEAR no_space.
  ENDIF.
*  MESSAGE s023 WITH repcounter.
*  Der Suchstring wurde in & Nachrichten ersetzt
ENDFORM.                               " REPLACE_ALL

*---------------------------------------------------------------------*
*       FORM ALLE_ENTMARKIEREN                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM alle_entmarkieren.
  FIELD-SYMBOLS:
    <wa>    TYPE any,
    <field> TYPE any.

  LOOP AT <git_lookin> ASSIGNING <wa>.
    ASSIGN COMPONENT <g_markfield> OF STRUCTURE <wa> TO <field>.
    IF <field> NE space.
      CLEAR <field>.
*      MODIFY <git_lookin>.
    ENDIF.
  ENDLOOP.
*  CLEAR: mark1_pos, mark2_pos.
ENDFORM.                               " ALLE_ENTMARKIEREN


*---------------------------------------------------------------------*
*       FORM REPLACE_TEXT                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM replace_text.
  DATA: rest(73),
        posf     LIKE sy-fdpos,
        posr     LIKE sy-fdpos,
        lenf     LIKE sy-fdpos.
*
*        REPLACE
* <------FIND---!-----------------------> = <LINE>
* 0      !   !  !                       !
*        !   !  POSR                    !
*        !   !=========LENF=============!
*        !   POSF                       !
*        sy-fdpos                       CONTENT_LEN
*

  posf = sy-fdpos + <g_searchparms>-searchlength.
  posr = sy-fdpos + <g_searchparms>-repllength.
  lenf = g_fieldsize - posf.

  rest = <g_currfield>+posf(lenf).
  <g_currfield>+sy-fdpos =
      <g_searchparms>-repltxt(<g_searchparms>-repllength).
  <g_currfield>+posr = rest.

ENDFORM.                               " REPLACE_TEXT


*---------------------------------------------------------------------*
*       FORM REPL_TEXT                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  RTEXT                                                         *
*  -->  OFFSET                                                        *
*  -->  ALL                                                           *
*---------------------------------------------------------------------*
FORM repl_text USING rtext TYPE any offset all.
  DATA: exit_flag.
  DATA: oldlen        TYPE i, newlen TYPE i,
        restlen       TYPE i, rest2len TYPE i,
        textlen       TYPE i,
        fieldtype(30) TYPE c.

  DATA: dref TYPE REF TO data.
  FIELD-SYMBOLS: <t1>, <t2>,<t3>, <text>.

  CREATE DATA dref LIKE rtext.
  ASSIGN dref->* TO <text>.

  CLEAR exit_flag.
  ASSIGN rtext(g_fieldsize) TO <t3>.
  WHILE <t3> CS <g_searchparms>-searchtxt.
    IF <g_searchparms>-repllength GT <g_searchparms>-searchlength.
      textlen = strlen( <t3> ) + ( <g_searchparms>-repllength -
                                   <g_searchparms>-searchlength ).
      IF textlen GT g_fieldsize.
        sy-subrc = 1.
        EXIT.
      ENDIF.
    ENDIF.
    CLEAR <text>.
    oldlen = sy-fdpos + newlen.
    restlen = g_fieldsize - oldlen.
    IF oldlen <> 0.
      ASSIGN rtext(oldlen) TO <t1>.
      <text> = <t1>.
    ENDIF.
    ASSIGN <text>+oldlen(restlen) TO <t1>.
    IF all NE 'X'.
      IF offs <= oldlen.
        <t1> = <g_searchparms>-repltxt.
        newlen = oldlen + <g_searchparms>-repllength.
        oldlen = oldlen + <g_searchparms>-searchlength.
        exit_flag = 'X'.
      ELSE.
        <t1> = <g_searchparms>-searchtxt.
        newlen = oldlen + <g_searchparms>-searchlength.
        oldlen = oldlen + <g_searchparms>-searchlength.
      ENDIF.
    ELSE.
      <t1> = <g_searchparms>-repltxt.
      newlen = oldlen + <g_searchparms>-repllength.
      oldlen = oldlen + <g_searchparms>-searchlength.
    ENDIF.
    restlen = g_fieldsize - oldlen.
    rest2len = g_fieldsize - newlen.
    IF restlen > 0.
      IF rest2len < restlen. rest2len = restlen. ENDIF.
      ASSIGN rtext+oldlen(restlen) TO <t1>.
      ASSIGN <text>+newlen(rest2len) TO <t2>.
      <t2> = <t1>.
    ENDIF.
    rtext = <text>.
    IF rest2len > 0.
      ASSIGN rtext+newlen(rest2len) TO <t3>.
    ELSE.
      ASSIGN ' ' TO <t3>.
    ENDIF.
    IF exit_flag = 'X'. EXIT. ENDIF.
  ENDWHILE.
ENDFORM.                               " REPL_TEXT
*&---------------------------------------------------------------------*
*&      Form  looptest
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM looptest.
  FIELD-SYMBOLS <field> TYPE any.
  FIELD-SYMBOLS <wa> TYPE any.


  LOOP AT <git_lookin> ASSIGNING <wa>.

    ASSIGN COMPONENT 'CARSLABEL' OF STRUCTURE <wa> TO <field>.

*    ASSIGN comp<temp2> = <wa>.
*    move

  ENDLOOP.
ENDFORM.                    " looptest

*---------------------------------------------------------------------*
*       FORM MSG_SRC_REPL_LOCAL                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  TOP_LINE                                                      *
*---------------------------------------------------------------------*
FORM msg_src_repl_local USING top_line.
  <g_searchparms>-setcursor = 'X'.


  check_if_in_table.
  IF sy-subrc = 0.
    IF <g_cursor>-field NE g_firstfield.
      offs = 0.
      startline = top_line + <g_cursor>-line - 1.
      <g_cursor>-field  = g_firstfield.
    ELSE.
      startline = top_line + <g_cursor>-line - 1.
      offs = <g_cursor>-fieldoffset.
    ENDIF.
  ELSE.
    offs = 0.
    startline = top_line.
    last_startline  = startline.
    <g_cursor>-field  = g_firstfield.
  ENDIF.
  PERFORM nextfield USING top_line.

ENDFORM.                               " MSG_SRC_REPL_LOCAL

*---------------------------------------------------------------------*
*       FORM check_field                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE(P_FIELD)                                                *
*---------------------------------------------------------------------*



FORM check_field USING VALUE(p_field) TYPE any  CHANGING length LIKE sy-fdpos.
*  check_field_macro p_field.
  WHILE offs > sy-fdpos.
    SHIFT  p_field LEFT BY length PLACES.
    SHIFT  p_field RIGHT BY length PLACES.
    IF  p_field CS <g_searchparms>-newtxt.
      length = strlen( <g_searchparms>-newtxt ) + sy-fdpos.
    ELSE.
      EXIT.
    ENDIF.
  ENDWHILE.
ENDFORM.                    "check_field



*---------------------------------------------------------------------*
*       FORM create_data_objects                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM create_data_objects.
  DATA :
    it_relationship TYPE ztt_do_relationship,
    wa_relationship TYPE LINE OF ztt_do_relationship,
    ptr_to_it       TYPE REF TO data,
    ptr_to_wa       TYPE REF TO data,
    ptr_to_tc       TYPE REF TO data,
    ptr_to_tc_lines TYPE REF TO data.

*  git_filter TYPE zslot_tt_do_so_rowobj
*    WITH HEADER LINE,
*  git_sort_cols TYPE zttsort_columns
*    WITH HEADER LINE,
*  git_cols_list TYPE zttsort_columns
*    WITH HEADER LINE,


*      TYPE REF TO cl_filter,
*    TYPE REF TO cl_sort_cols.
*
*   TYPE zslot_tt_do_so_rowobj
*    WITH HEADER LINE,
*  git_sort_cols TYPE zttsort_columns
*
  GET REFERENCE OF git_filter[] INTO ptr_to_it.
  GET REFERENCE OF git_filter   INTO ptr_to_wa.
  GET REFERENCE OF gtc_filter   INTO ptr_to_tc .
**  GET REFERENCE OF g_gcarslblctrl_lines INTO ptr_to_tc_lines .
  CREATE OBJECT gobj_filter
    EXPORTING
      im_ptr_to_table = ptr_to_it
      im_ptr_to_wa    = ptr_to_wa
      im_ptr_to_tc    = ptr_to_tc
      im_tc_name      = 'GTC_FILTER'.


*
*
*  GET REFERENCE OF git_zcars_cust[] INTO ptr_to_it.
*  GET REFERENCE OF git_zcars_cust INTO ptr_to_wa.
*
*
*  CREATE OBJECT:
*    gobj_sort_cols
*      EXPORTING
*          im_ptr_to_table = ptr_to_it
*          im_ptr_to_wa  = ptr_to_wa.



  GET REFERENCE OF git_sort_cols[]   INTO ptr_to_it.
  GET REFERENCE OF git_sort_cols     INTO ptr_to_wa.
  GET REFERENCE OF gtc_sort_cols     INTO ptr_to_tc.

  CREATE OBJECT gobj_sort_cols
    EXPORTING
      im_ptr_to_table = ptr_to_it
      im_ptr_to_wa    = ptr_to_wa
      im_ptr_to_tc    = ptr_to_tc
      im_tc_name      = 'GTC_SORT_COLS'.



  GET REFERENCE OF git_cols_list[]   INTO ptr_to_it.
  GET REFERENCE OF git_cols_list     INTO ptr_to_wa.
  GET REFERENCE OF gtc_cols_list     INTO ptr_to_tc.

  CREATE OBJECT gobj_cols_list
    EXPORTING
      im_ptr_to_table = ptr_to_it
      im_ptr_to_wa    = ptr_to_wa
      im_ptr_to_tc    = ptr_to_tc
      im_tc_name      = 'GTC_COLS_LIST'.


*  CALL METHOD:
*    gcars_items->lk_add_link
*      EXPORTING
*        im_ptr_publisher  = gcars_cust
*        im_ptr_subscriber = gcars_items
*        im_relationship   = 'NEXT_LABEL_ID'.


  CALL METHOD gobj_cols_list->lk_add_link_local
    EXPORTING
      im_ptr_publisher  = gobj_sort_cols
      im_ptr_subscriber = gobj_cols_list
      im_relationship   = cl_sort=>co_link_row_removed.


  CALL METHOD gobj_sort_cols->lk_add_link_local
    EXPORTING
      im_ptr_publisher  = gobj_cols_list
      im_ptr_subscriber = gobj_sort_cols
      im_relationship   = cl_sort=>co_link_row_removed.



ENDFORM.                    " create_data_objects


*---------------------------------------------------------------------*
*       FORM ask_complex_selections                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FIELD                                                         *
*---------------------------------------------------------------------*
FORM ask_complex_selections
  TABLES it_selection TYPE zslot_tt_do_selection_options
  USING pfield TYPE any
        ptablename TYPE tabname
        pfieldname TYPE fieldname
        pfieldlabel TYPE scrtext_m.

  DATA:
    wa_selection LIKE LINE OF it_selection,
    wa_tb_field  TYPE rstabfield,
    ctitle       TYPE sy-title,
    it_field     TYPE RANGE OF pfield
      WITH HEADER LINE.

  FIELD-SYMBOLS:
    <wa_field>     LIKE LINE OF it_field,
    <wa_selection> LIKE LINE OF it_selection,
    <low>          TYPE any,
    <high>         TYPE any.



  CONCATENATE
    'Multiple Selection for'
    pfieldlabel
   INTO ctitle SEPARATED BY space.


  wa_tb_field-tablename = ptablename.
  wa_tb_field-fieldname = pfieldname.


  REFRESH it_field.

  LOOP AT it_selection ASSIGNING <wa_selection>
    WHERE fieldname = wa_tb_field-fieldname.

    ASSIGN :
      <wa_selection>-ptrlow->* TO <low>,
      <wa_selection>-ptrhigh->* TO <high>.

    CLEAR it_field.

    it_field-sign = <wa_selection>-sign.
    it_field-option = <wa_selection>-option.
    it_field-low   = <low>.
    it_field-high  = <high>.
    APPEND it_field.

  ENDLOOP.


  CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
    EXPORTING
      title             = ctitle
*     TEXT              =
*     SIGNED            = 'X'
*     LOWER_CASE        = ' '
*     NO_INTERVAL_CHECK = ' '
*     JUST_DISPLAY      = ' '
*     JUST_INCL         = ' '
*     EXCLUDED_OPTIONS  =
*     DESCRIPTION       =
*     HELP_FIELD        =
*     SEARCH_HELP       =
      tab_and_field     = wa_tb_field
    TABLES
      range             = it_field
    EXCEPTIONS
      no_range_tab      = 1
      cancelled         = 2
      internal_error    = 3
      invalid_fieldname = 4
      OTHERS            = 5.
  IF sy-subrc = 0.
    DELETE it_selection WHERE fieldname = wa_tb_field-fieldname.

    LOOP AT it_field ASSIGNING <wa_field>.
      CREATE DATA wa_selection-ptrlow LIKE pfield.
      CREATE DATA wa_selection-ptrhigh LIKE pfield.

      ASSIGN :
        wa_selection-ptrlow->* TO <low>,
        wa_selection-ptrhigh->* TO <high>.
      wa_selection-fieldname = wa_tb_field-fieldname.
      wa_selection-sign = <wa_field>-sign.
      wa_selection-option = <wa_field>-option.
      <low>  = <wa_field>-low.
      <high> = <wa_field>-high.


      APPEND wa_selection TO it_selection.
    ENDLOOP.
  ENDIF.



ENDFORM.                    "ask_complex_selections



*---------------------------------------------------------------------*
*       FORM user_command                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  R_UCOMM                                                       *
*  -->  RS_SELFIELD                                                   *
*---------------------------------------------------------------------*
FORM user_command USING r_ucomm     LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.

  CALL METHOD gobj_data_object->it_alv_user_command
    EXPORTING
      im_ucomm    = r_ucomm
      im_selfield = rs_selfield.

ENDFORM.                    "user_command
