FUNCTION-POOL ZDO_TB_TOOLS_01.                 "MESSAGE-ID ..



FIELD-SYMBOLS:
  <git_lookin>    TYPE STANDARD TABLE,
  <gwa_lookin>    TYPE any,
  <g_searchparms> TYPE zst_do_searchparms,
  <g_pos>,
  <g_currfield>,
  <g_markfield>,
  <g_mode>,
  <g_cursor>      TYPE zst_do_cursor,
  <git_fields>    TYPE ztt_do_tb_field_to_search,
  <gwa_fields>    TYPE LINE OF ztt_do_tb_field_to_search.

TYPE-POOLS :
  sscr,
  rsds,
  cxtab,
  ZDO01.



DATA :
  git_ddfields         TYPE ddfields,
  g_repid              TYPE sy-repid,
  gobj_data_object     TYPE REF TO zcl_do_it,
  g_title              TYPE char72,
  g_db_field           TYPE char30,
  g_filter_on          TYPE char1,
  g_field              TYPE  char30,
  g_line               TYPE i,
  git_selection        TYPE  zif_do_seo=>tt_selection_options
    WITH HEADER LINE,
  git_filter           TYPE ztt_do_filter
    WITH HEADER LINE,
*  git_filter TYPE zslot_tt_do_so_rowobj
*    WITH HEADER LINE,
  git_sort_cols        TYPE ztt_do_sortable_columns
    WITH HEADER LINE,
  git_cols_list        TYPE ztt_do_sortable_columns
    WITH HEADER LINE,
  g_fieldsize          TYPE i,
  g_firstfield(161)    TYPE c,
  search_line          TYPE i,             "FDWE cursorposition
  startline            TYPE i,               "pos. suchen/erstzen
  last_field_index     TYPE i,
  last_startline       TYPE i,
*  use_upper_lower TYPE c,         "Groß/Kleinschreib-Flag lok. Suchen
  offs(4)              TYPE n,
  topcur               LIKE sy-tfill,
  current_line_save    TYPE i,
  listtab_lines        TYPE i,
  g_firstsearch(1),
  mark_count           TYPE i,
*  text_length TYPE i,
*  firstfield LIKE sy-tabix,
  firstflag(1),
  foundafter(1),
*  pos TYPE i,
  modifiziert,
  mess_change,
  length               TYPE p,
  gilines              TYPE i,
  fcode                LIKE sy-ucomm,
  gfcode               LIKE sy-ucomm,
  repcounter           TYPE i,
  gcb_technical        TYPE c,
  g_ask_filter_on      TYPE c,
  gcb_technical_filter TYPE c,
* the following temp variable is needed to remove type P from comparing/describing.
  temp_currfield(35)   TYPE c.




* DECLARATION OF TABLECONTROL 'GTC_SORT_COLS' ITSELF
CONTROLS: gtc_sort_cols TYPE TABLEVIEW USING SCREEN 0280.


CLASS zcl_selection_options   DEFINITION LOAD.
CLASS zcl_data_object         DEFINITION LOAD.
CLASS zcl_screen              DEFINITION LOAD.


*---------------------------------------------------------------------*
*       CLASS cl_sort_cols DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS cl_sort DEFINITION INHERITING FROM zcl_do_it.
  PUBLIC SECTION.
    CONSTANTS:
      co_link_row_removed TYPE  zdo_object_relationship VALUE 'ROW_REMOVED',
      co_field_label      TYPE char10 VALUE 'LABEL',
      co_field_name       TYPE char10 VALUE 'NAME'.




    EVENTS:
      row_removed
        EXPORTING
          VALUE(ex_field_name)  TYPE char72
          VALUE(ex_field_label) TYPE char72.

    METHODS:
      on_row_removed FOR EVENT row_removed OF cl_sort
        IMPORTING
            sender
            ex_field_name
            ex_field_label,
      it_wlal,
      it_wlse,
      it_flse,
      it_flal,
      lk_add_link_local
        IMPORTING
          im_ptr_publisher  TYPE REF TO cl_sort
          im_ptr_subscriber TYPE REF TO cl_sort
          im_relationship   TYPE  zdo_object_relationship,
      constructor
        IMPORTING
          im_ptr_to_table TYPE REF TO data
          im_ptr_to_wa    TYPE REF TO data
          im_ptr_to_tc    TYPE REF TO data OPTIONAL
          im_tc_name      TYPE typename OPTIONAL,
      toggle_content,
      get_content
        EXPORTING
          ex_option TYPE char10,
      set_content
        IMPORTING
          im_option TYPE char10.



  PROTECTED SECTION.
    METHODS:
      reset_row_seq,
      it_row_deleted REDEFINITION,
      function_code_initialize REDEFINITION.
    DATA:
      content_option TYPE char10.


ENDCLASS.


*---------------------------------------------------------------------*
*       CLASS cl_sort_cols IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS cl_sort IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        im_it_table_type = 'ZTT_DO_SORTABLE_COLUMNS'
*       im_table_name    = 'ZCARS_ITEM'
*       im_db_table_type = 'ZTTZCARS_ITEM'
        im_ptr_to_table  = im_ptr_to_table
        im_ptr_to_wa     = im_ptr_to_wa
        im_ptr_to_tc     = im_ptr_to_tc
        im_tc_name       = im_tc_name.
    CALL METHOD function_code_initialize.
    CLEAR ro_tb_multi_sort_allowed.

  ENDMETHOD.


*  METHOD tc_call_append.
*
*    it_objects   type zclob_tt_do_objects.
*
*    FIELD-SYMBOLS :
*      <wa_object>  TYPE LINE OF zclob_tt_do_objects,
*      <wa>         TYPE LINE OF zttzcars_label_and_zknmt.
*
*
*    CALL METHOD super->it_row_created
*      EXPORTING
*        im_ptr_to_row = im_ptr_to_row.
*
*    ASSIGN :
*      im_ptr_to_row->*       TO <wa>.
*
**    <wa>-carsid = carsid.
*    CALL METHOD me->get_subscribed
*      EXPORTING
*        im_relationship = 'APPEND'
*      IMPORTING
*        ex_it_link      = it_objects.
*
*  ENDMETHOD.

  METHOD on_row_removed.

    DATA :
      ilines LIKE sy-tabix.

    FIELD-SYMBOLS :
      <it_rowobject> TYPE  ztt_do_sortable_columns,
      <wa>           TYPE  LINE OF ztt_do_sortable_columns.

    ASSIGN :
      ptr_it_rowobject->* TO <it_rowobject>.

    DESCRIBE TABLE <it_rowobject> LINES ilines.


    IF ilines = 1.
      READ TABLE <it_rowobject> ASSIGNING <wa> INDEX ilines.
      IF NOT <wa>-content IS INITIAL.
        ilines = ilines + 1.
        CALL METHOD me->it_insert
          EXPORTING
            im_line = ilines.
      ENDIF.
    ELSE.
      ilines = ilines + 1.
      CALL METHOD me->it_insert
        EXPORTING
          im_line = ilines.
    ENDIF.





    READ TABLE <it_rowobject> ASSIGNING <wa> INDEX ilines.
    IF sy-subrc = 0.
      <wa>-rowseq = ilines.
      <wa>-fieldname = ex_field_name.
      <wa>-fieldlabel = ex_field_label.
      <wa>-sort_up    = 'X'.

      CASE content_option.
        WHEN cl_sort=>co_field_label.
          <wa>-content = <wa>-fieldlabel.
          IF <wa>-content IS INITIAL.
            <wa>-content = <wa>-fieldname.
          ENDIF.
        WHEN cl_sort=>co_field_name.
          <wa>-content = <wa>-fieldname.
      ENDCASE.


      CALL METHOD me->it_update
        EXPORTING
          im_db_cnt = <wa>-rowdbcnt.


    ENDIF.


    CALL METHOD me->reset_row_seq.

  ENDMETHOD.

  METHOD lk_add_link_local.
    CALL METHOD me->lk_add_link
      EXPORTING
        im_ptr_publisher  = im_ptr_publisher
        im_ptr_subscriber = im_ptr_subscriber
        im_relationship   = im_relationship.

    CASE im_relationship.
      WHEN cl_sort=>co_link_row_removed.
        SET HANDLER im_ptr_subscriber->on_row_removed
          FOR im_ptr_publisher.
    ENDCASE.
  ENDMETHOD.

  METHOD it_row_deleted.
    FIELD-SYMBOLS:
      <wa>           TYPE  LINE OF ztt_do_sortable_columns.

    ASSIGN :
      im_ptr_to_row->* TO <wa>.

    IF NOT <wa>-fieldname IS INITIAL.
      RAISE EVENT row_removed
        EXPORTING
            ex_field_name = <wa>-fieldname
            ex_field_label = <wa>-fieldlabel.

      CALL METHOD super->it_row_deleted
        EXPORTING
          im_ptr_to_row = im_ptr_to_row.
    ENDIF.
  ENDMETHOD.


  METHOD set_content.

    FIELD-SYMBOLS :
      <it_rowobject> TYPE  ztt_do_sortable_columns,
      <wa>           TYPE  LINE OF ztt_do_sortable_columns.

    ASSIGN :
      ptr_it_rowobject->* TO <it_rowobject>.

    CASE im_option.
      WHEN cl_sort=>co_field_label.
        LOOP AT <it_rowobject> ASSIGNING <wa>.
          <wa>-content = <wa>-fieldlabel.
          IF <wa>-content IS INITIAL.
            <wa>-content = <wa>-fieldname.
          ENDIF.
        ENDLOOP.

      WHEN cl_sort=>co_field_name.
        LOOP AT <it_rowobject> ASSIGNING <wa>.
          <wa>-content = <wa>-fieldname.
        ENDLOOP.
    ENDCASE.

    content_option = im_option.


  ENDMETHOD.

  METHOD get_content.
    ex_option = content_option.
  ENDMETHOD.

  METHOD  toggle_content.
    CASE content_option.
      WHEN cl_sort=>co_field_label.
        CALL METHOD me->set_content
          EXPORTING
            im_option = cl_sort=>co_field_name.
      WHEN cl_sort=>co_field_name.
        CALL METHOD me->set_content
          EXPORTING
            im_option = cl_sort=>co_field_label.
    ENDCASE.
  ENDMETHOD.

  METHOD function_code_initialize.

    DATA :
      cfcode TYPE sy-ucomm.

*    CALL METHOD :
*      super->function_code_initialize.

    CONCATENATE tc_name '_FLAL' INTO cfcode.
    CALL METHOD me->set_function_code
      EXPORTING
        im_function_code = cfcode
        im_method_to_use = 'IT_FLAL'.


    CONCATENATE tc_name '_FLSE' INTO cfcode.
    CALL METHOD me->set_function_code
      EXPORTING
        im_function_code = cfcode
        im_method_to_use = 'IT_FLSE'.

    CONCATENATE tc_name '_WLSE' INTO cfcode.
    CALL METHOD me->set_function_code
      EXPORTING
        im_function_code = cfcode
        im_method_to_use = 'IT_WLSE'.

    CONCATENATE tc_name '_WLAL' INTO cfcode.
    CALL METHOD me->set_function_code
      EXPORTING
        im_function_code = cfcode
        im_method_to_use = 'IT_WLAL'.

  ENDMETHOD.


  METHOD reset_row_seq.
    DATA:
      ilines TYPE i.

    FIELD-SYMBOLS :
      <it_rowobject> TYPE  ztt_do_sortable_columns.


    ASSIGN :
      ptr_it_rowobject->* TO <it_rowobject>.

    SORT <it_rowobject> BY rowseq.
    CALL METHOD it_sort_assign_row_seq.
    DESCRIBE TABLE <it_rowobject> LINES ilines.
    IF ilines = 0.
      CALL METHOD me->tc_set_displayed_lines
        EXPORTING
          im_lines = 1.
    ENDIF.

  ENDMETHOD.


  METHOD it_wlal.
    CALL METHOD :
      me->it_mark_all,
      me->tc_delete,
      me->reset_row_seq.

  ENDMETHOD.

  METHOD it_wlse.
    CALL METHOD:
      me->tc_delete,
      me->reset_row_seq.
  ENDMETHOD.

  METHOD it_flse.
    CALL METHOD:
      me->tc_delete,
      me->reset_row_seq.
  ENDMETHOD.

  METHOD it_flal.
    CALL METHOD :
      me->it_mark_all,
      me->tc_delete,
      me->reset_row_seq.
  ENDMETHOD.
ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS cl_sort_cols DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS cl_filter DEFINITION INHERITING FROM zcl_do_it.
  PUBLIC SECTION.
    CONSTANTS :
      co_field_label TYPE char10 VALUE 'LABEL',
      co_field_name  TYPE char10 VALUE 'NAME'.
    METHODS:
      constructor
        IMPORTING
          im_ptr_to_table TYPE REF TO data
          im_ptr_to_wa    TYPE REF TO data
          im_ptr_to_tc    TYPE REF TO data OPTIONAL
          im_tc_name      TYPE typename OPTIONAL,
      it_msel,
      it_force_integrity REDEFINITION,
      it_selection_dialog,
      toggle_content,
      set_it_field_list
        IMPORTING
          im_field_list TYPE spar_dfies,
      set_selection
        IMPORTING
          im_selection TYPE REF TO data,
      get_content
        EXPORTING
          ex_option TYPE char10,
      set_content
        IMPORTING
          im_option TYPE char10.



  PROTECTED SECTION.
    METHODS :
      update_selection_from_filter
        IMPORTING
          im_ptr_to_row TYPE REF TO data,
      button_change
        IMPORTING
          im_ptr_to_row TYPE REF TO data,
      it_row_updated REDEFINITION,
      function_code_initialize REDEFINITION.
    DATA:
      it_field_list    TYPE  spar_dfies,
      ptr_it_selection TYPE REF TO data,
      content_option   TYPE char10.



ENDCLASS.






*---------------------------------------------------------------------*
*       CLASS cl_filter IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS cl_filter IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        im_it_table_type = 'ZTT_DO_FILTER'
*       im_table_name    = 'ZCARS_ITEM'
*       im_db_table_type = 'ZTTZCARS_ITEM'
        im_ptr_to_table  = im_ptr_to_table
        im_ptr_to_wa     = im_ptr_to_wa
        im_ptr_to_tc     = im_ptr_to_tc
        im_tc_name       = im_tc_name.

    CALL METHOD function_code_initialize.
  ENDMETHOD.

  METHOD get_content.
    ex_option = content_option.
  ENDMETHOD.

  METHOD  toggle_content.
    CASE content_option.
      WHEN cl_filter=>co_field_label.
        CALL METHOD me->set_content
          EXPORTING
            im_option = cl_filter=>co_field_name.
      WHEN cl_filter=>co_field_name.
        CALL METHOD me->set_content
          EXPORTING
            im_option = cl_filter=>co_field_label.
    ENDCASE.
  ENDMETHOD.

  METHOD set_it_field_list.
    it_field_list[] = im_field_list[].
  ENDMETHOD.

  METHOD set_content.

    FIELD-SYMBOLS :
      <it_rowobject> TYPE  ztt_do_filter,
      <wa>           TYPE  LINE OF ztt_do_filter.

    ASSIGN :
      ptr_it_rowobject->* TO <it_rowobject>.

    CASE im_option.
      WHEN cl_filter=>co_field_label.
        LOOP AT <it_rowobject> ASSIGNING <wa>.
          <wa>-content = <wa>-fieldlabel.
          IF <wa>-content IS INITIAL.
            <wa>-content = <wa>-fieldname.
          ENDIF.
        ENDLOOP.

      WHEN cl_filter=>co_field_name.
        LOOP AT <it_rowobject> ASSIGNING <wa>.
          <wa>-content = <wa>-fieldname.
        ENDLOOP.
    ENDCASE.

    content_option = im_option.
  ENDMETHOD.


  METHOD it_force_integrity.

    DATA:
      ptr_to_row   TYPE REF TO data.

    FIELD-SYMBOLS :
      <low>          TYPE any,
      <high>         TYPE any,
      <it_selection> TYPE zif_do_seo=>tt_selection_options,
      <wa_selection> TYPE LINE OF zif_do_seo=>tt_selection_options,
      <wa_rowobject> TYPE LINE OF ztt_do_filter,
      <it_rowobject> TYPE ztt_do_filter.

    ASSIGN:
      ptr_it_selection->*    TO <it_selection>,
      ptr_it_rowobject->*    TO <it_rowobject>.

    CALL METHOD super->it_force_integrity.

    LOOP AT <it_rowobject> ASSIGNING <wa_rowobject>.
      GET REFERENCE OF <wa_rowobject> INTO ptr_to_row.
      CALL METHOD me->button_change
        EXPORTING
          im_ptr_to_row = ptr_to_row.

      READ TABLE <it_selection> ASSIGNING <wa_selection>
           WITH KEY fieldname = <wa_rowobject>-fieldname.
      IF sy-subrc = 0.
        ASSIGN:
         <wa_selection>-ptrlow->*   TO <low>,
         <wa_selection>-ptrhigh->*  TO <high>.
        <wa_rowobject>-low = <low>.
        <wa_rowobject>-high = <high>.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD it_row_updated.
    CALL METHOD:
      me->update_selection_from_filter
        EXPORTING
          im_ptr_to_row = im_ptr_to_row,
     me->button_change
        EXPORTING
          im_ptr_to_row = im_ptr_to_row.

  ENDMETHOD.

  METHOD set_selection.
    ptr_it_selection = im_selection.
*    it_selection = ch_selection.
  ENDMETHOD.

  METHOD update_selection_from_filter.


    DATA:
      firstrecord  TYPE sytabix,
      wa_selection TYPE LINE OF zif_do_seo=>tt_selection_options,
      ilines       TYPE i.

    FIELD-SYMBOLS :
      <low>          TYPE any,
      <high>         TYPE any,
      <it_selection> TYPE zif_do_seo=>tt_selection_options,
      <wa_selection> TYPE LINE OF zif_do_seo=>tt_selection_options,
      <wa>           TYPE LINE OF ztt_do_filter.

    ASSIGN:
      ptr_it_selection->*    TO <it_selection>,
      im_ptr_to_row->*       TO <wa>.



    LOOP AT <it_selection> ASSIGNING <wa_selection>
      WHERE fieldname = <wa>-fieldname.
      ilines = ilines + 1.
      IF ilines > 1.
        EXIT.
      ELSE.
        firstrecord = sy-tabix.
      ENDIF.
    ENDLOOP.



    CASE ilines.
      WHEN 0.
        IF <wa>-high IS INITIAL.
          IF NOT <wa>-low IS INITIAL.
            IF <wa>-low CS '*'.
              wa_selection-option = 'CP'.
            ELSE.
              wa_selection-option = 'EQ'.
            ENDIF.
            wa_selection-fieldname = <wa>-fieldname.
            wa_selection-sign = 'I'.
            CREATE DATA:
              wa_selection-ptrlow   TYPE (<wa>-rollname),
              wa_selection-ptrhigh  TYPE (<wa>-rollname).
            ASSIGN:
              wa_selection-ptrlow->*   TO <low>,
              wa_selection-ptrhigh->*  TO <high>.
            <low> = <wa>-low.
            <high> = <wa>-high.
            APPEND wa_selection TO <it_selection>.
          ENDIF.
        ELSE.
          wa_selection-fieldname = <wa>-fieldname.
          wa_selection-sign = 'I'.
          wa_selection-option = 'BT'.
          CREATE DATA:
            wa_selection-ptrlow   TYPE (<wa>-rollname),
            wa_selection-ptrhigh  TYPE (<wa>-rollname).
          ASSIGN:
            wa_selection-ptrlow->*   TO <low>,
            wa_selection-ptrhigh->*  TO <high>.
          <low> = <wa>-low.
          <high> = <wa>-high.
          APPEND wa_selection TO <it_selection>.
        ENDIF.
      WHEN OTHERS.
        IF <wa>-high IS INITIAL.
          IF <wa>-low IS INITIAL.
            DELETE <it_selection>  INDEX firstrecord .
          ELSE.
            READ TABLE <it_selection> ASSIGNING <wa_selection>
              INDEX firstrecord.
            IF sy-subrc = 0.
              IF <wa>-low CS '*'.
                <wa_selection>-option = 'CP'.
              ELSE.
                <wa_selection>-option = 'EQ'.
              ENDIF.
              <wa_selection>-sign = 'I'.
              ASSIGN:
               <wa_selection>-ptrlow->*   TO <low>,
               <wa_selection>-ptrhigh->*  TO <high>.
              <low> = <wa>-low.
              <high> = <wa>-high.
            ENDIF.
          ENDIF.
        ELSE.
          READ TABLE <it_selection> ASSIGNING <wa_selection>
            INDEX firstrecord.
          IF sy-subrc = 0.
            <wa_selection>-sign = 'I'.
            <wa_selection>-option = 'BT'.
            ASSIGN:
             <wa_selection>-ptrlow->*   TO <low>,
             <wa_selection>-ptrhigh->*  TO <high>.
            <low> = <wa>-low.
            <high> = <wa>-high.
          ENDIF.
        ENDIF.
    ENDCASE.


  ENDMETHOD.


  METHOD it_msel.


    DATA :
      ptr_to_row TYPE REF TO data,
      prt_data   TYPE REF TO data,
      cfield(30) TYPE c,
      newpos     TYPE sytabix,
      linepos    TYPE sytabix.



    FIELD-SYMBOLS :
      <low>          TYPE any,
      <high>         TYPE any,
      <it_selection> TYPE zif_do_seo=>tt_selection_options,
      <wa_selection> TYPE LINE OF zif_do_seo=>tt_selection_options,
      <data>         TYPE any,
      <it_rowobject> TYPE ztt_do_filter,
      <wa_rowobject> TYPE LINE OF ztt_do_filter,
      <tc>           TYPE cxtab_control.


    ASSIGN :
      ptr_it_selection->*  TO <it_selection>,
      ptr_it_rowobject->* TO <it_rowobject>,
      ptr_tb_control->* TO <tc>.


    CLEAR :
      cfield,
      linepos.

    GET CURSOR FIELD cfield LINE linepos.
    IF sy-subrc = 0 AND linepos > 0.
      newpos = linepos + <tc>-top_line - 1.
      READ TABLE  <it_rowobject> ASSIGNING <wa_rowobject>
        INDEX newpos.
      IF sy-subrc = 0.
        GET REFERENCE OF <wa_rowobject> INTO ptr_to_row.
        CALL METHOD me->update_selection_from_filter
          EXPORTING
            im_ptr_to_row = ptr_to_row.
        IF <wa_rowobject>-rollname IS INITIAL.
          CASE <wa_rowobject>-inttype.  "See Domain INTTYPE.
            WHEN 'I'.             "I  Integer number (4-byte integer with sign)
              CREATE DATA:
                 prt_data TYPE i.
            WHEN 'g'.             "g  Character string with variable length (ABAP type STRING)
              CREATE DATA:
                 prt_data TYPE string.
          ENDCASE.
        ELSE.
          CREATE DATA prt_data TYPE (<wa_rowobject>-rollname).
        ENDIF.
        ASSIGN
           prt_data->* TO <data>.

        PERFORM ask_complex_selections
                    TABLES
                       <it_selection>
                    USING
                       <data>
                       <wa_rowobject>-tablename  "'MARA'
                       <wa_rowobject>-fieldname
                       <wa_rowobject>-fieldlabel.




        READ TABLE <it_selection> ASSIGNING <wa_selection>
          WITH KEY fieldname = <wa_rowobject>-fieldname.
        IF sy-subrc = 0.
          ASSIGN:
            <wa_selection>-ptrlow->* TO <low>,
            <wa_selection>-ptrhigh->* TO <high>.
          <wa_rowobject>-low = <low>.
          <wa_rowobject>-high = <high>.
        ELSE.
          CLEAR :
            <wa_rowobject>-low,
            <wa_rowobject>-high.
        ENDIF.
        CALL METHOD me->button_change
          EXPORTING
            im_ptr_to_row = ptr_to_row.


      ENDIF.

    ENDIF.



*  GET CURSOR FIELD cfield LINE linepos.
*  IF sy-subrc = 0 AND linepos > 0.
*    newpos = linepos + <tc>-top_line - 1.
  ENDMETHOD.


  METHOD it_selection_dialog.
    DATA:
      wa_selection   TYPE LINE OF zif_do_seo=>tt_selection_options.
    FIELD-SYMBOLS:
      <low>          TYPE any,
      <high>         TYPE any,
      <wa_selection> TYPE LINE OF zif_do_seo=>tt_selection_options,
      <it_selection> TYPE zif_do_seo=>tt_selection_options,
      <it_rowobject> TYPE  ztt_do_filter,
      <wa_rowobject> TYPE LINE OF  ztt_do_filter.


    ASSIGN :
      ptr_it_selection->*  TO <it_selection>,
      ptr_it_rowobject->* TO <it_rowobject>.

    READ TABLE <it_rowobject> WITH KEY rowmarked = 'X'
      TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      LOOP AT <it_rowobject> ASSIGNING <wa_rowobject>
        WHERE rowmarked = 'X'.
        READ TABLE <it_selection>
          WITH KEY fieldname = <wa_rowobject>-fieldname
          TRANSPORTING NO FIELDS.
        IF sy-subrc NE 0.
          wa_selection-fieldname = <wa_rowobject>-fieldname.
          APPEND wa_selection TO <it_selection>.
        ENDIF.
      ENDLOOP.
    ENDIF.
    READ TABLE <it_selection> INDEX 1
      TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      CALL FUNCTION 'ZTB_TOOLS_01_SELECTION'
        TABLES
          it_selection  = <it_selection>
          it_field_list = it_field_list
        EXCEPTIONS
          OTHERS        = 1.


      LOOP AT <it_rowobject> ASSIGNING <wa_rowobject>.
        READ TABLE <it_selection>  ASSIGNING <wa_selection>
          WITH KEY fieldname = <wa_rowobject>-fieldname.
        IF sy-subrc = 0.
          ASSIGN:
             <wa_selection>-ptrlow->* TO <low>,
             <wa_selection>-ptrhigh->* TO <high>.
          <wa_rowobject>-low = <low>.
          <wa_rowobject>-high = <high>.
        ELSE.
          CLEAR :
            <wa_rowobject>-low,
            <wa_rowobject>-high.
        ENDIF.

      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD button_change.

    DATA:
      ilines   TYPE i,
      icon(50) TYPE c.
    FIELD-SYMBOLS :
      <it_selection> TYPE zif_do_seo=>tt_selection_options,
      <wa_selection> TYPE LINE OF zif_do_seo=>tt_selection_options,
      <wa>           TYPE LINE OF ztt_do_filter.

    ASSIGN:
      ptr_it_selection->*    TO <it_selection>,
      im_ptr_to_row->*       TO <wa>.

*    CALL FUNCTION 'ZTB_TOOLS_01_RANGE_WA'
*         EXPORTING
*              wa_range = grg_material
*         TABLES
*              it_range = grg_material.

    LOOP AT <it_selection> ASSIGNING <wa_selection>
      WHERE fieldname = <wa>-fieldname.
      ilines = ilines + 1.
      IF ilines > 1.
        EXIT.
      ENDIF.
    ENDLOOP.



    IF ilines > 1.
      icon = 'ICON_DISPLAY_MORE'.
    ELSE.
      icon = 'ICON_ENTER_MORE'.
    ENDIF.


    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name                  = icon
*       TEXT                  = ' '
        info                  = 'Multiple Selection'
*       ADD_STDINF            = 'X'
      IMPORTING
        result                = <wa>-pb
      EXCEPTIONS
        icon_not_found        = 1
        outputfield_too_short = 2
        OTHERS                = 3.
  ENDMETHOD.

  METHOD function_code_initialize.

    DATA :
      cfcode TYPE sy-ucomm.



    CONCATENATE tc_name '_MSEL' INTO cfcode.
    CALL METHOD me->set_function_code
      EXPORTING
        im_function_code = cfcode
        im_method_to_use = 'IT_MSEL'.
  ENDMETHOD.

ENDCLASS.





DATA :
  gobj_filter    TYPE REF TO cl_filter,
  gobj_sort_cols TYPE REF TO cl_sort,
  gobj_cols_list TYPE REF TO cl_sort.


LOAD-OF-PROGRAM.
*  PERFORM create_screen_objects.
  PERFORM create_data_objects.

  g_repid = sy-repid.

* DECLARATION OF TABLECONTROL 'GTC_COLS_LIST' ITSELF
  CONTROLS: gtc_cols_list TYPE TABLEVIEW USING SCREEN 0280.

* DECLARATION OF TABLECONTROL 'GTC_FILTER' ITSELF
  CONTROLS: gtc_filter TYPE TABLEVIEW USING SCREEN 0220.

* INCLUDE LZTB_TOOLS_01D...                  " Local class definition
