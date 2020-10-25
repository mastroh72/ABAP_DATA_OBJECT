CLASS zcl_do_gt_salv_table DEFINITION
  PUBLIC
  INHERITING FROM zcl_do_gt_event
  CREATE PUBLIC ABSTRACT.

  PUBLIC SECTION.
    METHODS: generate_alv IMPORTING io_cont TYPE REF TO cl_gui_container.


  PROTECTED SECTION.
    DATA:
      mo_container  TYPE REF TO cl_gui_container,
      mo_columns    TYPE REF TO cl_salv_columns_table,
      mo_salv       TYPE REF TO cl_salv_table,
      mo_table      TYPE REF TO data,
      ms_layout_key TYPE salv_s_layout_key.

    METHODS db_select.


    METHODS:
      enable_layout_settings,
      set_optimized_column_width,
      set_toolbar,
      set_all_column_units,
      set_one_column_unit
        IMPORTING
          VALUE(iv_col_name)  TYPE lvc_fname
          VALUE(iv_unit_name) TYPE lvc_fname,
      set_all_column_currency,
      set_one_column_currency
        IMPORTING
          VALUE(iv_col_name)  TYPE lvc_fname
          VALUE(iv_curr_name) TYPE lvc_fname,
      set_hidden_columns,
      set_one_column_hidden
        IMPORTING
          VALUE(iv_col_name) TYPE lvc_fname,
      set_all_column_descriptions,
      set_one_column_description
        IMPORTING
          VALUE(iv_desc)     TYPE string
          VALUE(iv_col_name) TYPE lvc_fname,
      set_prefix_column_description
        IMPORTING
          VALUE(iv_prefix)   TYPE string
          VALUE(iv_col_name) TYPE lvc_fname,
      set_color_table_contents,
      set_color_table_field,
      set_events.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_do_gt_salv_table IMPLEMENTATION.


  METHOD generate_alv.

    FIELD-SYMBOLS: <lt_table> TYPE ANY TABLE.

    mo_container = io_cont.

    TRY.
        ASSIGN mo_table->* TO <lt_table>.
        IF sy-subrc EQ 0 AND mo_salv IS NOT BOUND.
          cl_salv_table=>factory(
            EXPORTING
              r_container  = io_cont
            IMPORTING
              r_salv_table = mo_salv
            CHANGING
              t_table      = <lt_table> ).
        ENDIF.
      CATCH cx_salv_msg .
    ENDTRY.

    IF mo_salv IS BOUND.
      TRY.
          me->enable_layout_settings( ).

          mo_columns = mo_salv->get_columns( ).

          me->set_optimized_column_width(  ).

          me->set_all_column_units( ).

          me->set_hidden_columns( ).

          me->set_all_column_descriptions( ).

          me->set_toolbar( ).

          me->set_events(  ).

          me->set_color_table_field(  ).
*
          me->mo_salv->display( ).
*
        CATCH cx_salv_msg .
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD set_one_column_hidden.
    DATA:
      not_found TYPE REF TO cx_salv_not_found,
      o_column  TYPE REF TO cl_salv_column.


    TRY.
        o_column = mo_columns->get_column( iv_col_name ).
        o_column->set_visible( if_salv_c_bool_sap=>false ).

      CATCH cx_salv_not_found INTO not_found.

    ENDTRY.
  ENDMETHOD.

  METHOD set_hidden_columns.
*    me->set_one_column_hidden( 'KNUMV' ).
  ENDMETHOD.

  METHOD set_prefix_column_description.
    DATA:
      not_found TYPE REF TO cx_salv_not_found,
      o_column  TYPE REF TO cl_salv_column.

    TRY.
        o_column = mo_columns->get_column( iv_col_name ).

        o_column->set_short_text( |{ iv_prefix } { o_column->get_short_text( ) }| ).
        o_column->set_medium_text( |{ iv_prefix } { o_column->get_medium_text( ) }| ).
        o_column->set_long_text( |{ iv_prefix } { o_column->get_long_text( ) }| ).

      CATCH cx_salv_not_found INTO not_found.

    ENDTRY.
  ENDMETHOD.

  METHOD set_one_column_description.
    DATA:
      not_found TYPE REF TO cx_salv_not_found,
      o_column  TYPE REF TO cl_salv_column.

    TRY.
        o_column = mo_columns->get_column( iv_col_name ).

        o_column->set_short_text( |{ iv_desc }| ).
        o_column->set_medium_text( |{ iv_desc }|  ).
        o_column->set_long_text( |{ iv_desc }|  ).

      CATCH cx_salv_not_found INTO not_found.

    ENDTRY.
  ENDMETHOD.

  METHOD set_events.



*    DATA:
*              lo_col_tab  TYPE REF TO cl_salv_column_table.
**
**
**   Get VBELN column
*    TRY.
*        lo_col_tab ?= mo_columns->get_column( 'VBELN' ).
*      CATCH cx_salv_not_found.
*    ENDTRY.
**
**   Set the HotSpot for VBELN Column
*    TRY.
*        CALL METHOD lo_col_tab->set_cell_type
*          EXPORTING
*            value = if_salv_c_cell_type=>hotspot.
*        .
*      CATCH cx_salv_data_error .
*    ENDTRY.
**
**...Events
*    DATA: lo_events TYPE REF TO cl_salv_events_table.
**
**   all events
*    lo_events = mo_salv->get_event( ).
*
**
**   event handler
**   SET HANDLER co_report->on_link_click FOR lo_events.


  ENDMETHOD.

  METHOD set_all_column_descriptions.

*    me->set_one_column_description( iv_prefix  = me->o_data->search_condition( ) iv_col_name = 'SEARCH_KSCHL' ).



*    DATA:
*      not_found TYPE REF TO cx_salv_not_found,
*      o_column  TYPE REF TO cl_salv_column.
*
*    TRY.
*        o_column = o_columns->get_column( 'NET_ACTIVE_COMP' ).
*        o_column->set_short_text( |{ text-004 }| ).
*        o_column->set_medium_text( |{ text-004 }| ).
*        o_column->set_long_text( |{ text-003 }| ).
*
*      CATCH cx_salv_not_found INTO not_found.
*    ENDTRY.


  ENDMETHOD.

  METHOD enable_layout_settings.

    DATA:
      lo_layout_settings TYPE REF TO cl_salv_layout,
      ls_layout_key      TYPE salv_s_layout_key.

    lo_layout_settings = mo_salv->get_layout( ).
    ls_layout_key = ms_layout_key.

    lo_layout_settings->set_key( ls_layout_key ).

    lo_layout_settings->set_save_restriction( if_salv_c_layout=>restrict_none ).
    lo_layout_settings->set_default( abap_true ).


  ENDMETHOD.

  METHOD set_toolbar.

    DATA:
    o_functions TYPE REF TO cl_salv_functions_list.

    o_functions = mo_salv->get_functions( ).
    o_functions->set_all( ).

*    o_functions->set_group_export( abap_false ).
*
*    o_functions->set_group_filter( abap_false ).
*
*    o_functions->set_group_sort( abap_false ).
*    o_functions->set_sort_asc( ).
*  add custom function
*    try.
*    o_functions->add_function(
*      EXPORTING
*        name               =     'POSS_UASGN'
*        icon               =   conv #( ICON_UNASSIGN )
**        text               =
*        tooltip            =   'Unassign Document as Shipment'
*        position           =   if_salv_c_function_position=>left_of_salv_functions   " Positioning Function
*    ).
*    catch cx_salv_existing cx_salv_wrong_call.
*endtry.
*


  ENDMETHOD.

  METHOD set_one_column_unit.

    DATA:
      data_error TYPE REF TO cx_salv_data_error,
      not_found  TYPE REF TO cx_salv_not_found,
      o_column   TYPE REF TO cl_salv_column.

    TRY.
        o_column = mo_columns->get_column( iv_col_name ).
        o_column->set_quantity_column(  iv_unit_name ).
      CATCH cx_salv_not_found  INTO not_found.
      CATCH cx_salv_data_error INTO data_error.
    ENDTRY.

  ENDMETHOD.

  METHOD set_all_column_units.
*    set_one_column_unit( iv_col_name = 'KWMENG' iv_unit_name = 'VRKME' ).
  ENDMETHOD.


  METHOD set_one_column_currency.

    DATA:
      data_error TYPE REF TO cx_salv_data_error,
      not_found  TYPE REF TO cx_salv_not_found,
      o_column   TYPE REF TO cl_salv_column.

    TRY.
        o_column = mo_columns->get_column( iv_col_name ).
        o_column->set_currency_column(  iv_curr_name ).
      CATCH cx_salv_not_found  INTO not_found.
      CATCH cx_salv_data_error INTO data_error.
    ENDTRY.

  ENDMETHOD.

  METHOD set_all_column_currency.
*    set_one_column_currency( iv_col_name = '' iv_curr_name = '' ).
  ENDMETHOD.

  METHOD set_color_table_contents.
*    DATA:
*          lo_col_tab  TYPE REF TO cl_salv_column_table.
*    DATA: ls_color TYPE lvc_s_colo.    " Colors strucutre
**
**   get Columns object
*
**
*    INCLUDE <color>.
**
**   Get ERDAT column & set the yellow Color fot it
*    TRY.
*        lo_col_tab ?= mo_columns->get_column( 'ERDAT' ).
*        ls_color-col = col_total.
*        lo_col_tab->set_color( ls_color ).
*      CATCH cx_salv_not_found.
*    ENDTRY.
**
**.......Color for Specific Cell & Rows.................
**   Applying color on the 3rd Row and Column AUART
**   Applying color on the Entire 5th Row
**
*    DATA: lt_s_color TYPE lvc_t_scol,
*          ls_s_color TYPE lvc_s_scol,
*          la_vbak    LIKE LINE OF ct_vbak,
*          l_count    TYPE i.
**
*    LOOP AT ct_vbak INTO la_vbak.
*      l_count = l_count + 1.
*      CASE l_count.
**       Apply RED color to the AUART Cell of the 3rd Row
*        WHEN 3.
*          ls_s_color-fname     = 'AUART'.
*          ls_s_color-color-col = col_negative.
*          ls_s_color-color-int = 0.
*          ls_s_color-color-inv = 0.
*          APPEND ls_s_color TO lt_s_color.
*          CLEAR  ls_s_color.
**
**       Apply GREEN color to the entire row # 5
**         For entire row, we don't pass the Fieldname
*        WHEN 5.
*          ls_s_color-color-col = col_positive.
*          ls_s_color-color-int = 0.
*          ls_s_color-color-inv = 0.
*          APPEND ls_s_color TO lt_s_color.
*          CLEAR  ls_s_color.
*      ENDCASE.
**     Modify that data back to the output table
*      la_vbak-t_color = lt_s_color.
*      MODIFY ct_vbak FROM la_vbak.
*      CLEAR  la_vbak.
*      CLEAR  lt_s_color.
*    ENDLOOP.
**


  ENDMETHOD.

  METHOD set_color_table_field.
**   We will set this COLOR table field name of the internal table to
**   COLUMNS tab reference for the specific colors
*        mo_columns->set_color_column( 'T_COLOR' ).
*      CATCH cx_salv_data_error.                         "#EC NO_HANDLER
*    ENDTRY.
  ENDMETHOD.

  METHOD db_select.
    me->set_color_table_contents(  ).
    IF me->mo_salv IS BOUND.
      me->mo_salv->refresh( ).
    ENDIF.
    RAISE EVENT screen_data_changed.
  ENDMETHOD.


  METHOD set_optimized_column_width.
    mo_columns->set_optimize(  ).
  ENDMETHOD.

ENDCLASS.
