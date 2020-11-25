CLASS zcl_do_it DEFINITION
  PUBLIC
  INHERITING FROM zcl_do
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPE-POOLS abap .
    TYPE-POOLS cxtab .
*  type-pools ZDO .
*  type-pools ZIF_DO_SEO=>ZSLOT .

    CONSTANTS co_invisible TYPE char1 VALUE 'X' ##NO_TEXT.
    CONSTANTS co_visible TYPE char1 VALUE space ##NO_TEXT.
    CONSTANTS co_tc_visible TYPE char1 VALUE '0' ##NO_TEXT.
    CONSTANTS co_tc_invisible TYPE char1 VALUE '1' ##NO_TEXT.
    CONSTANTS co_char_0 TYPE zif_do=>td_hex_seperator VALUE '00' ##NO_TEXT.
    CONSTANTS co_char_1 TYPE zif_do=>td_hex_seperator VALUE '01' ##NO_TEXT.
    CONSTANTS co_char_2 TYPE zif_do=>td_hex_seperator VALUE '02' ##NO_TEXT.
    CONSTANTS co_false TYPE char1 VALUE space ##NO_TEXT.
    CONSTANTS co_field_list TYPE char1 VALUE ',' ##NO_TEXT.
    CONSTANTS co_link_reposition TYPE zdo_object_relationship VALUE 'REPOS' ##NO_TEXT.
    CONSTANTS co_link_data TYPE zdo_object_relationship VALUE 'DATA' ##NO_TEXT.
    CONSTANTS co_link_data_filter TYPE zdo_object_relationship VALUE 'DATAFILTER' ##NO_TEXT.
    CONSTANTS co_mandt TYPE fieldname VALUE 'MANDT' ##NO_TEXT.
    CONSTANTS co_mod_create TYPE zdo_rowmod VALUE 'C' ##NO_TEXT.
    CONSTANTS co_mod_delete TYPE zdo_rowmod VALUE 'D' ##NO_TEXT.
    CONSTANTS co_mod_empty TYPE zdo_rowmod VALUE 'E' ##NO_TEXT.
    CONSTANTS co_mod_original TYPE zdo_rowmod VALUE 'O' ##NO_TEXT.
    CONSTANTS co_mod_update TYPE zdo_rowmod VALUE 'U' ##NO_TEXT.
    CONSTANTS co_row_db_cnt TYPE fieldname VALUE 'ROWDBCNT' ##NO_TEXT.
    CONSTANTS co_row_filter TYPE fieldname VALUE 'ROWFILTEROUT' ##NO_TEXT.
    CONSTANTS co_row_marker TYPE fieldname VALUE 'ROWMARKED' ##NO_TEXT.
    CONSTANTS co_row_mod TYPE fieldname VALUE 'ROWMOD' ##NO_TEXT.
    CONSTANTS co_row_seq TYPE fieldname VALUE 'ROWSEQ' ##NO_TEXT.
    CONSTANTS co_tab TYPE zif_do=>td_hex_seperator VALUE '09' ##NO_TEXT.
    CONSTANTS co_true TYPE char1 VALUE 'X' ##NO_TEXT.
    DATA ptr_db_tb_where TYPE REF TO zcl_do_selection_options READ-ONLY .
    DATA ptr_ro_tb_mark TYPE REF TO zcl_do_selection_options READ-ONLY .
    DATA ptr_ro_tb_repos TYPE REF TO zcl_do_selection_options READ-ONLY .
    DATA ptr_ro_tb_where TYPE REF TO zcl_do_selection_options READ-ONLY .
    DATA db_rows_selected TYPE sydbcnt READ-ONLY .
    DATA ro_tb_rows TYPE sydbcnt READ-ONLY .

    EVENTS work_area_changed .

    CLASS-METHODS class_constructor .
    METHODS check_function_code
      CHANGING
        !ch_function_code TYPE sy-ucomm .
    METHODS class_set_main_object
      IMPORTING
        !im_object_id TYPE zdo_object_id OPTIONAL .
    METHODS constructor
      IMPORTING
        VALUE(im_it_table_type) TYPE typename OPTIONAL
        VALUE(im_table_name)    TYPE tabname OPTIONAL
        VALUE(im_db_table_type) TYPE typename OPTIONAL
        !im_ptr_to_table        TYPE REF TO data
        !im_ptr_to_wa           TYPE REF TO data OPTIONAL
        !im_ptr_to_tc           TYPE REF TO data OPTIONAL
        !im_tc_name             TYPE typename OPTIONAL
        !im_ptr_to_tc_lines     TYPE REF TO data OPTIONAL .
    METHODS db_exists
      RETURNING
        VALUE(re_exist) TYPE abap_bool .
    METHODS db_fields .
    METHODS db_fields_from_rowdb .
    METHODS db_fields_make_aggregate
      IMPORTING
        VALUE(im_aggregate_expression) TYPE char20 DEFAULT 'SUM'
        !im_fieldname                  TYPE fieldname .
    METHODS db_get_current_ver_of_upd_rows .
    METHODS db_get_more_rows_exist
      EXPORTING
        !ex_more_rows_exist TYPE char1 .
    METHODS db_get_rowdb
      RETURNING
        VALUE(ex_rowdb) TYPE REF TO data .
    METHODS db_lock .
    METHODS db_message_hits .
    METHODS db_read_only
      IMPORTING
        !im_read_only TYPE char1 .
    METHODS db_save .
    METHODS db_select
      EXPORTING
        !ex_more_rows_exist TYPE char1 .
    METHODS db_set_max_hits
      IMPORTING
        !im_max_hits TYPE int4 DEFAULT '0' .
    METHODS db_set_reposition_on_wa
      IMPORTING
        !im_active TYPE char1 .
    METHODS db_set_rowdb
      IMPORTING
        !im_ptr_to_table TYPE REF TO data .
    METHODS db_set_sync_on
      IMPORTING
        !im_active TYPE char1 .
    METHODS db_transaction .
    METHODS db_unlock .
    METHODS db_where .
    METHODS db_where_from_ptr_db_where .
    METHODS get_rowobject
      EXPORTING
        VALUE(ex_rowobject) TYPE ANY TABLE .
    METHODS it_alv .
    METHODS it_alv_get_variant
      EXPORTING
        VALUE(ex_variant) TYPE slis_vari .
    METHODS it_alv_set_program
      IMPORTING
        !im_program TYPE progname
        !im_screen  TYPE dynpronr OPTIONAL
        !im_variant TYPE slis_vari OPTIONAL .
    METHODS it_alv_set_variant
      IMPORTING
        !im_variant TYPE slis_vari .
    METHODS it_alv_tc_fields_only .
    METHODS it_alv_user_command
      IMPORTING
        !im_ucomm    TYPE sy-ucomm
        !im_selfield TYPE slis_selfield .
    METHODS it_append
      IMPORTING
        !im_lines TYPE sydbcnt DEFAULT 1 .
    METHODS it_compare .
    METHODS it_copy
      IMPORTING
        !im_line TYPE sy-tabix DEFAULT '0' .
    METHODS it_cursor
      CHANGING
        !ch_set_position TYPE char1
        !ch_field        TYPE fieldname
        !ch_offset       TYPE num4
        !ch_line         TYPE int4 .
    METHODS it_delete
      IMPORTING
        VALUE(im_line) TYPE sy-tabix DEFAULT '0' .
    METHODS it_demark_all .
    METHODS it_filter
      IMPORTING
        VALUE(im_publish_work_area_changed) TYPE char1 DEFAULT 'X' .
    METHODS it_filter_ask .
    METHODS it_find
      IMPORTING
        !im_method          TYPE sy-ucomm
      CHANGING
        !ch_start_from_line TYPE sy-tabix .
    METHODS it_force_integrity .
    METHODS it_function_code
      IMPORTING
        VALUE(im_main_object) TYPE char1 DEFAULT space
      CHANGING
        !ch_function_code     TYPE sy-ucomm .
    METHODS it_get_marked_rows .
    METHODS it_insert
      IMPORTING
        !im_line   TYPE sy-tabix
      EXPORTING
        !ex_db_cnt TYPE sydbcnt .
    METHODS it_mark_all .
    METHODS it_mark_block .
    METHODS it_mark_filter .
    METHODS it_mark_filter_ask .
    METHODS it_move_bottom .
    METHODS it_move_down .
    METHODS it_move_top .
    METHODS it_move_up .
    METHODS it_print .
    METHODS it_replace .
    METHODS it_selected .
    METHODS it_set_filter_auto_disable
      IMPORTING
        !im_auto_disable TYPE char1 DEFAULT space .
    METHODS it_set_reposition_on
      IMPORTING
        !im_active TYPE char1 .
    METHODS it_sort .
    METHODS it_sort_ask
      IMPORTING
        VALUE(im_include_row_object) TYPE char1 DEFAULT space .
    METHODS it_update
      IMPORTING
        !im_db_cnt TYPE sydbcnt .
    METHODS it_validate .
    METHODS lk_add_link
      IMPORTING
        !im_ptr_publisher   TYPE REF TO zcl_do_it
        !im_ptr_subscriber  TYPE REF TO zcl_do_it OPTIONAL
        !im_relationship    TYPE zdo_object_relationship
        !im_it_relationship TYPE ztt_do_relationship OPTIONAL .
    METHODS lk_deactivate_all_publishers
      IMPORTING
        VALUE(im_relationship) TYPE zdo_object_relationship DEFAULT zcl_do_it=>co_link_data .
    METHODS lk_set_active_publisher_id
      IMPORTING
        !im_publisher_id       TYPE zdo_object_id
        VALUE(im_relationship) TYPE zdo_object_relationship DEFAULT zcl_do_it=>co_link_data .
    METHODS on_work_area_changed
          FOR EVENT work_area_changed OF zcl_do_it
      IMPORTING
          !sender .
    METHODS set_function_code
      IMPORTING
        !im_function_code TYPE sy-ucomm
        !im_method_to_use TYPE rs38l_fnam
        !im_update_only   TYPE char1 DEFAULT space .
    METHODS set_rowobject .
    METHODS tc_append
      IMPORTING
        !im_lines TYPE sydbcnt DEFAULT 1 .
    METHODS tc_change_attribute .
    METHODS tc_change_group
      IMPORTING
        VALUE(im_group) TYPE scrfgrp1
        !im_screen_mode TYPE char1 .
    METHODS tc_choose .
    METHODS tc_column_filter .
    METHODS tc_column_mark_filter .
    METHODS tc_column_sort
      IMPORTING
        !im_method TYPE sy-ucomm .
    METHODS tc_copy .
    METHODS tc_delete .
    METHODS tc_find
      IMPORTING
        !im_method TYPE sy-ucomm .
    METHODS tc_free .
    METHODS tc_initialize .
    METHODS tc_insert .
    METHODS tc_refresh
      IMPORTING
        VALUE(im_publish_work_area_changed) TYPE char1 DEFAULT 'X' .
    METHODS tc_refresh_if_needed .
    METHODS tc_scroll
      IMPORTING
        !im_method TYPE sy-ucomm .
    METHODS tc_set_col
      IMPORTING
        !im_name            TYPE fieldname DEFAULT space
        VALUE(im_group)     TYPE char3 DEFAULT space
        VALUE(im_invisible) TYPE char1 DEFAULT space .
    METHODS tc_set_col_attribute
      IMPORTING
        !im_name        TYPE fieldname DEFAULT space
        !im_group       TYPE char3 DEFAULT space
        !im_required    TYPE char1 DEFAULT space
        !im_input       TYPE char1 DEFAULT space
        !im_output      TYPE char1 DEFAULT space
        !im_intensified TYPE char1 DEFAULT space
        !im_invisible   TYPE char1 DEFAULT space
        !im_length      TYPE int4 DEFAULT space
        !im_active      TYPE char1 DEFAULT space
        !im_display_3d  TYPE char1 DEFAULT space
        !im_value_help  TYPE char1 DEFAULT space
        !im_request     TYPE char1 DEFAULT space
        !im_color       TYPE i DEFAULT 0 .
    METHODS tc_set_disable_current_row
      IMPORTING
        !im_disable TYPE char1 .
    METHODS tc_set_displayed_lines
      IMPORTING
        !im_lines  TYPE sy-loopc
        !im_db_cnt TYPE zdo_rowdbcnt DEFAULT 0 .
    METHODS tc_set_intensify_current_row
      IMPORTING
        !im_intensify TYPE char1 .
    METHODS tc_set_keep_position
      IMPORTING
        !im_keep_position TYPE char1 DEFAULT 'X' .
    METHODS tc_set_row_attributes
      IMPORTING
        !im_name        TYPE fieldname DEFAULT space
        !im_group       TYPE char3 DEFAULT space
        !im_required    TYPE char1 DEFAULT space
        !im_input       TYPE char1 DEFAULT space
        !im_output      TYPE char1 DEFAULT space
        !im_intensified TYPE char1 DEFAULT space
        !im_invisible   TYPE char1 DEFAULT space
        !im_length      TYPE int4 DEFAULT space
        !im_active      TYPE char1 DEFAULT space
        !im_display_3d  TYPE char1 DEFAULT space
        !im_value_help  TYPE char1 DEFAULT space
        !im_request     TYPE char1 DEFAULT space
        !im_color       TYPE i DEFAULT 0 .
    METHODS tc_use_labels_from_screen
      IMPORTING
        !im_program TYPE progname
        !im_screen  TYPE dynpronr .
    METHODS wa_from_it
      IMPORTING
        VALUE(im_db_cnt) TYPE sydbcnt DEFAULT '0' .
    METHODS wa_get_field_value
      CHANGING
        !ch_it_field_values TYPE zif_do_seo=>tt_field_values .
    METHODS wa_raise_work_area_changed .
    METHODS wa_to_it
      IMPORTING
        VALUE(im_row_mod) TYPE zdo_rowmod DEFAULT space .
  PROTECTED SECTION.

    CLASS-DATA main_fcode_object_id TYPE zdo_object_id .
    CLASS-DATA it_zrowobject_structure TYPE spar_dfies .
    DATA db_more_rows_exist TYPE char1 .
    DATA db_reposition_on_wa_changed TYPE char1 VALUE space ##NO_TEXT.
    DATA db_sync_where_on_wa_changed TYPE char1 VALUE space ##NO_TEXT.
    DATA db_tb_it_type TYPE char72 .
    DATA db_tb_name TYPE tabname .
    DATA db_refresh_needed TYPE char1 .
    DATA it_db_tb_fields TYPE ztt_do_columns .
    DATA it_db_tb_keys TYPE spar_dfies .
    DATA it_db_tb_group_by TYPE ztt_do_columns .
    DATA it_db_tb_order_by TYPE ztt_do_columns .
    DATA it_db_tb_structure TYPE spar_dfies .
    DATA it_db_tb_where TYPE ztt_do_columns .
    DATA it_fcodes TYPE zif_do=>tt_function_codes .
    DATA it_maps TYPE zif_do=>tt_id_field_mapping .
    DATA it_reposition_on_wa_changed TYPE char1 VALUE space ##NO_TEXT.
    DATA it_ro_tb_find_fields TYPE ztt_do_tb_field_to_search .
    DATA it_ro_tb_marked_rows TYPE zif_do=>tt_markedrows .
    DATA it_ro_tb_sort_columns_prev TYPE ztt_do_sort_columns .
    DATA it_ro_tb_sort_columns TYPE ztt_do_sort_columns .
    DATA it_ro_tb_structure TYPE spar_dfies .
    DATA i_rows_to_select_rowdb TYPE int4 VALUE '0' ##NO_TEXT.
    DATA i_ro_tb_next_dbcnt TYPE int4 .
    DATA lg_distinct_rowdb TYPE char1 .
    DATA lg_append_rowdb TYPE char1 .
    DATA lg_copy_cursor TYPE char1 .
    DATA lg_copy_marked TYPE char1 VALUE 'X' ##NO_TEXT.
    DATA lg_group_by_rowdb TYPE char1 .
    DATA lg_corresponding_rowdb TYPE char1 .
    DATA lg_delete_cursor TYPE char1 .
    DATA lg_delete_marked TYPE char1 VALUE 'X' ##NO_TEXT.
    DATA lg_readonly TYPE char1 .
    DATA lg_auto_disable_filter TYPE char1 VALUE 'X' ##NO_TEXT.
    DATA lg_db_do_select TYPE char1 VALUE 'X' ##NO_TEXT.
    DATA ptr_it_db_tb_current TYPE REF TO data .
    DATA ptr_it_db_tb_delete TYPE REF TO data .
    DATA ptr_it_db_tb_update TYPE REF TO data .
    DATA ptr_it_rowdb TYPE REF TO data .
    DATA ptr_it_rowobject TYPE REF TO data .
    DATA ptr_it_rowobjupd TYPE REF TO data .
    DATA ptr_tb_control TYPE REF TO data .
    DATA ptr_wa_rowobject TYPE REF TO data .
    DATA ptr_wa_rowobjupd TYPE REF TO data .
    DATA ro_tb_default_cursor_field TYPE fieldname .
    DATA ro_tb_it_type TYPE typename .
    DATA ro_tb_multi_sort_allowed TYPE char1 VALUE 'X' ##NO_TEXT.
    DATA ro_tb_st_type TYPE typename .
    DATA ro_tb_name TYPE fieldname .
    DATA ro_tb_search_parms TYPE zst_do_searchparms .
    DATA tc_current_rowdbcnt TYPE zdo_rowdbcnt .
    DATA tc_disable_current_row TYPE char1 VALUE space ##NO_TEXT.
    DATA tc_displayed_lines TYPE sy-loopc .
    DATA tc_intensify_current_row TYPE char1 .
    DATA tc_lg_keep_position TYPE char1 VALUE space ##NO_TEXT.
    DATA tc_lg_add_blank_rows TYPE char1 VALUE 'X' ##NO_TEXT.
    DATA tc_name TYPE typename .
    DATA wa_db_tb_fields TYPE zst_do_columns .
    DATA wa_db_tb_order TYPE zst_do_columns .
    DATA wa_db_tb_where TYPE zst_do_where_clause .
    DATA wa_ro_tb_default_cursor_field TYPE fieldname .
    DATA it_ro_tb_alv_fieldcat TYPE slis_t_fieldcat_alv .
    DATA it_ro_tb_alv_sort TYPE slis_t_sortinfo_alv .
    DATA it_ro_tb_alv_layout TYPE slis_layout_alv .
    DATA it_ro_tb_alv_print TYPE slis_print_alv .
    DATA ro_tb_alv_program TYPE sy-repid .
    DATA ro_tb_alv_variant TYPE disvariant .
    DATA ro_tb_alv_events TYPE slis_t_event .
    DATA ro_tb_alv_subtotal TYPE char1 VALUE space ##NO_TEXT.
    DATA ro_tb_alv_expand TYPE char1 VALUE space ##NO_TEXT.
    DATA current_ucomm TYPE sy-ucomm .

    METHODS db_after_save .
    METHODS db_compare .
    METHODS db_initial_loop_rowdb .
    METHODS db_rowobjupd_to_rowdb .
    METHODS db_rowobjupd_to_rowdb_refresh .
    METHODS function_code_initialize .
    METHODS it_alv_override .
    METHODS it_data_filter_exit .
    METHODS it_refresh .
    METHODS it_refresh_and_notify .
    METHODS it_row_created
      IMPORTING
        !im_ptr_to_row TYPE REF TO data .
    METHODS it_row_deleted
      IMPORTING
        !im_ptr_to_row TYPE REF TO data .
    METHODS it_row_empty
      IMPORTING
        !im_ptr_to_row TYPE REF TO data .
    METHODS it_row_loaded
      IMPORTING
        !im_ptr_to_row TYPE REF TO data .
    METHODS it_row_loaded_begin .
    METHODS it_row_loaded_end .
    METHODS it_row_map_assign
      IMPORTING
        !im_ptr_to_row TYPE REF TO data .
    METHODS it_row_updated
      IMPORTING
        !im_ptr_to_row TYPE REF TO data .
    METHODS it_row_validate
      IMPORTING
        !im_ptr_to_row TYPE REF TO data .
    METHODS it_sort_assign_row_seq .
    METHODS load_it_main_table_fields
      IMPORTING
        !im_field_list TYPE string .
    METHODS ru_update
      IMPORTING
        !im_ptr_to_row TYPE REF TO data .
    METHODS tc_first_input_field .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_do_it IMPLEMENTATION.


  METHOD check_function_code.
* ...

    FIELD-SYMBOLS
        <wa_fcodes> TYPE zif_do=>ts_function_codes.
    READ TABLE it_fcodes ASSIGNING <wa_fcodes>
      WITH TABLE KEY fcode = ch_function_code.
    IF sy-subrc = 0.
      CALL METHOD me->(<wa_fcodes>-use_method).
      CLEAR ch_function_code.
    ENDIF.

  ENDMETHOD.


  METHOD class_constructor .
* ...

    CALL FUNCTION 'GET_FIELDTAB'
      EXPORTING
        langu               = sy-langu
        tabname             = 'ZST_DO_ROWOBJECT'
        withtext            = 'X'
      TABLES
        fieldtab            = it_zrowobject_structure
      EXCEPTIONS
        internal_error      = 1
        no_texts_found      = 2
        table_has_no_fields = 3
        table_not_activ     = 4
        OTHERS              = 5.
  ENDMETHOD.


  METHOD class_set_main_object.
* ...
    IF im_object_id IS INITIAL.
      main_fcode_object_id = me->id.
    ELSE.
      main_fcode_object_id = im_object_id.
    ENDIF.

  ENDMETHOD.


  METHOD constructor .
* ...
    DATA :
      wa_fieldcat   LIKE LINE OF it_ro_tb_alv_fieldcat,
      ilinecnt      TYPE i,
      wa_linedesc   TYPE ddtypelist,
      l_temp(160)   TYPE c,
      l_field(160)  TYPE c,
      it_ddicfields TYPE ddfields,
*    it_rowobjfields    TYPE spar_dfies,
      ls_stru       TYPE REF TO cl_abap_structdescr,
      ls_table      TYPE REF TO cl_abap_tabledescr,
      lt_comp       TYPE STANDARD TABLE OF abap_componentdescr,
      wa_cols       TYPE LINE OF cxtab_control-cols.

    FIELD-SYMBOLS :
      <wa_ro_tb_alv_feildcat>  LIKE wa_fieldcat,
      <wa_ro_tb_structure>     TYPE LINE OF spar_dfies,
      <wa_rowobjfields>        TYPE dfies,
      <cols>                   TYPE STANDARD TABLE,
      <tc>                     TYPE cxtab_control,
      <wa_linedesc>            TYPE ddtypedesc,
      <wa_cols>                TYPE LINE OF cxtab_control-cols,
*      <wa_linedesc_fields> TYPE ddtypedesc,

      <wa_main_table_fieldtab> TYPE dfies.

    CALL METHOD super->constructor.

    ro_tb_it_type = im_it_table_type.
    db_tb_name = im_table_name.



* Send Reference to Internal Table to get RTTS (Runtime Type Services) info for it.
* Get the definition of table type
    ls_table ?= cl_abap_tabledescr=>describe_by_data_ref( im_ptr_to_table ).
* Get the structure used by table type.
    ls_stru ?= ls_table->get_table_line_type(  ).

* GEt the structure Components
*    DATA(it_components) = ls_stru->get_components( ).
* Build a Dynamic Structure Type for the structure based on components
*    DATA(r_structdesc) = cl_abap_structdescr=>get( it_components ).
* Build a Dynamic Table Type for the based on the dynamic structure created
*    DATA(r_tabledesc) = cl_abap_tabledescr=>get( r_structdesc ).

    IF ls_stru->is_ddic_type(  ) = abap_true.
      it_ddicfields = ls_stru->get_ddic_field_list( p_langu = sy-langu p_including_substructres = abap_true ).
    ELSE.
*     refresh it_components.
*       it_components[] = r_structdesc->get_components( ).
      CALL FUNCTION 'Z_DO_TB_TOOLS_01_DDFIELDS'
        EXPORTING
          im_structure  = ls_stru
        IMPORTING
          it_ddicfields = it_ddicfields
        EXCEPTIONS
          error         = 1.
    ENDIF.





    IF im_db_table_type IS INITIAL.
      db_tb_it_type = im_it_table_type.
    ELSE.
      db_tb_it_type = im_db_table_type.
    ENDIF.
    tc_name = im_tc_name.
    TRANSLATE :
      ro_tb_it_type TO UPPER CASE,
      db_tb_name TO UPPER CASE.
*    CONCATENATE 'TYPE STANDARD TABLE OF'
*      main_table
*      INTO it_main_table_type SEPARATED BY space.
    IF NOT db_tb_it_type IS INITIAL.
      CREATE DATA ptr_it_db_tb_update TYPE (db_tb_it_type).
      CREATE DATA ptr_it_db_tb_current TYPE (db_tb_it_type).
      CREATE DATA ptr_it_db_tb_delete  TYPE (db_tb_it_type).
    ELSE.
      CREATE DATA ptr_it_db_tb_update TYPE HANDLE ls_table. "r_tabledesc.
      CREATE DATA ptr_it_db_tb_current TYPE HANDLE ls_table. "r_tabledesc.
      CREATE DATA ptr_it_db_tb_delete   TYPE HANDLE ls_table. "r_tabledesc.
    ENDIF.

    IF NOT ro_tb_it_type IS INITIAL.
      CREATE DATA ptr_it_rowobjupd TYPE (ro_tb_it_type).
      CREATE DATA ptr_it_rowdb TYPE (ro_tb_it_type).
    ELSE.
      CREATE DATA ptr_it_rowobjupd TYPE HANDLE ls_table. "r_tabledesc.
      CREATE DATA ptr_it_rowdb TYPE HANDLE ls_table. " r_tabledesc.
    ENDIF.

    ptr_it_rowobject = im_ptr_to_table.
    ptr_wa_rowobject = im_ptr_to_wa.

    CALL METHOD load_it_main_table_fields
      EXPORTING
        im_field_list = '*'.

*    it_main_table_fields
*    CREATE DATA ptr_it_rowobject TYPE (my_it_type).
    it_ro_tb_structure[] = it_ddicfields[].

** We are getting sent in a Table type.
*    CALL FUNCTION 'GET_FIELDTAB'
*      EXPORTING
*        langu               = sy-langu
*        tabname             = ro_tb_it_type
*        withtext            = 'X'
*      TABLES
*        fieldtab            = it_ro_tb_structure
*      EXCEPTIONS
*        internal_error      = 1
*        no_texts_found      = 2
*        table_has_no_fields = 3
*        table_not_activ     = 4
*        OTHERS              = 5.
** Table table then needs to get the structure of the table type.
*    DESCRIBE TABLE it_ro_tb_structure LINES ilinecnt.
*    IF ilinecnt = 0.
*      CALL FUNCTION 'DDIF_FIELDINFO_GET'
*        EXPORTING
*          tabname        = ro_tb_it_type
*          langu          = sy-langu
*          all_types      = 'X'
*        IMPORTING
*          lines_descr    = wa_linedesc
*        EXCEPTIONS
*          not_found      = 1
*          internal_error = 2
*          OTHERS         = 3.
*
*      READ TABLE wa_linedesc ASSIGNING <wa_linedesc> INDEX 1.
*
*      IF sy-subrc = 0.
*        it_ro_tb_structure[] = <wa_linedesc>-fields[].
*      ENDIF.
*
**      assign
**        wa_linedesc-fields to  .
**      append lines of wa_linedesc-fields to it_fieldtab.
**      it_fieldtab = wa_linedesc-fields.
*    ENDIF.

    CALL FUNCTION 'GET_FIELDTAB'
      EXPORTING
        langu               = sy-langu
        tabname             = db_tb_name
        withtext            = 'X'
      TABLES
        fieldtab            = it_db_tb_structure
      EXCEPTIONS
        internal_error      = 1
        no_texts_found      = 2
        table_has_no_fields = 3
        table_not_activ     = 4
        OTHERS              = 5.




    DATA(it_minus_rowobject) = it_ro_tb_structure[].
    DELETE it_minus_rowobject WHERE fieldname = 'MANDT'.
    LOOP AT it_minus_rowobject ASSIGNING FIELD-SYMBOL(<wa_minus_rowobject>)
        WHERE fieldname NE zcl_do_it=>co_row_marker.
      READ TABLE it_zrowobject_structure WITH KEY fieldname = <wa_minus_rowobject>-fieldname TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        DELETE it_minus_rowobject.
      ENDIF.
    ENDLOOP.

    CREATE OBJECT  :
      ptr_ro_tb_repos
        EXPORTING im_it_valid_fields = it_ro_tb_structure,
     ptr_ro_tb_where
        EXPORTING im_it_valid_fields = it_minus_rowobject,
      ptr_db_tb_where
        EXPORTING im_it_valid_fields = it_db_tb_structure.

    DELETE it_minus_rowobject WHERE fieldname = zcl_do_it=>co_row_marker.

    CREATE OBJECT ptr_ro_tb_mark
      EXPORTING
        im_it_valid_fields = it_minus_rowobject.





    CALL METHOD:
      ptr_ro_tb_mark->set_ask_filter_on
        EXPORTING
          im_ask_filter_on = space,
       ptr_ro_tb_where->set_ask_filter_on
        EXPORTING
          im_ask_filter_on = 'X',
      ptr_db_tb_where->set_ask_filter_on
        EXPORTING
          im_ask_filter_on = space.

    LOOP AT  it_ro_tb_structure ASSIGNING <wa_ro_tb_structure>.
*      wa_fieldcat-col_pos      = <wa_cols>-index.
*      SPLIT <wa_cols>-screen-name AT '-' INTO l_temp l_field.
*      wa_fieldcat-outputlen     =  <wa_cols>-vislength.
      wa_fieldcat-fieldname = <wa_ro_tb_structure>-fieldname.
      wa_fieldcat-outputlen = <wa_ro_tb_structure>-outputlen.
      wa_fieldcat-seltext_l = <wa_ro_tb_structure>-scrtext_l.
      wa_fieldcat-seltext_m = <wa_ro_tb_structure>-scrtext_l.
      wa_fieldcat-seltext_s = <wa_ro_tb_structure>-scrtext_l.
      CASE <wa_ro_tb_structure>-datatype.
        WHEN 'CURR' OR 'QUAN'.
          wa_fieldcat-do_sum = 'X'.
      ENDCASE.
*    wa_fieldcat-no_out = 'X'.
      APPEND wa_fieldcat TO it_ro_tb_alv_fieldcat.
    ENDLOOP.

    LOOP AT it_zrowobject_structure ASSIGNING <wa_rowobjfields>.
      READ TABLE it_ro_tb_structure
        WITH KEY fieldname = <wa_rowobjfields>-fieldname
          TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        DELETE it_ro_tb_structure INDEX sy-tabix.
        READ TABLE  it_ro_tb_alv_fieldcat
          ASSIGNING <wa_ro_tb_alv_feildcat>
          WITH KEY fieldname = <wa_rowobjfields>-fieldname.
        IF sy-subrc = 0.
          <wa_ro_tb_alv_feildcat>-no_out = 'X'.
          <wa_ro_tb_alv_feildcat>-tech   = 'X'.
        ENDIF.
      ENDIF.
    ENDLOOP.

    LOOP AT it_db_tb_structure
         ASSIGNING <wa_main_table_fieldtab>
         WHERE keyflag = 'X' AND fieldname NE 'MANDT'.

      APPEND <wa_main_table_fieldtab> TO it_db_tb_keys.
    ENDLOOP.

    ptr_tb_control = im_ptr_to_tc.
*  ptr_tb_control_lines = im_ptr_to_tc_lines.

*  DATA m TYPE c.
*  IF ptr_tb_control IS INITIAL.
*    m = '1'.
*  ELSE.
*    m = '2'.
*  ENDIF.

    REFRESH it_ro_tb_find_fields.

    READ TABLE it_ro_tb_structure ASSIGNING <wa_ro_tb_structure>
      INDEX 1.
    IF sy-subrc = 0.
      ro_tb_st_type = <wa_ro_tb_structure>-tabname.
    ENDIF.

    IF NOT ptr_tb_control IS INITIAL.

      ASSIGN :
        ptr_tb_control->* TO <tc>,
        <tc>-cols TO <cols>.

      READ TABLE <cols> INTO wa_cols INDEX 1.
      IF sy-subrc = 0.
        ro_tb_default_cursor_field = wa_cols-screen-name.
        l_field = wa_cols-screen-name.
        SPLIT l_field AT '-' INTO l_temp l_field.
        ro_tb_name = l_temp.
        WHILE l_field CS '-'.
          SPLIT l_field AT '-' INTO l_temp l_field.
          CONCATENATE l_temp ro_tb_name
            INTO ro_tb_name SEPARATED BY '-'.
        ENDWHILE.
      ENDIF.

      LOOP AT <cols> ASSIGNING <wa_cols>.

*         SCREEN      LIKE SCREEN,     "Attributes struktur SCREEN
*         INDEX       TYPE I,         "Position of a column on the
*screen
*         SELECTED(1) TYPE C,          "Indicator 'column selected'
*         VISLENGTH   LIKE ICON-OLENG, "Visualised length of a column
*         INVISIBLE(1) TYPE C,         "Indicator 'column invisible'

        SPLIT <wa_cols>-screen-name AT '-' INTO l_temp l_field.
        READ TABLE it_ro_tb_alv_fieldcat
           ASSIGNING <wa_ro_tb_alv_feildcat>
           WITH KEY fieldname = l_field.
        IF sy-subrc = 0.
          <wa_ro_tb_alv_feildcat>-col_pos      = <wa_cols>-index.
          <wa_ro_tb_alv_feildcat>-no_out       = space.
        ENDIF.
*      wa_fieldcat-fieldname     = l_field.
*      wa_fieldcat-outputlen     =  <wa_cols>-vislength.
*      READ TABLE it_ro_tb_structure ASSIGNING <wa_ro_tb_structure>
*        WITH KEY fieldname = l_field.
*      IF sy-subrc = 0.
*        wa_fieldcat-outputlen = <wa_ro_tb_structure>-outputlen.
*        wa_fieldcat-seltext_l = <wa_ro_tb_structure>-scrtext_l.
*        wa_fieldcat-seltext_m = <wa_ro_tb_structure>-scrtext_l.
*        wa_fieldcat-seltext_s = <wa_ro_tb_structure>-scrtext_l.
*        CASE <wa_ro_tb_structure>-datatype.
*          WHEN 'CURR' OR 'QUAN'.
*            wa_fieldcat-do_sum = 'X'.
*        ENDCASE.
*      ENDIF.
*      APPEND wa_fieldcat TO it_ro_tb_alv_fieldcat.
      ENDLOOP.

*    READ TABLE it_ro_tb_structure INDEX 1 ASSIGNING <wa_rowobjfields>.
*    IF sy-subrc = 0.
*
*      CONCATENATE ro_tb_name <wa_rowobjfields>-fieldname
*        INTO ro_tb_default_cursor_field SEPARATED BY '-'.
*      TRANSLATE ro_tb_default_cursor_field  TO UPPER CASE.
*
*    ENDIF.

      CALL FUNCTION 'Z_DO_TB_TOOLS_01_SEARCH_MAP'
        EXPORTING
          cxtab_control    = <tc>
        TABLES
          fields_to_search = it_ro_tb_find_fields.
    ENDIF.

    CALL METHOD db_read_only
      EXPORTING
        im_read_only = 'X'.

    CALL METHOD me->function_code_initialize.

    ro_tb_alv_program =  sy-repid.

*  change_label 'CU_VGW' text-055.    "Weight
*  change_label 'CU_VGE' text-055.    "Footage
*  change_label 'CU_BME' text-055.    "Weight UM
*  change_label 'CU_MEINS' text-055.  "Footage UM

  ENDMETHOD.


  METHOD db_after_save.
* ...
    FIELD-SYMBOLS:
      <it_rowobjupd> TYPE STANDARD TABLE.
*
    ASSIGN:
      ptr_it_rowobjupd->* TO <it_rowobjupd>.
*
*
*
    REFRESH <it_rowobjupd>.

    CALL METHOD:
      me->db_select,
      me->it_filter,
      me->tc_initialize.



  ENDMETHOD.


  METHOD db_compare.
* ...

    DATA :
      c_sort_field1 TYPE fieldname,
      c_sort_field2 TYPE fieldname,
      c_sort_field3 TYPE fieldname,
      c_sort_field4 TYPE fieldname,
      c_sort_field5 TYPE fieldname,
      c_sort_field6 TYPE fieldname,
      c_sort_field7 TYPE fieldname,
      c_match       TYPE c,
      i_from        TYPE i VALUE 0.

    FIELD-SYMBOLS :
      <it_current>    TYPE STANDARD TABLE,
      <it_updated>    TYPE STANDARD TABLE,
      <wa_keyfields>  TYPE dfies,
      <wa_fieldtab>   TYPE dfies,
      <wa_current>    TYPE any,
      <wa_updated>    TYPE any,
      <field_updated> TYPE any,
      <field_current> TYPE any.

    ASSIGN :
      ptr_it_db_tb_update->*  TO  <it_updated>,
      ptr_it_db_tb_current->* TO  <it_current>.

*I could sort by key fields on both tables
*then uses 2 loops with the from option leaving off where the last
*where the last one was found
    LOOP AT  it_db_tb_keys ASSIGNING <wa_keyfields>.
      CASE sy-tabix.
        WHEN 1.
          c_sort_field1 = <wa_keyfields>-fieldname.
        WHEN 2.
          c_sort_field2 = <wa_keyfields>-fieldname.
        WHEN 3.
          c_sort_field3 = <wa_keyfields>-fieldname.
        WHEN 4.
          c_sort_field4 = <wa_keyfields>-fieldname.
        WHEN 5.
          c_sort_field5 = <wa_keyfields>-fieldname.
        WHEN 6.
          c_sort_field6 = <wa_keyfields>-fieldname.
        WHEN 7.
          c_sort_field7 = <wa_keyfields>-fieldname.
      ENDCASE.
    ENDLOOP.

    SORT <it_updated>
      BY (c_sort_field1) (c_sort_field2) (c_sort_field3)
         (c_sort_field4) (c_sort_field5) (c_sort_field6)
         (c_sort_field7).

    SORT <it_current>
      BY (c_sort_field1) (c_sort_field2) (c_sort_field3)
         (c_sort_field4) (c_sort_field5) (c_sort_field6)
         (c_sort_field7).

*    READ TABLE it_keyfields ASSIGNING <wa_keyfields> INDEX 1.
*    IF sy-subrc NE 0.
*      SORT <it_updated> BY (<wa_keyfields>-fieldname).
*      SORT <it_current> BY (<wa_keyfields>-fieldname).
*    ENDIF.
*    LOOP AT it_keyfields ASSIGNING <wa_keyfields> FROM 2.
*      SORT <it_updated> BY sy-tabix (<wa_keyfields>-fieldname).
*      SORT <it_current> BY sy-tabix (<wa_keyfields>-fieldname).
*    ENDLOOP.

    LOOP AT <it_updated> ASSIGNING <wa_updated>.

      LOOP AT <it_current> ASSIGNING <wa_current> FROM i_from.

        CLEAR : c_match.
        c_match = 'X'.

        LOOP AT it_db_tb_keys ASSIGNING <wa_keyfields>.
          ASSIGN :
            COMPONENT <wa_keyfields>-fieldname
              OF STRUCTURE <wa_updated> TO <field_updated>,
            COMPONENT <wa_keyfields>-fieldname
              OF STRUCTURE <wa_current> TO  <field_current>.
          IF <field_updated> NE <field_current>.
            CLEAR c_match.
            EXIT.
          ENDIF.
        ENDLOOP.

        IF c_match = 'X'.
          i_from = sy-tabix.
*Compare all fields
          LOOP AT it_db_tb_structure ASSIGNING <wa_fieldtab>.
            ASSIGN :
              COMPONENT <wa_fieldtab>-fieldname
                OF STRUCTURE <wa_updated> TO <field_updated>,
              COMPONENT <wa_fieldtab>-fieldname
                OF STRUCTURE <wa_current> TO  <field_current>.
            IF <field_updated> NE <field_current>.
              CLEAR c_match.
              EXIT.
            ENDIF.
          ENDLOOP.
          EXIT.
        ELSE.
* Record was deleted before my updated
        ENDIF.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD db_exists.
    FIELD-SYMBOLS :
        <it_rowdb>     TYPE STANDARD TABLE.

    ASSIGN :
        ptr_it_rowdb->*  TO  <it_rowdb> .

    IF line_exists( <it_rowdb>[ 1 ] ).
      re_exist = abap_true.
    ELSE.
      re_exist = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD db_fields.
* ...
  ENDMETHOD.


  METHOD db_fields_from_rowdb.
* ...

    DATA:
      wa_db_tb_fields LIKE LINE OF it_db_tb_fields,
      c_skipped       TYPE char1,
      c_not_found     TYPE char1.

    FIELD-SYMBOLS:
      <wa_db_tb_structure> LIKE LINE OF it_db_tb_structure,
      <wa_ro_tb_structure> LIKE LINE OF it_ro_tb_structure.

    REFRESH it_db_tb_fields.

    LOOP AT it_ro_tb_structure
         ASSIGNING <wa_ro_tb_structure>.
      READ TABLE it_db_tb_structure ASSIGNING <wa_db_tb_structure>
        WITH KEY fieldname =  <wa_ro_tb_structure>-fieldname.
      IF sy-subrc = 0.
        IF c_not_found = 'X'.
          c_skipped = 'X'.
        ENDIF.
        wa_db_tb_fields-fieldname =  <wa_ro_tb_structure>-fieldname.
        APPEND wa_db_tb_fields TO it_db_tb_fields.
      ELSE.
        c_not_found = 'X'.
      ENDIF.
    ENDLOOP.

    IF c_skipped = 'X'.
      lg_corresponding_rowdb = 'X'.
    ELSE.
      CLEAR lg_corresponding_rowdb.
    ENDIF.

  ENDMETHOD.


  METHOD db_fields_make_aggregate.
* ...

    DATA :
      i_len TYPE i.

    FIELD-SYMBOLS:
      <wa_db_tb_fields> LIKE LINE OF it_db_tb_fields.

    READ TABLE it_db_tb_fields ASSIGNING <wa_db_tb_fields>
      WITH KEY fieldname = im_fieldname.
    IF sy-subrc = 0.
      CONCATENATE im_aggregate_expression '( '
        INTO <wa_db_tb_fields>-fieldname.
      CONCATENATE <wa_db_tb_fields>-fieldname im_fieldname
          ') AS' im_fieldname
        INTO <wa_db_tb_fields>-fieldname SEPARATED BY space.
    ENDIF.

  ENDMETHOD.


  METHOD db_get_current_ver_of_upd_rows.
* ...

* Build where for each updated record
    DATA :
      ilines            TYPE i,
      ptr_wa_main_table TYPE REF TO data,
      firstpass         TYPE c,
      iloopcnt          TYPE i,
      i_last_or         TYPE i,
      ikeycnt           TYPE i,
      it_updated_where  TYPE ztt_do_where_clause,
      wa_updated_where  TYPE LINE OF ztt_do_where_clause.

    FIELD-SYMBOLS :
      <field1>                 TYPE any,
      <field2>                 TYPE any,
      <wa_fieldtab>            TYPE dfies,
      <wa_main_table>          TYPE any,
      <it_updated>             TYPE STANDARD TABLE,
      <it_delete>              TYPE STANDARD TABLE,
      <rowmod>                 TYPE any,
      <field>                  TYPE any,
      <wa_rowobjupd>           TYPE any,
      <wa_main_table_fieldtab> TYPE dfies,
      <it_rowobjupd>           TYPE STANDARD TABLE,
      <it_current>             TYPE STANDARD TABLE.

    CREATE DATA ptr_wa_main_table TYPE (db_tb_name).

    ASSIGN :
      ptr_wa_main_table->*         TO <wa_main_table>,
      ptr_it_db_tb_update->*       TO <it_updated>,
      ptr_it_db_tb_delete->*       TO <it_delete>,
      ptr_it_rowobjupd->*          TO <it_rowobjupd>,
      ptr_it_db_tb_current->*      TO <it_current>.

    LOOP AT it_db_tb_structure
         ASSIGNING <wa_main_table_fieldtab>
         WHERE keyflag = 'X' AND fieldname NE 'MANDT'.

      ikeycnt = ikeycnt + 1.
    ENDLOOP.

    REFRESH :
       <it_current>,
      <it_updated>,
      <it_delete>.

    firstpass = 'X'.
    LOOP AT <it_rowobjupd> ASSIGNING <wa_rowobjupd>.

      ASSIGN
          COMPONENT zcl_do_it=>co_row_mod
            OF STRUCTURE <wa_rowobjupd>
            TO <rowmod>.

      CLEAR <wa_main_table>.
      LOOP AT it_db_tb_structure ASSIGNING <wa_fieldtab>.
        ASSIGN :
          COMPONENT <wa_fieldtab>-fieldname
            OF STRUCTURE <wa_main_table>
            TO <field1>,
          COMPONENT <wa_fieldtab>-fieldname
            OF STRUCTURE <wa_rowobjupd>
            TO <field2>.
        <field1> = <field2>.
      ENDLOOP.

      IF <rowmod> = zcl_do_it=>co_mod_delete.
        APPEND <wa_main_table> TO <it_delete>.
      ELSE.
        APPEND <wa_main_table> TO <it_updated>.
      ENDIF.

      IF <rowmod> = zcl_do_it=>co_mod_update. " update
*      IF firstpass = 'X'.
*        CLEAR firstpass.
*      ELSE.
*        wa_updated_where-where_condition = ' OR'.
*        APPEND wa_updated_where TO it_updated_where.
*      ENDIF.
        iloopcnt  = 1.
        LOOP AT it_db_tb_structure
             ASSIGNING <wa_main_table_fieldtab>
             WHERE keyflag = 'X' AND fieldname NE 'MANDT'.
          IF iloopcnt  = 1.
            wa_updated_where-where_condition = '('.
          ELSE.
            CLEAR : wa_updated_where.
          ENDIF.
          ASSIGN
            COMPONENT <wa_main_table_fieldtab>-fieldname
              OF STRUCTURE <wa_rowobjupd>
              TO <field>.
          CASE <wa_main_table_fieldtab>-inttype. "DATATYPE
*          WHEN 'I'.
*          WHEN 'C' OR 'c'.
            WHEN OTHERS.
              CONCATENATE
                 wa_updated_where-where_condition
                 <wa_main_table_fieldtab>-fieldname
                ' = '''
                INTO wa_updated_where-where_condition
                SEPARATED BY space.
              CONCATENATE
                 wa_updated_where-where_condition
                <field>
                ''''
                INTO wa_updated_where-where_condition.
          ENDCASE.
          IF iloopcnt NE ikeycnt.
            CONCATENATE
             wa_updated_where-where_condition
             ' AND'
             INTO  wa_updated_where-where_condition
             SEPARATED BY space.
          ELSE.
            CONCATENATE
             wa_updated_where-where_condition
             ' )'
             INTO  wa_updated_where-where_condition
             SEPARATED BY space.
          ENDIF.
          APPEND wa_updated_where TO it_updated_where.
          iloopcnt = iloopcnt + 1.
        ENDLOOP.

*      wa_updated_where-where_condition = ' OR'.
*      APPEND wa_updated_where TO it_updated_where.
*      i_last_or = sy-tabix.
      ENDIF.
      DESCRIBE TABLE it_updated_where LINES ilines.
      IF ilines > 0.
        SELECT *
              FROM (db_tb_name)
              APPENDING TABLE <it_current>
              WHERE (it_updated_where).
        REFRESH it_updated_where.
      ENDIF.
*    firstpass = 'X'.
*    ENDIF.
    ENDLOOP.

*  DESCRIBE TABLE it_updated_where LINES ilines.
*  IF ilines > 0.
*    SELECT *
*          FROM (db_tb_name)
*          APPENDING TABLE <it_current>
*          WHERE (it_updated_where).
*    REFRESH it_updated_where..
*  ENDIF.

*  IF i_last_or > 0.
*    DELETE it_updated_where INDEX i_last_or.
*  ENDIF.

*  SELECT *
*        FROM (db_tb_name)
*        INTO TABLE <it_current>
*        WHERE (it_updated_where).

*Loop threw the updated fields list for each row numb and
*compare those fields

  ENDMETHOD.


  METHOD db_get_more_rows_exist.
* ...
    ex_more_rows_exist = db_more_rows_exist.
  ENDMETHOD.


  METHOD db_get_rowdb.
    ex_rowdb = me->ptr_it_rowdb.
  ENDMETHOD.


  METHOD db_initial_loop_rowdb.
* ...
    DATA:
      imaps      TYPE i,
      ptr_to_row TYPE REF TO data.

    FIELD-SYMBOLS :
      <it_rowdb> TYPE STANDARD TABLE,
      <wa_rowdb> TYPE any,
      <rowmod>   TYPE any,
      <rowdbcnt> TYPE any.

    CALL METHOD it_row_loaded_begin.

    ASSIGN ptr_it_rowdb->* TO <it_rowdb>.


    DATA:
      lg_dbcnt_to_zero TYPE abap_bool VALUE abap_true,
      loop_where       TYPE string VALUE 'ROWDBCNT > 0'.

    LOOP AT <it_rowdb> ASSIGNING <wa_rowdb> WHERE (loop_where).
      lg_dbcnt_to_zero = abap_false.
      EXIT.
    ENDLOOP.

    IF lg_dbcnt_to_zero = abap_true.
      i_ro_tb_next_dbcnt = 1.
    ENDIF.

    READ TABLE it_maps
      WITH KEY active = 'X'
               relationship = zcl_do_it=>co_link_data
      TRANSPORTING NO FIELDS.

    IF sy-subrc = 0.

      LOOP AT <it_rowdb> ASSIGNING <wa_rowdb>.
        ASSIGN:
          COMPONENT zcl_do_it=>co_row_mod OF STRUCTURE <wa_rowdb>
                TO <rowmod>,
         COMPONENT zcl_do_it=>co_row_db_cnt OF STRUCTURE <wa_rowdb>
                TO <rowdbcnt>.
        <rowmod> = zcl_do_it=>co_mod_original. "Orginal Copy
        IF <rowdbcnt> = 0.
          <rowdbcnt> = i_ro_tb_next_dbcnt. "Give a unique id for row
          i_ro_tb_next_dbcnt = i_ro_tb_next_dbcnt + 1.
        ENDIF.
        GET REFERENCE OF <wa_rowdb> INTO ptr_to_row .

        CALL METHOD:
          it_row_map_assign
            EXPORTING
                im_ptr_to_row = ptr_to_row,
          it_row_loaded
            EXPORTING
              im_ptr_to_row = ptr_to_row.

      ENDLOOP.
    ELSE.
      LOOP AT <it_rowdb> ASSIGNING <wa_rowdb>.
        ASSIGN:
          COMPONENT zcl_do_it=>co_row_mod OF STRUCTURE <wa_rowdb>
                TO <rowmod>,
         COMPONENT zcl_do_it=>co_row_db_cnt OF STRUCTURE <wa_rowdb>
                TO <rowdbcnt>.
        <rowmod> = zcl_do_it=>co_mod_original. "Orginal Copy
        IF <rowdbcnt> = 0.
          <rowdbcnt> = i_ro_tb_next_dbcnt. "Give a unique id for row
          i_ro_tb_next_dbcnt = i_ro_tb_next_dbcnt + 1.
        ENDIF.
        GET REFERENCE OF <wa_rowdb> INTO ptr_to_row .

        CALL METHOD it_row_loaded
          EXPORTING
            im_ptr_to_row = ptr_to_row.
      ENDLOOP.

    ENDIF.

    CALL METHOD it_row_loaded_end.

  ENDMETHOD.


  METHOD db_lock.
* ...
  ENDMETHOD.


  METHOD db_message_hits.
* ...
    IF db_rows_selected = 0.
      MESSAGE s001(zdo) WITH db_rows_selected.
    ELSEIF db_more_rows_exist = 'X'.
      MESSAGE s002(zdo) WITH i_rows_to_select_rowdb.
    ELSE.
      MESSAGE s003(zdo) WITH db_rows_selected.
    ENDIF.
  ENDMETHOD.


  METHOD db_read_only.
* ...
    lg_readonly = im_read_only.
  ENDMETHOD.


  METHOD db_rowobjupd_to_rowdb.

*    DATA:
*       rowmarked TYPE zdo_rowmarked.

    DATA:
       prev_rowmod TYPE zdo_rowmod.

    FIELD-SYMBOLS:
      <rowmod>       TYPE any,
      <rowmod_rowdb> TYPE any,
      <rowdbcnt>     TYPE any,
*      <rowmarked>    TYPE any,
      <wa_rowobjupd> TYPE any,
      <wa_rowdb>     TYPE any,
      <it_rowobjupd> TYPE STANDARD TABLE,
      <it_rowdb>     TYPE STANDARD TABLE.

    ASSIGN:
      ptr_it_rowdb->* TO <it_rowdb>,
      ptr_it_rowobjupd->* TO <it_rowobjupd>.


    LOOP AT <it_rowobjupd> ASSIGNING <wa_rowobjupd>.
      ASSIGN:
          COMPONENT zcl_do_it=>co_row_mod
            OF STRUCTURE <wa_rowobjupd>
            TO <rowmod>,
          COMPONENT zcl_do_it=>co_row_db_cnt
            OF STRUCTURE <wa_rowobjupd>
            TO <rowdbcnt>.
*          COMPONENT zcl_do_it=>co_row_marker
*            OF STRUCTURE <wa_rowobjupd>
*            TO <rowmarked>.

*     rowmarked = <rowmarked>.
*      CLEAR: <rowmarked>.


      CASE <rowmod>.
        WHEN zcl_do_it=>co_mod_create.
          prev_rowmod =  <rowmod>.
          <rowmod> = zcl_do_it=>co_mod_original.
          APPEND <wa_rowobjupd> TO <it_rowdb>.
          <rowmod> =  prev_rowmod.
        WHEN zcl_do_it=>co_mod_update.
          READ TABLE <it_rowdb> ASSIGNING <wa_rowdb>
            WITH KEY (zcl_do_it=>co_row_db_cnt) = <rowdbcnt>.
          IF sy-subrc = 0.
            ASSIGN:
                COMPONENT zcl_do_it=>co_row_mod
                OF STRUCTURE <wa_rowdb>
                TO <rowmod_rowdb>.
            <wa_rowdb> = <wa_rowobjupd>.
            <rowmod_rowdb> = zcl_do_it=>co_mod_original.
          ENDIF.
        WHEN zcl_do_it=>co_mod_delete.
*          delete <it_rowdb> where (zcl_do_it=>co_row_db_cnt) = <rowdbcnt>.
* Determine it the delete is from key change.  If so don't delete.
          READ TABLE  <it_rowobjupd> TRANSPORTING NO FIELDS
                    WITH KEY (zcl_do_it=>co_row_db_cnt) = <rowdbcnt>
                             (zcl_do_it=>co_row_mod) = zcl_do_it=>co_mod_update.
          IF sy-subrc NE 0.
            READ TABLE <it_rowdb> WITH KEY (zcl_do_it=>co_row_db_cnt) = <rowdbcnt>
             TRANSPORTING NO FIELDS.
            IF sy-subrc = 0.
              DELETE <it_rowdb> INDEX sy-tabix.
            ENDIF.
          ENDIF.

      ENDCASE.

*      <rowmarked> = rowmarked.
    ENDLOOP.





  ENDMETHOD.


  METHOD db_rowobjupd_to_rowdb_refresh.

    FIELD-SYMBOLS:
      <it_rowobjupd> TYPE STANDARD TABLE.

    db_rowobjupd_to_rowdb( ).


    ASSIGN:
      ptr_it_rowobjupd->* TO <it_rowobjupd>.

    REFRESH <it_rowobjupd>.




  ENDMETHOD.


  METHOD db_save.
* ...

    FIELD-SYMBOLS :
      <it_updated> TYPE STANDARD TABLE,
      <it_delete>  TYPE STANDARD TABLE.

    ASSIGN :
      ptr_it_db_tb_update->* TO <it_updated>,
      ptr_it_db_tb_delete->* TO <it_delete>.

    MODIFY (db_tb_name) FROM TABLE <it_updated>.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.
    DELETE (db_tb_name) FROM TABLE <it_delete>.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.
    COMMIT WORK.

    db_rowobjupd_to_rowdb( ).
  ENDMETHOD.


  METHOD db_select.
* ...

    DATA :
      ilines          TYPE i,
      irows_to_select TYPE int4.
*    ptr_to_row       TYPE REF TO data.

    FIELD-SYMBOLS :
        <it_rowdb>     TYPE STANDARD TABLE.
*      <wa_rowdb>     TYPE ANY,
*      <rowmod>       TYPE ANY,
*      <rowdbcnt>     TYPE ANY.

    ASSIGN ptr_it_rowdb->* TO <it_rowdb>.

    IF i_rows_to_select_rowdb > 0.
      irows_to_select = i_rows_to_select_rowdb + 1.
    ELSE.
      irows_to_select = 0.
    ENDIF.

    IF lg_db_do_select = 'X'.
      IF lg_distinct_rowdb = 'X'.

        IF lg_append_rowdb = 'X'.
          IF lg_corresponding_rowdb = 'X'.
            IF lg_group_by_rowdb = 'X'.
              SELECT DISTINCT (it_db_tb_fields)
                FROM (db_tb_name) UP TO irows_to_select ROWS
                APPENDING CORRESPONDING FIELDS OF TABLE <it_rowdb>
                WHERE (it_db_tb_where)
                GROUP BY (it_db_tb_group_by)
                ORDER BY (it_db_tb_order_by).
            ELSE.
              SELECT DISTINCT (it_db_tb_fields)
                FROM (db_tb_name) UP TO irows_to_select ROWS
                APPENDING CORRESPONDING FIELDS OF TABLE <it_rowdb>
                WHERE (it_db_tb_where)
                ORDER BY (it_db_tb_order_by).
            ENDIF.
          ELSE.
            IF lg_group_by_rowdb = 'X'.
              SELECT DISTINCT (it_db_tb_fields)
                FROM (db_tb_name) UP TO irows_to_select ROWS
                APPENDING TABLE <it_rowdb>
                WHERE (it_db_tb_where)
                GROUP BY (it_db_tb_group_by)
                ORDER BY (it_db_tb_order_by).
            ELSE.
              SELECT DISTINCT (it_db_tb_fields)
                FROM (db_tb_name) UP TO irows_to_select ROWS
                APPENDING TABLE <it_rowdb>
                WHERE (it_db_tb_where)
                ORDER BY (it_db_tb_order_by).
            ENDIF.
          ENDIF.
        ELSE.
          CALL METHOD me->it_refresh.
          IF lg_corresponding_rowdb = 'X'.
            IF lg_group_by_rowdb = 'X'.
              SELECT DISTINCT (it_db_tb_fields)
                FROM (db_tb_name) UP TO irows_to_select ROWS
                INTO CORRESPONDING FIELDS OF TABLE <it_rowdb>
                WHERE (it_db_tb_where)
                GROUP BY (it_db_tb_group_by)
                ORDER BY (it_db_tb_order_by).
            ELSE.
              SELECT DISTINCT (it_db_tb_fields)
                FROM (db_tb_name) UP TO irows_to_select ROWS
                INTO CORRESPONDING FIELDS OF TABLE <it_rowdb>
                WHERE (it_db_tb_where)
                ORDER BY (it_db_tb_order_by).
            ENDIF.
          ELSE.
            IF lg_group_by_rowdb = 'X'.
              SELECT DISTINCT (it_db_tb_fields)
                FROM (db_tb_name) UP TO irows_to_select ROWS
                INTO TABLE <it_rowdb>
                WHERE (it_db_tb_where)
                GROUP BY (it_db_tb_group_by)
                ORDER BY (it_db_tb_order_by).
            ELSE.
              SELECT DISTINCT (it_db_tb_fields)
                FROM (db_tb_name) UP TO irows_to_select ROWS
                INTO TABLE <it_rowdb>
                WHERE (it_db_tb_where)
                ORDER BY (it_db_tb_order_by).
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.

        IF lg_append_rowdb = 'X'.
          IF lg_corresponding_rowdb = 'X'.
            IF lg_group_by_rowdb = 'X'.
              SELECT (it_db_tb_fields)
                FROM (db_tb_name) UP TO irows_to_select ROWS
                APPENDING CORRESPONDING FIELDS OF TABLE <it_rowdb>
                WHERE (it_db_tb_where)
                GROUP BY (it_db_tb_group_by)
                ORDER BY (it_db_tb_order_by).
            ELSE.
              SELECT (it_db_tb_fields)
                FROM (db_tb_name) UP TO irows_to_select ROWS
                APPENDING CORRESPONDING FIELDS OF TABLE <it_rowdb>
                WHERE (it_db_tb_where)
                ORDER BY (it_db_tb_order_by).
            ENDIF.
          ELSE.
            IF lg_group_by_rowdb = 'X'.
              SELECT (it_db_tb_fields)
                FROM (db_tb_name) UP TO irows_to_select ROWS
                APPENDING TABLE <it_rowdb>
                WHERE (it_db_tb_where)
                GROUP BY (it_db_tb_group_by)
                ORDER BY (it_db_tb_order_by).
            ELSE.
              SELECT (it_db_tb_fields)
                FROM (db_tb_name) UP TO irows_to_select ROWS
                APPENDING TABLE <it_rowdb>
                WHERE (it_db_tb_where)
                ORDER BY (it_db_tb_order_by).
            ENDIF.
          ENDIF.
        ELSE.
          CALL METHOD me->it_refresh.
          IF lg_corresponding_rowdb = 'X'.
            IF lg_group_by_rowdb = 'X'.
              SELECT (it_db_tb_fields)
                FROM (db_tb_name) UP TO irows_to_select ROWS
                INTO CORRESPONDING FIELDS OF TABLE <it_rowdb>
                WHERE (it_db_tb_where)
                GROUP BY (it_db_tb_group_by)
                ORDER BY (it_db_tb_order_by).
            ELSE.
              SELECT (it_db_tb_fields)
                FROM (db_tb_name) UP TO irows_to_select ROWS
                INTO CORRESPONDING FIELDS OF TABLE <it_rowdb>
                WHERE (it_db_tb_where)
                ORDER BY (it_db_tb_order_by).
            ENDIF.
          ELSE.
            IF lg_group_by_rowdb = 'X'.
              SELECT (it_db_tb_fields)
                FROM (db_tb_name) UP TO irows_to_select ROWS
                INTO TABLE <it_rowdb>
                WHERE (it_db_tb_where)
                GROUP BY (it_db_tb_group_by)
                ORDER BY (it_db_tb_order_by).
            ELSE.
              SELECT (it_db_tb_fields)
                FROM (db_tb_name) UP TO irows_to_select ROWS
                INTO TABLE <it_rowdb>
                WHERE (it_db_tb_where)
                ORDER BY (it_db_tb_order_by).
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      db_rows_selected = sy-dbcnt.
    ELSE.
      DESCRIBE TABLE <it_rowdb> LINES  db_rows_selected.
    ENDIF.


    IF i_rows_to_select_rowdb > 0.
      irows_to_select = i_rows_to_select_rowdb + 1.
      DESCRIBE TABLE <it_rowdb> LINES ilines.
      IF ilines > i_rows_to_select_rowdb.
        DELETE <it_rowdb> INDEX ilines.
        db_more_rows_exist = 'X'.
      ELSE.
        db_more_rows_exist = space.
      ENDIF.
    ELSE.
      db_more_rows_exist = space.
    ENDIF.

    CALL METHOD me->db_initial_loop_rowdb.

    ex_more_rows_exist = db_more_rows_exist.
  ENDMETHOD.


  METHOD db_set_max_hits .
* ...

    i_rows_to_select_rowdb = im_max_hits.
  ENDMETHOD.


  METHOD db_set_reposition_on_wa.
* ...

    IF im_active = 'X'.
      db_sync_where_on_wa_changed = 'X'.
      db_reposition_on_wa_changed = 'X'.
    ELSE.
      db_reposition_on_wa_changed = space.
    ENDIF.
  ENDMETHOD.


  METHOD db_set_rowdb.

    DATA:
       ls_table      TYPE REF TO cl_abap_tabledescr.

    lg_db_do_select =  space.



    ls_table ?= cl_abap_tabledescr=>describe_by_data_ref( im_ptr_to_table ).

    FIELD-SYMBOLS:
*        <it_rowobject> TYPE STANDARD TABLE,
      <it_rowobjupd> TYPE STANDARD TABLE,
      <it_rowdb>     TYPE ANY TABLE,
      <im_it_rowdb>  TYPE ANY TABLE,
      <wa>           TYPE any.

    CREATE DATA ptr_it_rowdb TYPE HANDLE ls_table.

    ASSIGN:
      ptr_wa_rowobject->* TO <wa>,
*        ptr_it_rowobject->* to <it_rowobject>,
      ptr_it_rowobjupd->* TO <it_rowobjupd>,
      ptr_it_rowdb->*  TO  <it_rowdb>,
      im_ptr_to_table->* TO <im_it_rowdb>.

    <it_rowdb> = <im_it_rowdb>.

    REFRESH:
    <it_rowobjupd>.

    CLEAR :
      <wa>,
      tc_current_rowdbcnt.

    i_ro_tb_next_dbcnt = 1.

* Need set number lines to fix filter issues  DB_ROWS_SELECTED
*      DESCRIBE TABLE <it_rowdb> lines db_rows_selected.

*      CALL METHOD me->db_initial_loop_rowdb.

    db_refresh_needed = 'X'.



* MAPSS
    IF line_exists( it_maps[ relationship = zcl_do_it=>co_link_data_filter ] ).
      DATA it_fieldname TYPE ztt_do_fieldname.
      LOOP AT it_maps ASSIGNING FIELD-SYMBOL(<wa_maps>)
        WHERE relationship = zcl_do_it=>co_link_data_filter
          AND active = 'X'.
        LOOP AT <wa_maps>-map ASSIGNING FIELD-SYMBOL(<wa_map>).
          APPEND <wa_map>-subscribed_field TO it_fieldname.
        ENDLOOP.
      ENDLOOP.
      CALL METHOD me->ptr_ro_tb_where->delete_condition_exclude
        EXPORTING
          im_it_field_name = it_fieldname.
    ELSE.
      " Remove any Filters that where applied
      CALL METHOD me->ptr_ro_tb_where->delete_condition.
    ENDIF.
    CALL METHOD me->tc_refresh_if_needed.

*      call method:
*            me->it_filter,
*      me->tc_initialize.

  ENDMETHOD.


  METHOD db_set_sync_on .
* ...
    db_sync_where_on_wa_changed = im_active.
  ENDMETHOD.


  METHOD db_transaction.
* ...

    CALL METHOD me->it_compare.

    CALL METHOD me->db_lock.

    CALL METHOD me->db_get_current_ver_of_upd_rows.

    CALL METHOD me->db_compare.

    CALL METHOD me->db_save.

    CALL METHOD me->db_unlock.

    CALL METHOD me->db_after_save.

  ENDMETHOD.


  METHOD db_unlock.
* ...
  ENDMETHOD.


  METHOD db_where.
* ...
  ENDMETHOD.


  METHOD db_where_from_ptr_db_where.
* ...

    CALL METHOD ptr_db_tb_where->get_database_where
      IMPORTING
        ex_it_where_clause = it_db_tb_where.
  ENDMETHOD.


  METHOD function_code_initialize.
* ...

    DATA :
      cfcode TYPE sy-ucomm.

* Setup Function Codes To Respond To
*  CONCATENATE tc_name '_TC_FRSH' INTO cfcode.
    cfcode =  'TC_RFSH'.
    CALL METHOD me->set_function_code
      EXPORTING
        im_function_code = cfcode
        im_method_to_use = 'TC_REFRESH'.

*Detail
*  CONCATENATE tc_name '_DET' INTO cfcode.
    cfcode =  'CHOOSE'.
    CALL METHOD me->set_function_code
      EXPORTING
        im_function_code = cfcode
        im_method_to_use = 'TC_CHOOSE'.

*  CONCATENATE tc_name '_DET' INTO cfcode.
    CALL METHOD me->set_function_code
      EXPORTING
        im_function_code = 'DET' "cfcode
        im_method_to_use = 'TC_CHOOSE'.

*Insert
*  CONCATENATE tc_name '_INSR' INTO cfcode.
    CALL METHOD me->set_function_code
      EXPORTING
        im_function_code = 'INSR' "cfcode
        im_method_to_use = 'TC_INSERT'
        im_update_only   = 'X'.

*Delete
*  CONCATENATE tc_name '_DELE' INTO cfcode.
    CALL METHOD me->set_function_code
      EXPORTING
        im_function_code = 'DELE' "cfcode
        im_method_to_use = 'TC_DELETE'
        im_update_only   = 'X'.

*Copy
*  CONCATENATE tc_name '_COPY' INTO cfcode.
    CALL METHOD me->set_function_code
      EXPORTING
        im_function_code = 'COPY' " cfcode
        im_method_to_use = 'TC_COPY'
        im_update_only   = 'X'.

*Print
*  CONCATENATE tc_name '_PRT' INTO cfcode.
    CALL METHOD me->set_function_code
      EXPORTING
        im_function_code = 'PRT' "cfcode
        im_method_to_use = 'IT_PRINT'.

*Mark All
*  CONCATENATE tc_name '_MARK' INTO cfcode.
    CALL METHOD me->set_function_code
      EXPORTING
        im_function_code = 'MARK' " cfcode
        im_method_to_use = 'IT_MARK_ALL'.

    CALL METHOD me->set_function_code
      EXPORTING
        im_function_code = 'BMARK' " cfcode
        im_method_to_use = 'IT_MARK_BLOCK'.

*Mark With Filter
*  CONCATENATE tc_name '_FMARK' INTO cfcode.
    CALL METHOD me->set_function_code
      EXPORTING
        im_function_code = 'FMARK' "cfcode
        im_method_to_use = 'IT_MARK_FILTER_ASK'.

*DeMark All
*  CONCATENATE tc_name '_DMRK' INTO cfcode.
    CALL METHOD me->set_function_code
      EXPORTING
        im_function_code = 'DMRK' "cfcode
        im_method_to_use = 'IT_DEMARK_ALL'.

*First Page
*  CONCATENATE tc_name '_P--' INTO cfcode.
    CALL METHOD me->set_function_code
      EXPORTING
        im_function_code = 'P--' "cfcode
        im_method_to_use = 'TC_SCROLL'.

*Page Up
*  CONCATENATE tc_name '_P-' INTO cfcode.
    CALL METHOD me->set_function_code
      EXPORTING
        im_function_code = 'P-' " cfcode
        im_method_to_use = 'TC_SCROLL'.

*Page Down
*  CONCATENATE tc_name '_P+' INTO cfcode.
    CALL METHOD me->set_function_code
      EXPORTING
        im_function_code = 'P+' "cfcode
        im_method_to_use = 'TC_SCROLL'.

*Last Page
*  CONCATENATE tc_name '_P++' INTO cfcode.
    CALL METHOD me->set_function_code
      EXPORTING
        im_function_code = 'P++' "cfcode
        im_method_to_use = 'TC_SCROLL'.

*Sort Column UP
*  CONCATENATE tc_name '_COL_U' INTO cfcode.
    CALL METHOD me->set_function_code
      EXPORTING
        im_function_code = 'COL_U' "cfcode
        im_method_to_use = 'TC_COLUMN_SORT'.

*Sort Column Down
*  CONCATENATE tc_name '_COL_D' INTO cfcode.
    CALL METHOD me->set_function_code
      EXPORTING
        im_function_code = 'COL_D' "cfcode
        im_method_to_use = 'TC_COLUMN_SORT'.

*Find
*  CONCATENATE tc_name '_FIND' INTO cfcode.
    CALL METHOD me->set_function_code
      EXPORTING
        im_function_code = 'FIND' "cfcode
        im_method_to_use = 'TC_FIND'.

*Find Again
*  CONCATENATE tc_name '_FIND+' INTO cfcode.
    CALL METHOD me->set_function_code
      EXPORTING
        im_function_code = 'FIND+' "cfcode
        im_method_to_use = 'TC_FIND'.

*Replace
*  CONCATENATE tc_name '_REPL' INTO cfcode.
    CALL METHOD me->set_function_code
      EXPORTING
        im_function_code = 'REPL' "cfcode
        im_method_to_use = 'TC_FIND'.

*Replace Again
*  CONCATENATE tc_name '_REPL+' INTO cfcode.
    CALL METHOD me->set_function_code
      EXPORTING
        im_function_code = 'REPL+' "cfcode
        im_method_to_use = 'TC_FIND'.

*Filter
*  CONCATENATE tc_name '_FLTR' INTO cfcode.
    CALL METHOD me->set_function_code
      EXPORTING
        im_function_code = 'FLTR' "cfcode
        im_method_to_use = 'IT_FILTER_ASK'.

*  CONCATENATE tc_name '_SORT' INTO cfcode.
    CALL METHOD me->set_function_code
      EXPORTING
        im_function_code = 'SORT' "cfcode
        im_method_to_use = 'IT_SORT_ASK'.

*  CONCATENATE tc_name '_MV_TP' INTO cfcode.
    CALL METHOD me->set_function_code
      EXPORTING
        im_function_code = 'MV_TP' "cfcode
        im_method_to_use = 'IT_MOVE_TOP'.

*  CONCATENATE tc_name '_MV_UP' INTO cfcode.
    CALL METHOD me->set_function_code
      EXPORTING
        im_function_code = 'MV_UP' "cfcode
        im_method_to_use = 'IT_MOVE_UP'.

*  CONCATENATE tc_name '_MV_DN' INTO cfcode.
    CALL METHOD me->set_function_code
      EXPORTING
        im_function_code = 'MV_DN' "cfcode
        im_method_to_use = 'IT_MOVE_DOWN'.

* CONCATENATE tc_name '_MV_BM' INTO cfcode.
    CALL METHOD me->set_function_code
      EXPORTING
        im_function_code = 'MV_BM' " cfcode
        im_method_to_use = 'IT_MOVE_BOTTOM'.

    CALL METHOD me->set_function_code
      EXPORTING
        im_function_code = 'DELALL' " cfcode
        im_method_to_use = 'IT_REFRESH_AND_NOTIFY'.

    CALL METHOD me->set_function_code
      EXPORTING
        im_function_code = 'ALV' " cfcode
        im_method_to_use = 'IT_ALV'.

    CALL METHOD me->set_function_code
      EXPORTING
        im_function_code = 'COL_F' "cfcode
        im_method_to_use = 'TC_COLUMN_FILTER'.

    CALL METHOD me->set_function_code
      EXPORTING
        im_function_code = 'COL_MF' "cfcode
        im_method_to_use = 'TC_COLUMN_MARK_FILTER'.

  ENDMETHOD.


  METHOD get_rowobject.
* ...
    FIELD-SYMBOLS :
      <wa>           TYPE any,
      <field>        TYPE any,
      <it_rowdb>     TYPE ANY TABLE,
      <it_rowobjupd> TYPE ANY TABLE,
      <it_rowobject> TYPE STANDARD TABLE.

    ASSIGN :
      ptr_it_rowdb->* TO <it_rowdb>,
      ptr_it_rowobject->* TO <it_rowobject>.

    REFRESH <it_rowobject>.
    LOOP AT <it_rowdb> ASSIGNING <wa>.
      ASSIGN COMPONENT 'ROWFILTEROUT' OF STRUCTURE <wa> TO <field>.
      IF <field> IS INITIAL.

        INSERT  <wa> INTO TABLE <it_rowobject>.
      ENDIF.
    ENDLOOP.

    LOOP AT <it_rowobjupd> ASSIGNING <wa>.
      ASSIGN COMPONENT 'ROWDBCNT' OF STRUCTURE <wa> TO <field>.

*      READ TABLE <it_rowobject> WITH KEY ('ROWDBCNT') = <field>
* TRANSPORTING NO FIELDS.
      READ TABLE <it_rowobject> INDEX 1 ASSIGNING <wa>.
*      DELETE <it_rowobject> where ('rowdbcnt') = <field>.
*      WITH TABLE KEY ('ROWDBCNT') = <field>.

* DELETE TABLE <it_rowobject> WITH TABLE KEY
*        rowdbcnt = <wa>-rowdbcnt.
*      READ TABLE <it_rowobject> with key rowdbcnt = <wa>-rowdbcnt  .
    ENDLOOP.

    ex_rowobject = <it_rowobject>.

  ENDMETHOD.


  METHOD it_alv.
* ...

    DATA:
      alv_save          TYPE c,
      l_sortfield(160)  TYPE c,
      l_temp(160)       TYPE c,
      wa_ro_tb_alv_sort LIKE LINE OF   it_ro_tb_alv_sort.

    FIELD-SYMBOLS:
      <wa_ro_tb_alv_fieldcat> LIKE LINE OF it_ro_tb_alv_fieldcat,
      <tc>                    TYPE cxtab_control,
      <it_cols>               TYPE cxtab_control-cols,
      <wa_cols>               TYPE LINE OF cxtab_control-cols,
      <wa_sort_columns>       TYPE zst_do_sort_columns,
      <it_rowobject>          TYPE STANDARD TABLE.

    ASSIGN :
      ptr_it_rowobject->* TO <it_rowobject>.

    REFRESH:
      it_ro_tb_alv_sort[].

    SORT it_ro_tb_sort_columns BY sortorder.

    LOOP AT it_ro_tb_sort_columns ASSIGNING <wa_sort_columns>.
      CLEAR wa_ro_tb_alv_sort.
      wa_ro_tb_alv_sort-spos = <wa_sort_columns>-sortorder.
      wa_ro_tb_alv_sort-fieldname = <wa_sort_columns>-fieldname.
      CASE <wa_sort_columns>-direction.
        WHEN 'D' OR 'd'.
          wa_ro_tb_alv_sort-down = 'X'.
        WHEN OTHERS. "A
          wa_ro_tb_alv_sort-up = 'X'.
      ENDCASE.
      wa_ro_tb_alv_sort-subtot = ro_tb_alv_subtotal.
      wa_ro_tb_alv_sort-expa   = ro_tb_alv_expand.
      APPEND wa_ro_tb_alv_sort TO it_ro_tb_alv_sort.
    ENDLOOP.

    ASSIGN:
      ptr_tb_control->* TO <tc>.

    IF  <tc> IS ASSIGNED.
      ASSIGN:
       <tc>-cols         TO <it_cols>.

      LOOP AT it_ro_tb_alv_fieldcat ASSIGNING <wa_ro_tb_alv_fieldcat>.
        <wa_ro_tb_alv_fieldcat>-no_out = 'X'.
      ENDLOOP.

      LOOP AT <it_cols> ASSIGNING <wa_cols>.
        l_sortfield = <wa_cols>-screen-name.
        WHILE l_sortfield CS '-'.
          SPLIT l_sortfield AT '-' INTO l_temp l_sortfield.
        ENDWHILE.
        READ TABLE it_ro_tb_alv_fieldcat ASSIGNING <wa_ro_tb_alv_fieldcat>
                WITH KEY fieldname = l_sortfield.
        IF sy-subrc = 0.
          CASE <wa_cols>-invisible.
            WHEN space.
              CLEAR <wa_ro_tb_alv_fieldcat>-no_out.
            WHEN 'X'.
              <wa_ro_tb_alv_fieldcat>-no_out = 'X'.
          ENDCASE.
        ENDIF.
      ENDLOOP.

    ENDIF.

    CALL METHOD me->it_alv_override.

*  CALL METHOD
*    ME->

    alv_save = 'U'.

    AUTHORITY-CHECK OBJECT 'S_ALV_LAYO'
             ID 'ACTVT' FIELD '23'.
    IF sy-subrc = 0.
      alv_save = 'A'.
    ENDIF.

    CALL FUNCTION 'Z_DO_TB_TOOLS_01_ALV'
      EXPORTING
        im_events         = ro_tb_alv_events[]
        im_sort           = it_ro_tb_alv_sort[]
        im_save           = alv_save
        im_variant        = ro_tb_alv_variant
        im_structure_name = ro_tb_st_type
        im_fieldcat       = it_ro_tb_alv_fieldcat
        im_data_object    = me
      TABLES
        it_outtab         = <it_rowobject>.

*
*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*       EXPORTING
*            i_callback_user_command  = slis_ev_user_command
*            it_events          =
*            i_save             =
*            is_variant         =
*            i_callback_program = ro_tb_alv_program
*            i_structure_name   =
*            it_fieldcat        =
*       TABLES
*            t_outtab           =
*       EXCEPTIONS
*            program_error      = 1
*            OTHERS             = 2.
**          .
*IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*ENDIF.

  ENDMETHOD.


  METHOD it_alv_get_variant .
* ...

    ex_variant = ro_tb_alv_variant-variant.

  ENDMETHOD.


  METHOD it_alv_override.
* ...
  ENDMETHOD.


  METHOD it_alv_set_program.
* ...

    DATA:
      c_alv_program LIKE ro_tb_alv_program.

*  IF im_screen IS INITIAL.
    c_alv_program = im_program.
*  ELSE.
*    CONCATENATE
*      im_program im_screen INTO c_alv_program SEPARATED BY '_'.
*  ENDIF.
*
    ro_tb_alv_variant-report = im_program.
    ro_tb_alv_variant-handle = im_screen.
    ro_tb_alv_variant-variant = im_variant.

  ENDMETHOD.


  METHOD it_alv_set_variant .
* ...

    ro_tb_alv_variant-variant = im_variant.

  ENDMETHOD.


  METHOD it_alv_tc_fields_only.
* ...
    FIELD-SYMBOLS :
      <wa_ro_tb_alv_fieldcat> LIKE LINE OF it_ro_tb_alv_fieldcat,
      <tc>                    TYPE cxtab_control,
      <cols>                  TYPE cxtab_control-cols,
      <wa>                    TYPE LINE OF cxtab_control-cols.

    DATA:
      l_temp(160)           TYPE c,
      l_sortfield(160)      TYPE c,
      wa_ro_tb_alv_fieldcat LIKE LINE OF it_ro_tb_alv_fieldcat.

    wa_ro_tb_alv_fieldcat-tech = ' '.

    MODIFY it_ro_tb_alv_fieldcat
      FROM wa_ro_tb_alv_fieldcat TRANSPORTING tech
      WHERE tech = 'X'
        AND fieldname NS 'ROW'.

    ASSIGN :
      ptr_tb_control->* TO <tc>,
      <tc>-cols         TO <cols>.
    LOOP AT <cols> ASSIGNING <wa>
      WHERE invisible = zcl_do_it=>co_tc_invisible.
      l_sortfield = <wa>-screen-name.
      WHILE l_sortfield CS '-'.
        SPLIT l_sortfield AT '-' INTO l_temp l_sortfield.
      ENDWHILE.
      READ TABLE it_ro_tb_alv_fieldcat
         ASSIGNING <wa_ro_tb_alv_fieldcat>
         WITH KEY fieldname = l_sortfield.
      IF sy-subrc = 0.
        <wa_ro_tb_alv_fieldcat>-tech = zcl_do_it=>co_invisible.
        <wa_ro_tb_alv_fieldcat>-no_out = zcl_do_it=>co_invisible.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD it_alv_user_command.
* ...

    FIELD-SYMBOLS :
      <tc>           TYPE cxtab_control,
      <wa_dbcnt>     TYPE any,
      <wa_rowobject> TYPE any,
      <it_rowobject> TYPE STANDARD TABLE.

    ASSIGN :
       ptr_wa_rowobject->* TO <wa_rowobject>,
       ptr_it_rowobject->* TO <it_rowobject>.

*  ch_start_from_line = ro_tb_search_parms-top_line.

*  ASSIGN ptr_it_rowobject->* TO <it_rowobject>.

    CASE im_ucomm.
      WHEN '&IC1'.
        IF im_selfield-tabindex > 0.
          READ TABLE  <it_rowobject> INDEX im_selfield-tabindex
            TRANSPORTING NO FIELDS..
          IF sy-subrc = 0.
            READ TABLE  <it_rowobject> INDEX im_selfield-tabindex
               INTO <wa_rowobject>.
            ASSIGN:
                COMPONENT zcl_do_it=>co_row_db_cnt
                OF STRUCTURE  <wa_rowobject> TO <wa_dbcnt>.
            tc_current_rowdbcnt = <wa_dbcnt>.

            ASSIGN :
              ptr_tb_control->* TO <tc>.
            IF sy-subrc = 0.
              <tc>-top_line = im_selfield-tabindex.
            ENDIF.
            RAISE EVENT work_area_changed.
          ENDIF.
        ENDIF.
    ENDCASE.

  ENDMETHOD.


  METHOD it_append.
* ...

*THIs method will append to the table one page full if attached to table
*control else it will append 1 record.

    DATA :
      prt_new_row        TYPE REF TO data.

    FIELD-SYMBOLS:
      <wa_new_row>   TYPE any,
      <it_rowobject> TYPE STANDARD TABLE,
      <dbcnt>        TYPE any.

    ASSIGN :
      ptr_it_rowobject->* TO <it_rowobject>.

    CREATE DATA prt_new_row LIKE LINE OF <it_rowobject>.

    ASSIGN :
        prt_new_row->* TO <wa_new_row>,
      COMPONENT zcl_do_it=>co_row_db_cnt
        OF STRUCTURE <wa_new_row> TO <dbcnt>.

    DO im_lines TIMES.
      <dbcnt> = 0.
      CALL METHOD me->it_row_empty
        EXPORTING
          im_ptr_to_row = prt_new_row.
      APPEND <wa_new_row> TO <it_rowobject>.
    ENDDO.

  ENDMETHOD.


  METHOD it_compare.
* ...

    DATA:
     l_ptr_row        TYPE REF TO data.

    FIELD-SYMBOLS :
      <wa_rowobject> TYPE any,
      <it_rowobject> TYPE STANDARD TABLE.

    ASSIGN :
      ptr_it_rowobject->*  TO <it_rowobject>.

*Loop threw each record of the row object table and compare to the
*row db table if any of the fields are different then added the record
*to the row object update table Change Mod to 'U'

    LOOP AT <it_rowobject> ASSIGNING <wa_rowobject>.
      GET REFERENCE OF <wa_rowobject> INTO l_ptr_row.
      CALL METHOD me->ru_update
        EXPORTING
          im_ptr_to_row = l_ptr_row.
    ENDLOOP.
  ENDMETHOD.


  METHOD it_copy.
* ...
*  FOR EACH ROWOBJECT WHERE ROWMARKED = X
*  OR ONLY FOR THE CURRENT LINE

    DATA :
      prt_new_row        TYPE REF TO data.

    FIELD-SYMBOLS :
      <wa_new_row>   TYPE any,
      <it_rowobject> TYPE STANDARD TABLE,
      <it_rowobjupd> TYPE STANDARD TABLE,
      <wa_rowobject> TYPE any,
      <mark_field>   TYPE any,
      <dbcnt>        TYPE any.

    ASSIGN :
      ptr_it_rowobjupd->* TO <it_rowobjupd>,
      ptr_it_rowobject->* TO <it_rowobject>.

    CREATE DATA prt_new_row LIKE LINE OF <it_rowobjupd>.

    ASSIGN :
        prt_new_row->* TO <wa_new_row>,
        COMPONENT zcl_do_it=>co_row_db_cnt
          OF STRUCTURE <wa_new_row> TO <dbcnt>.

    IF lg_copy_marked = 'X'.
      LOOP AT <it_rowobject> ASSIGNING <wa_rowobject>.

*   access to the component 'FLAG' of the table header                 *
        ASSIGN COMPONENT zcl_do_it=>co_row_marker
          OF STRUCTURE <wa_rowobject> TO <mark_field>.

        IF <mark_field> = 'X'.

          CLEAR <mark_field>.
          <wa_new_row> = <wa_rowobject>.
          <dbcnt> = 0.

          CALL METHOD :
            it_row_empty
              EXPORTING
                im_ptr_to_row = prt_new_row,
            it_row_created
              EXPORTING
                im_ptr_to_row = prt_new_row.

          APPEND <wa_new_row> TO <it_rowobject>.
          APPEND <wa_new_row> TO <it_rowobjupd>.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF lg_copy_cursor = 'X' AND im_line > 0.

    ENDIF.

  ENDMETHOD.


  METHOD it_cursor.
* ...

    IF ro_tb_search_parms-setcursor = 'X'.
      ch_set_position = 'X'.
      ch_field = ro_tb_search_parms-zcursor-field.
      ch_offset = ro_tb_search_parms-zcursor-fieldoffset.
      ch_line = ro_tb_search_parms-zcursor-line.
      CLEAR ro_tb_search_parms-setcursor.
    ENDIF.
  ENDMETHOD.


  METHOD it_data_filter_exit.
* uSE THIS METHOD TO OVERRIDE FILTER PASSED BY LINK.
  ENDMETHOD.


  METHOD it_delete.
* ...

    DATA :
      next_db_cnt    TYPE sydbcnt,
      repos_db_cnt   TYPE sydbcnt,
      l_ptr_row      TYPE REF TO data,
      on_deleted     TYPE char1,
      rowobjectindex LIKE sy-tabix.

    FIELD-SYMBOLS :
      <wa>           TYPE any,
      <it_rowobject> TYPE STANDARD TABLE,
      <wa_rowobject> TYPE any,
      <mark_field>   TYPE any,
      <rowmod>       TYPE any,
      <dbcnt>        TYPE any,
      <wadbcnt>      TYPE any.

    ASSIGN :
      ptr_wa_rowobject->* TO <wa>,
      ptr_it_rowobject->* TO <it_rowobject>,
      COMPONENT zcl_do_it=>co_row_db_cnt
            OF STRUCTURE <wa> TO <wadbcnt>.

    IF lg_delete_cursor = 'X' AND im_line > 0.
      READ TABLE <it_rowobject> ASSIGNING <wa_rowobject> INDEX im_line.
      IF sy-subrc = 0.
        rowobjectindex = sy-tabix.

        GET REFERENCE OF <wa_rowobject> INTO l_ptr_row.
        CALL METHOD me->it_row_deleted
          EXPORTING
            im_ptr_to_row = l_ptr_row.

        ASSIGN :
           COMPONENT zcl_do_it=>co_row_mod
             OF STRUCTURE <wa_rowobject> TO <rowmod>.

        IF <wa> IS ASSIGNED AND on_deleted IS INITIAL.
          IF <wadbcnt> = <dbcnt>.
            on_deleted = 'X'.
          ENDIF.
        ENDIF.

        <rowmod> = zcl_do_it=>co_mod_delete.
        CALL METHOD ru_update
          EXPORTING
            im_ptr_to_row = l_ptr_row.

        DELETE <it_rowobject> INDEX rowobjectindex.

      ENDIF.

      IF on_deleted = 'X'.
        IF im_line > 1.
          im_line = im_line - 1.
        ELSE.
          im_line = 2.
        ENDIF.
        READ TABLE <it_rowobject> ASSIGNING <wa_rowobject> INDEX im_line.
        IF sy-subrc = 0.
          ASSIGN
            COMPONENT zcl_do_it=>co_row_db_cnt
                OF STRUCTURE <wa_rowobject> TO <dbcnt>.
          next_db_cnt =  <dbcnt>.
        ENDIF.
      ENDIF.
    ENDIF.

    IF lg_delete_marked = 'X'.

      LOOP AT <it_rowobject> ASSIGNING <wa_rowobject>.

*   access to the component 'FLAG' of the table header                 *
        ASSIGN:
          COMPONENT zcl_do_it=>co_row_db_cnt
            OF STRUCTURE <wa_rowobject> TO <dbcnt>,
          COMPONENT zcl_do_it=>co_row_marker
            OF STRUCTURE <wa_rowobject> TO <mark_field>.

        IF <mark_field> = 'X'.
          rowobjectindex = syst-tabix.

          GET REFERENCE OF <wa_rowobject> INTO l_ptr_row.
          CALL METHOD me->it_row_deleted
            EXPORTING
              im_ptr_to_row = l_ptr_row.

          ASSIGN :
             COMPONENT zcl_do_it=>co_row_mod
               OF STRUCTURE <wa_rowobject> TO <rowmod>.

          IF <wa> IS ASSIGNED AND on_deleted IS INITIAL.

            IF <wadbcnt> = <dbcnt>.
              on_deleted = 'X'.
            ENDIF.
          ENDIF.

          <rowmod> = zcl_do_it=>co_mod_delete.
          CALL METHOD ru_update
            EXPORTING
              im_ptr_to_row = l_ptr_row.

*      CASE <rowmod>.
*        WHEN zcl_do_it=>co_mod_update OR
*             zcl_do_it=>co_mod_original.
*          <rowmod> = zcl_do_it=>co_mod_delete.
*          READ TABLE <it_rowobjupd>
*              WITH KEY (zcl_do_it=>co_row_db_cnt) = <dbcnt>
*              TRANSPORTING NO FIELDS.
*          IF sy-subrc = 0.
*            MODIFY <it_rowobjupd> FROM <wa_rowobject> INDEX sy-tabix.
*          ELSE.
*            APPEND <wa_rowobject> TO <it_rowobjupd>.
*          ENDIF.
*        WHEN zcl_do_it=>co_mod_create.
*          READ TABLE <it_rowobjupd>
*              WITH KEY (zcl_do_it=>co_row_db_cnt) = <dbcnt>
*              TRANSPORTING NO FIELDS.
*          IF sy-subrc = 0.
*            DELETE <it_rowobjupd> INDEX sy-tabix.
*          ENDIF.
**        WHEN zcl_do_it=>co_mod_empty.
*      ENDCASE.

          DELETE <it_rowobject> INDEX rowobjectindex.
        ELSEIF next_db_cnt IS INITIAL AND on_deleted = 'X'.
          next_db_cnt = <dbcnt>.
        ELSEIF on_deleted IS INITIAL OR repos_db_cnt IS INITIAL .
          repos_db_cnt = <dbcnt>.
        ENDIF.

      ENDLOOP.

      IF NOT next_db_cnt IS INITIAL.
        repos_db_cnt = next_db_cnt.
      ENDIF.

    ENDIF.

    IF <wa> IS ASSIGNED AND on_deleted = 'X' AND repos_db_cnt > 0.
      CALL METHOD wa_from_it
        EXPORTING
          im_db_cnt = repos_db_cnt.
*    CLEAR <wa>.
*    READ TABLE <it_rowobject> INTO <wa> INDEX 1.
*    IF sy-subrc NE 0.
*      CALL METHOD:
*        it_row_empty
*          EXPORTING
*            im_ptr_to_row = ptr_wa_rowobject,
*        wa_to_it.
*    ENDIF.
    ENDIF.

*  fOR EACH ROWOBJECT WHERE ROWMARKED = 'X'
*  oR oNLY THE CURRENT lINE IS DELETED
*
*  If RowMod is Create then delete RowObjUpd

*  Else Create a Delete Record in RowObjUpd

*  Delete Record out of RowObject
  ENDMETHOD.


  METHOD it_demark_all.
* ...

    FIELD-SYMBOLS:
      <it_rowobject> TYPE STANDARD TABLE,
      <rowmark>      TYPE any,
      <wa_rowobject> TYPE any.

    ASSIGN ptr_it_rowobject->* TO <it_rowobject>.

    LOOP AT <it_rowobject> ASSIGNING <wa_rowobject>.

*   access to the component 'FLAG' of the table header                 *
      ASSIGN COMPONENT zcl_do_it=>co_row_marker
          OF STRUCTURE <wa_rowobject>
          TO <rowmark>.

      <rowmark> = space.
    ENDLOOP.

  ENDMETHOD.


  METHOD it_filter.
* ...

    FIELD-SYMBOLS:
      <wa_dbcnt>       TYPE any,
      <it_rowobjupd>   TYPE STANDARD TABLE,
      <it_rowdb>       TYPE STANDARD TABLE,
      <it_rowobject>   TYPE STANDARD TABLE,
      <low>            TYPE any,
      <high>           TYPE any,
      <fieldvalue>     TYPE any,
      <rowdbfield>     TYPE any,
      <wa_fieldsvalue> TYPE zif_do_seo=>ts_field_values,
      <rowmod>         TYPE any,
      <rowdbcnt>       TYPE any,
      <filter>         TYPE any,
      <wa_rowdb>       TYPE any,
      <wa_rowobject>   TYPE any,
      <wa_rowobjupd>   TYPE any.

    DATA :
      prev_idx        TYPE sytabix,
      next_idx        TYPE sytabix,
      it_field_values TYPE zif_do_seo=>tt_field_values.

    ASSIGN :
      ptr_wa_rowobject->* TO <wa_rowobject>,
      ptr_it_rowobject->* TO <it_rowobject>,
      ptr_it_rowdb->*     TO <it_rowdb>,
      ptr_it_rowobjupd->* TO <it_rowobjupd>.

    CLEAR:
      prev_idx,
      next_idx.

    IF tc_lg_keep_position = 'X'.
      READ TABLE <it_rowobject>
          WITH KEY (zcl_do_it=>co_row_db_cnt) = tc_current_rowdbcnt
           TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        prev_idx = sy-tabix - 1.
        next_idx = sy-tabix.
      ENDIF.
    ENDIF.

*    DATA :
*      c_match TYPE c.

*  Compare RowDb to RowObject and move differences into Row Object Upd
    LOOP AT <it_rowobject> ASSIGNING <wa_rowobject>.
      ASSIGN :
        COMPONENT zcl_do_it=>co_row_db_cnt
          OF STRUCTURE <wa_rowobject>
          TO <rowdbcnt>,
        COMPONENT zcl_do_it=>co_row_mod
          OF STRUCTURE <wa_rowobject>
          TO <rowmod>.
      CASE <rowmod>.
        WHEN 'D'.
        WHEN 'E'.
        WHEN 'C'.
        WHEN 'O' OR 'U'.
          READ TABLE <it_rowdb> ASSIGNING <wa_rowdb>
            WITH KEY (zcl_do_it=>co_row_db_cnt) = <rowdbcnt>.
          IF sy-subrc = 0.
*FIND ROWOBJUPD IF EXIST UPDATED IT ELSE CREATE IT.
*mark row with u
            <rowmod> = 'U'.
          ENDIF.
      ENDCASE.
    ENDLOOP.

    CALL METHOD ptr_ro_tb_where->get_field_value
      IMPORTING
        ex_it_field_values = it_field_values.

    READ TABLE it_field_values INDEX 1 TRANSPORTING NO FIELDS.

    IF sy-subrc = 0 AND me->ptr_ro_tb_where->filter_on = 'X'.
      LOOP AT <it_rowdb> ASSIGNING <wa_rowdb>.
        ASSIGN
             COMPONENT zcl_do_it=>co_row_filter
               OF STRUCTURE <wa_rowdb>
               TO <filter>.
        CALL METHOD ptr_ro_tb_where->clear_value.
        LOOP AT it_field_values ASSIGNING <wa_fieldsvalue>.
          ASSIGN:
            COMPONENT <wa_fieldsvalue>-fieldname
              OF STRUCTURE  <wa_rowdb>
              TO <fieldvalue>.
          CALL METHOD ptr_ro_tb_where->assign_value
            EXPORTING
              im_field_name = <wa_fieldsvalue>-fieldname
              im_value      = <fieldvalue>.
        ENDLOOP.
        CALL METHOD ptr_ro_tb_where->is_match
          IMPORTING
            ex_match = <filter>.
*        <filter> = c_match.
*        IF c_match = 'X'.
*         <filter> = 'X'.
      ENDLOOP.




    ELSE.
      LOOP AT <it_rowdb> ASSIGNING <wa_rowdb>.
        ASSIGN
            COMPONENT zcl_do_it=>co_row_filter
              OF STRUCTURE <wa_rowdb>
              TO <filter>.
        <filter> = 'X'.
*        CLEAR <filter>.
      ENDLOOP.
    ENDIF.

*  Update RowObject with the new Filtered Rows
    REFRESH <it_rowobject>.
    LOOP AT <it_rowdb> ASSIGNING <wa_rowdb>.
      ASSIGN :
        COMPONENT zcl_do_it=>co_row_db_cnt
          OF STRUCTURE <wa_rowdb>
          TO <rowdbcnt>,
          COMPONENT zcl_do_it=>co_row_filter
            OF STRUCTURE <wa_rowdb>
            TO <filter>.
      IF <filter> = 'X'.
        READ TABLE <it_rowobjupd> ASSIGNING <wa_rowobjupd>
          WITH KEY (zcl_do_it=>co_row_db_cnt)  = <rowdbcnt>.
        IF sy-subrc = 0.
          APPEND <wa_rowobjupd> TO <it_rowobject>.
        ELSE.
          APPEND <wa_rowdb> TO <it_rowobject>.
        ENDIF.
      ENDIF.
    ENDLOOP.

    DESCRIBE TABLE <it_rowobject> LINES ro_tb_rows.
* fILTER REMOVED ALL THE LINES THEN PUT UP MESSAGE AND REMOVE THE FILTER.
    IF lg_auto_disable_filter = 'X' AND ro_tb_rows = 0 AND db_rows_selected NE 0 AND ptr_ro_tb_where->filter_on = 'X'.
      MESSAGE s004(zdo).
      me->ptr_ro_tb_where->set_filter_on( space ).
    ENDIF.


    CALL METHOD it_sort.

    ASSIGN :
      ptr_wa_rowobject->* TO <wa_rowobject>.

    IF tc_current_rowdbcnt NE 0.
      READ TABLE <it_rowobject>
        WITH KEY (zcl_do_it=>co_row_db_cnt) = tc_current_rowdbcnt
        INTO <wa_rowobject>.
      IF sy-subrc NE 0.
        IF next_idx NE 0 OR prev_idx NE 0.
* Look for next Record
          READ TABLE <it_rowobject> INDEX next_idx
            INTO <wa_rowobject>.
          IF sy-subrc NE 0.
* Look for the Previous Record
            READ TABLE <it_rowobject> INDEX prev_idx
              INTO <wa_rowobject>.
            IF sy-subrc NE 0.
* Put the user back to top
              READ TABLE <it_rowobject> INDEX 1 INTO <wa_rowobject>.
              IF sy-subrc NE 0.
                CLEAR <wa_rowobject>.
              ENDIF.
            ENDIF.
          ENDIF.
        ELSE.
          READ TABLE <it_rowobject> INDEX 1 INTO <wa_rowobject>.
          IF sy-subrc NE 0.
            CLEAR <wa_rowobject>.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      READ TABLE <it_rowobject> INDEX 1 INTO <wa_rowobject>.
      IF sy-subrc NE 0.
        CLEAR <wa_rowobject>.
      ENDIF.
    ENDIF.

    ASSIGN:
        COMPONENT zcl_do_it=>co_row_db_cnt
        OF STRUCTURE  <wa_rowobject> TO <wa_dbcnt>.
    tc_current_rowdbcnt = <wa_dbcnt>.

    IF im_publish_work_area_changed = 'X'.
      RAISE EVENT work_area_changed.
    ENDIF.

    CALL METHOD tc_initialize.

  ENDMETHOD.


  METHOD it_filter_ask.
* ...

    CALL METHOD me->ptr_ro_tb_where->ask_selection_options
      EXCEPTIONS
        no_changes = 1
        OTHERS     = 2.

    IF sy-subrc = 0.
      CALL METHOD me->it_filter.
    ENDIF.

  ENDMETHOD.


  METHOD it_find.
* ...

    FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.

    ASSIGN :
      ptr_it_rowobject->* TO <table>.

    GET CURSOR FIELD ro_tb_search_parms-zcursor-field
           LINE  ro_tb_search_parms-zcursor-line
           AREA  ro_tb_search_parms-zcursor-area
           OFFSET ro_tb_search_parms-zcursor-fieldoffset
           VALUE ro_tb_search_parms-zcursor-value.

*  ro_tb_search_parms-zcursor = do_cursor.

    ro_tb_search_parms-top_line  = ch_start_from_line.
    ro_tb_search_parms-tablename = ro_tb_name.
    ro_tb_search_parms-markfield = zcl_do_it=>co_row_marker.
*  ro_tb_search_parms-zcursor   = do_cursor.

    CALL FUNCTION 'Z_DO_TB_TOOLS_01_FIND_ASK'
      EXPORTING
        method      = im_method
      CHANGING
        searchparms = ro_tb_search_parms.

    CALL FUNCTION 'Z_DO_TB_TOOLS_01_FIND'
      TABLES
        findin      = <table>
        fields      = it_ro_tb_find_fields
      CHANGING
        searchparms = ro_tb_search_parms.

    ch_start_from_line = ro_tb_search_parms-top_line.

  ENDMETHOD.


  METHOD it_force_integrity.
* ...

    DATA :
      c_match TYPE char1,
      ptr_row TYPE REF TO data.

    FIELD-SYMBOLS:
      <wa_fieldtab>  LIKE LINE OF it_ro_tb_structure,
      <field>        TYPE any,
      <wa_rowobject> TYPE any,
      <it_rowobject> TYPE STANDARD TABLE,
      <dbcnt>        TYPE any.

    ASSIGN :
      ptr_it_rowobject->* TO <it_rowobject>.

    LOOP AT <it_rowobject> ASSIGNING <wa_rowobject>.
      ASSIGN :
        COMPONENT zcl_do_it=>co_row_db_cnt
          OF STRUCTURE <wa_rowobject> TO <dbcnt>.
      IF <dbcnt> < 1.
        GET REFERENCE OF <wa_rowobject> INTO ptr_row.
        <dbcnt> = 0.
        CALL METHOD me->it_row_empty
          EXPORTING
            im_ptr_to_row = ptr_row.
        CLEAR c_match.
        LOOP AT it_ro_tb_structure  ASSIGNING <wa_fieldtab>.
          ASSIGN :
            COMPONENT <wa_fieldtab>-fieldname
              OF STRUCTURE <wa_rowobject>
              TO <field>.
          IF NOT <field> IS INITIAL.
            c_match = 'X'.
            EXIT.
          ENDIF.
        ENDLOOP.
        IF c_match = 'X'.
          CALL METHOD me->it_row_created
            EXPORTING
              im_ptr_to_row = ptr_row.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD it_function_code.
* ...

    DATA :
      lg_filter_prompt TYPE char1,
      ilines           TYPE i,
      l_tc_name        TYPE fieldname,
      cfield(30)       TYPE c,
      l_ok             TYPE sy-ucomm,
      l_offset         TYPE i,
      wa_fcodes        TYPE zif_do=>ts_function_codes.

    current_ucomm  = ch_function_code.
    SEARCH ch_function_code FOR tc_name.
    IF sy-subrc = 0.
      l_offset = strlen( tc_name ) + 1.
      l_ok = ch_function_code+l_offset.
    ELSE.
*      CASE ch_function_code.
*        when 'CHOOSE'.

* If interal table is empty and data was selected from database
* Then a filter might have removed all data.
* Then we need to test if filter is on.
      CLEAR lg_filter_prompt.
      IF ro_tb_rows = 0 AND db_rows_selected NE 0.
        CASE ch_function_code.
          WHEN 'FLTR' OR 'COL_F'.
*            IF me->ptr_ro_tb_where->filter_on = 'X'.
            ch_function_code = 'FLTR'.
            lg_filter_prompt = 'X'.
*            ENDIF.
        ENDCASE.
      ENDIF.

*      GET CURSOR AREA L_TC_NAME.

      GET CURSOR FIELD cfield AREA l_tc_name.
      IF l_tc_name NE tc_name.
        IF l_tc_name IS INITIAL.
          IF  ( main_fcode_object_id NE me->id OR
            ch_function_code = 'CHOOSE' OR
            im_main_object = space ) AND lg_filter_prompt = space.
            EXIT.
          ENDIF.
        ELSEIF im_main_object = space.
          EXIT.
        ENDIF.
      ENDIF.
      l_ok = ch_function_code.
*    WHEN OTHERS.
*      EXIT.
*  ENDCASE.
    ENDIF.

    READ TABLE it_fcodes INTO wa_fcodes
      WITH TABLE KEY fcode = l_ok.
    IF sy-subrc NE 0.
      READ TABLE it_fcodes INTO wa_fcodes
        WITH TABLE KEY fcode = ch_function_code.
    ENDIF.
    IF sy-subrc = 0.
      IF lg_readonly = zcl_do_it=>co_true AND
         wa_fcodes-update_only = 'X'.
        EXIT.
      ENDIF.

*    CALL METHOD me->get_cursor.
      CASE l_ok.
        WHEN  'FIND'  OR
              'FIND+' OR
              'REPL'  OR
              'REPL+' OR
              'COL_D' OR
              'COL_U' OR
              'UP'    OR
              'DOWN'  OR
              'P--'   OR           "top of list
              'P-'    OR           "previous page
              'P+'    OR           "next page
              'P++'.               "bottom of list
          CALL METHOD me->(wa_fcodes-use_method)
            EXPORTING
              im_method = l_ok.
        WHEN OTHERS.
          CALL METHOD me->(wa_fcodes-use_method).
      ENDCASE.
      CLEAR ch_function_code.
    ENDIF.

  ENDMETHOD.


  METHOD it_get_marked_rows.
* ...

    FIELD-SYMBOLS <it_rowobject> TYPE STANDARD TABLE.
    ASSIGN ptr_it_rowobject->* TO <it_rowobject>.

    FIELD-SYMBOLS:
      <rowmark>      TYPE any,
      <wa_rowobject> TYPE any.

    DATA :
      wa_marked_rows TYPE zif_do=>ts_marked_rows.

    REFRESH it_ro_tb_marked_rows.
    LOOP AT <it_rowobject> ASSIGNING <wa_rowobject>.
      ASSIGN COMPONENT zcl_do_it=>co_row_marker
        OF STRUCTURE <wa_rowobject> TO <rowmark>.
      IF <rowmark> = 'X'.
        wa_marked_rows-rowid = sy-tabix.
        APPEND wa_marked_rows TO it_ro_tb_marked_rows.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD it_insert.
* ...

    DATA :
      prt_new_row        TYPE REF TO data.

    FIELD-SYMBOLS:
      <wa_new_row>   TYPE any,
      <dbcnt>        TYPE any,
      <it_rowobject> TYPE STANDARD TABLE.

    ASSIGN :
      ptr_it_rowobject->* TO <it_rowobject>.

    CREATE DATA prt_new_row LIKE LINE OF <it_rowobject>.

    ASSIGN :
        prt_new_row->* TO <wa_new_row>,
        COMPONENT zcl_do_it=>co_row_db_cnt
          OF STRUCTURE <wa_new_row> TO <dbcnt>.

    CALL METHOD me->it_row_empty
      EXPORTING
        im_ptr_to_row = prt_new_row.

    ex_db_cnt = <dbcnt>.

    IF im_line NE 0.
      INSERT <wa_new_row> INTO <it_rowobject> INDEX im_line.
    ELSE.
      INSERT <wa_new_row> INTO <it_rowobject> INDEX 1.
    ENDIF.

  ENDMETHOD.


  METHOD it_mark_all.
* ...

    FIELD-SYMBOLS:
      <it_rowobject> TYPE STANDARD TABLE,
      <rowmark>      TYPE any,
      <wa_rowobject> TYPE any.

    ASSIGN ptr_it_rowobject->* TO <it_rowobject>.

    LOOP AT <it_rowobject> ASSIGNING <wa_rowobject>.
*   access to the component 'FLAG' of the table header                 *
      ASSIGN COMPONENT zcl_do_it=>co_row_marker
          OF STRUCTURE <wa_rowobject>
          TO <rowmark>.
      <rowmark> = 'X'.
    ENDLOOP.
  ENDMETHOD.


  METHOD it_mark_block .
* ...

    FIELD-SYMBOLS:
      <it_rowobject> TYPE STANDARD TABLE,
      <rowmark>      TYPE any,
      <wa_rowobject> TYPE any.

    DATA:
      i_first_mark LIKE sy-tabix,
      i_last_mark  LIKE sy-tabix.

    ASSIGN ptr_it_rowobject->* TO <it_rowobject>.

    READ TABLE <it_rowobject>
      WITH KEY (zcl_do_it=>co_row_marker) = 'X'
      TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      i_first_mark = sy-tabix + 1.

      LOOP AT <it_rowobject> ASSIGNING <wa_rowobject>
        FROM i_first_mark.

*   access to the component 'FLAG' of the table header                 *
        ASSIGN COMPONENT zcl_do_it=>co_row_marker
            OF STRUCTURE <wa_rowobject>
            TO <rowmark>.
        IF <rowmark> NE 'X'.
          EXIT.
        ELSE.
          i_first_mark = sy-tabix + 1.
        ENDIF.
*    <rowmark> = 'X'.
      ENDLOOP.

      LOOP AT <it_rowobject> ASSIGNING <wa_rowobject>
        FROM i_first_mark.

*   access to the component 'FLAG' of the table header                 *
        ASSIGN COMPONENT zcl_do_it=>co_row_marker
            OF STRUCTURE <wa_rowobject>
            TO <rowmark>.
        IF <rowmark> = 'X'.
          EXIT.
        ELSE.
          i_last_mark = sy-tabix.
        ENDIF.
*    <rowmark> = 'X'.
      ENDLOOP.

      IF i_last_mark > 1 AND i_last_mark > i_first_mark.
        LOOP AT <it_rowobject> ASSIGNING <wa_rowobject>
             FROM i_first_mark TO i_last_mark.

*   access to the component 'FLAG' of the table header                 *
          ASSIGN COMPONENT zcl_do_it=>co_row_marker
              OF STRUCTURE <wa_rowobject>
              TO <rowmark>.

          <rowmark> = 'X'.
        ENDLOOP.

      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD it_mark_filter.
* ...

    DATA :
      it_field_values TYPE zif_do_seo=>tt_field_values.

    FIELD-SYMBOLS:
      <it_rowobject>   TYPE STANDARD TABLE,
      <fieldvalue>     TYPE any,
      <wa_fieldsvalue> TYPE zif_do_seo=>ts_field_values,
      <rowmark>        TYPE any,
      <wa_rowobject>   TYPE any.

    ASSIGN :
       ptr_it_rowobject->* TO <it_rowobject>.

    CALL METHOD ptr_ro_tb_mark->get_field_value
      IMPORTING
        ex_it_field_values = it_field_values.


    IF it_field_values[] IS NOT INITIAL.
      LOOP AT <it_rowobject> ASSIGNING <wa_rowobject>.

*   access to the component 'FLAG' of the table header                 *
        ASSIGN COMPONENT zcl_do_it=>co_row_marker
            OF STRUCTURE <wa_rowobject>
            TO <rowmark>.

        IF <rowmark> = space.

          CALL METHOD ptr_ro_tb_mark->clear_value.

          LOOP AT it_field_values ASSIGNING <wa_fieldsvalue>.
            ASSIGN:
              COMPONENT <wa_fieldsvalue>-fieldname
                OF STRUCTURE  <wa_rowobject>
                TO <fieldvalue>.
            CALL METHOD ptr_ro_tb_mark->assign_value
              EXPORTING
                im_field_name = <wa_fieldsvalue>-fieldname
                im_value      = <fieldvalue>.
          ENDLOOP.
          CALL METHOD ptr_ro_tb_mark->is_match
            IMPORTING
              ex_match = <rowmark>.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD it_mark_filter_ask .
* ...

    CALL METHOD me->ptr_ro_tb_mark->ask_selection_options
      EXCEPTIONS
        no_changes = 1
        OTHERS     = 2.

    IF sy-subrc = 0.
      CALL METHOD me->it_mark_filter.
    ENDIF.

  ENDMETHOD.


  METHOD it_move_bottom.
* ...

    FIELD-SYMBOLS:
      <it_rowobject> TYPE STANDARD TABLE,
      <rowmark>      TYPE any,
      <rowseq>       TYPE any,
      <wa_rowobject> TYPE any.

    DATA :
      next_seq       TYPE sydbcnt.

    ASSIGN ptr_it_rowobject->* TO <it_rowobject>.

    SORT <it_rowobject> BY (zcl_do_it=>co_row_seq).

    next_seq = 0.

    LOOP AT <it_rowobject> ASSIGNING <wa_rowobject>.
      ASSIGN :
       COMPONENT zcl_do_it=>co_row_marker
         OF STRUCTURE <wa_rowobject> TO <rowmark>,
       COMPONENT zcl_do_it=>co_row_seq
         OF STRUCTURE <wa_rowobject> TO <rowseq>.
      IF <rowmark> NE 'X'.
        next_seq = next_seq + 1.
        <rowseq>  = next_seq.
      ENDIF.
    ENDLOOP.

    LOOP AT <it_rowobject> ASSIGNING <wa_rowobject>.
      ASSIGN :
        COMPONENT zcl_do_it=>co_row_marker
          OF STRUCTURE <wa_rowobject> TO <rowmark>,
        COMPONENT zcl_do_it=>co_row_seq
          OF STRUCTURE <wa_rowobject> TO <rowseq>.
      IF <rowmark> = 'X'.
        next_seq = next_seq + 1.
        <rowseq>  = next_seq.
*      CLEAR <rowmark>.
      ENDIF.
    ENDLOOP.

    SORT <it_rowobject> BY (zcl_do_it=>co_row_seq).

  ENDMETHOD.


  METHOD it_move_down.
* ...

    FIELD-SYMBOLS:
      <it_rowobject> TYPE STANDARD TABLE,
      <rowmark>      TYPE any,
      <rowseq>       TYPE any,
      <prevseq>      TYPE any,
      <wa_rowobject> TYPE any.

    DATA :
      temp_seq       TYPE sydbcnt.
*    next_seq       TYPE sydbcnt.

    ASSIGN ptr_it_rowobject->* TO <it_rowobject>.

    SORT <it_rowobject> BY (zcl_do_it=>co_row_seq) DESCENDING.

*  next_seq = 0.

    LOOP AT <it_rowobject> ASSIGNING <wa_rowobject>.

      ASSIGN :
       COMPONENT zcl_do_it=>co_row_marker
         OF STRUCTURE <wa_rowobject> TO <rowmark>.

      IF <rowmark> = 'X'.
        ASSIGN:
         COMPONENT zcl_do_it=>co_row_seq
           OF STRUCTURE <wa_rowobject> TO <rowseq>.
        temp_seq = <rowseq>.
        IF <prevseq> IS ASSIGNED.
          <rowseq> = <prevseq>.
          <prevseq> = temp_seq.
        ENDIF.
*      CLEAR <rowmark>.
      ELSE.
        ASSIGN:
         COMPONENT zcl_do_it=>co_row_seq
           OF STRUCTURE <wa_rowobject> TO <prevseq>.
      ENDIF.
    ENDLOOP.

    SORT <it_rowobject> BY (zcl_do_it=>co_row_seq).

  ENDMETHOD.


  METHOD it_move_top.
* ...

    FIELD-SYMBOLS:
      <it_rowobject> TYPE STANDARD TABLE,
      <rowmark>      TYPE any,
      <rowseq>       TYPE any,
      <wa_rowobject> TYPE any.

    DATA :
      next_seq       TYPE sydbcnt.

    ASSIGN ptr_it_rowobject->* TO <it_rowobject>.

    SORT <it_rowobject> BY (zcl_do_it=>co_row_seq).

    next_seq = 0.

    LOOP AT <it_rowobject> ASSIGNING <wa_rowobject>.
      ASSIGN :
        COMPONENT zcl_do_it=>co_row_marker
          OF STRUCTURE <wa_rowobject> TO <rowmark>,
        COMPONENT zcl_do_it=>co_row_seq
          OF STRUCTURE <wa_rowobject> TO <rowseq>.
      IF <rowmark> = 'X'.
        next_seq = next_seq + 1.
        <rowseq>  = next_seq.
      ENDIF.
    ENDLOOP.

    LOOP AT <it_rowobject> ASSIGNING <wa_rowobject>.
      ASSIGN :
        COMPONENT zcl_do_it=>co_row_marker
          OF STRUCTURE <wa_rowobject> TO <rowmark>,
        COMPONENT zcl_do_it=>co_row_seq
          OF STRUCTURE <wa_rowobject> TO <rowseq>.
      IF <rowmark> NE 'X'.
        next_seq = next_seq + 1.
        <rowseq>  = next_seq.
      ENDIF.
    ENDLOOP.

*  LOOP AT <it_rowobject> ASSIGNING <wa_rowobject>.
*    ASSIGN :
*      COMPONENT zcl_do_it=>co_row_marker
*        OF STRUCTURE <wa_rowobject> TO <rowmark>.
*    IF <rowmark> = 'X'.
*      CLEAR <rowmark>.
*    ENDIF.
*  ENDLOOP.

    SORT <it_rowobject> BY (zcl_do_it=>co_row_seq).

  ENDMETHOD.


  METHOD it_move_up.
* ...

    FIELD-SYMBOLS:
      <it_rowobject> TYPE STANDARD TABLE,
      <rowmark>      TYPE any,
      <rowseq>       TYPE any,
      <prevseq>      TYPE any,
      <wa_rowobject> TYPE any.

    DATA :
      temp_seq       TYPE sydbcnt.
*    next_seq       TYPE sydbcnt.

    ASSIGN ptr_it_rowobject->* TO <it_rowobject>.

    SORT <it_rowobject> BY (zcl_do_it=>co_row_seq).

*  next_seq = 0.

    LOOP AT <it_rowobject> ASSIGNING <wa_rowobject>.

      ASSIGN :
       COMPONENT zcl_do_it=>co_row_marker
         OF STRUCTURE <wa_rowobject> TO <rowmark>.

      IF <rowmark> = 'X'.
        ASSIGN:
         COMPONENT zcl_do_it=>co_row_seq
           OF STRUCTURE <wa_rowobject> TO <rowseq>.
        temp_seq = <rowseq>.
        IF <prevseq> IS ASSIGNED.
          <rowseq> = <prevseq>.
          <prevseq> = temp_seq.
        ENDIF.
*      CLEAR <rowmark>.
      ELSE.
        ASSIGN:
         COMPONENT zcl_do_it=>co_row_seq
           OF STRUCTURE <wa_rowobject> TO <prevseq>.
      ENDIF.
    ENDLOOP.

    SORT <it_rowobject> BY (zcl_do_it=>co_row_seq).

  ENDMETHOD.


  METHOD it_print.
* ...
  ENDMETHOD.


  METHOD it_refresh.
* ...

    FIELD-SYMBOLS :
      <it_rowobject> TYPE STANDARD TABLE,
      <it_rowobjupd> TYPE STANDARD TABLE,
      <it_rowdb>     TYPE STANDARD TABLE,
      <wa>           TYPE any.

    ASSIGN :
      ptr_wa_rowobject->* TO <wa>,
      ptr_it_rowobject->* TO <it_rowobject>,
      ptr_it_rowobjupd->* TO <it_rowobjupd>,
      ptr_it_rowdb->*     TO <it_rowdb>.


    REFRESH:
       <it_rowdb>,
      <it_rowobject>,
      <it_rowobjupd>.

    CLEAR :
      <wa>,
      tc_current_rowdbcnt.

    i_ro_tb_next_dbcnt = 1.
*  RAISE EVENT work_area_changed.

  ENDMETHOD.


  METHOD it_refresh_and_notify.
* ...
    CALL METHOD me->it_refresh.
    RAISE EVENT work_area_changed.
  ENDMETHOD.


  METHOD it_replace.
* ...
  ENDMETHOD.


  METHOD it_row_created .
* ...

    FIELD-SYMBOLS :
      <row>   TYPE any,
      <mandt> TYPE any,
      <mod>   TYPE any.

    ASSIGN :
      im_ptr_to_row->* TO <row>,
      COMPONENT zcl_do_it=>co_mandt
        OF STRUCTURE <row> TO <mandt>,
      COMPONENT zcl_do_it=>co_row_mod
        OF STRUCTURE <row> TO <mod>.

    <mod> = zcl_do_it=>co_mod_create.
    IF <mandt> IS ASSIGNED.
      <mandt> = sy-mandt.
    ENDIF.

    CALL METHOD me->it_row_map_assign
      EXPORTING
        im_ptr_to_row = im_ptr_to_row.

  ENDMETHOD.


  METHOD it_row_deleted.
* ...
  ENDMETHOD.


  METHOD it_row_empty .
* ...

    FIELD-SYMBOLS :
      <row>   TYPE any,
      <dbcnt> TYPE any,
      <mod>   TYPE any.

    ASSIGN :
      im_ptr_to_row->* TO <row>,
      COMPONENT zcl_do_it=>co_row_db_cnt
        OF STRUCTURE <row> TO <dbcnt>,
      COMPONENT zcl_do_it=>co_row_mod
        OF STRUCTURE <row> TO <mod>.

    <mod> = zcl_do_it=>co_mod_empty.
    IF <dbcnt> = 0.
      i_ro_tb_next_dbcnt = i_ro_tb_next_dbcnt + 1.
      <dbcnt> = i_ro_tb_next_dbcnt.
    ENDIF.
  ENDMETHOD.


  METHOD it_row_loaded .
* ...
  ENDMETHOD.


  METHOD it_row_loaded_begin.
* ...
  ENDMETHOD.


  METHOD it_row_loaded_end.
* ...
  ENDMETHOD.


  METHOD it_row_map_assign.
* ...

    FIELD-SYMBOLS :
      <row>            TYPE any,
      <field>          TYPE any,
      <field_from>     TYPE any,
      <wa_field_value> TYPE LINE OF zif_do_seo=>tt_field_values,
      <wa_map>         TYPE LINE OF zif_do=>tt_field_mapping,
      <wa_maps>        TYPE LINE OF zif_do=>tt_id_field_mapping.

    ASSIGN :
      im_ptr_to_row->* TO <row>.

    LOOP AT it_maps ASSIGNING <wa_maps>
      WHERE active = 'X'
        AND relationship = zcl_do_it=>co_link_data.
      LOOP AT <wa_maps>-map ASSIGNING <wa_map>
        WHERE NOT subscribed_field IS INITIAL .

        ASSIGN :
          COMPONENT <wa_map>-subscribed_field
            OF STRUCTURE <row> TO <field>.
        IF sy-subrc = 0.
          ASSIGN
             <wa_map>-fieldvalue->* TO  <field_from> .
          IF <field_from> IS ASSIGNED.
            <field> =  <field_from>.
          ENDIF.
        ENDIF.

      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD it_row_updated.
* ...

  ENDMETHOD.


  METHOD it_row_validate .
* ...
*
*  FIELD-SYMBOLS :
*    <row>       TYPE ANY.
*
*
*  ASSIGN :
*    im_ptr_to_row->* TO <row>.

  ENDMETHOD.


  METHOD it_selected.
* ...
  ENDMETHOD.


  METHOD it_set_filter_auto_disable.

    lg_auto_disable_filter = im_auto_disable.

  ENDMETHOD.


  METHOD it_set_reposition_on .
* ...

    IF im_active = 'X'.
      it_reposition_on_wa_changed = 'X'.
    ELSE.
      it_reposition_on_wa_changed = space.
    ENDIF.

  ENDMETHOD.


  METHOD it_sort.
* ...

    DATA:
      c_direction TYPE char30,
      str_sort    TYPE string.

    FIELD-SYMBOLS :
      <it_rowobject>    TYPE STANDARD TABLE,
      <field>           TYPE any,
      <wa_rowobject>    TYPE any,
      <wa_sort_columns> TYPE zst_do_sort_columns.

    ASSIGN ptr_it_rowobject->* TO <it_rowobject>.

    SORT it_ro_tb_sort_columns BY sortorder DESCENDING.

    READ TABLE it_ro_tb_sort_columns ASSIGNING <wa_sort_columns> INDEX 1.
    IF sy-subrc = 0.

      CASE <wa_sort_columns>-direction.
        WHEN 'D' OR 'd'.
          SORT <it_rowobject> BY
            (<wa_sort_columns>-fieldname) DESCENDING.
        WHEN OTHERS. "A
          SORT <it_rowobject> BY
            (<wa_sort_columns>-fieldname) ASCENDING.
      ENDCASE.

*    CALL METHOD it_sort_assign_row_seq.

      LOOP AT it_ro_tb_sort_columns ASSIGNING <wa_sort_columns> FROM 2.
        CASE <wa_sort_columns>-direction.
          WHEN 'D' OR 'd'.
            SORT <it_rowobject> STABLE
                BY   (<wa_sort_columns>-fieldname) DESCENDING .
          WHEN OTHERS. "A
            SORT <it_rowobject> STABLE
              BY (<wa_sort_columns>-fieldname) ASCENDING .
        ENDCASE.

      ENDLOOP.
*   CALL METHOD it_sort_assign_row_seq.

    ENDIF.
    SORT it_ro_tb_sort_columns BY sortorder ASCENDING.

  ENDMETHOD.


  METHOD it_sort_ask.
* ...

    FIELD-SYMBOLS:
      <wa_zrowobject_structure> LIKE LINE OF it_zrowobject_structure,
      <wa_ro_tb_structure>      LIKE LINE OF it_ro_tb_structure.

    DATA :
      it_field_list TYPE ztt_do_field_list,
      wa_field_list TYPE LINE OF ztt_do_field_list.

    LOOP AT it_ro_tb_structure ASSIGNING <wa_ro_tb_structure> WHERE fieldname NE 'MANDT'.
      wa_field_list-fieldname   = <wa_ro_tb_structure>-fieldname.
      wa_field_list-fieldlabel  = <wa_ro_tb_structure>-scrtext_m.
      APPEND wa_field_list TO it_field_list.
    ENDLOOP.
    IF im_include_row_object = 'X'.
      LOOP AT it_zrowobject_structure ASSIGNING <wa_zrowobject_structure>.
        wa_field_list-fieldname   = <wa_zrowobject_structure>-fieldname.
        wa_field_list-fieldlabel  = <wa_zrowobject_structure>-scrtext_m.
        APPEND wa_field_list TO it_field_list.
      ENDLOOP.
    ENDIF.

    CALL FUNCTION 'Z_DO_TB_TOOLS_01_SORT'
      TABLES
        it_sort_columns = it_ro_tb_sort_columns
        it_field_list   = it_field_list
      EXCEPTIONS
        cancel          = 1
        no_chnages      = 2
        OTHERS          = 3.

    IF sy-subrc = 0.
      CALL METHOD me->it_sort.
    ELSEIF it_ro_tb_sort_columns_prev[] NE it_ro_tb_sort_columns[].
      CALL METHOD me->it_sort.
    ENDIF.

  ENDMETHOD.


  METHOD it_sort_assign_row_seq.
* ...

    FIELD-SYMBOLS :
      <it_rowobject> TYPE STANDARD TABLE,
      <field>        TYPE any,
      <wa_rowobject> TYPE any.

    ASSIGN ptr_it_rowobject->* TO <it_rowobject>.

    LOOP AT <it_rowobject>  ASSIGNING <wa_rowobject>.
      ASSIGN
        COMPONENT zcl_do_it=>co_row_seq
          OF STRUCTURE <wa_rowobject>
          TO <field>.
      <field> = sy-tabix.
    ENDLOOP.

  ENDMETHOD.


  METHOD it_update.
* ...

    DATA:
      l_ptr_row        TYPE REF TO data.

    FIELD-SYMBOLS :
      <wa_rowobject> TYPE any,
      <it_rowobject> TYPE STANDARD TABLE.

    ASSIGN :
      ptr_it_rowobject->*  TO <it_rowobject>.

    READ TABLE <it_rowobject> ASSIGNING <wa_rowobject>
      WITH KEY (zcl_do_it=>co_row_db_cnt) = im_db_cnt.

    IF sy-subrc = 0.
      GET REFERENCE OF <wa_rowobject> INTO l_ptr_row.
      CALL METHOD me->ru_update
        EXPORTING
          im_ptr_to_row = l_ptr_row.
    ENDIF.

  ENDMETHOD.


  METHOD it_validate.
* ...
  ENDMETHOD.


  METHOD lk_add_link .
* ...

*IM_PTR_PUBLISHER
*IM_PTR_SUBSCRIBER
*IM_IT_RELATIONSHIP

    DATA :
      ptr_subscriber TYPE REF TO zcl_do_it,
      wa_map         TYPE LINE OF zif_do=>tt_field_mapping,
      wa_maps        TYPE LINE OF zif_do=>tt_id_field_mapping.

    FIELD-SYMBOLS :
      <wa_relationship> LIKE LINE OF im_it_relationship,
      <wa_maps>         TYPE LINE OF zif_do=>tt_id_field_mapping,
      <wa_mapping>      TYPE LINE OF zif_do=>tt_field_mapping.

*    BEGIN OF zdo_st_do_id_field_mapping,
*    id TYPE zdo_object_id,
*    map TYPE  ZIF_DO=>ZDO_TT_do_field_mapping,

    IF im_ptr_subscriber IS INITIAL.
      ptr_subscriber = me.
    ELSE.
      ptr_subscriber = im_ptr_subscriber.
    ENDIF.

    CALL METHOD im_ptr_publisher->add_published
      EXPORTING
        im_id           = ptr_subscriber->id
        im_relationship = im_relationship
        im_ptr_object   = ptr_subscriber.

    CALL METHOD ptr_subscriber->add_subscribed
      EXPORTING
        im_id           = im_ptr_publisher->id
        im_relationship = im_relationship
        im_ptr_object   = im_ptr_publisher.

    CASE im_relationship.
      WHEN zcl_do_it=>co_link_data
        OR zcl_do_it=>co_link_reposition
        OR zcl_do_it=>co_link_data_filter.
        SET HANDLER  ptr_subscriber->on_work_area_changed
            FOR im_ptr_publisher.
        READ TABLE it_maps
          WITH KEY relationship = im_relationship
                   id = im_ptr_publisher->id
          ASSIGNING <wa_maps>.
        IF sy-subrc = 0.
          REFRESH <wa_maps>-map.
          LOOP AT im_it_relationship ASSIGNING <wa_relationship>.
*          wa_map-fieldvalue
            wa_map-relationship_seq = <wa_relationship>-relationship_seq.
            wa_map-published_field  = <wa_relationship>-published_field.
            wa_map-subscribed_field = <wa_relationship>-subscribed_field.
            wa_map-foreign_key = <wa_relationship>-foreignkey.
            APPEND  wa_map TO  <wa_maps>-map.
          ENDLOOP.
        ELSE.
          wa_maps-relationship = im_relationship.
          wa_maps-id = im_ptr_publisher->id.
          wa_maps-active = 'X'.
*        it_active_publisher_id = im_ptr_publisher->id.
          LOOP AT im_it_relationship ASSIGNING <wa_relationship>.
*          wa_map-fieldvalue
            wa_map-relationship_seq = <wa_relationship>-relationship_seq.
            wa_map-published_field  = <wa_relationship>-published_field.
            wa_map-subscribed_field = <wa_relationship>-subscribed_field.
            wa_map-foreign_key = <wa_relationship>-foreignkey.
            APPEND  wa_map TO  wa_maps-map.
          ENDLOOP.
          APPEND wa_maps TO it_maps.
        ENDIF.
    ENDCASE.

  ENDMETHOD.


  METHOD lk_deactivate_all_publishers .
* ...

    FIELD-SYMBOLS :
      <wa_maps>          TYPE LINE OF zif_do=>tt_id_field_mapping.

    LOOP AT it_maps ASSIGNING <wa_maps>
      WHERE relationship = im_relationship
        AND active = 'X'.
      CLEAR <wa_maps>-active.
    ENDLOOP.
  ENDMETHOD.


  METHOD lk_set_active_publisher_id.
* ...
*  it_active_publisher_id  = im_publisher_id.

    FIELD-SYMBOLS :
      <wa_maps>          TYPE LINE OF zif_do=>tt_id_field_mapping.

    READ TABLE it_maps ASSIGNING <wa_maps>
      WITH KEY relationship = im_relationship
               id = im_publisher_id.
    IF sy-subrc = 0.
      <wa_maps>-active = 'X'.
    ENDIF.
  ENDMETHOD.


  METHOD load_it_main_table_fields.
* ...

    DATA :
        it_fields TYPE STANDARD TABLE OF dfies.
    FIELD-SYMBOLS
      <wa> TYPE  dfies.

    CASE im_field_list.
      WHEN '*'.
        REFRESH it_db_tb_fields.
        wa_db_tb_fields-fieldname = '*'.
        APPEND wa_db_tb_fields TO it_db_tb_fields.
      WHEN 'ALL'.
        REFRESH it_db_tb_fields.
        LOOP AT it_db_tb_structure ASSIGNING <wa>.
          wa_db_tb_fields-fieldname = <wa>-fieldname.
          APPEND wa_db_tb_fields TO it_db_tb_fields.
        ENDLOOP.
      WHEN OTHERS.
        SPLIT im_field_list AT zcl_do_it=>co_field_list
          INTO TABLE it_db_tb_fields.
    ENDCASE.

  ENDMETHOD.


  METHOD on_work_area_changed.
* ...

    DATA :
      lg_publish      TYPE char1,
      index_rowobject TYPE sytabix,
      c_match         TYPE zdo_filter,
      ptr_selection   TYPE REF TO zcl_do_selection_options,
      lg_change_where TYPE char1,
      i_times         TYPE i,
      c_relationship  TYPE zdo_object_relationship,
      c_forgein_key   TYPE char1,
      wa_field_value  TYPE LINE OF zif_do_seo=>tt_field_values,
      it_field_value  TYPE zif_do_seo=>tt_field_values.

    FIELD-SYMBOLS :
      <tc>             TYPE cxtab_control,
      <wa_dbcnt>       TYPE any,
      <fieldvalue>     TYPE any,
      <it_rowobject>   TYPE STANDARD TABLE,
      <wa_rowobject>   TYPE any,
      <wa_field_value> TYPE LINE OF zif_do_seo=>tt_field_values,
      <wa_map>         TYPE LINE OF zif_do=>tt_field_mapping,
      <wa_maps>        TYPE LINE OF zif_do=>tt_id_field_mapping,
      <low>            TYPE any.

    DO 3 TIMES.
      CLEAR :
        c_match,
        c_forgein_key,
        lg_change_where.
      i_times = sy-index.

      CASE i_times.
        WHEN 1.
          c_relationship = zcl_do_it=>co_link_data.
        WHEN 2.
          c_relationship = zcl_do_it=>co_link_data_filter.
        WHEN 3.
          c_relationship = zcl_do_it=>co_link_reposition.
      ENDCASE.

      READ TABLE it_maps ASSIGNING <wa_maps>
        WITH KEY  relationship = c_relationship
                  active = 'X'
                  id = sender->id.

      IF sy-subrc = 0.

        REFRESH it_field_value.

*Build a List of Fields to ask for the values
        LOOP AT <wa_maps>-map ASSIGNING <wa_map>.
          wa_field_value-fieldname = <wa_map>-published_field.
          APPEND wa_field_value TO it_field_value.
          CLEAR <wa_map>-fieldvalue.
        ENDLOOP.

*    ENDIF.

*Get the values from the Sendor
        CALL METHOD sender->wa_get_field_value
          CHANGING
            ch_it_field_values = it_field_value.

*Save the values to the map
        LOOP AT it_field_value ASSIGNING <wa_field_value>.
          READ TABLE <wa_maps>-map ASSIGNING <wa_map>
            WITH KEY published_field = <wa_field_value>-fieldname.
          IF sy-subrc = 0.
            <wa_map>-fieldvalue = <wa_field_value>-fieldvalue.
          ENDIF.
        ENDLOOP.

*Determine the selection Object
        CASE c_relationship.
          WHEN zcl_do_it=>co_link_data.
            IF db_sync_where_on_wa_changed = 'X'.
              lg_change_where = 'X'.
              ptr_selection = me->ptr_db_tb_where.
            ENDIF.
          WHEN zcl_do_it=>co_link_data_filter.
            lg_change_where = 'X'.
            ptr_selection = me->ptr_ro_tb_where.
          WHEN zcl_do_it=>co_link_reposition.
            IF it_reposition_on_wa_changed = 'X'.
              lg_change_where = 'X'.
              ptr_selection = me->ptr_ro_tb_repos.
            ENDIF.
        ENDCASE.

*Rebuild where or position  keep in sync
        IF lg_change_where = 'X'.
          READ  TABLE  <wa_maps>-map WITH KEY foreign_key = 'X'
            TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            c_forgein_key = 'X'.
          ENDIF.

          IF c_forgein_key = 'X'.

            LOOP AT <wa_maps>-map ASSIGNING <wa_map>
              WHERE foreign_key = 'X'.
              ASSIGN
                 <wa_map>-fieldvalue->* TO <low>.
              CALL METHOD ptr_selection->delete_condition
                EXPORTING
                  im_field_name = <wa_map>-subscribed_field.

              CALL METHOD ptr_selection->add_condition
                EXPORTING
                  im_field_name = <wa_map>-subscribed_field
                  im_low        = <low>.
            ENDLOOP.

            CASE  c_relationship.

              WHEN zcl_do_it=>co_link_data.
                CALL METHOD me->db_where_from_ptr_db_where.
                IF db_reposition_on_wa_changed = 'X'.
                  CALL METHOD me->tc_refresh
                    EXPORTING
                      im_publish_work_area_changed = space.
                  lg_publish = 'X'.
                  db_refresh_needed = space.
* WAIT TO RAISe WORK ARREA CHANGED.
                ELSE.
                  CALL METHOD me->it_refresh.
                  CALL METHOD tc_initialize.
                  lg_publish = 'X'.
                  db_refresh_needed = 'X'.
*                RAISE EVENT work_area_changed.
                ENDIF.
              WHEN zcl_do_it=>co_link_data_filter.
                CALL METHOD:
                  me->it_data_filter_exit,
                  me->it_filter
                    EXPORTING
                       im_publish_work_area_changed = space.
                lg_publish = 'X'.
              WHEN zcl_do_it=>co_link_reposition.
                ASSIGN:
                    ptr_it_rowobject->* TO <it_rowobject>.
                LOOP AT <it_rowobject> ASSIGNING <wa_rowobject>.
                  CALL METHOD ptr_selection->clear_value.
                  LOOP AT it_field_value ASSIGNING <wa_field_value>.
                    READ TABLE <wa_maps>-map ASSIGNING <wa_map>  WITH KEY
                       published_field = <wa_field_value>-fieldname.
                    IF sy-subrc = 0.
                      ASSIGN:
                        COMPONENT <wa_map>-subscribed_field
                          OF STRUCTURE  <wa_rowobject>
                          TO <fieldvalue>.
                      CALL METHOD ptr_selection->assign_value
                        EXPORTING
                          im_field_name = <wa_map>-subscribed_field
                          im_value      = <fieldvalue>.
                    ENDIF.
                  ENDLOOP.
                  CALL METHOD ptr_selection->is_match
                    IMPORTING
                      ex_match = c_match.
                  IF c_match = 'X'.
                    index_rowobject = sy-tabix.
                    EXIT.
                  ENDIF.
                ENDLOOP.
                IF c_match = 'X'.
                  ASSIGN :
                     ptr_wa_rowobject->* TO <wa_rowobject>.
                  READ TABLE  <it_rowobject> INDEX index_rowobject
                    INTO <wa_rowobject>.
                  ASSIGN:
                      COMPONENT zcl_do_it=>co_row_db_cnt
                      OF STRUCTURE  <wa_rowobject> TO <wa_dbcnt>.
                  IF tc_current_rowdbcnt NE <wa_dbcnt>.
                    tc_current_rowdbcnt = <wa_dbcnt>.
                    lg_publish = 'X'.
                  ENDIF.
                  ASSIGN :
                      ptr_tb_control->* TO <tc>.
                  IF sy-subrc = 0.
* If reposiiotn not in display then reposiiton.
                    IF NOT ( <tc>-top_line + tc_displayed_lines > index_rowobject AND
                         <tc>-top_line < <tc>-top_line ).
                      <tc>-top_line = index_rowobject.
                    ENDIF.
                  ENDIF.
                ENDIF.

            ENDCASE.

          ENDIF.  " IF c_forgein_key = 'X'.

        ENDIF.  " IF lg_change_where = 'X'

      ENDIF.  "  it_maps  was found

    ENDDO.
    IF lg_publish = 'X'.
      RAISE EVENT work_area_changed.
    ENDIF.

  ENDMETHOD.


  METHOD ru_update .
* ...

    DATA:
      prev_rowmod TYPE zdo_rowmod,
      c_match_key TYPE abap_bool,
      c_match(1)  TYPE c.

    FIELD-SYMBOLS :
      <row>            TYPE any,
      <rowmod>         TYPE any,
      <rowmod_rowdb>   TYPE any,

      <dbcnt>          TYPE any,
      <wa_rowobjupd>   TYPE any,
      <rowobjectfield> TYPE any,
      <rowdbfield>     TYPE any,
      <wa_rowdb>       TYPE any,
      <wa_fieldtab>    TYPE dfies,
      <it_rowdb>       TYPE STANDARD TABLE,
      <it_rowobjupd>   TYPE STANDARD TABLE.

    ASSIGN :
      im_ptr_to_row->*     TO <row>,
      ptr_it_rowdb->*      TO <it_rowdb>,
      ptr_it_rowobjupd->*  TO <it_rowobjupd>,
      COMPONENT zcl_do_it=>co_row_db_cnt
        OF STRUCTURE <row> TO <dbcnt>,
      COMPONENT zcl_do_it=>co_row_mod
        OF STRUCTURE <row> TO <rowmod>.

    CASE <rowmod>.

*If any field has changed value since read from the database'
*flag the row as updated and added it to the row object update table
*if nothing has changed since read from the database delete
*the record from the row object update table if it exists
      WHEN zcl_do_it=>co_mod_update OR
           zcl_do_it=>co_mod_original.
        READ TABLE <it_rowdb> ASSIGNING <wa_rowdb>
          WITH KEY (zcl_do_it=>co_row_db_cnt) = <dbcnt>.
        IF sy-subrc = 0.

          " Store the rowmarked into it_Rowdb incase filter is adjusted it will bring back the marked rows

          ASSIGN :
              COMPONENT zcl_do_it=>co_row_marker
                OF STRUCTURE <wa_rowdb>
                TO <rowdbfield>,
              COMPONENT zcl_do_it=>co_row_marker
                OF STRUCTURE <row>
                TO <rowobjectfield>.
          <rowdbfield> = <rowobjectfield>.

          c_match = 'X'.
*        IF <wa_rowdb> NE <row>.
*          CLEAR : c_match.
*        ENDIF.
          LOOP AT it_ro_tb_structure  ASSIGNING <wa_fieldtab>.
            ASSIGN :
              COMPONENT <wa_fieldtab>-fieldname
                OF STRUCTURE <wa_rowdb>
                TO <rowdbfield>,
              COMPONENT <wa_fieldtab>-fieldname
                OF STRUCTURE <row>
                TO <rowobjectfield>.
            IF <rowdbfield> NE <rowobjectfield>.
              CLEAR : c_match.
              EXIT.
            ENDIF.
          ENDLOOP.

          IF c_match IS INITIAL.
* If something changed Determine if key changed
*Compare the keys to itrowdb...  If the key changes then set flag
* The flag will be used to delete the original and mark the
* new as updated
            c_match_key = abap_true.
            LOOP AT it_db_tb_structure ASSIGNING <wa_fieldtab> WHERE keyflag = 'X'.
              ASSIGN :
                COMPONENT <wa_fieldtab>-fieldname
                  OF STRUCTURE <wa_rowdb>
                  TO <rowdbfield>,
                COMPONENT <wa_fieldtab>-fieldname
                  OF STRUCTURE <row>
                  TO <rowobjectfield>.
              IF <rowdbfield> NE <rowobjectfield>.
                CLEAR c_match_key.
                EXIT.
              ENDIF.
            ENDLOOP.

            CALL METHOD :
              me->it_row_updated
                EXPORTING
                  im_ptr_to_row = im_ptr_to_row,
              me->it_row_validate
                EXPORTING
                  im_ptr_to_row = im_ptr_to_row.
            <rowmod> =  zcl_do_it=>co_mod_update.
            IF c_match_key = abap_true.  " Key did not Change
              READ TABLE <it_rowobjupd> ASSIGNING <wa_rowobjupd>
                  WITH KEY (zcl_do_it=>co_row_db_cnt) = <dbcnt>.
            ELSE. "Key Changed Then update the existing with U and create Deletion of rowdb record
              READ TABLE <it_rowobjupd> ASSIGNING <wa_rowobjupd>
                  WITH KEY (zcl_do_it=>co_row_db_cnt) = <dbcnt>
                           (zcl_do_it=>co_row_mod) = zcl_do_it=>co_mod_update.
            ENDIF.
            IF sy-subrc = 0.
              MODIFY <it_rowobjupd>
                FROM <row>
                INDEX sy-tabix.
            ELSE.
              APPEND <row> TO <it_rowobjupd>.
            ENDIF.
            IF c_match_key = abap_true. "Key Changed Then update the existing with U and create Deletion of rowdb record
              " Look for Deletion if Key was changed.
              READ TABLE <it_rowobjupd> ASSIGNING <wa_rowobjupd>
                WITH KEY (zcl_do_it=>co_row_db_cnt) = <dbcnt>
                         (zcl_do_it=>co_row_mod) = zcl_do_it=>co_mod_delete.
              IF sy-subrc = 0.
                DELETE <it_rowobjupd> INDEX sy-tabix.
              ENDIF.
            ELSE.
              READ TABLE <it_rowobjupd> ASSIGNING <wa_rowobjupd>
                WITH KEY (zcl_do_it=>co_row_db_cnt) = <dbcnt>
                         (zcl_do_it=>co_row_mod) = zcl_do_it=>co_mod_delete.
              IF sy-subrc = 0.
                MODIFY <it_rowobjupd>
                   FROM <wa_rowdb>
                   INDEX sy-tabix.
              ELSE.
                ASSIGN :
                   COMPONENT zcl_do_it=>co_row_mod
                      OF STRUCTURE <wa_rowdb>
                      TO <rowmod_rowdb>.
                prev_rowmod = <rowmod_rowdb>.
                <rowmod_rowdb> =   zcl_do_it=>co_mod_delete.  "Temporary Assign to Delete
                APPEND <wa_rowdb> TO <it_rowobjupd>.
                <rowmod_rowdb>  = prev_rowmod.
              ENDIF.
            ENDIF.
          ELSE.
            IF <rowmod> = zcl_do_it=>co_mod_update.
* Set mod back to orginal
              <rowmod> =  zcl_do_it=>co_mod_original.
            ENDIF.
            " Look for Deletion if Key was changed.
            READ TABLE <it_rowobjupd> ASSIGNING <wa_rowobjupd>
              WITH KEY (zcl_do_it=>co_row_db_cnt) = <dbcnt>
                       (zcl_do_it=>co_row_mod) = zcl_do_it=>co_mod_delete.
            IF sy-subrc = 0.
              DELETE <it_rowobjupd> INDEX sy-tabix.
            ENDIF.
            " Look for any other Update / Creation Record
            READ TABLE <it_rowobjupd> ASSIGNING <wa_rowobjupd>
                WITH KEY (zcl_do_it=>co_row_db_cnt) = <dbcnt>.
            IF sy-subrc = 0.
              DELETE <it_rowobjupd> INDEX sy-tabix.
            ENDIF.

          ENDIF.
        ELSE.

        ENDIF.

* If any field has a value not equal to initial then assume it is a new
* row and marked the row as created and added it to the
* rowobject UPdate
      WHEN zcl_do_it=>co_mod_empty.
        CLEAR c_match.
        LOOP AT it_ro_tb_structure  ASSIGNING <wa_fieldtab>.
          ASSIGN :
            COMPONENT <wa_fieldtab>-fieldname
              OF STRUCTURE <row>
              TO <rowobjectfield>.
          IF NOT <rowobjectfield> IS INITIAL.
            c_match = 'X'.
            EXIT.
          ENDIF.
        ENDLOOP.
        IF c_match = 'X'.
          CALL METHOD :
            me->it_row_created
              EXPORTING
                im_ptr_to_row = im_ptr_to_row,
             me->it_row_updated
              EXPORTING
                im_ptr_to_row = im_ptr_to_row,
             me->it_row_validate
              EXPORTING
                im_ptr_to_row = im_ptr_to_row.
          APPEND <row> TO <it_rowobjupd>.
        ENDIF.

* Update Row Object Update with the current row object record
      WHEN zcl_do_it=>co_mod_create.
        CALL METHOD :
          me->it_row_updated
            EXPORTING
              im_ptr_to_row = im_ptr_to_row,
          me->it_row_validate
            EXPORTING
              im_ptr_to_row = im_ptr_to_row.
        READ TABLE <it_rowobjupd> ASSIGNING <wa_rowobjupd>
            WITH KEY (zcl_do_it=>co_row_db_cnt) = <dbcnt>.
        IF sy-subrc = 0.
          MODIFY <it_rowobjupd>
            FROM <row>
            INDEX sy-tabix.
        ELSE.
          APPEND <row> TO <it_rowobjupd>.
        ENDIF.

      WHEN zcl_do_it=>co_mod_delete.
        READ TABLE <it_rowdb> ASSIGNING <wa_rowdb>
          WITH KEY (zcl_do_it=>co_row_db_cnt) = <dbcnt>.
        IF sy-subrc = 0.

          READ TABLE <it_rowobjupd>
              WITH KEY (zcl_do_it=>co_row_db_cnt) = <dbcnt>
              TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            MODIFY <it_rowobjupd> FROM <row> INDEX sy-tabix.
          ELSE.
            APPEND <row> TO <it_rowobjupd>.
          ENDIF.

        ELSE.

          READ TABLE <it_rowobjupd>
            WITH KEY (zcl_do_it=>co_row_db_cnt) = <dbcnt>
            TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            DELETE <it_rowobjupd> INDEX sy-tabix.
          ENDIF.

        ENDIF.

      WHEN OTHERS.
        CALL METHOD me->it_row_empty
          EXPORTING
            im_ptr_to_row = im_ptr_to_row.
    ENDCASE.

  ENDMETHOD.


  METHOD set_function_code.
* ...

    DATA :
      wa_fcodes TYPE zif_do=>ts_function_codes.

    CLEAR  wa_fcodes-fcode.
    READ TABLE it_fcodes INTO wa_fcodes
      WITH TABLE KEY fcode = im_function_code.
    wa_fcodes-use_method = im_method_to_use.
    wa_fcodes-update_only = im_update_only.
    IF wa_fcodes-fcode IS INITIAL.
      wa_fcodes-fcode = im_function_code.
      INSERT wa_fcodes INTO TABLE it_fcodes.
    ELSE.
      MODIFY TABLE it_fcodes FROM wa_fcodes.
    ENDIF.

  ENDMETHOD.


  METHOD set_rowobject.
* ...
  ENDMETHOD.


  METHOD tc_append .
* ...

*THIs method will append to the table one page full if attached to table
*control else it will append 1 record.

    FIELD-SYMBOLS <tc>            TYPE cxtab_control.

    ASSIGN :
      ptr_tb_control->* TO <tc>.

    CALL METHOD it_append
      EXPORTING
        im_lines = im_lines.

    <tc>-lines = <tc>-lines + im_lines.
  ENDMETHOD.


  METHOD tc_change_attribute.
* ...
    FIELD-SYMBOLS :
      <it_rowobject> TYPE STANDARD TABLE,
      <tc>           TYPE cxtab_control.

    ASSIGN :
      ptr_it_rowobject->*   TO <it_rowobject>,
      ptr_tb_control->* TO <tc>.

    DESCRIBE TABLE <it_rowobject> LINES <tc>-lines.
    IF <tc>-lines = 0.
      IF lg_readonly = zcl_do_it=>co_true OR tc_lg_add_blank_rows = zcl_do_it=>co_false.
        <tc>-lines = -1.
      ELSEIF tc_lg_add_blank_rows = zcl_do_it=>co_true.
        CALL METHOD me->it_append
          EXPORTING
            im_lines = tc_displayed_lines.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD tc_change_group.
* ...

    FIELD-SYMBOLS :
      <group> TYPE any,
      <tc>    TYPE cxtab_control,
      <cols>  TYPE cxtab_control-cols,
      <wa>    TYPE LINE OF cxtab_control-cols.

    ASSIGN :
      ptr_tb_control->* TO <tc>,
      <tc>-cols         TO <cols>.

    LOOP AT <cols> ASSIGNING <wa>.
      DO 4 TIMES.
        CASE sy-index.
          WHEN 1.
            ASSIGN <wa>-screen-group1 TO  <group>.
          WHEN 2.
            ASSIGN <wa>-screen-group2 TO  <group>.
          WHEN 3.
            ASSIGN <wa>-screen-group3 TO  <group>.
          WHEN 4.
            ASSIGN <wa>-screen-group4 TO  <group>.
        ENDCASE.

        CASE <group>.
          WHEN im_group.
            CASE im_screen_mode.
              WHEN 'I'.
                <wa>-screen-input = 1.
              WHEN 'O'.
                <wa>-screen-input = 0.
*          WHEN 'I'.
*            <wa>-screen-invisible = 1.
            ENDCASE.
        ENDCASE.
      ENDDO.

    ENDLOOP.
  ENDMETHOD.


  METHOD tc_choose .
* ...

    DATA :
      l_tc_name  TYPE fieldname,
      cfield(30) TYPE c,
      newpos     TYPE sytabix,
      linepos    TYPE sytabix.

    FIELD-SYMBOLS :
      <wa_dbcnt>     TYPE any,
      <wa_rowobject> TYPE any,
      <it_rowobject> TYPE STANDARD TABLE,
      <tc>           TYPE cxtab_control.

    CLEAR :
      cfield,
      linepos,
      l_tc_name.
    GET CURSOR FIELD cfield LINE linepos AREA l_tc_name.
    IF l_tc_name = tc_name.
      ASSIGN :
         ptr_wa_rowobject->* TO <wa_rowobject>,
         ptr_it_rowobject->* TO <it_rowobject>,
         ptr_tb_control->* TO <tc>.

      ro_tb_search_parms-zcursor-field = cfield.
      ro_tb_search_parms-zcursor-fieldoffset = 0.
      ro_tb_search_parms-zcursor-line = linepos.
      ro_tb_search_parms-setcursor = 'X'.

      IF sy-subrc = 0 AND linepos > 0.
        newpos = linepos + <tc>-top_line - 1.
        READ TABLE  <it_rowobject> INDEX newpos INTO <wa_rowobject>.
        IF sy-subrc NE 0.
          CLEAR <wa_rowobject>.
        ENDIF.
        ASSIGN:
            COMPONENT zcl_do_it=>co_row_db_cnt
            OF STRUCTURE  <wa_rowobject> TO <wa_dbcnt>.
        tc_current_rowdbcnt = <wa_dbcnt>.

        RAISE EVENT work_area_changed.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD tc_column_filter .
* ...

    FIELD-SYMBOLS:
      <wa_sort_columns> LIKE LINE OF it_ro_tb_sort_columns,
      <wa_cols>         TYPE cxtab_column,
      <cols>            TYPE cxtab_control-cols, "y_it_cols,
      <tc>              TYPE cxtab_control.

    DATA :
      it_fieldname     TYPE ztt_do_fieldname,
      wa_fieldname     LIKE LINE OF it_fieldname,
      l_temp(160)      TYPE c,
      l_sortfield(160) TYPE c.

    ASSIGN :
      ptr_tb_control->*   TO <tc>,
      <tc>-cols           TO <cols>.

    READ TABLE <cols> ASSIGNING  <wa_cols> WITH KEY selected = 'X'.
    IF sy-subrc = 0.
      LOOP AT <cols> ASSIGNING <wa_cols> WHERE selected = 'X'.
        l_sortfield = <wa_cols>-screen-name.
        WHILE l_sortfield CS '-'.
          SPLIT l_sortfield AT '-' INTO l_temp l_sortfield.
        ENDWHILE.
        wa_fieldname = l_sortfield.
        APPEND wa_fieldname TO it_fieldname.
        CLEAR <wa_cols>-selected.
      ENDLOOP.

      CALL METHOD me->ptr_ro_tb_where->ask_selection_options
        EXPORTING
          im_it_fieldname = it_fieldname
        EXCEPTIONS
          no_changes      = 1
          OTHERS          = 2.
      IF sy-subrc = 0.
        CALL METHOD me->it_filter.
      ENDIF.
*    LOOP AT <it_rowobject>  ASSIGNING <wa_rowobject>.
*      ASSIGN
*        COMPONENT zcl_do_it=>co_row_seq
*          OF STRUCTURE <wa_rowobject>
*          TO <field>.
*      <field> = sy-tabix.
*    ENDLOOP.
    ELSE.
      CALL METHOD me->it_filter_ask.

    ENDIF.

  ENDMETHOD.


  METHOD tc_column_mark_filter .
* ...

    FIELD-SYMBOLS:
      <wa_sort_columns> LIKE LINE OF it_ro_tb_sort_columns,
      <wa_cols>         TYPE cxtab_column,
      <cols>            TYPE cxtab_control-cols, "y_it_cols,
      <tc>              TYPE cxtab_control.

    DATA :
      it_fieldname     TYPE ztt_do_fieldname,
      wa_fieldname     LIKE LINE OF it_fieldname,
      l_temp(160)      TYPE c,
      l_sortfield(160) TYPE c.

    ASSIGN :
      ptr_tb_control->*   TO <tc>,
      <tc>-cols           TO <cols>.

    READ TABLE <cols> ASSIGNING  <wa_cols> WITH KEY selected = 'X'.
    IF sy-subrc = 0.
      LOOP AT <cols> ASSIGNING <wa_cols> WHERE selected = 'X'.
        l_sortfield = <wa_cols>-screen-name.
        WHILE l_sortfield CS '-'.
          SPLIT l_sortfield AT '-' INTO l_temp l_sortfield.
        ENDWHILE.
        wa_fieldname = l_sortfield.
        APPEND wa_fieldname TO it_fieldname.
        CLEAR <wa_cols>-selected.
      ENDLOOP.

      CALL METHOD me->ptr_ro_tb_mark->ask_selection_options
        EXPORTING
          im_it_fieldname = it_fieldname
        EXCEPTIONS
          no_changes      = 1
          OTHERS          = 2.
      IF sy-subrc = 0.
        CALL METHOD me->it_mark_filter.
      ENDIF.
*    LOOP AT <it_rowobject>  ASSIGNING <wa_rowobject>.
*      ASSIGN
*        COMPONENT zcl_do_it=>co_row_seq
*          OF STRUCTURE <wa_rowobject>
*          TO <field>.
*      <field> = sy-tabix.
*    ENDLOOP.
    ELSE.
      CALL METHOD me->it_mark_filter_ask.

    ENDIF.

  ENDMETHOD.


  METHOD tc_column_sort .
* ...

    FIELD-SYMBOLS:
      <wa_cols> TYPE cxtab_column,
      <cols>    TYPE cxtab_control-cols,
      <tc>      TYPE cxtab_control.

    DATA :
      ilines           TYPE i,
      l_temp(160)      TYPE c,
      wa_sort_columns  LIKE LINE OF it_ro_tb_sort_columns,
      l_sortfield(160) TYPE c.

    ASSIGN :
      ptr_tb_control->*   TO <tc>,
      <tc>-cols           TO <cols>.

    it_ro_tb_sort_columns_prev[] = it_ro_tb_sort_columns[].

    READ TABLE <cols> ASSIGNING  <wa_cols> WITH KEY selected = 'X'.
    IF sy-subrc = 0.
      REFRESH it_ro_tb_sort_columns.
      ilines  = 0.
      LOOP AT <cols> ASSIGNING <wa_cols> WHERE selected = 'X'.
        l_sortfield = <wa_cols>-screen-name.
        WHILE l_sortfield CS '-'.
          SPLIT l_sortfield AT '-' INTO l_temp l_sortfield.
        ENDWHILE.
        ilines = ilines + 1.
        wa_sort_columns-fieldname = l_sortfield.
        wa_sort_columns-sortorder = ilines.

        CASE im_method.
          WHEN 'COL_U'.
            wa_sort_columns-direction = 'A'.
          WHEN 'COL_D'.
            wa_sort_columns-direction = 'D'.
        ENDCASE.

        APPEND wa_sort_columns TO it_ro_tb_sort_columns.
        CLEAR <wa_cols>-selected.
      ENDLOOP.

      IF ilines = 1.
        CALL METHOD me->it_sort.
      ELSEIF ilines > 1.
        CALL METHOD me->it_sort_ask.
      ENDIF.
    ELSEIF ro_tb_multi_sort_allowed = 'X'.
      CALL METHOD me->it_sort_ask.

    ENDIF.

  ENDMETHOD.


  METHOD tc_copy .
* ...
    DATA :
      cursor_line TYPE i,
      ilines      TYPE i.

    FIELD-SYMBOLS :
      <wa>           TYPE any,
      <tc>           TYPE cxtab_control,
      <it_rowobject> TYPE STANDARD TABLE,
      <dbcnt>        TYPE any.

    ASSIGN :
      ptr_tb_control->*   TO <tc>,
      ptr_it_rowobject->* TO <it_rowobject>.

    DESCRIBE TABLE <it_rowobject> LINES ilines.

    IF lg_copy_cursor = 'X'.
      GET CURSOR LINE cursor_line.
      IF sy-subrc = 0.

      ENDIF.
* AREA a
    ENDIF.

    CALL METHOD it_copy.

    DESCRIBE TABLE <it_rowobject> LINES <tc>-lines.

    IF <tc>-lines NE ilines.
      ilines = ilines + 1.

      READ TABLE <it_rowobject> ASSIGNING <wa> INDEX ilines.
      IF sy-subrc = 0.
        ASSIGN
           COMPONENT zcl_do_it=>co_row_db_cnt
              OF STRUCTURE <wa> TO <dbcnt>.

        CALL METHOD wa_from_it
          EXPORTING
            im_db_cnt = <dbcnt>.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD tc_delete .
* ...

    DATA :
      tc_top_line     TYPE sy-dbcnt.

    FIELD-SYMBOLS :
      <lines>        TYPE i,
      <tc>           TYPE cxtab_control,
      <it_rowobject> TYPE STANDARD TABLE.

    ASSIGN :
      tc_displayed_lines    TO <lines>,
      ptr_tb_control->*     TO <tc>,
      ptr_it_rowobject->*   TO <it_rowobject>.

* delete marked lines

    DATA :
      c_area(40) TYPE c,
      l_selline  LIKE sy-stepl,
      c_field    TYPE fieldname.

*  GET CURSOR FIELD c_field
*             AREA  c_area.
*
** get current line
*  GET CURSOR LINE l_selline.
*  IF sy-subrc = 0 OR c_area NE tc_name.
*    c_field = ro_tb_default_cursor_field.
*  ENDIF.

    CALL METHOD it_delete.

    DESCRIBE TABLE <it_rowobject> LINES <tc>-lines.

    tc_top_line = <tc>-lines - <lines> + 1.

    IF <tc>-top_line > tc_top_line.
*    <tc>-top_line = <tc>-lines.
      <tc>-top_line = <tc>-lines - <lines> + 1.
    ENDIF.

*  ro_tb_search_parms-zcursor-field = c_field.
*  ro_tb_search_parms-zcursor-fieldoffset = 0.
*  ro_tb_search_parms-zcursor-line = 1.
*  ro_tb_search_parms-setcursor = 'X'.

  ENDMETHOD.


  METHOD tc_find .
* ...

    FIELD-SYMBOLS <tc> TYPE cxtab_control.

    ASSIGN :
      ptr_tb_control->* TO <tc>.

    CALL METHOD it_find
      EXPORTING
        im_method          = im_method
      CHANGING
        ch_start_from_line = <tc>-top_line.

  ENDMETHOD.


  METHOD tc_first_input_field.
* ...

    FIELD-SYMBOLS :
      <tc>   TYPE cxtab_control,
      <cols> TYPE cxtab_control-cols,
      <wa>   TYPE LINE OF cxtab_control-cols.

    ASSIGN :
      ptr_tb_control->* TO <tc>,
      <tc>-cols         TO <cols>.

    READ TABLE <cols>  ASSIGNING <wa> INDEX 1.
    IF sy-subrc = 0.
      ro_tb_default_cursor_field = <wa>-screen-name.
    ENDIF.

    IF lg_readonly = zcl_do_it=>co_false.
      LOOP AT  <cols> ASSIGNING <wa>.
        IF <wa>-screen-input = 1.
          ro_tb_default_cursor_field = <wa>-screen-name.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD tc_free.
* ...

    FIELD-SYMBOLS :
      <it_rowobject> TYPE STANDARD TABLE,
      <it_rowobjupd> TYPE STANDARD TABLE,
      <it_rowdb>     TYPE STANDARD TABLE.

    ASSIGN :
      ptr_it_rowobject->* TO <it_rowobject>,
      ptr_it_rowobjupd->* TO <it_rowobjupd>,
      ptr_it_rowdb->*     TO <it_rowdb>.

    FREE:
      <it_rowdb>,
      <it_rowobject>,
      <it_rowobjupd>.
  ENDMETHOD.


  METHOD tc_initialize.
* ...

    FIELD-SYMBOLS :
      <it_rowobject> TYPE STANDARD TABLE,
      <tc>           TYPE cxtab_control.

    ASSIGN :
      ptr_it_rowobject->* TO <it_rowobject>,
      ptr_tb_control->*   TO <tc>.

    IF <tc> IS ASSIGNED.
      DESCRIBE TABLE <it_rowobject> LINES <tc>-lines.
      IF tc_lg_keep_position NE 'X'.
        <tc>-top_line = 1.
      ELSE.
        IF tc_current_rowdbcnt = 0.
          <tc>-top_line = 1.
        ELSE.

          READ TABLE <it_rowobject>
            WITH
            KEY (zcl_do_it=>co_row_db_cnt) = tc_current_rowdbcnt
            TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            IF sy-tabix = 1.
              <tc>-top_line = 1.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD tc_insert .
* ...
    FIELD-SYMBOLS :
      <lines> TYPE i,
      <tc>    TYPE cxtab_control.

    DATA :
      c_area(40) TYPE c,
      l_selline  LIKE sy-stepl,
      l_lastline TYPE i,
      l_line     TYPE i,
      c_field    TYPE fieldname.

    ASSIGN :
      tc_displayed_lines      TO <lines>,
      ptr_tb_control->*       TO <tc>.

    GET CURSOR FIELD c_field
               AREA  c_area.

    CALL METHOD me->tc_first_input_field.

* get current line
    GET CURSOR LINE l_selline.
    IF sy-subrc <> 0 OR
       c_area   <> tc_name.                   " append line to table
      l_selline = <tc>-lines + 1.
*   set top line and new cursor line                                   *
      IF l_selline > <lines>.
        <tc>-top_line = l_selline - <lines> + 1 .
        l_line = <lines>.                                     " Was 1
      ELSE.
        <tc>-top_line = 1.
        l_line = l_selline.
      ENDIF.
      c_field = ro_tb_default_cursor_field.
    ELSE. "insert line into table
      l_selline = <tc>-top_line + l_selline - 1.
*   set top line and new cursor line                                   *
      l_lastline = l_selline + <lines> - 1.
      IF l_lastline <= <tc>-lines.
        <tc>-top_line = l_selline.
        l_line = 1.
      ELSEIF <lines> > <tc>-lines.
        <tc>-top_line = 1.
        l_line = l_selline.
      ELSE.
        <tc>-top_line = <tc>-lines - <lines> + 2 .
        l_line = l_selline - <tc>-top_line + 1.
      ENDIF.
      c_field = ro_tb_default_cursor_field.
    ENDIF.

    CALL METHOD it_insert
      EXPORTING
        im_line = l_selline.

    <tc>-lines = <tc>-lines + 1.

    ro_tb_search_parms-zcursor-field = c_field.
    ro_tb_search_parms-zcursor-fieldoffset = 0.
    ro_tb_search_parms-zcursor-line = l_line.
    ro_tb_search_parms-setcursor = 'X'.
  ENDMETHOD.


  METHOD tc_refresh.
* ...

    CALL METHOD:
      me->db_select,
      me->it_filter
        EXPORTING
           im_publish_work_area_changed  = im_publish_work_area_changed,
      me->tc_initialize.

  ENDMETHOD.


  METHOD tc_refresh_if_needed.
* ...

    IF db_refresh_needed = 'X'.
      CALL METHOD me->tc_refresh.
      db_refresh_needed = space.
    ENDIF.

  ENDMETHOD.


  METHOD tc_scroll .
* ...

    DATA l_on_line             TYPE i.
    DATA l_tc_new_top_line     TYPE i.
    DATA l_tc_name             TYPE fieldname.
    DATA l_tc_lines_name       TYPE fieldname.
    DATA l_tc_field_name       TYPE fieldname.

    FIELD-SYMBOLS :
      <tc>    TYPE cxtab_control,
      <lines> TYPE i.

    ASSIGN :
      ptr_tb_control->*  TO <tc>,
      tc_displayed_lines TO <lines>.

    IF <tc>-lines = 0.
*   yes, ...                                                           *
      l_tc_new_top_line = 1.
    ELSE.
*   no, ...                                                            *
      CALL FUNCTION 'SCROLLING_IN_TABLE'
        EXPORTING
          entry_act      = <tc>-top_line
          entry_from     = 1
          entry_to       = <tc>-lines
          last_page_full = 'X'
          loops          = <lines>
          ok_code        = im_method
          overlapping    = 'X'
        IMPORTING
          entry_new      = l_tc_new_top_line
        EXCEPTIONS
*         NO_ENTRY_OR_PAGE_ACT  = 01
*         NO_ENTRY_TO    = 02
*         NO_OK_CODE_OR_PAGE_GO = 03
          OTHERS         = 0.
    ENDIF.

* get actual tc and column                                             *
    GET CURSOR FIELD l_tc_field_name
               AREA  l_tc_name.

    IF syst-subrc = 0.
      IF l_tc_name = tc_name.
*     set actual column                                                *
*      SET CURSOR FIELD l_tc_field_name LINE 1.
        GET CURSOR LINE l_on_line.
        ro_tb_search_parms-zcursor-field = l_tc_field_name.
        ro_tb_search_parms-zcursor-fieldoffset = 0.
        ro_tb_search_parms-zcursor-line = l_on_line.
        ro_tb_search_parms-setcursor = 'X'.

      ENDIF.
    ENDIF.

* set the new top line                                                 *
    <tc>-top_line = l_tc_new_top_line.

  ENDMETHOD.


  METHOD tc_set_col.
* ...

    FIELD-SYMBOLS :
      <group> TYPE any,
      <tc>    TYPE cxtab_control,
      <cols>  TYPE cxtab_control-cols,
      <wa>    TYPE LINE OF cxtab_control-cols.

    DATA:
      temp_field_name TYPE fieldname.

    ASSIGN :
      ptr_tb_control->* TO <tc>,
      <tc>-cols         TO <cols>.

    CASE im_invisible.
      WHEN zcl_do_it=>co_visible.
        im_invisible = zcl_do_it=>co_tc_visible.
      WHEN zcl_do_it=>co_invisible.
        im_invisible = zcl_do_it=>co_tc_invisible.
    ENDCASE.

    IF NOT im_name IS INITIAL.
      IF im_name CS '-'.
        LOOP AT <cols> ASSIGNING <wa>.
          IF <wa>-screen-name = im_name.
            IF im_invisible BETWEEN '0' AND '1'.
              <wa>-invisible = im_invisible.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ELSE.
        CONCATENATE '*-' im_name INTO temp_field_name.
        LOOP AT <cols> ASSIGNING <wa>.
          IF <wa>-screen-name CP temp_field_name.
            IF im_invisible BETWEEN '0' AND '1'.
              <wa>-invisible = im_invisible.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ELSEIF NOT im_group IS INITIAL.
      LOOP AT <cols> ASSIGNING <wa>.
        DO 4 TIMES.
          CASE sy-index.
            WHEN 1.
              ASSIGN <wa>-screen-group1 TO  <group>.
            WHEN 2.
              ASSIGN <wa>-screen-group2 TO  <group>.
            WHEN 3.
              ASSIGN <wa>-screen-group3 TO  <group>.
            WHEN 4.
              ASSIGN <wa>-screen-group4 TO  <group>.
          ENDCASE.

          CASE <group>.
            WHEN im_group.
              IF im_invisible BETWEEN '0' AND '1'.
                <wa>-invisible = im_invisible.
              ENDIF.
          ENDCASE.
        ENDDO.
      ENDLOOP.
    ELSE.
      LOOP AT <cols> ASSIGNING <wa>.
        IF im_invisible BETWEEN '0' AND '1'.
          <wa>-invisible = im_invisible.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD tc_set_col_attribute .
* ...

    FIELD-SYMBOLS :
      <group> TYPE any,
      <tc>    TYPE cxtab_control,
      <cols>  TYPE cxtab_control-cols,
      <wa>    TYPE LINE OF cxtab_control-cols.

    ASSIGN :
      ptr_tb_control->* TO <tc>,
      <tc>-cols         TO <cols>.

    IF NOT im_name IS INITIAL.
      LOOP AT <cols> ASSIGNING <wa>.
        IF <wa>-screen-name = im_name.
          IF im_intensified BETWEEN '0' AND '1'.
            <wa>-screen-intensified = im_intensified.
          ENDIF.
          IF im_required BETWEEN '0' AND '1'.
            <wa>-screen-required = im_required.
          ENDIF.
          IF im_output BETWEEN '0' AND '1'.
            <wa>-screen-output = im_output.
          ENDIF.
          IF im_invisible BETWEEN '0' AND '1'.
            <wa>-screen-invisible = im_invisible.
          ENDIF.
          IF im_active BETWEEN '0' AND '1'.
            <wa>-screen-active = im_active.
          ENDIF.
          IF im_length BETWEEN '0' AND '1'.
            <wa>-screen-length = im_length.
          ENDIF.
          IF im_display_3d BETWEEN '0' AND '1'.
            <wa>-screen-display_3d = im_display_3d.
          ENDIF.
          IF im_value_help BETWEEN '0' AND '1'.
            <wa>-screen-value_help = im_value_help.
          ENDIF.
          IF im_request BETWEEN '0' AND '1'.
            <wa>-screen-request = im_request.
          ENDIF.
          IF im_input BETWEEN '0' AND '1'.
            <wa>-screen-input = im_input.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ELSEIF NOT im_group IS INITIAL.
      LOOP AT <cols> ASSIGNING <wa>.
        DO 4 TIMES.
          CASE sy-index.
            WHEN 1.
              ASSIGN <wa>-screen-group1 TO  <group>.
            WHEN 2.
              ASSIGN <wa>-screen-group2 TO  <group>.
            WHEN 3.
              ASSIGN <wa>-screen-group3 TO  <group>.
            WHEN 4.
              ASSIGN <wa>-screen-group4 TO  <group>.
          ENDCASE.

          CASE <group>.
            WHEN im_group.
              IF im_intensified BETWEEN '0' AND '1'.
                <wa>-screen-intensified = im_intensified.
              ENDIF.
              IF im_required BETWEEN '0' AND '1'.
                <wa>-screen-required = im_required.
              ENDIF.
              IF im_output BETWEEN '0' AND '1'.
                <wa>-screen-output = im_output.
              ENDIF.
              IF im_invisible BETWEEN '0' AND '1'.
                <wa>-screen-invisible = im_invisible.
              ENDIF.
              IF im_active BETWEEN '0' AND '1'.
                <wa>-screen-active = im_active.
              ENDIF.
              IF im_length BETWEEN '0' AND '1'.
                <wa>-screen-length = im_length.
              ENDIF.
              IF im_display_3d BETWEEN '0' AND '1'.
                <wa>-screen-display_3d = im_display_3d.
              ENDIF.
              IF im_value_help BETWEEN '0' AND '1'.
                <wa>-screen-value_help = im_value_help.
              ENDIF.
              IF im_request BETWEEN '0' AND '1'.
                <wa>-screen-request = im_request.
              ENDIF.
              IF im_input BETWEEN '0' AND '1'.
                <wa>-screen-input = im_input.
              ENDIF.
          ENDCASE.
        ENDDO.
      ENDLOOP.
    ELSE.
      LOOP AT <cols> ASSIGNING <wa>.

        IF im_intensified BETWEEN '0' AND '1'.
          <wa>-screen-intensified = im_intensified.
        ENDIF.
        IF im_required BETWEEN '0' AND '1'.
          <wa>-screen-required = im_required.
        ENDIF.
        IF im_output BETWEEN '0' AND '1'.
          <wa>-screen-output = im_output.
        ENDIF.
        IF im_invisible BETWEEN '0' AND '1'.
          <wa>-screen-invisible = im_invisible.
        ENDIF.
        IF im_active BETWEEN '0' AND '1'.
          <wa>-screen-active = im_active.
        ENDIF.
        IF im_length BETWEEN '0' AND '1'.
          <wa>-screen-length = im_length.
        ENDIF.
        IF im_display_3d BETWEEN '0' AND '1'.
          <wa>-screen-display_3d = im_display_3d.
        ENDIF.
        IF im_value_help BETWEEN '0' AND '1'.
          <wa>-screen-value_help = im_value_help.
        ENDIF.
        IF im_request BETWEEN '0' AND '1'.
          <wa>-screen-request = im_request.
        ENDIF.
        IF im_input BETWEEN '0' AND '1'.
          <wa>-screen-input = im_input.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD tc_set_disable_current_row .
* ...

    tc_disable_current_row = im_disable.

  ENDMETHOD.


  METHOD tc_set_displayed_lines.
* ...

    tc_displayed_lines = im_lines.

    IF ( tc_intensify_current_row = 'X' OR
        tc_disable_current_row   = 'X' ) AND
       im_db_cnt = tc_current_rowdbcnt.
      IF lg_readonly = zcl_do_it=>co_false.
        IF tc_disable_current_row = 'X'.
          CALL METHOD me->tc_set_row_attributes
            EXPORTING
              im_input       = '0'
              im_intensified = '1'.
        ELSE.
          CALL METHOD me->tc_set_row_attributes
            EXPORTING
              im_intensified = '1'.
        ENDIF.
      ELSE.
        CALL METHOD me->tc_set_row_attributes
          EXPORTING
            im_intensified = '1'.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD tc_set_intensify_current_row .
* ...

    tc_intensify_current_row = im_intensify.

  ENDMETHOD.


  METHOD tc_set_keep_position.
* ...

    tc_lg_keep_position = im_keep_position.

  ENDMETHOD.


  METHOD tc_set_row_attributes .
* ...
    TYPES:
      ty_it_cols        TYPE STANDARD TABLE OF cxtab_column.

    FIELD-SYMBOLS :
      <group>   TYPE any,
      <tc_cols> TYPE ty_it_cols,
      <tc>      TYPE cxtab_control.

    ASSIGN :
      ptr_tb_control->*     TO <tc>,
      <tc>-cols             TO <tc_cols>.

    IF NOT im_name IS INITIAL.
      LOOP AT SCREEN.
        IF screen-name = im_name.
          IF im_input BETWEEN '0' AND '1'.
            screen-input = im_input.
          ENDIF.
          IF im_intensified BETWEEN '0' AND '1'.
            screen-intensified = im_intensified.
          ENDIF.
          IF im_required BETWEEN '0' AND '1'.
            screen-required = im_required.
          ENDIF.
          IF im_output BETWEEN '0' AND '1'.
            screen-output = im_output.
          ENDIF.
          IF im_invisible BETWEEN '0' AND '1'.
            screen-invisible = im_invisible.
          ENDIF.
          IF im_active BETWEEN '0' AND '1'.
            screen-active = im_active.
          ENDIF.
          IF im_length BETWEEN '0' AND '1'.
            screen-length = im_length.
          ENDIF.
          IF im_display_3d BETWEEN '0' AND '1'.
            screen-display_3d = im_display_3d.
          ENDIF.
          IF im_value_help BETWEEN '0' AND '1'.
            screen-value_help = im_value_help.
          ENDIF.
          IF im_request BETWEEN '0' AND '1'.
            screen-request = im_request.
          ENDIF.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ELSEIF NOT im_group IS INITIAL.
      LOOP AT SCREEN.
        DO 4 TIMES.
          CASE sy-index.
            WHEN 1.
              ASSIGN screen-group1 TO  <group>.
            WHEN 2.
              ASSIGN screen-group2 TO  <group>.
            WHEN 3.
              ASSIGN screen-group3 TO  <group>.
            WHEN 4.
              ASSIGN screen-group4 TO  <group>.
          ENDCASE.

          CASE <group>.
            WHEN im_group.
              IF im_input BETWEEN '0' AND '1'.
                screen-input = im_input.
              ENDIF.
              IF im_intensified BETWEEN '0' AND '1'.
                screen-intensified = im_intensified.
              ENDIF.
              IF im_required BETWEEN '0' AND '1'.
                screen-required = im_required.
              ENDIF.
              IF im_output BETWEEN '0' AND '1'.
                screen-output = im_output.
              ENDIF.
              IF im_invisible BETWEEN '0' AND '1'.
                screen-invisible = im_invisible.
              ENDIF.
              IF im_active BETWEEN '0' AND '1'.
                screen-active = im_active.
              ENDIF.
              IF im_length BETWEEN '0' AND '1'.
                screen-length = im_length.
              ENDIF.
              IF im_display_3d BETWEEN '0' AND '1'.
                screen-display_3d = im_display_3d.
              ENDIF.
              IF im_value_help BETWEEN '0' AND '1'.
                screen-value_help = im_value_help.
              ENDIF.
              IF im_request BETWEEN '0' AND '1'.
                screen-request = im_request.
              ENDIF.
              MODIFY SCREEN.

          ENDCASE.
        ENDDO.
      ENDLOOP.
    ELSE.
      LOOP AT SCREEN.
        READ TABLE <tc_cols> WITH KEY screen-name = screen-name
          TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          IF im_input BETWEEN '0' AND '1'.
            screen-input = im_input.
          ENDIF.
          IF im_intensified BETWEEN '0' AND '1'.
            screen-intensified = im_intensified.
          ENDIF.
          IF im_required BETWEEN '0' AND '1'.
            screen-required = im_required.
          ENDIF.
          IF im_output BETWEEN '0' AND '1'.
            screen-output = im_output.
          ENDIF.
          IF im_invisible BETWEEN '0' AND '1'.
            screen-invisible = im_invisible.
          ENDIF.
          IF im_active BETWEEN '0' AND '1'.
            screen-active = im_active.
          ENDIF.
          IF im_length BETWEEN '0' AND '1'.
            screen-length = im_length.
          ENDIF.
          IF im_display_3d BETWEEN '0' AND '1'.
            screen-display_3d = im_display_3d.
          ENDIF.
          IF im_value_help BETWEEN '0' AND '1'.
            screen-value_help = im_value_help.
          ENDIF.
          IF im_request BETWEEN '0' AND '1'.
            screen-request = im_request.
          ENDIF.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD tc_use_labels_from_screen.
* ...

    DATA :
      cfound      TYPE c,
      ctable(132) TYPE c,
      cfield(132) TYPE c,
      it_d021t    TYPE STANDARD TABLE OF d021t.

    FIELD-SYMBOLS:
      <wa_ro_tb_alv_fieldcat> LIKE LINE OF it_ro_tb_alv_fieldcat,
      <wa_ro_tb_structure>    LIKE LINE OF it_ro_tb_structure,
      <wa>                    TYPE d021t.

    CALL METHOD it_alv_set_program
      EXPORTING
        im_program = im_program
        im_screen  = im_screen.

    SELECT *
      INTO TABLE it_d021t
      FROM d021t
      WHERE prog = im_program
        AND dynr = im_screen
        AND lang = sy-langu.

* add the ro_tb_name because a screen could have the same field
* on it more than once.
* for exampel it could have structure with vbeln and internal table
* with vbeln.  We want to get the labels from the table
* this is not perfect but hopefuly solve most situations
    LOOP AT it_d021t ASSIGNING <wa> WHERE fldn CS ro_tb_name.
      SPLIT <wa>-fldn AT '-' INTO ctable cfield.
      READ TABLE it_ro_tb_structure WITH KEY fieldname = cfield
        ASSIGNING <wa_ro_tb_structure>.
      IF sy-subrc = 0.
        cfound = 'X'.
        <wa_ro_tb_structure>-scrtext_s = <wa>-dtxt.
        <wa_ro_tb_structure>-scrtext_m = <wa>-dtxt.
        <wa_ro_tb_structure>-scrtext_l = <wa>-dtxt.
      ENDIF.
      READ TABLE it_ro_tb_alv_fieldcat WITH KEY fieldname = cfield
        ASSIGNING <wa_ro_tb_alv_fieldcat>.
      IF sy-subrc = 0.
        <wa_ro_tb_alv_fieldcat>-seltext_s =  <wa>-dtxt.
        <wa_ro_tb_alv_fieldcat>-seltext_m =  <wa>-dtxt.
        <wa_ro_tb_alv_fieldcat>-seltext_l =  <wa>-dtxt.
        <wa_ro_tb_alv_fieldcat>-ddictxt   = 'M'.
      ENDIF.
    ENDLOOP.

    TYPES:
      BEGIN OF ts_d020s,
        dylang          TYPE  sylangu,
        dyname          TYPE  progname,
        dynumb          TYPE  sychar04,
        request         TYPE  sychar01,
        suppress_checks TYPE  sychar01,
      END OF ts_d020s.

    DATA:
      ls_param_d020s TYPE ts_d020s,
      ls_d020s       TYPE d020s,
      lt_d021s       TYPE STANDARD TABLE OF d021s,
      lt_d022s       TYPE STANDARD TABLE OF d022s.

    ls_param_d020s-dylang = sy-langu.
    ls_param_d020s-dyname = im_program.
    ls_param_d020s-dynumb = im_screen.


    CALL FUNCTION 'RS_IMPORT_DYNPRO'
      EXPORTING
        dylang               = ls_param_d020s-dylang
        dyname               = ls_param_d020s-dyname
        dynumb               = ls_param_d020s-dynumb
      IMPORTING
        header               = ls_d020s
      TABLES
        ftab                 = lt_d021s
        pltab                = lt_d022s
      EXCEPTIONS
        button_error         = 1
        dylanguage_invalid   = 2
        dylanguage_not_inst  = 3
        dyname_invalid       = 4
        dynproload_not_found = 5
        dynpro_old           = 6
        dynumb_invalid       = 7
        ftab_invalid         = 8
        gen_error            = 9
        gen_ok               = 10
        header_invalid       = 11
        internal_error       = 12
        no_dynpro            = 13
        no_ftab_row          = 14
        no_memory            = 15
        no_processlogic      = 16
        pltab_invalid        = 17
        request_invalid      = 18
        OTHERS               = 19.
    IF sy-subrc = 0.
      LOOP AT lt_d021s ASSIGNING FIELD-SYMBOL(<ls_d021s>)
          WHERE fnam CS ro_tb_name AND stxt CS 'V'.
        SPLIT <ls_d021s>-fnam AT '-' INTO ctable cfield.
        READ TABLE it_ro_tb_structure WITH KEY fieldname = cfield
          ASSIGNING <wa_ro_tb_structure>.
        IF sy-subrc = 0.
          cfound = 'X'.
          <wa_ro_tb_structure>-sign = abap_true.
        ENDIF.
      ENDLOOP.
    ENDIF.


    IF cfound = 'X'.
      CALL METHOD:
      ptr_ro_tb_mark->set_valid_fields_descriptions
        EXPORTING im_it_valid_fields = it_ro_tb_structure,
      ptr_ro_tb_where->set_valid_fields_descriptions
        EXPORTING im_it_valid_fields = it_ro_tb_structure,
      ptr_db_tb_where->set_valid_fields_descriptions
        EXPORTING im_it_valid_fields = it_db_tb_structure.
    ENDIF.
  ENDMETHOD.


  METHOD wa_from_it .
* ...

    FIELD-SYMBOLS :
      <dbcnt>        TYPE any,
      <it_rowobject> TYPE STANDARD TABLE,
      <wa_rowobject> TYPE any,
      <wa>           TYPE any.

    ASSIGN :
      ptr_it_rowobject->*   TO <it_rowobject>,
      ptr_wa_rowobject->*   TO <wa>.

    IF <wa> IS ASSIGNED.
      IF im_db_cnt = 0.
        ASSIGN
          COMPONENT zcl_do_it=>co_row_db_cnt
            OF STRUCTURE  <wa> TO <dbcnt>.
      ELSE.
        ASSIGN
          im_db_cnt TO <dbcnt>.
      ENDIF.

      IF <dbcnt> > 0.
        READ TABLE <it_rowobject> ASSIGNING <wa_rowobject>
                WITH KEY (zcl_do_it=>co_row_db_cnt) = <dbcnt>.
        IF sy-subrc = 0.
          <wa> = <wa_rowobject>.
        ENDIF.
      ELSE.
        READ TABLE <it_rowobject> ASSIGNING <wa_rowobject> INDEX 1.
        IF sy-subrc = 0.
          <wa> = <wa_rowobject>.
        ENDIF.
      ENDIF.
    ENDIF.

    RAISE EVENT work_area_changed.

  ENDMETHOD.


  METHOD wa_get_field_value.
* ...

    FIELD-SYMBOLS :
      <field_wa>       TYPE any,
      <field>          TYPE any,
      <wa>             TYPE any,
      <wa_dfies>       TYPE LINE OF spar_dfies,
      <wa_field_value> TYPE LINE OF zif_do_seo=>tt_field_values.

    ASSIGN :
      ptr_wa_rowobject->*   TO   <wa>.

    LOOP AT ch_it_field_values ASSIGNING <wa_field_value>.

      READ TABLE it_ro_tb_structure ASSIGNING <wa_dfies>
            WITH KEY fieldname = <wa_field_value>-fieldname.
      IF sy-subrc = 0.
        IF <wa_dfies>-rollname IS INITIAL.
          CASE <wa_dfies>-inttype.  "See Domain INTTYPE.
              "C  Character String
              "N  Character String with Digits Only
              "D  Date (Date: YYYYMMDD)
              "T  Time (Time: HHMMSS)
              "X  Byte Seq. (heXadecimal), in DDIC metadata also for INT1/2/4
            WHEN 'I'.             "I  Integer number (4-byte integer with sign)
              CREATE DATA:
                  <wa_field_value>-fieldvalue  TYPE i.
              "b  1-byte integer, integer number <= 254
              "s  2-byte integer, only for length field before LCHR or LRAW
              "P  Packed number
              "F  Floating point number to accuracy of 8 bytes
            WHEN 'g'.             "g  Character string with variable length (ABAP type STRING)
              CREATE DATA:
                  <wa_field_value>-fieldvalue  TYPE string.
              "y  Byte sequence with variable length (ABAP type XSTRING)
              "u  Structured type, flat
              "v  Structured type, deep
              "h  Table type
              "V  Character string (old Dictionary type VARC)
              "r  Reference to class/interface
              "l  Reference to data object
              "a  Decimal Floating Point Number, 16 Digits
              "e  Decimal Floating Point Number, 34 Digits
              "j  Static Boxed Components
              "k  Generic Boxed Components
          ENDCASE.
        ELSE.
          CREATE DATA:
            <wa_field_value>-fieldvalue  TYPE (<wa_dfies>-rollname).
        ENDIF.
        ASSIGN COMPONENT <wa_field_value>-fieldname
          OF STRUCTURE <wa> TO <field>.
        IF sy-subrc = 0.

          ASSIGN
           <wa_field_value>-fieldvalue->* TO <field_wa>.
          <field_wa> = <field>.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD wa_raise_work_area_changed.
* ...
    RAISE EVENT work_area_changed.
  ENDMETHOD.


  METHOD wa_to_it .
* ...

    DATA:
      l_ptr_row    TYPE REF TO data.

    FIELD-SYMBOLS :
      <field_it>        TYPE any,
      <field_wa>        TYPE any,
      <wa>              TYPE any,
      <wa_rowobject>    TYPE any,
      <dbcnt>           TYPE any,
      <wa_rowobjfields> TYPE LINE OF spar_dfies,
      <it_rowobject>    TYPE STANDARD TABLE.

    ASSIGN :
      ptr_wa_rowobject->*  TO <wa>,
      ptr_it_rowobject->*  TO <it_rowobject>.

    IF NOT <wa> IS ASSIGNED.
      EXIT.
    ENDIF.

    IF <wa> IS INITIAL.
      EXIT.
    ENDIF.

    ASSIGN
      COMPONENT zcl_do_it=>co_row_db_cnt
        OF STRUCTURE <wa> TO <dbcnt>.

    IF im_row_mod = zcl_do_it=>co_mod_create.
      CLEAR <dbcnt>.
    ENDIF.

    IF <dbcnt> < 1.
      CALL METHOD me->it_row_empty
        EXPORTING
          im_ptr_to_row = ptr_wa_rowobject.
      IF im_row_mod = zcl_do_it=>co_mod_create.
        CALL METHOD me->it_row_created
          EXPORTING
            im_ptr_to_row = ptr_wa_rowobject.
      ENDIF.
      APPEND <wa> TO <it_rowobject>.
      ASSIGN
        COMPONENT zcl_do_it=>co_row_db_cnt
          OF STRUCTURE <wa> TO <dbcnt>.
    ENDIF.


    READ TABLE <it_rowobject> ASSIGNING  <wa_rowobject>
       WITH KEY (zcl_do_it=>co_row_db_cnt) = <dbcnt>.

    IF sy-subrc = 0.

*Assign Values of the fields of zrowobject structure to wa
      LOOP AT it_zrowobject_structure ASSIGNING <wa_rowobjfields>.
        ASSIGN :
          COMPONENT  <wa_rowobjfields>-fieldname
            OF STRUCTURE  <wa> TO <field_wa>,
          COMPONENT  <wa_rowobjfields>-fieldname
            OF STRUCTURE  <wa_rowobject> TO <field_it>.
        <field_wa> = <field_it>.
      ENDLOOP.

* does wa match rowobjupd

* does it match rowobjupd

* does wa match it

      <wa_rowobject> = <wa>.

      GET REFERENCE OF <wa_rowobject> INTO l_ptr_row.
      CALL METHOD me->ru_update
        EXPORTING
          im_ptr_to_row = l_ptr_row.

      RAISE EVENT work_area_changed.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
