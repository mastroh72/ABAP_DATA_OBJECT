CLASS zcl_do_sel_scr_exe DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .
    TYPE-POOLS zse .
    METHODS add_look_up
      IMPORTING
        !im_it_lu TYPE zse_tt_do_look_up .
    METHODS apply_selection
      IMPORTING
        !im_ucomm     TYPE syucomm
        !im_layout    TYPE slis_vari OPTIONAL
        !im_go_to_alv TYPE char1 DEFAULT space
        !im_maxhits   TYPE int4 DEFAULT 0
      EXPORTING
        !ex_title     TYPE char30
        !ex_dynnr     TYPE sydynnr .
    METHODS free_data .
    METHODS main_object_fcode
      CHANGING
        VALUE(im_fcode) TYPE syucomm .
    TYPE-POOLS zslot .

  PROTECTED SECTION.

    DATA wa_ucomm TYPE zse_st_do_look_up .
    DATA it_ucomm_once TYPE zse_tt_do_look_up .
    DATA obj_data TYPE REF TO zcl_do_it .
    DATA obj_db_where TYPE REF TO zcl_do_selection_options .
    DATA obj_ro_where TYPE REF TO zcl_do_selection_options .
    DATA it_filter TYPE zslot_tt_do_selection_options .
    DATA it_selection TYPE zslot_tt_do_selection_options .
    DATA it_ucomm TYPE zse_tt_do_sorted_look_up .
    DATA source_id TYPE zdo_object_id .
    DATA alv_layout TYPE slis_vari .
    DATA lg_go_to_alv TYPE char1 .
    DATA maxhits TYPE int4 .
    DATA c_data_type TYPE rollname .
    DATA c_reset_selection TYPE char1 VALUE space ##NO_TEXT.

    METHODS activate_all_publishers .
    METHODS deactivate_all_publishers .
    METHODS reset_listeners_wheres .
    METHODS reset_lu_wheres .
    METHODS set_pointers .
    METHODS set_query .
    METHODS apply_listeners_wheres .
    METHODS set_filter .

  PRIVATE SECTION.

ENDCLASS.

CLASS zcl_do_sel_scr_exe IMPLEMENTATION.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_SELECTION_EXE->ACTIVATE_ALL_PUBLISHERS
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD activate_all_publishers .
* ...
  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_SELECTION_EXE->ADD_LOOK_UP
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_IT_LU                       TYPE        zse_tt_do_look_up
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_look_up.
* ...

*  DATA:
*    wa LIKE LINE OF im_it_lu.

    DATA:
      dynnr TYPE dynpronr.

    FIELD-SYMBOLS:
      <wa> LIKE LINE OF im_it_lu.

    REFRESH:
      it_ucomm_once,
      it_ucomm.

    LOOP AT im_it_lu ASSIGNING <wa>.
      INSERT <wa> INTO TABLE it_ucomm.

      dynnr  = <wa>-dynnr.

      CALL METHOD <wa>-obj_data->it_alv_set_program
        EXPORTING
          im_program = <wa>-program
          im_screen  = dynnr.

    ENDLOOP.

*  LOOP AT im_it_lu INTO wa.
*    INSERT wa INTO table it_ucomm.
*  ENDLOOP.
    it_ucomm_once = im_it_lu.

    SORT it_ucomm_once BY obj_data.
    DELETE ADJACENT DUPLICATES FROM  it_ucomm_once COMPARING obj_data.

  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_SELECTION_EXE->APPLY_LISTENERS_WHERES
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD apply_listeners_wheres.
* ...
    REFRESH:
      it_selection,
      it_filter.
  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_SELECTION_EXE->APPLY_SELECTION
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_UCOMM                       TYPE        SYUCOMM
* | [--->] IM_LAYOUT                      TYPE        SLIS_VARI(optional)
* | [--->] IM_GO_TO_ALV                   TYPE        CHAR1 (default =SPACE)
* | [--->] IM_MAXHITS                     TYPE        INT4 (default =0)
* | [<---] EX_TITLE                       TYPE        CHAR30
* | [<---] EX_DYNNR                       TYPE        SYDYNNR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD apply_selection.
* ...

    DATA:
      dynnr TYPE dynpronr.

    maxhits = im_maxhits .
    alv_layout = im_layout.
    lg_go_to_alv = im_go_to_alv.
    wa_ucomm-ucomm = im_ucomm.

    REFRESH:
      it_selection,
      it_filter.

    CALL METHOD:
      me->free_data,
      me->set_pointers.

    IF NOT obj_data IS INITIAL.
      dynnr = wa_ucomm-dynnr.
      CALL METHOD obj_data->it_alv_set_program
        EXPORTING
          im_program = wa_ucomm-program
          im_screen  = dynnr
          im_variant = im_layout.

      ex_dynnr = wa_ucomm-dynnr.
      ex_title = wa_ucomm-title.
      source_id          = obj_data->id.
      obj_db_where       = obj_data->ptr_db_tb_where.
      obj_ro_where       = obj_data->ptr_ro_tb_where.

      CALL METHOD:
        me->reset_listeners_wheres,
        me->reset_lu_wheres,
        me->deactivate_all_publishers,
        obj_data->ptr_ro_tb_where->delete_condition,
        obj_db_where->delete_condition,
        obj_data->db_set_max_hits
          EXPORTING
            im_max_hits = maxhits,
        me->set_query,
        me->activate_all_publishers,
        obj_db_where->add_condition_by_selection
            EXPORTING
              im_it_selection = it_selection
              im_refresh_fieldnames = 'X',
        obj_data->db_where_from_ptr_db_where,
        me->apply_listeners_wheres,
        obj_data->db_select,
        me->set_filter,
        obj_ro_where->add_condition_by_sel_filter
            EXPORTING
              im_it_selection = it_filter
              im_refresh_fieldnames = 'X',
        obj_data->it_filter,
        obj_data->tc_initialize,
        obj_data->class_set_main_object,
        obj_data->db_message_hits.

      IF lg_go_to_alv = 'X'.
        CALL METHOD obj_data->it_alv.
      ENDIF.

    ENDIF.

  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_SELECTION_EXE->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
* ...
  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_SELECTION_EXE->DEACTIVATE_ALL_PUBLISHERS
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD deactivate_all_publishers.
* ...
  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_SELECTION_EXE->FREE_DATA
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD free_data.
* ...

    IF NOT obj_data IS INITIAL.
      CALL METHOD obj_data->tc_free.
    ENDIF.

  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_SELECTION_EXE->MAIN_OBJECT_FCODE
* +-------------------------------------------------------------------------------------------------+
* | [<-->] IM_FCODE                       TYPE        SYUCOMM
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD main_object_fcode.

    CALL METHOD obj_data->it_function_code
      EXPORTING
        im_main_object   = 'X'
      CHANGING
        ch_function_code = im_fcode.

  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_SELECTION_EXE->RESET_LISTENERS_WHERES
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD reset_listeners_wheres.
* ...
  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_SELECTION_EXE->RESET_LU_WHERES
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD reset_lu_wheres.
* ...
  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_SELECTION_EXE->SET_FILTER
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_filter.

    IF NOT wa_ucomm-conv_filter_method IS INITIAL.
      CALL METHOD me->(wa_ucomm-conv_filter_method).
    ENDIF.

  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_SELECTION_EXE->SET_POINTERS
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_pointers.
* ...

    READ TABLE it_ucomm INTO wa_ucomm
      WITH TABLE KEY ucomm = wa_ucomm-ucomm.
    IF sy-subrc = 0.
      obj_data = wa_ucomm-obj_data.
    ELSE.
      CLEAR:
        wa_ucomm,
        obj_data.
    ENDIF.

  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_SELECTION_EXE->SET_QUERY
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_query.
* ...

    IF NOT wa_ucomm-conv_method IS INITIAL.
      CALL METHOD me->(wa_ucomm-conv_method).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

