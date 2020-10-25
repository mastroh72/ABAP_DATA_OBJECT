DEFINE change_checkbox.
  read table it_ro_tb_alv_fieldcat
     assigning <wa_ro_tb_alv_fieldcat>
     with key fieldname = &1.
  if sy-subrc = 0.
    <wa_ro_tb_alv_fieldcat>-checkbox = &2.
  endif.
END-OF-DEFINITION.


DEFINE add_sort_columns.
  wa_ro_tb_sort_columns-sortorder = &1.
  wa_ro_tb_sort_columns-fieldname = &2.
  wa_ro_tb_sort_columns-direction = &3.
  append wa_ro_tb_sort_columns to it_ro_tb_sort_columns.
END-OF-DEFINITION.

DEFINE change_label.
  read table it_ro_tb_alv_fieldcat
     assigning <wa_ro_tb_alv_fieldcat>
     with key fieldname = &1.
  if sy-subrc = 0.
    <wa_ro_tb_alv_fieldcat>-seltext_l = &2.
    <wa_ro_tb_alv_fieldcat>-seltext_m = &2.
    <wa_ro_tb_alv_fieldcat>-seltext_s = &2.
    <wa_ro_tb_alv_fieldcat>-reptext_ddic = &2.
    <wa_ro_tb_alv_fieldcat>-ddictxt   = 'M'.
  endif.
END-OF-DEFINITION.


DEFINE change_tech.
  read table it_ro_tb_alv_fieldcat
     assigning <wa_ro_tb_alv_fieldcat>
     with key fieldname = &1.
  if sy-subrc = 0.
    <wa_ro_tb_alv_fieldcat>-tech = &2.
    <wa_ro_tb_alv_fieldcat>-no_out = &2.
  endif.
END-OF-DEFINITION.


DEFINE change_hotspot.
  read table it_ro_tb_alv_fieldcat
     assigning <wa_ro_tb_alv_fieldcat>
     with key fieldname = &1.
  if sy-subrc = 0.
    <wa_ro_tb_alv_fieldcat>-hotspot = &2.
    <wa_ro_tb_alv_fieldcat>-emphasize = &2.
  endif.
END-OF-DEFINITION.

DEFINE field_add.
  wa_db_tb_fields-fieldname =  &1.
  append wa_db_tb_fields to it_db_tb_fields.
END-OF-DEFINITION.

DEFINE group_add.
  wa_db_tb_group_by-fieldname =  &1.
  append wa_db_tb_group_by to it_db_tb_group_by.
END-OF-DEFINITION.

DEFINE change_sum.
  read table it_ro_tb_alv_fieldcat
     assigning <wa_ro_tb_alv_fieldcat>
     with key fieldname = &1.
  if sy-subrc = 0.
    <wa_ro_tb_alv_fieldcat>-do_sum = &2.
  endif.
END-OF-DEFINITION.

DEFINE change_ref.
  read table it_ro_tb_alv_fieldcat
     assigning <wa_ro_tb_alv_fieldcat>
     with key fieldname = &1.
  if sy-subrc = 0.
    <wa_ro_tb_alv_fieldcat>-ref_tabname = &2.
    <wa_ro_tb_alv_fieldcat>-ref_fieldname = &3.
  endif.
END-OF-DEFINITION.


DEFINE create_do.
  get reference of &2[]    into ptr_to_it.
  get reference of &3      into ptr_to_wa.
  get reference of &4      into ptr_to_tc .
  create object:
    &1
      exporting
          im_ptr_to_table = ptr_to_it
          im_ptr_to_wa  = ptr_to_wa
          im_ptr_to_tc  = ptr_to_tc
          im_tc_name    = &5.

END-OF-DEFINITION.



DEFINE create_relationship_row.
  wa_relationship-relationship_seq   = &1.
  wa_relationship-published_field    = &2.
  wa_relationship-subscribed_field   = &3.
  wa_relationship-foreignkey         = &4.
  append wa_relationship to it_relationship.
END-OF-DEFINITION.




DEFINE add_link.
  CALL METHOD :
    &1->lk_add_link
      EXPORTING
        im_ptr_publisher =  &2
        im_relationship    = &3
        im_it_relationship = it_relationship.
END-OF-DEFINITION.

DEFINE add_link_reposition.
  CALL METHOD :
    &1->lk_add_link
      EXPORTING
        im_ptr_publisher =  &2
        im_relationship    = ZCL_DO_IT=>co_link_reposition
        im_it_relationship = it_relationship.
END-OF-DEFINITION.

DEFINE add_link_data.
  CALL METHOD :
    &1->lk_add_link
      EXPORTING
        im_ptr_publisher =  &2
        im_relationship    = ZCL_DO_IT=>co_link_data
        im_it_relationship = it_relationship.
END-OF-DEFINITION.


DEFINE add_link_data_filter.
  CALL METHOD :
    &1->lk_add_link
      EXPORTING
        im_ptr_publisher =  &2
        im_relationship    = ZCL_DO_IT=>co_link_data_filter
        im_it_relationship = it_relationship.
END-OF-DEFINITION.

DEFINE is_condition.
  call method me->&1->is_condition
    exporting
      im_field_name = &2
    importing
      ex_exist = &3.
END-OF-DEFINITION.



DEFINE create_lookup.
  wa_lookup-ucomm         = &1.
  wa_lookup-obj_data      = &2.
  wa_lookup-conv_method   = &3.
  wa_lookup-title         = &4.
  wa_lookup-program       = &5.
  wa_lookup-dynnr         = &6.
  append wa_lookup to it_lookup.
END-OF-DEFINITION.


DEFINE create_lookup_with_filter.
  wa_lookup-ucomm         = &1.
  wa_lookup-obj_data      = &2.
  wa_lookup-conv_method   = &3.
  wa_lookup-title         = &4.
  wa_lookup-program       = &5.
  wa_lookup-dynnr         = &6.
  wa_lookup-conv_filter_method   = &7.
  append wa_lookup to it_lookup.
END-OF-DEFINITION.


DEFINE convert_so_method_generic.
  call method:
       obj_db_where->get_data_type
         exporting
           im_field_name = im_field
         importing
           ex_data_type = c_data_type.
  call function 'ZTB_TOOLS_01_RANGE_MAP_SO'
       exporting
            fieldname            = im_field
            data_type            = c_data_type
            reset_selection      = c_reset_selection
       tables
            it_range             = &1
            it_selection_options = &2
       exceptions
            no_range_tab         = 1
            others               = 2.
END-OF-DEFINITION.


DEFINE convert_so_method.
  call method:
       obj_db_where->get_data_type
         exporting
           im_field_name = im_field
         importing
           ex_data_type = c_data_type.
  call function 'ZTB_TOOLS_01_RANGE_MAP_SO'
       exporting
            fieldname            = im_field
            data_type            = c_data_type
            reset_selection      = c_reset_selection
       tables
            it_range             = &1
            it_selection_options = it_selection
       exceptions
            no_range_tab         = 1
            others               = 2.
END-OF-DEFINITION.


DEFINE convert_so_method_filter.
  call method:
       obj_ro_where->get_data_type
         exporting
           im_field_name = im_field
         importing
           ex_data_type = c_data_type.
  call function 'ZTB_TOOLS_01_RANGE_MAP_SO'
       exporting
            fieldname            = im_field
            data_type            = c_data_type
            reset_selection      = c_reset_selection
       tables
            it_range             = &1
            it_selection_options = IT_FILTER
       exceptions
            no_range_tab         = 1
            others               = 2.
END-OF-DEFINITION.
