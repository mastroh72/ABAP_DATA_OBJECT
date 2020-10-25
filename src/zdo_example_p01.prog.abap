*----------------------------------------------------------------------*
***INCLUDE ZDO_EXAMPLE_P01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&       Class (Implementation)  g_cl_t320
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS gcl_t320 IMPLEMENTATION.
  METHOD constructor.

    CALL METHOD:
     super->constructor
      EXPORTING
*        im_it_table_type = 'ZTT_DO_T320'
        im_table_name    = 'T320'
*        im_db_table_type = 'ZTT_DO_T320'
        im_ptr_to_table  = im_ptr_to_table
        im_ptr_to_wa     = im_ptr_to_wa
        im_ptr_to_tc     = im_ptr_to_tc
        im_tc_name       = im_tc_name,
    me->db_set_reposition_on_wa
    EXPORTING
    im_active = 'X',
    me->tc_set_intensify_current_row
       EXPORTING
         im_intensify = zcl_do_it=>co_true.

  ENDMETHOD.
ENDCLASS.               "g_cl_t320



CLASS gcl_t001w  IMPLEMENTATION.

  METHOD constructor.

    CALL METHOD:
     super->constructor
      EXPORTING
        im_it_table_type = 'ZTT_DO_T001W'
        im_table_name    = 'T001W'
        im_db_table_type = 'ZTT_DO_T001W'
        im_ptr_to_table  = im_ptr_to_table
        im_ptr_to_wa     = im_ptr_to_wa
        im_ptr_to_tc     = im_ptr_to_tc
        im_tc_name       = im_tc_name,
    me->tc_set_intensify_current_row
   EXPORTING
     im_intensify = zcl_do_it=>co_true.

  ENDMETHOD.                    "constructor
ENDCLASS.



CLASS gcl_selection  IMPLEMENTATION.
  METHOD convert_ranges_01.
    CALL METHOD convert_werks
      EXPORTING
        im_field = 'WERKS'.
  ENDMETHOD.

  METHOD convert_werks.
    convert_so_method so_werks.
  ENDMETHOD.                    "convert_werks

ENDCLASS.
