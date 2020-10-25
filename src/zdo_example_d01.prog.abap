*----------------------------------------------------------------------*
***INCLUDE ZDO_EXAMPLE_D01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&       Class g_cl_t320
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS gcl_t320 DEFINITION INHERITING FROM zcl_do_it.
  PUBLIC SECTION.



    METHODS:
      constructor
        IMPORTING
          im_ptr_to_table TYPE REF TO data
          im_ptr_to_wa    TYPE REF TO data
          im_ptr_to_tc    TYPE REF TO data OPTIONAL
          im_tc_name      TYPE typename OPTIONAL.

ENDCLASS.


CLASS gcl_t001w DEFINITION INHERITING FROM zcl_do_it.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          im_ptr_to_table TYPE REF TO data
          im_ptr_to_wa    TYPE REF TO data
          im_ptr_to_tc    TYPE REF TO data OPTIONAL
          im_tc_name      TYPE typename OPTIONAL.
ENDCLASS.                    "gcl_matnr_det DEFINITION



CLASS gcl_selection DEFINITION INHERITING FROM zcl_do_sel_scr_exe.

  PUBLIC SECTION.
    METHODS:
      convert_ranges_01.

  PROTECTED SECTION.
    METHODS:
      convert_werks          IMPORTING im_field TYPE fieldname.
ENDCLASS.
