*&---------------------------------------------------------------------*
*&  Include           ZDO_EXAMPLE_TOP
*&---------------------------------------------------------------------*

PROGRAM  zdo_example.

INCLUDE zdo_example_d01.
INCLUDE zdo_m001.


CONTROLS: gtc_t001w TYPE TABLEVIEW USING SCREEN 1000.
*&SPWIZARD: DECLARATION OF TABLECONTROL 'GTC_T320' ITSELF
CONTROLS: gtc_t320 TYPE TABLEVIEW USING SCREEN 1010.

CONSTANTS:
  BEGIN OF c_gts_0100,
    start_column TYPE i VALUE 5,
    start_line   TYPE i VALUE 3,
    end_column   TYPE i VALUE 97,
    end_line     TYPE i VALUE 10,
  END OF c_gts_0100.

* I wish ABAP would update they way to create a type with include structure.
* this syntax to really confusing.
TYPES:
BEGIN OF ty_st_do_t320.
        INCLUDE STRUCTURE t320.
TYPES:   mstroh_test(3) TYPE c.
        INCLUDE STRUCTURE zst_do_rowobject.
TYPES:
END OF ty_st_do_t320,
ty_tt_do_t320 TYPE STANDARD TABLE OF ty_st_do_t320.

DATA:
  gok_code       LIKE sy-ucomm,
  ok_code        LIKE sy-ucomm,
  GWA_SET_POS    TYPE ZST_DO_SET_CURSOR,
*  gilines        TYPE i,
*  gifieldoffset  TYPE num4,
*  gcfield        TYPE fieldname,
*  gcset_position TYPE char1,
  gc_1000_tc(4)  TYPE c VALUE '1000',
  gobj_execute   TYPE REF TO gcl_selection,
  gobj_t001w     TYPE REF TO gcl_t001w,
  git_t001w      TYPE ztt_do_t001w WITH HEADER LINE,
  gobj_t320      TYPE REF TO gcl_t320,
  git_t320       TYPE ty_tt_do_t320 WITH HEADER LINE. "ztt_do_t320 WITH HEADER LINE.

SELECTION-SCREEN BEGIN OF SCREEN 7000 AS SUBSCREEN.
SELECT-OPTIONS so_werks FOR git_t001w-werks.
SELECTION-SCREEN END OF SCREEN 7000.

LOAD-OF-PROGRAM.
  PERFORM create_data_objects.
