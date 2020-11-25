*&---------------------------------------------------------------------*
*&  Include           ZDO_EXAMPLE_F01
*&---------------------------------------------------------------------*



FORM create_data_objects.
  DATA :
    wa_lookup       TYPE ZIF_DO_SE=>ts_look_up,
    it_lookup       TYPE ZIF_DO_SE=>tt_look_up,
    it_relationship TYPE ZTT_DO_RELATIONSHIP,
    wa_relationship TYPE LINE OF ZTT_DO_RELATIONSHIP,
    ptr_to_it       TYPE REF TO data,
    ptr_to_wa       TYPE REF TO data,
    ptr_to_tc       TYPE REF TO data.

  create_do gobj_t001w git_t001w
            git_t001w  gtc_t001w 'GTC_T001W'.

  create_do gobj_t320 git_t320
            git_t320  gtc_t320 'GTC_T320'.


  REFRESH it_relationship.
  create_relationship_row 1 'WERKS' 'WERKS' 'X'.
  add_link_data gobj_t320 gobj_t001w.


  CREATE OBJECT gobj_execute.

  create_lookup 'BLUE'  gobj_t001w 'CONVERT_RANGES_01'
       'CRAP' sy-repid '1000'.


  CALL METHOD gobj_execute->add_look_up
    EXPORTING
      im_it_lu = it_lookup.

  CALL METHOD gobj_execute->apply_selection
    EXPORTING
      im_ucomm = 'BLUE'
*     im_layout    = p_layout
*     im_maxhits   = pmaxhits
*     im_go_to_alv = pgoalv
    IMPORTING
      ex_dynnr = gc_1000_tc.

ENDFORM.



FORM back_exit_cancel_input .
  CASE sy-ucomm.
    WHEN 'EXIT' OR 'CANCEL' OR 'BACK'.
      SET SCREEN 0.
      LEAVE.
  ENDCASE.
ENDFORM.
