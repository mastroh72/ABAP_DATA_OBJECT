CLASS zcl_do_gt_event DEFINITION
  PUBLIC
  INHERITING FROM zcl_do
  CREATE PUBLIC ABSTRACT.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ts_data_change,
        o_data   TYPE REF TO data,
        o_struct_type TYPE REF TO cl_abap_structdescr,
      END OF ts_data_change.

    CONSTANTS c_link_reposition TYPE zdo_object_relationship VALUE 'REPOS' ##NO_TEXT.
    CONSTANTS c_link_data TYPE zdo_object_relationship VALUE 'DATA' ##NO_TEXT.
    CONSTANTS c_link_data_filter TYPE zdo_object_relationship VALUE 'DATAFILTER' ##NO_TEXT.

    EVENTS screen_data_changed.

    METHODS on_screen_data_changed
          FOR EVENT screen_data_changed OF zcl_do_gt_event
      IMPORTING
          !sender .

    METHODS get_screen_data_change
      RETURNING VALUE(rs_data_change) TYPE ts_data_change.
*      CHANGING
*        !ch_it_field_values TYPE zslot_tt_do_field_values .
    METHODS lk_deactivate_all_publishers
      IMPORTING
        VALUE(iv_relationship) TYPE zdo_object_relationship DEFAULT zcl_do_gt_event=>c_link_data .
    METHODS lk_set_active_publisher_id
      IMPORTING
        !iv_publisher_id       TYPE zdo_object_id
        VALUE(iv_relationship) TYPE zdo_object_relationship DEFAULT zcl_do_gt_event=>c_link_data .
    METHODS lk_add_link
      IMPORTING
        !io_publisher    TYPE REF TO zcl_do_gt_event
        !io_subscriber   TYPE REF TO zcl_do_gt_event OPTIONAL
        !iv_relationship TYPE zdo_object_relationship .

  PROTECTED SECTION.

    DATA mo_struct TYPE REF TO data.
    DATA mo_struct_type TYPE REF TO cl_abap_structdescr.

    METHODS event_data_refresh
      IMPORTING is_data TYPE ts_data_change.
    METHODS event_data_filter
      IMPORTING is_data TYPE ts_data_change.
    METHODS event_data_reposition
      IMPORTING is_data TYPE ts_data_change.


  PRIVATE SECTION.
    TYPES:
      BEGIN OF ts_event_association,
        relationship TYPE zdo_object_relationship,
        id           TYPE zdo_object_id,
        active       TYPE abap_bool,
      END OF ts_event_association,
      tt_event_association
        TYPE STANDARD TABLE OF ts_event_association WITH DEFAULT KEY.

    DATA mt_event_association TYPE tt_event_association .

ENDCLASS.



CLASS zcl_do_gt_event IMPLEMENTATION.


  METHOD on_screen_data_changed.

    DATA :
*      lb_raise_event  TYPE abap_bool,
      lv_relationship TYPE zdo_object_relationship,
      ls_data         TYPE ts_data_change,
      i_times         TYPE i.

    FIELD-SYMBOLS :
      <ls_event_assoication>        TYPE LINE OF tt_event_association.


    DO 3 TIMES.
      i_times = sy-index.

      CASE i_times.
        WHEN 1.
          lv_relationship = zcl_do_gt_event=>c_link_data.
        WHEN 2.
          lv_relationship = zcl_do_gt_event=>c_link_data_filter.
        WHEN 3.
          lv_relationship = zcl_do_gt_event=>c_link_reposition.
      ENDCASE.

      READ TABLE mt_event_association ASSIGNING <ls_event_assoication>
        WITH KEY  relationship = lv_relationship
                  active = abap_true
                  id = sender->id.
      IF sy-subrc = 0.
*        lb_raise_event = abap_true.

        ls_data = sender->get_screen_data_change(  ).

        CASE  lv_relationship.
          WHEN zcl_do_gt_event=>c_link_data.
            me->event_data_refresh( ls_data ).
          WHEN zcl_do_gt_event=>c_link_data_filter.
            me->event_data_filter( ls_data ).
          WHEN zcl_do_gt_event=>c_link_reposition.
            me->event_data_reposition( ls_data  ).
        ENDCASE.

      ENDIF.  "  Mt_event_association  was found

    ENDDO.

*    IF  lb_raise_event = abap_true.
*      RAISE EVENT screen_data_changed.
*    ENDIF.
  ENDMETHOD.


  METHOD lk_deactivate_all_publishers .
* ...

    FIELD-SYMBOLS :
      <ls_event_association>          TYPE LINE OF  tt_event_association.

    LOOP AT mt_event_association ASSIGNING <ls_event_association>
      WHERE relationship = iv_relationship
        AND active = abap_true.
      <ls_event_association>-active = abap_false.
    ENDLOOP.
  ENDMETHOD.


  METHOD lk_set_active_publisher_id.

    FIELD-SYMBOLS :
      <ls_event_association>          TYPE LINE OF  tt_event_association.


    READ TABLE mt_event_association ASSIGNING <ls_event_association>
       WITH KEY relationship = iv_relationship
                id = iv_publisher_id.
    IF sy-subrc = 0.
      <ls_event_association>-active = abap_true.
    ENDIF.
  ENDMETHOD.



  METHOD lk_add_link .
* ...

*IM_PTR_PUBLISHER
*IM_PTR_SUBSCRIBER
*IM_IT_RELATIONSHIP

    DATA :
      ptr_subscriber       TYPE REF TO zcl_do_gt_event,
      ls_event_association LIKE LINE OF mt_event_association.



    IF io_subscriber IS INITIAL.
      ptr_subscriber = me.
    ELSE.
      ptr_subscriber = io_subscriber.
    ENDIF.

    CALL METHOD io_publisher->add_published
      EXPORTING
        im_id           = ptr_subscriber->id
        im_relationship = iv_relationship
        im_ptr_object   = ptr_subscriber.

    CALL METHOD ptr_subscriber->add_subscribed
      EXPORTING
        im_id           = io_publisher->id
        im_relationship = iv_relationship
        im_ptr_object   = io_publisher.

    CASE iv_relationship.
      WHEN zcl_do_gt_event=>c_link_data
        OR zcl_do_gt_event=>c_link_reposition
        OR zcl_do_gt_event=>c_link_data_filter.
        SET HANDLER  ptr_subscriber->on_screen_data_changed
            FOR io_publisher.
        READ TABLE mt_event_association
          WITH KEY relationship = iv_relationship
                   id = io_publisher->id
          ASSIGNING FIELD-SYMBOL(<ls_event_association>).
        IF sy-subrc = 0.

        ELSE.
          ls_event_association-relationship = iv_relationship.
          ls_event_association-id = io_publisher->id.
          ls_event_association-active = 'X'.
          APPEND ls_event_association TO mt_event_association.
        ENDIF.
    ENDCASE.

  ENDMETHOD.

   METHOD event_data_refresh.

  ENDMETHOD.

  METHOD event_data_filter.

  ENDMETHOD.

  METHOD event_data_reposition.

  ENDMETHOD.

  METHOD get_screen_data_change.
    rs_data_change-o_data = mo_struct.
    rs_data_change-o_struct_type = mo_struct_type.
  ENDMETHOD.


ENDCLASS.
