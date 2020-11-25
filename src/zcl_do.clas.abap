CLASS zcl_do DEFINITION
  PUBLIC.


 PUBLIC SECTION.

    DATA id TYPE ZDO_OBJECT_ID .

    METHODS constructor .
    METHODS get_id
      EXPORTING
        !ex_id TYPE ZDO_OBJECT_ID .
    TYPE-POOLS zclob .

  PROTECTED SECTION.

    TYPE-POOLS zclob .
    DATA it_published TYPE zif_do_clob=>tt_obj_relationship .
    DATA it_subscribed TYPE zif_do_clob=>tt_obj_relationship .

    METHODS add_published
      IMPORTING
        !im_id           TYPE ZDO_OBJECT_ID
        !im_relationship TYPE  zdo_object_relationship
        !im_ptr_object   TYPE REF TO object .
    METHODS add_subscribed
      IMPORTING
        !im_id           TYPE ZDO_OBJECT_ID
        !im_relationship TYPE  zdo_object_relationship
        !im_ptr_object   TYPE REF TO object .
    METHODS delete_published
      IMPORTING
        !im_id           TYPE ZDO_OBJECT_ID
        !im_relationship TYPE  zdo_object_relationship .
    METHODS delete_subscribed
      IMPORTING
        !im_id           TYPE ZDO_OBJECT_ID
        !im_relationship TYPE  zdo_object_relationship .
    METHODS get_published
      IMPORTING
        !im_relationship  TYPE  zdo_object_relationship
      EXPORTING
        VALUE(ex_it_link) TYPE zif_do_clob=>tt_objects .
    METHODS get_subscribed
      IMPORTING
        !im_relationship  TYPE  zdo_object_relationship
      EXPORTING
        VALUE(ex_it_link) TYPE zif_do_clob=>tt_objects .

  PRIVATE SECTION.

    CLASS-DATA next_object_id TYPE ZDO_OBJECT_ID VALUE 1 .
    CLASS-DATA next_link_id TYPE ZDO_OBJECT_ID VALUE 1 .
ENDCLASS.



CLASS ZCL_DO IMPLEMENTATION.


  METHOD add_published.
* ...

    DATA :
      wa     TYPE zif_do_clob=>ts_obj_relationship.

    READ TABLE it_published
      WITH KEY id = im_id  relationship = im_relationship
      TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
      wa-id = im_id.
      wa-relationship = im_relationship.
      wa-ptr_obj  = im_ptr_object.
      APPEND wa TO it_published.
    ENDIF.

  ENDMETHOD.


  METHOD add_subscribed.
* ...

    DATA :
      wa     TYPE zif_do_clob=>ts_obj_relationship.

    READ TABLE it_subscribed
      WITH KEY id = im_id  relationship = im_relationship
      TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
      wa-id = im_id.
      wa-relationship = im_relationship.
      wa-ptr_obj  = im_ptr_object.
      APPEND wa TO it_subscribed.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
* ...

    id = next_object_id.
    next_object_id = next_object_id + 1.

  ENDMETHOD.


  METHOD delete_published.
* ...

    DELETE it_published
      WHERE id = im_id AND relationship = im_relationship.
  ENDMETHOD.


  METHOD delete_subscribed.
* ...

    DELETE it_subscribed
      WHERE id = im_id AND relationship = im_relationship.

  ENDMETHOD.


  METHOD get_id.
* ...

    ex_id = id.
  ENDMETHOD.


  METHOD get_published .
* ...

    DATA :
      wa_object TYPE LINE OF zif_do_clob=>tt_objects.

    FIELD-SYMBOLS
      <wa_published>   TYPE LINE OF zif_do_clob=>tt_obj_relationship.

    LOOP AT it_published ASSIGNING <wa_published>
      WHERE  relationship = im_relationship.
      wa_object-id = <wa_published>-id.
      wa_object-ptr_obj =  <wa_published>-ptr_obj.
      APPEND wa_object TO ex_it_link.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_subscribed.
* ...

    DATA :
      wa_object TYPE LINE OF zif_do_clob=>tt_objects.

    FIELD-SYMBOLS
      <wa_subscribed>   TYPE LINE OF zif_do_clob=>tt_obj_relationship.

    LOOP AT it_subscribed ASSIGNING <wa_subscribed>
      WHERE  relationship = im_relationship.
      wa_object-id = <wa_subscribed>-id.
      wa_object-ptr_obj =  <wa_subscribed>-ptr_obj.
      APPEND wa_object TO ex_it_link.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
