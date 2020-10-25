CLASS zcl_do_gt_struct DEFINITION
  PUBLIC
  INHERITING FROM zcl_do_gt_event
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS assign_structure
      IMPORTING io_struct TYPE REF TO data.
  PROTECTED SECTION.
    METHODS event_data_refresh REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_do_gt_struct IMPLEMENTATION.
  METHOD assign_structure.
    mo_struct = io_struct.
    mo_struct_type ?= cl_abap_structdescr=>describe_by_data_ref( mo_struct ).
  ENDMETHOD.

  METHOD event_data_refresh.
    FIELD-SYMBOLS:
      <ls_new_value> TYPE any,
      <ls_struct>    TYPE any.

    IF mo_struct_type->absolute_name = is_data-o_struct_type->absolute_name.
      ASSIGN is_data-o_data->* TO <ls_new_value>.
      ASSIGN mo_struct->* TO <ls_struct>.
      IF <ls_new_value> IS ASSIGNED AND
         <ls_struct> IS ASSIGNED.
* should check if the 2 pointers are the same type.
        <ls_struct> = <ls_new_value>.
      ENDIF.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
