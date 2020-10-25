FUNCTION ztb_tools_01_ddfields.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_STRUCTURE) TYPE REF TO  CL_ABAP_STRUCTDESCR
*"  EXPORTING
*"     REFERENCE(IT_DDICFIELDS) TYPE  DDFIELDS
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  DATA(it_components) = im_structure->get_components( ).
  DATA(wa_struct_name) = im_structure->get_relative_name( ).

  REFRESH git_ddfields.
  PERFORM build_ddfields
              TABLES
                 it_components
              USING wa_struct_name.
  it_ddicfields[] = git_ddfields[].






ENDFUNCTION.
