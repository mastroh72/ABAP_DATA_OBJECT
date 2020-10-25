FUNCTION ZTB_TOOLS_01_ALV.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_EVENTS) TYPE  SLIS_T_EVENT
*"     REFERENCE(IM_SAVE) TYPE  CHAR1
*"     REFERENCE(IM_VARIANT) TYPE  DISVARIANT
*"     REFERENCE(IM_STRUCTURE_NAME) LIKE  DD02L-TABNAME
*"     REFERENCE(IM_FIELDCAT) TYPE  SLIS_T_FIELDCAT_ALV
*"     REFERENCE(IM_DATA_OBJECT) TYPE REF TO  ZCL_DO_IT
*"     REFERENCE(IM_SORT) TYPE  SLIS_T_SORTINFO_ALV
*"  TABLES
*"      IT_OUTTAB TYPE  STANDARD TABLE
*"----------------------------------------------------------------------
  gobj_data_object = im_data_object.

**   I_INTERFACE_CHECK                 = ' '
**   I_BYPASSING_BUFFER                =
**   I_BUFFER_ACTIVE                   = ' '
**   I_CALLBACK_PF_STATUS_SET          = ' '
**   I_CALLBACK_USER_COMMAND           = ' '
**   I_CALLBACK_TOP_OF_PAGE            = ' '
**   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
**   I_CALLBACK_HTML_END_OF_LIST       = ' '
**   I_BACKGROUND_ID                   = ' '
**   I_GRID_TITLE                      =
**   I_GRID_SETTINGS                   =
**   IS_LAYOUT                         =
**   IT_EXCLUDING                      =
**   IT_SPECIAL_GROUPS                 =
**   IT_SORT                           =
**   IT_FILTER                         =
**   IS_SEL_HIDE                       =
**   I_DEFAULT                         = 'X'
**   I_SAVE                            = ' '
**   IS_VARIANT                        =
**   IT_EVENTS                         =
**   IT_EVENT_EXIT                     =
**   IS_PRINT                          =
**   IS_REPREP_ID                      =
**   I_SCREEN_START_COLUMN             = 0
**   I_SCREEN_START_LINE               = 0
**   I_SCREEN_END_COLUMN               = 0
**   I_SCREEN_END_LINE                 = 0
**   IT_ALV_GRAPHICS                   =
**   IT_ADD_FIELDCAT                   =
**   IT_HYPERLINK                      =
**   I_HTML_HEIGHT_TOP                 =
**   I_HTML_HEIGHT_END                 =
** IMPORTING
**   E_EXIT_CAUSED_BY_CALLER           =
**   ES_EXIT_CAUSED_BY_USER            =

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program =   g_repid
            i_callback_user_command  = slis_ev_user_command
            it_sort            = im_sort[]
            it_events          = im_events[]
            i_save             = im_save
            is_variant         = im_variant
            i_structure_name   = im_structure_name
            it_fieldcat        = im_fieldcat
       TABLES
            t_outtab           = it_outtab
       EXCEPTIONS
            program_error      = 1
            OTHERS             = 2.
*          .







ENDFUNCTION.
