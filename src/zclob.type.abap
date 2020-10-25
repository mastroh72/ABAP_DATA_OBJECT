TYPE-POOL zclob .
TYPES :
  BEGIN OF  zclob_st_DO_objects,
    id      TYPE zdo_object_id,
    ptr_obj TYPE REF TO object,
  END OF  zclob_st_DO_objects,
  zclob_TT_DO_objects TYPE STANDARD TABLE OF zclob_st_DO_objects WITH DEFAULT KEY,
  BEGIN OF  zclob_st_DO_obj_relationship,
    id           TYPE zdo_object_id,
    relationship TYPE ZDO_OBJECT_RELATIONSHIP,
    ptr_obj      TYPE REF TO object,
  END OF  zclob_st_DO_obj_relationship,
  zclob_TT_DO_obj_relationship
    TYPE STANDARD TABLE OF zclob_st_DO_obj_relationship WITH DEFAULT KEY.
