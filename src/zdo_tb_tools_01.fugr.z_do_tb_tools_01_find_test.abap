FUNCTION Z_DO_TB_TOOLS_01_FIND_TEST.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"--------------------------------------------------------------------

  DATA :
    it_cars TYPE ZTTZCARS_LABEL_AND_ZKNMT,
    it_fields TYPE STANDARD TABLE OF zcars_item.

  TABLES : zcars_item.

 SELECT *
    INTO TABLE it_cars
    FROM zcars_item
    WHERE carsid = 102.




  CALL FUNCTION 'ZTB_TOOLS_01_FIND'
       EXPORTING
            findintype = 'ZCARS_LABEL_AND_ZKNMT'
       TABLES
            findin     = it_cars
            fields     = it_fields.
  .




ENDFUNCTION.
