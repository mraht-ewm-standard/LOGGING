*"* use this source file for your ABAP unit test classes
CLASS ltc_log DEFINITION FOR TESTING RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    METHODS test FOR TESTING.

ENDCLASS.



CLASS ltc_log IMPLEMENTATION.

  METHOD test.

    zial_cl_log=>create( iv_object    = 'ZIAL_LOG'
                         iv_subobject = 'MFS'
                         iv_extnumber = |AbapUnitTest: LCL_LOG| ).

    MESSAGE s002(sy) WITH |A success message| INTO DATA(lv_msg).
    zial_cl_log=>get( )->log_message( VALUE #( ( fnam = 'HUIDENT' low = '123456' ) ) ).

    zial_cl_log=>save( ).

  ENDMETHOD.

ENDCLASS.
