*"* use this source file for your ABAP unit test classes
CLASS lcl_log DEFINITION FOR TESTING RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    METHODS test FOR TESTING.

ENDCLASS.



CLASS lcl_log IMPLEMENTATION.

  METHOD test.

    ziot_cl_log=>init( iv_lgnum     = '0100'
                       iv_object    = '/SCWM/WME'
                       iv_subobject = 'MFS'
                       iv_extnumber = |AbapUnitTest: LCL_LOG| ).

    ziot_cl_log=>get( )->success( msgtx = |A success message| ).

    ziot_cl_log=>save( ).

  ENDMETHOD.

ENDCLASS.
