*"* use this source file for your ABAP unit test classes
CLASS ltc_log DEFINITION FOR TESTING RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    METHODS test FOR TESTING.

ENDCLASS.



CLASS ltc_log IMPLEMENTATION.

  METHOD test.

    zdgl_cl_log=>create( iv_object    = 'ZDGL_LOG'
                         iv_subobject = 'MFS'
                         iv_extnumber = |AbapUnitTest: LCL_LOG| ).

    zdgl_cl_log=>get( )->log_success( iv_msgtx = |A success message|
                                  it_msgde = VALUE #( ( fnam = 'HUIDENT' low = '123456' ) ) ).

    zdgl_cl_log=>save( ).

  ENDMETHOD.

ENDCLASS.
