"! <p class="shorttext synchronized">ABAP Unit Test: Template</p>
CLASS ltc_log DEFINITION FINAL
  CREATE PUBLIC
  FOR TESTING RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    TYPES t_dummy TYPE STANDARD TABLE OF dummy WITH EMPTY KEY.

    TYPES: BEGIN OF s_tdc_data,
             log_object1    TYPE balobj_d,
             log_subobject1 TYPE balsubobj,
             log_object2    TYPE balobj_d,
             log_subobject2 TYPE balsubobj,
             log_object3    TYPE balobj_d,
             log_subobject3 TYPE balsubobj,
             t_log_cnf      TYPE zial_tt_log_cnf,
           END OF s_tdc_data.

    CONSTANTS mc_tdc_cnt TYPE etobj_name VALUE 'ZIAL_TDC_LOG_SAP'.

    CLASS-DATA mo_aunit    TYPE REF TO zial_cl_aunit.
    CLASS-DATA ms_tdc_data TYPE s_tdc_data.

    CLASS-METHODS class_setup
      RAISING cx_ecatt_tdc_access.

    CLASS-METHODS class_teardown.

    METHODS setup.
    METHODS teardown.

    METHODS t0001 FOR TESTING RAISING cx_static_check.
    METHODS t0002 FOR TESTING RAISING cx_static_check.
    METHODS t0003 FOR TESTING RAISING cx_static_check.
    METHODS t0004 FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltc_log IMPLEMENTATION.

  METHOD class_setup.

    mo_aunit = zial_cl_aunit=>on_class_setup(
                   iv_tdc_cnt    = mc_tdc_cnt
                   iv_ign_errors = abap_true
                   ir_tdc_data   = REF #( ms_tdc_data )
                   it_sql_data   = VALUE #( ( tbl_name = 'ZIAL_T_LOG_CNF'
                                              tbl_data = REF #( ms_tdc_data-t_log_cnf ) ) ) ).

  ENDMETHOD.


  METHOD setup.

    mo_aunit->on_setup( ).

    zial_cl_log=>free( ).

  ENDMETHOD.


  METHOD teardown.

    mo_aunit->on_teardown( ).

  ENDMETHOD.


  METHOD class_teardown.

    mo_aunit->on_class_teardown( ).

  ENDMETHOD.


  METHOD t0001.

    CHECK mo_aunit->active( abap_true ).

    CLEAR: sy-msgty,
           sy-msgid,
           sy-msgno.

    DATA(lo_log) = CAST zial_cl_log_sap( zial_cl_log=>get( ) ).
    DO zial_cl_log_act=>mc_default-max_num_of_entries - 4 TIMES.
      INSERT VALUE #( ) INTO TABLE lo_log->mt_bapiret2.
    ENDDO.

    zial_cl_log=>get( )->log_message( iv_msgty = zial_cl_log=>mc_msgty-info
                                      iv_msgtx = |LOG_INFO| ).

    zial_cl_log=>get( )->save( ).

  ENDMETHOD.


  METHOD t0002.

    " No input => configured default

    CHECK mo_aunit->active( abap_true ).

    DATA(lo_log) = zial_cl_log=>create( ).
    DATA(ls_log_hdr) = lo_log->get_log_hdr( ).

    cl_abap_unit_assert=>assert_equals( exp = ls_log_hdr-object
                                        act = ms_tdc_data-log_object1 ).

    cl_abap_unit_assert=>assert_equals( exp = ls_log_hdr-subobject
                                        act = ms_tdc_data-log_subobject1 ).

  ENDMETHOD.


  METHOD t0003.

    " Valid input => input

    CHECK mo_aunit->active( abap_true ).

    DATA(lo_log) = zial_cl_log=>create( iv_object    = ms_tdc_data-log_object2
                                        iv_subobject = ms_tdc_data-log_subobject2 ).
    DATA(ls_log_hdr) = lo_log->get_log_hdr( ).

    cl_abap_unit_assert=>assert_equals( exp = ls_log_hdr-object
                                        act = ms_tdc_data-log_object2 ).

    cl_abap_unit_assert=>assert_equals( exp = ls_log_hdr-subobject
                                        act = ms_tdc_data-log_subobject2 ).

  ENDMETHOD.


  METHOD t0004.

    " Invalid input => configured default

    CHECK mo_aunit->active( abap_true ).

    DATA(lo_log) = zial_cl_log=>create( iv_object    = ms_tdc_data-log_object3
                                        iv_subobject = ms_tdc_data-log_subobject3 ).
    DATA(ls_log_hdr) = lo_log->get_log_hdr( ).

    cl_abap_unit_assert=>assert_equals( exp = ls_log_hdr-object
                                        act = ms_tdc_data-log_object1 ).

    cl_abap_unit_assert=>assert_equals( exp = ls_log_hdr-subobject
                                        act = ms_tdc_data-log_subobject1 ).

  ENDMETHOD.

ENDCLASS.
