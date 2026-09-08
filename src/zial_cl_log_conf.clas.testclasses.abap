"! <p class="shorttext synchronized">ABAP Unit Test: Template</p>
CLASS ltc_log_cnf DEFINITION FINAL
  CREATE PUBLIC
  FOR TESTING RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    TYPES t_dummy TYPE STANDARD TABLE OF dummy WITH EMPTY KEY.

    TYPES: BEGIN OF s_tdc_data,
             dflt_object1    TYPE balobj_d,
             dflt_subobject1 TYPE balsubobj,
             dflt_object2    TYPE balobj_d,
             dflt_subobject2 TYPE balsubobj,
             t_log_cnf1      TYPE zial_tt_log_cnf,
             t_log_cnf2      TYPE zial_tt_log_cnf,
           END OF s_tdc_data.

    CONSTANTS mc_tdc_cnt TYPE etobj_name VALUE 'ZIAL_TDC_LOG_CNF'.

    CLASS-DATA mo_aunit    TYPE REF TO zial_cl_aunit.
    CLASS-DATA ms_tdc_data TYPE s_tdc_data.

    CLASS-DATA mo_osql_test_environment TYPE REF TO if_osql_test_environment.

    CLASS-METHODS class_setup
      RAISING cx_ecatt_tdc_access.

    CLASS-METHODS class_teardown.

    METHODS setup.
    METHODS teardown.

    METHODS t0001 FOR TESTING RAISING cx_static_check.
    METHODS t0002 FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltc_log_cnf IMPLEMENTATION.

  METHOD class_setup.

    mo_aunit = zial_cl_aunit=>on_class_setup( iv_tdc_cnt  = mc_tdc_cnt
                                              ir_tdc_data = REF #( ms_tdc_data ) ).

  ENDMETHOD.


  METHOD setup.

    mo_aunit->on_setup( ).

  ENDMETHOD.


  METHOD teardown.

    mo_aunit->on_teardown( ).

  ENDMETHOD.


  METHOD class_teardown.

    mo_aunit->on_class_teardown( ).

  ENDMETHOD.


  METHOD t0001.

    CHECK mo_aunit->is_active( abap_true ).

    mo_aunit->set_sql_data( VALUE #( ( tbl_name = 'ZIAL_T_LOG_CNF'
                                       tbl_data = REF #( ms_tdc_data-t_log_cnf1 ) ) ) ).

    zial_cl_log_cnf=>free( ).
    DATA(ls_log_cnf) = zial_cl_log_cnf=>get( ).

    cl_abap_unit_assert=>assert_equals( exp = ms_tdc_data-dflt_object1
                                        act = ls_log_cnf-dflt_object ).

    cl_abap_unit_assert=>assert_equals( exp = ms_tdc_data-dflt_subobject1
                                        act = ls_log_cnf-dflt_subobject ).

  ENDMETHOD.


  METHOD t0002.

    CHECK mo_aunit->is_active( abap_true ).

    mo_aunit->set_sql_data( VALUE #( ( tbl_name = 'ZIAL_T_LOG_CNF'
                                       tbl_data = REF #( ms_tdc_data-t_log_cnf2 ) ) ) ).

    zial_cl_log_cnf=>free( ).
    DATA(ls_log_cnf) = zial_cl_log_cnf=>get( ).

    cl_abap_unit_assert=>assert_equals( exp = ms_tdc_data-dflt_object2
                                        act = ls_log_cnf-dflt_object ).

    cl_abap_unit_assert=>assert_equals( exp = ms_tdc_data-dflt_subobject2
                                        act = ls_log_cnf-dflt_subobject ).

  ENDMETHOD.

ENDCLASS.
