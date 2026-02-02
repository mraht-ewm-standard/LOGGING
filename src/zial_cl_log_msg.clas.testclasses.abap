"! <p class="shorttext synchronized">ABAP Unit Test: Template</p>
CLASS ltc_log_msg DEFINITION FINAL
  CREATE PUBLIC
  FOR TESTING RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    TYPES t_dummy TYPE STANDARD TABLE OF dummy WITH EMPTY KEY.

    TYPES: BEGIN OF s_tdc_data,
             t_dummy TYPE t_dummy,
           END OF s_tdc_data.

    CONSTANTS mc_tdc_cnt TYPE etobj_name VALUE 'ZIAL_TDC_LOG_MSG'.

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

ENDCLASS.


CLASS ltc_log_msg IMPLEMENTATION.

  METHOD class_setup.

    mo_aunit = zial_cl_aunit=>on_class_setup( iv_tdc_cnt    = mc_tdc_cnt
                                              iv_ign_errors = abap_true
                                              ir_tdc_data   = REF #( ms_tdc_data ) ).

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

    CHECK mo_aunit->is_active( abap_true ).

    MESSAGE s499(sy) WITH 'LGNUM' 'HUID' 'RSRC' 'NLPLA' INTO DATA(lv_exp_msgtx) ##NEEDED.
    DATA(lv_msgtx) = zial_cl_log_msg=>to_string( ).

    cl_abap_unit_assert=>assert_equals( exp = |LGNUM HUID RSRC NLPLA|
                                        act = lv_msgtx ).

  ENDMETHOD.


  METHOD t0002.

    CHECK mo_aunit->is_active( abap_true ).

    DATA(ls_exp_message) = zial_cl_log_msg=>to_bapiret( iv_msgtx = |&1 &2 &3 &4|
                                                        iv_msgv1 = 'LGNUM'
                                                        iv_msgv2 = 'HUID'
                                                        iv_msgv3 = 'RSRC'
                                                        iv_msgv4 = 'NLPLA' ).
    cl_abap_unit_assert=>assert_not_initial( ls_exp_message ).

  ENDMETHOD.


  METHOD t0003.

    CHECK mo_aunit->is_active( abap_true ).

    DATA(lv_msgtx) = zial_cl_log_msg=>to_string( iv_msgtx = 'Test: &1 &2 &3 &4'
                                                 iv_msgv1 = 'LGNUM'
                                                 iv_msgv2 = 'HUID'
                                                 iv_msgv3 = 'RSRC'
                                                 iv_msgv4 = 'NLPLA' ).

    cl_abap_unit_assert=>assert_equals( exp = |Test: LGNUM HUID RSRC NLPLA|
                                        act = lv_msgtx ).

  ENDMETHOD.

ENDCLASS.
