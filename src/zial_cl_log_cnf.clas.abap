CLASS zial_cl_log_cnf DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS get
      RETURNING VALUE(rs_log_cnf) TYPE zial_t_log_cnf.

    CLASS-METHODS is_valid_log_object
      IMPORTING iv_object        TYPE balobj_d
      RETURNING VALUE(rv_result) TYPE abap_bool.

    CLASS-METHODS is_valid_log_subobject
      IMPORTING iv_object        TYPE balobj_d
                iv_subobject     TYPE balsubobj
      RETURNING VALUE(rv_result) TYPE abap_bool.

    CLASS-METHODS free.

  PROTECTED SECTION.
    CONSTANTS: BEGIN OF mc_default,
                 log_object    TYPE balobj_d  VALUE 'SYSLOG' ##NO_TEXT, " Adjust to your needs
                 log_subobject TYPE balsubobj VALUE 'GENERAL' ##NO_TEXT,
               END OF mc_default.

    CLASS-DATA ms_log_cnf TYPE zial_t_log_cnf.

    CLASS-METHODS read.

ENDCLASS.


CLASS zial_cl_log_cnf IMPLEMENTATION.

  METHOD free.

    CLEAR ms_log_cnf.

  ENDMETHOD.


  METHOD get.

    IF ms_log_cnf IS INITIAL.
      read( ).
    ENDIF.

    rs_log_cnf = ms_log_cnf.

  ENDMETHOD.


  METHOD read.

    SELECT SINGLE FROM zial_t_log_cnf
      FIELDS *
      INTO @ms_log_cnf.

    IF    NOT is_valid_log_object( ms_log_cnf-dflt_object )
       OR NOT is_valid_log_subobject( iv_object    = ms_log_cnf-dflt_object
                                      iv_subobject = ms_log_cnf-dflt_subobject ).
      CLEAR: ms_log_cnf-dflt_object,
             ms_log_cnf-dflt_subobject.
    ENDIF.

    IF    ms_log_cnf-dflt_object    IS INITIAL
       OR ms_log_cnf-dflt_subobject IS INITIAL.
      ms_log_cnf-dflt_object    = mc_default-log_object.
      ms_log_cnf-dflt_subobject = mc_default-log_subobject.
    ENDIF.

  ENDMETHOD.


  METHOD is_valid_log_object.

    CHECK iv_object IS NOT INITIAL.

    CALL FUNCTION 'BAL_OBJECT_SELECT'
      EXPORTING  i_object = iv_object
      EXCEPTIONS OTHERS   = 99.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    rv_result = abap_true.

  ENDMETHOD.


  METHOD is_valid_log_subobject.

    CHECK iv_object    IS NOT INITIAL
      AND iv_subobject IS NOT INITIAL.

    CALL FUNCTION 'BAL_SUBOBJECT_SELECT'
      EXPORTING  i_object    = iv_object
                 i_subobject = iv_subobject
      EXCEPTIONS OTHERS      = 99.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    rv_result = abap_true.

  ENDMETHOD.

ENDCLASS.
