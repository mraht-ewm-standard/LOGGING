"! <p class="shorttext synchronized" lang="en">Logging: EWM Log</p>
CLASS zial_cl_log_ewm DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  INHERITING FROM zial_cl_log_sap.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        !iv_lgnum         TYPE /scwm/lgnum OPTIONAL
        !io_sap_log       TYPE REF TO /scwm/cl_log OPTIONAL
        !iv_object        TYPE balobj_d DEFAULT '/SCWM/WME'
        !iv_subobject     TYPE balsubobj
        !iv_extnumber     TYPE balnrext OPTIONAL
        !it_extnumber     TYPE stringtab OPTIONAL
        !iv_callstack_lvl TYPE numc1 DEFAULT zial_cl_log=>mc_callstack_lvl-info.

    "! Log SAP log messages
    "!
    "! @parameter io_log | SAP log object
    METHODS log_saplog
      IMPORTING
        !io_log TYPE REF TO /scwm/cl_log .

    "! Log API messages
    "!
    "! @parameter io_api_message | API log object
    METHODS log_api_message
      IMPORTING
        !io_api_message TYPE REF TO /scwm/if_api_message.
    "! Log delivery management messages
    "!
    "! @parameter io_dm_message | Delivery management log object
    "! @parameter it_dm_message | Delivery management log messages
    METHODS log_dm_message
      IMPORTING
        !io_dm_message TYPE REF TO /scwm/cl_dm_message_no OPTIONAL
        !it_dm_message TYPE /scdl/dm_message_tab.

  PROTECTED SECTION.
    CLASS-DATA: mo_instance   TYPE REF TO zial_cl_log_ewm,
                mv_has_error  TYPE abap_bool,
                mv_save_error TYPE abap_bool.

    DATA: mv_lgnum   TYPE /scwm/lgnum,
          mo_sap_log TYPE REF TO /scwm/cl_log.

    DATA message_text TYPE bapi_msg .
    DATA message_type TYPE symsgty .
    DATA content_type TYPE i .
    DATA message_class TYPE symsgid .
    DATA message_params TYPE bal_s_parm .
    DATA message_number TYPE symsgno .
    DATA message_var1 TYPE symsgv .
    DATA message_var2 TYPE symsgv .
    DATA message_var3 TYPE symsgv .
    DATA message_var4 TYPE symsgv .
    DATA message_priority TYPE balprobcl .
    DATA message_context TYPE bal_s_cont .

    DATA message_detail TYPE /scwm/tt_msg_details.

    METHODS handle_error REDEFINITION.

    METHODS build_validity REDEFINITION.

    METHODS add_msg_by_message_object REDEFINITION.
    METHODS add_msg_by_message_text REDEFINITION.

ENDCLASS.



CLASS zial_cl_log_ewm IMPLEMENTATION.

  METHOD add_msg_by_message_object.

    IF mo_sap_log IS BOUND.
      mo_sap_log->add_message( ip_msgty = message_type
                               ip_msgid = message_class
                               ip_msgno = message_number
                               ip_msgv1 = message_var1
                               ip_msgv2 = message_var2
                               ip_msgv3 = message_var3
                               ip_msgv4 = message_var4 ).
    ENDIF.

    super->add_msg_by_message_object( ).

  ENDMETHOD.


  METHOD add_msg_by_message_text.

    IF mo_sap_log IS BOUND.
      mo_sap_log->add_message( ip_msgty = message_type
                               ip_msg   = message_text ).
    ENDIF.

    super->add_msg_by_message_text( ).

  ENDMETHOD.


  METHOD build_validity.

    DATA(ls_log_act) = VALUE /scwm/log_act( ).

    DO 2 TIMES.

      CASE sy-index.
        WHEN 1.
          " Note: Configure Z-Subobject of Object
          " /SCWM/WME in Transaction/SCWM/ACTLOG
          CALL FUNCTION '/SCWM/LOG_ACT_READ_SINGLE'
            EXPORTING
              iv_lgnum     = me->mv_lgnum
              iv_subobject = ms_log_header-subobject
            IMPORTING
              es_log_act   = ls_log_act
            EXCEPTIONS
              not_found    = 1
              OTHERS       = 2.

        WHEN 2.
          ls_log_act-lgnum     = me->mv_lgnum.
          ls_log_act-subobject = ms_log_header-subobject.
          ls_log_act-validity  = me->mv_validity_in_days.

      ENDCASE.

      IF    sy-subrc            = 0
        AND ls_log_act-validity > 0.

        " Append valid expiration date
        CALL FUNCTION '/SCWM/APP_LOG_EXPIRY_DATE_DET'
          EXPORTING
            is_log_act = ls_log_act
          CHANGING
            cs_log     = ms_log_header.

        EXIT.

      ENDIF.

    ENDDO.

  ENDMETHOD.


  METHOD handle_error.

    " Ensures that error handling is only executed once for each error to avoid endless loop
    CHECK mv_has_error  EQ abap_false
      AND mv_save_error EQ abap_false.
    mv_has_error = abap_true.

*** Backup input data
    DATA(ls_log_msg) = VALUE zial_s_log_msg( hdr = VALUE #( object     = ms_log_header-object
                                                            subobject  = ms_log_header-subobject
                                                            extnumber  = ms_log_header-extnumber
                                                            aldate_del = ms_log_header-aldate_del )
                                             msg = VALUE #( msgid = mv_msg_class
                                                            msgty = mv_msg_type
                                                            msgtx = mv_msg_text
                                                            msgno = mv_msg_number
                                                            msgv1 = mv_msg_var1
                                                            msgv2 = mv_msg_var2
                                                            msgv3 = mv_msg_var3
                                                            msgv4 = mv_msg_var4 ) ).

    " Try to add error messages regarding failed logging to old log
    MESSAGE e001(zial_log) INTO DATA(lv_msg).
    log_message( ).

*** Close existing log and create a new one for error handling
    save( ).

    DATA(lo_log_sap) = NEW zial_cl_log_ewm( iv_lgnum       = me->mv_lgnum
                                            iv_object      = zial_cl_log=>mc_default-log_object
                                            iv_subobject   = zial_cl_log=>mc_default-log_subobject
                                            iv_extnumber   = TEXT-000 ).

    DATA(lt_bapiret) = error_handling( iv_process   = iv_process
                                       iv_subrc     = iv_subrc
                                       io_exception = io_exception
                                       is_log_msg   = ls_log_msg ).
    lo_log_sap->log_bapiret( lt_bapiret ).

    " Enable saving of the error log
    lo_log_sap->mv_save_error = abap_true.

    " Save log for error logging
    lo_log_sap->save_log( ).

    " Error handling could be executed again
    mv_has_error  = abap_false.
    mv_save_error = abap_false.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( iv_object        = iv_object
                        iv_subobject     = iv_subobject
                        iv_extnumber     = iv_extnumber
                        it_extnumber     = it_extnumber
                        iv_callstack_lvl = iv_callstack_lvl ).

    me->mv_lgnum   = iv_lgnum.
    me->mo_sap_log = io_sap_log.

    me->build_extnumber( iv_extnumber = iv_extnumber
                         it_extnumber = it_extnumber ).

    me->build_validity( ).

    me->create_log( ).

    me->det_caller( ).
    me->log_caller( ).

  ENDMETHOD.


  METHOD log_api_message.

    io_api_message->get_messages(
      IMPORTING
        et_bapiret = DATA(lt_bapiret) ).

    log_bapiret( lt_bapiret ).

  ENDMETHOD.


  METHOD log_dm_message.

    IF io_dm_message IS BOUND.

      DATA(lt_dm_message) = io_dm_message->get_messages( ).

    ELSEIF it_dm_message IS NOT INITIAL.

      lt_dm_message = it_dm_message.

    ELSE.

      RETURN.

    ENDIF.

    LOOP AT lt_dm_message ASSIGNING FIELD-SYMBOL(<ls_dm_message>).
      me->log_sy_message( is_symsg = CORRESPONDING #( <ls_dm_message> ) ).
    ENDLOOP.

  ENDMETHOD.


  METHOD log_saplog.

    CHECK io_log IS BOUND.

    me->log_bapiret( io_log->get_prot( ) ).

  ENDMETHOD.

ENDCLASS.
