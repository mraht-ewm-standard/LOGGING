"! <p class="shorttext synchronized">Logging: Factory</p>
CLASS zial_cl_log DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zial_cl_log_sap.

  PUBLIC SECTION.
    TYPES de_message_param_id TYPE n LENGTH 10.
    TYPES de_input_component  TYPE c LENGTH 150.

    TYPES: BEGIN OF s_msgvar,
             v1 TYPE symsgv,
             v2 TYPE symsgv,
             v3 TYPE symsgv,
             v4 TYPE symsgv,
           END OF s_msgvar.

    CONSTANTS: BEGIN OF mc_msg_content_type,
                 obj TYPE numc1 VALUE 1,
                 txt TYPE numc1 VALUE 2,
               END OF mc_msg_content_type.

    CONSTANTS: BEGIN OF mc_log_process,
                 create  TYPE char4 VALUE 'CREA' ##NO_TEXT,
                 init    TYPE char4 VALUE 'INIT' ##NO_TEXT,
                 add_msg TYPE char4 VALUE 'ADD'  ##NO_TEXT,
                 save    TYPE char4 VALUE 'SAVE' ##NO_TEXT,
               END OF mc_log_process.

    CONSTANTS: BEGIN OF mc_validity_period,
                 undef TYPE zial_de_log_validity_period VALUE -1,
               END OF mc_validity_period.

    CONSTANTS: BEGIN OF mc_detail_level,
                 none    TYPE zial_de_log_detail_level VALUE 0,
                 error   TYPE zial_de_log_detail_level VALUE 1,
                 warning TYPE zial_de_log_detail_level VALUE 2,
                 success TYPE zial_de_log_detail_level VALUE 3,
                 info    TYPE zial_de_log_detail_level VALUE 4,
                 undef   TYPE zial_de_log_detail_level VALUE 9,
               END OF mc_detail_level.

    CONSTANTS: BEGIN OF mc_msgty,
                 any_error TYPE char3   VALUE 'EAX',
                 error     TYPE symsgty VALUE 'E',
                 warning   TYPE symsgty VALUE 'W',
                 success   TYPE symsgty VALUE 'S',
                 info      TYPE symsgty VALUE 'I',
               END OF mc_msgty.

    CONSTANTS: BEGIN OF mc_msgty_prio,
                 error   TYPE balprobcl VALUE 1,
                 warning TYPE balprobcl VALUE 2,
                 success TYPE balprobcl VALUE 3,
                 info    TYPE balprobcl VALUE 4,
               END OF mc_msgty_prio.

    "! Get existing or create and return new log instance
    "!
    "! @parameter ro_instance | Instance
    CLASS-METHODS get
      RETURNING VALUE(ro_instance) TYPE zial_if_log_sap=>r_log_instance.

    "! Create new log instance
    "!
    "! @parameter iv_object    | Log object
    "! @parameter iv_subobject | Log subobject
    "! @parameter iv_extnumber | External number / description for a log
    "! @parameter it_extnumber | External number elements
    "! @parameter ro_instance  | Log instance
    CLASS-METHODS create
      IMPORTING iv_object          TYPE balobj_d  OPTIONAL
                iv_subobject       TYPE balsubobj OPTIONAL
                iv_extnumber       TYPE balnrext  OPTIONAL
                it_extnumber       TYPE stringtab OPTIONAL
      RETURNING VALUE(ro_instance) TYPE zial_if_log_sap=>r_log_instance.

    CLASS-METHODS delete
      IMPORTING iv_log_handle TYPE balloghndl.

    "! Save log to application log and optionally close log instance
    "!
    "! @parameter iv_finalize | Finalize/close log? (Y/N)
    CLASS-METHODS save
      IMPORTING iv_finalize TYPE abap_bool DEFAULT abap_true.

    CLASS-METHODS to_bapiret
      IMPORTING iv_msgid          TYPE symsgid        DEFAULT sy-msgid
                iv_msgty          TYPE symsgty        DEFAULT sy-msgty
                iv_msgno          TYPE symsgno        DEFAULT sy-msgno
                iv_msgtx          TYPE clike          OPTIONAL
                iv_msgv1          TYPE clike          DEFAULT sy-msgv1
                iv_msgv2          TYPE clike          DEFAULT sy-msgv2
                iv_msgv3          TYPE clike          DEFAULT sy-msgv3
                iv_msgv4          TYPE clike          DEFAULT sy-msgv4
                io_exception      TYPE REF TO cx_root OPTIONAL
      RETURNING VALUE(rs_bapiret) TYPE bapiret2.

    CLASS-METHODS to_bapirets
      IMPORTING iv_msgid          TYPE symsgid        DEFAULT sy-msgid
                iv_msgty          TYPE symsgty        DEFAULT sy-msgty
                iv_msgno          TYPE symsgno        DEFAULT sy-msgno
                iv_msgtx          TYPE clike          OPTIONAL
                iv_msgv1          TYPE clike          DEFAULT sy-msgv1
                iv_msgv2          TYPE clike          DEFAULT sy-msgv2
                iv_msgv3          TYPE clike          DEFAULT sy-msgv3
                iv_msgv4          TYPE clike          DEFAULT sy-msgv4
                io_exception      TYPE REF TO cx_root OPTIONAL
      RETURNING VALUE(rt_bapiret) TYPE bapiret2_t.

    "! Convert bapiret structure to message string
    "!
    "! @parameter iv_msgid   | Message ID
    "! @parameter iv_msgno   | Message number
    "! @parameter iv_msgtx   | Message text
    "! @parameter iv_msgty   | Message type
    "! @parameter iv_msgv1   | Message variable 1
    "! @parameter iv_msgv2   | Message variable 2
    "! @parameter iv_msgv3   | Message variable 3
    "! @parameter iv_msgv4   | Message variable 4
    "! @parameter is_bapiret | Bapiret message
    "! @parameter rv_result  | Message as string
    CLASS-METHODS to_string
      IMPORTING iv_msgid         TYPE symsgid  DEFAULT sy-msgid
                iv_msgno         TYPE symsgno  DEFAULT sy-msgno
                iv_msgtx         TYPE bapi_msg OPTIONAL
                iv_msgty         TYPE symsgty  DEFAULT sy-msgty
                iv_msgv1         TYPE symsgv   DEFAULT sy-msgv1
                iv_msgv2         TYPE symsgv   DEFAULT sy-msgv2
                iv_msgv3         TYPE symsgv   DEFAULT sy-msgv3
                iv_msgv4         TYPE symsgv   DEFAULT sy-msgv4
                is_bapiret       TYPE bapiret2 OPTIONAL
      RETURNING VALUE(rv_result) TYPE string.

    CLASS-METHODS to_symsg
      IMPORTING iv_msgid        TYPE symsgid  DEFAULT sy-msgid
                iv_msgty        TYPE symsgty  DEFAULT sy-msgty
                iv_msgno        TYPE symsgno  DEFAULT sy-msgno
                iv_msgtx        TYPE bapi_msg OPTIONAL
                iv_msgv1        TYPE symsgv   DEFAULT sy-msgv1
                iv_msgv2        TYPE symsgv   DEFAULT sy-msgv2
                iv_msgv3        TYPE symsgv   DEFAULT sy-msgv3
                iv_msgv4        TYPE symsgv   DEFAULT sy-msgv4
                is_bapiret      TYPE bapiret2 OPTIONAL
      RETURNING VALUE(rs_symsg) TYPE symsg.

    CLASS-METHODS show_msgtx
      IMPORTING iv_msgty TYPE msgty DEFAULT 'S'
                iv_msgtx TYPE msgtx
                iv_msgv1 TYPE msgv1 OPTIONAL
                iv_msgv2 TYPE msgv2 OPTIONAL
                iv_msgv3 TYPE msgv3 OPTIONAL
                iv_msgv4 TYPE msgv4 OPTIONAL
                iv_msgdl TYPE msgty OPTIONAL.

    "! Convert data dynamically into message details
    "! <p><strong>Note:</strong><br/>If you supply an element-wise table you'll have to provide
    "! at least one fieldname! You can use the fieldname table to define which attributes of a
    "! structure or table should to be logged.</p>
    "!
    "! @parameter it_fnam     | Fieldnames
    "! @parameter is_msgde    | Message details
    "! @parameter is_data     | Dynamic data as a structure
    "! @parameter it_data     | Dynamic data as a table
    "! @parameter iv_is_range | <p></p>
    "! @parameter rt_msgde    | Message details
    CLASS-METHODS to_msgde
      IMPORTING it_fnam         TYPE string_table     OPTIONAL
                is_msgde        TYPE rsra_s_parameter OPTIONAL
                is_data         TYPE data             OPTIONAL
                it_data         TYPE ANY TABLE        OPTIONAL
                iv_is_range     TYPE abap_bool        DEFAULT abap_false
      RETURNING VALUE(rt_msgde) TYPE rsra_t_alert_definition.

    "! Determine component names on base of input data
    "!
    "! @parameter it_input_data | Dynamic input data as table
    "! @parameter rv_components | Component names from input data
    CLASS-METHODS get_components_from_msgde
      IMPORTING it_input_data        TYPE rsra_t_alert_definition
      RETURNING VALUE(rv_components) TYPE de_input_component.

    "! Display messages in popup
    "!
    "! @parameter it_bapiret | List of bapiret2 messages
    CLASS-METHODS display_as_popup
      IMPORTING it_bapiret TYPE bapirettab.

    CLASS-METHODS free.

    "! <p class="shorttext synchronized">Check results for error</p>
    "!
    "! @parameter iv_severity | <p class="shorttext synchronized">Severity</p>
    "! @parameter it_bapiret  | <p class="shorttext synchronized">Protocol</p>
    "! @parameter rv_result   | <p class="shorttext synchronized">Error? (Y/N)</p>
    CLASS-METHODS has_error
      IMPORTING iv_severity      TYPE bapi_mtype OPTIONAL
                it_bapiret       TYPE bapirettab OPTIONAL
      PREFERRED PARAMETER it_bapiret
      RETURNING VALUE(rv_result) TYPE abap_bool.

    CLASS-METHODS get_last_error
      IMPORTING it_bapiret         TYPE bapiret2_t
      RETURNING VALUE(rs_bapiret2) TYPE bapiret2.

  PROTECTED SECTION.
    CONSTANTS mc_msg_ident          TYPE c LENGTH 9 VALUE 'MSG_IDENT' ##NO_TEXT.
    CONSTANTS mc_log_context_struct TYPE baltabname VALUE 'ZIAL_S_LOG_CONTEXT' ##NO_TEXT.

    CLASS-DATA mo_gui_docking_container TYPE REF TO cl_gui_docking_container.
    CLASS-DATA mo_gui_alv_grid          TYPE REF TO cl_gui_alv_grid.
    CLASS-DATA mv_sel_msg_param_id      TYPE de_message_param_id.
    CLASS-DATA mv_log_part_id           TYPE i.
    CLASS-DATA ms_symsg                 TYPE symsg.

    CLASS-DATA mo_instance              TYPE zial_if_log_sap=>r_log_instance.

    CLASS-METHODS harmonize_msg
      IMPORTING iv_msgid   TYPE symsgid
                iv_msgno   TYPE symsgno
                iv_msgty   TYPE symsgty
                iv_msgtx   TYPE bapi_msg OPTIONAL
                iv_msgv1   TYPE symsgv
                iv_msgv2   TYPE symsgv
                iv_msgv3   TYPE symsgv
                iv_msgv4   TYPE symsgv
                is_bapiret TYPE bapiret2 OPTIONAL
      EXPORTING ev_msgtx   TYPE bapi_msg
                es_symsg   TYPE symsg.

    CLASS-METHODS to_msgde_add_by_components
      IMPORTING io_struct_descr TYPE REF TO cl_abap_structdescr
                is_data         TYPE any
                it_fnam         TYPE string_table
      RETURNING VALUE(rt_msgde) TYPE rsra_t_alert_definition.

    CLASS-METHODS backup_sy_msg.
    CLASS-METHODS recover_sy_msg.

    CLASS-METHODS get_next_log_part_id
      RETURNING VALUE(rv_log_part_id) TYPE i.

ENDCLASS.


CLASS zial_cl_log IMPLEMENTATION.

  METHOD backup_sy_msg.

    ms_symsg = VALUE #( msgid = sy-msgid
                        msgno = sy-msgno
                        msgty = sy-msgty
                        msgv1 = sy-msgv1
                        msgv2 = sy-msgv2
                        msgv3 = sy-msgv3
                        msgv4 = sy-msgv4 ).

  ENDMETHOD.


  METHOD create.

    backup_sy_msg( ).

    mo_instance = NEW #( iv_object    = iv_object
                         iv_subobject = iv_subobject
                         iv_extnumber = iv_extnumber
                         it_extnumber = it_extnumber ).
    zial_cl_log_stack=>push( mo_instance ).

    recover_sy_msg( ).

    ro_instance = mo_instance.

  ENDMETHOD.


  METHOD delete.

    CHECK iv_log_handle IS NOT INITIAL.

    CALL FUNCTION 'BAL_LOG_DELETE'
      EXPORTING  i_log_handle = iv_log_handle
      EXCEPTIONS OTHERS       = 0.

    zial_cl_log_stack=>remove( iv_log_handle ).

  ENDMETHOD.


  METHOD display_as_popup.

    DATA(lt_bapiret) = it_bapiret.
    CALL FUNCTION 'RSCRMBW_DISPLAY_BAPIRET2'
      TABLES it_return = lt_bapiret.

  ENDMETHOD.


  METHOD free.

    FREE mo_instance.

    zial_cl_log_stack=>free( ).

  ENDMETHOD.


  METHOD get.

    mo_instance = zial_cl_log_stack=>pop( )-instance.
    IF mo_instance IS INITIAL.
      mo_instance = create( iv_extnumber = CONV #( TEXT-001 ) ).
      mo_instance->mv_is_fallback_log = abap_true.
    ENDIF.

    ro_instance = mo_instance.

  ENDMETHOD.


  METHOD get_components_from_msgde.

    DATA(lt_input_data) = it_input_data.
    DELETE ADJACENT DUPLICATES FROM lt_input_data COMPARING fnam.

    LOOP AT lt_input_data ASSIGNING FIELD-SYMBOL(<ls_input_data>).

      CASE sy-tabix.
        WHEN 1.
          rv_components = <ls_input_data>-fnam.

        WHEN OTHERS.
          rv_components = |{ rv_components }, { <ls_input_data>-fnam }|.

      ENDCASE.

    ENDLOOP.

    IF rv_components IS INITIAL.
      rv_components = 'N/A'.
    ENDIF.

  ENDMETHOD.


  METHOD get_next_log_part_id.

    rv_log_part_id = mv_log_part_id + 1.

  ENDMETHOD.


  METHOD get_last_error.

    LOOP AT it_bapiret INTO rs_bapiret2 WHERE type CA mc_msgty-any_error.
      EXIT.
    ENDLOOP.

  ENDMETHOD.


  METHOD has_error.

    IF iv_severity CA mc_msgty-any_error.
      rv_result = abap_true.
      RETURN.
    ENDIF.

    LOOP AT it_bapiret TRANSPORTING NO FIELDS WHERE type CA mc_msgty-any_error.
      rv_result = abap_true.
      EXIT.
    ENDLOOP.

  ENDMETHOD.


  METHOD recover_sy_msg.

    CHECK mo_instance->has_error( ) EQ abap_false.

    sy-msgid = ms_symsg-msgid.
    sy-msgno = ms_symsg-msgno.
    sy-msgty = ms_symsg-msgty.
    sy-msgv1 = ms_symsg-msgv1.
    sy-msgv2 = ms_symsg-msgv2.
    sy-msgv3 = ms_symsg-msgv3.
    sy-msgv4 = ms_symsg-msgv4.

  ENDMETHOD.


  METHOD save.

    CHECK zial_cl_log_stack=>is_empty( ) EQ abap_false.

    mo_instance = get( ).
    mo_instance->save( iv_finalize ).

  ENDMETHOD.


  METHOD show_msgtx.

    DATA(lv_msgdl) = iv_msgdl.
    IF lv_msgdl IS INITIAL.
      lv_msgdl = iv_msgty.
    ENDIF.

    DATA(lv_msgtx) = iv_msgtx.
    IF iv_msgv1 IS NOT INITIAL.
      REPLACE '&1' IN lv_msgtx WITH iv_msgv1.
    ENDIF.
    IF iv_msgv2 IS NOT INITIAL.
      REPLACE '&2' IN lv_msgtx WITH iv_msgv2.
    ENDIF.
    IF iv_msgv3 IS NOT INITIAL.
      REPLACE '&3' IN lv_msgtx WITH iv_msgv3.
    ENDIF.
    IF iv_msgv4 IS NOT INITIAL.
      REPLACE '&4' IN lv_msgtx WITH iv_msgv4.
    ENDIF.

    MESSAGE lv_msgtx TYPE iv_msgty DISPLAY LIKE lv_msgdl.

  ENDMETHOD.


  METHOD to_msgde.

    DATA lo_struct_descr TYPE REF TO cl_abap_structdescr.

    IF is_msgde IS NOT INITIAL.

      APPEND is_msgde TO rt_msgde.

    ELSEIF is_data IS NOT INITIAL.

      lo_struct_descr ?= cl_abap_typedescr=>describe_by_data( is_data ).

      rt_msgde = to_msgde_add_by_components( io_struct_descr = lo_struct_descr
                                             is_data         = is_data
                                             it_fnam         = it_fnam ).

    ELSEIF it_data IS NOT INITIAL.

      DATA(lo_table_descr) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( it_data ) ).
      DATA(lo_data_descr) = lo_table_descr->get_table_line_type( ).

      IF     it_fnam IS NOT INITIAL
         AND (    lo_data_descr->kind EQ cl_abap_typedescr=>kind_elem
               OR iv_is_range         EQ abap_true ).
        ASSIGN it_fnam[ 1 ] TO FIELD-SYMBOL(<lv_fnam>).
      ENDIF.

      CASE iv_is_range.
        WHEN abap_true.
          DATA(lt_r_range) = CONV rseloption( it_data ).
          IF <lv_fnam> IS ASSIGNED.
            rt_msgde = VALUE #( FOR <s_r_range> IN lt_r_range
                                ( fnam = <lv_fnam>
                                  sign = <s_r_range>-sign
                                  opt  = <s_r_range>-option
                                  low  = <s_r_range>-low
                                  high = <s_r_range>-high ) ).
          ELSE.
            rt_msgde = VALUE #( FOR <s_r_range> IN lt_r_range
                                ( sign = <s_r_range>-sign
                                  opt  = <s_r_range>-option
                                  low  = <s_r_range>-low
                                  high = <s_r_range>-high ) ).
          ENDIF.

        WHEN abap_false.
          LOOP AT it_data ASSIGNING FIELD-SYMBOL(<lx_data>).

            CASE lo_data_descr->kind.
              WHEN cl_abap_typedescr=>kind_elem.
                IF <lv_fnam> IS ASSIGNED.
                  INSERT VALUE #( fnam = <lv_fnam>
                                  low  = <lx_data> ) INTO TABLE rt_msgde.
                ELSE.
                  INSERT VALUE #( low = <lx_data> ) INTO TABLE rt_msgde.
                ENDIF.

              WHEN cl_abap_typedescr=>kind_struct.
                lo_struct_descr ?= lo_data_descr.
                INSERT LINES OF to_msgde_add_by_components( io_struct_descr = lo_struct_descr
                                                            is_data         = <lx_data>
                                                            it_fnam         = it_fnam ) INTO TABLE rt_msgde.

              WHEN OTHERS.
                EXIT.

            ENDCASE.

          ENDLOOP.

      ENDCASE.

    ELSE.

      RETURN.

    ENDIF.

  ENDMETHOD.


  METHOD to_msgde_add_by_components.

    LOOP AT io_struct_descr->components ASSIGNING FIELD-SYMBOL(<ls_component>).

      IF         it_fnam IS NOT INITIAL
         AND NOT line_exists( it_fnam[ table_line = <ls_component>-name ] ).
        CONTINUE.
      ENDIF.

      ASSIGN COMPONENT <ls_component>-name OF STRUCTURE is_data TO FIELD-SYMBOL(<lv_value>).
      IF <lv_value> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      APPEND VALUE #( fnam = <ls_component>-name
                      low  = <lv_value> ) TO rt_msgde.

    ENDLOOP.

  ENDMETHOD.


  METHOD harmonize_msg.

    zial_cl_log_msg=>harmonize_msg( EXPORTING iv_msgid   = iv_msgid
                                              iv_msgno   = iv_msgno
                                              iv_msgty   = iv_msgty
                                              iv_msgtx   = iv_msgtx
                                              iv_msgv1   = iv_msgv1
                                              iv_msgv2   = iv_msgv2
                                              iv_msgv3   = iv_msgv3
                                              iv_msgv4   = iv_msgv4
                                              is_bapiret = is_bapiret
                                    IMPORTING ev_msgtx   = ev_msgtx
                                              es_symsg   = es_symsg ).

  ENDMETHOD.


  METHOD to_bapiret.

    rs_bapiret = zial_cl_log_msg=>to_bapiret( iv_msgid     = iv_msgid
                                              iv_msgty     = iv_msgty
                                              iv_msgno     = iv_msgno
                                              iv_msgtx     = iv_msgtx
                                              iv_msgv1     = iv_msgv1
                                              iv_msgv2     = iv_msgv2
                                              iv_msgv3     = iv_msgv3
                                              iv_msgv4     = iv_msgv4
                                              io_exception = io_exception ).

  ENDMETHOD.


  METHOD to_bapirets.

    rt_bapiret = zial_cl_log_msg=>to_bapirets( iv_msgid     = iv_msgid
                                               iv_msgty     = iv_msgty
                                               iv_msgno     = iv_msgno
                                               iv_msgtx     = iv_msgtx
                                               iv_msgv1     = iv_msgv1
                                               iv_msgv2     = iv_msgv2
                                               iv_msgv3     = iv_msgv3
                                               iv_msgv4     = iv_msgv4
                                               io_exception = io_exception ).

  ENDMETHOD.


  METHOD to_string.

    rv_result = zial_cl_log_msg=>to_string( iv_msgid   = iv_msgid
                                            iv_msgno   = iv_msgno
                                            iv_msgtx   = iv_msgtx
                                            iv_msgty   = iv_msgty
                                            iv_msgv1   = iv_msgv1
                                            iv_msgv2   = iv_msgv2
                                            iv_msgv3   = iv_msgv3
                                            iv_msgv4   = iv_msgv4
                                            is_bapiret = is_bapiret ).

  ENDMETHOD.


  METHOD to_symsg.

    zial_cl_log_msg=>harmonize_msg( EXPORTING iv_msgid   = iv_msgid
                                              iv_msgno   = iv_msgno
                                              iv_msgty   = iv_msgty
                                              iv_msgtx   = iv_msgtx
                                              iv_msgv1   = iv_msgv1
                                              iv_msgv2   = iv_msgv2
                                              iv_msgv3   = iv_msgv3
                                              iv_msgv4   = iv_msgv4
                                              is_bapiret = is_bapiret
                                    IMPORTING es_symsg   = rs_symsg ).

  ENDMETHOD.

ENDCLASS.
