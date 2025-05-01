CLASS zial_cl_log_msg DEFINITION
  PUBLIC ABSTRACT FINAL
  GLOBAL FRIENDS zial_cl_log.

  PUBLIC SECTION.
    CONSTANTS: BEGIN OF mc_default,
                 msgid TYPE symsgid VALUE 'SY',  " 0Q
                 msgno TYPE symsgno VALUE '499', " 000
                 msgty TYPE symsgty VALUE 'I',
               END OF mc_default.

    "! Convert bapiret structure to message string
    "!
    "! @parameter iv_msgid   | Message ID
    "! @parameter iv_msgty   | Message type
    "! @parameter iv_msgno   | Message number
    "! @parameter iv_msgv1   | Message variable 1
    "! @parameter iv_msgv2   | Message variable 2
    "! @parameter iv_msgv3   | Message variable 3
    "! @parameter iv_msgv4   | Message variable 4
    "! @parameter is_bapiret | Bapiret message
    "! @parameter rv_result  | Message as string
    CLASS-METHODS to_string
      IMPORTING iv_msgid         TYPE symsgid  DEFAULT sy-msgid
                iv_msgty         TYPE symsgty  DEFAULT sy-msgty
                iv_msgno         TYPE symsgno  DEFAULT sy-msgno
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

  PROTECTED SECTION.
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

ENDCLASS.


CLASS zial_cl_log_msg IMPLEMENTATION.

  METHOD to_string.

    DATA(ls_symsg) = to_symsg( iv_msgid   = iv_msgid
                               iv_msgno   = iv_msgno
                               iv_msgty   = iv_msgty
                               iv_msgv1   = iv_msgv1
                               iv_msgv2   = iv_msgv2
                               iv_msgv3   = iv_msgv3
                               iv_msgv4   = iv_msgv4
                               is_bapiret = is_bapiret ).
    IF ls_symsg IS NOT INITIAL.
      MESSAGE ID ls_symsg-msgid TYPE ls_symsg-msgty NUMBER ls_symsg-msgno
              WITH ls_symsg-msgv1 ls_symsg-msgv2 ls_symsg-msgv3 ls_symsg-msgv4
              INTO rv_result.
    ENDIF.

  ENDMETHOD.


  METHOD to_symsg.

    harmonize_msg( EXPORTING iv_msgid   = iv_msgid
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


    METHOD harmonize_msg.

    CLEAR: ev_msgtx,
           es_symsg.

    IF is_bapiret IS NOT INITIAL.

      DATA(lv_msgid) = is_bapiret-id.
      DATA(lv_msgno) = is_bapiret-number.
      DATA(lv_msgty) = is_bapiret-type.
      DATA(lv_msgtx) = is_bapiret-message.
      DATA(lv_msgv1) = is_bapiret-message_v1.
      DATA(lv_msgv2) = is_bapiret-message_v2.
      DATA(lv_msgv3) = is_bapiret-message_v3.
      DATA(lv_msgv4) = is_bapiret-message_v4.

    ELSE.

      lv_msgid = iv_msgid.
      lv_msgno = iv_msgno.
      lv_msgty = iv_msgty.
      lv_msgtx = iv_msgtx.
      lv_msgv1 = iv_msgv1.
      lv_msgv2 = iv_msgv2.
      lv_msgv3 = iv_msgv3.
      lv_msgv4 = iv_msgv4.

    ENDIF.

    IF    lv_msgid IS INITIAL
       OR lv_msgno IS INITIAL.
      lv_msgid = mc_default-msgid.
      lv_msgno = mc_default-msgno.
    ENDIF.

    IF lv_msgty IS INITIAL.
      lv_msgty = mc_default-msgty.
    ENDIF.

    WHILE lv_msgtx CS '&'.

      DATA(lv_index) = sy-index.
      IF lv_index GT 8.
        EXIT.
      ENDIF.

      DATA(lv_search_str) = COND #( WHEN lv_index LT 5 THEN |&{ lv_index }|
                                    WHEN lv_index GT 4 THEN |&| ).
      DATA(lv_msgvar) = SWITCH #( lv_index
                                  WHEN 1 OR 5 THEN lv_msgv1
                                  WHEN 2 OR 6 THEN lv_msgv2
                                  WHEN 3 OR 7 THEN lv_msgv3
                                  WHEN 4 OR 8 THEN lv_msgv4 ).
      REPLACE FIRST OCCURRENCE OF lv_search_str IN lv_msgtx WITH lv_msgvar.

    ENDWHILE.

    MESSAGE ID lv_msgid TYPE lv_msgty NUMBER lv_msgno
            WITH lv_msgv1 lv_msgv2 lv_msgv3 lv_msgv4 INTO ev_msgtx.
    IF ev_msgtx IS INITIAL.
      ev_msgtx = lv_msgtx.
    ENDIF.

    es_symsg = VALUE #( msgid = lv_msgid
                        msgno = lv_msgno
                        msgty = lv_msgty
                        msgv1 = lv_msgv1
                        msgv2 = lv_msgv2
                        msgv3 = lv_msgv3
                        msgv4 = lv_msgv4 ).

  ENDMETHOD.


  METHOD to_bapiret.

    IF     iv_msgtx IS SUPPLIED
       AND iv_msgtx IS NOT INITIAL.

      harmonize_msg( EXPORTING iv_msgid = mc_default-msgid
                               iv_msgno = mc_default-msgno
                               iv_msgty = iv_msgty
                               iv_msgtx = CONV #( iv_msgtx )
                               iv_msgv1 = CONV #( iv_msgv1 )
                               iv_msgv2 = CONV #( iv_msgv2 )
                               iv_msgv3 = CONV #( iv_msgv3 )
                               iv_msgv4 = CONV #( iv_msgv4 )
                     IMPORTING ev_msgtx = DATA(lv_msgtx)
                               es_symsg = DATA(ls_symsg) ).

      rs_bapiret = VALUE #( id         = ls_symsg-msgid
                            number     = ls_symsg-msgno
                            type       = ls_symsg-msgty
                            message    = lv_msgtx
                            message_v1 = ls_symsg-msgv1
                            message_v2 = ls_symsg-msgv2
                            message_v3 = ls_symsg-msgv3
                            message_v4 = ls_symsg-msgv4 ).

    ELSEIF io_exception IS BOUND.

      CASE TYPE OF io_exception.
        WHEN TYPE zcx_if_check_class.
          rs_bapiret = CAST zcx_if_check_class( io_exception )->get_message( ).

        WHEN TYPE cx_root.
          rs_bapiret = to_bapiret( iv_msgtx = CONV #( io_exception->get_text( ) ) ).

        WHEN OTHERS.
          RETURN.

      ENDCASE.

    ELSEIF iv_msgty IS NOT INITIAL
       AND iv_msgid IS NOT INITIAL
       AND iv_msgno CN ' _'.

      rs_bapiret = VALUE #( type       = iv_msgty
                            id         = iv_msgid
                            number     = iv_msgno
                            message_v1 = CONV #( iv_msgv1 )
                            message_v2 = CONV #( iv_msgv2 )
                            message_v3 = CONV #( iv_msgv3 )
                            message_v4 = CONV #( iv_msgv4 ) ).
      MESSAGE ID iv_msgid TYPE iv_msgty NUMBER iv_msgno
              WITH iv_msgv1 iv_msgv2 iv_msgv3 iv_msgv4 INTO rs_bapiret-message.

    ELSE.

      RETURN.

    ENDIF.

    CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET_STABLE'
      IMPORTING  own_logical_system = rs_bapiret-system
      EXCEPTIONS OTHERS             = 0.

  ENDMETHOD.


  METHOD to_bapirets.

    IF io_exception IS BOUND.

      CASE TYPE OF io_exception.
        WHEN TYPE zcx_if_check_class.
          rt_bapiret = CAST zcx_if_check_class( io_exception )->get_messages( ).

        WHEN TYPE cx_root.
          rt_bapiret = VALUE #( ( to_bapiret( iv_msgtx = CONV #( io_exception->get_text( ) ) ) ) ).

        WHEN OTHERS.
          RETURN.

      ENDCASE.

    ELSE.

      rt_bapiret = VALUE #( ( to_bapiret( iv_msgid = iv_msgid
                                          iv_msgty = iv_msgty
                                          iv_msgno = iv_msgno
                                          iv_msgtx = iv_msgtx
                                          iv_msgv1 = iv_msgv1
                                          iv_msgv2 = iv_msgv2
                                          iv_msgv3 = iv_msgv3
                                          iv_msgv4 = iv_msgv4 ) ) ).

    ENDIF.

    LOOP AT rt_bapiret ASSIGNING FIELD-SYMBOL(<ls_bapiret>) WHERE system IS INITIAL.
      CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET_STABLE'
        IMPORTING  own_logical_system = <ls_bapiret>-system
        EXCEPTIONS OTHERS             = 0.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
