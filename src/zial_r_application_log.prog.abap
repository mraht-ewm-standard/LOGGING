*&---------------------------------------------------------------------*
*& Report ZIAL_R_APPLOG_SEARCH
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcas_r_applog_search.

INCLUDE sbaltype.

TABLES balhdr.

CONSTANTS: gc_log_context TYPE string VALUE 'ZIAL_S_LOG_CONTEXT'.

DATA gs_log_filter        TYPE bal_s_lfil.
DATA gt_log_header        TYPE balhdr_t.
DATA gt_log_handle        TYPE bal_t_logh.
DATA gt_log_handle_dis    TYPE bal_t_logh.
DATA gt_log_loaded        TYPE bal_t_logh.
DATA gs_msg_filter        TYPE bal_s_mfil.

DATA gs_display_profile   TYPE bal_s_prof.
DATA gv_table_name        TYPE fieldname.
DATA gv_field             TYPE fieldname.
DATA gv_msg_fits          TYPE abap_bool.
DATA lv_index             TYPE sy-index.


FIELD-SYMBOLS <gs_ldat> TYPE bal_s_ldat.
FIELD-SYMBOLS <gs_mhdr> TYPE bal_s_mhdr.
FIELD-SYMBOLS <gs_log_handle> TYPE balloghndl.
FIELD-SYMBOLS <gs_log_header>  TYPE balhdr.

* global data of function group SBAL
FIELD-SYMBOLS <gs_log_data>     TYPE bal_s_gdat.
FIELD-SYMBOLS <gt_messages>      TYPE ANY TABLE.
FIELD-SYMBOLS <gs_messages>      TYPE data.
FIELD-SYMBOLS <gv_msg>           TYPE data.
FIELD-SYMBOLS <gt_help>          TYPE ANY TABLE.

FIELD-SYMBOLS: <gv_var> TYPE numc1.
FIELD-SYMBOLS: <gv_con> TYPE numc1.
FIELD-SYMBOLS: <gv_src> TYPE numc1.
FIELD-SYMBOLS: <gv_par> TYPE numc1.

FIELD-SYMBOLS <s_message_check>   TYPE ANY TABLE.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_obj TYPE balhdr-object.
  PARAMETERS: p_subobj TYPE balhdr-subobject.
  PARAMETERS: p_extnum TYPE balhdr-extnumber.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECTION-SCREEN: BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(17) gv_from.
    PARAMETERS: p_datfr TYPE balhdr-aldate.
    PARAMETERS: p_timfr TYPE balhdr-altime.
  SELECTION-SCREEN: END OF LINE.

  SELECTION-SCREEN: BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(17) gv_to.
    PARAMETERS: p_datto TYPE balhdr-aldate.
    PARAMETERS: p_timto TYPE balhdr-altime.
  SELECTION-SCREEN: END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  PARAMETERS: p_user TYPE balhdr-aluser DEFAULT '*'.
  PARAMETERS: p_tcode TYPE balhdr-altcode DEFAULT '*'.
  PARAMETERS: p_prog  TYPE balhdr-alprog DEFAULT '*'.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-004.
  PARAMETERS: p_class1 TYPE balhdr-probclass RADIOBUTTON GROUP 1.
  PARAMETERS: p_class2 TYPE balhdr-probclass RADIOBUTTON GROUP 1.
  PARAMETERS: p_class3 TYPE balhdr-probclass RADIOBUTTON GROUP 1.
  PARAMETERS: p_class4 TYPE balhdr-probclass RADIOBUTTON GROUP 1 DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-005.
  SELECT-OPTIONS: s_msgid FOR sy-msgid NO INTERVALS.
  SELECT-OPTIONS: s_msgty FOR sy-msgty NO INTERVALS.
  SELECT-OPTIONS: s_msgno FOR sy-msgno.
  SELECT-OPTIONS: s_msgv FOR sy-msgv1.
SELECTION-SCREEN END OF BLOCK b5.

PARAMETERS: p_all   AS CHECKBOX DEFAULT 'X'.

DEFINE lm_check.
  IF s_&1[] IS INITIAL.
    CONTINUE.
  ELSE.
      ASSIGN COMPONENT '&1' OF STRUCTURE <gs_messages> TO <gv_msg>.
      IF <gv_msg> NOT IN s_&1.
        gv_msg_fits = abap_false.
        EXIT.
      ENDIF.
    ENDIF.
END-OF-DEFINITION.

INITIALIZATION.
  gv_from = TEXT-006.
  gv_to   = TEXT-007.
  p_datfr = sy-datum.
  p_datto = sy-datum.
  p_timto = '235959'.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_msgid-low.
  DATA gv_progname LIKE d020s-prog.
  gv_progname = sy-repid.
  CALL FUNCTION 'RS_HELP_HANDLING'
    EXPORTING
      dynpfield                 = 'S_MSGID-LOW'
      dynpname                  = sy-dynnr
      object                    = 'MI'
      suppress_selection_screen = 'X'
      progname                  = gv_progname.

START-OF-SELECTION.

  CASE abap_true.
    WHEN p_class1.
      DATA(lv_probclass) = '1'.

    WHEN p_class2.
      lv_probclass = '2'.

    WHEN p_class3.
      lv_probclass = '3'.

    WHEN p_class4.
      lv_probclass = '4'.

  ENDCASE.

  CALL FUNCTION 'BAL_FILTER_CREATE'
    EXPORTING
      i_object       = p_obj
      i_subobject    = p_subobj
      i_extnumber    = p_extnum
      i_aldate_from  = p_datfr
      i_aldate_to    = p_datto
      i_altime_from  = p_timfr
      i_altime_to    = p_timto
      i_probclass_to = lv_probclass
      i_alprog       = p_prog
      i_altcode      = p_tcode
      i_aluser       = p_user
    IMPORTING
      e_s_log_filter = gs_log_filter
    EXCEPTIONS
      OTHERS         = 0.

  CALL FUNCTION 'BAL_DB_SEARCH'
    EXPORTING
      i_s_log_filter = gs_log_filter
    IMPORTING
      e_t_log_header = gt_log_header
    EXCEPTIONS
      OTHERS         = 1.

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  CALL FUNCTION 'BAL_DB_LOAD'
    EXPORTING
      i_t_log_header = gt_log_header
    IMPORTING
      e_t_log_handle = gt_log_loaded
    EXCEPTIONS
      OTHERS         = 0.

  IF p_all = abap_true.

    ASSIGN ('(SAPLSBAL)G') TO <gs_log_data>.

    LOOP AT gt_log_loaded ASSIGNING <gs_log_handle>.
      CLEAR gv_msg_fits.

**   find this log
      READ TABLE <gs_log_data>-t_ldat ASSIGNING <gs_ldat>
          WITH KEY log_handle = <gs_log_handle>.
      CHECK sy-subrc = 0.

**   now check the messages of this log
      LOOP AT <gs_ldat>-messages-t_mhdr ASSIGNING <gs_mhdr>.
        ASSIGN COMPONENT 'CATEGORY-VAR' OF STRUCTURE <gs_mhdr> TO <gv_var>.
        ASSIGN COMPONENT 'CATEGORY-CON' OF STRUCTURE <gs_mhdr> TO <gv_con>.
        ASSIGN COMPONENT 'CATEGORY-SRC' OF STRUCTURE <gs_mhdr> TO <gv_src>.
        ASSIGN COMPONENT 'CATEGORY-PAR' OF STRUCTURE <gs_mhdr> TO <gv_par>.
        gv_table_name = 'MESSAGES-T_' && <gv_var> && <gv_con> && <gv_src> && <gv_par>.
        ASSIGN COMPONENT gv_table_name OF STRUCTURE <gs_ldat> TO <gt_messages>.
        LOOP AT <gt_messages> ASSIGNING <gs_messages>.
*         1. AND Condition:
          gv_msg_fits = abap_true.
          DO 3 TIMES.
            CASE sy-index.
              WHEN 1.
                lm_check msgid.
              WHEN 2.
                lm_check msgno.
              WHEN 3.
                lm_check msgty.
              WHEN OTHERS.
                EXIT.
            ENDCASE.
          ENDDO.
          CHECK gv_msg_fits = abap_true.

*         2. OR Condition:
          gv_msg_fits = abap_false.
          DO 4 TIMES.
            gv_field = 'MSGV' && sy-index.
            ASSIGN COMPONENT gv_field OF STRUCTURE <gs_messages> TO <gv_msg>.
            IF <gv_msg> IN s_msgv.
              gv_msg_fits = abap_true.
              EXIT.
            ENDIF.
          ENDDO.

          IF gv_msg_fits = abap_true.
            EXIT.
          ENDIF.

        ENDLOOP.

        IF gv_msg_fits = abap_true.
          EXIT.
        ENDIF.

      ENDLOOP.

      IF gv_msg_fits = abap_true.
        INSERT <gs_log_handle> INTO TABLE gt_log_handle.
      ENDIF.

    ENDLOOP.

    IF gt_log_handle IS INITIAL.
      MESSAGE i368(00) WITH TEXT-008.
      RETURN.
    ENDIF.

  ELSE.
    gs_msg_filter-msgid = s_msgid[].
    gs_msg_filter-msgno = s_msgno[].
    gs_msg_filter-msgty = s_msgty[].

  ENDIF.

  DATA ls_profile TYPE bal_s_prof.
  FIELD-SYMBOLS <ls_mess_fcat>  TYPE bal_s_fcat.
  CALL FUNCTION 'BAL_DSP_PROFILE_STANDARD_GET'
    IMPORTING
      e_s_display_profile = ls_profile.

  ls_profile-show_all = abap_true.

  APPEND VALUE #( ref_table = gc_log_context
                  ref_field = 'PROGRAM'
                  col_pos   = 3
                  coltext   = TEXT-009
                  outputlen = 30 ) TO ls_profile-mess_fcat.
  APPEND VALUE #( ref_table = gc_log_context
                  ref_field = 'INCLUDE'
                  col_pos   = 4
                  coltext   = TEXT-010
                  outputlen = 30 ) TO ls_profile-mess_fcat.
  APPEND VALUE #( ref_table = gc_log_context
                  ref_field = 'LINE'
                  col_pos   = 5
                  coltext   = TEXT-011
                  outputlen = 4 ) TO ls_profile-mess_fcat.

  READ TABLE ls_profile-mess_fcat
    ASSIGNING <ls_mess_fcat>
    WITH KEY ref_field = 'MSGID'.

  IF sy-subrc = 0.
    <ls_mess_fcat>-no_out  = abap_false.
    <ls_mess_fcat>-col_pos = 6.
  ENDIF.

  READ TABLE ls_profile-mess_fcat
    ASSIGNING <ls_mess_fcat>
    WITH KEY ref_field = 'MSGNO'.

  IF sy-subrc = 0.
    <ls_mess_fcat>-no_out  = abap_false.
    <ls_mess_fcat>-col_pos = 7.
  ENDIF.

  CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
    EXPORTING
      i_s_display_profile = ls_profile
      i_t_log_handle      = gt_log_handle
      i_s_msg_filter      = gs_msg_filter
    EXCEPTIONS
      no_authority        = 1
      OTHERS              = 2.

  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN 1.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                RAISING no_authority.
      WHEN 2.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDCASE.
  ENDIF.
