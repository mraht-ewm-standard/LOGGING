*&---------------------------------------------------------------------*
*& Report zdgl_r_application_log
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdgl_r_application_log.

INCLUDE zdgl_r_application_log_top.
INCLUDE zdgl_r_application_log_sel.
INCLUDE zdgl_r_application_log_cls.

INITIALIZATION.
  lcl_application=>on_init( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_msgid-low.
  lcl_application=>on_value_req_msgid( ).

AT SELECTION-SCREEN.
  lcl_application=>at_selection_screen( ).
