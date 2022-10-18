"! <p class="shorttext synchronized" lang="en">Log instance is missing</p>
CLASS zcx_log_instance_missing DEFINITION
  PUBLIC
  INHERITING FROM cx_dynamic_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_t100_dyn_msg .
    INTERFACES if_t100_message .

    METHODS constructor
      IMPORTING
        !previous LIKE previous OPTIONAL .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcx_log_instance_missing IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    CLEAR me->textid.
    if_t100_message~t100key = VALUE #( msgid = zial_cl_log=>mc_log_message_class
                                       msgno = '016' ).

  ENDMETHOD.

ENDCLASS.
