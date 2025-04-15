CLASS zial_apack_logging_core DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_apack_manifest.

    METHODS constructor.

ENDCLASS.


CLASS zial_apack_logging_core IMPLEMENTATION.

  METHOD constructor.

    if_apack_manifest~descriptor-group_id     = 'c-a-s.de'.
    if_apack_manifest~descriptor-artifact_id  = 'logging-core'.
    if_apack_manifest~descriptor-version      = '01.04.2025.001-rc'.
    if_apack_manifest~descriptor-git_url      = 'https://github.com/mraht-ewm-standard/LOGGING_CORE.git' ##NO_TEXT.

    if_apack_manifest~descriptor-dependencies = VALUE #(
        group_id = 'c-a-s.de'
        ( artifact_id = 'aunit'
          git_url     = 'https://github.com/mraht-ewm-standard/AUNIT.git' )
        ( artifact_id = 'logging-msg'
          git_url     = 'https://github.com/mraht-ewm-standard/LOGGING_MSG.git' )
        ( artifact_id = 'exc-mgmt'
          git_url     = 'https://github.com/mraht-ewm-standard/EXC_MGMT.git' ) ).

  ENDMETHOD.

ENDCLASS.
