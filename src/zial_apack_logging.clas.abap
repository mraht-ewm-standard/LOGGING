CLASS zial_apack_logging DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_apack_manifest.

    METHODS constructor.

ENDCLASS.


CLASS zial_apack_logging IMPLEMENTATION.

  METHOD constructor.

    if_apack_manifest~descriptor-group_id     = 'c-a-s.de'.
    if_apack_manifest~descriptor-artifact_id  = 'ewm-logging'.
    if_apack_manifest~descriptor-version      = '17.10.2022.001-rc'.
    if_apack_manifest~descriptor-git_url      = 'https://github.com/mraht-ewm-standard/LOGGING.git' ##NO_TEXT.

    if_apack_manifest~descriptor-dependencies = VALUE #(
        ( group_id       = 'c-a-s.de'
          artifact_id    = 'ewm-exc-mgmt'
          target_package = 'ZIAL_EXC_MGMT'
          git_url        = 'https://github.com/mraht-ewm-standard/EXC_MGMT.git' ) ).

  ENDMETHOD.

ENDCLASS.
