*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
CLASS ltc_log_msg DEFINITION DEFERRED.
CLASS zial_cl_log_msg DEFINITION LOCAL FRIENDS ltc_log_msg.
