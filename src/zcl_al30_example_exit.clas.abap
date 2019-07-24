CLASS zcl_al30_example_exit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

*"* public components of class ZCL_AL30_EXAMPLE_EXIT
*"* do not include other source files here!!!
    INTERFACES zif_al30_exit_class .
  PROTECTED SECTION.
*"* protected components of class ZCL_AL30_EXAMPLE_EXIT
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCL_AL30_EXAMPLE_EXIT
*"* do not include other source files here!!!
ENDCLASS.



CLASS zcl_al30_example_exit IMPLEMENTATION.
  METHOD zif_al30_exit_class~exit_after_save_data.

  ENDMETHOD.

  METHOD zif_al30_exit_class~exit_before_process_data.

  ENDMETHOD.

  METHOD zif_al30_exit_class~exit_before_save_data.

  ENDMETHOD.

  METHOD zif_al30_exit_class~exit_check_auth_data_record.

  ENDMETHOD.

  METHOD zif_al30_exit_class~exit_in_process_data_record.

  ENDMETHOD.

  METHOD zif_al30_exit_class~exit_process_catalog_of_field.

  ENDMETHOD.

  METHOD zif_al30_exit_class~exit_verify_change_row_data.

  ENDMETHOD.

  METHOD zif_al30_exit_class~exit_verify_field_data.

  ENDMETHOD.

ENDCLASS.
