INTERFACE zif_al30_exit_class
  PUBLIC .


  METHODS exit_before_read_data
    CHANGING
      !ct_data TYPE STANDARD TABLE .
  METHODS exit_after_save_data
    IMPORTING
      !it_data       TYPE STANDARD TABLE
      !it_data_del   TYPE STANDARD TABLE
      !iv_error_save TYPE sap_bool OPTIONAL .
  METHODS exit_before_save_data
    CHANGING
      !ct_data     TYPE STANDARD TABLE
      !ct_data_del TYPE STANDARD TABLE.
  METHODS exit_check_auth_data_read
    IMPORTING
      !is_row_data TYPE any
    EXCEPTIONS
      no_authorization .
  METHODS exit_in_process_data_read
    CHANGING
      !cs_row_data TYPE any .
  METHODS exit_verify_change_row_data
    EXPORTING
      !es_return   TYPE bapiret2
    CHANGING
      !cs_row_data TYPE any .
  METHODS exit_verify_field_data
    IMPORTING
      !iv_fieldname TYPE any
      !iv_value     TYPE any
    EXPORTING
      es_return     TYPE bapiret2 .
  METHODS exit_process_catalog_of_field
    CHANGING
      cs_fieldcat TYPE lvc_s_fcat.
ENDINTERFACE.
