INTERFACE zif_al30_exit_class
  PUBLIC .


  CLASS-METHODS exit_before_process_data
    CHANGING
      !ct_data TYPE STANDARD TABLE .
  CLASS-METHODS exit_after_save_data
    IMPORTING
      !it_data       TYPE STANDARD TABLE
      !it_data_del   TYPE STANDARD TABLE
      !iv_error_save TYPE sap_bool OPTIONAL .
  CLASS-METHODS exit_before_save_data
    CHANGING
      !ct_data     TYPE STANDARD TABLE
      !ct_data_del TYPE STANDARD TABLE.
  CLASS-METHODS exit_check_auth_data_record
    IMPORTING
      !is_row_data TYPE any
    EXCEPTIONS
      no_authorization .
  CLASS-METHODS exit_in_process_data_record
    CHANGING
      !cs_row_data TYPE any .
  CLASS-METHODS exit_verify_change_row_data
    EXPORTING
      !es_return   TYPE bapiret2
    CHANGING
      !cs_row_data TYPE any .
  CLASS-METHODS exit_verify_field_data
    IMPORTING
              !iv_fieldname    TYPE any
              !iv_value        TYPE any
    RETURNING VALUE(rs_return) TYPE bapiret2 .
  CLASS-METHODS exit_process_catalog_of_field
    CHANGING
      cs_fieldcat TYPE lvc_s_fcat.
ENDINTERFACE.
