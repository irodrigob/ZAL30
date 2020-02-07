"! <p class="shorttext synchronized">AL30 - Interface for exit</p>
INTERFACE zif_al30_exit_class
  PUBLIC .


  "! <p class="shorttext synchronized">Exit after read data</p>
  METHODS exit_after_read_data
    CHANGING
      !ct_data TYPE STANDARD TABLE .
  "! <p class="shorttext synchronized" >Exit before read data</p>
  METHODS exit_before_read_data
    CHANGING
      !ct_data TYPE STANDARD TABLE .
  "! <p class="shorttext synchronized" >Exit after save data</p>
  METHODS exit_after_save_data
    IMPORTING
      !it_data       TYPE STANDARD TABLE
      !it_data_del   TYPE STANDARD TABLE
      !iv_error_save TYPE sap_bool OPTIONAL .
  "! <p class="shorttext synchronized">Exit before save data</p>
  METHODS exit_before_save_data
    EXPORTING
      !ev_abort_save TYPE sap_bool
      !et_return     TYPE bapiret2_t
    CHANGING
      !ct_data       TYPE STANDARD TABLE
      !ct_data_del   TYPE STANDARD TABLE .
  "! <p class="shorttext synchronized">Exit check authorization for data read</p>
  METHODS exit_check_auth_data_read
    IMPORTING
      !is_row_data TYPE any
    EXCEPTIONS
      no_authorization .
  "! <p class="shorttext synchronized" >Exit in process for data read</p>
  METHODS exit_in_process_data_read
    CHANGING
      !cs_row_data TYPE any .

  "! <p class="shorttext synchronized" >Exit verify and change row data</p>
  "!
  "! @parameter et_return | <p class="shorttext synchronized" >Return table</p>
  METHODS exit_verify_change_row_data
    IMPORTING
      !iv_row      TYPE bapi_line OPTIONAL
    EXPORTING
      !et_return   TYPE bapiret2_t
    CHANGING
      !cs_row_data TYPE any .
  "! <p class="shorttext synchronized" >Exit verify row data</p>
  "! This exit enters both when modifying / inserting fields, and when recording data
  "! @parameter iv_row | <p class="shorttext synchronized" >Row number</p>
  "! @parameter iv_save_process | <p class="shorttext synchronized" >Enter in save process</p>
  "! @parameter is_row_data | <p class="shorttext synchronized" >Row data</p>
  "! @parameter et_return | <p class="shorttext synchronized" >Return table</p>
  METHODS exit_verify_row_data
    IMPORTING
      !iv_row          TYPE bapi_line OPTIONAL
      !is_row_data     TYPE any
      !iv_save_process TYPE sap_bool DEFAULT abap_false
    EXPORTING
      !et_return       TYPE bapiret2_t.
  "! <p class="shorttext synchronized" >Exit verify field data</p>
  METHODS exit_verify_field_data
    IMPORTING
      !iv_fieldname TYPE any
      !iv_value     TYPE any
    EXPORTING
      !es_return    TYPE bapiret2 .
  "! <p class="shorttext synchronized" >Exit process for fieldcatalog</p>
  METHODS exit_process_catalog_of_field
    CHANGING
      !cs_fieldcat TYPE lvc_s_fcat .
  "! <p class="shorttext synchronized">Exit for verify the data to be save</p>
  "! It allows the data to be recorded or deleted before performing the recording process
  "! @parameter it_data | <p class="shorttext synchronized">Data to be update or insert</p>
  "! @parameter it_data_del | <p class="shorttext synchronized">Data to be delete</p>
  "! @parameter et_return | <p class="shorttext synchronized">return</p>
  METHODS exit_verify_save_data
    IMPORTING
      it_data     TYPE STANDARD TABLE
      it_data_del TYPE STANDARD TABLE
    EXPORTING
      et_return   TYPE bapiret2_t.
    "! <p class="shorttext synchronized">Set editable mode the ALV Data</p>
    "! The ev_edit_mode parameter can be returned with two values:
    "! zif_al30_data => cv_mode_change and zif_al30_data => cv_mode_view
    "! @parameter it_data | <p class="shorttext synchronized">Data</p>
    "! @parameter ev_edit_mode | <p class="shorttext synchronized">Edit mode</p>
    METHODS exit_set_edit_mode_alv
      IMPORTING
        !it_data      TYPE STANDARD TABLE
      EXPORTING
        !ev_edit_mode TYPE cdchngind.
ENDINTERFACE.
