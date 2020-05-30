"! <p class="shorttext synchronized">AL30 - Interface for exit</p>
INTERFACE zif_al30_exit_class
  PUBLIC .


  "! <p class="shorttext synchronized">Exit after read data</p>
  METHODS exit_after_read_data
    CHANGING
      !ct_data TYPE STANDARD TABLE .
  "! <p class="shorttext synchronized">Exit before read data</p>
  METHODS exit_before_read_data
    CHANGING
      !ct_data TYPE STANDARD TABLE .
  "! <p class="shorttext synchronized">Exit after save data</p>
  METHODS exit_after_save_data
    IMPORTING
      !it_data       TYPE STANDARD TABLE
      !it_data_del   TYPE STANDARD TABLE
      !iv_error_save TYPE sap_bool OPTIONAL .
  "! <p class="shorttext synchronized">Exit before save data</p>
  "! @parameter iv_langu | <p class="shorttext synchronized">Language</p>
  "! @parameter ev_abort_save | <p class="shorttext synchronized">Abort save</p>
  "! @parameter et_return | <p class="shorttext synchronized">Return</p>
  "! @parameter ct_data | <p class="shorttext synchronized">Data</p>
  "! @parameter ct_data_del | <p class="shorttext synchronized">Data to delete</p>
  METHODS exit_before_save_data
    IMPORTING
      !iv_langu      TYPE sylangu DEFAULT sy-langu
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
  "! <p class="shorttext synchronized">Exit in process for data read</p>
  METHODS exit_in_process_data_read
    CHANGING
      !cs_row_data TYPE any .

  "! <p class="shorttext synchronized">Exit verify and change row data</p>
  "!
  "! @parameter et_return | <p class="shorttext synchronized">Return table</p>
  "! @parameter iv_langu | <p class="shorttext synchronized">Language</p>
  "! @parameter iv_row | <p class="shorttext synchronized">Row number</p>
  "! @parameter et_return | <p class="shorttext synchronized">Return</p>
  "! @parameter cs_row_data | <p class="shorttext synchronized">Row data</p>
  METHODS exit_verify_change_row_data
    IMPORTING
      !iv_row      TYPE bapi_line OPTIONAL
      !iv_langu    TYPE sylangu DEFAULT sy-langu
    EXPORTING
      !et_return   TYPE bapiret2_t
    CHANGING
      !cs_row_data TYPE any .
  "! <p class="shorttext synchronized">Exit verify row data</p>
  "! This exit enters both when modifying / inserting fields, and when save data
  "! @parameter iv_row | <p class="shorttext synchronized">Row number</p>
  "! @parameter iv_save_process | <p class="shorttext synchronized">Enter in save process</p>
  "! @parameter is_row_data | <p class="shorttext synchronized">Row data</p>
  "! @parameter iv_langu | <p class="shorttext synchronized">Language</p>
  "! @parameter et_return | <p class="shorttext synchronized">Return table</p>
  METHODS exit_verify_row_data
    IMPORTING
      !iv_row          TYPE bapi_line OPTIONAL
      !is_row_data     TYPE any
      !iv_save_process TYPE sap_bool DEFAULT abap_false
      !iv_langu        TYPE sylangu DEFAULT sy-langu
    EXPORTING
      !et_return       TYPE bapiret2_t.
  "! <p class="shorttext synchronized">Exit verify field data</p>
  "! @parameter iv_fieldname | <p class="shorttext synchronized">Fieldname</p>
  "! @parameter iv_value | <p class="shorttext synchronized">Value</p>
  "! @parameter iv_langu | <p class="shorttext synchronized">Language</p>
  METHODS exit_verify_field_data
    IMPORTING
      !iv_fieldname TYPE any
      !iv_value     TYPE any
      !iv_langu     TYPE sylangu DEFAULT sy-langu
    EXPORTING
      !es_return    TYPE bapiret2 .
  "! <p class="shorttext synchronized">Exit process for fieldcatalog</p>
  METHODS exit_process_catalog_of_field
    CHANGING
      !cs_fieldcat TYPE lvc_s_fcat .
  "! <p class="shorttext synchronized">Exit for verify the data to be save</p>
  "! It allows the data to be recorded or deleted before performing the recording process
  "! @parameter it_data | <p class="shorttext synchronized">Data to be update or insert</p>
  "! @parameter it_data_del | <p class="shorttext synchronized">Data to be delete</p>
  "! @parameter iv_langu | <p class="shorttext synchronized">Language</p>
  "! @parameter et_return | <p class="shorttext synchronized">return</p>
  METHODS exit_verify_save_data
    IMPORTING
      !it_data     TYPE STANDARD TABLE
      !it_data_del TYPE STANDARD TABLE
      !iv_langu    TYPE sylangu DEFAULT sy-langu
    EXPORTING
      et_return    TYPE bapiret2_t.
  "! <p class="shorttext synchronized">Set editable mode the ALV Data</p>
  "! The ev_edit_mode parameter can be returned with two values:
  "! @parameter it_data | <p class="shorttext synchronized">Data</p>
  "! @parameter ev_edit_mode | <p class="shorttext synchronized">Edit mode</p>
  METHODS exit_set_edit_mode_alv
    IMPORTING
      !it_data      TYPE STANDARD TABLE
    EXPORTING
      !ev_edit_mode TYPE cdchngind.
  "! <p class="shorttext synchronized" lang="en">Exit UI5 for change F4 search help</p>
  "! Field that in UI5 will still have search help. Only it's possible change the next fields:
  "! LABEL_FIELD_CODE and LABEL_FIELD_DESCRIPTION
  "! @parameter it_fields_text | <p class="shorttext synchronized">Descriptions of field</p>
  "! @parameter is_field_ddic | <p class="shorttext synchronized">Information of fields in DDIC</p>
  "! @parameter iv_langu | <p class="shorttext synchronized">Language</p>
  "! @parameter ev_no_include | <p class="shorttext synchronized">No include field un the catalog</p>
  "! @parameter cs_f4_catalog | <p class="shorttext synchronized">Data of catalog</p>
  METHODS exit_ui5_change_f4_catalog
    IMPORTING
      !it_fields_text TYPE zif_al30_data=>tt_fields_text_view
      !is_field_ddic  TYPE dd03p
      !iv_langu       TYPE sy-langu
    EXPORTING
      !ev_no_include  TYPE sap_bool
    CHANGING
      cs_f4_catalog   TYPE zif_al30_ui5_data=>ts_f4_catalog.
  "! <p class="shorttext synchronized">Exit UI5 for get data for foreign key</p>
  "! If data is returned, the ev_own_data parameter must be returned to "X" so that they are not overwritten
  "! @parameter iv_fieldname | <p class="shorttext synchronized">Fieldname</p>
  "! @parameter iv_checktable | <p class="shorttext synchronized">Checktable</p>
  "! @parameter iv_langu | <p class="shorttext synchronized">Language</p>
  "! @parameter ev_own_data | <p class="shorttext synchronized">Own data obtained</p>
  "! @parameter et_data | <p class="shorttext synchronized">Values</p>
  METHODS exit_ui5_get_f4_data_forgn_key
    IMPORTING
      !iv_fieldname  TYPE fieldname
      !iv_checktable TYPE tabname
      !iv_langu      TYPE sylangu
    EXPORTING
      !ev_own_data   TYPE sap_bool
      !et_data       TYPE zif_al30_ui5_data=>tt_f4_data.
  "! <p class="shorttext synchronized">Data filling from foreign key</p>
  "! If data is returned, the ev_completed_data parameter must be returned to "X" so that they are not overwritten
  "! @parameter io_foreign_key_data | <p class="shorttext synchronized">Data from foreign key</p>
  "! @parameter ev_filled_data | <p class="shorttext synchronized">The data has been filled in</p>
  "! @parameter et_data | <p class="shorttext synchronized">F4 Data</p>
  METHODS exit_ui5_fill_f4_foreign_key
    IMPORTING
      it_foreign_key_data TYPE STANDARD TABLE
      !iv_fieldname  TYPE fieldname
    EXPORTING
      ev_completed_data   TYPE sap_bool
      et_data             TYPE zif_al30_ui5_data=>tt_f4_data.
  "! <p class="shorttext synchronized">Exit UI5 for the post-filling process</p>
  "!
  "! @parameter iv_fieldname | <p class="shorttext synchronized">Fieldname</p>
  "! @parameter iv_checktable | <p class="shorttext synchronized">checktable</p>
  "! @parameter iv_domain | <p class="shorttext synchronized">Domain</p>
  "! @parameter ct_data | <p class="shorttext synchronized">Values</p>
  METHODS exit_ui5_post_fill_f4_data
    IMPORTING
      iv_fieldname  TYPE dd03p-fieldname
      iv_checktable TYPE dd03p-checktable OPTIONAL
      iv_domain     TYPE dd03p-domname OPTIONAL
    CHANGING
      ct_data       TYPE zif_al30_ui5_data=>tt_f4_data.
ENDINTERFACE.
