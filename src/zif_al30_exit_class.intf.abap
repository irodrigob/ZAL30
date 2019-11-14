interface ZIF_AL30_EXIT_CLASS
  public .


  methods EXIT_BEFORE_READ_DATA
    changing
      !CT_DATA type STANDARD TABLE .
  methods EXIT_AFTER_SAVE_DATA
    importing
      !IT_DATA type STANDARD TABLE
      !IT_DATA_DEL type STANDARD TABLE
      !IV_ERROR_SAVE type SAP_BOOL optional .
  methods EXIT_BEFORE_SAVE_DATA
    exporting
      !EV_ABORT_SAVE type SAP_BOOL
      !ET_RETURN type BAPIRET2_T
    changing
      !CT_DATA type STANDARD TABLE
      !CT_DATA_DEL type STANDARD TABLE .
  methods EXIT_CHECK_AUTH_DATA_READ
    importing
      !IS_ROW_DATA type ANY
    exceptions
      NO_AUTHORIZATION .
  methods EXIT_IN_PROCESS_DATA_READ
    changing
      !CS_ROW_DATA type ANY .
  methods EXIT_VERIFY_CHANGE_ROW_DATA
    exporting
      !ES_RETURN type BAPIRET2
    changing
      !CS_ROW_DATA type ANY .
  methods EXIT_VERIFY_FIELD_DATA
    importing
      !IV_FIELDNAME type ANY
      !IV_VALUE type ANY
    exporting
      !ES_RETURN type BAPIRET2 .
  methods EXIT_PROCESS_CATALOG_OF_FIELD
    changing
      !CS_FIELDCAT type LVC_S_FCAT .
endinterface.
