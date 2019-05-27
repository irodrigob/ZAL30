interface ZIF_AL30_EXIT_CLASS
  public .


  class-methods EXIT_BEFORE_PROCESS_DATA
    changing
      !CT_DATA type STANDARD TABLE .
  class-methods EXIT_AFTER_SAVE_DATA
    importing
      !IT_DATA type STANDARD TABLE
      !IV_ERROR_SAVE type SAP_BOOL optional .
  class-methods EXIT_BEFORE_SAVE_DATA
    changing
      !CT_DATA type STANDARD TABLE .
  class-methods EXIT_CHECK_AUTH_DATA_RECORD
    importing
      !IS_ROW_DATA type ANY
    exceptions
      NO_AUTHORIZATION .
  class-methods EXIT_IN_PROCESS_DATA_RECORD
    changing
      !CS_ROW_DATA type ANY .
  class-methods EXIT_VERIFY_CHANGE_ROW_DATA
    exporting
      !ES_RETURN type BAPIRET2
    changing
      !CS_ROW_DATA type ANY .
  class-methods EXIT_VERIFY_FIELD_DATA
    importing
      !IV_FIELDNAME type ANY
      !IV_VALUE type ANY
      !RS_RETURN type BAPIRET2 .
endinterface.
