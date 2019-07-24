CLASS zcx_al30 DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS no_values_f4_view TYPE sotr_conc VALUE '62379100861F1ED99EFA079B976B012C' ##NO_TEXT.
    CONSTANTS view_dont_exist TYPE sotr_conc VALUE '62379100861F1ED99EFA079C0AE4412C' ##NO_TEXT.
    CONSTANTS error_save_view TYPE sotr_conc VALUE '62379100861F1ED99EFA0798A5D80129' ##NO_TEXT.
    CONSTANTS no_authorization TYPE sotr_conc VALUE '62379100861F1ED99EFA079B1E9E012C' ##NO_TEXT.
    CONSTANTS invalid_params TYPE sotr_conc VALUE '62379100861F1ED9A28C4D0084271E2B' ##NO_TEXT.
    CONSTANTS view_locked TYPE sotr_conc VALUE '62379100861F1ED9A89B0FC57B795F2C' ##NO_TEXT.
    DATA mv_message TYPE string .

    METHODS constructor
      IMPORTING
        !textid     LIKE textid OPTIONAL
        !previous   LIKE previous OPTIONAL
        !mv_message TYPE string OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_al30 IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        textid   = textid
        previous = previous.
    IF mv_message IS SUPPLIED.
      me->mv_message = mv_message .
    ENDIF.
  ENDMETHOD.
ENDCLASS.
