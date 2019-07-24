class ZCX_AL30 definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  constants NO_VALUES_F4_VIEW type SOTR_CONC value '62379100861F1ED99EFA079B976B012C' ##NO_TEXT.
  constants VIEW_DONT_EXIST type SOTR_CONC value '62379100861F1ED99EFA079C0AE4412C' ##NO_TEXT.
  constants ERROR_SAVE_VIEW type SOTR_CONC value '62379100861F1ED99EFA0798A5D80129' ##NO_TEXT.
  constants NO_AUTHORIZATION type SOTR_CONC value '62379100861F1ED99EFA079B1E9E012C' ##NO_TEXT.
  constants INVALID_PARAMS type SOTR_CONC value '62379100861F1ED9A28C4D0084271E2B' ##NO_TEXT.
  constants VIEW_LOCKED type SOTR_CONC value '62379100861F1ED9A89B0FC57B795F2C' ##NO_TEXT.
  data MV_MESSAGE type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !MV_MESSAGE type STRING optional .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_AL30 IMPLEMENTATION.


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
