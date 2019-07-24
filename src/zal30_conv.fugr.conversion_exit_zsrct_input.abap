FUNCTION conversion_exit_zsrct_input .
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(INPUT)
*"  EXPORTING
*"     VALUE(OUTPUT)
*"----------------------------------------------------------------------

  DATA:
    lt_dd07v TYPE TABLE OF dd07v.

  CALL FUNCTION 'DDIF_DOMA_GET'
    EXPORTING
      name      = zif_al30_data=>cv_domain_source_text
      langu     = sy-langu
    TABLES
      dd07v_tab = lt_dd07v.

  READ TABLE lt_dd07v ASSIGNING FIELD-SYMBOL(<ls_dd07>) WITH KEY ddtext = input.
  IF sy-subrc = 0.
    output = <ls_dd07>-domvalue_l.
  ENDIF.

ENDFUNCTION.
