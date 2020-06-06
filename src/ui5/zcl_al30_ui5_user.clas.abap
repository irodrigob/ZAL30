CLASS zcl_al30_ui5_user DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_user_configuration,
             date_format        TYPE string,
             decimal_separator  TYPE c LENGTH 1,
             thousand_separator TYPE c LENGTH 1,
           END OF ts_user_configuration.

    "! <p class="shorttext synchronized">Get user configuration</p>
    "! Return the configuration for the date forma, decimal and thousand separator
    "! @parameter iv_user | <p class="shorttext synchronized">Username</p>
    "! @parameter es_configuration | <p class="shorttext synchronized">Configuration</p>
    METHODS get_user_configuration
      IMPORTING
        !iv_user          TYPE string
      EXPORTING
        !es_configuration TYPE zcl_al30_ui5_user=>ts_user_configuration.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_al30_ui5_user IMPLEMENTATION.
  METHOD get_user_configuration.

    CLEAR: es_configuration.

    " En este caso el usuario pasado es el usuario SAP
    SELECT SINGLE dcpfm, datfm INTO @DATA(ls_user_masterdata)
    FROM usr01
    WHERE bname EQ @iv_user.
    IF sy-subrc = 0.
      " Formato de fecha
      es_configuration-date_format = ls_user_masterdata-datfm.

      " Separadores decimales y miles.
      CASE ls_user_masterdata-dcpfm.
        WHEN 'X'.
          es_configuration-decimal_separator = '.'.
          es_configuration-thousand_separator = ','.
        WHEN OTHERS.
          es_configuration-decimal_separator = ','.
          es_configuration-thousand_separator = '.'.
      ENDCASE.

    ELSE.
      " Si no existe se pone una configuración por defecto
      es_configuration-date_format = zif_al30_ui5_data=>cs_user-default_configuration-date_format.
      es_configuration-decimal_separator = zif_al30_ui5_data=>cs_user-default_configuration-decimal_separator.
      es_configuration-thousand_separator = zif_al30_ui5_data=>cs_user-default_configuration-thousand_separator.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
