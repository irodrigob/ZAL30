class ZCL_AL30_DATA definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_AL30_DATA
*"* do not include other source files here!!!

  class-methods FILL_RETURN
    importing
      !iv_type type ANY
      !iv_number type ANY
      !iv_message_v1 type ANY optional
      !iv_message_v2 type ANY optional
      !iv_message_v3 type ANY optional
      !iv_message_v4 type ANY optional
    returning
      value(rs_return) type BAPIRET2 .
  class-methods F4_VIEW
    importing
      !iv_program type SYREPID
      !iv_dynpro type SYDYNNR
      !iv_dynprofield type HELP_INFO-DYNPROFLD
    exporting
      value(ev_view) type TABNAME
    raising
      ZCX_AL30 .
  CLASS-METHODS get_fcat_control_edit_view
    RETURNING
      VALUE(rt_fieldcat_control) TYPE lvc_t_fcat .
protected section.
*"* protected components of class ZCL_AL30_DATA
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_AL30_DATA
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_AL30_DATA IMPLEMENTATION.


method F4_VIEW.
  TYPES: BEGIN OF ty_values,
           tabname TYPE tabname,
           text TYPE as4text,
         END OF ty_values.

  DATA lt_values TYPE STANDARD TABLE OF ty_values.
  DATA lt_return_tab TYPE TABLE OF ddshretval.
  FIELD-SYMBOLS <ls_return_tab> TYPE ddshretval.
  FIELD-SYMBOLS <ls_values> TYPE ty_values.

  SELECT a~tabname b~ddtext INTO TABLE lt_values
         FROM zal30_t_view AS a LEFT OUTER JOIN dd02t AS b ON
              b~tabname = a~tabname
              AND b~ddlanguage = sy-langu.

  IF sy-subrc = 0.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield    = 'TABNAME'
        dynpprog    = iv_program
        dynpnr      = iv_dynpro
        dynprofield = iv_dynprofield
        value_org   = 'S'
      TABLES
        value_tab   = lt_values
        return_tab  = lt_return_tab[].

* Miro que registro se ha seleccionado y lo devuelvo al parámetro.
    READ TABLE lt_return_tab ASSIGNING <ls_return_tab> INDEX 1.
    IF sy-subrc = 0.
      READ TABLE lt_values ASSIGNING <ls_values> INDEX <ls_return_tab>-recordpos.
      IF sy-subrc = 0.
        ev_view = <ls_values>-tabname.
      ENDIF.
    ENDIF.

  ELSE.
    RAISE EXCEPTION TYPE zcx_al30
      EXPORTING
        textid = zcx_al30=>no_values_f4_view.

  ENDIF.

endmethod.


method FILL_RETURN.

  CLEAR rs_return.

  rs_return-type = iv_type.
  rs_return-id = 'ZAL30'.
  rs_return-number = iv_number.
  rs_return-message_v1 = iv_message_v1.
  rs_return-message_v2 = iv_message_v2.
  rs_return-message_v3 = iv_message_v3.
  rs_return-message_v4 = iv_message_v4.


  CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
    EXPORTING
      id         = rs_return-id
      number     = rs_return-number
      language   = SY-LANGU
      textformat = 'ASC'
      message_v1 = rs_return-message_v1
      message_v2 = rs_return-message_v2
      message_v3 = rs_return-message_v3
      message_v4 = rs_return-message_v4
    IMPORTING
      message    = rs_return-message.

endmethod.


method GET_FCAT_CONTROL_EDIT_VIEW.

  DATA ls_fieldcat TYPE LINE OF lvc_t_fcat.

* Operación del registro: 'I' -> Insertar. 'U' -> Actualizar. 'D' -> Borrar.
  ls_fieldcat-fieldname = zif_al30_data=>cv_field_updkz.
  ls_fieldcat-rollname = 'CHAR01'.
  ls_fieldcat-ref_table = 'SYST'.
  ls_fieldcat-ref_field = 'TABIX'.
  ls_fieldcat-tech = 'X'.
  APPEND ls_fieldcat TO rt_fieldcat_control.
  CLEAR ls_fieldcat.

* Línea del registro según la lectura en el diccionario
* Este campo se usa sobretodo para saber si un registro viene del diccionario, o no.
  ls_fieldcat-fieldname = zif_al30_data=>cv_field_tabix_ddic.
  ls_fieldcat-rollname = 'SYTABIX'.
  ls_fieldcat-ref_table = 'SYST'.
  ls_fieldcat-ref_field = 'TABIX'.
  ls_fieldcat-tech = 'X'.
  APPEND ls_fieldcat TO rt_fieldcat_control.
  CLEAR ls_fieldcat.

endmethod.
ENDCLASS.
