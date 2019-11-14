*&---------------------------------------------------------------------*
*&  Include           ZAL30_MAIN_VIEW_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  CASE mv_okcode_9000.
    WHEN 'EXECUTE'. " Se pasa a la pantalla de resultados
      mv_read_data_view = abap_true. " Indica que se quiere leer los datos
      SET SCREEN 9001. LEAVE SCREEN.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_9000 INPUT.
  SET SCREEN 0. LEAVE SCREEN.
ENDMODULE.                 " EXIT_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT_9001  INPUT
*&---------------------------------------------------------------------*
MODULE exit_9001 INPUT.
  " Si la transacción de llamada no es la misma que la original es que se accede a los
  " datos pasando por la dynpro 9002 o directamente a la 9001(si no hay campos de selección). En
  " ese caso lo que hacemos es irnos a la dynpro 9002, si hay campos de selección, o salir si no los tienes.
  IF ms_conf_screen-origin_tcode NE zif_al30_data=>cs_prog_tcode-view.
    READ TABLE mt_fields TRANSPORTING NO FIELDS WITH KEY sel_screen = abap_true.
    IF sy-subrc = 0.
      SET SCREEN 9002. LEAVE SCREEN.
    ELSE.
      SET SCREEN 0. LEAVE SCREEN.
    ENDIF.
  ELSE.
    SET SCREEN 9000. LEAVE SCREEN.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.
  IF mo_alv IS BOUND.
    CALL METHOD mo_alv->check_changed_data( ).
  ENDIF.

  IF mv_datos_validos = abap_true.

    CASE mv_okcode_9001.
      WHEN 'SAVE'.
        PERFORM grabar_datos.
      WHEN 'TRANSP'.
        PERFORM transportar_datos.
    ENDCASE.
  ELSE.
    MESSAGE s000 WITH 'Correct the problem before any action.'.

  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  F4_VIEW  INPUT
*&---------------------------------------------------------------------*
MODULE f4_view INPUT.
  DATA lo_excep TYPE REF TO zcx_al30.
  DATA ld_view TYPE tabname.

  TRY.
      CALL METHOD zcl_al30_util=>f4_view
        EXPORTING
          iv_program     = sy-repid
          iv_dynpro      = sy-dynnr
          iv_dynprofield = 'ZAL30_T_VIEW-TABNAME'.

    CATCH zcx_al30 INTO lo_excep.
      MESSAGE s018.
  ENDTRY.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  READ_VIEW  INPUT
*&---------------------------------------------------------------------*
MODULE read_view INPUT.

* Proceso de validez y lectura de la vista para las pantallas
  PERFORM process_load_view_dynpro.

* Si la transacción original que se llama no es la transacción directa del programa
* entonces se va a mirar:
* 1) Si la vista no tiene pantalla de selección definida se va directamente a la dynpro de datos
* 2) Si la vista tiene pantalla de selección definida se va la dynpro donde estará la pantalla de selección
  IF ms_conf_screen-origin_tcode NE zif_al30_data=>cs_prog_tcode-view.
    READ TABLE mt_fields TRANSPORTING NO FIELDS WITH KEY sel_screen = abap_true.
    IF sy-subrc = 0.
      SET SCREEN 9002. LEAVE SCREEN.
    ELSE.
      SET SCREEN 9001. LEAVE SCREEN.
    ENDIF.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module INIT_9000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE init_9000 OUTPUT.
  PERFORM inicializacion_prog.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module READ_DATA_VIEW OUTPUT
*&---------------------------------------------------------------------*
MODULE read_data_view OUTPUT.

* Solo se leen los datos si la variable global así lo indica.
  IF mv_read_data_view = abap_true.

    mv_read_data_view = abap_false. " Se marca a false para que no se vuelvan a releer los datos

* Obtención de los datos introducidos en la pantalla de selección
    PERFORM selection_screen.

* Lectura de datos de la vista
    PERFORM read_data_view.

* Si no hay datos se muestra un mensaje pero se continua para que muestren los datos
    IF <it_datos> IS ASSIGNED.
      IF <it_datos> IS INITIAL.
        MESSAGE s044 WITH ms_view-tabname.
      ENDIF.
    ENDIF.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module SELECTION_SCREEN OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE selection_screen OUTPUT.
  PERFORM selection_screen.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  EXIT_9002  INPUT
*&---------------------------------------------------------------------*
MODULE exit_9002 INPUT.
  SET SCREEN 0. LEAVE SCREEN.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9002  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9002 INPUT.
  CASE mv_okcode_9002.
    WHEN 'EXECUTE'. " Se pasa a la pantalla de resultados
      mv_read_data_view = abap_true. " Indica que se quiere leer los datos
      SET SCREEN 9001. LEAVE SCREEN.
  ENDCASE.
ENDMODULE.
