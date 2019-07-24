*&---------------------------------------------------------------------*
*&  Include           ZAL30_MAIN_VIEW_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  IF mo_alv IS BOUND.
    CALL METHOD mo_alv->check_changed_data( ).
  ENDIF.

  IF mv_datos_validos = abap_true.

    CASE d_okcode_9000.
      WHEN 'SAVE'.
        PERFORM grabar_datos.
      WHEN 'TRANSP'.
        PERFORM transportar_datos.
    ENDCASE.
  ELSE.
    MESSAGE s000 WITH 'Correct the problem before any action.'.

  ENDIF.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_9000 INPUT.
* Limpio los datos para dejarlo todo al estado inicial.
  PERFORM clean_data.

  SET SCREEN 0. LEAVE SCREEN.

ENDMODULE.                 " EXIT_9000  INPUT
