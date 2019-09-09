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
  SET SCREEN 9000. LEAVE SCREEN.
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

* Obtención de los datos introducidos en la pantalla de selección
  PERFORM selection_screen.

* Lectura de datos de la vista
  PERFORM read_data_view.

  IF <it_datos> IS ASSIGNED.
    IF <it_datos> IS INITIAL.
      MESSAGE s044 WITH ms_view-tabname.

      " Se vuelve a la pantalla principal si no hay datos.
      SET SCREEN 9000. LEAVE SCREEN.

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
