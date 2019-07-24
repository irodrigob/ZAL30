*----------------------------------------------------------------------*
***INCLUDE ZAL30_MAIN_CONF_I01 .
*----------------------------------------------------------------------*
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
*&---------------------------------------------------------------------*
*&      Module  CHANGE_VIEW  INPUT
*&---------------------------------------------------------------------*
MODULE change_view INPUT.

  IF mv_datos_validos = abap_true.

* Si estoy creando la vista verifico que sea correcta
    IF mv_mode = zif_al30_data=>cv_mode_insert.
      PERFORM check_view_insert.
    ELSE.
      PERFORM change_view.
    ENDIF.

  ENDIF.

ENDMODULE.                 " CHANGE_VIEW  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  CASE d_okcode_9000.
    WHEN 'CREA'. " Create view
      PERFORM create_view.

    WHEN 'DELE'. " Delete view
      PERFORM delete_view.

    WHEN 'DICH'. " Display <-> Change

      PERFORM display_change.

    WHEN 'OTHER'. " Other view

      PERFORM other_view.

    WHEN 'SAVE'. " Save view

      PERFORM save_view.

    WHEN 'EHEAD'.

      PERFORM expand_fields.

    WHEN 'TRANS'.

      PERFORM transport_entries USING 'K'. " Las entradas se insertan

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9000  INPUT

*&SPWIZARD: INPUT MODULE FOR TS 'TAB_FIELDS'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GETS ACTIVE TAB
MODULE tab_fields_active_tab_get INPUT.

*  D_OKCODE_9000 = SY-UCOMM.
  CASE d_okcode_9000.
    WHEN c_tab_fields-tab1.
      g_tab_fields-pressed_tab = c_tab_fields-tab1.
    WHEN c_tab_fields-tab2.
      g_tab_fields-pressed_tab = c_tab_fields-tab2.
    WHEN c_tab_fields-tab3.
      g_tab_fields-pressed_tab = c_tab_fields-tab3.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.

ENDMODULE.                    "TAB_FIELDS_ACTIVE_TAB_GET INPUT
*&---------------------------------------------------------------------*
*&      Module  VALID_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE valid_data INPUT.

* Por defectos los datos son validos.
  mv_datos_validos = abap_true.

* Lanzo la verficiación para los dos ALV pero en ningún caso recupero
* si hay bien. No lo recupero porque me da falsos positivos. Cuando
* pongo valores y doy enter me da el posible error, pero si pulso cualquier botón
* el check_changed_data me dice que es correcto cuando no es verdad.
* Este mal funcionamiento posiblemente sea por el tipo de edición que se valida cuando
* da Enter.
* Por eso, la variables de datos correctos se gestionan dentro de la propia clase de validación.
  IF mo_alv_gen IS BOUND.
    CALL METHOD mo_alv_gen->check_changed_data( ).
  ENDIF.

* Verificos datos de textos
  IF mo_alv_text IS BOUND.
    CALL METHOD mo_alv_text->check_changed_data( ).
  ENDIF.

  IF mv_datos_validos_text IS INITIAL OR
     mv_datos_validos_gen IS INITIAL.
    CLEAR mv_datos_validos.
  ENDIF.

ENDMODULE.                 " VALID_DATA  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_VIEW  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
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

ENDMODULE.                 " F4_VIEW  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_CLASS_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_class_exit INPUT.
  DATA ls_return TYPE bapiret2.


  ls_return = mo_controller->check_exit_class( zal30_t_view-exit_class ).

  IF ls_return-type = zif_al30_data=>cs_msg_type-error.
    MESSAGE ID ls_return-id TYPE ls_return-type
                NUMBER ls_return-number
                WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.
  ENDIF.

ENDMODULE.                 " CHECK_CLASS_EXIT  INPUT
