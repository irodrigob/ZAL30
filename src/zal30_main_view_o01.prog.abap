*----------------------------------------------------------------------*
***INCLUDE ZAL30_MAIN_VIEW_01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET TITLEBAR 'T-2'.
  SET PF-STATUS '9000'.
ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*& Module SCREEN_FIELDS_9000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE screen_fields_9000 OUTPUT.

  " Desactivo el campo de entrada de la tabla porque ya viene preinformado
  LOOP AT SCREEN.

    " En el caso que la transacción de origen no sea distinta a la del programa y la tabla
    " esta informando lo que se hace es proteger el campo para que no se pueda cambiar. El motivo
    " es que viene de una transacción por parámetro y se evita que pueda cambiar de tabla.
    " Además se marca que se ha deshabilitado el campo de entrada de la tabla.
    IF screen-name = 'ZAL30_T_VIEW-TABNAME' .
      IF ms_conf_screen-origin_tcode NE zif_al30_data=>cs_prog_tcode-view
         AND ms_view-tabname IS NOT INITIAL.
        screen-input = 0.
      ELSE.
        screen-input = 1.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.

  ENDLOOP.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ALV_VIEW  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alv_view OUTPUT.
  DATA ld_repid TYPE sy-repid.
  DATA ld_dynnr TYPE sy-dynnr.

  IF mo_container IS NOT BOUND.

    ld_repid = sy-repid.
    ld_dynnr = sy-dynnr.

    CREATE OBJECT mo_container
      EXPORTING
        repid = ld_repid
        dynnr = ld_dynnr
        side  = cl_gui_docking_container=>dock_at_left
        ratio = 95.
*        extension = 450.

* Creo el ALV
    CREATE OBJECT mo_alv
      EXPORTING
        i_parent = mo_container.

* Layout
    ms_layout-col_opt = abap_true.
    ms_layout-cwidth_opt = abap_true.
    ms_layout-stylefname = zif_al30_data=>cs_control_fields_alv_data-style.

* Activo el evento de validación de datos
    SET HANDLER mo_controller->on_data_changed FOR mo_alv.
    SET HANDLER mo_controller->on_hotspot_click FOR mo_alv.

* La verificación cuando se haga enter
    CALL METHOD mo_alv->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter. " mc_evt_modified

* Mostrar campos
    CALL METHOD mo_alv->set_table_for_first_display
      EXPORTING
        i_bypassing_buffer   = 'X'
        is_layout            = ms_layout
        it_toolbar_excluding = mt_excluding
      CHANGING
        it_fieldcatalog      = mt_fieldcat
        it_outtab            = <it_datos>[]
        it_filter            = mt_filters.

* Activo el estado de edicion del ALV
    PERFORM edit_mode_alv.


  ELSE.

    " Se vuelve a indicar que tenga en cuenta las columnas optimizadas. Hay que ponerlo antes del refresh para que funcione
    mo_alv->set_frontend_layout( EXPORTING is_layout = ms_layout ).

    mo_alv->refresh_table_display( EXPORTING is_stable = ms_stable ).

  ENDIF.
ENDMODULE.                 " CREATE_ALV  OUTPUT
*&---------------------------------------------------------------------*
*& Module STATUS_9001 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9001 OUTPUT.
  DATA lt_fcode TYPE TABLE OF sy-ucomm.

  " Si no hay nombre de la tabla en el idioma de logon se muestra el nombre técnico
  IF mv_text_view IS NOT INITIAL.
    SET TITLEBAR 'T-1' WITH mv_text_view.
  ELSE.
    SET TITLEBAR 'T-1' WITH ms_view-tabname.
  ENDIF.

* Determinados botones solo estan activos en modo edicion
  IF mv_mode = zif_al30_data=>cv_mode_change.

* Quito el boton de transporte si la configuracion de la vista no lo permite.
    IF mv_pedir_orden = abap_false.
      APPEND 'TRANSP' TO lt_fcode.
    ENDIF.
    IF lt_fcode IS NOT INITIAL.
      SET PF-STATUS 'PANT-1' EXCLUDING lt_fcode.
    ELSE.
      SET PF-STATUS 'PANT-1'.
    ENDIF.

  ELSE.
    APPEND 'SAVE' TO lt_fcode.
    APPEND 'TRANSP' TO lt_fcode.
    SET PF-STATUS 'PANT-1' EXCLUDING lt_fcode.
  ENDIF.


  CLEAR mv_okcode_9001.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9002 OUTPUT.
  SET PF-STATUS '9002'.
  SET TITLEBAR 'T-3' WITH ms_conf_screen-origin_tcode_text.
ENDMODULE.
