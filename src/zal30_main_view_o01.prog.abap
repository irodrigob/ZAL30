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
    ms_layout-stylefname = zif_al30_data=>cv_field_style.

* Activo el evento de validación de datos
    CREATE OBJECT mo_event_receiver_alv.
    SET HANDLER mo_event_receiver_alv->handle_data_changed FOR mo_alv.

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
    mo_alv->set_frontend_layout( EXPORTING is_layout = VALUE #( cwidth_opt = abap_true ) ).

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
