*----------------------------------------------------------------------*
***INCLUDE ZAL30_MAIN_CONF_O01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  DATA lt_fcode TYPE TABLE OF sy-ucomm.

* El boton de grabar solo se habilita cuando se esta
* actualizando
  IF mv_mode = zif_al30_data=>cv_mode_change.
    SET PF-STATUS 'P9000'.
  ELSE.
    APPEND 'SAVE' TO lt_fcode.
    SET PF-STATUS 'P9000' EXCLUDING lt_fcode.
  ENDIF.


  SET TITLEBAR 'T9000'.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  FIELD_DYNPRO_9000  OUTPUT
*&---------------------------------------------------------------------*
MODULE field_dynpro_9000 OUTPUT.
  LOOP AT SCREEN.

* Si no hay modo de edición desactivo todos los
* elementos del grupo1 que sea igual a 'EDI' (campo de edicion).
* En el caso de los campos de texto de la aplicación lo dejo ináctivo
* para evitar que salga con "*" típico de campos de password.
* Los campos principal (group3 = 'APL') activo la entrada de datos.
    IF mv_mode IS INITIAL.
      IF screen-group1 = 'EDI' AND
         ( screen-name = 'ZAL30_T_VIEW-EXIT_CLASS' OR screen-name = 'ZAL30_T_VIEW-TEXTTABLE' ).
        screen-active = 0.
      ELSEIF screen-group1 = 'EDI'.
        screen-invisible = 1.
      ELSEIF screen-group3 = 'APL'.
        screen-input = 1.
      ENDIF.

* Si es el de creacion:
* Los campos del grupo3 = 'APL' (campos principal de aplicacion) los
* habilito/activo.
* Los campos del grupo2 = 'CRE' (botones de creacion) los activo.
* Los campos del grupo2 = 'BOE' (botones cuando existe la aplicación) los oculto.
* Los del grupo2 'FFI' (frame de campos de la vista) se ocultan
* Los del grupo2 'FFH' (frame de cabecera de la vista) se ocultan
    ELSEIF mv_mode = zif_al30_data=>cv_mode_insert.

      IF screen-group3 = 'APL'.
*        screen-active = 1.
        screen-input = 0.
      ELSEIF screen-group2 = 'CRE'.
        screen-invisible = 0.
      ELSEIF screen-group2 = 'BOE'.
        screen-invisible = 1.
      ELSEIF screen-group2 = 'FFI'.
        screen-invisible = 1.
      ELSEIF screen-group2 = 'FFH' AND
             ( screen-name = 'ZAL30_T_VIEW-EXIT_CLASS' OR screen-name = 'ZAL30_T_VIEW-TEXTTABLE' ).
        screen-active = 0.
      ELSEIF screen-group2 = 'FFH'.
        screen-invisible = 1.
      ENDIF.


* Si se esta modificando
* Los del grupo3 'APL'(campo principal de aplicacion) se desactiva su entrada de datos
* Los del grupo2 'ENT' (campo de entrada) se habilitan.
* Los del grupo2 'BOE' (botones si existe aplicacion) se habilitan
* Los del grupo2 'CRE' (botones de creacion) se ocultan.
* Los del grupo2 'FFI' (frame de campos de la vista) se habilitan según el
* valor de la variable d_expand_fields.
* Los del grupo2 'FFH' (frame de opciones de la vista) se habilitan según el
* valor de la variable d_expand_header.
* El campo del nombre de aplicacion se deshabilita.
    ELSEIF mv_mode = zif_al30_data=>cv_mode_change.
      IF screen-group2 = 'BOE'.
        screen-invisible = 0.
      ELSEIF screen-group2 = 'CRE'.
        screen-invisible = 1.
      ELSEIF screen-group3 = 'APL'.
        screen-input = 0.
      ELSEIF screen-group2 = 'ENT'.
        screen-input = 1.
*        screen-output = 0.
      ELSEIF screen-group2 = 'FFI'.
        IF mv_expand_fields = abap_true.
          screen-invisible = 0.
        ELSE.
          screen-invisible = 1.
        ENDIF.
      ELSEIF screen-group2 = 'FFH'.
        IF mv_expand_header = abap_true.
          screen-invisible = 0.
        ELSE.
          screen-invisible = 1.
        ENDIF.
      ENDIF.


* Si se esta visualizando..
* Los del grupo3 'APL' (campo de aplicacion) se deshabilitan.
* Los del grupo2 'BOE' (botones si existe aplicacion) se habilitan
* Los del grupo2 'CRE' (botones de creacion) se ocultan.
* Los del grupo4 'DAT' (botones que depende si el mandante se puede modificar) dependen
* de las variables globales que indidican como esta el sistema.
* Los del grupo4 'TRK' (botones que depende si el mandante se puede transportar) dependen
* de las variables globales que indidican como esta el sistema.

    ELSEIF mv_mode = zif_al30_data=>cv_mode_view.

      IF screen-group2 = 'BOE'.
        screen-invisible = 0.
      ELSEIF screen-group2 = 'CRE'.
        screen-invisible = 1.
      ELSEIF screen-group3 = 'APL'.
        screen-input = 0.
*        screen-output = 1.
      ELSEIF screen-group4 = 'TRK'.
        IF mv_pedir_orden = abap_true.
          screen-invisible = 0.
        ELSE.
          screen-invisible = 1.
        ENDIF.
      ELSEIF screen-group4 = 'DAT'.
        IF mv_perm_modif_datos = abap_true.
          screen-invisible = 0.
        ELSE.
          screen-invisible = 1.
        ENDIF.
      ENDIF.

    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

  CLEAR d_okcode_9000.
ENDMODULE.                 " FIELD_DYNPRO_9000  OUTPUT

*&SPWIZARD: OUTPUT MODULE FOR TS 'TAB_FIELDS'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: SETS ACTIVE TAB
MODULE tab_fields_active_tab_set OUTPUT.
  tab_fields-activetab = g_tab_fields-pressed_tab.
  CASE g_tab_fields-pressed_tab.
    WHEN c_tab_fields-tab1.
      g_tab_fields-subscreen = '9001'.
    WHEN c_tab_fields-tab2.
      g_tab_fields-subscreen = '9002'.
    WHEN c_tab_fields-tab3.
      g_tab_fields-subscreen = '9003'.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.                    "TAB_FIELDS_ACTIVE_TAB_SET OUTPUT
*&---------------------------------------------------------------------*
*&      Module  ALV_GEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alv_gen OUTPUT.
  IF mv_mode = zif_al30_data=>cv_mode_change OR
     mv_mode = zif_al30_data=>cv_mode_view.

    IF mo_container_gen IS NOT BOUND.

      CREATE OBJECT mo_container_gen
        EXPORTING
          container_name = 'ALV_GEN'.

* Creo el objeto de atributos genericos
      CREATE OBJECT mo_alv_gen
        EXPORTING
          i_parent = mo_container_gen.

* Catalogo de campos
      PERFORM alv_fieldcat_gen.

* Layout
      ms_layout_gen-stylefname = 'CELLTAB'.
      ms_layout_gen-cwidth_opt = abap_true.

* Campos con dropdown
      mo_alv_gen->set_drop_down_table(
        it_drop_down_alias = mt_dropdown_gen ).

* Mostrar campos
      CALL METHOD mo_alv_gen->set_table_for_first_display
        EXPORTING
          i_bypassing_buffer   = 'X'
          is_layout            = ms_layout_gen
          it_toolbar_excluding = mt_excluding
        CHANGING
          it_fieldcatalog      = mt_fieldcat_gen
          it_outtab            = mt_fields[].

* La verificación cuando se cambie el valor
      CALL METHOD mo_alv_gen->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_modified.

* Activo el evento de validación de datos
      CREATE OBJECT mo_event_receiver_gen.
      SET HANDLER mo_event_receiver_gen->handle_data_changed FOR mo_alv_gen.
      SET HANDLER mo_event_receiver_gen->handle_toolbar FOR mo_alv_gen.

      " Se lanza el evento para que pinta la barra de herramientas
      mo_alv_gen->set_toolbar_interactive( ).

    ELSE.

      " Se vuelve a indicar que tenga en cuenta las columnas optimizadas. Hay que ponerlo antes del refresh para que funcione
      mo_alv_gen->set_frontend_layout( EXPORTING is_layout = VALUE #( cwidth_opt = abap_true ) ).

      CALL METHOD mo_alv_gen->refresh_table_display( EXPORTING is_stable = ms_stable ).
    ENDIF.

  ENDIF.


* El ALV se activa/desactiva la entrada de datos segun el modo de visualización
  IF mo_alv_gen IS BOUND.
    CASE mv_mode.
      WHEN zif_al30_data=>cv_mode_change.
        CALL METHOD mo_alv_gen->set_ready_for_input
          EXPORTING
            i_ready_for_input = 1.
      WHEN zif_al30_data=>cv_mode_view.
        CALL METHOD mo_alv_gen->set_ready_for_input
          EXPORTING
            i_ready_for_input = 0.
    ENDCASE.
  ENDIF.

ENDMODULE.                 " ALV_GEN  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  ALV_TEXT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alv_text OUTPUT.

  IF mv_mode = zif_al30_data=>cv_mode_change OR
     mv_mode = zif_al30_data=>cv_mode_view.

    IF mo_container_text IS NOT BOUND.

      CREATE OBJECT mo_container_text
        EXPORTING
          container_name = 'ALV_TEXT'.

* Creo el ALV de atributos genericos
      CREATE OBJECT mo_alv_text
        EXPORTING
          i_parent = mo_container_text.

* Activo el evento de validación de datos
      CREATE OBJECT mo_event_receiver_text.
      SET HANDLER mo_event_receiver_text->handle_data_changed FOR mo_alv_text.

* La verificación cuando se haga enter
      CALL METHOD mo_alv_text->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter. " mc_evt_modified

* Catalogo de campos
      PERFORM alv_fieldcat_text.

* Layout
      ms_layout_text-stylefname = 'CELLTAB'.

* Mostrar campos
      CALL METHOD mo_alv_text->set_table_for_first_display
        EXPORTING
          i_bypassing_buffer   = 'X'
          is_layout            = ms_layout_text
          it_toolbar_excluding = mt_excluding
        CHANGING
          it_fieldcatalog      = mt_fieldcat_text
          it_outtab            = mt_fields_text[]
          it_filter            = mt_filter.

* Fuerzo la actualización de los método para despues aplicar el filtro.
      cl_gui_cfw=>flush( ).


    ELSE.

      " Se vuelve a indicar que tenga en cuenta las columnas optimizadas. Hay que ponerlo antes del refresh para que funcione
      mo_alv_text->set_frontend_layout( EXPORTING is_layout = VALUE #( cwidth_opt = abap_true ) ).

      CALL METHOD mo_alv_text->refresh_table_display( EXPORTING is_stable = ms_stable ).
    ENDIF.

  ENDIF.


* El ALV se activa/desactiva la entrada de datos segun el modo de visualización
  IF mo_alv_text IS BOUND.
    CASE mv_mode.
      WHEN zif_al30_data=>cv_mode_change.
        CALL METHOD mo_alv_text->set_ready_for_input
          EXPORTING
            i_ready_for_input = 1.
      WHEN zif_al30_data=>cv_mode_view.
        CALL METHOD mo_alv_text->set_ready_for_input
          EXPORTING
            i_ready_for_input = 0.
    ENDCASE.
  ENDIF.


ENDMODULE.                 " ALV_TEXT  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOOLBAR_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE toolbar_fields OUTPUT.
  DATA ls_events_toolbar TYPE cntl_simple_event.

* La barra de herramientas solo se muestra en modo modificacion.
  IF mv_mode = zif_al30_data=>cv_mode_change.

    IF mo_cont_toolbar_fields IS NOT BOUND.

      CREATE OBJECT mo_cont_toolbar_fields
        EXPORTING
          container_name = 'TOOLBAR_FIELDS'.

      CREATE OBJECT mo_toolbar_fields
        EXPORTING
          parent = mo_cont_toolbar_fields.

* Añado los eventos que quiero capturar
      ls_events_toolbar-eventid = cl_gui_toolbar=>m_id_function_selected.
      ls_events_toolbar-appl_event = 'X'.
      APPEND ls_events_toolbar TO mt_evts_toolbar_fields.
      CLEAR ls_events_toolbar.

      ls_events_toolbar-eventid = cl_gui_toolbar=>m_id_dropdown_clicked.
      ls_events_toolbar-appl_event = 'X'.
      APPEND ls_events_toolbar TO mt_evts_toolbar_fields.
      CLEAR ls_events_toolbar.

* Creo el objeto que gestionara los eventos
      CREATE OBJECT mo_event_toolbar_fields.

* Paso los eventos que quiero contralar a la barra de herramientas
      CALL METHOD mo_toolbar_fields->set_registered_events
        EXPORTING
          events = mt_evts_toolbar_fields.

* Asocio los eventos a los metodos de la clase que gestiona los eventos.
      SET HANDLER mo_event_toolbar_fields->on_function_selected
                  FOR mo_toolbar_fields.
      SET HANDLER mo_event_toolbar_fields->on_dropdown_clicked
                  FOR mo_toolbar_fields.

* Creo un grupo de botones que asociaré a la barra de herramientas
      CLEAR mt_buttongroup.

* Parametros: Codigo, icono, tipo, texto, tooltip
      " Verificación de integridad de datos
      PERFORM fill_button_table USING cs_toolbar_functions-check_dict icon_check cntb_btype_button space TEXT-b02.
      " Sincronización de datos
      PERFORM fill_button_table USING cs_toolbar_functions-sync_dict icon_generate cntb_btype_button space TEXT-b01.
      " Selección de idioma
      PERFORM fill_button_table USING cs_toolbar_functions-lang_menu space cntb_btype_dropdown TEXT-b03 TEXT-b03.
      " Añadir campos virtuales
      PERFORM fill_button_table USING cs_toolbar_functions-virtual_field icon_virtual_phio_class cntb_btype_button TEXT-b04 TEXT-b04.
      " Borrar campos
      PERFORM fill_button_table USING cs_toolbar_functions-delete_field icon_delete_row cntb_btype_button TEXT-b05 TEXT-b05.

* Paso los botones a la barra de herramientras
      CALL METHOD mo_toolbar_fields->add_button_group
        EXPORTING
          data_table       = mt_buttongroup
        EXCEPTIONS
          cntb_error_fcode = 1.

    ELSE.
* Si el container de la barra esta creada lo que me aseguro que este visible.
      mo_toolbar_fields->set_visible( abap_true ).
    ENDIF.

  ELSE.

* Si el modo no es el de modificar y ya he creado la barra entonces la oculta.
    IF mo_toolbar_fields IS BOUND.
      mo_toolbar_fields->set_visible( abap_false ).
    ENDIF.
  ENDIF.
ENDMODULE.                 " TOOLBAR_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*& Module STATUS_9004 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9004 OUTPUT.
  SET PF-STATUS 'P9004'.
  SET TITLEBAR 'T9004'.
ENDMODULE.
