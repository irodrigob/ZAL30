*&---------------------------------------------------------------------*
*&  Include           ZAL30_MAIN_CONF_C01
*&---------------------------------------------------------------------*

CLASS lcl_event_text DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_data_changed
                  FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed.
ENDCLASS.                    "lcl_event_text DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_text IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_text IMPLEMENTATION.

  METHOD handle_data_changed.
    FIELD-SYMBOLS <ls_mod_cells> TYPE LINE OF lvc_t_modi.
    FIELD-SYMBOLS <tbl> TYPE zif_al30_data=>tt_fields_text_view_alv.
    FIELD-SYMBOLS <wa> TYPE LINE OF zif_al30_data=>tt_fields_text_view_alv.


    DATA ld_tabix TYPE sytabix.

* Inicialmente los datos son correctos.
    mv_datos_validos_text = abap_true.

* Recupero los datos modificados para actualizarlo a la tabla principal.
* No sería porque al llamar el método "check_changed_data" el ALV ya lo hace. Pero
* este método se llama en el PAI y cuando se cambian de pestañas este no se llama. Por
* ello no tengo más remedio que hacer yo los cambios manualmente, de esta manera los
* datos se verán siempre actualizados.
    ASSIGN er_data_changed->mp_mod_rows->* TO <tbl>.

* la tabla MT_MOD_CELLS contiene las filas exactas cambiadas. Pero este no coincide
* con lo que hay en MP_MOD_ROWS, ya que solo contiene los registros modificados.
* Por eso tengo que hacer mi propio "tabix" para saber que posicion leer.
    ld_tabix = 1.

* Leo los campos modificados y verifico que los valores de texto no esten en blanco
* Si lo están genero un error.
    LOOP AT er_data_changed->mt_mod_cells ASSIGNING <ls_mod_cells>.
      AT NEW row_id.
* Leo el registro modificado asociado al ROW_ID
        UNASSIGN <wa>.
        READ TABLE <tbl> ASSIGNING <wa> INDEX ld_tabix.
*        IF sy-subrc = 0.
*
** Leo el registro de la tabla global
**          READ TABLE mt_fields_text ASSIGNING FIELD-SYMBOL(<ls_fields>) INDEX <ls_mod_cells>-row_id.
**          IF sy-subrc = 0.
*** Paso los datos
**            <ls_fields> = <wa>.
**          ENDIF.
*        ENDIF.
      ENDAT.

      ADD 1 TO ld_tabix.
      CASE <ls_mod_cells>-fieldname.
        WHEN 'SCRTEXT_S' OR 'SCRTEXT_M' OR 'SCRTEXT_L' OR 'REPTEXT'.

          IF <ls_mod_cells>-value IS INITIAL.
            mv_datos_validos_text = abap_false.
            CALL METHOD er_data_changed->add_protocol_entry
              EXPORTING
                i_msgid     = zif_al30_data=>cv_msg_id
                i_msgno     = '014'
                i_msgty     = zif_al30_data=>cs_msg_type-error
                i_fieldname = <ls_mod_cells>-fieldname
                i_row_id    = <ls_mod_cells>-row_id.
          ELSE.
            " Si se toca el campo de cabecera se actualiza en la tabla de campos
            IF <wa> IS ASSIGNED.
              IF <ls_mod_cells>-fieldname = 'REPTEXT'.
                READ TABLE mt_fields ASSIGNING FIELD-SYMBOL(<ls_fields>) WITH KEY fieldname = <wa>-fieldname.
                IF sy-subrc = 0.
                  <ls_fields>-reptext = <ls_mod_cells>-value.
                ENDIF.
              ENDIF.
            ENDIF.
            er_data_changed->modify_cell( EXPORTING i_row_id    = <ls_mod_cells>-row_id
                                                    i_tabix     = <ls_mod_cells>-tabix
                                                    i_fieldname = <ls_mod_cells>-fieldname
                                                    i_value     = <ls_mod_cells>-value ).
          ENDIF.
      ENDCASE.
    ENDLOOP.

* Actualizo los dos listados
    CALL METHOD mo_alv_text->refresh_table_display( EXPORTING is_stable = ms_stable ).
    CALL METHOD mo_alv_gen->refresh_table_display( EXPORTING is_stable = ms_stable ).

  ENDMETHOD.                    "handle_data_changed

ENDCLASS.                    "lcl_event_text IMPLEMENTATION

CLASS lcl_event_gen DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_data_changed
                  FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed.

    METHODS:
      handle_toolbar
                  FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive.
ENDCLASS.                    "lcl_event_text DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_text IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_gen IMPLEMENTATION.

  METHOD handle_data_changed.
    FIELD-SYMBOLS <ls_mod_cells> TYPE LINE OF lvc_t_modi.
    FIELD-SYMBOLS <tbl> TYPE zif_al30_data=>tt_fields_view_alv.
    FIELD-SYMBOLS <wa> TYPE LINE OF zif_al30_data=>tt_fields_view_alv.
    DATA ld_tabix TYPE sytabix.

* Recupero los datos modificados para actualizarlo a la tabla principal.
* No sería porque al llamar el método "check_changed_data" el ALV ya lo hace. Pero
* este método se llama en el PAI y cuando se cambian de pestañas este no se llama. Por
* ello no tengo más remedio que hacer yo los cambios manualmente, de esta manera los
* datos se verán siempre actualizados.
    ASSIGN er_data_changed->mp_mod_rows->* TO <tbl>.

* El objetivo es capturar si el origen de datos ha sido modificados. Si es así, se llama al procedimiento que actualiza los valores
    DATA(lv_source_text_change) = abap_false.
    LOOP AT er_data_changed->mt_mod_cells ASSIGNING <ls_mod_cells>.
      AT NEW row_id.
* Leo el registro modificado asociado al ROW_ID
        READ TABLE <tbl> ASSIGNING <wa> INDEX ld_tabix.
        IF sy-subrc = 0.
* Leo el registro de la tabla global
          READ TABLE mt_fields ASSIGNING FIELD-SYMBOL(<ls_fields>) INDEX <ls_mod_cells>-row_id.
          IF sy-subrc = 0.
* Paso los datos
            <ls_fields> = <wa>.

            " Si el campo se ha marcado como técnico hay campos que se tienen que informar como falso
            " En caso contrario se informará lo que ya tenga.
            IF <ls_fields>-tech = abap_true.
              <ls_fields>-sel_screen = abap_false.
              <ls_fields>-mandatory = abap_false.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDAT.

      " Se comprueba el campo de origen de texto. Esto activa una variable que realiza al final del proceso
      " se resincronizan la editabilidad de los campos de texto y sus valores.
      IF <ls_mod_cells>-fieldname = zif_al30_data=>cs_fix_field_conf-source_text.
        lv_source_text_change = abap_true.
      ENDIF.

      " Si se modifica el campo técnico se tiene sincronizar los campos asociados
      IF <ls_mod_cells>-fieldname = zif_al30_data=>cs_fix_field_conf-tech.
        DATA(lv_tech_change) = abap_true.
      ENDIF.

      " Si el campo modificado es el elemento de datos virtual hay que chequear que sea valido
      IF <ls_mod_cells>-fieldname = zif_al30_data=>cs_fix_field_conf-virtual_dtel.
        IF zcl_al30_util=>exist_data_element( CONV #( <ls_mod_cells>-value ) ) = abap_false.
          CALL METHOD er_data_changed->add_protocol_entry
            EXPORTING
              i_msgid     = 'ZAL30'
              i_msgno     = '021'
              i_msgty     = zif_al30_data=>cs_msg_type-error
              i_fieldname = <ls_mod_cells>-fieldname " Se escoge el último campo de la clave
              i_msgv1     = <ls_mod_cells>-value
              i_row_id    = <ls_mod_cells>-row_id
              i_tabix     = <ls_mod_cells>-tabix.
        ENDIF.
      ENDIF.

    ENDLOOP.

    " Si el origen del texto cambio lanzo el proceso que actualiza los campos editables de texto y
    " sincronizar el textos segun el diccionario, solo para campos cuyo origen de texto es el diccionario.
    IF lv_source_text_change = abap_true.
      PERFORM enabled_field_texts_source IN PROGRAM (sy-cprog).
      PERFORM syncro_field_texts_source  IN PROGRAM (sy-cprog).
    ENDIF.


* Activación/desacticacion del campo de checkbox cuando el campo técnico varia
    IF lv_tech_change = abap_true.
      PERFORM enabled_field_tech.
    ENDIF.

* Actualizo los dos listados
    CALL METHOD mo_alv_text->refresh_table_display( EXPORTING is_stable = ms_stable ).
    CALL METHOD mo_alv_gen->refresh_table_display( EXPORTING is_stable = ms_stable ).

  ENDMETHOD.                    "handle_data_changed

  METHOD handle_toolbar.

    " Se añade un separador
    INSERT VALUE #( butn_type = zif_al30_data=>cs_alv_toolbar_type-separator  ) INTO TABLE e_object->mt_toolbar.

  ENDMETHOD.

ENDCLASS.
