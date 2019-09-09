*&---------------------------------------------------------------------*
*&  Include           ZAL30_MAIN_CONF_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INITIALIZACION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM initializacion .

* Creo el objeto que orquestará todas las operaciones
  CREATE OBJECT mo_controller.

* Funciones que se excluirán de los ALV
  PERFORM func_excluding_alv.

* Catalogo general de campos
  PERFORM alv_fieldcatalog.

* Indico que los refrescos de los ALV no se moverán de filas y columna
  ms_stable-row = abap_true.
  ms_stable-col = abap_true.

* Por defectos los datos de las pestañas son correctos
  mv_datos_validos_text = abap_true.
  mv_datos_validos_gen = abap_true.

* El frame donde se muestran los datos de cabera y campos se muestra por defecto
  mv_expand_fields = abap_true.
  mv_expand_header = abap_true.

* Diferencias entre la vista y el diccionario. Inicialmente no las hay.
  mv_diff_dict = abap_false.

* Se recupera si se podra grabar la configuración
  mv_pedir_orden = mo_controller->allowed_transport( ).

* Variable donde se guarda la orden de transporte donde se guarda la
* configuración
  CLEAR mv_orden_transporte.

* Idioma de visualización de textos
  mv_lang_vis = sy-langu.

* Se recupera si se podrá grabar la configuración
*  d_perm_modif_datos = go_controller->allowed_modify_data( ).
* Lo dejo abierto porque es una tabla de worbench y en cualquier sistema se podra modificar.
* y lo que devuelve el método anterior solo tiene sentido para tablas de customizing. Aún asi,
* dejo la variable porque si en un futuro quiero restringir por otra casuística.
  mv_perm_modif_datos = abap_true.

* Relleno la tablas de alv para los filtros comunes.
  PERFORM fill_filtro_comunes.


ENDFORM.                    " INITIALIZACION
*&---------------------------------------------------------------------*
*&      Form  CHANGE_VIEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM change_view .
  DATA ls_return TYPE bapiret2.
  DATA ld_name_view TYPE tabname.

* Paso el nombre de la vista a una variable para que no se limpie
* cuando se busca los datos de la vista
  ld_name_view = zal30_t_view-tabname.

  PERFORM read_view USING ld_name_view
                    CHANGING ls_return.

* Miro si la vista existe. En caso contrario lanzo la operación de creacion
  IF ls_return-type = zif_al30_data=>cs_msg_type-error.

* Vuelvo a recuperar el nombre de la vista
    zal30_t_view-tabname = ld_name_view.

* En ese caso saco el mensaje pasado. Lo pongo de tipo 'S'
* porque aunque no exista, debe existir la opción de crearlo.
    MESSAGE ID ls_return-id TYPE zif_al30_data=>cs_msg_type-success
            NUMBER ls_return-number
            WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.

* Activo que la único edición posible es la de creacion
    mv_mode = zif_al30_data=>cv_mode_insert.


  ELSE.

* Activo el modo de visualizacion
* PARA IR MÁS RAPIDO EN LAS PRUEBAS LO PONGO EN MODO
* MODIFICACION
    mv_mode = zif_al30_data=>cv_mode_change.

* Chequeo si hay diferencias entre la configuracion guardada y la tabla del diccionario
    mo_controller->check_changes_dict_view( EXPORTING is_view   = zal30_t_view
*                                                      it_fields_view_alv = mt_fields
*                                                      it_fields_text_view_alv = mt_fields_text
                                            IMPORTING ev_diff_fields = DATA(lv_diff_fields)
                                                      ev_diff_text = DATA(lv_diff_text) ).

    IF lv_diff_fields = abap_true OR lv_diff_text = abap_true.
      " Si las hay se mira si tiene la opción de autoajuste. Si la tiene se ejecuta el ajuste de manera automática
      IF mo_controller->view_have_auto_adjust( zal30_t_view-tabname  ) = abap_true.
        mo_controller->auto_adjust_view_ddic(
          EXPORTING
            iv_name_view = zal30_t_view-tabname
          IMPORTING
            es_return    = ls_return
            es_view = zal30_t_view
            et_fields_text_view_alv = mt_fields_text
            et_fields_view_alv = mt_fields ).

        " Solo si hay errores en el ajuste
        IF ls_return-type = zif_al30_data=>cs_msg_type-error.
          MESSAGE ID ls_return-id TYPE ls_return-type
                       NUMBER ls_return-number
                       WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.
        ELSE.
          MESSAGE s035.
        ENDIF.
      ELSE.
        mv_diff_dict = abap_true.
        MESSAGE w024.
      ENDIF.

    ELSE.
      mv_diff_dict = abap_false.
    ENDIF.

  ENDIF.

ENDFORM.                    " CHANGE_VIEW
*&---------------------------------------------------------------------*
*&      Form  CHECK_VIEW_INSERT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_view_insert .
  DATA ls_return TYPE bapiret2.

* Verifico que la vista/tabla sea correcta
  CALL METHOD mo_controller->check_view
    EXPORTING
      iv_name_view = zal30_t_view-tabname
      iv_operation = zif_al30_data=>cv_operation_insert
    IMPORTING
      es_return    = ls_return
      ev_text_view = d_text_view.

  IF ls_return-type = zif_al30_data=>cs_msg_type-error.
    MESSAGE ID ls_return-id TYPE ls_return-type
            NUMBER ls_return-number
            WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.
  ENDIF.

ENDFORM.                    " CHECK_VIEW_INSERT
*&---------------------------------------------------------------------*
*&      Form  CREATE_VIEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_view .
  DATA ls_return TYPE bapiret2.
  DATA ls_return_read TYPE bapiret2.
  DATA ld_name_view TYPE tabname.

  CALL METHOD mo_controller->create_view
    EXPORTING
      iv_name_view            = zal30_t_view-tabname
    IMPORTING
      es_return               = ls_return
      ev_text_view            = d_text_view
      et_fields_view_alv      = mt_fields
      et_fields_text_view_alv = mt_fields_text
      es_view                 = zal30_t_view.

* Si no hay error indico que la edición va ser la actualizacion.
  IF ls_return-type NE zif_al30_data=>cs_msg_type-error.
    mv_mode = zif_al30_data=>cv_mode_change.

* Ajuste de campos y textos de los listados
    PERFORM adjust_field_and_text.


  ELSE.

* Si hay error vuelto a la situación anterior.
    CLEAR mv_mode.

* El mensaje de error lo saco como warning para que pueda cambiar la vista si necesidad de salir
* del programa
    MESSAGE ID ls_return-id TYPE zif_al30_data=>cs_msg_type-warning
              NUMBER ls_return-number
              WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.


  ENDIF.



ENDFORM.                    " CREATE_VIEW
*&---------------------------------------------------------------------*
*&      Form  DELETE_VIEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM delete_view .

  DATA ld_answer TYPE c.
  DATA ls_return TYPE bapiret2.

* Confirmacion del borrado
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = TEXT-t03
      text_question         = TEXT-t04
      text_button_1         = TEXT-t05
      text_button_2         = TEXT-t06
      default_button        = '2'
      display_cancel_button = ''
    IMPORTING
      answer                = ld_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  IF ld_answer = '1'.

    ls_return = mo_controller->delete_view( zal30_t_view-tabname ).

* Saco el mensaje devuelto tal cual se produce.
    MESSAGE ID ls_return-id TYPE ls_return-type
          NUMBER ls_return-number
          WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.

* Si el mensaje es correcto pongo el modo de edición al inicial
    IF ls_return-type = zif_al30_data=>cs_msg_type-success.

      PERFORM clean_data.

    ENDIF.

  ELSE.
    MESSAGE s033.
  ENDIF.

ENDFORM.                    " DELETE_VIEW
*&---------------------------------------------------------------------*
*&      Form  CLEAN_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM clean_data .
  CLEAR: mv_mode, zal30_t_view, mt_fields, d_text_view.

* Si el ALV de atributros genericos esta instancio lo libero de memoria,
* así como, su container.
  IF mo_alv_gen IS BOUND.
    mo_alv_gen->free( ).
    mo_container_gen->free( ).
    FREE: mo_alv_gen, mo_container_gen.
  ENDIF.

* Lo mismo para los atributos de texto
  IF mo_alv_text IS BOUND.
    mo_alv_text->free( ).
    mo_container_text->free( ).
    FREE: mo_alv_text, mo_container_text.
  ENDIF.

ENDFORM.                    " CLEAN_DATA
*&---------------------------------------------------------------------*
*&      Form  OTHER_VIEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM other_view .

* Limpio los datos para dejarlo todo al estado inicial.
  PERFORM clean_data.

ENDFORM.                    " OTHER_VIEW
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_CHANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_change .

  IF mv_datos_validos = abap_true.

    CASE mv_mode.

      WHEN zif_al30_data=>cv_mode_view.
        mv_mode = zif_al30_data=>cv_mode_change.
      WHEN zif_al30_data=>cv_mode_change.
        mv_mode = zif_al30_data=>cv_mode_view.

    ENDCASE.

  ELSE.

* Saco los mensajes de las pestañas que tienen error
    PERFORM mostrar_msg_tab_error.

  ENDIF.

ENDFORM.                    " DISPLAY_CHANGE

*&---------------------------------------------------------------------*
*&      Form  FUNC_EXCLUDING_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM func_excluding_alv .
  CLEAR: mt_excluding.

  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row TO mt_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy TO mt_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row TO mt_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_loc_cut TO mt_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row TO mt_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row TO mt_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_loc_move_row TO mt_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste TO mt_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO mt_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo TO mt_excluding.

ENDFORM.                    " FUNC_EXCLUDING_ALV
*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_MSG_TAB_ERROR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM mostrar_msg_tab_error .

* Saco los mensaje de tipo warning para que vea donde están los errores.
  IF mv_datos_validos_gen = abap_false.
    MESSAGE w015.
  ENDIF.

  IF mv_datos_validos_text = abap_false.
    MESSAGE w016.
  ENDIF.
ENDFORM.                    " MOSTRAR_MSG_TAB_ERROR
*&---------------------------------------------------------------------*
*&      Form  SAVE_VIEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM save_view .
  DATA ls_return TYPE bapiret2.

  IF mv_datos_validos = abap_true.

    ls_return = mo_controller->save_view_alv( is_view = zal30_t_view
                                              it_fields_view_alv = mt_fields
                                              it_fields_text_view_alv = mt_fields_text ).

    MESSAGE ID ls_return-id TYPE ls_return-type
              NUMBER ls_return-number
              WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.

  ELSE.

* Saco los mensajes de las pestañas que tienen error
    PERFORM mostrar_msg_tab_error.

  ENDIF.
ENDFORM.                    " SAVE_VIEW
*&---------------------------------------------------------------------*
*&      Form  READ_VIEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_view  USING    pe_name_view
                CHANGING ps_return TYPE bapiret2.

* Debido a que en la creacion se pueden inicializar valores lo que hago es
* leer la vista y así tengo los datos exactos,
  CALL METHOD mo_controller->read_view_alv
    EXPORTING
      iv_name_view            = pe_name_view
      iv_all_language         = abap_true
    IMPORTING
      es_return               = ps_return
      es_view                 = zal30_t_view
      et_fields_view_alv      = mt_fields
      et_fields_text_view_alv = mt_fields_text
      ev_text_view            = d_text_view.

  " Guardo los textos originales de la vista para poder recuperar
  " el texto en caso necesario
  mt_fields_text_orig = mt_fields_text.

* Ajuste de campos y textos de los listados
  PERFORM adjust_field_and_text.

ENDFORM.                    " READ_VIEW
*&---------------------------------------------------------------------*
*&      Form  EXPAND_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM expand_fields .

* Cuando se pulsa el botón cambio el estado el cual estaba.
  IF mv_expand_fields = abap_true.
    mv_expand_fields = abap_false.
  ELSE.
    mv_expand_fields = abap_true.
  ENDIF.

ENDFORM.                    " EXPAND_FIELDS
*&---------------------------------------------------------------------*
*&      Form  FILL_BUTTON_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_button_table  USING fcode  TYPE ui_func
                              icon   TYPE iconname
                              type   TYPE tb_btype
                              text   TYPE text40
                              tip.
  DATA ld_tip TYPE iconquick.

  ld_tip = tip.

  CALL METHOD mo_toolbar_fields->fill_buttons_data_table
    EXPORTING
      fcode      = fcode
      icon       = icon
      butn_type  = type
      text       = text
      quickinfo  = ld_tip
    CHANGING
      data_table = mt_buttongroup.

ENDFORM.                    " FILL_BUTTON_TABLE
*&---------------------------------------------------------------------*
*&      Form  SYNC_DICTIONARY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM sync_dictionary .
  DATA ls_return TYPE bapiret2.
  DATA ld_keep_text TYPE sap_bool.
  DATA ld_answer TYPE c.

* Confirmación para mantener los textos
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = TEXT-t07
      text_question         = TEXT-t08
      text_button_1         = TEXT-t09
      text_button_2         = TEXT-t10
      default_button        = '1'
      display_cancel_button = ''
    IMPORTING
      answer                = ld_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  IF ld_answer = '1'.
    ld_keep_text = abap_true.
  ELSE.
    ld_keep_text = abap_false.
  ENDIF.

  CALL METHOD mo_controller->adjust_view_dictionary
    EXPORTING
      iv_keep_text            = ld_keep_text
    IMPORTING
      es_return               = ls_return
      ev_text_view            = d_text_view
    CHANGING
      cs_view                 = zal30_t_view
      ct_fields_view_alv      = mt_fields
      ct_fields_text_view_alv = mt_fields_text.

* Si no hay errores indico que no hay diferencias con el diccionario
  IF ls_return-type NE zif_al30_data=>cs_msg_type-error.
    mv_diff_dict = abap_false.
  ENDIF.

* El mensaje lo saco tal cual
  MESSAGE ID ls_return-id TYPE ls_return-type
                 NUMBER ls_return-number
                 WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.

* Actualizo los dos listados
  CALL METHOD mo_alv_text->refresh_table_display( EXPORTING is_stable = ms_stable ).
  CALL METHOD mo_alv_gen->refresh_table_display( EXPORTING is_stable = ms_stable ).


ENDFORM.                    " SYNC_DICTIONARY
*&---------------------------------------------------------------------*
*&      Form  CHECK_DICTIONARY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_dictionary .

  mo_controller->check_changes_dict_view( EXPORTING is_view   = zal30_t_view
                                          IMPORTING ev_diff_fields = DATA(lv_diff_fields)
                                                    ev_diff_text = DATA(lv_diff_text) ).

  IF lv_diff_fields = abap_true OR lv_diff_text = abap_true.
    mv_diff_dict = abap_true.
    MESSAGE w024 WITH zal30_t_view-tabname.
  ELSE.
    mv_diff_dict = abap_false .
    MESSAGE s025 WITH zal30_t_view-tabname.
  ENDIF.

ENDFORM.                    " CHECK_DICTIONARY
*&---------------------------------------------------------------------*
*&      Form  LOAD_DROPDOWN_SOURCE_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM load_dropdown_source_text .

  zcl_al30_util=>get_key_value_domain(
    EXPORTING
      iv_domain = zif_al30_data=>cv_domain_source_text
    IMPORTING
      et_values = DATA(lt_values) ).

  LOOP AT lt_values ASSIGNING FIELD-SYMBOL(<ls_values>).
    APPEND VALUE #( handle = mc_hndl_source_text int_value = <ls_values>-key value = <ls_values>-value ) TO mt_dropdown_gen.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ENABLED_FIELD_TEXTS_SOURCE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM enabled_field_texts_source .

* Sacamos los campos asociados a los atributos de texto
  DATA(lt_fields_alv) = zcl_al30_util=>get_fields_struc( zif_al30_data=>cv_fields_attr_text ).

  LOOP AT mt_fields ASSIGNING FIELD-SYMBOL(<ls_fields>).


    IF <ls_fields>-source_text = zif_al30_data=>cs_source_text-dictionary.
      DATA(lv_style) = cl_gui_alv_grid=>mc_style_disabled.
    ELSE.
      lv_style = cl_gui_alv_grid=>mc_style_enabled.
    ENDIF.

    LOOP AT mt_fields_text ASSIGNING FIELD-SYMBOL(<ls_fields_text>) WHERE fieldname = <ls_fields>-fieldname.
      LOOP AT lt_fields_alv ASSIGNING FIELD-SYMBOL(<ls_fields_alv>).
        READ TABLE <ls_fields_text>-celltab ASSIGNING FIELD-SYMBOL(<ls_celltab>) WITH KEY fieldname = <ls_fields_alv>.
        IF sy-subrc NE 0.
          INSERT VALUE #( fieldname = <ls_fields_alv> style = lv_style ) INTO TABLE <ls_fields_text>-celltab.
        ELSE.
          <ls_celltab>-style = lv_style.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_FILTRO_COMUNES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_filtro_comunes .

  CLEAR mt_filter.

  APPEND VALUE #( fieldname = 'SPRAS' sign = 'I' option = 'EQ' low = mv_lang_vis ) TO mt_filter.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHANGE_LANGUAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM change_language  USING    pe_fcode.

* Indico el nuevo idioma de visualización.
  mv_lang_vis = pe_fcode+5.

* Establezco el filtro de idioma al listado.
  PERFORM set_filter_language_alv CHANGING mo_alv_text.

* Se pone el idioma de cabecera al ALV de campos.
  PERFORM text_heading_fields.

* Fuerzo la actualización de los métodos que se vean los filtros
  cl_gui_cfw=>flush( ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_FILTER_LANGUAGE_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_MO_ALV_TEXT  text
*----------------------------------------------------------------------*
FORM set_filter_language_alv CHANGING ps_alv TYPE REF TO cl_gui_alv_grid.

  DATA lt_filter TYPE lvc_t_filt.
  DATA ls_filter TYPE lvc_t_filt.

  " Recupero los filtros que pueda tener
  CALL METHOD ps_alv->get_filter_criteria
    IMPORTING
      et_filter = lt_filter.

* Elimino el filtro de campo idioma, que se rellena de manera fija
  DELETE lt_filter WHERE fieldname = 'SPRAS'.

* Creo el filtro
  PERFORM fill_filtro_comunes. " Rellena los filtros comunes para los alvs

  APPEND LINES OF mt_filter TO lt_filter.

* Se establece el filtro
  CALL METHOD ps_alv->set_filter_criteria
    EXPORTING
      it_filter                 = lt_filter
    EXCEPTIONS
      no_fieldcatalog_available = 1
      OTHERS                    = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  TEXT_HEADING_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM text_heading_fields .

  " Actualizo el texto del campo segun el origen del texto del campo
  LOOP AT mt_fields ASSIGNING FIELD-SYMBOL(<ls_fields>).

    READ TABLE mt_fields_text ASSIGNING FIELD-SYMBOL(<ls_fields_text>)
                           WITH KEY fieldname = <ls_fields>-fieldname
                                    spras = mv_lang_vis.

    IF sy-subrc = 0.
      <ls_fields>-reptext = <ls_fields_text>-reptext.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SUBMENU_LANGUAGES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM submenu_languages  USING pe_posx pe_posy.

  DATA: lo_menu TYPE REF TO cl_ctmenu.
  DATA: lv_fcode TYPE ui_func.
  DATA: lv_text TYPE gui_text.
  DATA ld_checked TYPE cua_active.

  CREATE OBJECT lo_menu.

* Recupero los idiomas instalados en el sistema
  mo_controller->get_logon_languages( IMPORTING et_lang = DATA(lt_languages) ).

  LOOP AT lt_languages ASSIGNING FIELD-SYMBOL(<ls_logon_lang>).
    CONCATENATE mc_id_lang_button <ls_logon_lang>-lang INTO lv_fcode.
    lv_text = <ls_logon_lang>-description.

* Si el idioma de visualización es el mismo que se esta leyendo la opcion del
* menu saldra marcada. De esta manera se sabe cual es idioma que se esta
* tratando.
    IF <ls_logon_lang>-lang = mv_lang_vis.
      ld_checked = abap_true.
    ELSE.
      ld_checked = abap_false.
    ENDIF.

    CALL METHOD lo_menu->add_function
      EXPORTING
        fcode   = lv_fcode
        text    = lv_text
        checked = ld_checked.

  ENDLOOP.
  IF sy-subrc = 0.
    CALL METHOD mo_toolbar_fields->track_context_menu
      EXPORTING
        context_menu = lo_menu
        posx         = pe_posx
        posy         = pe_posy.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ADJUST_FIELD_AND_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM adjust_field_and_text .
* Activación/Desactivación de campos de entrada de textos segun la opción de origen del texto
  PERFORM enabled_field_texts_source.

* Se pone el idioma de cabecera al ALV de campos.
  PERFORM text_heading_fields.

* Activación/desacticacion del campo de checkbox si el campo es técnico
  PERFORM enabled_field_tech.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_FIELDCAT_GEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_fieldcat_gen .

  FIELD-SYMBOLS <ls_fieldcat> TYPE LINE OF lvc_t_fcat.
  DATA ls_fieldcat_gen TYPE LINE OF lvc_t_fcat.

  CLEAR: mt_fieldcat_gen.

* Recorro los campos y los voy colocando
  LOOP AT mt_fieldcat ASSIGNING <ls_fieldcat>.

    CLEAR: ls_fieldcat_gen.

    CASE <ls_fieldcat>-fieldname.
      WHEN 'FIELDNAME'. " Nombre del campo
* El nombre del campo se pone en todos
        <ls_fieldcat>-col_opt = abap_true.
        ls_fieldcat_gen = <ls_fieldcat>.

      WHEN 'REPTEXT'. " Texto cabecera
        <ls_fieldcat>-col_opt = abap_true.

* El texto de cabecera se pone siempre.
        ls_fieldcat_gen = <ls_fieldcat>.

* En los atributos generales ha de salir en la segunda posicion justo despues del nombre del campo.
        ls_fieldcat_gen-col_pos = 2.

      WHEN 'KEY_DDIC'. " Indicador de campo clave
        ls_fieldcat_gen = <ls_fieldcat>.
        ls_fieldcat_gen-checkbox = abap_true.


* Opciones de visualización van a los atributos principales
      WHEN 'NO_OUTPUT' OR 'TECH' OR 'MANDATORY'.
        <ls_fieldcat>-col_opt = abap_true.
        ls_fieldcat_gen = <ls_fieldcat>.
        ls_fieldcat_gen-checkbox = abap_true.
        ls_fieldcat_gen-edit = abap_true.
        ls_fieldcat_gen-outputlen = 9.
      WHEN 'SOURCE_TEXT'.  " Origen del texto del campo
        <ls_fieldcat>-col_opt = abap_true.
        ls_fieldcat_gen = <ls_fieldcat>.
        ls_fieldcat_gen-edit = abap_true.
        ls_fieldcat_gen-drdn_hndl = mc_hndl_source_text. " Handle del dropdown
        ls_fieldcat_gen-drdn_alias = abap_true.
        ls_fieldcat_gen-convexit = 'ZSRCT'.

        " Se carga los valores para el dropdown
        PERFORM load_dropdown_source_text.

* Indicador de campo que pertenece a la tabla de textos
      WHEN 'FIELD_TEXTTABLE'.
        ls_fieldcat_gen-fieldname = <ls_fieldcat>-fieldname.
        ls_fieldcat_gen-checkbox = abap_true.
        ls_fieldcat_gen-coltext = TEXT-c01.
        ls_fieldcat_gen-col_pos = 12.

* Indicador que es el campo idioma de la tabla de textos
      WHEN 'LANG_TEXTTABLE'.
        ls_fieldcat_gen-fieldname = <ls_fieldcat>-fieldname.
        ls_fieldcat_gen-checkbox = abap_true.
        ls_fieldcat_gen-coltext = TEXT-c02.
        ls_fieldcat_gen-col_pos = 13.
      WHEN 'CHECKBOX'. " Indicador si es un campo de checkbox
        ls_fieldcat_gen-fieldname = <ls_fieldcat>-fieldname.
        ls_fieldcat_gen-checkbox = abap_true.
        ls_fieldcat_gen-coltext = TEXT-c03.
        ls_fieldcat_gen-col_pos = 10.
        ls_fieldcat_gen-edit = abap_true.

      WHEN 'SEL_SCREEN'. " Indicador si se mostrará en la pantalla de selección
        ls_fieldcat_gen-fieldname = <ls_fieldcat>-fieldname.
        ls_fieldcat_gen-checkbox = abap_true.
        ls_fieldcat_gen-coltext = TEXT-c04.
        ls_fieldcat_gen-col_pos = 11.
        ls_fieldcat_gen-edit = abap_true.

    ENDCASE.

    IF ls_fieldcat_gen IS NOT INITIAL.
      APPEND ls_fieldcat_gen TO mt_fieldcat_gen.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_FIELDCAT_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_fieldcat_text .
  FIELD-SYMBOLS <ls_fieldcat> TYPE LINE OF lvc_t_fcat.
  DATA ls_fieldcat_text TYPE LINE OF lvc_t_fcat.

  CLEAR: mt_fieldcat_text.

* Recorro los campos y los voy colocando
  LOOP AT mt_fieldcat ASSIGNING <ls_fieldcat>.

    CLEAR: ls_fieldcat_text.

    CASE <ls_fieldcat>-fieldname.
      WHEN 'FIELDNAME'. " Nombre del campo
* El nombre del campo se pone en todos
        <ls_fieldcat>-col_opt = abap_true.
        ls_fieldcat_text = <ls_fieldcat>.

      WHEN 'REPTEXT'. " Texto cabecera
        <ls_fieldcat>-col_opt = abap_true.
* El texto de cabecera se pone siempre.
        ls_fieldcat_text = <ls_fieldcat>.

* Para el catalogo de textos ha de ser editable
        ls_fieldcat_text-edit = 'X'.

      WHEN 'SPRAS'.
* El comportamiento del campo debe ser el mismo en los dos listados
        ls_fieldcat_text = <ls_fieldcat>.
        ls_fieldcat_text-col_opt = abap_true.
        ls_fieldcat_text-no_out = abap_true.
        ls_fieldcat_text-col_pos = 2.

* Texto corto, largo y grande para ALV de textos
      WHEN 'SCRTEXT_S' OR 'SCRTEXT_M' OR 'SCRTEXT_L'.
        <ls_fieldcat>-col_opt = abap_true.
        ls_fieldcat_text = <ls_fieldcat>.
        ls_fieldcat_text-edit = 'X'.

    ENDCASE.

    IF ls_fieldcat_text IS NOT INITIAL.
      APPEND ls_fieldcat_text TO mt_fieldcat_text.
    ENDIF.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_fieldcatalog .

  CLEAR: mt_fieldcat.


* En la estructura de diccionario contiene todos los campos que contendrán los dos ALV
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = zif_al30_data=>cv_ddic_fields
      i_bypassing_buffer     = abap_true
    CHANGING
      ct_fieldcat            = mt_fieldcat[]
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
* Implement suitable error handling here
    MESSAGE x012 WITH zif_al30_data=>cv_ddic_fields.
  ENDIF.

* Borro los campos de mandante y tabla que no serán necesarios.
  DELETE mt_fieldcat WHERE fieldname = 'MANDT'.
  DELETE mt_fieldcat WHERE fieldname = 'TABNAME'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  TRANSPORT_ENTRIES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM transport_entries  USING pe_objfunc TYPE objfunc.
  DATA ls_return TYPE bapiret2.

* Aunque el boton de transporte no es visible si el sistema esta cerrado, hago
* una segunda comprobación por si ponen el codigo de funcion por la barra de comandos.
  IF mv_pedir_orden = abap_true.

    " Si lanza el proceso que lo que hará es validar y/o solicitar la orden de transporte
    " para guardar las entradas.
    mo_controller->check_select_transport_order( EXPORTING iv_category = zif_al30_data=>cs_order_category-workbench
                                                 IMPORTING es_return = ls_return
                                                 CHANGING cv_order = mv_orden_transporte ).



    IF mv_orden_transporte IS NOT INITIAL.
      mo_controller->transport_view_alv( EXPORTING is_view = zal30_t_view
                                                   it_fields_view_alv = mt_fields
                                                   it_fields_text_view_alv = mt_fields_text
                                                   iv_spras = mv_lang_vis
                                         IMPORTING es_return = ls_return
                                         CHANGING cv_order = mv_orden_transporte ).

    ENDIF.
    IF ls_return IS NOT INITIAL.
      MESSAGE ID ls_return-id TYPE ls_return-type
                  NUMBER ls_return-number
                  WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  syncro_field_texts_source
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM syncro_field_texts_source .

* El objetivo es que todos los campos que tengan la opción de textos
* del diccionario, ponerle el texto del diccionario
  LOOP AT mt_fields ASSIGNING FIELD-SYMBOL(<ls_fields>)
                    WHERE source_text = zif_al30_data=>cs_source_text-dictionary.
    READ TABLE mt_fields_text ASSIGNING FIELD-SYMBOL(<ls_fields_text>) WITH KEY fieldname = <ls_fields>-fieldname
                                                                                spras = mv_lang_vis.
    IF sy-subrc = 0.
      READ TABLE mt_fields_text_orig ASSIGNING FIELD-SYMBOL(<ls_fields_text_orig>)
                                     WITH KEY fieldname = <ls_fields>-fieldname
                                              spras = <ls_fields_text>-spras.
      IF sy-subrc = 0.
        <ls_fields>-reptext = <ls_fields_text>-reptext = <ls_fields_text_orig>-reptext.
        <ls_fields_text>-scrtext_l = <ls_fields_text_orig>-scrtext_l.
        <ls_fields_text>-scrtext_s = <ls_fields_text_orig>-scrtext_s.
        <ls_fields_text>-scrtext_m = <ls_fields_text_orig>-scrtext_m.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ENABLED_FIELD_TECH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM enabled_field_tech .


  LOOP AT mt_fields ASSIGNING FIELD-SYMBOL(<ls_fields>).

    IF <ls_fields>-tech = abap_true.
      DATA(lv_style) = cl_gui_alv_grid=>mc_style_disabled.
    ELSE.
      lv_style = cl_gui_alv_grid=>mc_style_enabled.
    ENDIF.

    " Campo pantalla de seleccion
    READ TABLE <ls_fields>-celltab  ASSIGNING FIELD-SYMBOL(<ls_celltab>)
                                    WITH KEY fieldname = zif_al30_data=>cs_fix_field_conf-sel_screen.
    IF sy-subrc NE 0.
      INSERT VALUE #( fieldname = zif_al30_data=>cs_fix_field_conf-sel_screen
                      style = lv_style )
                      INTO TABLE <ls_fields>-celltab.
    ELSE.
      <ls_celltab>-style = lv_style.
    ENDIF.

    " Campo obligatorio
    READ TABLE <ls_fields>-celltab  ASSIGNING <ls_celltab>
                                    WITH KEY fieldname = zif_al30_data=>cs_fix_field_conf-mandatory.
    IF sy-subrc NE 0.
      INSERT VALUE #( fieldname = zif_al30_data=>cs_fix_field_conf-mandatory
                      style = lv_style )
                      INTO TABLE <ls_fields>-celltab.
    ELSE.
      <ls_celltab>-style = lv_style.
    ENDIF.
    " Campo salida
    READ TABLE <ls_fields>-celltab  ASSIGNING <ls_celltab>
                                    WITH KEY fieldname = zif_al30_data=>cs_fix_field_conf-no_output.
    IF sy-subrc NE 0.
      INSERT VALUE #( fieldname = zif_al30_data=>cs_fix_field_conf-no_output
                      style = lv_style )
                      INTO TABLE <ls_fields>-celltab.
    ELSE.
      <ls_celltab>-style = lv_style.
    ENDIF.

    " Origen del texto
    READ TABLE <ls_fields>-celltab  ASSIGNING <ls_celltab>
                                    WITH KEY fieldname = zif_al30_data=>cs_fix_field_conf-source_text.
    IF sy-subrc NE 0.
      INSERT VALUE #( fieldname = zif_al30_data=>cs_fix_field_conf-source_text
                      style = lv_style )
                      INTO TABLE <ls_fields>-celltab.
    ELSE.
      <ls_celltab>-style = lv_style.
    ENDIF.

    " Checkbox
    READ TABLE <ls_fields>-celltab  ASSIGNING <ls_celltab>
                                    WITH KEY fieldname = zif_al30_data=>cs_fix_field_conf-checkbox.
    IF sy-subrc NE 0.
      INSERT VALUE #( fieldname = zif_al30_data=>cs_fix_field_conf-checkbox
                      style = lv_style )
                      INTO TABLE <ls_fields>-celltab.
    ELSE.
      <ls_celltab>-style = lv_style.
    ENDIF.
  ENDLOOP.

ENDFORM.
