*----------------------------------------------------------------------*
***INCLUDE ZAL30_MAIN_VIEW_F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F4_VIEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f4_view .
  DATA lo_excep TYPE REF TO zcx_al30.
  DATA ld_view TYPE tabname.

  TRY.
      CALL METHOD zcl_al30_util=>f4_view
        EXPORTING
          iv_program     = sy-repid
          iv_dynpro      = sy-dynnr
          iv_dynprofield = 'P_VIEW'.

    CATCH zcx_al30 INTO lo_excep.
      MESSAGE s018.
  ENDTRY.
ENDFORM.                                                    " F4_VIEW
*&---------------------------------------------------------------------*
*&      Form  VALIDACION_PANTALLA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM chequeo_vista.
  DATA ls_return TYPE bapiret2.
  DATA ld_diff TYPE sap_bool.
  DATA ld_auth TYPE sap_bool.



* Verifico que la vista/tabla sea correcta
  CALL METHOD mo_controller->check_view
    EXPORTING
      iv_name_view = zal30_t_view-tabname
      iv_operation = zif_al30_data=>cv_operation_read
    IMPORTING
      es_return    = ls_return.

  IF ls_return-type = zif_al30_data=>cs_msg_type-error.
    " Antes de dar el mensaje de error se mira si la tabla que se pide es la que gestiona la autorización
    " de los usuarios. Si es así, lo que haré es crearla de manera automática.
    IF zal30_t_view-tabname = zif_al30_data=>cs_internal_tables-auth_user.
      PERFORM create_view_user_auth CHANGING ls_return.
    ENDIF.
    IF ls_return IS NOT INITIAL.
      MESSAGE ID ls_return-id TYPE ls_return-type
              NUMBER ls_return-number
              WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.
    ENDIF.
  ENDIF.

  " Se lanza el chequeo autorización de la vista
  PERFORM check_autorization CHANGING ld_auth mv_mode.

  IF ld_auth = abap_false.
    MESSAGE e041.
  ELSE.
    " Si tiene autorizacion pero para visualizar se informa
    IF mv_mode = zif_al30_data=>cv_mode_view.
      MESSAGE s042.
    ENDIF.
  ENDIF.

* Lectura de los campos de la vista
  PERFORM read_view.

* Miro que no existan diferencias entre la configuracion de la vista y el diccionario.
* Si la vista no tiene el autoajuste automático no se puede continuar con la edición
  mo_controller->check_changes_dict_view( EXPORTING is_view = ms_view
                                          IMPORTING ev_diff_fields = DATA(lv_diff_fields)
                                                    ev_diff_text = DATA(lv_diff_text) ).
  IF lv_diff_fields = abap_true OR lv_diff_text = abap_true.
    IF mo_controller->view_have_auto_adjust( ms_view-tabname ) = abap_true.
      mo_controller->auto_adjust_view_ddic(
        EXPORTING
          iv_name_view = ms_view-tabname
        IMPORTING
          es_return    = ls_return
          et_fields_view_alv = mt_fields
          et_fields_text_view_alv = mt_fields_text
          es_view = ms_view ).

      " Solo si hay errores en el ajuste
      IF ls_return-type = zif_al30_data=>cs_msg_type-error.
        MESSAGE ID ls_return-id TYPE ls_return-type
                     NUMBER ls_return-number
                     WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.
      ENDIF.
    ELSE.
      MESSAGE e024.
    ENDIF.
  ENDIF.

* Se instancia la clase que gestionará las exit
  IF ms_view-exit_class IS NOT INITIAL.
    mo_controller->instance_exit_class( iv_exit_class = ms_view-exit_class ).
  ENDIF.

* Se pasan los campos y vista a la clase de la vista para que los use
  mo_controller->set_data_conf_view(
    EXPORTING
      it_fields_view_alv      = mt_fields
      it_fields_text_view_alv =  mt_fields_text
      is_view                 = ms_view
      it_fields_ddic = mt_fields_ddic  ).

ENDFORM.                    " VALIDACION_PANTALLA
*&---------------------------------------------------------------------*
*&      Form  INICIALIZACION_DATOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM inicializacion_datos .

* Indico que los refrescos de los ALV no se moverán de filas y columna
  ms_stable-row = abap_true.
  ms_stable-col = abap_true.

* Por defecto los datos son válidos
  mv_datos_validos = abap_true.

ENDFORM.                    " INICIALIZACION_DATOS
*&---------------------------------------------------------------------*
*&      Form  INICIALIZACION_PROG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM inicializacion_prog .
* Creo el objeto que orquestará todas las operaciones
  IF mo_controller IS NOT BOUND.
    CREATE OBJECT mo_controller.
  ENDIF.

* Se informa el lenguaje de visualización
  IF mv_lang_vis IS INITIAL.
    mv_lang_vis = sy-langu.
  ENDIF.

* Indico que los refrescos de los ALV no se moverán de filas y columna
  ms_stable-row = abap_true.
  ms_stable-col = abap_true.

* Transacción original que se llama
  IF ms_conf_screen-origin_tcode IS INITIAL.
    ms_conf_screen-origin_tcode = cl_dynpro=>get_current_transaction( ).
    " Se busca la descripción de la transacción original
    SELECT SINGLE ttext INTO ms_conf_screen-origin_tcode_text
           FROM tstct
           WHERE sprsl = sy-langu
                 AND tcode = ms_conf_screen-origin_tcode.
  ENDIF.

ENDFORM.                    " INICIALIZACION_PROG
*&---------------------------------------------------------------------*
*&      Form  CATALOGO_CAMPOS_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM catalogo_campos_alv .
  DATA ls_return TYPE bapiret2.

* El catalogo de campos como la tabla interna de datos tendrá campos adicionales
* para la edicion.
  CALL METHOD mo_controller->get_fieldcat_view
    EXPORTING
      iv_mode     = mv_mode
    IMPORTING
      es_return   = ls_return
      et_fieldcat = mt_fieldcat.

* Si hay mensaje lo saco tal cual, aunque sea de error.
  IF ls_return-type IS NOT INITIAL.
    MESSAGE ID ls_return-id TYPE ls_return-type
                NUMBER ls_return-number
                WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.

  ENDIF.

ENDFORM.                    " CATALOGO_CAMPOS_ALV
*&---------------------------------------------------------------------*
*&      Form  READ_DATA_VIEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_data_view .
  DATA ls_filter TYPE zif_al30_data=>ts_filter_read_data.

  CLEAR <it_datos>.


  " Se lee los filtros de la tabla. No todas las tablas tendrán filtros porque depende
  " de su configuración.
  READ TABLE ms_conf_screen-sel_screen ASSIGNING FIELD-SYMBOL(<ls_sel_screen>)
                                       WITH KEY tabname = ms_view-tabname.
  IF sy-subrc = 0.
    ls_filter-fields_ranges = <ls_sel_screen>-fields_ranges.
    ls_filter-where_clauses = <ls_sel_screen>-where_clauses.
    ls_filter-expressions = <ls_sel_screen>-expressions.
  ENDIF.

  CALL METHOD mo_controller->read_data
    EXPORTING
      is_filters = ls_filter
    IMPORTING
      es_return  = DATA(ls_return)
    CHANGING
      co_data    = mo_datos.

* Si hay un mensaje de error lo devuelvo como warning para que no salga del programa,
* pero que se vea bien.
  IF ls_return-type = 'E'.
    MESSAGE ID ls_return-id TYPE 'W'
              NUMBER ls_return-number
              WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.
  ELSE.

* Si un mensaje que no es de error lo saco para que se vea. Ya que podria ser el de las autorizaciones.
    IF ls_return IS NOT INITIAL.
      MESSAGE ID ls_return-id TYPE ls_return-type
                    NUMBER ls_return-number
                    WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.
    ENDIF.


* En el modo edición recorro los datos para hacer lo siguiente se hace una ajuste de los campo segun sus valores.
    IF mv_mode = zif_al30_data=>cv_mode_change.

      PERFORM adjust_data_post_read.

    ENDIF.
  ENDIF.


ENDFORM.                    " READ_DATA_VIEW
*&---------------------------------------------------------------------*
*&      Form  CLEAN_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM clean_data .
  CLEAR: mv_mode, ms_view, mt_fields, <it_datos>, mv_text_view.

  IF mo_alv IS BOUND.
    mo_alv->free( ).
    mo_container->free( ).
    FREE: mo_alv, mo_container.
  ENDIF.
ENDFORM.                    " CLEAN_DATA
*&---------------------------------------------------------------------*
*&      Form  CREATE_IT_DATA_VIEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_it_data_view .

  CALL METHOD mo_controller->create_it_data_view
    EXPORTING
      iv_mode   = mv_mode
    IMPORTING
      et_data   = mo_datos
      es_return = DATA(ls_return).

* Si no hay error al crear la tabla de datos y estoy en modo edicion, creo una
* replica que será la que guarde los registros que se borren
  IF ls_return-type NE zif_al30_data=>cs_msg_type-error AND mv_mode = zif_al30_data=>cv_mode_change.
    CALL METHOD mo_controller->create_it_data_view
      EXPORTING
        iv_mode   = mv_mode
      IMPORTING
        et_data   = mo_datos_del
        es_return = ls_return.
    IF ls_return-type NE zif_al30_data=>cs_msg_type-error.
      ASSIGN mo_datos_del->* TO <it_datos_del>.
    ENDIF.
  ENDIF.


  IF ls_return-type IS NOT INITIAL.
    MESSAGE ID ls_return-id TYPE ls_return-type
                NUMBER ls_return-number
                WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.
  ELSE.

    ASSIGN mo_datos->* TO <it_datos>.

  ENDIF.

ENDFORM.                    " CREATE_IT_DATA_VIEW
*&---------------------------------------------------------------------*
*&      Form  CREATE_DATA_VIEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_data_view .

* Si la tabla tiene la opcion de grabar de entradas y el sistema permite transportar
* entonces se solicitará orden de tranporte.
  IF ms_view-transport = abap_true AND mo_controller->allowed_transport( ) = abap_true.
    mv_pedir_orden = abap_true.
  ELSE.
    mv_pedir_orden = abap_false.
  ENDIF.

* Si hay tabla de textos me quedo con el nombre del campo de idioma
  IF ms_view-texttable IS NOT INITIAL.
    READ TABLE mt_fields ASSIGNING FIELD-SYMBOL(<ls_fields>) WITH KEY lang_texttable = abap_true.
    IF sy-subrc = 0.
      mv_field_lang_textable = <ls_fields>-fieldname.
    ENDIF.
  ENDIF.

* Se bloquea la tabla para que no se pueda editar a la vez. Si no se puede bloquear se visualizarán los datos
  PERFORM lock_view.

* Creo la tabla interna de datos
  PERFORM create_it_data_view.

* Leo el catalogo de campos
  PERFORM catalogo_campos_alv.

ENDFORM.                    " CREATE_DATA_VIEW
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM data_changed  CHANGING ps_data_changed TYPE REF TO cl_alv_changed_data_protocol
                             ps_onf4
                             ps_onf4_before
                             ps_onf4_after
                             ps_ucomm.

  FIELD-SYMBOLS <ls_rows> TYPE lvc_s_moce.

  DATA ls_control_data TYPE ts_control_data.
  DATA ld_tabix_ddic TYPE sytabix.


* Inicialmente los datos son correctos.
  mv_datos_validos = abap_true.

* Líneas borradas
* Solo se borran las líneas que provienen del diccionario y que estarán
* en la tabla <it_datos_del>. De esta manera se simplifica el tratamiento
  LOOP AT ps_data_changed->mt_deleted_rows ASSIGNING <ls_rows>.

    PERFORM row_delete USING <ls_rows>
                       CHANGING ps_data_changed.

  ENDLOOP.

* Líneas moificadas/insertadas
* Esta es la parte más compleja porque en "mp_mod_rows" están las líneas
* tanto insertadas como borradas. Y pueden modificar líneas que han sido previamente
* insertadas
  PERFORM row_insert_modify CHANGING ps_data_changed
                                     mv_datos_validos.

  " Si hay mensajes de error los datos no serán validos
  READ TABLE ps_data_changed->mt_protocol TRANSPORTING NO FIELDS WITH KEY msgty = zif_al30_data=>cs_msg_type-error.
  IF sy-subrc = 0.
    mv_datos_validos = abap_false.
  ENDIF.

ENDFORM.                    " DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  ROW_DELETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM row_delete  USING pe_rows TYPE lvc_s_moce
                 CHANGING ps_data_changed TYPE REF TO cl_alv_changed_data_protocol.

  FIELD-SYMBOLS <ls_wa> TYPE any.
  FIELD-SYMBOLS <field> TYPE any.
  DATA ld_tabix TYPE sytabix.
  DATA ld_fieldname TYPE fieldname.

* Me posicino en el registro marcado para borrar
  READ TABLE <it_datos> ASSIGNING <ls_wa> INDEX pe_rows-row_id.
  IF sy-subrc = 0.

* Miro si el registro viene del diccionario
    ASSIGN COMPONENT zif_al30_data=>cs_control_fields_alv_data-is_dict
                     OF STRUCTURE <ls_wa> TO <field>.
    IF sy-subrc = 0.
* Si viene del diccionario paso el registro a otra tabla.
      IF <field> = abap_true.

* Pongo el indicador de registro borrado, para tenerlo en cuenta en procesos posteriores.
        ASSIGN COMPONENT zif_al30_data=>cs_control_fields_alv_data-updkz
                             OF STRUCTURE <ls_wa> TO <field>.
        IF sy-subrc = 0.
          <field> = zif_al30_data=>cv_mode_delete.
          APPEND <ls_wa> TO <it_datos_del>.
        ENDIF.

      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " ROW_DELETE
*&---------------------------------------------------------------------*
*&      Form  ROW_INSERT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM row_insert USING pe_modif TYPE lvc_s_modi
                      pe_datos_im TYPE any
                CHANGING ps_data_changed TYPE REF TO cl_alv_changed_data_protocol.

  FIELD-SYMBOLS <ls_fieldcat> TYPE lvc_s_fcat.
  FIELD-SYMBOLS <lt_style> TYPE lvc_t_styl.

* Lo primero que hago es habilitar los campos clave, ya que estos datos no éstán
* guardados todavía en la base de datos
  LOOP AT mt_fieldcat ASSIGNING <ls_fieldcat> WHERE key = abap_true.

    " Los estilos se modificarán directamente en la tabla porque al final de proceso se aplican los estilos
    " que hayan podido ser modificados en las exit o procesos estándar
    ASSIGN COMPONENT zif_al30_data=>cs_control_fields_alv_data-style OF STRUCTURE pe_datos_im TO <lt_style>.
    IF sy-subrc = 0.
      READ TABLE <lt_style> ASSIGNING FIELD-SYMBOL(<ls_style>) WITH KEY fieldname = <ls_fieldcat>-fieldname.
      IF sy-subrc = 0.
        <ls_style>-style = cl_gui_alv_grid=>mc_style_enabled.
      ELSE.
        INSERT VALUE #( fieldname = <ls_fieldcat>-fieldname style = cl_gui_alv_grid=>mc_style_enabled ) INTO TABLE <lt_style>.
      ENDIF.
      " Se comenta porque si luego se cambia los estilos del mismo campo solo tiene en cuenta el primer registro del campo.
*    CALL METHOD ps_data_changed->modify_style
*      EXPORTING
*        i_row_id    = pe_modif-row_id
*        i_fieldname = <ls_fieldcat>-fieldname
*        i_style     = cl_gui_alv_grid=>mc_style_enabled.
    ENDIF.

* Si el tipo de datos es mandante informo del mismo (necesario para el transporte).

    IF <ls_fieldcat>-datatype = zif_al30_data=>cs_datatype-mandt.
      CALL METHOD ps_data_changed->modify_cell
        EXPORTING
          i_row_id    = pe_modif-row_id
          i_fieldname = <ls_fieldcat>-fieldname
          i_value     = sy-mandt.

      " Se informa el campo de idioma de la tabla de texto
    ELSEIF <ls_fieldcat>-fieldname = mv_field_lang_textable AND mv_field_lang_textable IS NOT INITIAL.
      CALL METHOD ps_data_changed->modify_cell
        EXPORTING
          i_row_id    = pe_modif-row_id
          i_fieldname = <ls_fieldcat>-fieldname
          i_value     = sy-langu.
    ENDIF.

  ENDLOOP.

  " Pongo que esa línea es insertada
  CALL METHOD ps_data_changed->modify_cell
    EXPORTING
      i_row_id    = pe_modif-row_id
      i_fieldname = zif_al30_data=>cs_control_fields_alv_data-updkz
      i_value     = zif_al30_data=>cv_mode_insert.

  " Pongo el número de linea
  CALL METHOD ps_data_changed->modify_cell
    EXPORTING
      i_row_id    = pe_modif-row_id
      i_fieldname = zif_al30_data=>cs_control_fields_alv_data-tabix
      i_value     = pe_modif-row_id.

ENDFORM.                    " ROW_INSERT
*&---------------------------------------------------------------------*
*&      Form  ROW_INSERT_MODIFY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM row_insert_modify  CHANGING ps_data_changed TYPE REF TO cl_alv_changed_data_protocol
                                 ps_datos_validos.
* Tabla con datos insertados o modificados
  FIELD-SYMBOLS <lt_datos_im> TYPE table.
  FIELD-SYMBOLS <ls_datos_im> TYPE any.
  FIELD-SYMBOLS <ls_modif> TYPE lvc_s_modi.
*  DATA ld_count TYPE sytabix.

* Recupero los datos modificados o insertados
  ASSIGN ps_data_changed->mp_mod_rows->* TO <lt_datos_im>.

  IF sy-subrc = 0.

    " Las celdas se pasan a una tabla local. El motivo es que en procedimiento verify_change_row_data se pasan los datos modificados al listado.
    " Si se esta insertando una nueva línea y hay un campo que se no se informa nada en el momento de pasarlo al listado, provoca que se añade una nueva
    " línea al mt_mod_cells volviendo a procesar el nuevo campo y provocando un bucle infinito, porque se añade una y otra vez.
    DATA(lt_mod_cells) = ps_data_changed->mt_mod_cells.
    LOOP AT lt_mod_cells ASSIGNING <ls_modif>.

** El row_id que se inserte o modifica no coincide con el regustro en la tabla <LT_DATOS_IM>,
** aunque los registros siempre están consecutivos. Con lo que si tenemos dos row_id: 5 y 6
** sabremos que el 5 están la linea 1 de la tabla de registros modificados. Y la seis en la dos.
** Por ello tengo un contador propio para saber el registros en la tabla de datos
      AT NEW row_id.
*        ADD 1 TO ld_count.

* Leo el registro modificado/insertado
        READ TABLE <lt_datos_im> ASSIGNING <ls_datos_im> INDEX <ls_modif>-tabix.

* Trato la linea para ver si se ha modificado o insertado
        PERFORM trat_new_row_id USING
                                      "ld_count
                                      <ls_modif>
                                      <ls_datos_im>
                                CHANGING ps_data_changed.

* Compruebo que los camps claves  que se haya introducido/modificado no esten introducido
        PERFORM check_key_duplicate USING <ls_datos_im>
                                          <ls_modif>
                                    CHANGING ps_data_changed
                                             ps_datos_validos.

      ENDAT.

      PERFORM verify_content_fields USING <ls_modif>
                                    CHANGING ps_data_changed
                                             ps_datos_validos.

* Al final de cada registro se lanza la verificacion a nivel de fila
      AT END OF row_id.
        PERFORM verify_change_row_data USING <ls_modif>
                                      <ls_datos_im>
                                CHANGING ps_data_changed
                                         ps_datos_validos.
      ENDAT.

    ENDLOOP.


  ENDIF.

ENDFORM.                    " ROW_INSERT_MODIFY
*&---------------------------------------------------------------------*
*&      Form  ROW_MODIFY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM row_modify  USING pe_modif TYPE lvc_s_modi
                       pe_datos_im TYPE any
                CHANGING ps_data_changed TYPE REF TO cl_alv_changed_data_protocol.


* Miro el valor del campo que indica el tipo de actualización
  ASSIGN COMPONENT zif_al30_data=>cs_control_fields_alv_data-updkz OF STRUCTURE pe_datos_im TO FIELD-SYMBOL(<field>).
  IF sy-subrc = 0.

* Si el registro no ha sido insertado previamente pongo el status a modificado.
    IF <field> NE zif_al30_data=>cv_mode_insert.

* Y informo la tabla de datos
      CALL METHOD ps_data_changed->modify_cell
        EXPORTING
          i_row_id    = pe_modif-row_id
          i_fieldname = zif_al30_data=>cs_control_fields_alv_data-updkz
          i_value     = zif_al30_data=>cv_mode_change.

    ENDIF.

  ENDIF.

ENDFORM.                    " ROW_MODIFY
*&---------------------------------------------------------------------*
*&      Form  TRAT_NEW_ROW_ID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM trat_new_row_id  USING  "pe_count TYPE sytabix
                             pe_modif TYPE lvc_s_modi
                             pe_datos_im TYPE any
                      CHANGING ps_data_changed TYPE REF TO cl_alv_changed_data_protocol.

* Lo primero que averiguo si es una inserción u modificación. Para saberlo miro
* Si el registro están en la tabla "mt_inserted_rows", si lo esta entonces se esta insertando.
  READ TABLE ps_data_changed->mt_inserted_rows
       TRANSPORTING NO FIELDS
       WITH KEY row_id = pe_modif-row_id.

  IF sy-subrc = 0.

* Hago las operaciones cuando una linea se inserta
    PERFORM row_insert USING pe_modif
                             pe_datos_im
           CHANGING ps_data_changed.

  ELSE.

* Si no esta en la tabla es que se esta insertando
    PERFORM row_modify USING pe_modif
                             pe_datos_im
                     CHANGING ps_data_changed.

  ENDIF.

ENDFORM.                    " TRAT_NEW_ROW_ID
*&---------------------------------------------------------------------*
*&      Form  CHECK_KEY_DUPLICATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_key_duplicate  USING    pe_datos_im TYPE any
                                   pe_modif TYPE lvc_s_modi
                          CHANGING ps_data_changed TYPE REF TO cl_alv_changed_data_protocol
                                   ps_datos_validos TYPE sap_bool.

  FIELD-SYMBOLS <ls_datos> TYPE any.

  DATA ld_valor TYPE string.
  DATA ld_found TYPE sap_bool.
  DATA lv_cond TYPE string.

* Primero miro si el registro viene del diccionario. En caso afirmativo no compruebo
* clave duplicada ya que no la pueden modificar
  ASSIGN COMPONENT zif_al30_data=>cs_control_fields_alv_data-is_dict OF STRUCTURE pe_datos_im TO FIELD-SYMBOL(<field>).
  IF sy-subrc = 0.

    IF <field> = abap_false.
* No leo los campos claves que están marcados como técnicos y que además pertenezcan a la tabla de textos(como el idioma), ya que no se van a introducir.
      LOOP AT mt_fields ASSIGNING FIELD-SYMBOL(<ls_fields>) WHERE tech = abap_false
                                                      AND key_ddic = abap_true
                                                      AND field_texttable = abap_false.

        ASSIGN COMPONENT <ls_fields>-fieldname OF STRUCTURE pe_datos_im TO <field>.
        IF sy-subrc = 0.

          lv_cond = COND string( LET sep = 'AND' cond = |{ <ls_fields>-fieldname } = '{ <field> }'| IN WHEN lv_cond IS INITIAL THEN cond ELSE |{ lv_cond } { sep } { cond }| ).

        ENDIF.

      ENDLOOP.
* Si no hay campos clave no tiene sentido hacer verificaciones.
      IF sy-subrc = 0.

* Añado la condición de no busque los registros previamente borrados
        lv_cond = COND string( LET sep1 = 'AND' cond = |{ zif_al30_data=>cs_control_fields_alv_data-updkz } NE '{ zif_al30_data=>cv_mode_delete }'| IN WHEN lv_cond IS INITIAL THEN cond ELSE |{ lv_cond } { sep1 } { cond }| ).

* Como los cambios se realizan fuera del evento DATA_CHANGED en la tabla
* <IT_DATOS> tengo los valores antiguos y por lo que nunca se encontrará "a si mismo" el registro
* modificado. La excepcion son lo que vienen del diccionario. Cuya clave no se puede cambiar y en ese
* caso me encontrará el mismo registro.
* Lo que si que hay que controlar es la posición del registro sea distinta a la del registro
* que el ALV me dice que esta modificado. Si es el mismo significa que estoy en el mismo.
* Esto habitualmente no pasaria salvo cuando hay un campo obligatorio que no esta informado, y segun
* su configuración deberia estarlo. En ese caso, saco un mensaje de error a través del DATA_CHANGED, con
* lo cual si se pulsa un botón o algo, cuando entra al data_changed este registro SAP lo tiene como
* modificado, aunque no se haya modificado ningún valor. Lo tiene modificado porque le he indicado que habia
* un error en ese registro.

        ld_found = abap_false.
        LOOP AT <it_datos> ASSIGNING <ls_datos> WHERE (lv_cond).
          IF sy-tabix NE pe_modif-row_id.
            ld_found = abap_true.
          ENDIF.
          EXIT.
        ENDLOOP.
        IF ld_found = abap_true.
          ps_datos_validos = abap_false.
          CALL METHOD ps_data_changed->add_protocol_entry
            EXPORTING
              i_msgid     = zif_al30_data=>cv_msg_id
              i_msgno     = '043'
              i_msgty     = zif_al30_data=>cs_msg_type-error
              i_fieldname = <ls_fields>-fieldname " Se escoge el último campo de la clave
              i_row_id    = pe_modif-row_id
              i_tabix     = pe_modif-tabix.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_KEY_DUPLICATE
*&---------------------------------------------------------------------*
*&      Form  GRABAR_DATOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM grabar_datos .

  " Si se puede pedir orden de transporte se llama al método que validar y/o seleccionará la orden de transporter
  IF mv_pedir_orden = abap_true.
    mo_controller->check_select_transport_order( EXPORTING iv_category = zif_al30_data=>cs_order_category-workbench
                                               IMPORTING es_return = DATA(ls_return)
                                               CHANGING cv_order = mv_orden_transporte ).
  ENDIF.

  " Si no hay errores y hay orden de transporte, se hace la grabación
  IF ls_return IS INITIAL AND ( mv_pedir_orden = abap_false OR ( mv_orden_transporte IS NOT INITIAL AND mv_pedir_orden = abap_true ) ) .

    CALL METHOD mo_controller->save_data
      EXPORTING
        iv_allow_request = mv_pedir_orden
      IMPORTING
        et_return        = DATA(lt_return)
      CHANGING
        ct_datos_del     = <it_datos_del>
        ct_datos         = <it_datos>
        cv_order         = mv_orden_transporte.

    " Si hay mensajes se mira cuantos hay. Si solo hay uno se visualiza como mensaje directamente
    " en caso contrario se mostrará los mensajes en una ventana
    IF lt_return IS NOT INITIAL.
      DESCRIBE TABLE lt_return.
      IF sy-tfill = 1 .
        READ TABLE lt_return REFERENCE INTO DATA(lo_return) INDEX 1.
        " El mensaje se cambia a 'S' y se muestra como el tipo original
        MESSAGE ID lo_return->id TYPE 'S'
                    NUMBER lo_return->number
                    WITH lo_return->message_v1 lo_return->message_v2 lo_return->message_v3 lo_return->message_v4
                    DISPLAY LIKE lo_return->type.
      ELSE.
        PERFORM show_messages USING lt_return.
      ENDIF.
    ENDIF.

    " Si no hay error se hace los mismos ajustes que se hacen al leer los datos.
    IF NOT line_exists( lt_return[ type = zif_al30_data=>cs_msg_type-error ] ).
      PERFORM adjust_data_post_read.
    ENDIF.

  ELSE.
    MESSAGE ID ls_return-id TYPE ls_return-type
                    NUMBER ls_return-number
                    WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.
  ENDIF.

ENDFORM.                    " GRABAR_DATOS
*&---------------------------------------------------------------------*
*&      Form  VERIFY_CONTENT_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM verify_content_fields  USING  pe_modif TYPE lvc_s_modi
                          CHANGING ps_data_changed TYPE REF TO cl_alv_changed_data_protocol
                                   ps_datos_validos TYPE sap_bool.
  DATA ls_return TYPE bapiret2.

  mo_controller->verify_field_data( EXPORTING iv_fieldname  = pe_modif-fieldname
                                              iv_value      = pe_modif-value
                                    IMPORTING es_return = ls_return ).

  IF ls_return IS NOT INITIAL.

    CALL METHOD ps_data_changed->add_protocol_entry
      EXPORTING
        i_msgid     = ls_return-id
        i_msgno     = ls_return-number
        i_msgty     = ls_return-type
        i_fieldname = pe_modif-fieldname
        i_row_id    = pe_modif-row_id
        i_tabix     = pe_modif-tabix.
  ENDIF.

  " Si el mensaje es errónea se marca que los datos no son validos.
  IF ls_return-type = zif_al30_data=>cs_msg_type-error.
    ps_datos_validos = abap_false.
  ENDIF.

ENDFORM.                    " VERIFY_CONTENT_FIELDS
*&---------------------------------------------------------------------*
*&      Form  CHECK_AUTORIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_autorization CHANGING ps_auth TYPE sap_bool
                                 ps_mode TYPE char1.


  ps_auth = abap_true. " Por defecto se tiene autorizacion
  ps_mode = zif_al30_data=>cv_mode_change. " Puede modificar

  DATA(lv_level_auth) = mo_controller->check_authorization_view( iv_view_name   = zal30_t_view-tabname
                                           iv_view_action = COND #( WHEN ps_mode = zif_al30_data=>cv_mode_change THEN zif_al30_data=>cs_action_auth-update ELSE zif_al30_data=>cs_action_auth-show )
                                           iv_user        = sy-uname ).

  " Segun el nivel de autorización cambiará que es lo que puede hacer.
  CASE lv_level_auth.
    WHEN zif_al30_data=>cs_level_auth_user-non.
      ps_auth = abap_false.
    WHEN zif_al30_data=>cs_level_auth_user-read.
      ps_mode = zif_al30_data=>cv_mode_view.
    WHEN zif_al30_data=>cs_level_auth_user-full.
      " Nada, valores por defecto
  ENDCASE.


** Se mira si tiene control de autorización por usuario
*  IF mo_controller->view_have_user_auth( zal30_t_view-tabname ) = abap_true.
*    DATA(lv_level_auth) = mo_controller->get_level_auth_view( iv_user = sy-uname iv_view = ms_view-tabname ).
*
*    " Segun el nivel de autorización cambiará que es lo que puede hacer.
*    CASE lv_level_auth.
*      WHEN zif_al30_data=>cs_level_auth_user-non.
*        ps_auth = abap_false.
*      WHEN zif_al30_data=>cs_level_auth_user-read.
*        ps_mode = zif_al30_data=>cv_mode_view.
*      WHEN zif_al30_data=>cs_level_auth_user-full.
*        " Nada, valores por defecto
*    ENDCASE.
*  ENDIF.
*
*  IF ps_auth = abap_true.
*    IF mo_controller->view_have_sap_auth( zal30_t_view-tabname ).
*
*      TRY.
*          CALL METHOD mo_controller->check_sap_authorization
*            EXPORTING
*              iv_view_name   = zal30_t_view-tabname
*              iv_view_action = COND #( WHEN ps_mode = zif_al30_data=>cv_mode_change THEN zif_al30_data=>cs_action_auth-update ELSE zif_al30_data=>cs_action_auth-show ).
*
*        CATCH zcx_al30 .
*          ps_auth = abap_false.
*      ENDTRY.
*
*    ENDIF.
*
*  ENDIF.

ENDFORM.                    " CHECK_AUTORIZATION
*&---------------------------------------------------------------------*
*&      Form  EDIT_MODE_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM edit_mode_alv .
  DATA lv_mode TYPE int4.
  IF mo_alv IS BOUND.

    CASE mv_mode.
      WHEN zif_al30_data=>cv_mode_change.

        lv_mode = 1.

        mo_controller->set_edit_mode_alv_data( EXPORTING it_data = <it_datos>
                                               IMPORTING ev_edit_mode = DATA(lv_edit_mode) ).

        " Si el modo de edición es visualizar se cambia el modo que se pasará al ALV. En caso contrario se indica el determinado
        " por defecto
        lv_mode = COND #( WHEN lv_edit_mode = zif_al30_data=>cv_mode_view THEN 0 ELSE lv_mode ).

      WHEN zif_al30_data=>cv_mode_view.
        lv_mode = 0.

    ENDCASE.

    CALL METHOD mo_alv->set_ready_for_input
      EXPORTING
        i_ready_for_input = lv_mode.

  ENDIF.

ENDFORM.                    " EDIT_MODE_ALV
*&---------------------------------------------------------------------*
*&      Form  TRANSPORTAR_DATOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM transportar_datos .
  FIELD-SYMBOLS <ls_index> TYPE lvc_s_row.
  FIELD-SYMBOLS <lt_datos> TYPE STANDARD TABLE.
  FIELD-SYMBOLS <field> TYPE any.
  DATA lt_index TYPE lvc_t_row.
  DATA lv_answer TYPE c.
  DATA lo_datos TYPE REF TO data.
  DATA ls_return TYPE bapiret2.

  CALL METHOD mo_alv->get_selected_rows
    IMPORTING
      et_index_rows = lt_index.

* Se ha de seleccionar una entrada para poder transportar
  IF lt_index IS NOT INITIAL.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar       = 'Transportation content table'(001)
        text_question  = 'Do you want to transport the selected table entries?'(002)
      IMPORTING
        answer         = lv_answer
      EXCEPTIONS
        text_not_found = 1
        OTHERS         = 2.

    IF lv_answer = '1'.

      " Se pide la orden a transportar, o se valida la última utilizada
      mo_controller->check_select_transport_order( EXPORTING iv_category = zif_al30_data=>cs_order_category-workbench
                                                   IMPORTING es_return = ls_return
                                                   CHANGING cv_order = mv_orden_transporte ).

      IF ls_return-type NE zif_al30_data=>cs_msg_type-error.

        " Se crea una tabla local para pasar los datos seleccionados
        CREATE DATA lo_datos LIKE <it_datos>.
        ASSIGN lo_datos->* TO <lt_datos>.

* Monto los campos clave de las entradas seleccionadas
        LOOP AT lt_index ASSIGNING <ls_index>.
          READ TABLE <it_datos> ASSIGNING FIELD-SYMBOL(<ls_datos>) INDEX <ls_index>-index.
          IF sy-subrc = 0.
            INSERT <ls_datos> INTO TABLE <lt_datos>.
          ENDIF.

        ENDLOOP.

* Llamo al proceso encargado que añadira las entradas a la tabla
        ls_return = mo_controller->transport_data_entries( EXPORTING it_data = <lt_datos>
                                                            CHANGING cv_order = mv_orden_transporte ).
        IF ls_return IS NOT INITIAL.
          MESSAGE ID ls_return-id TYPE ls_return-type
                     NUMBER ls_return-number
                     WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.
        ENDIF.

      ENDIF.

    ENDIF.

  ELSE.
    MESSAGE s044.
  ENDIF.

ENDFORM                    "transportar_datos
.                    " TRANSPORTAR_DATOS
*&---------------------------------------------------------------------*
*&      Form  VERIFY_CHANGE_ROW_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM verify_change_row_data USING  pe_modif TYPE lvc_s_modi
                            ps_row_data TYPE any
                     CHANGING ps_data_changed TYPE REF TO cl_alv_changed_data_protocol
                               ps_datos_validos TYPE sap_bool.
  FIELD-SYMBOLS <ls_row_data> TYPE any.
  FIELD-SYMBOLS <ls_fieldcat> TYPE lvc_s_fcat.
  FIELD-SYMBOLS <field> TYPE any.
  FIELD-SYMBOLS: <styles> TYPE lvc_t_styl.
  DATA lo_wa TYPE REF TO data.

* Creo una estructura identifica a la vista que se trata. * Creo una estructura identifica a los datos que se están tratando.
  CREATE DATA lo_wa LIKE LINE OF <it_datos>.

  IF lo_wa IS BOUND.
    ASSIGN lo_wa->* TO <ls_row_data>.

* Paso los datos
    MOVE-CORRESPONDING ps_row_data TO <ls_row_data>.

    CALL METHOD mo_controller->verify_change_row_data
      EXPORTING
        iv_row      = pe_modif-row_id
      IMPORTING
        et_return   = DATA(lt_return)
      CHANGING
        cs_row_data = <ls_row_data>.

* Si hay mensaje lo devuelvo a la clase
    IF lt_return IS NOT INITIAL.
      LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<ls_return>).
        CALL METHOD ps_data_changed->add_protocol_entry
          EXPORTING
            i_msgid     = <ls_return>-id
            i_msgno     = <ls_return>-number
            i_msgty     = <ls_return>-type
            i_msgv1     = <ls_return>-message_v1
            i_msgv2     = <ls_return>-message_v2
            i_msgv3     = <ls_return>-message_v3
            i_msgv4     = <ls_return>-message_v4
            i_fieldname = <ls_return>-field
            i_row_id    = pe_modif-row_id
            i_tabix     = pe_modif-row_id.

        " Si hay algún error se devuelve que los datos no son válidos
        ps_datos_validos = COND #( WHEN <ls_return>-type = zif_al30_data=>cs_msg_type-error THEN abap_false ELSE ps_datos_validos ).

      ENDLOOP.
    ENDIF.

* Paso los datos al listado
    LOOP AT mt_fieldcat ASSIGNING <ls_fieldcat>.

      ASSIGN COMPONENT <ls_fieldcat>-fieldname OF STRUCTURE <ls_row_data> TO <field>.
      IF sy-subrc = 0.
        " Si el campo de idioma de la tabla de texto esta vacio lo informo. Aunque en principio no debería suceder porque al
        " insertar registros se inserta de manera automática.
        IF <ls_fieldcat>-fieldname = mv_field_lang_textable AND mv_field_lang_textable IS NOT INITIAL AND <field> IS NOT INITIAL.
          CALL METHOD ps_data_changed->modify_cell
            EXPORTING
              i_row_id    = pe_modif-row_id
              i_fieldname = <ls_fieldcat>-fieldname
              i_value     = sy-langu.

        ELSE.
          CALL METHOD ps_data_changed->modify_cell
            EXPORTING
              i_row_id    = pe_modif-row_id
              i_fieldname = <ls_fieldcat>-fieldname
              i_value     = <field>.
        ENDIF.

      ENDIF.

    ENDLOOP.

* Ahora se actualiza los estilos por si se hubiesen modificado en la exit
    ASSIGN COMPONENT zif_al30_data=>cs_control_fields_alv_data-style OF STRUCTURE <ls_row_data> TO <styles>.
    IF sy-subrc = 0.
      LOOP AT <styles> ASSIGNING FIELD-SYMBOL(<style>).
        CALL METHOD ps_data_changed->modify_style
          EXPORTING
            i_row_id    = pe_modif-row_id
            i_fieldname = <style>-fieldname
            i_style     = <style>-style.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " VERIFY_CHANGE_ROW_DATA
*&---------------------------------------------------------------------*
*&      Form  CREATE_VIEW_USER_AUTH
*&---------------------------------------------------------------------*
FORM create_view_user_auth CHANGING ps_return TYPE bapiret2.


  DATA(ls_default_values) = VALUE zif_al30_data=>ts_default_values_create( auth_user = abap_false  change_log = abap_true auto_adjust = abap_true ).

  mo_controller->create_view(
    EXPORTING
      iv_name_view            = ms_view-tabname
      iv_use_default_values   = abap_true
      is_default_values       = ls_default_values
  IMPORTING
    es_return               = ps_return ).

  " Si no hay error limpio la estructura de mensajes
  IF ps_return-type NE zif_al30_data=>cs_msg_type-error.
    CLEAR ps_return.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  READ_VIEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_view .

  CLEAR: ms_view, mt_fields, mt_fields_text.

* Leo los datos de la vista
  CALL METHOD mo_controller->read_view_alv
    EXPORTING
      iv_name_view            = zal30_t_view-tabname
      iv_all_language         = abap_false
    IMPORTING
      ev_text_view            = mv_text_view
      es_view                 = ms_view
      et_fields_view_alv      = mt_fields
      et_fields_text_view_alv = mt_fields_text
      et_fields_ddic          = mt_fields_ddic.

  " Se ajuste el texto de cabecera de la tabla de campos por el valor de la tabla de textos
  PERFORM text_heading_fields.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  LOCK_VIEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM lock_view .

  " Solo se bloquea cuando se edite la tabla
  IF mv_mode = zif_al30_data=>cv_mode_change.
    TRY.

        mo_controller->lock_view( ).

      CATCH zcx_al30 INTO DATA(lx_excep).
        mv_mode = zif_al30_data=>cv_mode_view.
        MESSAGE lx_excep->mv_message TYPE 'S' .

    ENDTRY.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ADJUST_DATA_POST_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM adjust_data_post_read .
  FIELD-SYMBOLS <lt_celltab> TYPE lvc_t_styl.

  LOOP AT <it_datos> ASSIGNING FIELD-SYMBOL(<ls_wa>).

    ASSIGN COMPONENT zif_al30_data=>cs_control_fields_alv_data-style OF STRUCTURE <ls_wa> TO <lt_celltab>.
    IF sy-subrc = 0.
      " Los campos claves se marcarán como no editables.
      LOOP AT mt_fields ASSIGNING FIELD-SYMBOL(<ls_fields>) WHERE key_ddic = abap_true.
        READ TABLE <lt_celltab> ASSIGNING FIELD-SYMBOL(<ls_celltab>) WITH TABLE KEY fieldname = <ls_fields>-fieldname.
        IF sy-subrc NE 0.
          INSERT VALUE #( fieldname = <ls_fields>-fieldname style = cl_gui_alv_grid=>mc_style_disabled )  INTO TABLE <lt_celltab>.
        ELSE.
          <ls_celltab>-style = cl_gui_alv_grid=>mc_style_disabled.
        ENDIF.
      ENDLOOP.

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTION_SCREEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM selection_screen .
  DATA lt_opt_text TYPE tt_opt_selscreen_field_text.

  IF ms_view-tabname IS NOT INITIAL.

    " Se mira si para la tabla ya existe una configuración de pantalla de selección. Esto permite no crear
    " id de selección cada vez que se cambia la tabla. Si ya se ha creado en su momento se usa el mismo ID.
    " creo que esto evitará errores ya que esas pantalla se tienen que crear de manera dinámica
    READ TABLE ms_conf_screen-sel_screen ASSIGNING FIELD-SYMBOL(<ls_sel_screen>) WITH KEY tabname = ms_view-tabname.
    IF sy-subrc NE 0.
      APPEND INITIAL LINE TO ms_conf_screen-sel_screen ASSIGNING <ls_sel_screen>.
      <ls_sel_screen>-tabname = ms_view-tabname.

      " Se obtiene el tamaño del texto del campo donde se guardará el texto
      DATA(lv_text_len) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_name( zif_al30_data=>cs_selection_screen_view-data_element_text_field ) )->output_length.

      " Se construyen los campos que tienen marcado la opción de pantalla de selección
      " El tipo de campo será 'S' para indicar que es un select options
      LOOP AT mt_fields ASSIGNING FIELD-SYMBOL(<ls_fields>) WHERE sel_screen = abap_true.

        " La tabla cambia si el campo viene de la tabla de textos o de la tabla principal
        DATA(lv_tabname) = COND #( WHEN <ls_fields>-field_texttable = abap_true THEN ms_view-texttable ELSE <ls_fields>-tabname ).

        INSERT VALUE #( tablename = lv_tabname fieldname = <ls_fields>-fieldname
                        type = 'S' ) INTO TABLE <ls_sel_screen>-fields.

        " El texto se tomará el que tenga mayor longitud y no supere los 30 carácteres
        READ TABLE mt_fields_text ASSIGNING FIELD-SYMBOL(<ls_fields_text>)
                                  WITH KEY tabname = <ls_fields>-tabname
                                           fieldname = <ls_fields>-fieldname.
        IF sy-subrc = 0.
          " Se informan todos los textos en una tabla junto a su longitud, el que sea más largo y no supere el tamaño del campo ese será el escogido.
          " El motivo es simple, ya que cada uno puede tener el texto que quiera en elemento de datos o de manera manual. Y no siempre el texto más largo
          " estará en el campo donde debería. Ejemplo el elemento de datos XUBNAME tiene el mejor texto en la cabecera.
          CLEAR lt_opt_text.
          IF <ls_fields_text>-scrtext_l IS NOT INITIAL.
            INSERT VALUE #( text = <ls_fields_text>-scrtext_l len = strlen( <ls_fields_text>-scrtext_l ) ) INTO TABLE lt_opt_text.
          ENDIF.
          IF <ls_fields_text>-scrtext_m IS NOT INITIAL.
            INSERT VALUE #( text = <ls_fields_text>-scrtext_m len = strlen( <ls_fields_text>-scrtext_m ) ) INTO TABLE lt_opt_text.
          ENDIF.
          IF <ls_fields_text>-scrtext_s IS NOT INITIAL.
            INSERT VALUE #( text = <ls_fields_text>-scrtext_s len = strlen( <ls_fields_text>-scrtext_s ) ) INTO TABLE lt_opt_text.
          ENDIF.
          IF <ls_fields_text>-reptext IS NOT INITIAL.
            INSERT VALUE #( text = <ls_fields_text>-reptext len = strlen( <ls_fields_text>-reptext ) ) INTO TABLE lt_opt_text.
          ENDIF.
          " Debería haber textos porque sino el campo saldrá sin texto
          IF lt_opt_text IS NOT INITIAL.
            SORT lt_opt_text BY len DESCENDING. " Me quedo con la longitud más alta
            READ TABLE lt_opt_text ASSIGNING FIELD-SYMBOL(<ls_opt_text>) INDEX 1.

            INSERT VALUE #( tablename = lv_tabname fieldname = <ls_fields>-fieldname text = <ls_opt_text>-text )
                   INTO TABLE <ls_sel_screen>-fields_text.

          ELSE.
            INSERT VALUE #( tablename = lv_tabname fieldname = <ls_fields>-fieldname text = <ls_fields>-reptext ) INTO TABLE <ls_sel_screen>-fields_text.
          ENDIF.
        ELSE. " Si no hay texto se pone el texto del campo
          INSERT VALUE #( tablename = lv_tabname fieldname = <ls_fields>-fieldname text = <ls_fields>-reptext )
          INTO TABLE <ls_sel_screen>-fields_text.
        ENDIF.
      ENDLOOP.

    ENDIF.

    IF <ls_sel_screen>-selid IS INITIAL.

      CALL FUNCTION 'FREE_SELECTIONS_INIT'
        EXPORTING
          kind                     = 'F' " Los campos se pasan en el fichero en el parámetro FIELDS_TAB
        IMPORTING
          selection_id             = <ls_sel_screen>-selid
        TABLES
          fields_tab               = <ls_sel_screen>-fields[]
          field_texts              = <ls_sel_screen>-fields_text[]
          field_desc               = <ls_sel_screen>-fields_desc[]
        EXCEPTIONS
          fields_incomplete        = 01
          fields_no_join           = 02
          field_not_found          = 03
          no_tables                = 04
          table_not_found          = 05
          expression_not_supported = 06
          incorrect_expression     = 07
          illegal_kind             = 08
          area_not_found           = 09
          inconsistent_area        = 10
          kind_f_no_fields_left    = 11
          kind_f_no_fields         = 12
          too_many_fields          = 13
          dup_field                = 14
          field_no_type            = 15
          field_ill_type           = 16
          dup_event_field          = 17
          node_not_in_ldb          = 18
          area_no_field            = 19
          OTHERS                   = 20.

    ENDIF.

    IF <ls_sel_screen>-selid IS NOT INITIAL.
      CALL FUNCTION 'FREE_SELECTIONS_DIALOG'
        EXPORTING
          selection_id    = <ls_sel_screen>-selid
          tree_visible    = space
          as_subscreen    = abap_true
          no_frame        = abap_false
        IMPORTING
          where_clauses   = <ls_sel_screen>-where_clauses[]
          field_ranges    = <ls_sel_screen>-fields_ranges[]
          expressions     = <ls_sel_screen>-expressions[]
        TABLES
          fields_tab      = <ls_sel_screen>-fields[]
        EXCEPTIONS
          internal_error  = 1
          no_action       = 2
          selid_not_found = 3
          illegal_status  = 4
          OTHERS          = 5.

    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PROCESS_LOAD_VIEW_DYNPRO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM process_load_view_dynpro .

* Se comprueba la validez de la vista
  PERFORM chequeo_vista.

* Inicialización de datos
  PERFORM inicializacion_datos.

* Creacion de objetos de la vista
  PERFORM create_data_view.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form TEXT_HEADING_FIELDS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM text_heading_fields .
  " Actualizo el texto del campo segun el origen del texto del campo
  LOOP AT mt_fields ASSIGNING FIELD-SYMBOL(<ls_fields>).

    READ TABLE mt_fields_text ASSIGNING FIELD-SYMBOL(<ls_fields_text>)
                           WITH KEY fieldname = <ls_fields>-fieldname
                                    spras = mv_lang_vis.

    IF sy-subrc = 0.
      <ls_fields>-reptext = <ls_fields_text>-reptext.
    ENDIF.

    " Si no hay texto, se pone el nombre del campo
    IF <ls_fields>-reptext IS INITIAL.
      <ls_fields>-reptext = <ls_fields>-fieldname.
    ENDIF.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SHOW_MESSAGES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM show_messages  USING pe_return TYPE bapiret2_t.
  TYPES: BEGIN OF ts_message_list,
           semaphor TYPE c,
           message  TYPE bapi_msg,
         END OF ts_message_list.
  TYPES: tt_message_list TYPE STANDARD TABLE OF ts_message_list WITH EMPTY KEY.

  DATA(lt_message_list) = VALUE tt_message_list( FOR <wa> IN pe_return ( message = <wa>-message
                                                     semaphor = COND #( WHEN <wa>-type = zif_al30_data=>cs_msg_type-error
                                                                        THEN zif_al30_data=>cs_semaphor_alv_excep-error
                                                                        ELSE COND #( WHEN <wa>-type = zif_al30_data=>cs_msg_type-success
                                                                                     THEN zif_al30_data=>cs_semaphor_alv_excep-ok
                                                                                     ELSE COND #( WHEN <wa>-type = zif_al30_data=>cs_msg_type-warning
                                                                                                  THEN zif_al30_data=>cs_semaphor_alv_excep-warning ) ) ) ) ).

  TRY.
      cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_salv_table)
                              CHANGING  t_table      = lt_message_list ).

      IF lo_salv_table IS BOUND.

        lo_salv_table->get_columns( )->set_optimize( abap_true ).

        lo_salv_table->get_columns( )->set_exception_column( 'SEMAPHOR' ).

        lo_salv_table->set_screen_popup( start_column = '5'
                                         end_column   = '100'
                                         start_line   = '5'
                                         end_line     = '10' ).
        lo_salv_table->display( ).
      ENDIF.

    CATCH cx_salv_msg ##NO_HANDLER.
    CATCH cx_salv_not_found ##NO_HANDLER.
  ENDTRY.

ENDFORM.
