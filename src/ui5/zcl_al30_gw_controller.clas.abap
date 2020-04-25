CLASS zcl_al30_gw_controller DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Get the view list</p>
    "!
    "! @parameter iv_user | <p class="shorttext synchronized" >User</p>
    "! @parameter iv_langu | <p class="shorttext synchronized" >Language</p>
    "! @parameter it_r_views | <p class="shorttext synchronized" >Views to filter</p>
    "! @parameter et_view | <p class="shorttext synchronized" >View list</p>
    METHODS get_views
      IMPORTING
        !iv_user    TYPE string
        !iv_langu   TYPE sylangu DEFAULT sy-langu
        !it_r_views TYPE zif_al30_data=>tt_r_tabname OPTIONAL
      EXPORTING
        !et_views   TYPE zif_al30_ui5_data=>tt_view_list_auth.
    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    "!
    METHODS constructor.
    "! <p class="shorttext synchronized">Check the authorization level in view</p>
    "!
    "! @parameter iv_view_name | <p class="shorttext synchronized" >View name</p>
    "! @parameter iv_view_action | <p class="shorttext synchronized" l>'U' Update 'S' Show</p>
    "! @parameter iv_user | <p class="shorttext synchronized">Username</p>
    "! @parameter rv_level_auth | <p class="shorttext synchronized">Level auth</p>
    METHODS check_authorization_view
      IMPORTING
                !iv_view_name        TYPE tabname
                !iv_view_action      TYPE any OPTIONAL
                !iv_user             TYPE syuname OPTIONAL
      RETURNING VALUE(rv_level_auth) TYPE zal30_e_level_auth .
    "! <p class="shorttext synchronized">Read View</p>
    "!
    "! @parameter iv_view_name | <p class="shorttext synchronized">View name</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Language</p>
    "! @parameter et_fields | <p class="shorttext synchronized">Fields</p>
    METHODS read_view
      IMPORTING
        !iv_view_name TYPE tabname
        !iv_mode      TYPE char1 OPTIONAL
        !iv_langu     TYPE sylangu DEFAULT sy-langu
      EXPORTING
        !et_fields    TYPE zif_al30_ui5_data=>tt_view_fields.
    "! <p class="shorttext synchronized">Read data from view</p>
    "!
    "! @parameter iv_view_name | <p class="shorttext synchronized">View name</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Language</p>
    "! @parameter ev_data | <p class="shorttext synchronized">Data in JSON format</p>
    "! @parameter ev_data_template | <p class="shorttext synchronized">Data template in JSON format</p>
    METHODS read_data
      IMPORTING
        !iv_view_name     TYPE tabname
        !iv_langu         TYPE sylangu DEFAULT sy-langu
        !iv_mode          TYPE char1 OPTIONAL
      EXPORTING
        !ev_data          TYPE string
        !ev_data_template TYPE string.

    "! <p class="shorttext synchronized">Lock the view</p>
    "!
    "! @parameter iv_view_name | <p class="shorttext synchronized">View name</p>
    "! @parameter ev_locked | <p class="shorttext synchronized">View locked</p>
    "! @parameter ev_lock_by_user | <p class="shorttext synchronized">Lock by user</p>
    METHODS lock_view
      IMPORTING
        !iv_view_name    TYPE tabname
      EXPORTING
        !ev_locked       TYPE sap_bool
        !ev_lock_by_user TYPE string.
    "! <p class="shorttext synchronized">validation and determination of values of a row</p>
    "!
    "! @parameter iv_view_name | <p class="shorttext synchronized">View name</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Language</p>
    "! @parameter iv_fieldname | <p class="shorttext synchronized">Fieldname modified</p>
    "! @parameter ev_data | <p class="shorttext synchronized">Data in JSON format</p>
    "! @parameter ev_data_template | <p class="shorttext synchronized">Data template in JSON format</p>
    METHODS row_validation_determination
      IMPORTING
        !iv_view_name TYPE tabname
        !iv_fieldname TYPE fieldname
        !iv_langu     TYPE sylangu DEFAULT sy-langu
        !iv_row       TYPE string
      EXPORTING
        !ev_row       TYPE string.
  PROTECTED SECTION.
    DATA mo_controller TYPE REF TO zcl_al30_controller.
    DATA mo_conf TYPE REF TO zcl_al30_conf.
    DATA mo_view TYPE REF TO zcl_al30_view_ui5.


    "! <p class="shorttext synchronized">Create internal table for view data</p>
    "!
    "! @parameter iv_view_name | <p class="shorttext synchronized">View name</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Language</p>
    "! @parameter iv_mode | <p class="shorttext synchronized">Mode:'U' Update - 'V' View</p>
    "! @parameter eo_data | <p class="shorttext synchronized">Data</p>
    "! @parameter es_return | <p class="shorttext synchronized">Return</p>
    "! @parameter et_fields_view | <p class="shorttext synchronized">Fields of table</p>
    "! @parameter et_fields_text_view | <p class="shorttext synchronized">Texts of fields of table</p>
    "! @parameter et_fields_ddic | <p class="shorttext synchronized">Fields of data from data dictioary</p>
    METHODS create_it_data_view
      IMPORTING
        !iv_view_name        TYPE tabname
        !iv_langu            TYPE sylangu
        !iv_mode             TYPE char1
      EXPORTING
        !eo_data             TYPE REF TO data
        !es_return           TYPE bapiret2
        !et_fields_view      TYPE zif_al30_data=>tt_fields_view
        !et_fields_text_view TYPE zif_al30_data=>tt_fields_text_view
        !et_fields_ddic      TYPE dd03ptab .

    "! <p class="shorttext synchronized">Reading the view configuration to be used in the view data class</p>
    "!
    "! @parameter iv_view_name | <p class="shorttext synchronized">View name</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Language</p>
    "! @parameter es_return | <p class="shorttext synchronized">Return</p>
    "! @parameter et_fields_view | <p class="shorttext synchronized">Fields of table</p>
    "! @parameter et_fields_text_view | <p class="shorttext synchronized">Texts of fields of table</p>
    "! @parameter et_fields_ddic | <p class="shorttext synchronized">Fields of data from data dictioary</p>
    METHODS read_view_conf_for_data
      IMPORTING
                iv_view_name        TYPE tabname
                iv_langu            TYPE sylangu
      EXPORTING es_return           TYPE bapiret2
                es_view             TYPE zal30_t_view
                et_fields_view      TYPE zif_al30_data=>tt_fields_view
                et_fields_text_view TYPE zif_al30_data=>tt_fields_text_view
                et_fields_ddic      TYPE dd03ptab .
    "! <p class="shorttext synchronized">Complete data for template data</p>
    "! @parameter it_fields_ddic | <p class="shorttext synchronized">Fields of data from data dictionary</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Language</p>
    "! @parameter it_fields_text_view | <p class="shorttext synchronized">Texts of fields of table</p>
    "! @parameter cs_data | <p class="shorttext synchronized">Data</p>
    METHODS complete_data_template
      IMPORTING
        it_fields_ddic TYPE dd03ptab
        it_fields_view TYPE zif_al30_data=>tt_fields_view
        iv_langu       TYPE sylangu
      CHANGING
        cs_data        TYPE any.
    "! <p class="shorttext synchronized">Adapt ALV style to UI5</p>
    "! @parameter co_data | <p class="shorttext synchronized">Values</p>
    METHODS adapt_alv_field_style_2_ui5
      CHANGING
        co_data TYPE REF TO data.
    "! <p class="shorttext synchronized">Adapt UI5 style to ALV</p>
    "! @parameter co_data | <p class="shorttext synchronized">Values</p>
    METHODS adapt_ui5_field_style_2_alv
      CHANGING
        co_data TYPE REF TO data.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_al30_gw_controller IMPLEMENTATION.


  METHOD check_authorization_view.

    rv_level_auth = mo_controller->check_authorization_view(
       EXPORTING
         iv_view_name   = iv_view_name
         iv_view_action = COND #( WHEN iv_view_action IS INITIAL THEN  zif_al30_data=>cs_action_auth-update ELSE iv_view_action )
         iv_user        = COND #( WHEN iv_user IS INITIAL THEN sy-uname ELSE iv_user ) ).

  ENDMETHOD.


  METHOD constructor.
    mo_controller = NEW zcl_al30_controller(  ).

    " Se instancias las clases encargadas de gestionar la configuración y la vista
    mo_conf = NEW zcl_al30_conf( ).
    mo_view = NEW zcl_al30_view_ui5( ).

  ENDMETHOD.


  METHOD create_it_data_view.

    CLEAR: es_return, et_fields_ddic, et_fields_text_view, et_fields_view.

    " Se leen los datos de la vista y se pasan dichos valores a la clase encargada de gestionar los datos
    read_view_conf_for_data( EXPORTING iv_langu = iv_langu
                                       iv_view_name = iv_view_name
                             IMPORTING es_return = es_return
                                       et_fields_ddic = et_fields_ddic
                                       et_fields_text_view = et_fields_text_view
                                       et_fields_view = et_fields_view ).

    IF es_return IS INITIAL.

      CALL METHOD mo_view->create_it_data_view
        EXPORTING
          iv_mode   = iv_mode
        IMPORTING
          es_return = es_return
          et_data   = eo_data.

    ENDIF.

  ENDMETHOD.


  METHOD get_views.

    mo_view->view_list( EXPORTING iv_langu = iv_langu
                                  it_r_views = it_r_views
                        IMPORTING et_view_list = DATA(lt_views) ).

    " Se recorre los datos y se mira si te tiene autorización
    LOOP AT lt_views REFERENCE INTO DATA(lo_views).
      DATA(lv_auth) = mo_controller->check_authorization_view( EXPORTING iv_view_name   = lo_views->view_name
                                                                         iv_view_action = zif_al30_data=>cs_action_auth-update
                                                                         iv_user        = COND #( WHEN iv_user IS INITIAL THEN sy-uname ELSE iv_user ) ).

      IF lv_auth NE zif_al30_data=>cs_level_auth_user-non. " Si tiene permiso se añade
        INSERT VALUE #( view_name = lo_views->view_name
                        view_desc = lo_views->view_desc
                        level_auth = lv_auth ) INTO TABLE et_views.
      ENDIF.
      CLEAR lv_auth.
    ENDLOOP.

  ENDMETHOD.


  METHOD read_data.
    FIELD-SYMBOLS <data> TYPE STANDARD TABLE.

    " Si el modo pasado no es el esperado se le pone el de visualizar
    DATA(lv_mode) = COND #( WHEN iv_mode = zif_al30_data=>cv_mode_change OR iv_mode = zif_al30_data=>cv_mode_view THEN iv_mode ELSE zif_al30_data=>cv_mode_view ).


    " Se llama al proceso que creará la tabla interna para poder leer los datos
    create_it_data_view( EXPORTING iv_view_name = iv_view_name
                                   iv_langu = iv_langu
                                   iv_mode = lv_mode
                         IMPORTING eo_data = DATA(lo_data)
                                   es_return = DATA(ls_return)
                                   et_fields_ddic = DATA(lt_fields_ddic)
                                   et_fields_view = DATA(lt_fields_view) ).

    IF ls_return IS INITIAL AND lo_data IS BOUND.

      mo_view->read_data( EXPORTING is_filters = VALUE zif_al30_data=>ts_filter_read_data(  )
                          IMPORTING es_return = ls_return
                          CHANGING co_data = lo_data ).

      " Se adapta lo estilos de SAP a los de UI5
      adapt_alv_field_style_2_ui5( CHANGING co_data = lo_data ).


      ASSIGN lo_data->* TO <data>.
      ev_data = zcl_al30_ui5_json=>zserialize( data = <data> pretty_name = /ui2/cl_json=>pretty_mode-none ).

      " Se crea un registro para los datos template. Este servirá para añadir nuevos registros desde el frontend.
      UNASSIGN <data>.
      create_it_data_view( EXPORTING iv_view_name = iv_view_name
                                     iv_langu = iv_langu
                                     iv_mode = lv_mode
                           IMPORTING eo_data = DATA(lo_data_template) ).
      ASSIGN lo_data_template->* TO <data>.
      APPEND INITIAL LINE TO <data> ASSIGNING FIELD-SYMBOL(<wa_template_data>).
      " Se completan datos en los datos del template que solo es más sencillo que lo complete  el backend
      complete_data_template( EXPORTING it_fields_ddic = lt_fields_ddic
                                        it_fields_view = lt_fields_view
                                        iv_langu = iv_langu
                              CHANGING cs_data = <wa_template_data> ) .


      ev_data_template = zcl_al30_ui5_json=>zserialize( data = <data> pretty_name = /ui2/cl_json=>pretty_mode-none ).

    ENDIF.

  ENDMETHOD.


  METHOD read_view.

    " Si el modo pasado no es el esperado se le pone el de visualizar
    DATA(lv_mode) = COND #( WHEN iv_mode = zif_al30_data=>cv_mode_change OR iv_mode = zif_al30_data=>cv_mode_view THEN iv_mode ELSE zif_al30_data=>cv_mode_view ).

    " Se leen los datos básicos para poder acceder a la clase de datos
    read_view_conf_for_data( EXPORTING iv_view_name = iv_view_name
                                       iv_langu = iv_langu
                             IMPORTING es_return = DATA(ls_return)
                                       et_fields_view = DATA(lt_fields) ).

    IF ls_return IS INITIAL.

      " Se recupera el catalogo de campos
      mo_view->get_fieldcat_view(
        EXPORTING
          iv_mode = iv_mode
        IMPORTING
          es_return       = ls_return
          et_fieldcat     = DATA(lt_fieldcat) ).

      LOOP AT lt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fieldcat>).
        DATA(ls_fields) = CORRESPONDING zif_al30_ui5_data=>ts_view_fields( <ls_fieldcat> ).
        ls_fields-len = <ls_fieldcat>-intlen.
        ls_fields-type = <ls_fieldcat>-inttype.

        " Se buscan en los campos de la vista para obtener si es campos clave
        READ TABLE lt_fields ASSIGNING FIELD-SYMBOL(<ls_fields>) WITH KEY fieldname = <ls_fieldcat>-fieldname.
        IF sy-subrc = 0.
          ls_fields-key_ddic = <ls_fields>-key_ddic.
        ENDIF.

        INSERT ls_fields INTO TABLE et_fields.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD read_view_conf_for_data.

    CLEAR: es_return, es_view, et_fields_ddic, et_fields_text_view, et_fields_view.

    " La lectura de datos se hace en varios pasos.
    " 1) Se leen los datos de la vista en formato ALV. Esto es necesario para reaprovechar el resto de método
* Leo los datos de la vista
    CALL METHOD mo_conf->read_view
      EXPORTING
        iv_name_view    = iv_view_name
        iv_langu        = iv_langu
        iv_all_language = abap_false
      IMPORTING
        es_return       = es_return
        es_view         = es_view
        et_fields       = et_fields_view
        et_fields_text  = et_fields_text_view
        et_fields_ddic  = et_fields_ddic.


    IF es_return IS INITIAL. " Si no hay errores se continua el proceso

      " Si la vista tiene configurada exit se llama al proceso que la instancia
      IF es_view-exit_class IS NOT INITIAL.
        mo_view->instance_exit_class( es_view-exit_class ).
      ENDIF.

      " 2) Se pasan los datos leídos de la configuración a la clase de obtención de valores
      mo_view->set_data_conf_view( is_view = es_view
                             it_fields_view = et_fields_view
                             it_fields_text_view = et_fields_text_view
                             it_fields_ddic = et_fields_ddic ).

    ENDIF.
  ENDMETHOD.
  METHOD lock_view.
    TRY.

        " Se leen los datos de la vista para determinar si hay tabla de texto para poderla bloquear
        CALL METHOD mo_conf->read_view
          EXPORTING
            iv_name_view    = iv_view_name
            iv_read_ddic    = abap_false
            iv_all_language = abap_false
          IMPORTING
            es_view         = DATA(ls_view).

        mo_view->lock_view( EXPORTING iv_view_name = iv_view_name
                                      iv_view_text = ls_view-texttable ).

        ev_locked = abap_false.
        ev_lock_by_user = space.

      CATCH zcx_al30 INTO DATA(lx_excep).
        ev_locked = abap_true.
        ev_lock_by_user = lx_excep->mv_msgv1.

    ENDTRY.
  ENDMETHOD.


  METHOD complete_data_template.

    " Campo mandante
    READ TABLE it_fields_ddic ASSIGNING FIELD-SYMBOL(<ls_fields_ddic>) WITH KEY datatype = zif_al30_data=>cs_datatype-mandt.
    IF sy-subrc = 0.
      ASSIGN COMPONENT <ls_fields_ddic>-fieldname OF STRUCTURE cs_data TO FIELD-SYMBOL(<field>).
      IF sy-subrc = 0.
        <field> = sy-mandt.
      ENDIF.
    ENDIF.

    " Campo de idioma de la tabla de texto
    READ TABLE it_fields_view ASSIGNING FIELD-SYMBOL(<ls_fields_view>) WITH KEY field_texttable = abap_true
                                                                                    lang_texttable = abap_true.
    IF sy-subrc = 0.
      ASSIGN COMPONENT <ls_fields_ddic>-fieldname OF STRUCTURE cs_data TO <field>.
      IF sy-subrc = 0.
        <field> = iv_langu.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD adapt_alv_field_style_2_ui5.
    FIELD-SYMBOLS <data> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <lt_alv_style> TYPE lvc_t_styl.
    FIELD-SYMBOLS <lt_ui5_style> TYPE zal30_i_ui5_fields_styles.

    ASSIGN co_data->* TO <data>.

    LOOP AT <data> ASSIGNING FIELD-SYMBOL(<wa>).
      ASSIGN COMPONENT zif_al30_data=>cs_control_fields_alv_data-style OF STRUCTURE <wa> TO <lt_alv_style>.
      IF sy-subrc = 0.
        IF <lt_alv_style> IS NOT INITIAL.
          ASSIGN COMPONENT zif_al30_ui5_data=>cs_control_fields_ui5_data-style OF STRUCTURE <wa> TO <lt_ui5_style>.
          IF sy-subrc = 0.
            LOOP AT <lt_alv_style> ASSIGNING FIELD-SYMBOL(<ls_alv_style>).
              " Como puede ser que el estilo lo hayan metido en el estilo de ui5, lo  verifico antes de insertarlo
              IF <ls_alv_style>-style = cl_gui_alv_grid=>mc_style_disabled.
                READ TABLE <lt_ui5_style> TRANSPORTING NO FIELDS WITH KEY fieldname = <ls_alv_style>-fieldname
                                                                          editable = zif_al30_ui5_data=>cs_javascript_boolean-false.
                IF sy-subrc NE 0.
                  INSERT VALUE #( fieldname = <ls_alv_style>-fieldname editable = zif_al30_ui5_data=>cs_javascript_boolean-false ) INTO TABLE <lt_ui5_style>.
                ENDIF.
              ENDIF.
              IF <ls_alv_style>-style = cl_gui_alv_grid=>mc_style_enabled.
                READ TABLE <lt_ui5_style> TRANSPORTING NO FIELDS WITH KEY fieldname = <ls_alv_style>-fieldname
                                                                          editable = zif_al30_ui5_data=>cs_javascript_boolean-true.
                IF sy-subrc NE 0.
                  INSERT VALUE #( fieldname = <ls_alv_style>-fieldname editable = zif_al30_ui5_data=>cs_javascript_boolean-true ) INTO TABLE <lt_ui5_style>.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.

          " Se limpia la tabla de estilos de ALV porque no es necesario que viaje y asi se reduce el tamaño de datos a enviar
          CLEAR <lt_alv_style>.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD row_validation_determination.
    FIELD-SYMBOLS <data> TYPE STANDARD TABLE.
    " El mismo valor que entra es el que sale. En el proceso ya se irán cambiando valores
    ev_row = iv_row.

    "Para procesar el registro hay que crear una tabla interna del mismo tipo que la viene para guarda el valor pasado por parámetros

    " Se llama al proceso que creará la tabla interna para poder leer los datos
    create_it_data_view( EXPORTING iv_view_name = iv_view_name
                                   iv_langu = iv_langu
                                   iv_mode = zif_al30_data=>cv_mode_change
                         IMPORTING eo_data = DATA(lo_data)
                                   es_return = DATA(ls_return)
                                   et_fields_ddic = DATA(lt_fields_ddic)
                                   et_fields_view = DATA(lt_fields_view) ).

    IF ls_return IS INITIAL AND lo_data IS BOUND.
      " Se añade un registro en blanco para poder hace el mapeo
      ASSIGN lo_data->* TO <data>.
      INSERT INITIAL LINE INTO TABLE <data> ASSIGNING FIELD-SYMBOL(<wa>).

      " Se transforma el JSON al registro de la tabla
      zcl_al30_ui5_json=>deserialize( EXPORTING json = iv_row
                                                pretty_name = /ui2/cl_json=>pretty_mode-none
                                      CHANGING data = <wa> ).

      " Se adaptan los estilos de UI5 al del ALV por si se modifican en las exit
      adapt_ui5_field_style_2_alv( CHANGING co_data = lo_data ).

    ENDIF.


  ENDMETHOD.

  METHOD adapt_ui5_field_style_2_alv.
    FIELD-SYMBOLS <data> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <lt_alv_style> TYPE lvc_t_styl.
    FIELD-SYMBOLS <lt_ui5_style> TYPE zal30_i_ui5_fields_styles.

    ASSIGN co_data->* TO <data>.

    LOOP AT <data> ASSIGNING FIELD-SYMBOL(<wa>).
      ASSIGN COMPONENT zif_al30_data=>cs_control_fields_alv_data-style OF STRUCTURE <wa> TO <lt_alv_style>.
      IF sy-subrc = 0.
        IF <lt_alv_style> IS NOT INITIAL.
          ASSIGN COMPONENT zif_al30_ui5_data=>cs_control_fields_ui5_data-style OF STRUCTURE <wa> TO <lt_ui5_style>.
          IF sy-subrc = 0.
            " Los estilos del ALV se borran cuando se envian a UI5 para reducir los datos que viajan. Por lo tanto, ahora hay que leer e insertar
            LOOP AT <lt_ui5_style> ASSIGNING FIELD-SYMBOL(<ls_ui5_style>).
              INSERT VALUE #( fieldname = <ls_ui5_style>-fieldname
                              style = COND #( WHEN <ls_ui5_style>-editable = abap_true THEN cl_gui_alv_grid=>mc_style_disabled ELSE cl_gui_alv_grid=>mc_style_enabled ) )
                     INTO TABLE <lt_alv_style>.
            ENDLOOP.
          ENDIF.
          " Se limpia la tabla de estilos de UI5
          CLEAR <lt_ui5_style>.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
