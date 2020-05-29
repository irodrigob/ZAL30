CLASS zcl_al30_gw_controller DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Get the view list</p>
    "!
    "! @parameter iv_user | <p class="shorttext synchronized">User</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Language</p>
    "! @parameter it_r_views | <p class="shorttext synchronized">Views to filter</p>
    "! @parameter et_view | <p class="shorttext synchronized">View list</p>
    METHODS get_views
      IMPORTING
        !iv_user    TYPE string
        !iv_langu   TYPE sylangu DEFAULT sy-langu
        !it_r_views TYPE zif_al30_data=>tt_r_tabname OPTIONAL
      EXPORTING
        !et_views   TYPE zif_al30_ui5_data=>tt_view_list.
    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    "!
    METHODS constructor.
    "! <p class="shorttext synchronized">Check the authorization level in view</p>
    "!
    "! @parameter iv_view_name | <p class="shorttext synchronized">View name</p>
    "! @parameter iv_view_action | <p class="shorttext synchronized">'U' Update 'S' Show</p>
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
    "! @parameter iv_row | <p class="shorttext synchronized">Row data in JSON format</p>
    "! @parameter ev_row | <p class="shorttext synchronized">Row data in JSON format</p>
    METHODS row_validation_determination
      IMPORTING
        !iv_view_name TYPE tabname
        !iv_langu     TYPE sylangu DEFAULT sy-langu
        !iv_row       TYPE string
      EXPORTING
        !ev_row       TYPE string.
    "! <p class="shorttext synchronized">Verify field data</p>
    "!
    "! @parameter iv_view_name | <p class="shorttext synchronized">View name</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Language</p>
    "! @parameter iv_fieldname | <p class="shorttext synchronized">Fieldname modified</p>
    "! @parameter iv_value | <p class="shorttext synchronized">Value</p>
    "! @parameter ev_message_type | <p class="shorttext synchronized">Message type: 'E' error 'S' or empty is success</p>
    "! @parameter ev_message | <p class="shorttext synchronized">Message</p>
    METHODS verify_field_data
      IMPORTING
        !iv_langu        TYPE sylangu DEFAULT sy-langu
        !iv_view_name    TYPE tabname
        !iv_fieldname    TYPE fieldname
        !iv_value        TYPE any
      EXPORTING
        !ev_message_type TYPE bapi_mtype
        !ev_message      TYPE string.
    "! <p class="shorttext synchronized">Save data</p>
    "!
    "! @parameter iv_view_name | <p class="shorttext synchronized">View name</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Language</p>
    "! @parameter iv_data | <p class="shorttext synchronized">Data in JSON format</p>
    "! @parameter iv_original_data | <p class="shorttext synchronized">Original data</p>
    "! @parameter iv_transport_order | <p class="shorttext synchronized">Transport order</p>
    "! @parameter ev_data | <p class="shorttext synchronized">Data in JSON format</p>
    "! @parameter ev_return | <p class="shorttext synchronized">Return of process</p>
    "! @parameter ev_order | <p class="shorttext synchronized">Order used for transport</p>
    METHODS save_data
      IMPORTING
        !iv_view_name       TYPE tabname
        !iv_langu           TYPE sylangu DEFAULT sy-langu
        !iv_data            TYPE string
        !iv_original_data   TYPE string
        !iv_transport_order TYPE trkorr OPTIONAL
      EXPORTING
        !ev_data            TYPE string
        !ev_return          TYPE string
        !ev_order           TYPE trkorr.

    "! <p class="shorttext synchronized">Get user orders</p>
    "! @parameter iv_user | <p class="shorttext synchronized">User</p>
    "! @parameter et_orders | <p class="shorttext synchronized">Orders of user</p>
    METHODS get_user_orders
      IMPORTING
        !iv_user   TYPE syuname DEFAULT sy-uname
        !iv_langu  TYPE sylangu DEFAULT sy-langu
      EXPORTING
        !et_orders TYPE zcl_al30_ui5_transport_order=>tt_user_orders.
    "! <p class="shorttext synchronized">Check the transport order</p>
    "!
    "! @parameter iv_langu | <p class="shorttext synchronized">Language</p>
    "! @parameter iv_order | <p class="shorttext synchronized">Order</p>
    "! @parameter ev_order | <p class="shorttext synchronized">New Order</p>
    METHODS check_transport_order
      IMPORTING
        !iv_langu  TYPE sylangu DEFAULT sy-langu
        !iv_order  TYPE trkorr
      EXPORTING
        !ev_order  TYPE trkorr
        !es_return TYPE zif_al30_ui5_data=>ts_return.
    "! <p class="shorttext synchronized">Get catalog of fields with search help</p>
    "!
    "! @parameter iv_langu | <p class="shorttext synchronized">Language</p>
    "! @parameter iv_view_name | <p class="shorttext synchronized">View name</p>
    "! @parameter et_catalog | <p class="shorttext synchronized">Catalog of search help</p>
    METHODS get_f4_catalog
      IMPORTING
        !iv_langu     TYPE sylangu DEFAULT sy-langu
        !iv_view_name TYPE tabname
      EXPORTING
        !et_catalog   TYPE zif_al30_ui5_data=>tt_f4_catalog.
    "! <p class="shorttext synchronized">Get data for search help of a field</p>
    "!
    "! @parameter iv_langu | <p class="shorttext synchronized">Language</p>
    "! @parameter iv_view_name | <p class="shorttext synchronized">View name</p>
    "! @parameter iv_field_name | <p class="shorttext synchronized">Field name</p>
    "! @parameter et_data | <p class="shorttext synchronized">Data for the search help</p>
    METHODS get_f4_data
      IMPORTING
        !iv_langu      TYPE sylangu DEFAULT sy-langu
        !iv_view_name  TYPE tabname
        !iv_field_name TYPE fieldname
      EXPORTING
        !et_data       TYPE zif_al30_ui5_data=>tt_f4_data.

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
    "! @parameter et_fields_ddic | <p class="shorttext synchronized">Fields of data from data dictionary</p>
    "! @parameter es_view | <p class="shorttext synchronized">View info</p>
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
        !et_fields_ddic      TYPE dd03ptab
        !es_view             TYPE zal30_t_view.

    "! <p class="shorttext synchronized">Reading the view configuration to used in view data class</p>
    "!Reading the view configuration to be used in the view data class
    "! @parameter iv_view_name | <p class="shorttext synchronized">View name</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Language</p>
    "! @parameter es_return | <p class="shorttext synchronized">Return</p>
    "! @parameter et_fields_view | <p class="shorttext synchronized">Fields of table</p>
    "! @parameter et_fields_text_view | <p class="shorttext synchronized">Texts of fields of table</p>
    "! @parameter et_fields_ddic | <p class="shorttext synchronized">Fields of data from data dictionary</p>
    "! @parameter et_foreign_key_ddic | <p class="shorttext synchronized">Foreing key of data from data dictionary</p>
    METHODS read_view_conf_for_data
      IMPORTING
                iv_view_name        TYPE tabname
                iv_langu            TYPE sylangu
      EXPORTING es_return           TYPE bapiret2
                es_view             TYPE zal30_t_view
                et_fields_view      TYPE zif_al30_data=>tt_fields_view
                et_fields_text_view TYPE zif_al30_data=>tt_fields_text_view
                et_fields_ddic      TYPE dd03ptab
                et_foreign_key_ddic TYPE dd05mttyp .
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
    "! <p class="shorttext synchronized">Adapt ALV data to UI5</p>
    "! 1) Apply conversion exit
    "! 2) Adapt ALV field style to UI5
    "! @parameter it_fields_ddic | <p class="shorttext synchronized">DDIC fields</p>
    "! @parameter co_data | <p class="shorttext synchronized">Values</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Language</p>
    METHODS adapt_alv_data_2_ui5
      IMPORTING
        !it_fields_ddic TYPE dd03ptab
        !iv_langu       TYPE sylangu DEFAULT sy-langu
      CHANGING
        !co_data        TYPE REF TO data.
    "! <p class="shorttext synchronized">Adapt UI5 data to ALV</p>
    "! 1) Apply conversion exit
    "! 2) Adapt UI5 field style to ALV
    "! @parameter it_fields_ddic | <p class="shorttext synchronized">DDIC fields</p>
    "! @parameter co_data | <p class="shorttext synchronized">Values</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Language</p>
    METHODS adapt_ui5_data_2_alv
      IMPORTING
        !it_fields_ddic TYPE dd03ptab
        !iv_langu       TYPE sylangu DEFAULT sy-langu
      CHANGING
        !co_data        TYPE REF TO data.
    "! <p class="shorttext synchronized">Adapt UI5 style to ALV</p>
    "! @parameter cs_row | <p class="shorttext synchronized">Row data</p>
    METHODS adapt_ui5_field_style_2_alv
      CHANGING
        cs_row TYPE any.
    "! <p class="shorttext synchronized">Split data to save</p>
    "! @parameter ct_data | <p class="shorttext synchronized">Data inserted o changed</p>
    "! @parameter ct_data_del | <p class="shorttext synchronized">Data to delete</p>
    METHODS split_data_to_save
      CHANGING
        ct_data     TYPE STANDARD TABLE
        ct_data_del TYPE STANDARD TABLE.
    "! <p class="shorttext synchronized">Convert return(BAPIRET2_T) to UI5 return</p>
    "! @parameter it_return | <p class="shorttext synchronized">Return in BAPIRET2_T format</p>
    "! @parameter et_return_ui5 | <p class="shorttext synchronized">Return UI5 format</p>
    METHODS conv_return_2_return_ui5
      IMPORTING
        it_return     TYPE bapiret2_t
        iv_langu      TYPE sylangu DEFAULT sy-langu
      EXPORTING
        et_return_ui5 TYPE zif_al30_ui5_data=>tt_return.
    "! <p class="shorttext synchronized">Apply output conversion exit to the row</p>
    "! @parameter it_fields_ddic | <p class="shorttext synchronized">DDIC fields</p>
    "! @parameter cs_row | <p class="shorttext synchronized">Row data</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Language</p>
    METHODS apply_conv_exit_output
      IMPORTING
        iv_langu       TYPE sylangu DEFAULT sy-langu
        it_fields_ddic TYPE dd03ptab
      CHANGING
        cs_row         TYPE any.
    "! <p class="shorttext synchronized">Adapt ALV style to UI5</p>
    "! @parameter co_data | <p class="shorttext synchronized">Values</p>
    METHODS adapt_alv_field_style_2_ui5
      CHANGING
        cs_row TYPE any.
    "! <p class="shorttext synchronized">Apply input conversion exit to the row</p>
    "! @parameter it_fields_ddic | <p class="shorttext synchronized">DDIC fields</p>
    "! @parameter cs_row | <p class="shorttext synchronized">Row data</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Language</p>
    METHODS apply_conv_exit_input
      IMPORTING
        it_fields_ddic TYPE dd03ptab
        iv_langu       TYPE sylangu DEFAULT sy-langu
      CHANGING
        cs_row         TYPE any.
    "! <p class="shorttext synchronized">Get allowed transport for the view</p>
    "! @parameter iv_view_name | <p class="shorttext synchronized">View name</p>
    "! @parameter rv_allowed | <p class="shorttext synchronized">Allowed</p>
    METHODS get_allowed_transport
      IMPORTING !iv_view_name     TYPE tabname
      RETURNING VALUE(rv_allowed) TYPE sap_bool.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_al30_gw_controller IMPLEMENTATION.


  METHOD adapt_alv_data_2_ui5.
    FIELD-SYMBOLS <data> TYPE STANDARD TABLE.
    ASSIGN co_data->* TO <data>.

    LOOP AT <data> ASSIGNING FIELD-SYMBOL(<wa>).

      " Aplico las rutinas de conversion a los campos que la tengan
      apply_conv_exit_output( EXPORTING it_fields_ddic = it_fields_ddic
                                        iv_langu = iv_langu
                                        CHANGING cs_row = <wa> ).

      " Se convierten los estilo del ALV a UI5.
      adapt_alv_field_style_2_ui5( CHANGING cs_row = <wa> ).

    ENDLOOP.

  ENDMETHOD.


  METHOD adapt_ui5_field_style_2_alv.
    FIELD-SYMBOLS <data> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <lt_alv_style> TYPE lvc_t_styl.
    FIELD-SYMBOLS <lt_ui5_style> TYPE zal30_i_ui5_fields_styles.

    ASSIGN COMPONENT zif_al30_data=>cs_control_fields_alv_data-style OF STRUCTURE cs_row TO <lt_alv_style>.
    IF sy-subrc = 0.
      IF <lt_alv_style> IS NOT INITIAL.
        ASSIGN COMPONENT zif_al30_ui5_data=>cs_control_fields_ui5_data-style OF STRUCTURE cs_row TO <lt_ui5_style>.
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

  ENDMETHOD.


  METHOD check_authorization_view.

    rv_level_auth = mo_controller->check_authorization_view(
       EXPORTING
         iv_view_name   = iv_view_name
         iv_view_action = COND #( WHEN iv_view_action IS INITIAL THEN  zif_al30_data=>cs_action_auth-update ELSE iv_view_action )
         iv_user        = COND #( WHEN iv_user IS INITIAL THEN sy-uname ELSE iv_user ) ).

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


  METHOD constructor.
    mo_controller = NEW zcl_al30_controller(  ).

    " Se instancias las clases encargadas de gestionar la configuración y la vista
    mo_conf = NEW zcl_al30_conf( ).
    mo_view = NEW zcl_al30_view_ui5( ).

  ENDMETHOD.


  METHOD create_it_data_view.

    CLEAR: es_return, et_fields_ddic, et_fields_text_view, et_fields_view, es_view.

    " Se leen los datos de la vista y se pasan dichos valores a la clase encargada de gestionar los datos
    read_view_conf_for_data( EXPORTING iv_langu = iv_langu
                                       iv_view_name = iv_view_name
                             IMPORTING es_return = es_return
                                       et_fields_ddic = et_fields_ddic
                                       et_fields_text_view = et_fields_text_view
                                       et_fields_view = et_fields_view
                                       es_view = es_view ).

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

    mo_view->get_allowed_transport( EXPORTING it_r_views = it_r_views
                                    IMPORTING et_allowed_transport = DATA(lt_allowed_transport) ).

    " Se recorre los datos y se mira si te tiene autorización
    LOOP AT lt_views REFERENCE INTO DATA(lo_views).
      DATA(lv_auth) = mo_controller->check_authorization_view( EXPORTING iv_view_name   = lo_views->view_name
                                                                         iv_view_action = zif_al30_data=>cs_action_auth-update
                                                                         iv_user        = COND #( WHEN iv_user IS INITIAL THEN sy-uname ELSE iv_user ) ).

      IF lv_auth NE zif_al30_data=>cs_level_auth_user-non. " Si tiene permiso se añade
        INSERT VALUE #( view_name = lo_views->view_name
                        view_desc = lo_views->view_desc
                        level_auth = lv_auth ) INTO TABLE et_views ASSIGNING FIELD-SYMBOL(<s_views>).

        " Se mira si se puede para transportar
        READ TABLE lt_allowed_transport ASSIGNING FIELD-SYMBOL(<ls_allowed_transport>) WITH TABLE KEY view_name = lo_views->view_name.
        IF sy-subrc = 0.
          <s_views>-allowed_transport = <ls_allowed_transport>-allowed.
        ENDIF.

      ENDIF.
      CLEAR lv_auth.


    ENDLOOP.

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


  METHOD read_data.
    FIELD-SYMBOLS <data> TYPE STANDARD TABLE.

    " Si el modo pasado no es el esperado se le pone el de visualizar
    DATA(lv_mode) = COND #( WHEN iv_mode = zif_al30_data=>cv_mode_change OR iv_mode = zif_al30_data=>cv_mode_view THEN iv_mode ELSE zif_al30_data=>cv_mode_view ).

    mo_view->set_language( iv_langu ). " Idioma del servicio

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


      " Se adapta los datos del ALV a los de UI5.
      adapt_alv_data_2_ui5( EXPORTING it_fields_ddic = lt_fields_ddic
                            CHANGING co_data = lo_data ).


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
                                       et_fields_view = DATA(lt_fields)
                                       et_fields_ddic = DATA(lt_fields_ddic) ).

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
        ls_fields-tabname = iv_view_name. " Pongo el nombre de la vista, porque en el fieldcatalog viene el valor 1
        ls_fields-len = <ls_fieldcat>-intlen.
        ls_fields-type = <ls_fieldcat>-inttype.

        " Se buscan en los campos de la vista para determinar alguna configuración que no esta el catalogo de campos
        READ TABLE lt_fields ASSIGNING FIELD-SYMBOL(<ls_fields>) WITH KEY fieldname = <ls_fieldcat>-fieldname.
        IF sy-subrc = 0.
          ls_fields-key_ddic = <ls_fields>-key_ddic.
          ls_fields-mandatory = <ls_fields>-mandatory.
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
        iv_name_view        = iv_view_name
        iv_langu            = iv_langu
        iv_all_language     = abap_false
      IMPORTING
        es_return           = es_return
        es_view             = es_view
        et_fields           = et_fields_view
        et_fields_text      = et_fields_text_view
        et_fields_ddic      = et_fields_ddic
        et_foreign_key_ddic = et_foreign_key_ddic.


    IF es_return IS INITIAL. " Si no hay errores se continua el proceso

      " Si la vista tiene configurada exit se llama al proceso que la instancia
      IF es_view-exit_class IS NOT INITIAL.
        mo_view->instance_exit_class( es_view-exit_class ).
      ENDIF.

      " 2) Se pasan los datos leídos de la configuración a la clase de obtención de valores
      mo_view->set_data_conf_view( is_view = es_view
                             it_fields_view = et_fields_view
                             it_fields_text_view = et_fields_text_view
                             it_fields_ddic = et_fields_ddic
                             it_foreign_key_ddic = et_foreign_key_ddic ).

    ENDIF.
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

      " Se adaptan los datos de UI5 al del ALV por si se modifican en las exit
      adapt_ui5_data_2_alv( EXPORTING it_fields_ddic = lt_fields_ddic
                                     iv_langu = iv_langu
                            CHANGING co_data = lo_data ).

      mo_view->set_language( iv_langu ). " Idioma del servicio

      " Al método de verificación y validación se le tiene que pasar la fila del ALV en que se modifica el registro. Ese dato viene en el campo
      " fijo ZAL30_TABIX que lo recupero para pasarlo
      ASSIGN COMPONENT zif_al30_data=>cs_control_fields_alv_data-tabix OF STRUCTURE <wa> TO FIELD-SYMBOL(<tabix>).

      mo_view->verify_change_row_data(
        EXPORTING
          iv_row      = <tabix>
        CHANGING
          cs_row_data = <wa> ).

      " Se adapta los datos de SAP a UI5
      adapt_alv_data_2_ui5( EXPORTING it_fields_ddic = lt_fields_ddic
                            CHANGING co_data = lo_data ).

      " Se convierte de los datos de sap a JSON
      ev_row = zcl_al30_ui5_json=>zserialize( data = <wa> pretty_name = /ui2/cl_json=>pretty_mode-none ).

    ENDIF.


  ENDMETHOD.


  METHOD verify_field_data.
    " hay que leer la vista par para poder llamar al proceso de verificación de campo
    read_view_conf_for_data(
      EXPORTING
        iv_view_name        = iv_view_name
        iv_langu            = iv_langu
      IMPORTING
        et_fields_view      = DATA(lt_fields_view)
        et_fields_text_view = DATA(lt_fields_text_view) ).

    mo_view->verify_field_data(
      EXPORTING
        iv_fieldname = iv_fieldname
        iv_value     = iv_value
      IMPORTING
        es_return    = DATA(ls_return) ).

    " Si hay mensaje de retorno se devuelve a los parámetros de salida
    IF ls_return IS NOT INITIAL.
      ev_message_type = ls_return-type.
      " Se rellena el mensaje si no lo estuve.
      IF ls_return-message IS INITIAL.
        ev_message = zcl_al30_util=>fill_return( EXPORTING iv_type = ls_return-type
                                                            iv_number = ls_return-number
                                                            iv_message_v1 = ls_return-message_v1
                                                            iv_message_v2 = ls_return-message_v2
                                                            iv_message_v3 = ls_return-message_v3
                                                            iv_message_v4 = ls_return-message_v4
                                                            iv_langu = iv_langu )-message.
      ELSE.
        ev_message = ls_return-message.
      ENDIF.

    ELSE.
      CLEAR: ev_message, ev_message_type.
    ENDIF.

  ENDMETHOD.
  METHOD save_data.

    FIELD-SYMBOLS <data> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <data_ok> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <original_data> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <data_del> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <data_del_orig> TYPE STANDARD TABLE.
    DATA lt_return_ui5 TYPE zif_al30_ui5_data=>tt_return.
    DATA lo_data_del_orig TYPE REF TO data.

    " El mismo valor que entra es el que sale. En el proceso ya se irán cambiando valores
    ev_data = iv_data.

    mo_view->set_language( iv_langu ). " Idioma del servicio

    " Si hay orden se lanza el proceso de validación de la orden, el motivo es que la orden que se escoje en UI5 puede ser valida pero no tener tarea valida. Ese chequeo validará que
    " tenga tarea y en caso de no tenerla añadirá una tarea valida

    IF iv_transport_order IS NOT INITIAL.

      check_transport_order(
        EXPORTING
          iv_langu  = iv_langu
          iv_order  = iv_transport_order
        IMPORTING
          ev_order  = ev_order
          es_return = DATA(ls_return_ui5) ).

      IF ls_return_ui5 IS NOT INITIAL. " Si hay mensaje es que hay error y el proceso se cancela
        INSERT ls_return_ui5 INTO TABLE lt_return_ui5.
      ENDIF.
    ENDIF.

    IF lt_return_ui5 IS INITIAL.

      "Para procesar el registro hay que crear una tabla interna del mismo tipo que la viene para guarda el valor pasado por parámetros

      " Se llama al proceso que creará la tabla interna para poder leer los datos
      create_it_data_view( EXPORTING iv_view_name = iv_view_name
                                     iv_langu = iv_langu
                                     iv_mode = zif_al30_data=>cv_mode_change
                           IMPORTING eo_data = DATA(lo_data)
                                     es_return = DATA(ls_return)
                                     et_fields_ddic = DATA(lt_fields_ddic)
                                     et_fields_view = DATA(lt_fields_view)
                                     es_view = DATA(ls_view) ).

      IF ls_return IS INITIAL AND lo_data IS BOUND.
        " Se añade un registro en blanco para poder hace el mapeo
        ASSIGN lo_data->* TO <data>.

        " Creo la tabla dinámica para el borrado
        mo_view->create_it_data_view( EXPORTING iv_mode = zif_al30_data=>cv_mode_change
                                      IMPORTING et_data = DATA(lo_data_del) ).
        ASSIGN lo_data_del->* TO <data_del>.

        " Se crea la tabla para los datos originales
        mo_view->create_it_data_view( EXPORTING iv_mode = zif_al30_data=>cv_mode_change
                                      IMPORTING et_data = DATA(lo_original_data) ).

        " Se convierte los datos originales
        ASSIGN lo_original_data->* TO <original_data>.
        zcl_al30_ui5_json=>deserialize( EXPORTING json = iv_original_data
                                              pretty_name = /ui2/cl_json=>pretty_mode-none
                                    CHANGING data = <original_data> ).

        " Se pasa a la clase para que lo tenga para el proceso de grabación
        mo_view->set_original_data( lo_original_data ).

        " Se transforma el JSON al registro de la tabla
        zcl_al30_ui5_json=>deserialize( EXPORTING json = iv_data
                                                  pretty_name = /ui2/cl_json=>pretty_mode-none
                                        CHANGING data = <data> ).

        " Se adaptan los datos de UI5 al del ALV por si se modifican en las exit
        adapt_ui5_data_2_alv( EXPORTING it_fields_ddic = lt_fields_ddic
                                  iv_langu = iv_langu
                         CHANGING co_data = lo_data ).

        " Separo los datos borrados de los modificados/insertados
        split_data_to_save( CHANGING ct_data = <data>
                                    ct_data_del = <data_del> ).

        " Como en proceso de grabación los datos original se "limpian" si va todo bien, lo que hago es crear una tabla local
        " temporal para guardar los datos originales del borrado. Para cuando el proceso de grabación termine ponerlo en la tabla original
        " y pueda ser procesado por UI5.
        CREATE DATA lo_data_del_orig LIKE <data_del>.
        ASSIGN lo_data_del_orig->* TO <data_del_orig>.
        INSERT LINES OF <data_del> INTO TABLE <data_del_orig>.


        mo_view->verify_save_data(
          EXPORTING
            it_data_del = <data_del>
            iv_save_process = abap_true
          IMPORTING
            et_return = DATA(lt_return)
          CHANGING ct_data   = <data> ).

        " Si hay errores no se continua el proceso
        IF ( line_exists( lt_return[ type = zif_al30_data=>cs_msg_type-error ] ) OR
            line_exists( lt_return[ type = zif_al30_data=>cs_msg_type-dump ] ) ).
        ELSE.
          " Se valida que no haya ningúna línea errónea.
          READ TABLE <data> TRANSPORTING NO FIELDS WITH KEY (zif_al30_data=>cs_control_fields_alv_data-row_status) = zif_al30_data=>cs_msg_type-error.
          IF sy-subrc NE 0.

            CALL METHOD mo_view->save_data
              EXPORTING
                iv_allow_request = get_allowed_transport( iv_view_name )
              IMPORTING
                et_return        = DATA(lt_return_save)
              CHANGING
                cv_order         = ev_order
                ct_datos         = <data>
                ct_datos_del     = <data_del>.

            INSERT LINES OF lt_return_save INTO TABLE lt_return.

            " Si hay registros en los datos de borrado originales pero no hay en los que se han enviado a procesar. Es que todo ha ido
            " bien, por lo tanto, inserto los originales.Sino, añado lo que se ha enviado por el servicio
            IF <data_del> IS INITIAL AND <data_del_orig> IS NOT INITIAL.
              INSERT LINES OF <data_del_orig> INTO TABLE <data>.
            ELSE.
              INSERT LINES OF <data_del> INTO TABLE <data>.
            ENDIF.

          ENDIF.

        ENDIF.

        " Se adapta los datos de SAP a los de UI5
        adapt_alv_data_2_ui5( EXPORTING it_fields_ddic = lt_fields_ddic
                              CHANGING co_data = lo_data ).

        " Se convierte de los datos de sap a JSON
        ev_data = zcl_al30_ui5_json=>zserialize( data = <data> pretty_name = /ui2/cl_json=>pretty_mode-none ).

        " Si hay mennsajes se adaptan al formato de ui5
        IF lt_return IS NOT INITIAL.
          conv_return_2_return_ui5( EXPORTING it_return = lt_return
                                              iv_langu = iv_langu
                                    IMPORTING et_return_ui5 = lt_return_ui5 ).

          ev_return = zcl_al30_ui5_json=>zserialize( data = lt_return_ui5 pretty_name = /ui2/cl_json=>pretty_mode-none ).
        ENDIF.

      ENDIF.

    ELSE.
      " Se transforma el mensaje de error a JSON para devolverlo
      ev_return = zcl_al30_ui5_json=>zserialize( data = lt_return_ui5 pretty_name = /ui2/cl_json=>pretty_mode-none ).
    ENDIF.

  ENDMETHOD.


  METHOD split_data_to_save.
    CLEAR ct_data_del.

    " Se leen los registros marcados para borrar
    DATA(lv_cond) = |{ zif_al30_data=>cs_control_fields_alv_data-updkz } = '{ zif_al30_data=>cv_mode_delete }'|.
    LOOP AT ct_data ASSIGNING FIELD-SYMBOL(<ls_data>) WHERE (lv_cond).
      DATA(lv_tabix) = sy-tabix.

      INSERT <ls_data> INTO TABLE ct_data_del.
      DELETE ct_data INDEX lv_tabix.

    ENDLOOP.

  ENDMETHOD.


  METHOD conv_return_2_return_ui5.
    CLEAR: et_return_ui5.

    LOOP AT it_return ASSIGNING FIELD-SYMBOL(<ls_return>).
      INSERT VALUE #( type = <ls_return>-type ) INTO TABLE et_return_ui5 ASSIGNING FIELD-SYMBOL(<ls_return_ui5>).

      " El mensaje aunque este informado lo fuerzo en el idioma del parámetro. El motivo es que no se en que idioma puede estar debido a las exit.
      <ls_return_ui5>-message = zcl_al30_util=>fill_return( EXPORTING iv_type = <ls_return>-type
                                                                     iv_number = <ls_return>-number
                                                                     iv_id = <ls_return>-id
                                                                     iv_message_v1 = <ls_return>-message_v1
                                                                     iv_message_v2 = <ls_return>-message_v2
                                                                     iv_message_v3 = <ls_return>-message_v3
                                                                     iv_message_v4 = <ls_return>-message_v4
                                                                     iv_langu = iv_langu )-message.

    ENDLOOP.

  ENDMETHOD.


  METHOD apply_conv_exit_output.

    LOOP AT it_fields_ddic ASSIGNING FIELD-SYMBOL(<ls_fields_ddic>) WHERE convexit IS NOT INITIAL.
      ASSIGN COMPONENT <ls_fields_ddic>-fieldname OF STRUCTURE cs_row TO FIELD-SYMBOL(<field>).
      " Hay determinadas conversiones que no se pueden hacer dinámicas, como la de unidad que se pasa el idioma
      CASE <ls_fields_ddic>-convexit.
        WHEN 'CUNIT'.
          CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
            EXPORTING
              input          = <field>
              language       = iv_langu
            IMPORTING
              output         = <field>
            EXCEPTIONS
              unit_not_found = 1
              OTHERS         = 2.

        WHEN OTHERS.
          DATA(lv_function) = |CONVERSION_EXIT_{ <ls_fields_ddic>-convexit }_OUTPUT|.
          TRY.
              CALL FUNCTION lv_function
                EXPORTING
                  input  = <field>
                IMPORTING
                  output = <field>.
            CATCH cx_root.
          ENDTRY.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD adapt_alv_field_style_2_ui5.
    FIELD-SYMBOLS <lt_alv_style> TYPE lvc_t_styl.
    FIELD-SYMBOLS <lt_ui5_style> TYPE zal30_i_ui5_fields_styles.

    ASSIGN COMPONENT zif_al30_data=>cs_control_fields_alv_data-style OF STRUCTURE cs_row TO <lt_alv_style>.
    IF sy-subrc = 0.
      IF <lt_alv_style> IS NOT INITIAL.
        ASSIGN COMPONENT zif_al30_ui5_data=>cs_control_fields_ui5_data-style OF STRUCTURE cs_row TO <lt_ui5_style>.
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
  ENDMETHOD.

  METHOD adapt_ui5_data_2_alv.
    FIELD-SYMBOLS <data> TYPE STANDARD TABLE.
    ASSIGN co_data->* TO <data>.

    LOOP AT <data> ASSIGNING FIELD-SYMBOL(<wa>).

      " Aplico las rutinas de conversion a los campos que la tengan
      apply_conv_exit_input( EXPORTING it_fields_ddic = it_fields_ddic
                                        CHANGING cs_row = <wa> ).

      " Se convierten los estilo del ALV a UI5.
      adapt_ui5_field_style_2_alv( CHANGING cs_row = <wa> ).

    ENDLOOP.
  ENDMETHOD.


  METHOD apply_conv_exit_input.
    LOOP AT it_fields_ddic ASSIGNING FIELD-SYMBOL(<ls_fields_ddic>) WHERE convexit IS NOT INITIAL.
      ASSIGN COMPONENT <ls_fields_ddic>-fieldname OF STRUCTURE cs_row TO FIELD-SYMBOL(<field>).
      " Hay determinadas conversiones que no se pueden hacer dinámicas, como la de unidad que se pasa el idioma
      CASE <ls_fields_ddic>-convexit.
        WHEN 'CUNIT'.
          CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
            EXPORTING
              input          = <field>
              language       = iv_langu
            IMPORTING
              output         = <field>
            EXCEPTIONS
              unit_not_found = 1
              OTHERS         = 2.

        WHEN OTHERS.
          DATA(lv_function) = |CONVERSION_EXIT_{ <ls_fields_ddic>-convexit }_INPUT|.
          TRY.
              CALL FUNCTION lv_function
                EXPORTING
                  input  = <field>
                IMPORTING
                  output = <field>.
            CATCH cx_root.
          ENDTRY.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_user_orders.
    NEW zcl_al30_ui5_transport_order( iv_langu )->get_user_orders(
      EXPORTING
        iv_user   = iv_user
      IMPORTING
        et_orders = et_orders ).
  ENDMETHOD.

  METHOD get_allowed_transport.
    mo_view->get_allowed_transport( EXPORTING it_r_views = VALUE #( ( sign = 'I' option = 'EQ' low = iv_view_name ) )
                                    IMPORTING et_allowed_transport = DATA(lt_allowed) ).

    READ TABLE lt_allowed ASSIGNING FIELD-SYMBOL(<ls_allowed>) INDEX 1.
    IF sy-subrc = 0.
      rv_allowed = <ls_allowed>-allowed.
    ENDIF.
  ENDMETHOD.

  METHOD check_transport_order.

    CLEAR: ev_order, es_return.

    ev_order = iv_order.

    " Se valida la orden, este método si la tarea no es valida añade una nueva.
    zcl_al30_util=>check_transport_order(
      EXPORTING
        iv_category = zif_al30_data=>cs_order_category-workbench
      IMPORTING
        es_return   = DATA(ls_return)
      CHANGING
        cv_order    = ev_order ).

    " Solo se devuelven mensaje de error. Si la orden es correcta no se devuelve nada.
    IF ls_return-type = zif_al30_data=>cs_msg_type-error.
      es_return-type = ls_return-type.

      IF ls_return-message IS INITIAL.
        ls_return-message = zcl_al30_util=>fill_return( EXPORTING iv_type = ls_return-type
                                                                            iv_number = ls_return-number
                                                                            iv_id = ls_return-id
                                                                            iv_message_v1 = ls_return-message_v1
                                                                            iv_message_v2 = ls_return-message_v2
                                                                            iv_message_v3 = ls_return-message_v3
                                                                            iv_message_v4 = ls_return-message_v4
                                                                            iv_langu = iv_langu )-message.
      ELSE.
        es_return-message = ls_return-message.
      ENDIF.

    ENDIF.


  ENDMETHOD.

  METHOD get_f4_catalog.

    " Se lee los datos de la vista
    read_view_conf_for_data( EXPORTING iv_view_name        = iv_view_name
                                       iv_langu            = iv_langu ).

    mo_view->set_language( iv_langu ).

    " Se obtiene el catalogo de campos con ayuda para búsqueda
    mo_view->get_f4_catalog( IMPORTING et_catalog = et_catalog ).

  ENDMETHOD.

  METHOD get_f4_data.

    " Se lee los datos de la vista
    read_view_conf_for_data( EXPORTING iv_view_name        = iv_view_name
                                       iv_langu            = iv_langu ).

    mo_view->set_language( iv_langu ).

    mo_view->get_f4_data( EXPORTING iv_field_name = iv_field_name
                          IMPORTING et_data = et_data ).

  ENDMETHOD.

ENDCLASS.
