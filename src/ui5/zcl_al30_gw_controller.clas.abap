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
    METHODS read_data
      IMPORTING
        !iv_view_name TYPE tabname
        !iv_langu     TYPE sylangu DEFAULT sy-langu
        !iv_mode      TYPE char1 OPTIONAL
      EXPORTING
        !ev_data      TYPE string.

    "! <p class="shorttext synchronized">Read data from view</p>
    "!
    "! @parameter iv_view_name | <p class="shorttext synchronized">View name</p>
    "! @parameter ev_locked | <p class="shorttext synchronized">View locked</p>
    METHODS lock_view
      IMPORTING
        !iv_view_name TYPE tabname
      EXPORTING
        !ev_locked    TYPE sap_bool.

  PROTECTED SECTION.
    DATA mo_controller TYPE REF TO zcl_al30_controller.

    "! <p class="shorttext synchronized">Create internal table for view data</p>
    "!
    "! @parameter iv_view_name | <p class="shorttext synchronized">View name</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Language</p>
    "! "! @parameter iv_langu | <p class="shorttext synchronized">Mode:'U' Update - 'V' View</p>
    METHODS create_it_data_view
      IMPORTING
        !iv_view_name TYPE tabname
        !iv_langu     TYPE sylangu
        !iv_mode      TYPE char1
      EXPORTING
        !eo_data      TYPE REF TO data
        !es_return    TYPE bapiret2.

    "! <p class="shorttext synchronized">Reading the view configuration to be used in the view data class</p>
    "!
    "! @parameter iv_view_name | <p class="shorttext synchronized">View name</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Language</p>
    METHODS read_view_conf_for_data
      IMPORTING
                iv_view_name TYPE tabname
                iv_langu     TYPE sylangu
      EXPORTING es_return    TYPE bapiret2.
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
  ENDMETHOD.


  METHOD create_it_data_view.

    CLEAR es_return.

    " Se leen los datos de la vista y se pasan dichos valores a la clase encargada de gestionar los datos
    read_view_conf_for_data( EXPORTING iv_langu = iv_langu
                                       iv_view_name = iv_view_name
                             IMPORTING es_return = es_return ).

    IF es_return IS INITIAL.
      mo_controller->create_it_data_view(
        EXPORTING
          iv_mode   = iv_mode
        IMPORTING
          et_data   = eo_data
          es_return = es_return ).

    ENDIF.

  ENDMETHOD.


  METHOD get_views.

    mo_controller->view_list(
      EXPORTING
        iv_langu     = iv_langu
        it_r_views = it_r_views
      IMPORTING
        et_view_list = DATA(lt_views) ).

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
                                   es_return = DATA(ls_return) ).

    IF ls_return IS INITIAL.
      mo_controller->read_data(
        EXPORTING
          is_filters = VALUE zif_al30_data=>ts_filter_read_data(  )
        IMPORTING
          es_return  = ls_return
        CHANGING
          co_data    = lo_data ).

      " El último paso es convertir los datos en un string JSON
      IF lo_data IS BOUND.

        ASSIGN lo_data->* TO <data>.
        ev_data = zcl_al30_ui5_json=>zserialize( data = <data> pretty_name = /ui2/cl_json=>pretty_mode-none ).

      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD read_view.

    " Si el modo pasado no es el esperado se le pone el de visualizar
    DATA(lv_mode) = COND #( WHEN iv_mode = zif_al30_data=>cv_mode_change OR iv_mode = zif_al30_data=>cv_mode_view THEN iv_mode ELSE zif_al30_data=>cv_mode_view ).

    " Se leen los datos básicos para poder acceder a la clase de datos
    read_view_conf_for_data( EXPORTING iv_view_name = iv_view_name
                                       iv_langu = iv_langu
                             IMPORTING es_return = DATA(ls_return) ).

    IF ls_return IS INITIAL.

      " Se recupera el catalogo de campos
      CALL METHOD mo_controller->get_fieldcat_view
        EXPORTING
          iv_mode     = lv_mode
        IMPORTING
          es_return   = ls_return
          et_fieldcat = DATA(lt_fieldcat).

      LOOP AT lt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fieldcat>).
        DATA(ls_fields) = CORRESPONDING zif_al30_ui5_data=>ts_view_fields( <ls_fieldcat> ).
        ls_fields-len = <ls_fieldcat>-intlen.
        ls_fields-type = <ls_fieldcat>-inttype.
        INSERT ls_fields INTO TABLE et_fields.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD read_view_conf_for_data.

    CLEAR es_return.

    " La lectura de datos se hace en varios pasos.
    " 1) Se leen los datos de la vista en formato ALV. Esto es necesario para reaprovechar el resto de método
    CALL METHOD mo_controller->read_view_alv
      EXPORTING
        iv_name_view            = iv_view_name
        iv_all_language         = abap_false
        iv_langu                = iv_langu
      IMPORTING
        es_view                 = DATA(ls_view)
        et_fields_view_alv      = DATA(lt_fields)
        et_fields_text_view_alv = DATA(lt_fields_text)
        et_fields_ddic          = DATA(lt_fields_ddic)
        es_return               = es_return.

    IF es_return IS INITIAL. " Si no hay errores se continua el proceso

      " Si la vista tiene configurada exit se llama al proceso que la instancia
      IF ls_view-exit_class IS NOT INITIAL.
        mo_controller->instance_exit_class( ls_view-exit_class ).
      ENDIF.

      " 2) Se pasan los datos leídos de la configuración a la clase de obtención de valores
      mo_controller->set_data_conf_view(
    EXPORTING
      it_fields_view_alv      = lt_fields
      it_fields_text_view_alv =  lt_fields_text
      is_view                 = ls_view
      it_fields_ddic = lt_fields_ddic  ).

    ENDIF.
  ENDMETHOD.
  METHOD lock_view.
    TRY.

        mo_controller->lock_view( ).

        ev_locked = abap_true.

      CATCH zcx_al30 INTO DATA(lx_excep).
        ev_locked = abap_false.

    ENDTRY.
  ENDMETHOD.

ENDCLASS.
