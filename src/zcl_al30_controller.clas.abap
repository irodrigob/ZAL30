CLASS zcl_al30_controller DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

*"* public components of class ZCL_AL30_CONTROLLER
*"* do not include other source files here!!!
    METHODS constructor .
    "! <p class="shorttext synchronized">View List created</p>
    "!
    "! @parameter iv_langu | <p class="shorttext synchronized">language</p>
    "! @parameter it_r_views | <p class="shorttext synchronized">Views to filter</p>
    "! @parameter et_view_list | <p class="shorttext synchronized">View list</p>
    METHODS view_list
      IMPORTING
        !iv_langu     TYPE sylangu DEFAULT sy-langu
        !it_r_views   TYPE zif_al30_data=>tt_r_tabname OPTIONAL
      EXPORTING
        !et_view_list TYPE zcl_al30_view=>tt_view_list .
    METHODS read_view
      IMPORTING
        !iv_name_view        TYPE tabname
        !iv_read_ddic        TYPE sap_bool DEFAULT abap_true
        !iv_all_language     TYPE sap_bool DEFAULT abap_false
        !iv_langu            TYPE sylangu DEFAULT sy-langu
      EXPORTING
        !ev_text_view        TYPE as4text
        !es_return           TYPE bapiret2
        !es_view             TYPE zal30_t_view
        !et_fields_view      TYPE zif_al30_data=>tt_fields_view
        !et_fields_text_view TYPE zif_al30_data=>tt_fields_text_view
        !et_fields_ddic      TYPE dd03ptab
        !et_foreign_key_ddic TYPE dd05mttyp .
    METHODS read_view_alv
      IMPORTING
        !iv_name_view            TYPE tabname
        !iv_read_ddic            TYPE sap_bool DEFAULT abap_true
        !iv_all_language         TYPE sap_bool DEFAULT abap_false
        !iv_langu                TYPE sylangu DEFAULT sy-langu
      EXPORTING
        !ev_text_view            TYPE as4text
        !es_return               TYPE bapiret2
        !es_view                 TYPE zal30_t_view
        !et_fields_view_alv      TYPE zif_al30_data=>tt_fields_view_alv
        !et_fields_text_view_alv TYPE zif_al30_data=>tt_fields_text_view_alv
        !et_fields_ddic          TYPE dd03ptab
        !et_foreign_key_ddic     TYPE dd05mttyp .
    METHODS check_view
      IMPORTING
        !iv_name_view TYPE tabname
        !iv_operation TYPE zif_al30_data=>tv_operation
      EXPORTING
        !es_return    TYPE bapiret2
        !ev_text_view TYPE as4text .
    METHODS create_view
      IMPORTING
        !iv_name_view            TYPE tabname
        !iv_use_default_values   TYPE sap_bool DEFAULT abap_false
        !is_default_values       TYPE zif_al30_data=>ts_default_values_create OPTIONAL
      EXPORTING
        !es_return               TYPE bapiret2
        !ev_text_view            TYPE as4text
        !et_fields_view_alv      TYPE zif_al30_data=>tt_fields_view_alv
        !et_fields_text_view_alv TYPE zif_al30_data=>tt_fields_text_view_alv
        !et_fields_ddic          TYPE dd03ptab
        !es_view                 TYPE zal30_t_view .
    METHODS delete_view
      IMPORTING
        !iv_name_view    TYPE tabname
      RETURNING
        VALUE(rs_return) TYPE bapiret2 .
    METHODS save_view
      IMPORTING
        !is_view             TYPE zal30_t_view
        !it_fields_view      TYPE zif_al30_data=>tt_fields_view
        !it_fields_text_view TYPE zif_al30_data=>tt_fields_text_view
      RETURNING
        VALUE(rs_return)     TYPE bapiret2 .
    METHODS save_view_alv
      IMPORTING
        !is_view                 TYPE zal30_t_view
        !it_fields_text_view_alv TYPE zif_al30_data=>tt_fields_text_view_alv
        !it_fields_view_alv      TYPE zif_al30_data=>tt_fields_view_alv
      RETURNING
        VALUE(rs_return)         TYPE bapiret2 .
    METHODS read_data
      IMPORTING
        !is_filters TYPE zif_al30_data=>ts_filter_read_data
      EXPORTING
        !es_return  TYPE bapiret2
      CHANGING
        !co_data    TYPE REF TO data .
    METHODS get_fieldcat_view
      IMPORTING
        !iv_mode     TYPE char1
      EXPORTING
        !es_return   TYPE bapiret2
        !et_fieldcat TYPE lvc_t_fcat .
    METHODS create_it_data_view
      IMPORTING
        !iv_mode   TYPE char1
      EXPORTING
        !et_data   TYPE REF TO data
        !es_return TYPE bapiret2
        !es_data   TYPE REF TO data .
    METHODS save_data
      IMPORTING
        !iv_allow_request TYPE sap_bool DEFAULT abap_false
      EXPORTING
        !et_return        TYPE bapiret2_t
      CHANGING
        !ct_datos_del     TYPE STANDARD TABLE
        !ct_datos         TYPE STANDARD TABLE
        !cv_order         TYPE e070-trkorr .
    METHODS verify_field_data
      IMPORTING
        !iv_fieldname TYPE any
        !iv_value     TYPE any
      EXPORTING
        !es_return    TYPE bapiret2 .
    METHODS adjust_view_dictionary
      IMPORTING
        !iv_keep_text            TYPE sap_bool DEFAULT abap_true
        !iv_langu                TYPE sylangu DEFAULT sy-langu
      EXPORTING
        VALUE(es_return)         TYPE bapiret2
        !ev_text_view            TYPE as4text
      CHANGING
        !ct_fields_view_alv      TYPE zif_al30_data=>tt_fields_view_alv OPTIONAL
        !ct_fields_text_view_alv TYPE zif_al30_data=>tt_fields_text_view_alv OPTIONAL
        !cs_view                 TYPE zal30_t_view .
    METHODS check_changes_dict_view
      IMPORTING
        !it_fields_view_alv      TYPE zif_al30_data=>tt_fields_view_alv OPTIONAL
        !it_fields_text_view_alv TYPE zif_al30_data=>tt_fields_text_view_alv OPTIONAL
        !is_view                 TYPE zal30_t_view
        !iv_langu                TYPE sylangu DEFAULT sy-langu
        !iv_read_view            TYPE sap_bool DEFAULT abap_false
      EXPORTING
        !ev_diff_fields          TYPE sap_bool
        !ev_diff_text            TYPE sap_bool .
    METHODS check_sap_authorization
      IMPORTING
        !iv_view_name   TYPE tabname
        !iv_view_action TYPE any DEFAULT zif_al30_data=>cs_action_auth-update
      RAISING
        zcx_al30 .
    "! <p class="shorttext synchronized">Check the authorization level in view</p>
    "!
    "! @parameter iv_view_name | <p class="shorttext synchronized">View name</p>
    "! @parameter iv_view_action | <p class="shorttext synchronized">'U' Update 'S' Show</p>
    "! @parameter iv_user | <p class="shorttext synchronized">Username</p>
    "! @parameter rv_level_auth | <p class="shorttext synchronized">Level auth</p>
    METHODS check_authorization_view
      IMPORTING
        !iv_view_name        TYPE tabname
        !iv_view_action      TYPE any
        !iv_user             TYPE syuname
      RETURNING
        VALUE(rv_level_auth) TYPE zal30_e_level_auth .
    METHODS transport_data_entries
      IMPORTING
        !it_data         TYPE STANDARD TABLE
      CHANGING
        !cv_order        TYPE e070-trkorr
      RETURNING
        VALUE(rs_return) TYPE bapiret2 .
    METHODS check_exit_class
      IMPORTING
        !iv_exit_class   TYPE zal30_e_exit_class
      RETURNING
        VALUE(rs_return) TYPE bapiret2 .
    METHODS verify_change_row_data
      IMPORTING
        !iv_row      TYPE bapi_line
      EXPORTING
        !et_return   TYPE bapiret2_t
      CHANGING
        !cs_row_data TYPE any .
    METHODS get_logon_languages
      IMPORTING
        !iv_langu  TYPE sylangu DEFAULT sy-langu
      EXPORTING
        !et_lang   TYPE zif_al30_data=>tt_logon_lang
        !et_r_lang TYPE zif_al30_data=>tt_r_lang .
    METHODS allowed_transport
      RETURNING
        VALUE(rv_allowed) TYPE sap_bool .
    METHODS transport_view_alv
      IMPORTING
        !is_view                 TYPE zal30_t_view
        !iv_spras                TYPE sylangu DEFAULT sy-langu
        !it_fields_view_alv      TYPE zif_al30_data=>tt_fields_view_alv
        !it_fields_text_view_alv TYPE zif_al30_data=>tt_fields_text_view_alv
      EXPORTING
        !es_return               TYPE bapiret2
      CHANGING
        !cv_order                TYPE e070-trkorr .
    METHODS check_select_transport_order
      IMPORTING
        !iv_category TYPE e070-korrdev
      EXPORTING
        !es_return   TYPE bapiret2
      CHANGING
        !cv_order    TYPE e070-trkorr .
    METHODS view_have_user_auth
      IMPORTING
        !iv_view       TYPE tabname
      RETURNING
        VALUE(rv_have) TYPE sap_bool .
    METHODS view_have_sap_auth
      IMPORTING
        !iv_view       TYPE tabname
      RETURNING
        VALUE(rv_have) TYPE sap_bool .
    METHODS view_have_auto_adjust
      IMPORTING
        !iv_view       TYPE tabname
      RETURNING
        VALUE(rv_have) TYPE sap_bool .
    METHODS get_level_auth_view
      IMPORTING
        !iv_view             TYPE tabname
        !iv_user             TYPE syuname DEFAULT sy-uname
      RETURNING
        VALUE(rv_level_auth) TYPE zal30_e_level_auth .
    METHODS auto_adjust_view_ddic
      IMPORTING
        !iv_name_view            TYPE tabname
        !iv_read_view            TYPE sap_bool DEFAULT abap_false
      EXPORTING
        !es_return               TYPE bapiret2
        !et_fields_view_alv      TYPE zif_al30_data=>tt_fields_view_alv
        !et_fields_text_view_alv TYPE zif_al30_data=>tt_fields_text_view_alv
        !es_view                 TYPE zal30_t_view
        !et_foreign_key_ddic     TYPE dd05mttyp .
    "! <p class="shorttext synchronized">Lock view</p>
    METHODS lock_view
      IMPORTING
        !iv_view_name TYPE tabname OPTIONAL
        !iv_view_text TYPE tabname OPTIONAL
      RAISING
        zcx_al30 .
    "! <p class="shorttext synchronized">Instace exit class</p>
    METHODS instance_exit_class
      IMPORTING
        !iv_exit_class   TYPE zal30_e_exit_class
      RETURNING
        VALUE(rs_return) TYPE bapiret2 .
    METHODS set_data_conf_view
      IMPORTING
        !it_fields_view_alv      TYPE zif_al30_data=>tt_fields_view_alv
        !it_fields_text_view_alv TYPE zif_al30_data=>tt_fields_text_view_alv
        !it_fields_ddic          TYPE dd03ptab
        !is_view                 TYPE zal30_t_view
        !it_foreign_key_ddic     TYPE dd05mttyp .
    "! <p class="shorttext synchronized">Read info of a data element</p>
    "!
    "! @parameter iv_dtel | <p class="shorttext synchronized">Data element</p>
    "! @parameter es_info | <p class="shorttext synchronized">info</p>
    METHODS read_single_data_element
      IMPORTING
        !iv_dtel  TYPE rollname
        !iv_langu TYPE sylangu DEFAULT sy-langu
      EXPORTING
        !es_info  TYPE dfies
      RAISING
        zcx_al30 .
    "! <p class="shorttext synchronized">Read info in all languages</p>
    "!
    "! @parameter iv_dtel | <p class="shorttext synchronized">Data element</p>
    "! @parameter et_info | <p class="shorttext synchronized">Information in each language</p>
    METHODS read_data_element_all_lang
      IMPORTING
        !iv_dtel TYPE rollname
      EXPORTING
        !et_info TYPE zcl_al30_conf=>tt_info_data
      RAISING
        zcx_al30 .
    "! <p class="shorttext synchronized">Set editable mode the ALV Data</p>
    "!
    "! @parameter it_data | <p class="shorttext synchronized">Data</p>
    "! @parameter ev_edit_mode | <p class="shorttext synchronized">Edit mode</p>
    METHODS set_edit_mode_alv_data
      IMPORTING
        !it_data      TYPE STANDARD TABLE
      EXPORTING
        !ev_edit_mode TYPE cdchngind .
  PROTECTED SECTION.
*"* private components of class ZCL_AL30_CONTROLLER
*"* do not include other source files here!!!

    DATA mo_view TYPE REF TO zcl_al30_view.
    DATA mo_conf TYPE REF TO zcl_al30_conf.

    METHODS conv_view_from_alv
      IMPORTING
        it_fields_view_alv      TYPE zif_al30_data=>tt_fields_view_alv OPTIONAL
        it_fields_text_view_alv TYPE zif_al30_data=>tt_fields_text_view_alv OPTIONAL
      EXPORTING
        et_fields_view          TYPE zif_al30_data=>tt_fields_view
        et_fields_text_view     TYPE zif_al30_data=>tt_fields_text_view.
    METHODS conv_view_to_alv
      IMPORTING
        it_fields_view          TYPE zif_al30_data=>tt_fields_view OPTIONAL
        it_fields_text_view     TYPE zif_al30_data=>tt_fields_text_view OPTIONAL
      EXPORTING
        et_fields_view_alv      TYPE zif_al30_data=>tt_fields_view_alv
        et_fields_text_view_alv TYPE zif_al30_data=>tt_fields_text_view_alv.
*"* protected components of class ZCL_AL30_CONTROLLER
*"* do not include other source files here!!!
  PRIVATE SECTION.


ENDCLASS.



CLASS zcl_al30_controller IMPLEMENTATION.


  METHOD adjust_view_dictionary.

* Convierto los datos del alv a un formato compatible con la vista
    conv_view_from_alv( EXPORTING it_fields_view_alv = ct_fields_view_alv
                                  it_fields_text_view_alv = ct_fields_text_view_alv
                        IMPORTING et_fields_view = DATA(lt_fields_view)
                                 et_fields_text_view = DATA(lt_fields_text_view) ).

    mo_conf->adjust_view_dictionary(
      EXPORTING
        iv_keep_text        = iv_keep_text
        iv_langu = iv_langu
      IMPORTING
        es_return           = es_return
        ev_text_view        =  ev_text_view
      CHANGING
        ct_fields_view      = lt_fields_view
        ct_fields_text_view = lt_fields_text_view
        cs_view             = cs_view ).

* Al reves una vez modificados los datos
    conv_view_to_alv( EXPORTING it_fields_view = lt_fields_view
                                it_fields_text_view = lt_fields_text_view
                      IMPORTING et_fields_view_alv = ct_fields_view_alv
                                et_fields_text_view_alv = ct_fields_text_view_alv ).

  ENDMETHOD.


  METHOD allowed_transport.
    rv_allowed = zcl_al30_util=>allowed_transport( ).
  ENDMETHOD.


  METHOD auto_adjust_view_ddic.

    CLEAR es_return.

    " Se leen los campos de la vista si se ha indicado
    IF iv_read_view = abap_true.
      read_view_alv(
        EXPORTING
          iv_name_view        = iv_name_view
          iv_read_ddic = abap_false
        IMPORTING
          es_return           = es_return
          es_view             = es_view
          et_fields_view_alv      = et_fields_view_alv
          et_fields_text_view_alv =  et_fields_text_view_alv
          et_foreign_key_ddic = et_foreign_key_ddic ).

    ENDIF.

    " Si no hay errores se hace el ajuste
    IF es_return-type NE zif_al30_data=>cs_msg_type-error.

      adjust_view_dictionary(
        EXPORTING
          iv_keep_text            = abap_true
        IMPORTING
          es_return               = es_return
        CHANGING
          ct_fields_view_alv      = et_fields_view_alv
          ct_fields_text_view_alv = et_fields_text_view_alv
          cs_view                 = es_view ).

    ENDIF.
  ENDMETHOD.


  METHOD check_authorization_view.


    " Primera validación es si tiene autorización a medida por tabla, si la tabla lo tiene habilitado.
    "Si no lo tiene se marca el nivel que se le haya pasado. Si no se le ha pasado nada o se le pasa actualización es que tiene acceso a todo
    rv_level_auth = COND #( WHEN view_have_user_auth( iv_view_name ) = abap_true
                            THEN get_level_auth_view( iv_user = COND #( WHEN iv_user IS INITIAL THEN sy-uname ELSE iv_user ) iv_view = iv_view_name )
                            ELSE COND #( WHEN iv_view_action = zif_al30_data=>cs_action_auth-show
                                         THEN zif_al30_data=>cs_level_auth_user-read ELSE zif_al30_data=>cs_level_auth_user-full ) ).

    " Si en la primera no tiene autorización ya no se valida el resto
    IF rv_level_auth NE zif_al30_data=>cs_level_auth_user-non.

      " Segunda si tiene habilitada la áutorización a nivel de SAP se ejecuta según el nivel de autorización del paso anterior.
      " Si no tiene habilitado que valide la autorización en SAP se queda con el nivel anterior.
      TRY.
          " Se determina la acción de la vista para pasarsela al método
          DATA(lv_view_action) = COND zif_al30_data=>tv_action_auth( WHEN rv_level_auth = zif_al30_data=>cs_level_auth_user-read THEN zif_al30_data=>cs_action_auth-show
                                                                     ELSE zif_al30_data=>cs_action_auth-update ).
          IF view_have_sap_auth( iv_view_name ) = abap_true.
            check_sap_authorization( iv_view_name = iv_view_name
                                          iv_view_action = lv_view_action ).
          ENDIF.

        CATCH zcx_al30 . " Si hay excepción no tendrá autorización
          rv_level_auth = zif_al30_data=>cs_level_auth_user-non.
      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD check_changes_dict_view.

* Si los campos de la vista no se pasán por parámetro se obtiene la vista por completo
    IF iv_read_view = abap_false.
* Convierto los datos del alv a un formato compatible con la vista
      conv_view_from_alv( EXPORTING it_fields_view_alv = it_fields_view_alv
                                    it_fields_text_view_alv = it_fields_text_view_alv
                          IMPORTING et_fields_text_view = DATA(lt_fields_text_view)
                                    et_fields_view = DATA(lt_fields_view) ).
      DATA(ls_view) = is_view.
    ELSE.
      read_view(
        EXPORTING
          iv_name_view        = is_view-tabname
          iv_read_ddic        = abap_false
        IMPORTING
          es_view = ls_view
          et_fields_view      = lt_fields_view
          et_fields_text_view =  lt_fields_text_view ).

    ENDIF.
    mo_conf->check_changes_dict( EXPORTING is_view = ls_view
                                                        it_fields  = lt_fields_view
                                                        it_fields_text = lt_fields_text_view
                                                        iv_langu = iv_langu
                                              IMPORTING ev_diff_fields = ev_diff_fields
                                                        ev_diff_text = ev_diff_text ).

  ENDMETHOD.


  METHOD check_exit_class.

    rs_return = mo_conf->check_exit_class( iv_exit_class = iv_exit_class ).

  ENDMETHOD.


  METHOD check_sap_authorization.


    TRY.
        CALL METHOD mo_view->check_sap_authorization
          EXPORTING
            iv_view_name   = iv_view_name
            iv_view_action = iv_view_action.
      CATCH zcx_al30 .
        RAISE EXCEPTION TYPE zcx_al30
          EXPORTING
            textid = zcx_al30=>no_authorization.
    ENDTRY.





  ENDMETHOD.


  METHOD check_select_transport_order.

    NEW zcl_al30_util( )->check_select_transport_order(
      EXPORTING
        iv_category = iv_category
      IMPORTING
        es_return   = es_return
      CHANGING
        cv_order    = cv_order ).

  ENDMETHOD.


  METHOD check_view.

    CASE iv_operation.

      WHEN zif_al30_data=>cv_operation_insert. " Insert new view

        mo_conf->check_view_insert( EXPORTING iv_name_view = iv_name_view
                                     IMPORTING es_return = es_return
                                               ev_text_view = ev_text_view ).

      WHEN zif_al30_data=>cv_operation_read. " Read the view existing

        mo_conf->check_view_read( EXPORTING iv_name_view = iv_name_view
                                     IMPORTING es_return = es_return
                                               ev_text_view = ev_text_view ).

      WHEN OTHERS.
        es_return = zcl_al30_util=>fill_return( iv_type = 'E' iv_number = '040' iv_message_v1 = iv_operation ).
    ENDCASE.

  ENDMETHOD.


  METHOD constructor.

    " Se instancias las clases encargadas de gestionar la configuración y la vista
    mo_conf = NEW zcl_al30_conf( ).
    mo_view = NEW zcl_al30_view( ).

  ENDMETHOD.


  METHOD conv_view_from_alv.

    IF it_fields_view_alv IS SUPPLIED.
      CLEAR et_fields_view.
      et_fields_view = CORRESPONDING #( it_fields_view_alv ).
    ENDIF.

    IF it_fields_text_view_alv IS SUPPLIED.
      CLEAR et_fields_text_view.
      et_fields_text_view = CORRESPONDING #( it_fields_text_view_alv ).
    ENDIF.


  ENDMETHOD.


  METHOD conv_view_to_alv.

    IF it_fields_view IS SUPPLIED.
      CLEAR et_fields_view_alv.
      et_fields_view_alv = CORRESPONDING #( it_fields_view ).
    ENDIF.

    IF it_fields_text_view IS SUPPLIED.
      CLEAR et_fields_text_view_alv.
      et_fields_text_view_alv = CORRESPONDING #( it_fields_text_view ).
    ENDIF.

  ENDMETHOD.


  METHOD create_it_data_view.

    CALL METHOD mo_view->create_it_data_view
      EXPORTING
        iv_mode   = iv_mode
      IMPORTING
        es_return = es_return
        et_data   = et_data
        es_data   = es_data.

  ENDMETHOD.


  METHOD create_view.

* Primero verifico que la vista sea correcta
    check_view( EXPORTING iv_name_view = iv_name_view
                          iv_operation = 'I'
                IMPORTING es_return = es_return
                          ev_text_view = ev_text_view ).

    IF es_return-type NE zif_al30_data=>cs_msg_type-error.

* Ahora llamo a la creación
      mo_conf->insert_view( EXPORTING iv_name_view = iv_name_view
                                      iv_use_default_values = iv_use_default_values
                                      is_default_values = is_default_values
                                   IMPORTING es_return = es_return
                                              et_fields      = DATA(lt_fields)
                                              et_fields_text = DATA(lt_fields_text)
                                              et_fields_ddic = et_fields_ddic
                                              es_view = es_view ).

      " Conversión a formato ALV
      conv_view_to_alv(
        EXPORTING
          it_fields_view          = lt_fields
          it_fields_text_view     = lt_fields_text
        IMPORTING
          et_fields_view_alv      = et_fields_view_alv
          et_fields_text_view_alv =  et_fields_text_view_alv ).

    ENDIF.

  ENDMETHOD.


  METHOD delete_view.

    rs_return = mo_conf->delete_view( iv_name_view ).

  ENDMETHOD.


  METHOD get_fieldcat_view.

    mo_view->get_fieldcat_view(
      EXPORTING
        iv_mode = iv_mode
      IMPORTING
        es_return       = es_return
        et_fieldcat     = et_fieldcat ).

  ENDMETHOD.


  METHOD get_level_auth_view.
    rv_level_auth = mo_view->get_level_auth_view( iv_view = iv_view iv_user = iv_user ).
  ENDMETHOD.


  METHOD get_logon_languages.

    mo_conf->get_logon_languages( EXPORTING iv_langu  = iv_langu
                                               IMPORTING et_lang   = et_lang
                                                         et_r_lang = et_r_lang ).

  ENDMETHOD.


  METHOD instance_exit_class.
    rs_return = mo_view->instance_exit_class( iv_exit_class ).
  ENDMETHOD.


  METHOD lock_view.

    mo_view->lock_view( EXPORTING iv_view_name = iv_view_name
                                  iv_view_text = iv_view_text ).

  ENDMETHOD.


  METHOD read_data.

    CLEAR es_return.

    mo_view->read_data( EXPORTING is_filters = is_filters
                        IMPORTING es_return = es_return
                        CHANGING co_data = co_data ).

  ENDMETHOD.


  METHOD read_data_element_all_lang.
    mo_conf->read_data_element_all_lang( EXPORTING iv_rollname = iv_dtel
                                         IMPORTING et_info = et_info ).
  ENDMETHOD.


  METHOD read_single_data_element.
    mo_conf->read_single_data_element( EXPORTING iv_rollname = iv_dtel
                                       IMPORTING es_info = es_info ).
  ENDMETHOD.


  METHOD read_view.

    CLEAR es_return.

* Leo los datos de la vista
    CALL METHOD mo_conf->read_view
      EXPORTING
        iv_name_view        = iv_name_view
        iv_read_ddic        = iv_read_ddic
        iv_langu            = iv_langu
        iv_all_language     = iv_all_language
      IMPORTING
        ev_text_view        = ev_text_view
        es_return           = es_return
        es_view             = es_view
        et_fields           = et_fields_view
        et_fields_text      = et_fields_text_view
        et_fields_ddic      = et_fields_ddic
        et_foreign_key_ddic = et_foreign_key_ddic.

  ENDMETHOD.


  METHOD read_view_alv.


    CLEAR es_return.

* Leo los datos de la vista
    CALL METHOD read_view
      EXPORTING
        iv_name_view        = iv_name_view
        iv_read_ddic        = iv_read_ddic
        iv_all_language     = iv_all_language
      IMPORTING
        ev_text_view        = ev_text_view
        es_return           = es_return
        es_view             = es_view
        et_fields_view      = DATA(lt_fields_view)
        et_fields_text_view = DATA(lt_fields_text_view)
        et_fields_ddic      = et_fields_ddic
        et_foreign_key_ddic = et_foreign_key_ddic.

* Convierto los datos al formato ALV
    conv_view_to_alv( EXPORTING it_fields_view = lt_fields_view
                                it_fields_text_view = lt_fields_text_view
                      IMPORTING et_fields_view_alv = et_fields_view_alv
                                et_fields_text_view_alv = et_fields_text_view_alv ).


  ENDMETHOD.


  METHOD save_data.

    CALL METHOD mo_view->save_data
      EXPORTING
        iv_allow_request = iv_allow_request
      IMPORTING
        et_return        = et_return
      CHANGING
        cv_order         = cv_order
        ct_datos         = ct_datos
        ct_datos_del     = ct_datos_del.

  ENDMETHOD.


  METHOD save_view.

    CLEAR rs_return.


    rs_return = mo_conf->save_view( is_view = is_view
                                   it_fields  = it_fields_view
                                   it_fields_text = it_fields_text_view ).

  ENDMETHOD.


  METHOD save_view_alv.

* convierto los datos del alv a un formato compatible con la vista
    conv_view_from_alv( EXPORTING it_fields_view_alv = it_fields_view_alv
                                  it_fields_text_view_alv = it_fields_text_view_alv
                        IMPORTING et_fields_view = DATA(lt_fields_view)
                                  et_fields_text_view = DATA(lt_fields_text_view) ).

    rs_return = save_view( is_view = is_view
                           it_fields_view  = lt_fields_view
                           it_fields_text_view = lt_fields_text_view ).


  ENDMETHOD.


  METHOD set_data_conf_view.

    conv_view_from_alv( EXPORTING it_fields_view_alv = it_fields_view_alv
                                it_fields_text_view_alv = it_fields_text_view_alv
                      IMPORTING et_fields_view = DATA(lt_fields_view)
                                et_fields_text_view = DATA(lt_fields_text_view) ).


    mo_view->set_data_conf_view( is_view = is_view
                                 it_fields_view = lt_fields_view
                                 it_fields_text_view = lt_fields_text_view
                                 it_fields_ddic = it_fields_ddic
                                 it_foreign_key_ddic = it_foreign_key_ddic ).
  ENDMETHOD.


  METHOD set_edit_mode_alv_data.
    mo_view->set_edit_mode_alv( EXPORTING it_data = it_data
                                IMPORTING ev_edit_mode = ev_edit_mode ).
  ENDMETHOD.


  METHOD transport_data_entries.

    rs_return = mo_view->transport_entries( EXPORTING it_data = it_data
                                            CHANGING cv_order = cv_order ).


  ENDMETHOD.


  METHOD transport_view_alv.

* Convierto los datos del alv a un formato compatible con la vista
    conv_view_from_alv( EXPORTING it_fields_view_alv = it_fields_view_alv
                                  it_fields_text_view_alv = it_fields_text_view_alv
                        IMPORTING et_fields_view = DATA(lt_fields_view)
                                  et_fields_text_view = DATA(lt_fields_text_view) ).

    mo_conf->transport_view(
                 EXPORTING is_view = is_view
                           it_fields_view = lt_fields_view
                           it_fields_text_view = lt_fields_text_view
                 IMPORTING es_return = es_return
                 CHANGING cv_order = cv_order           ).

  ENDMETHOD.


  METHOD verify_change_row_data.

    CLEAR et_return.

    mo_view->verify_change_row_data( EXPORTING iv_row = iv_row
                                     IMPORTING et_return     = et_return
                                     CHANGING cs_row_data   = cs_row_data ).

  ENDMETHOD.


  METHOD verify_field_data.


    CLEAR es_return.

    mo_view->verify_field_data( EXPORTING iv_fieldname = iv_fieldname
                                iv_value = iv_value
                                IMPORTING es_return = es_return ).


  ENDMETHOD.


  METHOD view_have_auto_adjust.
    rv_have = mo_view->view_have_auto_adjust( iv_view ).
  ENDMETHOD.


  METHOD view_have_sap_auth.
    rv_have = mo_view->view_have_sap_auth( iv_view ).
  ENDMETHOD.


  METHOD view_have_user_auth.
    rv_have = mo_view->view_have_user_auth( iv_view ).
  ENDMETHOD.


  METHOD view_list.
    mo_view->view_list( EXPORTING iv_langu = iv_langu
                                  it_r_views = it_r_views
                        IMPORTING et_view_list = et_view_list ).
  ENDMETHOD.
ENDCLASS.
