class ZCL_AL30_CONTROLLER definition
  public
  final
  create public .

public section.

*"* public components of class ZCL_AL30_CONTROLLER
*"* do not include other source files here!!!
  methods CONSTRUCTOR .
    "! <p class="shorttext synchronized">View List created</p>
    "!
    "! @parameter iv_langu | <p class="shorttext synchronized">language</p>
    "! @parameter it_r_views | <p class="shorttext synchronized">Views to filter</p>
    "! @parameter et_view_list | <p class="shorttext synchronized">View list</p>
  methods VIEW_LIST
    importing
      !IV_LANGU type SYLANGU default SY-LANGU
      !IT_R_VIEWS type ZIF_AL30_DATA=>TT_R_TABNAME optional
    exporting
      !ET_VIEW_LIST type ZCL_AL30_VIEW=>TT_VIEW_LIST .
  methods READ_VIEW
    importing
      !IV_NAME_VIEW type TABNAME
      !IV_READ_DDIC type SAP_BOOL default ABAP_TRUE
      !IV_ALL_LANGUAGE type SAP_BOOL default ABAP_FALSE
      !IV_LANGU type SYLANGU default SY-LANGU
    exporting
      !EV_TEXT_VIEW type AS4TEXT
      !ES_RETURN type BAPIRET2
      !ES_VIEW type ZAL30_T_VIEW
      !ET_FIELDS_VIEW type ZIF_AL30_DATA=>TT_FIELDS_VIEW
      !ET_FIELDS_TEXT_VIEW type ZIF_AL30_DATA=>TT_FIELDS_TEXT_VIEW
      !ET_FIELDS_DDIC type DD03PTAB
      !ET_FOREIGN_KEY_DDIC type DD05MTTYP .
  methods READ_VIEW_ALV
    importing
      !IV_NAME_VIEW type TABNAME
      !IV_READ_DDIC type SAP_BOOL default ABAP_TRUE
      !IV_ALL_LANGUAGE type SAP_BOOL default ABAP_FALSE
      !IV_LANGU type SYLANGU default SY-LANGU
    exporting
      !EV_TEXT_VIEW type AS4TEXT
      !ES_RETURN type BAPIRET2
      !ES_VIEW type ZAL30_T_VIEW
      !ET_FIELDS_VIEW_ALV type ZIF_AL30_DATA=>TT_FIELDS_VIEW_ALV
      !ET_FIELDS_TEXT_VIEW_ALV type ZIF_AL30_DATA=>TT_FIELDS_TEXT_VIEW_ALV
      !ET_FIELDS_DDIC type DD03PTAB
      !ET_FOREIGN_KEY_DDIC type DD05MTTYP .
  methods CHECK_VIEW
    importing
      !IV_NAME_VIEW type TABNAME
      !IV_OPERATION type ZIF_AL30_DATA=>TV_OPERATION
    exporting
      !ES_RETURN type BAPIRET2
      !EV_TEXT_VIEW type AS4TEXT .
  methods CREATE_VIEW
    importing
      !IV_NAME_VIEW type TABNAME
      !IV_USE_DEFAULT_VALUES type SAP_BOOL default ABAP_FALSE
      !IS_DEFAULT_VALUES type ZIF_AL30_DATA=>TS_DEFAULT_VALUES_CREATE optional
    exporting
      !ES_RETURN type BAPIRET2
      !EV_TEXT_VIEW type AS4TEXT
      !ET_FIELDS_VIEW_ALV type ZIF_AL30_DATA=>TT_FIELDS_VIEW_ALV
      !ET_FIELDS_TEXT_VIEW_ALV type ZIF_AL30_DATA=>TT_FIELDS_TEXT_VIEW_ALV
      !ET_FIELDS_DDIC type DD03PTAB
      !ES_VIEW type ZAL30_T_VIEW .
  methods DELETE_VIEW
    importing
      !IV_NAME_VIEW type TABNAME
    returning
      value(RS_RETURN) type BAPIRET2 .
  methods SAVE_VIEW
    importing
      !IS_VIEW type ZAL30_T_VIEW
      !IT_FIELDS_VIEW type ZIF_AL30_DATA=>TT_FIELDS_VIEW
      !IT_FIELDS_TEXT_VIEW type ZIF_AL30_DATA=>TT_FIELDS_TEXT_VIEW
    returning
      value(RS_RETURN) type BAPIRET2 .
  methods SAVE_VIEW_ALV
    importing
      !IS_VIEW type ZAL30_T_VIEW
      !IT_FIELDS_TEXT_VIEW_ALV type ZIF_AL30_DATA=>TT_FIELDS_TEXT_VIEW_ALV
      !IT_FIELDS_VIEW_ALV type ZIF_AL30_DATA=>TT_FIELDS_VIEW_ALV
    returning
      value(RS_RETURN) type BAPIRET2 .
  methods READ_DATA
    importing
      !IS_FILTERS type ZIF_AL30_DATA=>TS_FILTER_READ_DATA
    exporting
      !ES_RETURN type BAPIRET2
    changing
      !CO_DATA type ref to DATA .
  methods GET_FIELDCAT_VIEW
    importing
      !IV_MODE type CHAR1
    exporting
      !ES_RETURN type BAPIRET2
      !ET_FIELDCAT type LVC_T_FCAT .
  methods CREATE_IT_DATA_VIEW
    importing
      !IV_MODE type CHAR1
    exporting
      !ET_DATA type ref to DATA
      !ES_RETURN type BAPIRET2
      !ES_DATA type ref to DATA .
  methods SAVE_DATA
    importing
      !IV_ALLOW_REQUEST type SAP_BOOL default ABAP_FALSE
    exporting
      !ET_RETURN type BAPIRET2_T
    changing
      !CT_DATOS_DEL type STANDARD TABLE
      !CT_DATOS type STANDARD TABLE
      !CV_ORDER type E070-TRKORR .
  methods VERIFY_FIELD_DATA
    importing
      !IV_FIELDNAME type ANY
      !IV_VALUE type ANY
    exporting
      !ES_RETURN type BAPIRET2 .
  methods ADJUST_VIEW_DICTIONARY
    importing
      !IV_KEEP_TEXT type SAP_BOOL default ABAP_TRUE
      !IV_LANGU type SYLANGU default SY-LANGU
    exporting
      value(ES_RETURN) type BAPIRET2
      !EV_TEXT_VIEW type AS4TEXT
    changing
      !CT_FIELDS_VIEW_ALV type ZIF_AL30_DATA=>TT_FIELDS_VIEW_ALV optional
      !CT_FIELDS_TEXT_VIEW_ALV type ZIF_AL30_DATA=>TT_FIELDS_TEXT_VIEW_ALV optional
      !CS_VIEW type ZAL30_T_VIEW .
  methods CHECK_CHANGES_DICT_VIEW
    importing
      !IT_FIELDS_VIEW_ALV type ZIF_AL30_DATA=>TT_FIELDS_VIEW_ALV optional
      !IT_FIELDS_TEXT_VIEW_ALV type ZIF_AL30_DATA=>TT_FIELDS_TEXT_VIEW_ALV optional
      !IS_VIEW type ZAL30_T_VIEW
      !IV_LANGU type SYLANGU default SY-LANGU
      !IV_READ_VIEW type SAP_BOOL default ABAP_FALSE
    exporting
      !EV_DIFF_FIELDS type SAP_BOOL
      !EV_DIFF_TEXT type SAP_BOOL .
  methods CHECK_SAP_AUTHORIZATION
    importing
      !IV_VIEW_NAME type TABNAME
      !IV_VIEW_ACTION type ANY default ZIF_AL30_DATA=>CS_ACTION_AUTH-UPDATE
    raising
      ZCX_AL30 .
    "! <p class="shorttext synchronized">Check the authorization level in view</p>
    "!
    "! @parameter iv_view_name | <p class="shorttext synchronized">View name</p>
    "! @parameter iv_view_action | <p class="shorttext synchronized">'U' Update 'S' Show</p>
    "! @parameter iv_user | <p class="shorttext synchronized">Username</p>
    "! @parameter rv_level_auth | <p class="shorttext synchronized">Level auth</p>
  methods CHECK_AUTHORIZATION_VIEW
    importing
      !IV_VIEW_NAME type TABNAME
      !IV_VIEW_ACTION type ANY
      !IV_USER type SYUNAME
    returning
      value(RV_LEVEL_AUTH) type ZAL30_E_LEVEL_AUTH .
  methods TRANSPORT_DATA_ENTRIES
    importing
      !IT_DATA type STANDARD TABLE
    changing
      !CV_ORDER type E070-TRKORR
    returning
      value(RS_RETURN) type BAPIRET2 .
  methods CHECK_EXIT_CLASS
    importing
      !IV_EXIT_CLASS type ZAL30_E_EXIT_CLASS
    returning
      value(RS_RETURN) type BAPIRET2 .
  methods VERIFY_CHANGE_ROW_DATA
    importing
      !IV_ROW type BAPI_LINE
    exporting
      !ET_RETURN type BAPIRET2_T
    changing
      !CS_ROW_DATA type ANY .
  methods GET_LOGON_LANGUAGES
    importing
      !IV_LANGU type SYLANGU default SY-LANGU
    exporting
      !ET_LANG type ZIF_AL30_DATA=>TT_LOGON_LANG
      !ET_R_LANG type ZIF_AL30_DATA=>TT_R_LANG .
  methods ALLOWED_TRANSPORT
    returning
      value(RV_ALLOWED) type SAP_BOOL .
  methods TRANSPORT_VIEW_ALV
    importing
      !IS_VIEW type ZAL30_T_VIEW
      !IV_SPRAS type SYLANGU default SY-LANGU
      !IT_FIELDS_VIEW_ALV type ZIF_AL30_DATA=>TT_FIELDS_VIEW_ALV
      !IT_FIELDS_TEXT_VIEW_ALV type ZIF_AL30_DATA=>TT_FIELDS_TEXT_VIEW_ALV
    exporting
      !ES_RETURN type BAPIRET2
    changing
      !CV_ORDER type E070-TRKORR .
  methods CHECK_SELECT_TRANSPORT_ORDER
    importing
      !IV_CATEGORY type E070-KORRDEV
    exporting
      !ES_RETURN type BAPIRET2
    changing
      !CV_ORDER type E070-TRKORR .
  methods VIEW_HAVE_USER_AUTH
    importing
      !IV_VIEW type TABNAME
    returning
      value(RV_HAVE) type SAP_BOOL .
  methods VIEW_HAVE_SAP_AUTH
    importing
      !IV_VIEW type TABNAME
    returning
      value(RV_HAVE) type SAP_BOOL .
  methods VIEW_HAVE_AUTO_ADJUST
    importing
      !IV_VIEW type TABNAME
    returning
      value(RV_HAVE) type SAP_BOOL .
  methods GET_LEVEL_AUTH_VIEW
    importing
      !IV_VIEW type TABNAME
      !IV_USER type SYUNAME default SY-UNAME
    returning
      value(RV_LEVEL_AUTH) type ZAL30_E_LEVEL_AUTH .
  methods AUTO_ADJUST_VIEW_DDIC
    importing
      !IV_NAME_VIEW type TABNAME
      !IV_READ_VIEW type SAP_BOOL default ABAP_FALSE
    exporting
      !ES_RETURN type BAPIRET2
      !ET_FIELDS_VIEW_ALV type ZIF_AL30_DATA=>TT_FIELDS_VIEW_ALV
      !ET_FIELDS_TEXT_VIEW_ALV type ZIF_AL30_DATA=>TT_FIELDS_TEXT_VIEW_ALV
      !ES_VIEW type ZAL30_T_VIEW
      !ET_FOREIGN_KEY_DDIC type DD05MTTYP .
    "! <p class="shorttext synchronized">Lock view</p>
  methods LOCK_VIEW
    importing
      !IV_VIEW_NAME type TABNAME optional
      !IV_VIEW_TEXT type TABNAME optional
    raising
      ZCX_AL30 .
    "! <p class="shorttext synchronized">Instace exit class</p>
  methods INSTANCE_EXIT_CLASS
    importing
      !IV_EXIT_CLASS type ZAL30_E_EXIT_CLASS
    returning
      value(RS_RETURN) type BAPIRET2 .
  methods SET_DATA_CONF_VIEW
    importing
      !IT_FIELDS_VIEW_ALV type ZIF_AL30_DATA=>TT_FIELDS_VIEW_ALV
      !IT_FIELDS_TEXT_VIEW_ALV type ZIF_AL30_DATA=>TT_FIELDS_TEXT_VIEW_ALV
      !IT_FIELDS_DDIC type DD03PTAB
      !IS_VIEW type ZAL30_T_VIEW
      !IT_FOREIGN_KEY_DDIC type DD05MTTYP .
    "! <p class="shorttext synchronized">Read info of a data element</p>
    "!
    "! @parameter iv_dtel | <p class="shorttext synchronized">Data element</p>
    "! @parameter es_info | <p class="shorttext synchronized">info</p>
  methods READ_SINGLE_DATA_ELEMENT
    importing
      !IV_DTEL type ROLLNAME
      !IV_LANGU type SYLANGU default SY-LANGU
    exporting
      !ES_INFO type DFIES
    raising
      ZCX_AL30 .
    "! <p class="shorttext synchronized">Read info in all languages</p>
    "!
    "! @parameter iv_dtel | <p class="shorttext synchronized">Data element</p>
    "! @parameter et_info | <p class="shorttext synchronized">Information in each language</p>
  methods READ_DATA_ELEMENT_ALL_LANG
    importing
      !IV_DTEL type ROLLNAME
    exporting
      !ET_INFO type ZCL_AL30_CONF=>TT_INFO_DATA
    raising
      ZCX_AL30 .
    "! <p class="shorttext synchronized">Set editable mode the ALV Data</p>
    "!
    "! @parameter it_data | <p class="shorttext synchronized">Data</p>
    "! @parameter ev_edit_mode | <p class="shorttext synchronized">Edit mode</p>
  methods SET_EDIT_MODE_ALV_DATA
    importing
      !IT_DATA type STANDARD TABLE
    exporting
      !EV_EDIT_MODE type CDCHNGIND .
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



CLASS ZCL_AL30_CONTROLLER IMPLEMENTATION.


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
        ET_FOREIGN_KEY_DDIC = ET_FOREIGN_KEY_DDIC.

* Convierto los datos al formato ALV
    conv_view_to_alv( EXPORTING it_fields_view = lt_fields_view
                                it_fields_text_view = lt_fields_text_view
                      IMPORTING et_fields_view_alv = et_fields_view_alv
                                et_fields_text_view_alv = et_fields_text_view_alv ).


  ENDMETHOD.


  METHOD save_data.

    mo_view->verify_save_data(
      EXPORTING
        it_data_del = ct_datos_del
        iv_save_process = abap_true
      IMPORTING
        et_return = DATA(lt_return)
      CHANGING ct_data   = ct_datos ).

    " Si hay errores no se continua el proceso
    IF ( line_exists( lt_return[ type = zif_al30_data=>cs_msg_type-error ] ) OR
        line_exists( lt_return[ type = zif_al30_data=>cs_msg_type-dump ] ) ).
      et_return = lt_return.
    ELSE.
      " Se valida que no haya ningúna línea errónea.
      READ TABLE ct_datos TRANSPORTING NO FIELDS WITH KEY (zif_al30_data=>cs_control_fields_alv_data-row_status) = zif_al30_data=>cs_msg_type-error.
      IF sy-subrc NE 0.

        CALL METHOD mo_view->save_data
          EXPORTING
            iv_allow_request = iv_allow_request
          IMPORTING
            et_return        = et_return
          CHANGING
            cv_order         = cv_order
            ct_datos         = ct_datos
            ct_datos_del     = ct_datos_del.

        " Se añaden los posibles mensajes de la validación a los que devuelva el propio método
        INSERT LINES OF lt_return INTO TABLE et_return.

      ENDIF.

    ENDIF.

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
