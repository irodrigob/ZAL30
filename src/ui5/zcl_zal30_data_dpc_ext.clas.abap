CLASS zcl_zal30_data_dpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zcl_zal30_data_dpc
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .
  PROTECTED SECTION.

    DATA mo_controller TYPE REF TO zcl_al30_gw_controller .

    METHODS checkauthviewset_get_entity
        REDEFINITION .
    METHODS getviewsset_get_entityset
        REDEFINITION .
    METHODS lockviewset_get_entity
        REDEFINITION .
    METHODS readdataset_get_entity
        REDEFINITION .
    METHODS readviewset_get_entityset
        REDEFINITION .
    METHODS rowvalidationdet_create_entity
        REDEFINITION .
    METHODS rowvalidationdet_get_entity REDEFINITION.
    METHODS verifyfielddatas_get_entity REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_zal30_data_dpc_ext IMPLEMENTATION.


  METHOD checkauthviewset_get_entity.

    " Se recupera la vista pasada
    READ TABLE it_key_tab ASSIGNING FIELD-SYMBOL(<ls_key_tab>) WITH KEY name = 'VIEWNAME'.
    IF sy-subrc = 0.
      er_entity-level_auth = mo_controller->check_authorization_view( iv_view_name   = CONV #( <ls_key_tab>-value ) ).
      er_entity-view_name = <ls_key_tab>-value.
    ENDIF.

  ENDMETHOD.


  METHOD constructor.
    super->constructor(  ).

    mo_controller = NEW zcl_al30_gw_controller(  ). " Se instancia el controlador para GW
  ENDMETHOD.


  METHOD getviewsset_get_entityset.
    DATA lv_langu TYPE sylangu.
    DATA lv_user TYPE string.
    DATA lt_r_views TYPE zif_al30_data=>tt_r_tabname.

    CLEAR et_entityset.

    " Se recupera los valores de los filtros

    " Idioma
    READ TABLE it_filter_select_options ASSIGNING FIELD-SYMBOL(<ls_filter>) WITH KEY property = 'LANGU'.
    IF sy-subrc = 0.
      READ TABLE <ls_filter>-select_options ASSIGNING FIELD-SYMBOL(<ls_select_options>) INDEX 1.
      CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
        EXPORTING
          input            = <ls_select_options>-low
        IMPORTING
          output           = lv_langu
        EXCEPTIONS
          unknown_language = 1
          OTHERS           = 2.

    ENDIF.

    " Usuario
    READ TABLE it_filter_select_options ASSIGNING <ls_filter> WITH KEY property = 'USER'.
    IF sy-subrc = 0.
      READ TABLE <ls_filter>-select_options ASSIGNING <ls_select_options> INDEX 1.
      IF sy-subrc = 0.
        lv_user = <ls_select_options>-low.
      ENDIF.
    ENDIF.

    " Vista
    READ TABLE it_filter_select_options ASSIGNING <ls_filter> WITH KEY property = 'VIEWNAME'.
    IF sy-subrc = 0.
      LOOP AT <ls_filter>-select_options ASSIGNING <ls_select_options>.
        INSERT VALUE #( sign = <ls_select_options>-sign option = <ls_select_options>-option
                        low = <ls_select_options>-low high = <ls_select_options>-high ) INTO TABLE lt_r_views.
      ENDLOOP.
    ENDIF.

    " Si no hay idioma o no es válido entonces se informa el idioma del sistema
    lv_langu = COND #( WHEN lv_langu IS INITIAL THEN sy-langu ELSE lv_langu ).

    mo_controller->get_views(
      EXPORTING
        iv_user  = lv_user
        iv_langu = lv_langu
        it_r_views = lt_r_views
      IMPORTING
        et_views = DATA(lt_views) ).

    et_entityset = VALUE #( FOR <wa> IN lt_views ( view_name = <wa>-view_name view_desc = <wa>-view_desc
                                                   level_auth = <wa>-level_auth ) ).


  ENDMETHOD.


  METHOD lockviewset_get_entity.
    DATA lv_viewname TYPE tabname.

    " Nombre de la vista
    READ TABLE it_key_tab ASSIGNING FIELD-SYMBOL(<ls_key_tab>) WITH KEY name = 'VIEWNAME'.
    IF sy-subrc = 0.
      lv_viewname = <ls_key_tab>-value.
    ENDIF.

    mo_controller->lock_view( EXPORTING iv_view_name = lv_viewname
                              IMPORTING ev_locked = er_entity-already_locked
                                        ev_lock_by_user = er_entity-lockbyuser ).

    " Para que no de error el servicio hay que devolver valor en al menos un campo.
    er_entity-tabname = lv_viewname.

  ENDMETHOD.


  METHOD readdataset_get_entity.
    DATA lv_langu TYPE sylangu.
    DATA lv_viewname TYPE tabname.
    DATA lv_mode TYPE c LENGTH 1.

    CLEAR: er_entity.

    " Se recupera el diioma
    READ TABLE it_key_tab ASSIGNING FIELD-SYMBOL(<ls_key_tab>) WITH KEY name = 'LANGU'.
    IF sy-subrc = 0.
      CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
        EXPORTING
          input            = <ls_key_tab>-value
        IMPORTING
          output           = lv_langu
        EXCEPTIONS
          unknown_language = 1
          OTHERS           = 2.
    ENDIF.

    " Nombre de la vista
    READ TABLE it_key_tab ASSIGNING <ls_key_tab> WITH KEY name = 'VIEWNAME'.
    IF sy-subrc = 0.
      lv_viewname = <ls_key_tab>-value.
    ENDIF.

    " Modo de edición
    READ TABLE it_key_tab ASSIGNING <ls_key_tab> WITH KEY name = 'MODE'.
    IF sy-subrc = 0.
      lv_mode = <ls_key_tab>-value.
    ENDIF.

    " Se llama al controlador para leer los datos
    mo_controller->read_data(
      EXPORTING
        iv_view_name = lv_viewname
        iv_langu     = lv_langu
        iv_mode      = lv_mode
      IMPORTING
        ev_data      = er_entity-data
        ev_data_template = er_entity-data_template ).

  ENDMETHOD.


  METHOD readviewset_get_entityset.
    DATA lv_langu TYPE sylangu.
    DATA lv_viewname TYPE tabname.
    DATA lv_mode TYPE c LENGTH 1.

    CLEAR: et_entityset.

    " Se recupera el diioma
    READ TABLE it_filter_select_options ASSIGNING FIELD-SYMBOL(<ls_filter>) WITH KEY property = 'LANGU'.
    IF sy-subrc = 0.
      READ TABLE <ls_filter>-select_options ASSIGNING FIELD-SYMBOL(<ls_select_options>) INDEX 1.
      CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
        EXPORTING
          input            = <ls_select_options>-low
        IMPORTING
          output           = lv_langu
        EXCEPTIONS
          unknown_language = 1
          OTHERS           = 2.

    ENDIF.

    " Nombre de la vista
    READ TABLE it_filter_select_options ASSIGNING <ls_filter> WITH KEY property = 'VIEWNAME'.
    IF sy-subrc = 0.
      READ TABLE <ls_filter>-select_options ASSIGNING <ls_select_options> INDEX 1.
      IF sy-subrc = 0.
        lv_viewname = <ls_select_options>-low.
      ENDIF.
    ENDIF.

    " Modo de edición
    READ TABLE it_filter_select_options ASSIGNING <ls_filter> WITH KEY property = 'MODE'.
    IF sy-subrc = 0.
      READ TABLE <ls_filter>-select_options ASSIGNING <ls_select_options> INDEX 1.
      IF sy-subrc = 0.
        lv_mode = <ls_select_options>-low.
      ENDIF.
    ENDIF.

    " Lectura de la vista
    mo_controller->read_view(
      EXPORTING
        iv_view_name = lv_viewname
        iv_langu     = lv_langu
        iv_mode = lv_mode
      IMPORTING
        et_fields    = DATA(lt_fields) ).

    LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_fields>).
      DATA(ls_entityset) = CORRESPONDING  zcl_zal30_data_mpc=>ts_readview( <ls_fields> ).
      INSERT ls_entityset INTO TABLE et_entityset.
    ENDLOOP.

  ENDMETHOD.


  METHOD rowvalidationdet_create_entity.
    DATA lv_langu TYPE sy-langu.
    DATA lv_viewname TYPE zal30_t_view-tabname.
    DATA ls_data TYPE zcl_zal30_data_mpc=>ts_rowvalidationdetermination.

    " Lectura de los datos provenientes del body de la llamada
    io_data_provider->read_entry_data( IMPORTING es_data = ls_data ).

    " Si iguala los datos de salida a los de entrada.
    er_entity = ls_data.

    mo_controller->row_validation_determination(
      EXPORTING
        iv_view_name =  ls_data-tabname
        iv_langu     = ls_data-langu
        iv_row       = ls_data-row
      IMPORTING
        ev_row       = er_entity-row ).


  ENDMETHOD.
  METHOD rowvalidationdet_get_entity.

  ENDMETHOD.

  METHOD verifyfielddatas_get_entity.
    DATA lv_langu TYPE sy-langu.


    READ TABLE it_key_tab ASSIGNING FIELD-SYMBOL(<ls_key_tab>) WITH KEY name = 'LANGU'.
    IF sy-subrc = 0.
      CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
        EXPORTING
          input            = <ls_key_tab>-value
        IMPORTING
          output           = lv_langu
        EXCEPTIONS
          unknown_language = 1
          OTHERS           = 2.

    ENDIF.

    READ TABLE it_key_tab ASSIGNING <ls_key_tab> WITH KEY name = 'COLUMN'.
    IF sy-subrc = 0.
      DATA(lv_fieldname) = CONV fieldname( <ls_key_tab>-value ).
    ENDIF.

    READ TABLE it_key_tab ASSIGNING <ls_key_tab> WITH KEY name = 'VIEWNAME'.
    IF sy-subrc = 0.
      DATA(lv_viewname) = CONV tabname( <ls_key_tab>-value ).
    ENDIF.

    READ TABLE it_key_tab ASSIGNING <ls_key_tab> WITH KEY name = 'VALUE'.
    IF sy-subrc = 0.
      DATA(lv_value) = <ls_key_tab>-value.
    ENDIF.

    er_entity-fieldname = lv_fieldname.
    er_entity-tabname = lv_viewname.
    er_entity-value = lv_value.
    er_entity-langu = lv_langu.

    mo_controller->verify_field_data(
      EXPORTING
        iv_langu        = lv_langu
        iv_view_name    = lv_viewname
        iv_fieldname    = lv_fieldname
        iv_value        = lv_value
      IMPORTING
        ev_message_type = er_entity-message_type
        ev_message      = er_entity-message ).

  ENDMETHOD.

ENDCLASS.
