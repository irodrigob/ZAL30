CLASS zcl_zal30_data_dpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zcl_zal30_data_dpc
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor.
  PROTECTED SECTION.
    DATA mo_controller TYPE REF TO zcl_al30_gw_controller.
    METHODS getviewsset_get_entityset REDEFINITION.
    METHODS checkauthviewset_get_entity REDEFINITION.
    METHODS readviewset_get_entityset REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_zal30_data_dpc_ext IMPLEMENTATION.
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
  METHOD constructor.
    super->constructor(  ).

    mo_controller = NEW zcl_al30_gw_controller(  ). " Se instancia el controlador para GW
  ENDMETHOD.

  METHOD checkauthviewset_get_entity.

    " Se recupera la vista pasada
    READ TABLE it_key_tab ASSIGNING FIELD-SYMBOL(<ls_key_tab>) WITH KEY name = 'VIEWNAME'.
    IF sy-subrc = 0.
      er_entity-level_auth = mo_controller->check_authorization_view( iv_view_name   = CONV #( <ls_key_tab>-value ) ).
      er_entity-view_name = <ls_key_tab>-value.
    ENDIF.

  ENDMETHOD.

  METHOD readviewset_get_entityset.
    DATA lv_langu TYPE sylangu.
    DATA lv_viewname TYPE tabname.

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

    " Lectura de la vista
    mo_controller->read_view(
      EXPORTING
        iv_view_name = lv_viewname
        iv_langu     = lv_langu
      IMPORTING
        et_fields    = DATA(lt_fields) ).

    LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_fields>).
      DATA(ls_entityset) = CORRESPONDING  zcl_zal30_data_mpc=>ts_readview( <ls_fields> ).
      INSERT ls_entityset INTO TABLE et_entityset.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
