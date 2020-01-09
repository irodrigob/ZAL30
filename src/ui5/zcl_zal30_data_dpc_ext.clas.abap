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
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_zal30_data_dpc_ext IMPLEMENTATION.
  METHOD getviewsset_get_entityset.
    DATA lv_langu TYPE sylangu.
    DATA lv_user TYPE string.

    CLEAR et_entityset.

    " Se recupera los valores de los filtros
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

    READ TABLE it_filter_select_options ASSIGNING <ls_filter> WITH KEY property = 'USER'.
    IF sy-subrc = 0.
      READ TABLE <ls_filter>-select_options ASSIGNING <ls_select_options> INDEX 1.
      IF sy-subrc = 0.
        lv_user = <ls_select_options>-low.
      ENDIF.
    ENDIF.

    " Si no hay idioma o no es válido entonces se informa el idioma del sistema
    lv_langu = COND #( WHEN lv_langu IS INITIAL THEN sy-langu ELSE lv_langu ).

    mo_controller->get_views(
      EXPORTING
        iv_user  = lv_user
        iv_langu = lv_langu
      IMPORTING
        et_views = DATA(lt_views) ).

    et_entityset = VALUE #( FOR <wa> IN lt_views ( view_name = <wa>-view_name view_desc = <wa>-view_desc ) ).


  ENDMETHOD.
  METHOD constructor.
    super->constructor(  ).

    mo_controller = NEW zcl_al30_gw_controller(  ). " Se instancia el controlador para GW
  ENDMETHOD.

  METHOD checkauthviewset_get_entity.

    " Se recupera la vista pasada
    READ TABLE it_key_tab ASSIGNING FIELD-SYMBOL(<ls_key_tab>) WITH KEY name = 'VIEW_NAME'.
    IF sy-subrc = 0.
      er_entity-level_auth = mo_controller->check_authorization_view( iv_view_name   = CONV #( <ls_key_tab>-value ) ).
      er_entity-view_name = <ls_key_tab>-value.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
