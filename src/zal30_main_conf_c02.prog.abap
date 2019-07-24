*&---------------------------------------------------------------------*
*&  Include           ZAL30_MAIN_CONF_C02
*&---------------------------------------------------------------------*
CLASS lcl_event_toolbar_fields DEFINITION.

  PUBLIC SECTION.
    METHODS:
      on_function_selected
                    FOR EVENT function_selected OF cl_gui_toolbar
        IMPORTING fcode.
    METHODS:
      on_dropdown_clicked
                    FOR EVENT dropdown_clicked OF cl_gui_toolbar
        IMPORTING fcode posx posy.

ENDCLASS.                    "lcl_event_toolbar_fields DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_toolbar_fields IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_toolbar_fields IMPLEMENTATION.
  METHOD on_function_selected.
    CASE fcode.
      WHEN mc_check_dict. " Check dictionary
        PERFORM check_dictionary.
      WHEN mc_sync_dict. " Adjust from the dictionary
        PERFORM sync_dictionary.
      WHEN OTHERS.
* Si el código de boton contiene el identificador de los menus del idioma
* entonces es un cambio de idioma.
        IF fcode CS mc_id_lang_button.
          PERFORM change_language USING fcode.
        ENDIF.
    ENDCASE.
  ENDMETHOD.                    "on_function_selected
  METHOD on_dropdown_clicked.
    IF fcode CS mc_id_lang_button.
      PERFORM submenu_languages USING posx posy.
    ENDIF.
  ENDMETHOD.                    "on_dropdown_clicked


ENDCLASS.                    "lcl_event_toolbar_fields IMPLEMENTATION
