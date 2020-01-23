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
      WHEN cs_toolbar_functions-check_dict. " Check dictionary
        PERFORM check_dictionary.
      WHEN cs_toolbar_functions-sync_dict. " Adjust from the dictionary
        PERFORM sync_dictionary.
      WHEN cs_toolbar_functions-virtual_field. " Add virtual field
        PERFORM show_dynp_add_virtual_field.
      WHEN cs_toolbar_functions-delete_field. " Delete fiel
        PERFORM delete_fields.
      WHEN OTHERS.
* Si el código de boton contiene el identificador de los menus del idioma
* entonces es un cambio de idioma.
        IF fcode CS cs_toolbar_functions-id_lang_button.
          PERFORM change_language USING fcode.
        ENDIF.
    ENDCASE.
  ENDMETHOD.                    "on_function_selected
  METHOD on_dropdown_clicked.
    IF fcode CS cs_toolbar_functions-id_lang_button.
      PERFORM submenu_languages USING posx posy.
    ENDIF.
  ENDMETHOD.                    "on_dropdown_clicked


ENDCLASS.                    "lcl_event_toolbar_fields IMPLEMENTATION
