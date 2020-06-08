*&---------------------------------------------------------------------*
*&  Include           ZAL30_MAIN_CONF_TOP
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Types
*----------------------------------------------------------------------*
TYPE-POOLS: cntb, icon, abap, cntl.

*----------------------------------------------------------------------*
* Tables of dictionary
*----------------------------------------------------------------------*
TABLES: zal30_t_view.

*----------------------------------------------------------------------*
* Internal tables
*----------------------------------------------------------------------*
* Campos de la vista
DATA mt_fields TYPE zif_al30_data=>tt_fields_view_alv.
DATA mt_fields_text TYPE zif_al30_data=>tt_fields_text_view_alv.
DATA mt_fields_text_orig TYPE zif_al30_data=>tt_fields_text_view_alv.

*----------------------------------------------------------------------*
* Constantes
*----------------------------------------------------------------------*
DATA mc_hndl_source_text TYPE int4 VALUE '1'.
DATA mc_hndl_lbl_type_header TYPE int4 VALUE '2'.

CONSTANTS: BEGIN OF cs_toolbar_functions,
             " Submenu de idiomas
             lang_menu      TYPE ui_func VALUE 'LANG_MENU',
             " Verificacion de diccionario
             check_dict     TYPE ui_func VALUE 'FC2',
             " Sincronizacion con el diccionario
             sync_dict      TYPE ui_func VALUE 'FC1',
             " Añadir campo virtual
             virtual_field  TYPE ui_func VALUE 'VIRTUAL_FIELD',
             " Id del boton de menu de idioma
             id_lang_button TYPE ui_func VALUE 'LANG_',
             " Borrar campo
             delete_field   TYPE ui_func VALUE 'DEL_FIELD',
           END OF cs_toolbar_functions.

*----------------------------------------------------------------------*
* Variables
*----------------------------------------------------------------------*
* boton pulsado en dynpro 9000, la principal.
DATA d_okcode_9000 TYPE sy-ucomm.

* Modo de visualización del programa.
DATA mv_mode TYPE char1.

* Texto de la vista (se toma del diccionario)
DATA d_text_view TYPE as4text.

* Variable que controla si se muestra el frame con los datos de los campos
DATA mv_expand_fields TYPE sap_bool.

* Variable que controla si se muestra el frame con los datos de la vista
DATA mv_expand_header TYPE sap_bool.

* Verificación que los datos de la pestaña son correctos
DATA mv_datos_validos TYPE sap_bool.
DATA mv_datos_validos_text TYPE sap_bool.
DATA mv_datos_validos_gen TYPE sap_bool.

* Variable que sabe si hay diferencias con el diccionario
DATA mv_diff_dict TYPE sap_bool.

* Idioma de visualización de los textos
DATA mv_lang_vis TYPE sy-langu.

* Orden donde se guardan los datos
DATA mv_orden_transporte TYPE e070-trkorr.

* Se generará una orden al grabar los datos
DATA mv_pedir_orden TYPE sap_bool.

* Se podrá modificar los datos
DATA mv_perm_modif_datos TYPE sap_bool.

*----------------------------------------------------------------------*
* Class
*----------------------------------------------------------------------*
DATA mo_cnt_al30 TYPE REF TO zcl_al30_controller.

*----------------------------------------------------------------------*
* Declaración objetos para las pestañas.
*----------------------------------------------------------------------*
*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TAB_FIELDS'
CONSTANTS: BEGIN OF c_tab_fields,
             tab1 LIKE sy-ucomm VALUE 'TAB_FIELDS_GENERAL',
             tab2 LIKE sy-ucomm VALUE 'TAB_FIELDS_TEXTS',
             tab3 LIKE sy-ucomm VALUE 'TAB_FIELDS_OTHERS',
           END OF c_tab_fields.
*&SPWIZARD: DATA FOR TABSTRIP 'TAB_FIELDS'
CONTROLS:  tab_fields TYPE TABSTRIP.
DATA: BEGIN OF g_tab_fields,
        subscreen   LIKE sy-dynnr,
        prog        LIKE sy-repid VALUE 'ZAL30_MAIN_CONF',
        pressed_tab LIKE sy-ucomm VALUE c_tab_fields-tab1,
      END OF g_tab_fields.

*----------------------------------------------------------------------*
* Declaración para los ALV
*----------------------------------------------------------------------*
* Catalogo de campos global que alimentará a los distintos ALV
DATA mt_fieldcat TYPE lvc_t_fcat.

* Funciones de la barra del ALV que se excluiran
DATA mt_excluding TYPE ui_functions.
DATA ms_stable TYPE lvc_s_stbl.

* -> Atributos generales
DATA ms_layout_gen TYPE lvc_s_layo.
DATA mo_alv_gen TYPE REF TO cl_gui_alv_grid.
DATA mo_container_gen TYPE REF TO cl_gui_custom_container.
DATA mt_fieldcat_gen TYPE lvc_t_fcat.
DATA mt_dropdown_gen TYPE lvc_t_dral.
CLASS lcl_event_gen DEFINITION DEFERRED.
DATA mo_event_receiver_gen TYPE REF TO lcl_event_gen.
* <-- Atributos generales

* -> Atributos de textos
DATA ms_layout_text TYPE lvc_s_layo.
DATA mo_alv_text TYPE REF TO cl_gui_alv_grid.
DATA mo_container_text TYPE REF TO cl_gui_custom_container.
DATA mt_fieldcat_text TYPE lvc_t_fcat.
DATA mt_dropdown_text TYPE lvc_t_dral.
CLASS lcl_event_text DEFINITION DEFERRED.
DATA mo_event_receiver_text TYPE REF TO lcl_event_text.
* <- Atributos de textos

* Filtro comunes para ambos listados
DATA mt_filter TYPE lvc_t_filt.

*----------------------------------------------------------------------*
* Declaración para la barra de herramientas de los campos
*----------------------------------------------------------------------*
DATA mo_cont_toolbar_fields TYPE REF TO cl_gui_custom_container.
DATA mo_toolbar_fields TYPE REF TO cl_gui_toolbar.
DATA mt_evts_toolbar_fields TYPE TABLE OF cntl_simple_event.
CLASS lcl_event_toolbar_fields DEFINITION DEFERRED.
DATA mo_event_toolbar_fields TYPE REF TO lcl_event_toolbar_fields.
DATA mt_buttongroup TYPE ttb_button.

*----------------------------------------------------------------------*
* Declaración para la dynpro de añadir campo virtual
*----------------------------------------------------------------------*
DATA mv_okcode_9004 TYPE syucomm.
DATA: BEGIN OF ms_virtual_field,
        text_prefix TYPE string,
        name        TYPE fieldname,
        dtel        TYPE zal30_e_virtual_rollname,
      END OF ms_virtual_field.
