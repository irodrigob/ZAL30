*&---------------------------------------------------------------------*
*&  Include           ZAL30_MAIN_VIEW_TOP
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Tipos de datos
*----------------------------------------------------------------------*
TYPE-POOLS: cntb, icon, abap.

TYPES: BEGIN OF ts_control_data,
         tabix TYPE sytabix,
         updkz TYPE char01,
       END OF ts_control_data.
TYPES: tt_control_data TYPE STANDARD TABLE OF ts_control_data.

*----------------------------------------------------------------------*
* Internal tables
*----------------------------------------------------------------------*
* Campos de la parametrización de la vista
DATA mt_fields TYPE zif_al30_data=>tt_fields_view_alv.
DATA mt_fields_text TYPE zif_al30_data=>tt_fields_text_view_alv.
data mt_fields_ddic type DD03PTAB.

DATA mt_control_data TYPE tt_control_data.

*----------------------------------------------------------------------*
* Datos de la vista
*----------------------------------------------------------------------*
* Datos para que procese el ALV
DATA mo_datos TYPE REF TO data.
FIELD-SYMBOLS <it_datos> TYPE STANDARD TABLE.

* Registros que se tienen que borrar borran
DATA mo_datos_del TYPE REF TO data.
FIELD-SYMBOLS <it_datos_del> TYPE STANDARD TABLE.


*----------------------------------------------------------------------*
* Variables
*----------------------------------------------------------------------*
* boton pulsado en dynpro 9000, la principal.
DATA d_okcode_9000 TYPE sy-ucomm.

* Modo de visualización del programa.
DATA mv_mode TYPE char1.

* Texto de la vista (se toma del diccionario)
DATA mv_text_view TYPE as4text.

* Verificación que los datos de la pestaña son correctos
DATA mv_datos_validos TYPE sap_bool.

* Datos principales de la vista
DATA ms_view TYPE zal30_t_view.

* Se generará una orden al grabar los datos
DATA mv_pedir_orden TYPE sap_bool.

* Orden donde se guardan los datos
DATA mv_orden_transporte TYPE e070-trkorr.

* Texto de idioma de la tabla de texto
DATA mv_field_lang_textable TYPE fieldname.

*----------------------------------------------------------------------*
* Class
*----------------------------------------------------------------------*
DATA mo_controller TYPE REF TO zcl_al30_controller.

*----------------------------------------------------------------------*
* Declaración para los ALV
*----------------------------------------------------------------------*

* Catalogo de campos de la vista
DATA mt_fieldcat TYPE lvc_t_fcat.

* Funciones de la barra del ALV que se excluiran
DATA mt_excluding TYPE ui_functions.
DATA ms_stable TYPE lvc_s_stbl.
DATA ms_layout TYPE lvc_s_layo.
DATA mt_filters TYPE lvc_t_filt.
DATA mo_alv TYPE REF TO cl_gui_alv_grid.
DATA mo_container TYPE REF TO cl_gui_docking_container.
CLASS lcl_event_alv DEFINITION DEFERRED.
DATA mo_event_receiver_alv TYPE REF TO lcl_event_alv.
