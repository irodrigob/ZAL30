*&---------------------------------------------------------------------*
*&  Include           ZAL30_MAIN_VIEW_C01
*&---------------------------------------------------------------------*

CLASS lcl_event_alv DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_data_changed
         FOR EVENT data_changed OF cl_gui_alv_grid
             IMPORTING er_data_changed.
ENDCLASS.                    "lcl_event_alv DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_alv IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_alv IMPLEMENTATION.
  METHOD handle_data_changed.
    PERFORM data_changed CHANGING er_data_changed.
  ENDMETHOD.                    "handle_data_changed
ENDCLASS.                    "lcl_event_alv IMPLEMENTATION