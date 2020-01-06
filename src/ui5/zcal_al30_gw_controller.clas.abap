CLASS zcal_al30_gw_controller DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Get the view list</p>
    "!
    "! @parameter iv_user | <p class="shorttext synchronized" lang="en">User</p>
    "! @parameter iv_langu | <p class="shorttext synchronized" lang="en">Language</p>
    "! @parameter et_view | <p class="shorttext synchronized" lang="en">View list</p>
    METHODS get_views
      IMPORTING
        !iv_user  TYPE string
        !iv_langu TYPE sylangu DEFAULT sy-langu
      EXPORTING
        !et_views  TYPE zcl_al30_view=>tt_view_list.
    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    "!
    METHODS constructor.
  PROTECTED SECTION.
    DATA mo_controller TYPE REF TO zcl_al30_controller.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcal_al30_gw_controller IMPLEMENTATION.
  METHOD get_views.

    mo_controller->view_list(
      EXPORTING
        iv_langu     = iv_langu
      IMPORTING
        et_view_list = et_views ).

  ENDMETHOD.

  METHOD constructor.
    mo_controller = NEW zcl_al30_controller(  ).
  ENDMETHOD.

ENDCLASS.
