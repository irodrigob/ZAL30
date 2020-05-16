CLASS zcl_al30_ui5_transport_order DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_user_orders,
             order       TYPE trkorr,
             user        TYPE uname,
             description TYPE bsstring,
           END OF ts_user_orders.
    TYPES tt_user_orders TYPE STANDARD TABLE OF ts_user_orders.

    "! <p class="shorttext synchronized">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        !iv_langu TYPE sylangu DEFAULT sy-langu.

    "! <p class="shorttext synchronized">Get user orders</p>
    "! @parameter iv_user | <p class="shorttext synchronized">User</p>
    "! @parameter et_orders | <p class="shorttext synchronized">Orders of user</p>
    METHODS get_user_orders
      IMPORTING
        !iv_user   TYPE syuname DEFAULT sy-uname
      EXPORTING
        !et_orders TYPE tt_user_orders.
  PROTECTED SECTION.
    DATA mv_langu TYPE sy-langu.

    "! <p class="shorttext synchronized">Fill selection orders</p>
    "! Some options do not apply to the application, but are left for the future
    "! @parameter iv_type_workbench | <p class="shorttext synchronized">Workbench type order</p>
    "! @parameter iv_type_customizing | <p class="shorttext synchronized">Customizingc</p>
    "! @parameter iv_type_transport | <p class="shorttext synchronized">Transport copies type order</p>
    "! @parameter iv_status_modif | <p class="shorttext synchronized">Modifiable order</p>
    "! @parameter iv_status_rele | <p class="shorttext synchronized">Released order</p>
    "! @parameter rt_selections | <p class="shorttext synchronized">Selections table</p>
    METHODS fill_selections_orders
      IMPORTING
        !iv_type_workbench   TYPE sap_bool DEFAULT abap_true
        !iv_type_customizing TYPE sap_bool DEFAULT abap_false
        !iv_type_transport   TYPE sap_bool DEFAULT abap_false
        !iv_status_modif     TYPE sap_bool DEFAULT abap_true
        !iv_status_rele      TYPE sap_bool DEFAULT abap_false
      RETURNING
        VALUE(rt_selections) TYPE trwbo_selection .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_al30_ui5_transport_order IMPLEMENTATION.
  METHOD get_user_orders.
    DATA lt_request TYPE trwbo_request_headers.

    CLEAR: et_orders.

    " Carga de los filtros para buscar las ordenes
    DATA(ls_selection) = fill_selections_orders(  ).

    " Lectura de las ordenes
    CALL FUNCTION 'TRINT_SELECT_REQUESTS'
      EXPORTING
        iv_username_pattern  = iv_user
        is_selection         = ls_selection
        iv_complete_projects = 'X'
      IMPORTING
        et_requests          = lt_request.

    " Solo se leen las ordenes padre
    et_orders = VALUE #( FOR <wa> IN lt_request WHERE ( strkorr IS INITIAL )
                                                ( user = <wa>-as4user
                                                 order = <wa>-trkorr
                                                 description = <wa>-as4text ) ).

  ENDMETHOD.

  METHOD fill_selections_orders.
    CLEAR rt_selections.

    IF iv_type_workbench = abap_true.
      rt_selections-reqfunctions(1)     = sctsc_type_workbench.
    ENDIF.
    IF iv_type_customizing = abap_true.
      rt_selections-reqfunctions+1(1)   = sctsc_type_customizing.
    ENDIF.
    IF iv_type_transport = abap_true.
      rt_selections-reqfunctions+2(1)   = sctsc_type_transport.
    ENDIF.

* Types of assigned tasks
    rt_selections-taskfunctions      = sctsc_types_tasks.

* Status para ordenes modificables
    IF iv_status_modif = abap_true.
      rt_selections-taskstatus(1)     = sctsc_state_protected.
      rt_selections-taskstatus+1(1)   = sctsc_state_changeable.
    ENDIF.

* Status para ordenes liberadas
    IF iv_status_rele = abap_true.
      rt_selections-taskstatus+2(1)   = sctsc_state_released.
      rt_selections-taskstatus+3(1)   = sctsc_state_notconfirmed.
    ENDIF.

    IF iv_status_modif = abap_true AND iv_status_rele = abap_false.
      rt_selections-reqstatus(1)   = sctsc_state_protected.
      rt_selections-reqstatus+1(1) = sctsc_state_changeable.
*    r_selections-reqstatus+2(1) = sctsc_state_export_started.
    ELSEIF iv_status_modif = abap_false AND iv_status_rele = abap_true.
      rt_selections-reqstatus(1)   = sctsc_state_released.
      rt_selections-reqstatus+1(1) = sctsc_state_export_started.
    ELSEIF iv_status_modif = abap_true AND iv_status_rele = abap_true.
      rt_selections-reqstatus      = sctsc_states_all.
    ENDIF.

  ENDMETHOD.

  METHOD constructor.
    mv_langu = iv_langu.
  ENDMETHOD.

ENDCLASS.
