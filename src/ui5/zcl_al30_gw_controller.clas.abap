CLASS zcl_al30_gw_controller DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Get the view list</p>
    "!
    "! @parameter iv_user | <p class="shorttext synchronized" lang="en">User</p>
    "! @parameter iv_langu | <p class="shorttext synchronized" lang="en">Language</p>
    "! @parameter it_r_views | <p class="shorttext synchronized" lang="en">Views to filter</p>
    "! @parameter et_view | <p class="shorttext synchronized" lang="en">View list</p>
    METHODS get_views
      IMPORTING
        !iv_user    TYPE string
        !iv_langu   TYPE sylangu DEFAULT sy-langu
        !it_r_views TYPE zif_al30_data=>tt_r_tabname OPTIONAL
      EXPORTING
        !et_views   TYPE zif_al30_ui5_data=>tt_view_list_auth.
    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    "!
    METHODS constructor.
    "! <p class="shorttext synchronized" lang="en">Check the authorization level in view</p>
    "!
    "! @parameter iv_view_name | <p class="shorttext synchronized" lang="en">View name</p>
    "! @parameter iv_view_action | <p class="shorttext synchronized" lang="en">'U' Update 'S' Show</p>
    "! @parameter iv_user | <p class="shorttext synchronized" lang="en">Username</p>
    "! @parameter rv_level_auth | <p class="shorttext synchronized" lang="en">Level auth</p>
    METHODS check_authorization_view
      IMPORTING
                !iv_view_name        TYPE tabname
                !iv_view_action      TYPE any OPTIONAL
                !iv_user             TYPE syuname OPTIONAL
      RETURNING VALUE(rv_level_auth) TYPE zal30_e_level_auth .
  PROTECTED SECTION.
    DATA mo_controller TYPE REF TO zcl_al30_controller.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_al30_gw_controller IMPLEMENTATION.


  METHOD check_authorization_view.

    rv_level_auth = mo_controller->check_authorization_view(
       EXPORTING
         iv_view_name   = iv_view_name
         iv_view_action = COND #( WHEN iv_view_action IS INITIAL THEN  zif_al30_data=>cs_action_auth-update ELSE iv_view_action )
         iv_user        = COND #( WHEN iv_user IS INITIAL THEN sy-uname ELSE iv_user ) ).

  ENDMETHOD.


  METHOD constructor.
    mo_controller = NEW zcl_al30_controller(  ).
  ENDMETHOD.


  METHOD get_views.

    mo_controller->view_list(
      EXPORTING
        iv_langu     = iv_langu
        it_r_views = it_r_views
      IMPORTING
        et_view_list = DATA(lt_views) ).

    " Se recorre los datos y se mira si te tiene autorización
    LOOP AT lt_views REFERENCE INTO DATA(lo_views).
      DATA(lv_auth) = mo_controller->check_authorization_view( EXPORTING iv_view_name   = lo_views->view_name
                                                                         iv_view_action = zif_al30_data=>cs_action_auth-update
                                                                         iv_user        = COND #( WHEN iv_user IS INITIAL THEN sy-uname ELSE iv_user ) ).

      IF lv_auth NE zif_al30_data=>cs_level_auth_user-non. " Si tiene permiso se añade
        INSERT VALUE #( view_name = lo_views->view_name
                        view_desc = lo_views->view_desc
                        level_auth = lv_auth ) INTO TABLE et_views.
      ENDIF.
      CLEAR lv_auth.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
