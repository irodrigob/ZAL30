CLASS zcl_al30_ui5_test_exit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_al30_exit_class.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_al30_ui5_test_exit IMPLEMENTATION.
  METHOD zif_al30_exit_class~exit_before_read_data.
    FIELD-SYMBOLS <lt_style> TYPE lvc_t_styl.


    LOOP AT ct_data ASSIGNING FIELD-SYMBOL(<ls_data>).
      DATA(lv_tabix) = sy-tabix.

      " Ejemplo donde si la fecha es inferior al 01/01/2020 se desactiva.
      ASSIGN COMPONENT 'FIELDDATE' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<field>).
      IF sy-subrc = 0.
        IF <field> < '20200101'.
          ASSIGN COMPONENT zif_al30_data=>cs_control_fields_alv_data-style OF STRUCTURE <ls_data> TO <lt_style>.
          IF sy-subrc = 0.
            INSERT VALUE #( fieldname = zif_al30_data=>cs_control_fields_alv_data-style style = cl_gui_alv_grid=>mc_style_disabled )  INTO TABLE <lt_style>.
          ENDIF.
        ENDIF.
      ENDIF.

      " Relleno de la posición en el campo virtual
      ASSIGN COMPONENT 'VIR_POS' OF STRUCTURE <ls_data> TO <field>.
      IF sy-subrc = 0.
        <field> = lv_tabix.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
