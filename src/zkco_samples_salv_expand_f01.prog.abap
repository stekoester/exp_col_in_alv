FORM data_get.
  DO.
    ASSIGN COMPONENT sy-index OF STRUCTURE gcs_tadir_objects TO FIELD-SYMBOL(<lv_object>).
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.
    SELECT pgmid, object, obj_name, korrnum, srcsystem, author, srcdep, devclass
      FROM tadir APPENDING TABLE @gt_tadir                                        "#EC CI_GENBUFF
      UP TO 5 ROWS
      WHERE pgmid    EQ 'R3TR'
        AND object   EQ @<lv_object>
        AND devclass EQ 'SCTS_CAT'.
    IF sy-subrc EQ 0.
      DATA(ls_tadir) = gt_tadir[ lines( gt_tadir ) ].
      APPEND VALUE #( pgmid = ls_tadir-pgmid
                      object = ls_tadir-object
                      expand = lcl_handle_events=>get_icon( iv_type = 'E' )
                      cell_type = VALUE #( ( columnname = 'EXPAND'
                                             value      = if_salv_c_cell_type=>hotspot ) ) ) TO gt_tadir_output.
    ENDIF.
  ENDDO.
ENDFORM.

FORM data_display.
  CALL SCREEN 100.
ENDFORM.

FORM alv_display.
  TRY.
      cl_salv_table=>factory(
        EXPORTING
          r_container = NEW cl_gui_custom_container( 'ALV' )
        IMPORTING
          r_salv_table   = go_salv_table
        CHANGING
          t_table        = gt_tadir_output ).

      DATA(lr_events) = go_salv_table->get_event( ).
      DATA(lr_event_handler) = NEW lcl_handle_events( ).
      SET HANDLER lr_event_handler->handle_added_function FOR lr_events.
      SET HANDLER lr_event_handler->handle_link_click FOR lr_events.

      TRY.
          go_salv_table->get_columns( )->set_cell_type_column( 'CELL_TYPE' ).
        CATCH cx_salv_data_error.
      ENDTRY.
      TRY.
          go_salv_table->get_columns( )->set_optimize( ).
        CATCH cx_salv_data_error.
      ENDTRY.

      TRY.
          go_salv_table->get_functions( )->add_function( name = lcl_handle_events=>gcs_toolbar-expall_name
                                                         tooltip = lcl_handle_events=>gcs_toolbar-expall_tooltip
                                                         icon = CONV #( lcl_handle_events=>gcs_toolbar-expall_icon )
                                                         position = lcl_handle_events=>gcs_toolbar-expall_position ).
        CATCH cx_salv_wrong_call.
        CATCH cx_salv_existing.
      ENDTRY.
      TRY.
          go_salv_table->get_functions( )->add_function( name = lcl_handle_events=>gcs_toolbar-colall_name
                                                         tooltip = lcl_handle_events=>gcs_toolbar-colall_tooltip
                                                         icon = CONV #( lcl_handle_events=>gcs_toolbar-colall_icon )
                                                         position = lcl_handle_events=>gcs_toolbar-colall_position ).
        CATCH cx_salv_wrong_call.
        CATCH cx_salv_existing.
      ENDTRY.

      TRY.
          go_salv_table->get_columns( )->get_column( 'EXPAND' )->set_alignment( if_salv_c_alignment=>centered ).
        CATCH cx_salv_not_found.
      ENDTRY.
      go_salv_table->get_display_settings( )->set_striped_pattern( abap_true ).

      go_salv_table->display( ).
    CATCH cx_salv_msg.
  ENDTRY.
ENDFORM.
