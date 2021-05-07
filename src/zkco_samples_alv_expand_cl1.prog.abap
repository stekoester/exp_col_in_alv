CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA:
      BEGIN OF gcs_toolbar,
        expall_name     TYPE salv_de_function VALUE 'EXPALL',
        expall_icon     TYPE iconname VALUE icon_expand_all,
        colall_name     TYPE salv_de_function VALUE 'COLALL',
        colall_icon     TYPE iconname VALUE icon_collapse_all,
        expall_tooltip  TYPE string,
        colall_tooltip  TYPE string,
        expall_position TYPE salv_de_function_pos VALUE if_salv_c_function_position=>right_of_salv_functions,
        colall_position TYPE salv_de_function_pos VALUE if_salv_c_function_position=>right_of_salv_functions,
      END OF gcs_toolbar.

    CLASS-METHODS:
      class_constructor,

      get_icon
        IMPORTING
          iv_type        TYPE char1
        RETURNING
          VALUE(rv_icon) TYPE text40.

    METHODS:
      handle_user_command   FOR EVENT user_command  OF cl_gui_alv_grid
        IMPORTING
          e_ucomm
          sender,
      handle_added_function FOR EVENT toolbar       OF cl_gui_alv_grid
        IMPORTING
          e_object
          e_interactive
          sender,
      handle_link_click     FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING
          e_row_id
          e_column_id
          es_row_no
          sender.
ENDCLASS.

CLASS lcl_handle_events IMPLEMENTATION.

  METHOD class_constructor.
    gcs_toolbar-expall_tooltip = 'Expand all Details'(e01).
    gcs_toolbar-colall_tooltip = 'Collapse all Details'(c01).
  ENDMETHOD.


  METHOD handle_user_command.
    CASE e_ucomm.
      WHEN gcs_toolbar-expall_name.
        LOOP  AT gt_tadir_output ASSIGNING FIELD-SYMBOL(<ls_tadir_output>)
              WHERE expand(3) EQ icon_expand(3).
          DATA(lv_add_subrows_index) = sy-tabix + 1.
          <ls_tadir_output>-expand = lcl_handle_events=>get_icon( iv_type = 'C' ).
          DATA(lt_tadir) = VALUE zkco_samples_alv_tadir_t( FOR ls_tadir IN gt_tadir
                                                           WHERE ( pgmid  EQ <ls_tadir_output>-pgmid AND
                                                                   object EQ <ls_tadir_output>-object )
                                                           ( ls_tadir ) ).
          LOOP AT lt_tadir REFERENCE INTO DATA(lr_tadir).
            INSERT CORRESPONDING #( lr_tadir->* EXCEPT expand ) INTO gt_tadir_output
                   INDEX lv_add_subrows_index.
          ENDLOOP.
        ENDLOOP.
        IF sy-subrc EQ 0.
          DATA(lv_refresh_alv) = abap_true.
        ENDIF.
      WHEN gcs_toolbar-colall_name.
        LOOP  AT gt_tadir_output ASSIGNING <ls_tadir_output>
              WHERE expand(3) EQ icon_collapse(3).
          <ls_tadir_output>-expand = lcl_handle_events=>get_icon( iv_type = 'E' ).
          DELETE gt_tadir_output  WHERE pgmid  EQ <ls_tadir_output>-pgmid
                                    AND object EQ <ls_tadir_output>-object
                                    AND expand IS INITIAL.
        ENDLOOP.
        IF sy-subrc EQ 0.
          lv_refresh_alv = abap_true.
        ENDIF.
    ENDCASE.
    IF lv_refresh_alv EQ abap_true.
      sender->refresh_table_display(
        EXPORTING
          is_stable      = VALUE #( row = abap_true col = abap_false )
          i_soft_refresh = abap_true
        EXCEPTIONS
          finished       = 1                " Display was Ended (by Export)
          OTHERS         = 2 ).
      IF sy-subrc <> 0.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD handle_added_function.
    APPEND LINES OF VALUE ttb_button( ( function = gcs_toolbar-expall_name
                                        icon     = gcs_toolbar-expall_icon
                                        quickinfo = gcs_toolbar-expall_tooltip )
                                      ( function = gcs_toolbar-colall_name
                                        icon     = gcs_toolbar-colall_icon
                                        quickinfo = gcs_toolbar-colall_tooltip ) ) TO e_object->mt_toolbar.
  ENDMETHOD.

  METHOD get_icon.
    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name                  = SWITCH #( iv_type WHEN 'E' THEN icon_expand
                                                  WHEN 'C' THEN icon_collapse )
        info                  = SWITCH text40( iv_type WHEN 'E' THEN 'Expand Details'(e02)
                                                       WHEN 'C' THEN 'Collapse Details'(c02) )
        add_stdinf            = ' '
      IMPORTING
        result                = rv_icon
      EXCEPTIONS
        icon_not_found        = 0
        outputfield_too_short = 0
        OTHERS                = 0.
  ENDMETHOD.

  METHOD handle_link_click.
    CASE e_column_id-fieldname.
      WHEN 'EXPAND'.
        ASSIGN gt_tadir_output[ e_row_id-index ] TO FIELD-SYMBOL(<ls_tadir_output>).
        IF <ls_tadir_output>-expand(3) EQ icon_expand(3).
          <ls_tadir_output>-expand = lcl_handle_events=>get_icon( iv_type = 'C' ).
          DATA(lt_tadir) = VALUE zkco_samples_alv_tadir_t( FOR ls_tadir IN gt_tadir
                                                           WHERE ( pgmid  EQ <ls_tadir_output>-pgmid AND
                                                                   object EQ <ls_tadir_output>-object )
                                                           ( ls_tadir ) ).
          LOOP AT lt_tadir REFERENCE INTO DATA(lr_tadir).
            INSERT CORRESPONDING #( lr_tadir->* EXCEPT expand ) INTO gt_tadir_output
                   INDEX e_row_id-index + sy-tabix.
          ENDLOOP.
        ELSE.
          <ls_tadir_output>-expand = lcl_handle_events=>get_icon( iv_type = 'E' ).
          DELETE gt_tadir_output  WHERE pgmid  EQ <ls_tadir_output>-pgmid
                                    AND object EQ <ls_tadir_output>-object
                                    AND expand IS INITIAL.
        ENDIF.

        sender->refresh_table_display(
          EXPORTING
            is_stable      = VALUE #( row = abap_true col = abap_false )
            i_soft_refresh = abap_true
          EXCEPTIONS
            finished       = 1                " Display was Ended (by Export)
            OTHERS         = 2 ).
        IF sy-subrc <> 0.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
