*&---------------------------------------------------------------------*
*& Include          ZMINESWEEPER_CLS
*&---------------------------------------------------------------------*
CLASS lcl_minesweeper DEFINITION.
  PUBLIC SECTION.
    CONSTANTS:
      picture_path          TYPE char256 VALUE 'https://www.economicsnetwork.ac.uk/cheer/ch17/hand1.gif', "your path to picture
      mode_easy             TYPE string  VALUE 'EASY',
      mode_normal           TYPE string  VALUE 'NORMAL',
      mode_hard             TYPE string  VALUE 'HARD',
      mode_own              TYPE string  VALUE 'OWN',
      neighbourhood_neumann TYPE string  VALUE 'VON_NEUMANN',
      neighbourhood_moore   TYPE string  VALUE 'MOORE'.
    CLASS-METHODS:
      show_score,
      display_picture  IMPORTING iv_imgpath   TYPE char256
                        CHANGING co_control   TYPE REF TO cl_gui_picture
                                 co_container TYPE REF TO cl_gui_container.
    METHODS:
      constructor      IMPORTING iv_mode   TYPE string
                                 iv_neigh  TYPE string
                                 iv_size_x TYPE i
                                 iv_size_y TYPE i
                                 iv_mines  TYPE i,
      display_arena.
  PRIVATE SECTION.
    DATA:
      av_p_mode        TYPE string,
      av_p_neighood    TYPE string,
      av_p_size_x      TYPE i,
      av_p_size_y      TYPE i,
      av_p_mines       TYPE i,
      "arenas
      ao_arena_display TYPE REF TO data,
      ao_arena_hidden  TYPE REF TO data,
      ao_arena_record  TYPE REF TO data,
      "set for randomization
      at_mines         TYPE TABLE OF t_random WITH KEY primary_key COMPONENTS x y,
      at_set           TYPE TABLE OF t_random WITH KEY primary_key COMPONENTS x y,
      "salv
      ao_alv_table     TYPE REF TO cl_salv_table.
    METHODS:
      build_arena     CHANGING co_arena_display TYPE REF TO data
                               co_arena_hidden  TYPE REF TO data
                               co_arena_record  TYPE REF TO data,
      build_set       EXPORTING et_set          TYPE tt_random,
      "event handlers
      hdl_dbl_click   FOR EVENT double_click OF cl_salv_events_table
                      IMPORTING row column,
      hdl_user_cmd    FOR EVENT added_function OF cl_salv_events
                      IMPORTING e_salv_function.
ENDCLASS.
CLASS lcl_minesweeper IMPLEMENTATION.
  METHOD show_score.
    "
  ENDMETHOD.
  METHOD display_picture.
    IF co_control IS NOT BOUND.
      co_control = NEW cl_gui_picture( parent = co_container )."funny, need parameter but doesn't need to be bounded
      co_control->set_3d_border( 1 ).
      co_control->set_display_mode( cl_gui_picture=>display_mode_stretch ).
      co_control->set_position( height = 75 left   = 35 top    = 130 width  = 320 ).

      co_control->load_picture_from_url( EXPORTING  url = iv_imgpath
                                         EXCEPTIONS OTHERS = 1 ).
    ENDIF.
  ENDMETHOD.
  METHOD constructor.
    av_p_mode     = iv_mode.
    av_p_neighood = iv_neigh.
    CASE av_p_mode.
      WHEN mode_easy.
        av_p_size_x = 8.
        av_p_size_y = 8.
        av_p_mines  = 10.
      WHEN mode_normal.
        av_p_size_x = 16.
        av_p_size_y = 16.
        av_p_mines  = 40.
      WHEN mode_hard.
        av_p_size_x = 30.
        av_p_size_y = 16.
        av_p_mines  = 99.
      WHEN mode_own.
        av_p_size_x = iv_size_x.
        av_p_size_y = iv_size_y.
        av_p_mines  = iv_mines.
    ENDCASE.
    me->build_arena( CHANGING co_arena_display = me->ao_arena_display
                              co_arena_hidden  = me->ao_arena_hidden
                              co_arena_record  = me->ao_arena_record ).
    me->build_set( IMPORTING et_set = me->at_set ).
  ENDMETHOD.
  METHOD display_arena.
    FIELD-SYMBOLS: <fs_disp_table> TYPE ANY TABLE.
    ASSIGN me->ao_arena_display->* TO <fs_disp_table>.

    cl_salv_table=>factory( IMPORTING r_salv_table = me->ao_alv_table
                             CHANGING t_table      = <fs_disp_table> ).

    me->ao_alv_table->get_columns( )->set_color_column( 'COLOR' ).

    "adjust column width
    DO me->av_p_size_x TIMES.
      DATA(lv_colname) = CONV lvc_fname( 'C' && shift_left( CONV string( sy-index ) ) ).
      me->ao_alv_table->get_columns( )->get_column( lv_colname )->set_output_length( 2 ).
    ENDDO.

    "custom status
    me->ao_alv_table->set_screen_status( report        = 'ZMINESWEEPER'
                                         pfstatus      = 'ZMINESWEEPER_GAME'
                                         set_functions = me->ao_alv_table->c_functions_all ).
    "set cell selection
    me->ao_alv_table->get_selections( )->set_selection_mode( IF_SALV_C_SELECTION_MODE=>cell ).

    "set event handlers
    DATA(lo_events) = me->ao_alv_table->get_event( ).
    SET HANDLER me->hdl_dbl_click FOR lo_events.
    SET HANDLER me->hdl_user_cmd FOR lo_events.

    me->ao_alv_table->display( ).
  ENDMETHOD.
  METHOD build_arena.
    FIELD-SYMBOLS: <fs_ref_1> TYPE INDEX TABLE,
                   <fs_ref_2> TYPE INDEX TABLE.

    DATA(lt_fieldcat) = VALUE lvc_t_fcat( FOR i = 1 UNTIL i > av_p_size_x ( datatype  = 'CHAR'
                                                                            inttype   = 'C'
                                                                            intlen    = '0004'
                                                                            domname   = 'ICON'
                                                                            dd_outlen = 4
                                                                            col_pos   = i
                                                                            fieldname = 'C' && shift_left( CONV string( i ) )
                                                                           ) ).
    APPEND VALUE lvc_s_fcat( col_pos   = av_p_size_x + 1
                             fieldname = 'COLOR'
                             ref_field = 'COLOR'
                             ref_table = 'ZTMP_COLOR_SS'
                            ) TO lt_fieldcat.

    cl_alv_table_create=>create_dynamic_table( EXPORTING it_fieldcatalog = lt_fieldcat
                                               IMPORTING ep_table        = co_arena_display ).

    ASSIGN co_arena_display->* TO <fs_ref_1>.
    CREATE DATA co_arena_hidden LIKE <fs_ref_1>.
    ASSIGN co_arena_hidden->* TO <fs_ref_2>.


    CREATE DATA co_arena_record LIKE LINE OF <fs_ref_1>.
    ASSIGN co_arena_record->*   TO FIELD-SYMBOL(<fs_record>).

    DO av_p_size_y TIMES.
      INSERT <fs_record> INTO TABLE <fs_ref_1>.
      INSERT <fs_record> INTO TABLE <fs_ref_2>.
    ENDDO.
  ENDMETHOD.
  METHOD build_set.
    DO me->av_p_size_x TIMES.
      DATA(lv_index_x) = sy-index.
      DO me->av_p_size_y TIMES.
        DATA(lv_index_y) = sy-index.
        APPEND VALUE #( x = lv_index_x y = lv_index_y ) TO et_set.
      ENDDO.
    ENDDO.
  ENDMETHOD.
  METHOD hdl_dbl_click.

  ENDMETHOD.
  METHOD hdl_user_cmd.
    CASE e_salv_function.
      WHEN 'SET_MINE'.
      WHEN 'REMOVE'.
      WHEN 'CHECK'.
      WHEN 'LEGEND'.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
