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
    CLASS-DATA:
      ao_score_container    TYPE REF TO cl_gui_container,
      ao_score_alv          TYPE REF TO cl_salv_table.
    CLASS-METHODS:
      show_score,
      pbo,
      pai,
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
    CONSTANTS:
      cv_bomb          TYPE icon_d VALUE '@5C@',
      cv_flag          TYPE icon_d VALUE '@DF@',
      cv_correct       TYPE icon_d VALUE '@01@',
      cv_incorrect     TYPE icon_d VALUE '@02@'.
    DATA:
      av_p_mode        TYPE string,
      av_p_neighood    TYPE string,
      av_p_size_x      TYPE i,
      av_p_size_y      TYPE i,
      av_p_mines       TYPE i,
      "flag sets after 1st click - mine randomized. 1st click shouldn't
      "be a mine that's why it's here
      av_p_randomized  TYPE abap_bool,
      "timecounter - points
      av_p_run_started TYPE i,
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
      build_arena      CHANGING co_arena_display TYPE REF TO data
                                co_arena_hidden  TYPE REF TO data
                                co_arena_record  TYPE REF TO data,

      build_set       EXPORTING et_set           TYPE tt_random,
      "check board and end game
      check,
      "after dbl click checks if there's a mine and if it's not there reveals sorrounded emtpy fields
      check_minefield IMPORTING iv_x             TYPE i
                                iv_y             TYPE i
                       CHANGING co_arena_display TYPE REF TO data
                                co_arena_hidden  TYPE REF TO data,
      "checks cell's sorroundings
      check_neighbourhood
                      IMPORTING iv_x             TYPE i
                                iv_y             TYPE i
                       CHANGING ct_arena_display TYPE INDEX TABLE
                                ct_arena_check   TYPE INDEX TABLE
                                ct_reveal        TYPE tt_random
                                ct_checked       TYPE tt_random,
      "checks if cell is is selected neighbourhood
      is_in_selected_neighbourhood
                      IMPORTING iv_root_x        TYPE i
                                iv_root_y        TYPE i
                                iv_checked_x     TYPE i
                                iv_checked_y     TYPE i
                      RETURNING VALUE(rv_is_in) TYPE abap_bool,
      "displays end game screen
      game_over       IMPORTING iv_win           TYPE abap_bool,
      "points counters
      points_cnt_start,
      points_cnt_end RETURNING VALUE(rs_points) TYPE zminesweeper,
      "set colors indicating no. of bombs (according to no.)
      set_color_brd    CHANGING co_color_brd     TYPE REF TO data,
      "set numbers indicating no of boms around
      set_number_brd  IMPORTING it_set           TYPE tt_random
                       CHANGING co_color_brd     TYPE REF TO data,
      "randomize minefield
      set_mines       IMPORTING iv_forbid_x      TYPE i
                                iv_forbid_y      TYPE i
                      EXPORTING et_rand          TYPE tt_random
                       CHANGING ct_set           TYPE tt_random,
      "updates display table with fields from previous check
      show_empty_fields
                      IMPORTING it_reveal        TYPE tt_random
                       CHANGING ct_arena_display TYPE INDEX TABLE
                                ct_arena_check   TYPE INDEX TABLE,
      "event handlers
      hdl_dbl_click   FOR EVENT double_click OF cl_salv_events_table
                      IMPORTING row column,
      hdl_user_cmd    FOR EVENT added_function OF cl_salv_events
                      IMPORTING e_salv_function.
ENDCLASS.
CLASS lcl_minesweeper IMPLEMENTATION.
  METHOD show_score.
    CALL SCREEN '0010' STARTING AT 1 1.
  ENDMETHOD.
  METHOD pai.
    lcl_minesweeper=>ao_score_alv->close_screen( ).
    lcl_minesweeper=>ao_score_container->free( ).
    CLEAR: lcl_minesweeper=>ao_score_alv, lcl_minesweeper=>ao_score_container.
    LEAVE TO SCREEN 0.
  ENDMETHOD.
  METHOD pbo.
    CASE sy-dynnr.
      WHEN '0010'.
        SET PF-STATUS 'ZMINESWEEPER_SCORE'.
        lcl_minesweeper=>ao_score_container ?= NEW cl_gui_custom_container( container_name = 'SCORES_CONTAINER' ).
        SELECT FROM zminesweeper FIELDS * INTO TABLE @DATA(lt_scores).
        SORT lt_scores BY score DESCENDING.
        cl_salv_table=>factory( EXPORTING r_container  = lcl_minesweeper=>ao_score_container
                                IMPORTING r_salv_table = lcl_minesweeper=>ao_score_alv
                                 CHANGING t_table      = lt_scores ).
        lcl_minesweeper=>ao_score_alv->get_columns( )->get_column( 'MANDT' )->set_technical( ).
        lcl_minesweeper=>ao_score_alv->get_columns( )->get_column( 'SIZE_X' )->set_long_text( 'Width' ).
        lcl_minesweeper=>ao_score_alv->get_columns( )->get_column( 'SIZE_Y' )->set_long_text( 'Height' ).
        lcl_minesweeper=>ao_score_alv->get_columns( )->get_column( 'MINES' )->set_long_text( 'Mines' ).
        lcl_minesweeper=>ao_score_alv->get_columns( )->get_column( 'UNAME' )->set_long_text( 'User' ).
        lcl_minesweeper=>ao_score_alv->get_columns( )->get_column( 'CREATION_TMSTP' )->set_long_text( 'Creation time' ).
        lcl_minesweeper=>ao_score_alv->get_columns( )->get_column( 'SCORE' )->set_long_text( 'Scores' ).
        lcl_minesweeper=>ao_score_alv->display( ).
    ENDCASE.
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

    "if you want to debug
    "ASSIGN me->ao_arena_hidden->* TO <fs_disp_table>.
    "default
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
    me->ao_alv_table->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>cell ).

    "set event handlers
    DATA(lo_events) = me->ao_alv_table->get_event( ).
    SET HANDLER me->hdl_dbl_click FOR lo_events.
    SET HANDLER me->hdl_user_cmd FOR lo_events.

    me->points_cnt_start( ).
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
  METHOD check.
    FIELD-SYMBOLS: <fs_hid_table> TYPE INDEX TABLE,
                   <fs_dsp_table> TYPE INDEX TABLE.

    ASSIGN me->ao_arena_hidden->*  TO <fs_hid_table>.
    ASSIGN me->ao_arena_display->* TO <fs_dsp_table>.
    DATA(lv_win) = abap_true.
    LOOP AT <fs_dsp_table> ASSIGNING FIELD-SYMBOL(<fs_dsp_rec>).
      DATA(lv_index) = sy-tabix.
      READ TABLE <fs_hid_table> ASSIGNING FIELD-SYMBOL(<fs_hid_rec>) INDEX lv_index.
      DO me->av_p_size_x TIMES.
        DATA(lv_x) = sy-index.
        ASSIGN COMPONENT lv_x OF STRUCTURE <fs_dsp_rec> TO FIELD-SYMBOL(<fs_dsp_fld>).
        ASSIGN COMPONENT lv_x OF STRUCTURE <fs_hid_rec> TO FIELD-SYMBOL(<fs_hid_fld>).
        IF <fs_dsp_fld> = cv_flag.
          IF <fs_hid_fld> <> cv_bomb.
            lv_win = abap_false.
            EXIT.
          ENDIF.
        ELSE.
          IF <fs_hid_fld> = cv_bomb.
            lv_win = abap_false.
            EXIT.
          ENDIF.
        ENDIF.
      ENDDO.
    ENDLOOP.
    me->game_over( lv_win ).
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
  METHOD check_neighbourhood.
    READ TABLE ct_reveal TRANSPORTING NO FIELDS WITH KEY x = iv_x y = iv_y.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.
    READ TABLE ct_checked TRANSPORTING NO FIELDS WITH KEY x = iv_x y = iv_y.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.
    APPEND VALUE #( x = iv_x y = iv_y ) TO ct_checked.
    READ TABLE ct_arena_check ASSIGNING FIELD-SYMBOL(<fs_check_rec>) INDEX iv_y.
    ASSIGN COMPONENT iv_x OF STRUCTURE <fs_check_rec> TO FIELD-SYMBOL(<fs_check_fld>).
    CASE <fs_check_fld>.
      WHEN ''.
        APPEND VALUE #( x = iv_x y = iv_y ) TO ct_reveal.
        DO 3 TIMES.
          DATA(lv_y_index) = iv_y + 2 - sy-index.
          IF lv_y_index < 1 OR lv_y_index > av_p_size_y.
            CONTINUE."out of table
          ENDIF.
          DO 3 TIMES.
            DATA(lv_x_index) = iv_x + 2 - sy-index.
            IF lv_x_index < 1 OR lv_x_index > av_p_size_x.
              CONTINUE."out of table
            ENDIF.
            IF me->is_in_selected_neighbourhood( iv_root_x    = iv_x       iv_root_y    = iv_y
                                                 iv_checked_x = lv_x_index iv_checked_y = lv_y_index ).
              me->check_neighbourhood( EXPORTING iv_x             = lv_x_index
                                                 iv_y             = lv_y_index
                                        CHANGING ct_arena_display = ct_arena_display
                                                 ct_arena_check   = ct_arena_check
                                                 ct_reveal        = ct_reveal
                                                 ct_checked       = ct_checked ).
            ENDIF.
          ENDDO.
        ENDDO.
      WHEN cv_bomb.
        RETURN.
      WHEN OTHERS.
        APPEND VALUE #( x = iv_x y = iv_y ) TO ct_reveal.
    ENDCASE.
  ENDMETHOD.
  METHOD is_in_selected_neighbourhood.
    IF me->av_p_neighood = neighbourhood_moore.
      rv_is_in = abap_true."because we check always wides = moore's neighbourhood
    ELSE.
      DATA(lv_offset) = abs( iv_root_x - iv_checked_x ) + abs( iv_root_y - iv_checked_y ).
      IF lv_offset < 2.
        rv_is_in = abap_true.
      ELSE.
        rv_is_in = abap_false.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD check_minefield.
    FIELD-SYMBOLS: <fs_check_brd> TYPE INDEX TABLE,
                   <fs_displ_brd> TYPE INDEX TABLE.

    "assign vars
    ASSIGN co_arena_display->* TO <fs_displ_brd>.
    ASSIGN co_arena_hidden->* TO <fs_check_brd>.

    READ TABLE <fs_displ_brd> ASSIGNING FIELD-SYMBOL(<fs_displ_rec>) INDEX iv_y.
    ASSIGN COMPONENT iv_x OF STRUCTURE <fs_displ_rec> TO FIELD-SYMBOL(<fs_displ_fld>).

    READ TABLE <fs_check_brd> ASSIGNING FIELD-SYMBOL(<fs_check_rec>) INDEX iv_y.
    ASSIGN COMPONENT iv_x OF STRUCTURE <fs_check_rec> TO FIELD-SYMBOL(<fs_check_fld>).

    "check bomb field
    IF <fs_check_fld> = cv_bomb.
      "game ends
      me->game_over( abap_false ).
    ELSE.
      "check neighbour cells
      DATA lt_reveal TYPE tt_random.
      DATA lt_checked TYPE tt_random.
      me->check_neighbourhood( EXPORTING iv_x             = iv_x
                                         iv_y             = iv_y
                                CHANGING ct_arena_display = <fs_displ_brd>
                                         ct_arena_check   = <fs_check_brd>
                                         ct_reveal        = lt_reveal
                                         ct_checked       = lt_checked ).
      SORT lt_reveal BY y x.
      "reveal empty fields if it is possible according to selected neighbourhood
      me->show_empty_fields( EXPORTING it_reveal        = lt_reveal
                              CHANGING ct_arena_display = <fs_displ_brd>
                                       ct_arena_check   = <fs_check_brd> ).
    ENDIF.
  ENDMETHOD.
  METHOD game_over.
    FIELD-SYMBOLS: <fs_hidden_fld> TYPE INDEX TABLE,
                   <fs_disp_fld>   TYPE INDEX TABLE.

    IF iv_win = abap_true.
      DATA(ls_rec) = points_cnt_end( ).
      MODIFY zminesweeper FROM ls_rec.
      MESSAGE 'You won. Your score: ' && ls_rec-score TYPE 'I' DISPLAY LIKE 'S'.
    ELSE.
      MESSAGE 'You have lost' TYPE 'I' DISPLAY LIKE 'E'.
      "show to user where are the mines are
      ASSIGN me->ao_arena_hidden->*  TO <fs_hidden_fld>.
      ASSIGN me->ao_arena_display->* TO <fs_disp_fld>.
      <fs_disp_fld> = <fs_hidden_fld>.
      ao_alv_table->refresh( ).
    ENDIF.
  ENDMETHOD.
  METHOD points_cnt_start.
    GET RUN TIME FIELD me->av_p_run_started.
  ENDMETHOD.
  METHOD points_cnt_end.
    GET RUN TIME FIELD DATA(lv_stop).
    GET TIME STAMP FIELD DATA(lv_tmstp).
    DATA(lv_points) = ( me->av_p_mines * me->av_p_mines ) / ( me->av_p_size_x * me->av_p_size_y ) * ( lv_stop - me->av_p_run_started ).
    rs_points = VALUE #( uname          = sy-uname
                         creation_tmstp = lv_tmstp
                         score          = lv_points
                         size_x         = av_p_size_x
                         size_y         = av_p_size_y
                         mines          = av_p_mines ).
  ENDMETHOD.
  METHOD set_color_brd.
    "some definitions
    DATA(ls_red) = VALUE lvc_s_scol( color-int = 1 color-col = 6 ).
    DATA(ls_blue) = VALUE lvc_s_scol( color-int = 1 color-col = 1 ).
    DATA(ls_green) = VALUE lvc_s_scol( color-int = 1 color-col = 5 ).
    DATA(ls_yellow) = VALUE lvc_s_scol( color-int = 1 color-col = 3 ).

    "set colors
    FIELD-SYMBOLS: <fs_num_brd> TYPE INDEX TABLE,
                   <fs_color_t> TYPE lvc_t_scol,
                   <fs_color_s> TYPE lvc_s_scol.

    ASSIGN co_color_brd->* TO <fs_num_brd>.

    LOOP AT <fs_num_brd> ASSIGNING FIELD-SYMBOL(<fs_record>).
      DO me->av_p_size_x TIMES.
        DATA(lv_counter) = sy-index.
        ASSIGN COMPONENT lv_counter OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_field>).
        IF <fs_field> CO ' 0123456789'.
          CASE <fs_field>.
            WHEN 0.
              ASSIGN ls_blue    TO <fs_color_s>.
            WHEN 1.
              ASSIGN ls_green   TO <fs_color_s>.
            WHEN 2.
              ASSIGN ls_yellow  TO <fs_color_s>.
            WHEN OTHERS.
              ASSIGN ls_red     TO <fs_color_s>.
          ENDCASE.
          <fs_color_s>-fname = 'C' && shift_left( CONV string( lv_counter ) ).
          ASSIGN COMPONENT 'COLOR' OF STRUCTURE <fs_record> TO <fs_color_t>.
          APPEND <fs_color_s> TO <fs_color_t>.
          UNASSIGN: <fs_color_s>, <fs_color_t>.
        ENDIF.
        UNASSIGN <fs_field>.
      ENDDO.
    ENDLOOP.
  ENDMETHOD.
  METHOD set_number_brd.
    DATA: lv_neighbourhood_x TYPE i,
          lv_neighbourhood_y TYPE i.

    FIELD-SYMBOLS: <fs_num_brd> TYPE INDEX TABLE.

    ASSIGN co_color_brd->* TO <fs_num_brd>.

    LOOP AT it_set ASSIGNING FIELD-SYMBOL(<fs_set>).
      "always moore neighbourhood, only numbers
      lv_neighbourhood_y = <fs_set>-y - 1.
      DO 3 TIMES."N/Current/S
        IF lv_neighbourhood_y > 0 AND lv_neighbourhood_y <= me->av_p_size_y.
          READ TABLE <fs_num_brd> ASSIGNING FIELD-SYMBOL(<fs_record>) INDEX lv_neighbourhood_y.
          lv_neighbourhood_x = <fs_set>-x - 1.
          DO 3 TIMES."W/C/E
            IF lv_neighbourhood_x > 0 AND lv_neighbourhood_x <= me->av_p_size_x.
              ASSIGN COMPONENT lv_neighbourhood_x OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_field>).
              IF <fs_field> IS INITIAL.
                <fs_field> = '1'.
              ELSEIF <fs_field> <> cv_bomb.
                <fs_field> = <fs_field> + 1.
              ENDIF.
              UNASSIGN <fs_field>.
            ENDIF.
            lv_neighbourhood_x = lv_neighbourhood_x + 1.
          ENDDO.
          UNASSIGN <fs_record>.
        ENDIF.
        lv_neighbourhood_y = lv_neighbourhood_y + 1.
      ENDDO.
    ENDLOOP.
  ENDMETHOD.
  METHOD set_mines.
    FIELD-SYMBOLS: <fs_arena_hide> TYPE INDEX TABLE.

    "delete click point
    DELETE ct_set WHERE x = iv_forbid_x AND y = iv_forbid_y.

    ASSIGN me->ao_arena_hidden->* TO <fs_arena_hide>.
    DATA(lv_max_index) = lines( ct_set ).

    DO me->av_p_mines TIMES.
      DATA(lv_index)     = sy-index.
      DATA(lv_cur_index) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                                        min = 1
                                                        max = lv_max_index )->get_next( ).
      READ TABLE ct_set ASSIGNING FIELD-SYMBOL(<fs_rand>) INDEX lv_cur_index.
      APPEND <fs_rand> TO et_rand.

      READ TABLE <fs_arena_hide> ASSIGNING FIELD-SYMBOL(<fs_record>) INDEX <fs_rand>-y.
      ASSIGN COMPONENT <fs_rand>-x OF STRUCTURE <fs_record> TO FIELD-SYMBOL(<fs_field>).

      <fs_field> = cv_bomb.

      DELETE TABLE ct_set FROM <fs_rand>.
      lv_max_index = lv_max_index - 1.
    ENDDO.
  ENDMETHOD.
  METHOD show_empty_fields.
    FIELD-SYMBOLS: <fs_display_color> TYPE lvc_t_scol,
                   <fs_check_color>   TYPE lvc_t_scol.

    LOOP AT it_reveal ASSIGNING FIELD-SYMBOL(<fs_reveal>).
      READ TABLE ct_arena_display ASSIGNING FIELD-SYMBOL(<fs_display_rec>) INDEX <fs_reveal>-y.
      READ TABLE ct_arena_check   ASSIGNING FIELD-SYMBOL(<fs_check_rec>)   INDEX <fs_reveal>-y.
      ASSIGN COMPONENT <fs_reveal>-x OF STRUCTURE <fs_display_rec> TO FIELD-SYMBOL(<fs_display_field>).
      ASSIGN COMPONENT <fs_reveal>-x OF STRUCTURE <fs_check_rec>   TO FIELD-SYMBOL(<fs_check_field>).
      <fs_display_field> = <fs_check_field>.
      ASSIGN COMPONENT 'COLOR' OF STRUCTURE <fs_display_rec> TO <fs_display_color>.
      ASSIGN COMPONENT 'COLOR' OF STRUCTURE <fs_check_rec>   TO <fs_check_color>.
      READ TABLE <fs_display_color> TRANSPORTING NO FIELDS WITH KEY fname = 'C' && shift_left( CONV string( <fs_reveal>-x ) ).
      IF sy-subrc <> 0.
        READ TABLE <fs_check_color> ASSIGNING FIELD-SYMBOL(<fs_check_color_rec>) WITH KEY fname = 'C' && shift_left( CONV string( <fs_reveal>-x ) ).
        APPEND <fs_check_color_rec> TO <fs_display_color>.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD hdl_dbl_click.
    IF me->av_p_randomized = abap_false.
      me->set_mines( EXPORTING iv_forbid_x = CONV int4( column+1 )
                               iv_forbid_y = row
                     IMPORTING et_rand = at_mines
                      CHANGING ct_set  = at_set ).
      me->set_number_brd( EXPORTING it_set       = at_mines
                           CHANGING co_color_brd = me->ao_arena_hidden ).
      me->set_color_brd( CHANGING co_color_brd = me->ao_arena_hidden ).
      me->av_p_randomized = abap_true.
    ENDIF.
    me->check_minefield( EXPORTING iv_x = CONV int4( column+1 )
                                   iv_y = row
                          CHANGING co_arena_display = me->ao_arena_display
                                   co_arena_hidden  = me->ao_arena_hidden ).
    me->ao_alv_table->refresh( ).
  ENDMETHOD.
  METHOD hdl_user_cmd.
    FIELD-SYMBOLS: <fs_arena_display> TYPE INDEX TABLE.
    ASSIGN me->ao_arena_display->* TO <fs_arena_display>.

    me->ao_alv_table->get_metadata( ).
    DATA(lt_cells) = me->ao_alv_table->get_selections( )->get_selected_cells( ).

    CASE e_salv_function.
      WHEN 'SET_MINE' OR 'REMOVE'.
        LOOP AT lt_cells ASSIGNING FIELD-SYMBOL(<fs_cell>).
          READ TABLE <fs_arena_display> ASSIGNING FIELD-SYMBOL(<fs_arena_display_rec>) INDEX <fs_cell>-row.
          ASSIGN COMPONENT <fs_cell>-columnname OF STRUCTURE <fs_arena_display_rec> TO FIELD-SYMBOL(<fs_arena_display_fld>).
          <fs_arena_display_fld> = COND #( WHEN e_salv_function = 'SET_MINE' THEN cv_flag
                                           ELSE '' ).
        ENDLOOP.
        me->ao_alv_table->refresh( ).
      WHEN 'CHECK'.
        me->check( ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
