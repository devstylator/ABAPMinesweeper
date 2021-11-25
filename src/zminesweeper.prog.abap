*&---------------------------------------------------------------------*
*& Report ZMINESWEEPER
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zminesweeper.

INCLUDE zminesweeper_top. "Datatypes.
INCLUDE zminesweeper_cls. "Model class definition & implementation
INCLUDE zminesweeper_io.  "PAI/PBO modules.
INCLUDE zminesweeper_scr. "Screens definitions (selection screen).

INITIALIZATION.
  SET PF-STATUS 'ZMINESWEEPER_SEL'.

AT SELECTION-SCREEN OUTPUT.
  lcl_minesweeper=>display_picture( EXPORTING iv_imgpath   = lcl_minesweeper=>picture_path
                                     CHANGING co_control   = go_control
                                              co_container = go_container ).

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'EXE'.
      IF go_control IS BOUND.
        go_control->clear_picture( ).
        go_control->free( ).
        CLEAR: go_control.
      ENDIF.
      go_model = NEW lcl_minesweeper( iv_mode   = COND #( WHEN easy   = 'X' THEN lcl_minesweeper=>mode_easy
                                                          WHEN normal = 'X' THEN lcl_minesweeper=>mode_normal
                                                          WHEN hard   = 'X' THEN lcl_minesweeper=>mode_hard
                                                          WHEN own    = 'X' THEN lcl_minesweeper=>mode_own )
                                      iv_neigh  = COND #( WHEN p_m_n  = 'X' THEN lcl_minesweeper=>neighbourhood_neumann
                                                          ELSE lcl_minesweeper=>neighbourhood_moore )
                                      iv_size_x = p_x
                                      iv_size_y = p_y
                                      iv_mines  = p_m ).
      go_model->display_arena( ).
    WHEN 'BACK'.
      LEAVE PROGRAM.
    WHEN 'SCORE'.
      lcl_minesweeper=>show_score( ).
  ENDCASE.
