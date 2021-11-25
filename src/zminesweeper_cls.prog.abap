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
      display_picture IMPORTING iv_imgpath   TYPE char256
                       CHANGING co_control   TYPE REF TO cl_gui_picture
                                co_container TYPE REF TO cl_gui_container.
    METHODS:
      constructor     IMPORTING iv_mode   TYPE string
                                iv_neigh  TYPE string
                                iv_size_x TYPE i
                                iv_size_y TYPE i
                                iv_mines  TYPE i.
  PRIVATE SECTION.
    DATA:
      av_p_mode       TYPE string,
      av_p_neighood   TYPE string,
      av_p_size_x     TYPE i,
      av_p_size_y     TYPE i,
      av_p_mines      TYPE i.
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
    av_p_size_x   = iv_size_x.
    av_p_size_y   = iv_size_y.
    av_p_mines    = iv_mines.
  ENDMETHOD.
ENDCLASS.
