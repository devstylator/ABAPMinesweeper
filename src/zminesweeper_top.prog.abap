*&---------------------------------------------------------------------*
*& Include          ZMINESWEEPER_TYP
*&---------------------------------------------------------------------*
CLASS lcl_minesweeper DEFINITION DEFERRED.

TYPES: BEGIN OF t_moore,
        row TYPE i,
        col TYPE string,
       END OF t_moore.

TYPES: BEGIN OF t_neighborhood,
        row TYPE i,
        col TYPE string,
       END OF t_neighborhood.

TYPES: BEGIN OF t_random,
        x TYPE i,
        y TYPE i,
       END OF t_random.

TYPES: tt_random TYPE TABLE OF t_random.

DATA: go_model TYPE REF TO lcl_minesweeper,
      "picture container and object
      go_control   TYPE REF TO cl_gui_picture,
      go_container TYPE REF TO cl_gui_container.
