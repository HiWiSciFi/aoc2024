FUNCTION Z_FB_AOC_2024_D08_2
  IMPORTING
    IT_INPUT TYPE STRING_TABLE
  EXPORTING
    EV_RESULT TYPE INT8.



  CLEAR ev_result.

  TYPES: BEGIN OF ltys_vec,
           x TYPE i,
           y TYPE i,
         END OF ltys_vec.
  TYPES ltyt_unique_vec TYPE SORTED TABLE OF ltys_vec WITH UNIQUE KEY y x.

  TYPES: BEGIN OF ltys_emitters,
           id       TYPE char1,
           emitters TYPE ltyt_unique_vec,
         END OF ltys_emitters.
  TYPES ltyt_emitters TYPE HASHED TABLE OF ltys_emitters WITH UNIQUE KEY id.

  DATA lt_emitters  TYPE ltyt_emitters.
  DATA lt_antinodes TYPE ltyt_unique_vec.

  LOOP AT it_input ASSIGNING FIELD-SYMBOL(<fv_line>).
    DATA(lv_line_index) = sy-tabix.
    DO strlen( <fv_line> ) TIMES.
      DATA(lv_offset) = sy-index - 1.
      DATA(lv_char) = <fv_line>+lv_offset(1).
      IF lv_char = '.'.
        CONTINUE.
      ENDIF.

      ASSIGN lt_emitters[ id = lv_char ]-emitters TO FIELD-SYMBOL(<ft_emitters>).
      IF <ft_emitters> IS NOT ASSIGNED.
        INSERT VALUE #( id       = lv_char
                        emitters = VALUE #( ) ) INTO TABLE lt_emitters ASSIGNING FIELD-SYMBOL(<fs_emitters_entry>).
        ASSIGN <fs_emitters_entry>-emitters TO <ft_emitters>.
      ENDIF.
      INSERT VALUE #( x = lv_offset
                      y = lv_line_index ) INTO TABLE <ft_emitters>.
      UNASSIGN <ft_emitters>.
    ENDDO.
  ENDLOOP.

  LOOP AT lt_emitters ASSIGNING <fs_emitters_entry>.
    LOOP AT <fs_emitters_entry>-emitters ASSIGNING FIELD-SYMBOL(<fs_emitter_1>).
      LOOP AT <fs_emitters_entry>-emitters ASSIGNING FIELD-SYMBOL(<fs_emitter_2>).
        IF <fs_emitter_1>-x = <fs_emitter_2>-x AND <fs_emitter_1>-y = <fs_emitter_2>-y.
          CONTINUE.
        ENDIF.

        " Add emitters as antinodes
        INSERT <fs_emitter_1> INTO TABLE lt_antinodes.
        INSERT <fs_emitter_2> INTO TABLE lt_antinodes.

        " Calc diff vec: v2 - v1
        DATA(ls_diff) = VALUE ltys_vec( x = <fs_emitter_2>-x - <fs_emitter_1>-x
                                        y = <fs_emitter_2>-y - <fs_emitter_1>-y ).

        " Continue in direction 1 until out of bounds
        DO.
          DATA(ls_antinode) = VALUE ltys_vec( x = <fs_emitter_1>-x - ( ls_diff-x * sy-index )
                                              y = <fs_emitter_1>-y - ( ls_diff-y * sy-index ) ).
          IF ls_antinode-x < 0 OR ls_antinode-x >= strlen( it_input[ 1 ] ) OR ls_antinode-y <= 0 OR ls_antinode-y > lines(
              it_input ).
            EXIT.
          ENDIF.
          INSERT ls_antinode INTO TABLE lt_antinodes.
        ENDDO.

        " Continue in direction 2 until out of bounds
        DO.
          ls_antinode = VALUE ltys_vec( x = <fs_emitter_2>-x + ( ls_diff-x * sy-index )
                                        y = <fs_emitter_2>-y + ( ls_diff-y * sy-index ) ).
          IF ls_antinode-x < 0 OR ls_antinode-x >= strlen( it_input[ 1 ] ) OR ls_antinode-y <= 0 OR ls_antinode-y > lines(
              it_input ).
            EXIT.
          ENDIF.
          INSERT ls_antinode INTO TABLE lt_antinodes.
        ENDDO.

      ENDLOOP.
    ENDLOOP.
  ENDLOOP.

  ev_result = lines( lt_antinodes ).
ENDFUNCTION.
