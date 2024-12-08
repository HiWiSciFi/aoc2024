FUNCTION Z_FB_AOC_2024_D06_2
  IMPORTING
    IT_INPUT TYPE STRING_TABLE
  EXPORTING
    EV_RESULT TYPE INT8.



  TYPES: BEGIN OF ltys_pos,
           x TYPE i,
           y TYPE i,
         END OF ltys_pos.

  TYPES: BEGIN OF ltys_move,
           pos TYPE ltys_pos,
           dir TYPE char1,
         END OF ltys_move.

  TYPES ltyt_pos TYPE SORTED TABLE OF ltys_pos WITH UNIQUE KEY x y.
  TYPES ltyt_move TYPE SORTED TABLE OF ltys_move WITH UNIQUE KEY pos dir.

  DATA lv_field_size TYPE ltys_pos.
  DATA lv_startpos TYPE ltys_pos.
  DATA lv_currpos TYPE ltys_pos.
  DATA lt_obstacles TYPE ltyt_pos.
  DATA lt_visited TYPE ltyt_move.
  DATA lv_dir TYPE char1 VALUE 'N'.

  CLEAR ev_result.

  lv_field_size = VALUE #( x = strlen( it_input[ 1 ] ) y = lines( it_input ) ).

  LOOP AT it_input ASSIGNING FIELD-SYMBOL(<fv_line>).
    DATA(lv_currline) = sy-tabix.
    DO strlen( <fv_line> ) TIMES.
      DATA(lv_offset) = sy-index - 1.
      DATA(lv_char) = <fv_line>+lv_offset(1).
      CASE lv_char.
        WHEN '#'.
          INSERT VALUE #( x = lv_offset y = lv_currline ) INTO TABLE lt_obstacles.
        WHEN '^'.
          lv_startpos = VALUE #( x = lv_offset y = lv_currline ).
      ENDCASE.
    ENDDO.
  ENDLOOP.

  " Can be optimized, but currently runs in under 2 minutes --> let's leave it for now
  " First idea that comes to mind is to limit the locations for the new obstacle to tiles
  " that are visited during part 1. Only those can ever be hit, since there only is a
  " single new obstacle.
  DO strlen( it_input[ 1 ] ) TIMES.
    DATA(lv_xpos) = sy-index - 1.
    DO lines( it_input ) TIMES.
      DATA(lv_loop) = abap_false.
      lv_currpos = lv_startpos.
      CLEAR lt_visited.
      lv_dir = 'N'.

      DATA(lt_obstacles_t) = lt_obstacles.
      IF line_exists( lt_obstacles_t[ x = lv_xpos y = sy-index ] ).
        CONTINUE.
      ENDIF.
      INSERT VALUE #( x = lv_xpos y = sy-index ) INTO TABLE lt_obstacles_t.

      DO.
        IF line_exists( lt_visited[ pos = lv_currpos dir = lv_dir ] ).
          lv_loop = abap_true.
          EXIT.
        ENDIF.

        INSERT VALUE #( pos = lv_currpos dir = lv_dir ) INTO TABLE lt_visited.

        CASE lv_dir.
          WHEN 'N'.
            IF line_exists( lt_obstacles_t[ x = lv_currpos-x y = lv_currpos-y - 1 ] ).
              lv_dir = 'E'.
            ELSE.
              lv_currpos-y = lv_currpos-y - 1.
            ENDIF.
          WHEN 'E'.
            IF line_exists( lt_obstacles_t[ x = lv_currpos-x + 1 y = lv_currpos-y ] ).
              lv_dir = 'S'.
            ELSE.
              lv_currpos-x = lv_currpos-x + 1.
            ENDIF.
          WHEN 'S'.
            IF line_exists( lt_obstacles_t[ x = lv_currpos-x y = lv_currpos-y + 1 ] ).
              lv_dir = 'W'.
            ELSE.
              lv_currpos-y = lv_currpos-y + 1.
            ENDIF.
          WHEN 'W'.
            IF line_exists( lt_obstacles_t[ x = lv_currpos-x - 1 y = lv_currpos-y ] ).
              lv_dir = 'N'.
            ELSE.
              lv_currpos-x = lv_currpos-x - 1.
            ENDIF.
        ENDCASE.

        IF lv_currpos-x < 0 OR lv_currpos-x > lv_field_size-x - 1 OR lv_currpos-y < 1 OR lv_currpos-y > lv_field_size-y.
          EXIT.
        ENDIF.
      ENDDO.

      IF lv_loop = abap_true.
        ev_result = ev_result + 1.
      ENDIF.
    ENDDO.
  ENDDO.

ENDFUNCTION.
