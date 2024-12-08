FUNCTION Z_FB_AOC_2024_D06_1
  IMPORTING
    IT_INPUT TYPE STRING_TABLE
  EXPORTING
    EV_RESULT TYPE INT8.



  TYPES: BEGIN OF ltys_pos,
         x type i,
         y type i,
         end of ltys_pos.

  TYPES ltyt_pos TYPE SORTED TABLE OF ltys_pos WITH UNIQUE KEY x y.

  DATA lv_field_size TYPE ltys_pos.
  DATA lv_currpos TYPE ltys_pos.
  DATA lt_obstacles TYPE ltyt_pos.
  DATA lt_visited TYPE ltyt_pos.
  DATA lv_dir TYPE c VALUE 'N'.

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
          lv_currpos = VALUE #( x = lv_offset y = lv_currline ).
      ENDCASE.
    ENDDO.
  ENDLOOP.

  DATA(lv_border) = abap_false.

  WHILE lv_border <> abap_true.
    INSERT lv_currpos INTO TABLE lt_visited.

    CASE lv_dir.
      WHEN 'N'.
        IF line_exists( lt_obstacles[ x = lv_currpos-x y = lv_currpos-y - 1 ] ).
          lv_dir = 'E'.
        ELSE.
          lv_currpos-y = lv_currpos-y - 1.
        ENDIF.
      WHEN 'E'.
        IF line_exists( lt_obstacles[ x = lv_currpos-x + 1 y = lv_currpos-y ] ).
          lv_dir = 'S'.
        ELSE.
          lv_currpos-x = lv_currpos-x + 1.
        ENDIF.
      WHEN 'S'.
        IF line_exists( lt_obstacles[ x = lv_currpos-x y = lv_currpos-y + 1 ] ).
          lv_dir = 'W'.
        ELSE.
          lv_currpos-y = lv_currpos-y + 1.
        ENDIF.
      WHEN 'W'.
        IF line_exists( lt_obstacles[ x = lv_currpos-x - 1 y = lv_currpos-y ] ).
          lv_dir = 'N'.
        ELSE.
          lv_currpos-x = lv_currpos-x - 1.
        ENDIF.
    ENDCASE.

    IF lv_currpos-x < 0 OR lv_currpos-x > lv_field_size-x - 1 OR lv_currpos-y < 1 OR lv_currpos-y > lv_field_size-y.
      lv_border = abap_true.
    ENDIF.
  ENDWHILE.

  ev_result = lines( lt_visited ).

ENDFUNCTION.
