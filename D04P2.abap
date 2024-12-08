FUNCTION Z_FB_AOC_2024_D04_2
  IMPORTING
    IT_INPUT TYPE STRING_TABLE
  EXPORTING
    EV_RESULT TYPE INT8.



  CLEAR ev_result.

  TYPES: BEGIN OF ltys_vec,
           x TYPE i,
           y TYPE i,
           c TYPE char1,
         END OF ltys_vec.

  TYPES ltyt_vectab     TYPE STANDARD TABLE OF ltys_vec WITH NON-UNIQUE DEFAULT KEY.
  TYPES ltyt_vecchartab TYPE STANDARD TABLE OF ltyt_vectab WITH NON-UNIQUE DEFAULT KEY.

  DATA(lt_patterns) = VALUE ltyt_vecchartab( ( VALUE #( ( x = -1 y = -1 c = 'M' )
                                                        ( x = -1 y =  1 c = 'M' )
                                                        ( x =  1 y = -1 c = 'S' )
                                                        ( x =  1 y =  1 c = 'S' ) ) )
                                             ( VALUE #( ( x = -1 y = -1 c = 'M' )
                                                        ( x = -1 y =  1 c = 'S' )
                                                        ( x =  1 y = -1 c = 'M' )
                                                        ( x =  1 y =  1 c = 'S' ) ) )
                                             ( VALUE #( ( x = -1 y = -1 c = 'S' )
                                                        ( x = -1 y =  1 c = 'S' )
                                                        ( x =  1 y = -1 c = 'M' )
                                                        ( x =  1 y =  1 c = 'M' ) ) )
                                             ( VALUE #( ( x = -1 y = -1 c = 'S' )
                                                        ( x = -1 y =  1 c = 'M' )
                                                        ( x =  1 y = -1 c = 'S' )
                                                        ( x =  1 y =  1 c = 'M' ) ) ) ).

  FIND ALL OCCURRENCES OF 'A' IN TABLE it_input
       RESULTS DATA(lt_match_result).

  LOOP AT lt_match_result ASSIGNING FIELD-SYMBOL(<fs_match>).
    LOOP AT lt_patterns ASSIGNING FIELD-SYMBOL(<ft_pattern>).

      DATA(ls_currpos) = VALUE ltys_vec( x = <fs_match>-offset
                                         y = <fs_match>-line ).
      DATA(lv_hit) = abap_true.

      LOOP AT <ft_pattern> ASSIGNING FIELD-SYMBOL(<fs_comp>).
        ls_currpos-x = <fs_match>-offset + <fs_comp>-x.
        ls_currpos-y = <fs_match>-line + <fs_comp>-y.

        IF ls_currpos-x < 0 OR ls_currpos-x >= strlen( it_input[ 1 ] ) OR ls_currpos-y < 1 OR ls_currpos-y > lines(
            it_input ).
          lv_hit = abap_false.
          EXIT.
        ENDIF.

        DATA(lv_line) = it_input[ ls_currpos-y ].
        DATA(lv_offset) = ls_currpos-x.
        DATA(lv_char) = lv_line+lv_offset(1).

        IF lv_char <> <fs_comp>-c.
          lv_hit = abap_false.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lv_hit = abap_true.
*        WRITE: <fs_match>-offset, <fs_match>-line, <fs_dir>-x, <fs_dir>-y, /.
        ev_result = ev_result + 1.
      ENDIF.

    ENDLOOP.
  ENDLOOP.
ENDFUNCTION.
