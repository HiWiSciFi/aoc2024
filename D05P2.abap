FUNCTION Z_FB_AOC_2024_D05_2
  IMPORTING
    IT_INPUT TYPE STRING_TABLE
  EXPORTING
    EV_RESULT TYPE INT8.



  TYPES: BEGIN OF ltys_edge,
           from TYPE i,
           to   TYPE i,
         END OF ltys_edge.

  TYPES: BEGIN OF ltys_page,
           id       TYPE i,
           incoming TYPE i,
         END OF ltys_page.

  TYPES: BEGIN OF ltys_sorted_node,
           id TYPE i,
         END OF ltys_sorted_node.

  TYPES: BEGIN OF ltys_weighted_node,
           id       TYPE i,
           incoming TYPE i,
         END OF ltys_weighted_node.

  TYPES ltyt_edges          TYPE HASHED TABLE OF ltys_edge WITH UNIQUE KEY from to.
  TYPES ltyt_nodes          TYPE HASHED TABLE OF ltys_page WITH UNIQUE KEY id.
  TYPES ltyt_sorted_nodes   TYPE STANDARD TABLE OF ltys_sorted_node WITH KEY id.
  TYPES ltyt_weighted_nodes TYPE HASHED TABLE OF ltys_weighted_node WITH UNIQUE KEY id.

  CLEAR ev_result.

  DATA lt_edges TYPE ltyt_edges.
  DATA lt_nodes TYPE ltyt_nodes.

  DATA(lv_updates_section) = abap_false.

  LOOP AT it_input ASSIGNING FIELD-SYMBOL(<fv_line>).
    IF strlen( <fv_line> ) = 0.
      lv_updates_section = abap_true.
      CONTINUE.
    ENDIF.

    IF lv_updates_section <> abap_true.
      " Create list of dependencies
      SPLIT <fv_line> AT '|' INTO TABLE DATA(lt_splits).
      INSERT VALUE #( from = lt_splits[ 1 ]
                      to   = lt_splits[ 2 ] ) INTO TABLE lt_edges.

      IF NOT line_exists( lt_nodes[ id = lt_splits[ 1 ] ] ).
        INSERT VALUE #( id       = lt_splits[ 1 ]
                        incoming = 0 ) INTO TABLE lt_nodes.
      ENDIF.

      IF NOT line_exists( lt_nodes[ id = lt_splits[ 2 ] ] ).
        INSERT VALUE #( id       = lt_splits[ 2 ]
                        incoming = 1 ) INTO TABLE lt_nodes.
      ELSE.
        lt_nodes[ id = lt_splits[ 2 ] ]-incoming = lt_nodes[ id = lt_splits[ 2 ] ]-incoming + 1.
      ENDIF.
    ELSE.
      " Check if updates are valid
      SPLIT <fv_line> AT ',' INTO TABLE lt_splits.
      DATA(lv_valid) = abap_true.

      " Part 1 (check validity)
      DO lines( lt_splits ) TIMES.
        DATA(lv_outer) = sy-index.
        DO lines( lt_splits ) - lv_outer TIMES.
          LOOP AT lt_edges ASSIGNING FIELD-SYMBOL(<fv_edge>) WHERE to = lt_splits[ lv_outer ].
            IF <fv_edge>-from = lt_splits[ sy-index + lv_outer ].
              lv_valid = abap_false.
              EXIT.
            ENDIF.
          ENDLOOP.
          IF lv_valid = abap_false.
            EXIT.
          ENDIF.
        ENDDO.
        IF lv_valid = abap_false.
          EXIT.
        ENDIF.
      ENDDO.

      " Skip valid entries
      IF lv_valid = abap_true.
        CONTINUE.
      ENDIF.

      " Sort splits
      DATA lt_sorted TYPE ltyt_sorted_nodes.
      CLEAR lt_sorted.

      DATA lt_isplits TYPE ltyt_nodes.
      CLEAR lt_isplits.
      LOOP AT lt_splits ASSIGNING FIELD-SYMBOL(<fv_splits_temp>).
        INSERT VALUE #( id = <fv_splits_temp> ) INTO TABLE lt_isplits.
      ENDLOOP.

      " Get relevant rules
      DATA lt_filtered TYPE ltyt_edges.
      CLEAR lt_filtered.
      LOOP AT lt_edges ASSIGNING <fv_edge>.
        IF line_exists( lt_isplits[ id = <fv_edge>-to ] ) AND line_exists( lt_isplits[ id = <fv_edge>-from ] ).
          INSERT <fv_edge> INTO TABLE lt_filtered.
        ENDIF.
      ENDLOOP.

      DATA lt_weighted TYPE ltyt_weighted_nodes.
      CLEAR lt_weighted.
      LOOP AT lt_splits ASSIGNING FIELD-SYMBOL(<fv_split>).
        DATA(lv_counter) = 0.
        LOOP AT lt_filtered ASSIGNING FIELD-SYMBOL(<fv_filtered>) WHERE to = <fv_split>.
          lv_counter = lv_counter + 1.
        ENDLOOP.
        INSERT VALUE #( id       = <fv_split>
                        incoming = lv_counter ) INTO TABLE lt_weighted.
      ENDLOOP.

      DATA(lv_left) = lines( lt_weighted ).

      WHILE lv_left > 0.

        DATA lt_decrease TYPE ltyt_sorted_nodes.
        CLEAR lt_decrease.
        LOOP AT lt_weighted ASSIGNING FIELD-SYMBOL(<fv_weighted>) WHERE incoming = 0.
          INSERT VALUE #( id = <fv_weighted>-id ) INTO TABLE lt_sorted.
          LOOP AT lt_filtered ASSIGNING <fv_filtered> WHERE from = <fv_weighted>-id.
            INSERT VALUE #( id = <fv_filtered>-to ) INTO TABLE lt_decrease.
          ENDLOOP.
        ENDLOOP.

        DELETE lt_weighted WHERE incoming = 0.

        LOOP AT lt_decrease ASSIGNING FIELD-SYMBOL(<fv_decrease>).
          IF line_exists( lt_weighted[ id = <fv_decrease>-id ] ).
            lt_weighted[ id = <fv_decrease>-id ]-incoming = lt_weighted[ id = <fv_decrease>-id ]-incoming - 1.
          ENDIF.
        ENDLOOP.

        lv_left = lines( lt_weighted ).
      ENDWHILE.

      " add to result
      ev_result = ev_result + lt_sorted[ lines( lt_sorted ) / 2 ]-id.
    ENDIF.
  ENDLOOP.
ENDFUNCTION.
