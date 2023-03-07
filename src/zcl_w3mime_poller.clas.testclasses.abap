class lcl_poller_test definition final for testing
  duration short
  risk level harmless.

  private section.
    methods manual_state_check for testing.
    methods detect_changes for testing.
    methods merge_changes for testing.
endclass.

class lcl_poller_test implementation.

  method manual_state_check.

    data lo type ref to zcl_w3mime_poller.
    data lt_targets type zcl_w3mime_poller=>tt_target.
    data lt_act type zcl_w3mime_poller=>tt_file_state.
    field-symbols <t> like line of lt_targets.

    append initial line to lt_targets assigning <t>.
    <t>-directory = 'c:\tmp'.

    try.
      "FOR MANUAL TEST ONLY
*      create object lo exporting it_targets = lt_targets.
*      lt_act = lo->read_current_state( ).
*      lt_act = lo->update_state( ).
*      lt_act = lo->update_state( ).
    catch cx_root.
      cl_abap_unit_assert=>fail( 'Unexpected error' ).
    endtry.

  endmethod.

  define _add_state.
    append initial line to &1 assigning <i>.
    <i>-path      = &2.
    <i>-timestamp = &3.
  end-of-definition.

  method detect_changes.
    data:
      lt_prev type zcl_w3mime_poller=>tt_file_state,
      lt_cur  like lt_prev,
      lt_act  like lt_prev,
      lt_exp  like lt_prev.

    field-symbols <i> like line of lt_prev.

    _add_state lt_prev 'A' '20180801120000'.
    _add_state lt_prev 'B' '20180801120000'.

    _add_state lt_cur 'A'   '20180801120000'.
    _add_state lt_cur 'B'   '20180801120001'.
    _add_state lt_cur 'NEW' '20180801120005'.

    _add_state lt_exp 'B'   '20180801120001'.
    _add_state lt_exp 'NEW' '20180801120005'.

    lt_act = zcl_w3mime_poller=>detect_changes( it_prev = lt_prev it_cur = lt_cur ).
    cl_abap_unit_assert=>assert_equals( act = lt_act exp = lt_exp ).

  endmethod.

  method merge_changes.
    data:
      lt_prev type zcl_w3mime_poller=>tt_file_state,
      lt_cur  like lt_prev,
      lt_exp  like lt_prev.

    field-symbols <i> like line of lt_prev.

    _add_state lt_prev 'A' '20180801120000'.
    _add_state lt_prev 'B' '20180801120000'.

    _add_state lt_cur 'A'   '20180801120000'.
    _add_state lt_cur 'B'   '20180801120001'.
    _add_state lt_cur 'NEW' '20180801120005'.

    _add_state lt_exp 'A'   '20180801120000'.
    _add_state lt_exp 'B'   '20180801120001'.
    _add_state lt_exp 'NEW' '20180801120005'.

    zcl_w3mime_poller=>merge_changes( exporting it_changes = lt_cur changing ct_state = lt_prev ).
    cl_abap_unit_assert=>assert_equals( act = lt_prev exp = lt_exp ).

  endmethod.

endclass.
