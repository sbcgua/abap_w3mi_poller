class zcl_w3mime_poller definition
  public
  final
  create public .

  public section.

    types:
      begin of ty_target,
        directory type string,
        filter    type string,
      end of ty_target .
    types:
      tt_target type standard table of ty_target with default key .
    types:
      begin of ty_file_state,
        path      type string,
        isdir     type abap_bool, " for future use
        timestamp type char14,
      end of ty_file_state .
    types:
      tt_file_state type standard table of ty_file_state with key path .

    events changed
      exporting
        value(changed_list) type tt_file_state .
    events error
      exporting
        value(error_text) type string .

    methods constructor
      importing
        !it_targets  type tt_target
        !iv_interval type i default 1 " 1 second
      raising
        zcx_w3mime_error .
    methods start
      raising
        zcx_w3mime_error .
    methods handle_timer
        for event finished of cl_gui_timer .
    methods read_current_state
      returning
        value(rt_state) type tt_file_state
      raising
        zcx_w3mime_error .
    methods update_state
      returning
        value(rt_changes) type tt_file_state
      raising
        zcx_w3mime_error .
    class-methods detect_changes
      importing
        !it_prev          type tt_file_state
        !it_cur           type tt_file_state
      returning
        value(rt_changes) type tt_file_state .
    class-methods merge_changes
      importing
        !it_changes type tt_file_state
      changing
        !ct_state   type tt_file_state .

  protected section.
  private section.
    data mt_targets    type tt_target.
    data mt_file_state type tt_file_state.
    data mo_timer      type ref to cl_gui_timer.
ENDCLASS.



CLASS ZCL_W3MIME_POLLER IMPLEMENTATION.


  method constructor.
    if lines( it_targets ) = 0.
      zcx_w3mime_error=>raise( 'Specify poll targets' ).    "#EC NOTEXT
    endif.

    data lv_sep type c.
    cl_gui_frontend_services=>get_file_separator( changing file_separator = lv_sep ).
    mt_targets = it_targets.

    field-symbols: <t> like line of mt_targets.
    loop at mt_targets assigning <t>.
      if <t>-directory is initial.
        zcx_w3mime_error=>raise( 'Target directory cannot be empty' ). "#EC NOTEXT
      endif.
      if substring( val = <t>-directory off = strlen( <t>-directory ) - 1 len = 1 ) <> lv_sep.
        <t>-directory = <t>-directory && lv_sep.
      endif.
      if <t>-filter is initial.
        <t>-filter = '*.*'.
      endif.
    endloop.

    create object mo_timer.
    set handler me->handle_timer for mo_timer.
    mo_timer->interval = iv_interval.

  endmethod.


  method detect_changes.

    " TODO detect deletions ! file in prev but not in cur
    field-symbols <c> like line of it_cur.
    field-symbols <p> like line of it_cur.

    " assuming both inputs are sorted
    loop at it_cur assigning <c>.
      read table it_prev assigning <p> with key path = <c>-path binary search.
      if sy-subrc is not initial or <c>-timestamp <> <p>-timestamp.
        append <c> to rt_changes.
      endif.
    endloop.

  endmethod.


  method handle_timer.

    data lx         type ref to zcx_w3mime_error.
    data lv_text    type string.
    data lt_changes like mt_file_state.

    try.
      lt_changes = update_state( ).
    catch zcx_w3mime_error into lx.
      lv_text = lx->get_text( ).
      raise event error exporting error_text = lv_text.
    endtry.

    if lines( lt_changes ) > 0.
      raise event changed exporting changed_list = lt_changes.
    endif.

    mo_timer->run( ).

  endmethod.


  method merge_changes.

    if lines( it_changes ) = 0.
      return.
    endif.
    append lines of it_changes to ct_state.
    sort ct_state by path ascending timestamp descending.
    delete adjacent duplicates from ct_state comparing path.

  endmethod.


  method read_current_state.

    data lt_files type standard table of file_info.
    field-symbols <state> like line of rt_state.
    field-symbols <t>     like line of mt_targets.

    loop at mt_targets assigning <t>.
      lt_files = zcl_w3mime_fs=>read_dir(
        iv_dir    = <t>-directory
        iv_filter = <t>-filter ).

      field-symbols <file> like line of lt_files.
      loop at lt_files assigning <file> where isdir is initial.
        append initial line to rt_state assigning <state>.
        <state>-path = <t>-directory && <file>-filename.
        concatenate <file>-writedate <file>-writetime into <state>-timestamp. " Hmmm
*      cl_abap_tstmp=>systemtstmp_syst2utc(
*        exporting
*          syst_date = <file>-writedate
*          syst_time = <file>-writetime
*        importing
*          utc_tstmp = <state>-timestamp ).
      endloop.
    endloop.

    sort rt_state by path.

  endmethod.


  method start.
    mt_file_state = read_current_state( ). " Sorted
    mo_timer->run( ).
  endmethod.


  method update_state.

    data lt_state like mt_file_state.

    lt_state = read_current_state( ). " Sorted

    rt_changes = detect_changes(
      it_prev = mt_file_state
      it_cur  = lt_state ).

    merge_changes(
      exporting
        it_changes = rt_changes
      changing
        ct_state = mt_file_state ).

  endmethod.
ENDCLASS.
