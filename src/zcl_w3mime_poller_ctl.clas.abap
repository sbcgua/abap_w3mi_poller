class ZCL_W3MIME_POLLER_CTL definition
  public
  final
  create public .

  public section.

    methods constructor
      importing
        iv_interval type i
        it_targets  type zif_w3mime=>tty_poll_targets
        iv_optimize_dir_reads type abap_bool default abap_false
      raising
        zcx_w3mime_error.

    methods start
      raising
        zcx_w3mime_error.

    methods handle_changed
      for event changed of zcl_w3mime_poller
      importing changed_list.
    methods handle_error
      for event error of zcl_w3mime_poller
      importing error_text.

  protected section.
  private section.

    data mo_poller  type ref to zcl_w3mime_poller.
    data mt_targets type zif_w3mime=>tty_poll_targets.

    class-methods format_dt
      importing
        iv_ts         type zcl_w3mime_poller=>ty_file_state-timestamp
      returning
        value(rv_str) type string.

ENDCLASS.



CLASS ZCL_W3MIME_POLLER_CTL IMPLEMENTATION.


  method constructor.

    data lt_file_targets type zcl_w3mime_poller=>tt_target.
    data lv_idx  type char10.
    data lv_msg  type string.

    field-symbols <t> like line of mt_targets.
    field-symbols <ft> like line of lt_file_targets.

    if lines( it_targets ) = 0.
      zcx_w3mime_error=>raise( 'Specify poll targets' ). "#EC NOTEXT
    endif.

    mt_targets = it_targets.
    write / 'Targets:'. "#EC NOTEXT

    loop at mt_targets assigning <t>.
      lv_idx = sy-tabix.

      <t>-path = to_upper( <t>-path ). " To search after
      zcl_w3mime_validator=>validate_params(
        iv_filename = <t>-path
        is_w3key    = <t>-w3key ).

      zcl_w3mime_fs=>resolve_filename(
        exporting
          iv_path      = <t>-path
        importing
          ev_filename  = <t>-filename
          ev_directory = <t>-directory ).

      lv_msg = |  ({ condense( lv_idx ) }):|
        && | { <t>-w3key-objid } [{ <t>-w3key-relid }]|
        && | <=> { <t>-path }|. " format_dt( ls_attr ) }]|.
      write / lv_msg.

      append initial line to lt_file_targets assigning <ft>.
      <ft>-directory = <t>-directory.
      <ft>-filter    = <t>-filename.

    endloop.

    write / 'Staring polling ...'. "#EC NOTEXT
    uline.
    create object mo_poller
      exporting
        iv_optimize_dir_reads = iv_optimize_dir_reads
        it_targets  = lt_file_targets
        iv_interval = iv_interval.
    set handler me->handle_changed for mo_poller.
    set handler me->handle_error for mo_poller.

  endmethod.


  method format_dt.

    data ts type char14.

    ts = iv_ts.

    rv_str =
      |{ ts+0(4) }-{ ts+4(2) }-{ ts+6(2) } | &&
      |{ ts+8(2) }:{ ts+10(2) }:{ ts+12(2) }|.

  endmethod.


  method handle_changed.

    data lv_msg  type string.
    data lx type ref to zcx_w3mime_error.

    field-symbols <i> like line of changed_list.
    field-symbols <t> like line of mt_targets.

    loop at changed_list assigning <i>.
      <i>-path = to_upper( <i>-path ).
      read table mt_targets assigning <t> with key path = <i>-path.
      assert sy-subrc is initial.

      lv_msg = |File changed: { <t>-filename } [{ format_dt( <i>-timestamp ) }]|.
      write / lv_msg.

      try.
        zcl_w3mime_utils=>upload(
          iv_filename = <t>-path
          iv_type     = <t>-w3key-relid
          iv_key      = <t>-w3key-objid ).
      catch zcx_w3mime_error into lx.
        message lx->msg type 'E'.
      endtry.
    endloop.

  endmethod.


  method handle_error.
    message error_text type 'E'.
  endmethod.


  method start.
    mo_poller->start( ).
  endmethod.
ENDCLASS.
