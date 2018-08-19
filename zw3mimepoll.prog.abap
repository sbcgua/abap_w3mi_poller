*&---------------------------------------------------------------------*
*& Report  ZW3MIMEPOLL
*&---------------------------------------------------------------------*

report zw3mimepoll.
tables sscrfields.

constants gc_w3mime_poller_version type string value '1.0.0'.

types:
  begin of ty_w3obj_key,
    relid    type wwwdata-relid,
    objid    type wwwdata-objid,
  end of ty_w3obj_key.


*&---------------------------------------------------------------------*
*& CLASS lcl_validator
*&---------------------------------------------------------------------*

class lcl_validator definition final.
  public section.
    class-methods validate_params
      importing
        iv_filename        type string
        is_w3key           type ty_w3obj_key
        iv_skip_file_check type abap_bool default abap_false
      raising zcx_w3mime_error.

endclass. "lcl_validator

class lcl_validator implementation.

  method validate_params.

    if iv_skip_file_check is initial
       and abap_false = cl_gui_frontend_services=>file_exist( iv_filename ).
      zcx_w3mime_error=>raise( 'File does not exist' ). "#EC NOTEXT
    endif.

    if abap_false = zcl_w3mime_storage=>check_obj_exists( iv_type = is_w3key-relid iv_key = is_w3key-objid ).
      zcx_w3mime_error=>raise( 'MIME object does not exist' ). "#EC NOTEXT
    endif.

  endmethod.  " validate_params.

endclass. "lcl_validator

*&---------------------------------------------------------------------*
*& lcl_poller
*&---------------------------------------------------------------------*

class lcl_poller definition final.
  public section.

    types:
      begin of ty_poll_target,
        path      type string,
        filename  type string,
        directory type string,
        w3key     type ty_w3obj_key,
        timestamp type timestamp,
      end of ty_poll_target,
      tt_poll_targets type standard table of ty_poll_target with default key.

    methods constructor
      importing
        iv_interval type i
        it_targets  type tt_poll_targets
      raising zcx_w3mime_error.

    methods start
      raising zcx_w3mime_error.

    methods handle_changed for event changed of zcl_w3mime_poller importing changed_list.
    methods handle_error for event error of zcl_w3mime_poller importing error_text.

    class-methods format_dt
      importing iv_ts         type zcl_w3mime_poller=>ty_file_state-timestamp
      returning value(rv_str) type string.

  private section.

    data: mo_poller  type ref to zcl_w3mime_poller,
          mt_targets type tt_poll_targets.

endclass.   "lcl_poller

class lcl_poller implementation.

  method format_dt.
    data ts type char14.
    ts = iv_ts.

    rv_str = |{ ts+0(4) }-{ ts+4(2) }-{ ts+6(2) } |
          && |{ ts+8(2) }:{ ts+10(2) }:{ ts+12(2) }|.

  endmethod.  " format_dt.

  method constructor.

    data:
          lt_file_targets type zcl_w3mime_poller=>tt_target,
          lv_idx  type char10,
          lv_msg  type string.

    field-symbols: <t> like line of mt_targets.
    field-symbols: <ft> like line of lt_file_targets.

    if lines( it_targets ) = 0.
      zcx_w3mime_error=>raise( 'Specify poll targets' ). "#EC NOTEXT
    endif.

    mt_targets = it_targets.
    write / 'Targets:'. "#EC NOTEXT

    loop at mt_targets assigning <t>.
      lv_idx = sy-tabix.

      lcl_validator=>validate_params(
        iv_filename = <t>-path
        is_w3key    = <t>-w3key ).

      zcl_w3mime_fs=>resolve_filename(
        exporting iv_path      = <t>-path
        importing ev_filename  = <t>-filename
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
        it_targets  = lt_file_targets
        iv_interval = iv_interval.
    set handler me->handle_changed for mo_poller.
    set handler me->handle_error for mo_poller.

  endmethod.  " constructor.

  method start.
    mo_poller->start( ).
  endmethod.  "start.

  method handle_changed.
    data:
          lv_msg  type string,
          lx type ref to zcx_w3mime_error.

    field-symbols <i> like line of changed_list.
    field-symbols <t> like line of mt_targets.

    loop at changed_list assigning <i>.
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

  endmethod.  "handle_changed

  method handle_error.
    message error_text type 'E'.
  endmethod.  " handle_error.

endclass. " lcl_poller

*&---------------------------------------------------------------------*
*& lcl_app
*&---------------------------------------------------------------------*

class lcl_app definition final.
  public section.
    class-methods run
      importing
        it_targets  type lcl_poller=>tt_poll_targets
        do_download type abap_bool
        do_upload   type abap_bool
      raising zcx_w3mime_error.
endclass.

class lcl_app implementation.

  method run.
    if lines( it_targets ) = 0.
      message 'Please specify at least one target pair' type 'E'. "#EC NOTEXT
    endif.

    field-symbols <t> like line of it_targets.
    loop at it_targets assigning <t>.
      lcl_validator=>validate_params(
        iv_filename        = <t>-path
        is_w3key           = <t>-w3key
        iv_skip_file_check = boolc( do_download = abap_true ) ).
    endloop.

    if do_upload is not initial.
      loop at it_targets assigning <t>.
        zcl_w3mime_utils=>upload(
          iv_filename = <t>-path
          iv_type     = <t>-w3key-relid
          iv_key      = <t>-w3key-objid ).
      endloop.
      write: / 'Initial action:' color 7, 'Files uploaded to the system'. "#EC NOTEXT
    elseif do_download is not initial.
      loop at it_targets assigning <t>.
        zcl_w3mime_utils=>download(
          iv_filename = <t>-path
          iv_type     = <t>-w3key-relid
          iv_key      = <t>-w3key-objid ).
      endloop.
      write: / 'Initial action:' color 7, 'Files downloaded to the frontend'. "#EC NOTEXT
    endif.

    data lo_poller type ref to lcl_poller.
    create object lo_poller
      exporting
        it_targets  = it_targets
        iv_interval = 1. " 1 sec
    lo_poller->start( ).

  endmethod.  " run.

endclass.

**********************************************************************

constants:
  GC_FILE_PARAM_NAME TYPE CHAR20 VALUE 'ZW3MIMEPOLL_FILE',
  GC_OBJ_PARAM_NAME  TYPE CHAR20 VALUE 'ZW3MIMEPOLL_OBJ'.

selection-screen begin of block b1 with frame title txt_b1.

selection-screen begin of line.
selection-screen comment (24) t_obj1 for field p_obj1.
parameters p_obj1  type w3objid.
parameters p_file1 type char255.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment (24) t_obj2 for field p_obj2.
parameters p_obj2  type w3objid.
parameters p_file2 type char255.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment (24) t_obj3 for field p_obj3.
parameters p_obj3  type w3objid.
parameters p_file3 type char255.
selection-screen end of line.

selection-screen end of block b1.

selection-screen begin of block b2 with frame title txt_b2.

selection-screen begin of line.
selection-screen comment (24) txt_noac for field p_noact.
parameters p_noact type char1 radiobutton group r1 default 'X'.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment (24) txt_down for field p_down.
parameters p_down type char1 radiobutton group r1.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment (24) txt_upl for field p_upl.
parameters p_upl type char1 radiobutton group r1.
selection-screen end of line.

selection-screen end of block b2.

selection-screen function key 1.

initialization.
  txt_b1   = 'Poll targets'.            "#EC NOTEXT
  t_obj1   = 'W3MI object / File path'. "#EC NOTEXT
  t_obj2   = 'W3MI object / File path'. "#EC NOTEXT
  t_obj3   = 'W3MI object / File path'. "#EC NOTEXT

  txt_b2   = 'Start parameters'.        "#EC NOTEXT
  txt_noac = 'Just start polling'.      "#EC NOTEXT
  txt_down = 'Download before polling'. "#EC NOTEXT
  txt_upl  = 'Upload before polling'.   "#EC NOTEXT

  sscrfields-functxt_01 = 'Set dummy'.  "#EC NOTEXT

  get parameter id GC_FILE_PARAM_NAME field p_file1.
  get parameter id GC_OBJ_PARAM_NAME field p_obj1.

at selection-screen on value-request for p_file1.
  perform f4_file_path changing p_file1.

at selection-screen on value-request for p_obj1.
  perform f4_mime_path changing p_obj1.

at selection-screen on value-request for p_file2.
  perform f4_file_path changing p_file2.

at selection-screen on value-request for p_obj2.
  perform f4_mime_path changing p_obj2.

at selection-screen on value-request for p_file3.
  perform f4_file_path changing p_file3.

at selection-screen on value-request for p_obj3.
  perform f4_mime_path changing p_obj3.

at selection-screen on p_file1.
  if p_file1 is not initial.
    set parameter id GC_FILE_PARAM_NAME field p_file1.
  endif.

at selection-screen on p_obj1.
  if p_obj1 is not initial.
    set parameter id GC_OBJ_PARAM_NAME field p_obj1.
  endif.

at selection-screen.
  case sy-ucomm.
    when 'FC01'.          "Set dummy
      p_obj1  = 'ZMIME_POLLER_TEST'.
      p_file1 = 'zmime_poller_test.txt'.
  endcase.

**********************************************************************
* MAIN
**********************************************************************

start-of-selection.

  data:
        gt_targets type lcl_poller=>tt_poll_targets,
        gx         type ref to zcx_w3mime_error.

  field-symbols <g_target> like line of gt_targets.

  append initial line to gt_targets assigning <g_target>.
  <g_target>-w3key-relid = 'MI'. " Fix to mime for the moment
  <g_target>-w3key-objid = p_obj1.
  <g_target>-path        = p_file1.

  append initial line to gt_targets assigning <g_target>.
  <g_target>-w3key-relid = 'MI'. " Fix to mime for the moment
  <g_target>-w3key-objid = p_obj2.
  <g_target>-path        = p_file2.

  append initial line to gt_targets assigning <g_target>.
  <g_target>-w3key-relid = 'MI'. " Fix to mime for the moment
  <g_target>-w3key-objid = p_obj3.
  <g_target>-path        = p_file3.

  try.
    delete gt_targets where w3key-objid is initial and path is initial.
    lcl_app=>run(
      it_targets  = gt_targets
      do_upload   = p_upl
      do_download = p_down ).
  catch zcx_w3mime_error into gx.
    message gx->msg type 'E'.
  endtry.

**********************************************************************
* FORMS
**********************************************************************

form f4_file_path changing c_path type char255.

  data:
        lt_files type filetable,
        lv_rc    type i,
        lv_uact  type i.

  field-symbols <file> like line of lt_files.

  cl_gui_frontend_services=>file_open_dialog(
    changing
      file_table  = lt_files
      rc          = lv_rc
      user_action = lv_uact
    exceptions others = 4 ).

  if sy-subrc > 0 OR lv_uact <> cl_gui_frontend_services=>action_ok.
    return.
  endif.

  read table lt_files assigning <file> index 1.
  if sy-subrc = 0.
    c_path = <file>-filename.
    set parameter id GC_FILE_PARAM_NAME field c_path.
  endif.

endform.                    "set_file_path

*&---------------------------------------------------------------------*
*&      Form  set_mime_path
*&---------------------------------------------------------------------*
form f4_mime_path changing c_path.

  types:
    begin of t_w3head,
      objid type wwwdata-objid,
      text  type wwwdata-text,
    end of t_w3head.

  data:
        ls_return type ddshretval,
        lt_data   type standard table of t_w3head,
        lt_return type standard table of ddshretval.

  select distinct objid text from wwwdata
    into corresponding fields of table lt_data
    where relid = 'MI'
    and   objid like 'Z%'
    order by objid.

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield        = 'OBJID'
      dynprofield     = 'P_MPATH'
      value_org       = 'S'
    tables
      value_tab       = lt_data
      return_tab      = lt_return
    exceptions
      parameter_error = 1
      no_values_found = 2
      others          = 3.

  if sy-subrc is not initial.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

  read table lt_return into ls_return index 1.
  c_path = ls_return-fieldval.
  set parameter id GC_OBJ_PARAM_NAME field ls_return-fieldval.

endform.                    "set_mime_path
