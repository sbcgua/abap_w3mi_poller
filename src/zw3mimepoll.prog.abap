report zw3mimepoll.

class lcl_app definition final.
  public section.
    class-methods run_file_by_file
      importing
        it_targets  type zif_w3mime=>tty_poll_targets
        do_download type abap_bool
        do_upload   type abap_bool
      raising
        zcx_w3mime_error.
    class-methods run_by_package
      importing
        iv_package  type devclass
        ext_to_dir  type abap_bool
        do_download type abap_bool
        do_upload   type abap_bool
      raising
        zcx_w3mime_error.
endclass.

class lcl_app implementation.

  method run_file_by_file.
    if lines( it_targets ) = 0.
      message 'Please specify at least one target pair' type 'E'. "#EC NOTEXT
    endif.

    field-symbols <t> like line of it_targets.
    loop at it_targets assigning <t>.
      zcl_w3mime_validator=>validate_params(
        iv_filename        = <t>-path
        is_w3key           = <t>-w3key
        iv_skip_file_check = boolc( do_download = abap_true ) ).
    endloop.

    if do_upload = abap_true.
      loop at it_targets assigning <t>.
        zcl_w3mime_utils=>upload(
          iv_filename = <t>-path
          iv_type     = <t>-w3key-relid
          iv_key      = <t>-w3key-objid ).
      endloop.
      write: / 'Initial action:' color 7, 'Files uploaded to the system'. "#EC NOTEXT
    elseif do_download = abap_true.
      loop at it_targets assigning <t>.
        zcl_w3mime_utils=>download(
          iv_filename = <t>-path
          iv_type     = <t>-w3key-relid
          iv_key      = <t>-w3key-objid ).
      endloop.
      write: / 'Initial action:' color 7, 'Files downloaded to the frontend'. "#EC NOTEXT
    endif.

    data lo_poller type ref to zcl_w3mime_poller_ctl.
    create object lo_poller
      exporting
        it_targets  = it_targets
        iv_interval = 1. " 1 sec
    lo_poller->start( ).

  endmethod.

  method run_by_package.
    write: / 'TODO'.
  endmethod.

endclass.

**********************************************************************

tables sscrfields.

constants gc_file_param_name type char20 value 'ZW3MIMEPOLL_FILE'.
constants gc_obj_param_name  type char20 value 'ZW3MIMEPOLL_OBJ'.

selection-screen begin of block b0 with frame title txt_b0.
  parameters p_m_fbf type xfeld radiobutton group mode default 'X' user-command sw.
  parameters p_m_pkg type xfeld radiobutton group mode.
selection-screen end of block b0.

selection-screen begin of block b1 with frame title txt_b1.

  selection-screen begin of line.
    selection-screen comment (24) t_obj1 for field p_obj1 modif id fbf.
    parameters p_obj1  type w3objid modif id fbf.
    parameters p_file1 type char255 modif id fbf.
  selection-screen end of line.

  selection-screen begin of line.
    selection-screen comment (24) t_obj2 for field p_obj2 modif id fbf.
    parameters p_obj2  type w3objid modif id fbf.
    parameters p_file2 type char255 modif id fbf.
  selection-screen end of line.

  selection-screen begin of line.
    selection-screen comment (24) t_obj3 for field p_obj3 modif id fbf.
    parameters p_obj3  type w3objid modif id fbf.
    parameters p_file3 type char255 modif id fbf.
  selection-screen end of line.

  selection-screen begin of line.
    selection-screen comment (24) t_obj4 for field p_obj4 modif id fbf.
    parameters p_obj4  type w3objid modif id fbf.
    parameters p_file4 type char255 modif id fbf.
  selection-screen end of line.

  selection-screen begin of line.
    selection-screen comment (24) t_obj5 for field p_obj5 modif id fbf.
    parameters p_obj5  type w3objid modif id fbf.
    parameters p_file5 type char255 modif id fbf.
  selection-screen end of line.

selection-screen end of block b1.

selection-screen begin of block b2 with frame title txt_b2.
  parameters p_pkg type devclass modif id pkg.
  parameters p_e2dir type xfeld modif id pkg.
selection-screen end of block b2.

selection-screen begin of block b3 with frame title txt_b3.

  selection-screen begin of line.
    selection-screen comment (24) txt_noac for field p_noact.
    parameters p_noact type xfeld radiobutton group r1 default 'X'.
  selection-screen end of line.

  selection-screen begin of line.
    selection-screen comment (24) txt_down for field p_down.
    parameters p_down type xfeld radiobutton group r1.
  selection-screen end of line.

  selection-screen begin of line.
    selection-screen comment (24) txt_upl for field p_upl.
    parameters p_upl type xfeld radiobutton group r1.
  selection-screen end of line.

selection-screen end of block b3.

selection-screen function key 1.

at selection-screen output.
  perform modify_screen.

form modify_screen.
  data active_group like screen-group1.
  if p_m_pkg = abap_true.
    active_group = 'PKG'.
  else.
    active_group = 'FBF'.
  endif.

  loop at screen.
    check screen-group1 is not initial.
    if screen-group1 = active_group.
      screen-active = '1'.
    else.
      screen-active = '0'.
    endif.
    modify screen.
  endloop.
endform.

initialization.
  txt_b0   = 'Mode'.                    "#EC NOTEXT

  txt_b1   = 'Poll targets'.            "#EC NOTEXT
  t_obj1   = 'W3MI object / File path'. "#EC NOTEXT
  t_obj2   = 'W3MI object / File path'. "#EC NOTEXT
  t_obj3   = 'W3MI object / File path'. "#EC NOTEXT
  t_obj4   = 'W3MI object / File path'. "#EC NOTEXT
  t_obj5   = 'W3MI object / File path'. "#EC NOTEXT

  txt_b2   = 'Poll target (whole package)'.  "#EC NOTEXT

  txt_b3   = 'Start parameters'.        "#EC NOTEXT
  txt_noac = 'Just start polling'.      "#EC NOTEXT
  txt_down = 'Download before polling'. "#EC NOTEXT
  txt_upl  = 'Upload before polling'.   "#EC NOTEXT

  sscrfields-functxt_01 = 'Set dummy'.  "#EC NOTEXT

  get parameter id gc_file_param_name field p_file1.
  get parameter id gc_obj_param_name field p_obj1.

  perform modify_screen.

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

at selection-screen on value-request for p_file4.
  perform f4_file_path changing p_file4.

at selection-screen on value-request for p_obj4.
  perform f4_mime_path changing p_obj4.

at selection-screen on value-request for p_file5.
  perform f4_file_path changing p_file5.

at selection-screen on value-request for p_obj5.
  perform f4_mime_path changing p_obj5.

at selection-screen on p_file1.
  if p_file1 is not initial.
    set parameter id gc_file_param_name field p_file1.
  endif.

at selection-screen on p_obj1.
  if p_obj1 is not initial.
    set parameter id gc_obj_param_name field p_obj1.
  endif.

at selection-screen.
  case sy-ucomm.
    when 'FC01'.          "Set dummy
      p_obj1  = 'ZMIME_POLLER_TEST'.
      p_file1 = 'zmime_poller_test.txt'.
    when others.
  endcase.

**********************************************************************
* MAIN
**********************************************************************

start-of-selection.
  perform main.

**********************************************************************
* FORMS
**********************************************************************

form collect_fbf_targets using pt_targets type zif_w3mime=>tty_poll_targets.

  field-symbols <target> like line of pt_targets.

  clear pt_targets.

  append initial line to pt_targets assigning <target>.
  <target>-w3key-relid = 'MI'. " Fix to mime for the moment
  <target>-w3key-objid = p_obj1.
  <target>-path        = to_upper( p_file1 ).

  append initial line to pt_targets assigning <target>.
  <target>-w3key-relid = 'MI'. " Fix to mime for the moment
  <target>-w3key-objid = p_obj2.
  <target>-path        = to_upper( p_file2 ).

  append initial line to pt_targets assigning <target>.
  <target>-w3key-relid = 'MI'. " Fix to mime for the moment
  <target>-w3key-objid = p_obj3.
  <target>-path        = to_upper( p_file3 ).

  append initial line to pt_targets assigning <target>.
  <target>-w3key-relid = 'MI'. " Fix to mime for the moment
  <target>-w3key-objid = p_obj4.
  <target>-path        = to_upper( p_file4 ).

  append initial line to pt_targets assigning <target>.
  <target>-w3key-relid = 'MI'. " Fix to mime for the moment
  <target>-w3key-objid = p_obj5.
  <target>-path        = to_upper( p_file5 ).

  delete pt_targets where w3key-objid is initial and path is initial.

endform.

form main.

  data lt_targets type zif_w3mime=>tty_poll_targets.
  data lx         type ref to zcx_w3mime_error.

  perform collect_fbf_targets using lt_targets.

  try.
    if p_m_fbf = abap_true.
      lcl_app=>run_file_by_file(
        it_targets  = lt_targets
        do_upload   = p_upl
        do_download = p_down ).
    else.
      lcl_app=>run_by_package(
        iv_package  = p_pkg
        ext_to_dir  = p_e2dir
        do_upload   = p_upl
        do_download = p_down ).
    endif.
  catch zcx_w3mime_error into lx.
    message lx->msg type 'S' display like 'E'.
  endtry.

endform.

form f4_file_path changing c_path type char255.
  c_path = zcl_w3mime_fs=>choose_file_dialog( ).
  if c_path is not initial.
    set parameter id gc_file_param_name field c_path.
  endif.
endform.

form f4_mime_path changing c_path type w3objid.
  c_path = zcl_w3mime_storage=>choose_mime_dialog( ).
  if c_path is not initial.
    set parameter id gc_obj_param_name field c_path.
  endif.
endform.
