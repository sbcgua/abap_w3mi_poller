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
      zcx_w3mime_error=>raise( 'File does not exist' ).
    endif.

    if abap_false = zcl_w3mime_storage=>check_obj_exists( iv_type = is_w3key-relid iv_key = is_w3key-objid ).
      zcx_w3mime_error=>raise( 'MIME object does not exist' ).
    endif.

  endmethod.  " validate_params.

endclass. "lcl_validator

*&---------------------------------------------------------------------*
*& lcl_poller
*&---------------------------------------------------------------------*

class lcl_poller definition final.
  public section.

    types:
      begin of ty_file_attr,
        mdate type dats,
        mtime type tims,
        stamp type timestamp,
      end of ty_file_attr,
      begin of ty_poll_target,
        w3key     type ty_w3obj_key,
        filename  type string,
        directory type string,
        timestamp type timestamp,
      end of ty_poll_target,
      tt_poll_targets type standard table of ty_poll_target with default key.

    methods constructor
      importing
        iv_interval type i
        it_targets  type tt_poll_targets
      raising zcx_w3mime_error.

    methods start.
    methods handle_timer for event finished of cl_gui_timer.

    class-methods format_dt
      importing is_attr       type ty_file_attr
      returning value(rv_str) type string.

  private section.

    data: mo_timer   type ref to cl_gui_timer,
          mt_targets type tt_poll_targets.

    methods read_attributes
      importing
        iv_dir  type string
        iv_file type string
      returning value(rv_attr) type ty_file_attr
      raising zcx_w3mime_error.

endclass.   "lcl_poller

class lcl_poller implementation.

  method format_dt.
    rv_str = |{ is_attr-mdate+0(4) }-{ is_attr-mdate+4(2) }-{ is_attr-mdate+6(2) } |
          && |{ is_attr-mtime+0(2) }:{ is_attr-mtime+2(2) }:{ is_attr-mtime+4(2) }|.
  endmethod.  " format_dt.

  method constructor.

    data:
          ls_attr type ty_file_attr,
          lv_idx  type char10,
          lv_msg  type string.

    field-symbols: <target> like line of mt_targets.

    if lines( it_targets ) = 0.
      zcx_w3mime_error=>raise( 'Specify poll targets' ).
    endif.
    mt_targets = it_targets.

    write / 'Targets:'.

    loop at mt_targets assigning <target>.
      lv_idx = sy-tabix.

      lcl_validator=>validate_params(
        iv_filename = <target>-directory && <target>-filename
        is_w3key    = <target>-w3key ).

      ls_attr = read_attributes(
        iv_dir  = <target>-directory
        iv_file = <target>-filename ).

      <target>-timestamp = ls_attr-stamp.

      lv_msg = |  ({ condense( lv_idx ) }): {
               <target>-w3key-objid } [{ <target>-w3key-relid
               }] <=> { <target>-directory && <target>-filename
               } [{ format_dt( ls_attr ) }]|.
      write / lv_msg.

    endloop.

    write / 'Staring polling ...'.
    uline.
    create object mo_timer.
    set handler me->handle_timer for mo_timer.
    mo_timer->interval = iv_interval.

  endmethod.  " constructor.

  method read_attributes.

    data:
          lt_files type standard table of file_info.

    field-symbols <file> like line of lt_files.

    lt_files = zcl_w3mime_fs=>read_dir(
      iv_dir    = iv_dir
      iv_filter = iv_file ).

    read table lt_files assigning <file> index 1.
    if sy-subrc > 0.
      zcx_w3mime_error=>raise( 'Cannot get file attributes' ).
    endif.

    rv_attr-mdate = <file>-writedate.
    rv_attr-mtime = <file>-writetime.

    cl_abap_tstmp=>systemtstmp_syst2utc(
      exporting
        syst_date = rv_attr-mdate
        syst_time = rv_attr-mtime
      importing
        utc_tstmp = rv_attr-stamp ).

  endmethod.  " read_attributes.

  method start.

    mo_timer->run( ).

  endmethod.  "start.

  method handle_timer.

    data: ls_attr type ty_file_attr,
          lv_idx  type char10,
          lv_msg  type string,
          lx type ref to zcx_w3mime_error.

    field-symbols: <target> like line of mt_targets.

    loop at mt_targets assigning <target>.
      lv_idx = sy-tabix.

      try.
        ls_attr = read_attributes(
          iv_dir  = <target>-directory
          iv_file = <target>-filename ).
      catch zcx_w3mime_error into lx.
        message lx->msg type 'E'.
      endtry.

      if <target>-timestamp < ls_attr-stamp.
        lv_msg = |File changed: { <target>-filename } ({
                  condense( lv_idx ) }) [{ format_dt( ls_attr ) }]|.
        write / lv_msg.

        <target>-timestamp = ls_attr-stamp.

        try.
          zcl_w3mime_utils=>upload(
            iv_filename = <target>-directory && <target>-filename
            iv_type = <target>-w3key-relid
            iv_key  = <target>-w3key-objid ).
        catch zcx_w3mime_error into lx.
          message lx->msg type 'E'.
        endtry.

      endif.

    endloop.

    mo_timer->run( ).

  endmethod.  "handle_timer

endclass. " lcl_poller

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
        go_poller  type ref to lcl_poller,
        gt_targets type lcl_poller=>tt_poll_targets,
        gv_msg     type string,
        gx         type ref to zcx_w3mime_error.

  field-symbols <g_target> like line of gt_targets.

  append initial line to gt_targets assigning <g_target>.
  <g_target>-w3key-relid = 'MI'. " Fix to mime for the moment
  <g_target>-w3key-objid = p_obj1.
  <g_target>-filename    = p_file1.

  append initial line to gt_targets assigning <g_target>.
  <g_target>-w3key-relid = 'MI'. " Fix to mime for the moment
  <g_target>-w3key-objid = p_obj2.
  <g_target>-filename    = p_file2.

  append initial line to gt_targets assigning <g_target>.
  <g_target>-w3key-relid = 'MI'. " Fix to mime for the moment
  <g_target>-w3key-objid = p_obj3.
  <g_target>-filename    = p_file3.

  try.

    loop at gt_targets assigning <g_target>.
      if <g_target>-w3key-objid is initial and <g_target>-filename is initial.
        delete gt_targets index sy-tabix.
        continue.
      endif.

      zcl_w3mime_fs=>resolve_filename(
        exporting iv_path      = <g_target>-filename
        importing ev_filename  = <g_target>-filename
                  ev_directory = <g_target>-directory ).

      lcl_validator=>validate_params(
        iv_filename        = <g_target>-directory && <g_target>-filename
        is_w3key           = <g_target>-w3key
        iv_skip_file_check = p_down ).
    endloop.

    if lines( gt_targets ) = 0.
      message 'Please specify at least one target pair' type 'E'.
    endif.

    if p_upl is not initial.
      loop at gt_targets assigning <g_target>.
        zcl_w3mime_utils=>upload(
          iv_filename = <g_target>-directory && <g_target>-filename
          iv_type = <g_target>-w3key-relid
          iv_key  = <g_target>-w3key-objid ).
      endloop.
      write: / 'Initial action:' color 7, 'Files uploaded to the system'.
    elseif p_down is not initial.
      loop at gt_targets assigning <g_target>.
        zcl_w3mime_utils=>download(
          iv_filename = <g_target>-directory && <g_target>-filename
          iv_type = <g_target>-w3key-relid
          iv_key  = <g_target>-w3key-objid ).
      endloop.
      write: / 'Initial action:' color 7, 'Files downloaded to the frontend'.
    endif.

    create object go_poller
      exporting
        it_targets  = gt_targets
        iv_interval = 1. " 1 sec

    go_poller->start( ).

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
