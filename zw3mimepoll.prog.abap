*&---------------------------------------------------------------------*
*& Report  ZW3MIMEPOLL
*&---------------------------------------------------------------------*

report zw3mimepoll.
tables sscrfields.

*&---------------------------------------------------------------------*
*& lcx_error
*&---------------------------------------------------------------------*

class lcx_error definition final inheriting from cx_static_check.
  public section.
    data mv_message type string read-only.
    methods constructor importing iv_message type string.
    class-methods raise importing iv_message type string raising lcx_error.
endclass. " lcx_error

class lcx_error implementation.

  method constructor.
    super->constructor( ).
    mv_message = iv_message.
  endmethod.  " constructor.

  method raise.
    raise exception type lcx_error exporting iv_message = iv_message.
  endmethod.  " raise.

endclass. " lcx_error

*&---------------------------------------------------------------------*
*& lcl_resource_updater
*&---------------------------------------------------------------------*
class lcl_resource_updater definition final.
  public section.

    types: ty_w3tab type standard table of w3mime.

    class-methods upload_file
      importing iv_filename type string
                is_object   type wwwdatatab
      raising lcx_error.

    class-methods download_file
      importing iv_filename type string
                is_object   type wwwdatatab
      raising lcx_error.

    class-methods check_obj_exists
      importing is_object     type wwwdatatab
      returning value(rv_yes) type abap_bool.

  private section.

    class-methods read_file
      importing iv_filename    type string
      exporting et_data        type ty_w3tab
                ev_size        type i
      raising lcx_error.

    class-methods write_file
      importing iv_filename    type string
                iv_size        type i
      changing  ct_data        type ty_w3tab
      raising lcx_error.

    class-methods update_object
      importing is_object   type wwwdatatab
                it_data     type ty_w3tab
                iv_size     type i
      raising lcx_error.

    class-methods get_object
      importing is_object   type wwwdatatab
      exporting et_data     type ty_w3tab
                ev_size     type i
      raising lcx_error.

    class-methods get_object_info
      changing cs_object   type wwwdatatab
      raising lcx_error.

endclass. "lcl_resource_updater

class lcl_resource_updater implementation.

  method upload_file.

    data: lt_data   type ty_w3tab,
          ls_object like is_object,
          lv_size   type i.

    if abap_false = check_obj_exists( is_object ).
      lcx_error=>raise( 'MIME object does not exist' ).
    endif.

    read_file( exporting iv_filename = iv_filename
               importing et_data     = lt_data
                         ev_size     = lv_size ).

    ls_object = is_object.
    get_object_info( changing cs_object = ls_object ).
    ls_object-chname = sy-uname.
    ls_object-tdate  = sy-datum.
    ls_object-ttime  = sy-uzeit.

    update_object( is_object = ls_object
                   it_data   = lt_data
                   iv_size   = lv_size ).

  endmethod.  " upload_file.

  method download_file.

    data: lt_data type ty_w3tab,
          lv_size type i.

    if abap_false = check_obj_exists( is_object ).
      lcx_error=>raise( 'MIME object does not exist' ).
    endif.

    get_object( exporting is_object = is_object
                importing ev_size   = lv_size
                          et_data   = lt_data ).

    write_file( exporting iv_filename = iv_filename
                          iv_size     = lv_size
                changing  ct_data     = lt_data ).

  endmethod.  " download_file.

  method read_file.

    clear: et_data, ev_size.

    cl_gui_frontend_services=>gui_upload(
      exporting
        filename   = iv_filename
        filetype   = 'BIN'
      importing
        filelength = ev_size
      changing
        data_tab   = et_data
      exceptions
        others     = 1 ).

    if sy-subrc > 0.
      lcx_error=>raise( 'Cannot read file' ).
    endif.

  endmethod.  " read_file.

  method write_file.

    cl_gui_frontend_services=>gui_download(
      exporting
        filename     = iv_filename
        filetype     = 'BIN'
        bin_filesize = iv_size
      changing
        data_tab   = ct_data
      exceptions
        others     = 1 ).

    if sy-subrc > 0.
      lcx_error=>raise( 'Cannot read file' ).
    endif.

  endmethod.  " write_file.

  method update_object.

    data ls_param type wwwparams.

    ls_param-relid = is_object-relid.
    ls_param-objid = is_object-objid.
    ls_param-name  = 'filesize'.
    ls_param-value = iv_size.
    condense ls_param-value.

    call function 'WWWPARAMS_MODIFY_SINGLE'
      exporting
        params = ls_param
      exceptions
        other = 1.

    if sy-subrc > 0.
      lcx_error=>raise( 'Cannot upload W3xx data' ).
    endif.

    call function 'WWWDATA_EXPORT'
      exporting
        key               = is_object
      tables
        mime              = it_data
      exceptions
        wrong_object_type = 1
        export_error      = 2.

    if sy-subrc > 0.
      lcx_error=>raise( 'Cannot upload W3xx data' ).
    endif.

  endmethod.  " update_object.

  method get_object.

    data lv_value type w3_qvalue.

    clear: et_data, ev_size.

    call function 'WWWPARAMS_READ'
      exporting
        relid = is_object-relid
        objid = is_object-objid
        name  = 'filesize'
      importing
        value = lv_value
      exceptions
        others = 1.

    if sy-subrc > 0.
      lcx_error=>raise( 'Cannot read W3xx filesize parameter' ).
    endif.

    ev_size = lv_value.

    call function 'WWWDATA_IMPORT'
      exporting
        key               = is_object
      tables
        mime              = et_data
      exceptions
        wrong_object_type = 1
        export_error      = 2.

    if sy-subrc > 0.
      lcx_error=>raise( 'Cannot upload W3xx data' ).
    endif.

  endmethod.  " get_object.

  method check_obj_exists.

    data dummy like is_object-relid.

    select single relid into dummy
      from wwwdata
      where relid = is_object-relid
      and   objid = is_object-objid
      and   srtf2 = 0.

    rv_yes = boolc( sy-subrc = 0 ).

  endmethod.  " check_obj_exists.

  method get_object_info.

    select single * into corresponding fields of cs_object
      from wwwdata
      where relid = cs_object-relid
      and   objid = cs_object-objid
      and   srtf2 = 0.

    if sy-subrc > 0.
      lcx_error=>raise( 'Cannot read W3xx info' ).
    endif.

  endmethod.  " get_object_info.

endclass. "lcl_resource_updater

*&---------------------------------------------------------------------*
*& lcl_validator
*&---------------------------------------------------------------------*
class lcl_validator definition final.
  public section.
    class-methods validate_params
      importing
        iv_filename        type string
        is_obj             type wwwdatatab
        iv_skip_file_check type abap_bool default abap_false
      raising lcx_error.

endclass. "lcl_validator

class lcl_validator implementation.

  method validate_params.

    if iv_skip_file_check is initial.
      if abap_false = cl_gui_frontend_services=>file_exist( iv_filename ).
        lcx_error=>raise( 'File does not exist' ).
      endif.
    endif.

    if abap_false = lcl_resource_updater=>check_obj_exists( is_obj ).
      lcx_error=>raise( 'MIME object does not exist' ).
    endif.

  endmethod.  " validate_params.

endclass. "lcl_validator



*&---------------------------------------------------------------------*
*& lcl_poller
*&---------------------------------------------------------------------*

class lcl_poller definition final.
  public section.

    types: begin of ty_file_attr,
             mdate type dats,
             mtime type tims,
             stamp type timestamp,
           end of ty_file_attr.

    methods constructor
      importing iv_interval type i
                iv_filename type string
                is_obj      type wwwdatatab
      raising lcx_error.

    methods start.
    methods handle_finished for event finished of cl_gui_timer.

  private section.
    data: mo_timer     type ref to cl_gui_timer,
          mv_filename  type string,
          mv_directory type string,
          ms_obj       type wwwdatatab,
          mv_timestamp type timestamp.

    methods read_attributes
      returning value(rv_attr) type ty_file_attr
      raising lcx_error.

endclass.   "lcl_poller

class lcl_poller implementation.

  method constructor.

    data:
          lv_offs  type i,
          lv_sep   type c.

    lcl_validator=>validate_params( iv_filename = iv_filename is_obj = is_obj ).

    ms_obj = is_obj.

    cl_gui_frontend_services=>get_file_separator( changing file_separator = lv_sep ).
    find first occurrence of lv_sep in reverse( iv_filename ) match offset lv_offs.

    if sy-subrc = 0.
      lv_offs      = strlen( iv_filename ) - lv_offs.
      mv_directory = substring( val = iv_filename len = lv_offs ).
      mv_filename  = substring( val = iv_filename off = lv_offs ).
    else.
      cl_gui_frontend_services=>get_sapgui_workdir( changing sapworkdir = mv_directory ).
      mv_directory = mv_directory && lv_sep.
      mv_filename  = iv_filename.
    endif.


    data lv_attr type ty_file_attr.
    lv_attr      = read_attributes( ).
    mv_timestamp = lv_attr-stamp.

    data lv_message type string.
    lv_message = |Polling started: { lv_attr-mdate } { lv_attr-mtime }|.
    write / lv_message.

    create object mo_timer.
    set handler me->handle_finished for mo_timer.
    mo_timer->interval = iv_interval.

  endmethod.  " constructor.

  method read_attributes.

    data:
          lv_cnt   type i,
          lt_files type standard table of file_info,
          ls_file  like line of lt_files.

    cl_gui_frontend_services=>directory_list_files(
      exporting directory  = mv_directory
                filter     = mv_filename
      changing  file_table = lt_files
                count      = lv_cnt
      exceptions others    = 4 ).
    if sy-subrc > 0.
      lcx_error=>raise( 'Cannot get file attributes' ).
    endif.

    read table lt_files into ls_file index 1.
    if sy-subrc > 0.
      lcx_error=>raise( 'Cannot get file attributes' ).
    endif.

    rv_attr-mdate = ls_file-writedate.
    rv_attr-mtime = ls_file-writetime.

    cl_abap_tstmp=>systemtstmp_syst2utc(
      exporting syst_date = rv_attr-mdate
                syst_time = rv_attr-mtime
      importing utc_tstmp = rv_attr-stamp ).

  endmethod.  " read_attributes.

  method start.

    mo_timer->run( ).

  endmethod.  "start.

  method handle_finished.

    data: lv_attr type ty_file_attr,
          lx type ref to lcx_error.

    try.
      lv_attr = read_attributes( ).
    catch lcx_error into lx.
      message lx->mv_message type 'E'.
    endtry.

    if mv_timestamp < lv_attr-stamp.
      data lv_message type string.
      lv_message = |File changed: { lv_attr-mdate } { lv_attr-mtime }|.
      write / lv_message.

      mv_timestamp = lv_attr-stamp.

      try.
        lcl_resource_updater=>upload_file(
          iv_filename = mv_directory && mv_filename
          is_object   = ms_obj ).
      catch lcx_error into lx.
        message lx->mv_message type 'E'.
      endtry.

    endif.

    mo_timer->run( ).

  endmethod.

endclass. " lcl_poller

**********************************************************************

constants:
  GC_FILE_PARAM_NAME TYPE CHAR20 VALUE 'ZW3MIMEPOLL_FILE',
  GC_OBJ_PARAM_NAME  TYPE CHAR20 VALUE 'ZW3MIMEPOLL_OBJ'.

selection-screen begin of block b1 with frame title txt_b1.

selection-screen begin of line.
selection-screen comment (24) txt_file for field p_file.
parameter p_file type char255.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment (24) txt_obj for field p_obj.
parameter p_obj type w3objid.
selection-screen end of line.

selection-screen end of block b1.

selection-screen begin of block b2 with frame title txt_b2.

selection-screen begin of line.
selection-screen comment (24) txt_noac for field p_noact.
parameter p_noact type char1 radiobutton group r1 default 'X'.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment (24) txt_down for field p_down.
parameter p_down type char1 radiobutton group r1.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment (24) txt_upl for field p_upl.
parameter p_upl type char1 radiobutton group r1.
selection-screen end of line.

selection-screen end of block b2.

selection-screen function key 1.

initialization.
  txt_b1   = 'Poll target'.             "#EC NOTEXT
  txt_file = 'Path to file'.            "#EC NOTEXT
  txt_obj  = 'W3MI object'.             "#EC NOTEXT

  txt_b2   = 'Start parameters'.        "#EC NOTEXT
  txt_noac = 'Just start polling'.      "#EC NOTEXT
  txt_down = 'Download before polling'. "#EC NOTEXT
  txt_upl  = 'Upload before polling'.   "#EC NOTEXT

  sscrfields-functxt_01 = 'Set dummy'.  "#EC NOTEXT

  get parameter id GC_FILE_PARAM_NAME field p_file.
  get parameter id GC_OBJ_PARAM_NAME field p_obj.

at selection-screen on value-request for p_file.
  perform f4_file_path changing p_file.

at selection-screen on value-request for p_obj.
  perform f4_mime_path changing p_obj.

at selection-screen on p_file.
  if p_file is not initial.
    set parameter id GC_FILE_PARAM_NAME field p_file.
  endif.

at selection-screen on p_obj.
  if p_obj is not initial.
    set parameter id GC_OBJ_PARAM_NAME field p_obj.
  endif.

at selection-screen.
  case sy-ucomm.
    when 'FC01'.          "Set dummy
      p_obj  = 'ZMIME_POLLER_TEST'.
      p_file = 'zmime_poller_test.txt'.
  endcase.

**********************************************************************
* MAIN
**********************************************************************

start-of-selection.

  data:
        go_poller type ref to lcl_poller,
        gs_obj    type wwwdatatab,
        gv_file   type string,
        gv_msg    type string,
        gx        type ref to lcx_error.

  gs_obj-relid = 'MI'. " Fix to mime for the moment
  gs_obj-objid = p_obj.
  gv_file      = p_file.

  try.

    lcl_validator=>validate_params( iv_filename        = gv_file
                                    is_obj             = gs_obj
                                    iv_skip_file_check = p_down ).

    gv_msg = |Run parameters: object = { gs_obj-relid } { gs_obj-objid }, filename = { gv_file }|.
    write: / gv_msg.

    if p_upl is not initial.
      lcl_resource_updater=>upload_file(
        iv_filename = gv_file
        is_object   = gs_obj ).
      write: / 'Initial action: File uploaded to the system'.
    elseif p_down is not initial.
      lcl_resource_updater=>download_file(
        iv_filename = gv_file
        is_object   = gs_obj ).
      write: / 'Initial action: File downloaded to the frontend'.
    endif.

    create object go_poller
      exporting
        iv_filename = gv_file
        is_obj      = gs_obj
        iv_interval = 1. " 1 sec

    go_poller->start( ).

  catch lcx_error into gx.
    message gx->mv_message type 'E'.
  endtry.

**********************************************************************
* FORMS
**********************************************************************

form f4_file_path changing c_path.

  data l_path type localfile.

  call function 'F4_FILENAME'
    importing
      file_name = l_path.

  c_path = l_path.
  set parameter id GC_FILE_PARAM_NAME field l_path.

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