class ZCL_W3MIME_FS definition
  public
  final
  create public .

public section.

  types:
    tt_files type standard table of file_info with key filename .

  class-data C_SEP type CHAR1 read-only .

  class-methods CHOOSE_DIR_DIALOG
    returning
      value(RV_PATH) type CHAR255 .
  class-methods CHOOSE_FILE_DIALOG
    returning
      value(RV_PATH) type CHAR255 .
  class-methods READ_FILE
    importing
      !IV_FILENAME type STRING
    exporting
      !ET_DATA type LVC_T_MIME
      !EV_SIZE type I
    raising
      ZCX_W3MIME_ERROR .
  class-methods WRITE_FILE
    importing
      !IV_FILENAME type STRING
      !IV_SIZE type I
    changing
      !CT_DATA type LVC_T_MIME
    raising
      ZCX_W3MIME_ERROR .
  class-methods READ_FILE_X
    importing
      !IV_FILENAME type STRING
    returning
      value(RV_DATA) type XSTRING
    raising
      ZCX_W3MIME_ERROR .
  class-methods WRITE_FILE_X
    importing
      !IV_FILENAME type STRING
      !IV_DATA type XSTRING
    raising
      ZCX_W3MIME_ERROR .
  class-methods PARSE_PATH
    importing
      value(IV_PATH) type STRING
    exporting
      value(EV_DIRECTORY) type STRING
      value(EV_FILENAME) type STRING
      value(EV_EXTENSION) type STRING .
  class-methods RESOLVE_FILENAME
    importing
      value(IV_PATH) type STRING
    exporting
      value(EV_FILENAME) type STRING
      value(EV_DIRECTORY) type STRING
    raising
      ZCX_W3MIME_ERROR .
  class-methods READ_DIR
    importing
      !IV_DIR type STRING
      !IV_FILTER type STRING default '*.*'
    returning
      value(RT_FILES) type TT_FILES
    raising
      ZCX_W3MIME_ERROR .
  class-methods PATH_JOIN
    importing
      !IV_P1 type STRING
      !IV_P2 type STRING
    returning
      value(RT_JOINED) type STRING .
  class-methods PATH_IS_RELATIVE
    importing
      !IV_TO type STRING
      !IV_FROM type STRING
    returning
      value(RV_YES) type ABAP_BOOL .
  class-methods PATH_RELATIVE
    importing
      !IV_FROM type STRING
      !IV_TO type STRING
    returning
      value(RV_PATH) type STRING .
  class-methods CLASS_CONSTRUCTOR .
  class-methods PATH_ENSURE_DIR_TAIL
    importing
      !I_PATH type STRING
    returning
      value(R_PATH) type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_W3MIME_FS IMPLEMENTATION.


method choose_dir_dialog.
  data l_str type string.

  cl_gui_frontend_services=>directory_browse(
    changing
      selected_folder      = l_str
    exceptions
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      others               = 4 ).

  if sy-subrc is initial.
    rv_path = l_str.
  endif.

endmethod.


method choose_file_dialog.
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
    return. " Empty value
  endif.

  read table lt_files assigning <file> index 1.
  if sy-subrc = 0.
    rv_path = <file>-filename.
  endif.

endmethod.


method class_constructor.
  cl_gui_frontend_services=>get_file_separator( changing file_separator = c_sep exceptions others = 4 ).
  if sy-subrc is not initial.
    c_sep = '\'. " Assume windows (eclipse ???)
  endif.
endmethod.


method parse_path.
  data:
        lv_offs type i.

  clear: ev_filename, ev_extension, ev_directory.
  if strlen( iv_path ) = 0.
    return.
  endif.

  find first occurrence of c_sep in reverse( iv_path ) match offset lv_offs.

  if sy-subrc = 0.
    lv_offs      = strlen( iv_path ) - lv_offs.
    ev_directory = substring( val = iv_path len = lv_offs ).
    ev_filename  = substring( val = iv_path off = lv_offs ).
  else.
    ev_filename  = iv_path.
  endif.

  find first occurrence of '.' in reverse( ev_filename ) match offset lv_offs.

  if sy-subrc = 0.
    lv_offs      = strlen( ev_filename ) - lv_offs - 1.
    ev_extension = substring( val = ev_filename off = lv_offs ).
    ev_filename  = substring( val = ev_filename len = lv_offs ).
  endif.

endmethod. "#EC CI_VALPAR


method path_ensure_dir_tail.
  r_path = i_path.
  if r_path is not initial and substring( val = r_path off = strlen( r_path ) - 1 len = 1 ) <> c_sep.
    r_path = r_path && c_sep.
  endif.
endmethod.


method path_is_relative.
  " Does not support .. at the moment
  data l_offs type i.
  l_offs = find( val = iv_to sub = iv_from ).
  rv_yes = boolc( l_offs = 0 ). " Match, starts from start
endmethod.


method path_join.
  " Does not support .. at the moment

  if iv_p1 is not initial.
    rt_joined = iv_p1.
  endif.

  if iv_p2 is not initial.
    if rt_joined is not initial and substring( val = rt_joined off = strlen( rt_joined ) - 1 len = 1 ) <> c_sep.
      rt_joined = rt_joined && c_sep.
    endif.

    rt_joined = rt_joined && iv_p2.
  endif.

endmethod.


method path_relative.
  " Does not support .. at the moment
  if path_is_relative( iv_to = iv_to iv_from = iv_from ) = abap_true.
    rv_path = substring( val = iv_to off = strlen( iv_from ) ).
    if strlen( rv_path ) > 0 and rv_path+0(1) = c_sep.
      shift rv_path left by 1 places.
    endif.
  else.
    rv_path = iv_to. " Hmmm, right just for abs path, think how to do it properly
  endif.
endmethod.


method read_dir.
  data:
        lv_cnt   type i.

  cl_gui_frontend_services=>directory_list_files(
    exporting directory  = iv_dir
              filter     = iv_filter
    changing  file_table = rt_files
              count      = lv_cnt
    exceptions others    = 4 ).

  if sy-subrc is not initial.
    zcx_w3mime_error=>raise( 'Cannot read directory' ). "#EC NOTEXT
  endif.

endmethod.  " read_dir.


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
    zcx_w3mime_error=>raise( 'Cannot read file' ). "#EC NOTEXT
  endif.

endmethod.  " read_file.


method read_file_x.
  data:
        lt_data type lvc_t_mime,
        lv_size type i.

  read_file(
    exporting
      iv_filename = iv_filename
    importing
      et_data = lt_data
      ev_size = lv_size ).

  call function 'SCMS_BINARY_TO_XSTRING'
    exporting
      input_length = lv_size
    importing
      buffer       = rv_data
    tables
      binary_tab   = lt_data.

endmethod.  " read_file_x.


method resolve_filename.
  data:
        lv_sep       type c,
        lv_extension type string.

  parse_path(
    exporting
      iv_path = iv_path
    importing
      ev_directory = ev_directory
      ev_filename  = ev_filename
      ev_extension = lv_extension ).

  ev_filename = ev_filename && lv_extension.
  if strlen( ev_filename ) = 0.
    zcx_w3mime_error=>raise( 'Cannot resolve filename' ). "#EC NOTEXT
  endif.

  if ev_directory is initial.
    cl_gui_frontend_services=>get_sapgui_workdir( changing sapworkdir = ev_directory exceptions others = 4 ).
    if sy-subrc is initial. " hmmm ? eclipse ?
      ev_directory = ev_directory && c_sep.
    else.
      ev_directory = 'c:\tmp\'. " TODO refactor, hack for eclipse unit test
    endif.
  endif.

endmethod.  "#EC CI_VALPAR


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
    zcx_w3mime_error=>raise( 'Cannot write file' ). "#EC NOTEXT
  endif.

endmethod.  " write_file.


method write_file_x.

  data:
        lt_data type lvc_t_mime,
        lv_size type i.

  call function 'SCMS_XSTRING_TO_BINARY'
    exporting
      buffer        = iv_data
    importing
      output_length = lv_size
    tables
      binary_tab    = lt_data.

  write_file(
    exporting
      iv_filename = iv_filename
      iv_size = lv_size
    changing
      ct_data = lt_data ).

endmethod.  " write_file_x.
ENDCLASS.
