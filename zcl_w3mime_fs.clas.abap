class ZCL_W3MIME_FS definition
  public
  final
  create public .

public section.

  types:
    tt_files type standard table of file_info with key filename .

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
  class-methods PARSE_FILENAME
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
protected section.
private section.
ENDCLASS.



CLASS ZCL_W3MIME_FS IMPLEMENTATION.


method parse_filename.
  data:
        lv_offs type i,
        lv_sep  type c.

  clear: ev_filename, ev_extension, ev_directory.
  if strlen( iv_path ) = 0.
    return.
  endif.

  cl_gui_frontend_services=>get_file_separator( changing file_separator = lv_sep ).

  find first occurrence of lv_sep in reverse( iv_path ) match offset lv_offs.

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

endmethod.  " parse_filename.


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
    zcx_w3mime_error=>raise( 'Cannot read directory' ).
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
    zcx_w3mime_error=>raise( 'Cannot read file' ).
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

  parse_filename(
    exporting
      iv_path = iv_path
    importing
      ev_directory = ev_directory
      ev_filename  = ev_filename
      ev_extension = lv_extension ).

  ev_filename = ev_filename && lv_extension.
  if strlen( ev_filename ) = 0.
    zcx_w3mime_error=>raise( 'Cannot resolve filename' ).
  endif.

  if ev_directory is initial.
    cl_gui_frontend_services=>get_file_separator( changing file_separator = lv_sep ).
    cl_gui_frontend_services=>get_sapgui_workdir( changing sapworkdir = ev_directory ).
    ev_directory = ev_directory && lv_sep.
  endif.

endmethod.  " resolve_filename.


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
    zcx_w3mime_error=>raise( 'Cannot read file' ).
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
