class zcl_w3mime_fs definition
  public
  final
  create public .

  public section.

    types:
      tt_files type standard table of file_info with key filename .
    types:
      ty_char255 type c length 255.

    class-data c_sep type c length 1 read-only .

    class-methods choose_dir_dialog
      returning
        value(rv_path) type ty_char255 .
    class-methods choose_file_dialog
      returning
        value(rv_path) type ty_char255 .
    class-methods read_file
      importing
        !iv_filename type string
      exporting
        !et_data     type lvc_t_mime
        !ev_size     type i
      raising
        zcx_w3mime_error .
    class-methods write_file
      importing
        !iv_filename type string
        !iv_size     type i
      changing
        !ct_data     type lvc_t_mime
      raising
        zcx_w3mime_error .
    class-methods read_file_x
      importing
        !iv_filename   type string
      returning
        value(rv_data) type xstring
      raising
        zcx_w3mime_error .
    class-methods write_file_x
      importing
        !iv_filename type string
        !iv_data     type xstring
      raising
        zcx_w3mime_error .
    class-methods parse_path
      importing
        value(iv_path)      type string
      exporting
        value(ev_directory) type string
        value(ev_filename)  type string
        value(ev_extension) type string .
    class-methods resolve_filename
      importing
        value(iv_path)      type string
      exporting
        value(ev_filename)  type string
        value(ev_directory) type string
      raising
        zcx_w3mime_error .
    class-methods read_dir
      importing
        !iv_dir         type string
        !iv_filter      type string default '*.*'
      returning
        value(rt_files) type tt_files
      raising
        zcx_w3mime_error .
    class-methods path_join
      importing
        !iv_p1           type string
        !iv_p2           type string
      returning
        value(rt_joined) type string .
    class-methods path_is_relative
      importing
        !iv_to        type string
        !iv_from      type string
      returning
        value(rv_yes) type abap_bool .
    class-methods path_relative
      importing
        !iv_from       type string
        !iv_to         type string
      returning
        value(rv_path) type string .
    class-methods class_constructor .
    class-methods path_ensure_dir_tail
      importing
        !i_path       type string
      returning
        value(r_path) type string .

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

    data lt_files type filetable.
    data lv_rc    type i.
    data lv_uact  type i.

    field-symbols <file> like line of lt_files.

    cl_gui_frontend_services=>file_open_dialog(
      changing
        file_table  = lt_files
        rc          = lv_rc
        user_action = lv_uact
      exceptions others = 4 ).

    if sy-subrc > 0 or lv_uact <> cl_gui_frontend_services=>action_ok.
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

    data lv_offs type i.

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

  endmethod.                                             "#EC CI_VALPAR


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

    data lv_cnt type i.

    cl_gui_frontend_services=>directory_list_files(
      exporting
        directory  = iv_dir
        filter     = iv_filter
      changing
        file_table = rt_files
        count      = lv_cnt
      exceptions
        others    = 4 ).

    if sy-subrc is not initial.
      zcx_w3mime_error=>raise( 'Cannot read directory' ).   "#EC NOTEXT
    endif.

  endmethod.


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
      zcx_w3mime_error=>raise( 'Cannot read file' ).        "#EC NOTEXT
    endif.

  endmethod.


  method read_file_x.

    data lt_data type lvc_t_mime.
    data lv_size type i.

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

  endmethod.


  method resolve_filename.

    data lv_sep       type c.
    data lv_extension type string.

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
      cl_gui_cfw=>flush( ).
      if sy-subrc is initial. " hmmm ? eclipse ?
        ev_directory = ev_directory && c_sep.
      else.
        ev_directory = 'c:\tmp\'. " TODO refactor, hack for eclipse unit test
      endif.
    endif.

  endmethod.                                             "#EC CI_VALPAR


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
      zcx_w3mime_error=>raise( 'Cannot write file' ).       "#EC NOTEXT
    endif.

  endmethod.


  method write_file_x.

    data lt_data type lvc_t_mime.
    data lv_size type i.

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

  endmethod.
ENDCLASS.
