report zw3mimepoll.

class lcl_app_file_by_file definition final.
  public section.
    class-methods run
      importing
        it_targets  type zif_w3mime=>tty_poll_targets
        do_download type abap_bool
        do_upload   type abap_bool
      raising
        zcx_w3mime_error.

endclass.

class lcl_app_file_by_file implementation.

  method run.
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
      zcl_w3mime_utils=>upload_targets( it_targets ).
      write: / 'Initial action:' color 7, 'Files uploaded to the system'. "#EC NOTEXT
    elseif do_download = abap_true.
      zcl_w3mime_utils=>download_targets( it_targets ).
      write: / 'Initial action:' color 7, 'Files downloaded to the frontend'. "#EC NOTEXT
    endif.

    data lo_poller type ref to zcl_w3mime_poller_ctl.
    create object lo_poller
      exporting
        it_targets  = it_targets
        iv_interval = 1. " 1 sec
    lo_poller->start( ).

  endmethod.

endclass.

**********************************************************************

class lcl_app_by_package definition final.
  public section.
    class-methods run
      importing
        iv_package  type devclass
        iv_rootdir  type string
        fn_regex    type string
        ext_to_dir  type abap_bool
        do_download type abap_bool
        do_upload   type abap_bool
      raising
        zcx_w3mime_error.
  private section.

    types tty_packages type standard table of devclass with default key.
    types tty_sorted_strings type sorted table of string with unique key table_line.

    class-methods validate_params
      importing
        iv_package  type devclass
        iv_rootdir  type string
      raising
        zcx_w3mime_error.

    class-methods find_all_packages
      importing
        iv_package  type devclass
      returning
        value(rt_packages) type tty_packages
      raising
        zcx_w3mime_error.

    class-methods find_all_targets
      importing
        it_packages type tty_packages
        ext_to_dir type abap_bool
      returning
        value(rt_targets) type zif_w3mime=>tty_poll_targets
      raising
        zcx_w3mime_error.

    class-methods prove_dirs
      importing
        it_dirs type tty_sorted_strings
        iv_rootdir type string
      raising
        zcx_w3mime_error.

    class-methods filter_with_regex
      importing
        fn_regex type string
      changing
        ct_targets type zif_w3mime=>tty_poll_targets
      raising
        zcx_w3mime_error.

    class-methods filter_existing_files_only
      changing
        ct_targets type zif_w3mime=>tty_poll_targets
      raising
        zcx_w3mime_error.

    class-methods writes
      importing
        iv_str type string.

endclass.

class lcl_app_by_package implementation.

  method writes.
    write: / iv_str.
  endmethod.

  method validate_params.

    if iv_package is initial.
      zcx_w3mime_error=>raise( |Package cannot be empty| ).
    endif.
    if iv_rootdir is initial.
      zcx_w3mime_error=>raise( |Target dir cannot be empty| ).
    endif.
    if cl_gui_frontend_services=>directory_exist( iv_rootdir ) = abap_false.
      zcx_w3mime_error=>raise( 'Target dir does not exist' ).
    endif.

    " validate package exists
    data lv_devclass type devclass.
    select single devclass from tdevc into lv_devclass
      where devclass = iv_package.
    if sy-subrc <> 0.
      zcx_w3mime_error=>raise( |Package [{ iv_package }] not found| ).
    endif.

  endmethod.

  method find_all_packages.

    data lt_subpkgs like rt_packages.
    field-symbols <pkg> like line of rt_packages.

    append iv_package to rt_packages.

    loop at rt_packages assigning <pkg>.
      select devclass from tdevc
        into table lt_subpkgs
        where parentcl = <pkg>.
      if sy-subrc = 0.
        append lines of lt_subpkgs to rt_packages.
      endif.
    endloop.

  endmethod.

  method find_all_targets.

    data lt_objects type table of tadir-obj_name.
    data lv_obj_name type tadir-obj_name.
    data ls_meta type zcl_w3mime_storage=>ty_meta.
    field-symbols <pkg> like line of it_packages.
    field-symbols <t> like line of rt_targets.

    loop at it_packages assigning <pkg>.
      select obj_name from tadir
        appending table lt_objects
        where pgmid    = 'R3TR'
        and object     = 'W3MI'
        and devclass   = <pkg>
        and delflag    = abap_false.
    endloop.

    loop at lt_objects into lv_obj_name.
      append initial line to rt_targets assigning <t>.
      ls_meta = zcl_w3mime_storage=>get_object_meta( lv_obj_name ).
      <t>-w3key-relid = 'MI'.
      <t>-w3key-objid = lv_obj_name.
      <t>-filename    = to_lower( ls_meta-filename ).
      if ext_to_dir = abap_true.
        <t>-directory = to_lower( ls_meta-ext ).
        shift <t>-directory left deleting leading '.'.
      endif.
      <t>-path = zcl_w3mime_fs=>path_join(
        iv_p1 = <t>-directory
        iv_p2 = <t>-filename ).
    endloop.

  endmethod.

  method prove_dirs.

    field-symbols <dir> like line of it_dirs.
    data lv_rc type i.
    data lv_path type string.

    loop at it_dirs assigning <dir>.
      lv_path = zcl_w3mime_fs=>path_join(
        iv_p1 = iv_rootdir
        iv_p2 = <dir> ).
      if cl_gui_frontend_services=>directory_exist( lv_path ) = abap_false.
        write: / 'Creating dir', lv_path, '...'.
        cl_gui_frontend_services=>directory_create(
          exporting
            directory = lv_path
          changing
            rc = lv_rc
          exceptions
            others = 4 ).
        if  sy-subrc <> 0 or lv_rc <> 0.
          zcx_w3mime_error=>raise( |Cannot create dir: { lv_path }| ).
        endif.
      endif.
    endloop.

  endmethod.

  method filter_with_regex.

    data lo_regex   type ref to cl_abap_regex.
    data lo_matcher type ref to cl_abap_matcher.
    data lv_idx     type i.
    field-symbols <t> like line of ct_targets.

    try.
      create object lo_regex
        exporting
          pattern     = fn_regex
          ignore_case = abap_true.
    catch cx_sy_regex.
      zcx_w3mime_error=>raise( 'Bad regex' ).
    endtry.

    loop at ct_targets assigning <t>.
      lv_idx = sy-tabix.
      lo_matcher = lo_regex->create_matcher( text = <t>-filename ).
      if lo_matcher->match( ) = abap_false.
        delete ct_targets index lv_idx.
      endif.
    endloop.

  endmethod.

  method run.

    validate_params(
      iv_package = iv_package
      iv_rootdir = iv_rootdir ).

    write: / 'Root package:', iv_package.
    write: / 'Root dir:', iv_rootdir.

    data lt_packages type tty_packages.

    lt_packages = find_all_packages( iv_package ).
    writes( |Found relevant (sub)packages: { lines( lt_packages ) }| ).

    data lt_uniq_paths type tty_sorted_strings.
    data lt_uniq_dirs type tty_sorted_strings.
    data lt_targets type zif_w3mime=>tty_poll_targets.
    data lv_idx type i.
    field-symbols <t> like line of lt_targets.

    lt_targets = find_all_targets(
      it_packages = lt_packages
      ext_to_dir  = ext_to_dir ).

    writes( |Found MIME objects: { lines( lt_targets ) }| ).

    if fn_regex is not initial.
      filter_with_regex(
        exporting
          fn_regex = fn_regex
        changing
          ct_targets = lt_targets ).
      writes( |  after regex: { lines( lt_targets ) }| ).
    endif.

    loop at lt_targets assigning <t>.
      lv_idx = sy-tabix.
      write: / ` `, <t>-w3key-objid, <t>-filename.
      if <t>-filename is initial.
        write: '!EMPTY FILENAME' color 6 inverse on.
        delete lt_targets index lv_idx.
        continue.
      endif.
      if ext_to_dir = abap_true and <t>-directory is initial.
        write: '!EMPTY EXT' color 6 inverse on.
        delete lt_targets index lv_idx.
        continue.
      endif.
      insert <t>-path into table lt_uniq_paths.
      if sy-subrc <> 0.
        write: '!DUPLICATE PATH' color 6 inverse on.
        clear <t>-w3key.
        continue.
      endif.
      if <t>-directory is not initial.
        insert <t>-directory into table lt_uniq_dirs.
      endif.
      <t>-path = zcl_w3mime_fs=>path_join(
        iv_p1 = iv_rootdir
        iv_p2 = <t>-path ).
    endloop.

    uline.
    writes( |Found proper MIME objects: { lines( lt_targets ) }| ).

    if ext_to_dir = abap_true.
      prove_dirs(
        iv_rootdir = iv_rootdir
        it_dirs = lt_uniq_dirs ).
    endif.

    if do_download = abap_true.
      zcl_w3mime_utils=>download_targets( lt_targets ).
      write: / 'Initial action:' color 7, 'Files downloaded to the frontend'. "#EC NOTEXT
    else. " No action or upload
      filter_existing_files_only( changing ct_targets = lt_targets ).
      if do_upload = abap_true.
        zcl_w3mime_utils=>upload_targets( lt_targets ).
        write: / 'Initial action:' color 7, 'Files uploaded to the system'. "#EC NOTEXT
      endif.
    endif.

    uline.
    if lines( lt_targets ) = 0.
      write: / 'No relevant targets remain. Check your params. End of processing.'.
      return.
    endif.

    loop at lt_targets assigning <t>.
      clear: <t>-directory, <t>-filename. " cleanup temporary data
    endloop.

    data lo_poller type ref to zcl_w3mime_poller_ctl.
    create object lo_poller
      exporting
        iv_optimize_dir_reads = abap_true
        it_targets  = lt_targets
        iv_interval = 1. " 1 sec
    lo_poller->start( ).

  endmethod.

  method filter_existing_files_only.

    data lv_idx type i.
    data lv_path type string.
    field-symbols <t> like line of ct_targets.

    loop at ct_targets assigning <t>.
      lv_idx = sy-tabix.
      if cl_gui_frontend_services=>file_exist( <t>-path ) = abap_false.
        lv_path = zcl_w3mime_fs=>path_join(
          iv_p1 = <t>-directory
          iv_p2 = <t>-filename ).
        write: / '[WARN]' color 3, 'File', lv_path, 'was not found. Excluding from further processing'.
        delete ct_targets index lv_idx.
      endif.
    endloop.

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
  parameters p_root type char255 modif id pkg.
  parameters p_regex type string lower case modif id pkg.
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

at selection-screen on value-request for p_root.
  perform f4_dir_path changing p_root.

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
      lcl_app_file_by_file=>run(
        it_targets  = lt_targets
        do_upload   = p_upl
        do_download = p_down ).
    else.
      lcl_app_by_package=>run(
        iv_package  = p_pkg
        iv_rootdir  = |{ p_root }|
        fn_regex    = |{ p_regex }|
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

form f4_dir_path changing c_path type char255.
  c_path = zcl_w3mime_fs=>choose_dir_dialog( ).
endform.

form f4_mime_path changing c_path type w3objid.
  c_path = zcl_w3mime_storage=>choose_mime_dialog( ).
  if c_path is not initial.
    set parameter id gc_obj_param_name field c_path.
  endif.
endform.
