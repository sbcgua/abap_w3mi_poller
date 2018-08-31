class ZCL_W3MIME_ZIP_WRITER definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IO_ZIP type ref to CL_ABAP_ZIP optional .
  methods ADD
    importing
      !IV_FILENAME type STRING
      !IV_DATA type STRING .
  methods ADDX
    importing
      !IV_FILENAME type STRING
      !IV_XDATA type XSTRING .
  methods GET_BLOB
    returning
      value(RV_BLOB) type XSTRING .
protected section.
private section.
  data mo_zip  type ref to cl_abap_zip.
  data mo_conv type ref to cl_abap_conv_out_ce.

ENDCLASS.



CLASS ZCL_W3MIME_ZIP_WRITER IMPLEMENTATION.


method add.
  data lv_xdata type xstring.
  mo_conv->convert(
    exporting data = iv_data
    importing buffer = lv_xdata ).

  addx(
    iv_filename = iv_filename
    iv_xdata    = lv_xdata ).
endmethod.  " add.


method addx.
  mo_zip->delete(
    exporting
      name = iv_filename
    exceptions others = 1 ). " ignore exceptions

  mo_zip->add( name = iv_filename content = iv_xdata ).
endmethod.  " addx.


method constructor.
  if io_zip is bound.
    mo_zip = io_zip.
  else.
    create object mo_zip.
  endif.
  mo_conv = cl_abap_conv_out_ce=>create( encoding = '4110' ). " UTF8
endmethod.  " constructor.


method get_blob.
  rv_blob = mo_zip->save( ).
endmethod.  " get_blob
ENDCLASS.
