interface zif_w3mime
  public .

  constants version type string value 'v1.1.0'.
  constants origin type string value 'https://github.com/sbcgua/abap_w3mi_poller'.
  constants license type string value 'MIT'.

  types:
    begin of ty_w3obj_key,
      relid    type wwwdata-relid,
      objid    type wwwdata-objid,
    end of ty_w3obj_key.

  types:
    begin of ty_poll_target,
      path      type string,
      filename  type string,
      directory type string,
      w3key     type ty_w3obj_key,
      timestamp type timestamp,
    end of ty_poll_target,
    tty_poll_targets type standard table of ty_poll_target with default key.

endinterface.
