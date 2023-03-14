interface zif_w3mime
  public .

  constants version type string value 'v1.0.0'.
  constants origin type string value 'https://github.com/sbcgua/abap_w3mi_poller'.
  constants license type string value 'MIT'.

  types:
    begin of ty_w3obj_key,
      relid    type wwwdata-relid,
      objid    type wwwdata-objid,
    end of ty_w3obj_key.

endinterface.
