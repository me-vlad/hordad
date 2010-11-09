{application, hordad_dht,
 [{description, "Hordad distributed database"},
 {vsn, "0.1"},
 {modules, [hordad_ddb_app, hordad_ddb_sup]},
 {registered, []},
 {applications, [kernel, stdlib, crypto, hordad_lcf, hordad_log]},
 {mod, {hordad_ddb_app, []}}
 ]}.
