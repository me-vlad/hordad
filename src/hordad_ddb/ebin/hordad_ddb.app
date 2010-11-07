{application, hordad_ddb,
 [{description, "Hordad distributed database"},
 {vsn, "0.1"},
 {modules, [hordad_ddb_app, hordad_ddb_sup]},
 {registered, []},
 {applications, [kernel, stdlib, hordad_lcf, hordad_log]},
 {mod, {hordad_ddb_app, []}}
 ]}.
