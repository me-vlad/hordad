{application, hordad_ldb,
 [{description, "Persistent storage manager"},
 {vsn, "0.1"},
 {modules, [hordad_ldb_app, hordad_ldb_sup]},
 {registered, []},
 {applications, [kernel, stdlib, hordad_lcf, hordad_log]},
 {mod, {hordad_ldb_app, []}}
 ]}.
