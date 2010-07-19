{application, hordad_ldb,
 [{description, "Persistent storage manager"},
 {vsn, "0.1"},
 {modules, [hordad_ldb]},
 {registered, []},
 {applications, [kernel, stdlib, hordad_lcf, hordad_log]}
 ]}.
