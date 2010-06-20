{application, hordad_gts,
 [{description, "Geo Targeting system"},
 {vsn, "0.1"},
 {modules, [hordad_gts_app,
            hordad_gts_sup,
            hordad_gts_mysql,
            hordad_gts,
            hordad_gts_lib
           ]},
 {registered, [hordad_gts, hordad_gts_mysql]},
 {applications, [kernel, stdlib, hordad_lcf, hordad_log]},
 {mod, {hordad_gts_app, []}}
 ]}.
