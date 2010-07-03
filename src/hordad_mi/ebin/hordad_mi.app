{application, hordad_mi,
 [{description, "Hordad management interface"},
 {vsn, "0.1"},
 {modules, [hordad_mi_app, hordad_mi_sup]},
 {registered, []},
 {applications, [kernel, stdlib, hordad_lcf, hordad_log]},
 {mod, {hordad_mi_app, []}}
 ]}.
