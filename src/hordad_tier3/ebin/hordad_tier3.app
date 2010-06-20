{application, hordad_tier3,
 [{description, "Worker applications supervisor"},
 {vsn, "0.1"},
 {modules, [hordad_tier3_app, hordad_tier3_sup]},
 {registered, []},
 {applications, [kernel, stdlib, hordad_lcf, hordad_log]},
 {mod, {hordad_tier3_app, []}}
 ]}.
