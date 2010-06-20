{application, hordad_tier2,
 [{description, "System applications supervisor"},
 {vsn, "0.1"},
 {modules, [hordad_tier2_app, hordad_tier2_sup]},
 {registered, []},
 {applications, [kernel, stdlib, hordad_lcf, hordad_log]},
 {mod, {hordad_tier2_app, []}}
 ]}.
