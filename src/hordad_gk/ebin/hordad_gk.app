{application, hordad_gk,
 [{description, "Hordad GateKeeper"},
 {vsn, "0.1"},
 {modules, [hordad_gk_app, hordad_gk_sup, hordad_gk]},
 {registered, [hordad_gk]},
 {applications, [kernel, stdlib, hordad_lcf, hordad_log]},
 {mod, {hordad_gk_app, []}}
 ]}.
