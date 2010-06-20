{application, hordad_registrar,
 [{description, "Application dispatcher"},
 {vsn, "0.1"},
 {modules, [hordad_registrar_app, hordad_registrar_sup, hordad_registrar]},
 {registered, [hordad_registrar]},
 {applications, [kernel, stdlib, hordad_lcf, hordad_log]},
 {mod, {hordad_registrar_app, []}}
 ]}.
