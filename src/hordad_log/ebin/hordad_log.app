{application, hordad_log,
 [{description, "Hordad logging facility"},
 {vsn, "0.1"},
 {modules, [hordad_log_app, hordad_log_sup, hordad_log,
            hordad_log_text_h]},
 {registered, [hordad_log]},
 {applications, [kernel, stdlib, hordad_lcf]},
 {mod, {hordad_log_app, []}}
 ]}.
