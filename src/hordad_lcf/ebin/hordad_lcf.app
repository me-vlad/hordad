{application, hordad_lcf,
 [{description, "Local configuration facility"},
 {vsn, "0.1"},
 {modules, [hordad_lcf_app, hordad_lcf_sup, hordad_lcf]},
 {registered, [hordad_lcf]},
 {applications, [kernel, stdlib]},
 {mod, {hordad_lcf_app, []}}
 ]}.
