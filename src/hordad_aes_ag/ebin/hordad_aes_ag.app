{application, hordad_aes_ag,
 [{description, "AES aggregator"},
 {vsn, "0.1"},
 {modules, [hordad_aes_ag_app, hordad_aes_ag_sup]},
 {registered, [hordad_aes_ag]},
 {applications, [kernel, stdlib, hordad_lcf, hordad_log]},
 {mod, {hordad_aes_ag_app, []}}
 ]}.
