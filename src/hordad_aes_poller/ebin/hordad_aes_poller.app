{application, hordad_aes_poller,
 [{description, "AES poller"},
 {vsn, "0.1"},
 {modules, [hordad_aes_poller_app, hordad_aes_poller_sup]},
 {registered, []},
 {applications, [kernel, stdlib, hordad_lcf, hordad_log]},
 {mod, {hordad_aes_poller_app, []}}
 ]}.
