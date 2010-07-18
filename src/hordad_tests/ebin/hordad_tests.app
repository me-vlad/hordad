{application, hordad_tests,
 [{description, "Tests framework"},
 {vsn, "0.1"},
 {modules, [hordad_tests_app]},
 {registered, []},
 {applications, [kernel, stdlib, hordad_lcf, hordad_log]},
 {mod, {hordad_tests_app, []}}
 ]}.
