{application, hordad_master,
 [{description, "Hordad Master application"},
  {vsn, "0.1"},
  {modules, [hordad_master_app, hordad_master_sup, hordad_master]},
  {registered, [hordad_master]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {hordad_master_app, []}}
 ]}.
