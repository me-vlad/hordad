{application, hordad_dht,
 [{description, "Hordad DHT overlay network"},
 {vsn, "0.1"},
 {modules, [hordad_dht_app, hordad_dht_sup]},
 {registered, []},
 {applications, [kernel, stdlib, crypto, hordad_lcf, hordad_log]},
 {mod, {hordad_dht_app, []}}
 ]}.
