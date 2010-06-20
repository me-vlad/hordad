{application, hordad_rooms,
 [{description, "Room manager"},
 {vsn, "0.1"},
 {modules, [hordad_rooms_app, hordad_rooms_sup, hordad_rooms]},
 {registered, [hordad_rooms]},
 {applications, [kernel, stdlib, hordad_lcf, hordad_log]},
 {mod, {hordad_rooms_app, []}}
 ]}.
