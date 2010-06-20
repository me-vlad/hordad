{application, hordad_aes_agent,
 [{description, "Availability Ensuring System - agent"},
 {vsn, "0.1"},
 {modules, [hordad_aes_agent_app, hordad_aes_agent_sup,
            hordad_aes_agent, hordad_aes_agent_lib]},
 {registered, [hordad_aes_agent]},
 {applications, [kernel, stdlib, hordad_lcf, hordad_log]},
 {mod, {hordad_aes_agent_app, []}}
 ]}.
