{application, edns,
  [{description, "A Erlang OTP DNS Server"},
   {vsn, "0.1.0"},
   {modules, [
     edns,
     ed_app, 
     ed_sup, 
     ed_udp_server,
     ed_udp_handler_sup,
     ed_udp_handler_server,
     ed_zone_sup,
     ed_zone_registry_server,
     ed_zone_data_sup,
     ed_zone_data_server,
     ed_query_resolver,
     simple_backend,
     ed_utils,
     dummy_parked_backend,
     ed_zone_validator
   ]},
   {registered, [ed_sup]},
   {applications, [kernel, stdlib]},
   {mod, {ed_app, []}},
   {env, [
     {port, 1051}
   ]}
  ]
}.
