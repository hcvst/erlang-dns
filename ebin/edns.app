{application, edns,
  [{description, "A Erlang OTP DNS Server"},
   {vsn, "0.1.0"},
   {modules, [
     edns,
     ed_app, 
     ed_sup, 
     ed_server,
     ed_lkup_sup,
     ed_lkup_server
   ]},
   {registered, [ed_sup]},
   {applications, [kernel, stdlib]},
   {mod, {ed_app, []}},
   {env, [
     {port, 1053}
   ]}
  ]
}.
