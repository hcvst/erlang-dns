{application, edns,
  [{description, "A Erlang OTP DNS Server"},
   {vsn, "0.1.0"},
   {modules, [ed_app, ed_sup, ed_server]},
   {registered, [ed_sup]},
   {applications, [kernel, stdlib]},
   {mod, {ed_app, []}}
  ]
}.
