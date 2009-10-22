{application, someapp,
 [{description, "DIY Provider"},
  {vsn, "0.1"},
  {modules, [someapp, someapp_sup]},
  {registered, [someapp, someapp_sup]},
  {applications, [kernel, stdlib]},
  {env, []},
  {mod, {someapp_app, []}}]}.
