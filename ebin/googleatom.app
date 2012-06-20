{application, googleatom,
 [{description, "Google ATOM WebServices"},
  {vsn, "1.0"},
  {modules, [
		blogger_srv,
		picasa_srv,
		google,
		googleatom_app,
		googleatom_sup
	    ]},
  {registered, []},
  {mod, {googleatom_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto, inets, ssl]}]}.
