# Questions about Neovim interface ?

How is the host registered ?
Through runtime/autoload/remote/host.vim calling `remote#host#Register`.

`remote#host#Register` prototype ?
`void(host_name, file_pattern, provider_funcref)`

`provider_funcref` prototype ?
see _runtime/autoload/provider/*.vim_ `provider#*#Require`:
`channel_id(host_object)`
It is supposed to call `provider#Poll` to run a channel
that perform a _poll_ request via RPC.

How `provider#Poll` work and how to call it ?
`channel_id(argv, orig_name, log_env)`
`argv` is a list of strings for a command line call
`orig_name` is the host name
`log_env` is a loging file name in case it fail

Interesting example of more or less the same as I want to do ?
https://github.com/adolenc/cl-neovim
