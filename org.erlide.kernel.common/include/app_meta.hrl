
-type options() :: [{atom(), any()}].
-type meta_path() :: string() |
                     {atom(), string()}.
-record(layout, {
                 src = ["src"] :: [meta_path()],
                 include = ["include"] :: [meta_path()],
                 ebin = "ebin" :: meta_path(),
                 doc = ["doc"] :: [meta_path()],
                 priv = "priv" :: meta_path()
                }).
-type meta_module() :: module() |
                       {module(), options()}.
-type meta_app() :: atom() |
                    {atom(), meta_path()} |
                    {atom, #layout{}}.

-record(app_meta, {
               name :: atom(),
               description = "" :: string(),
               id = "" :: string(),
               vsn = "" :: string(),
               modules = [] :: [meta_module()],
               maxT  = infinity :: integer() | 'infinity',
               registered = [] :: [atom()],
               included_applications = [] :: [meta_app()],
               applications = [] :: [meta_app()],
               env = [] :: [{any(), any()}],
               mod = undefined :: {module(), any()} | 'undefined',
               start_phases = undefined :: [{atom(), any()}] | 'undefined',
               otp_version = undefined :: string() | 'undefined',
               layout = #layout{},
               compiler_options = [] :: options()
              }).

