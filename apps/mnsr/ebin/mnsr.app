{application,mnsr,
             [{description,"mnsr"},
              {vsn,"0.0.1"},
              {modules,[mnsr,mnsr_app,mnsr_event_proc,mnsr_resource,mnsr_sup]},
              {registered,[]},
              {applications,[kernel,stdlib,inets,crypto,mochiweb,webmachine]},
              {mod,{mnsr_app,[]}},
              {env,[]}]}.
