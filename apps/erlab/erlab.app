{application, erlab, [
    {description, "Run regular expressions on Erlang source files"},
    {vsn, "1.0.0"},
    {modules, [
        erlcount,
        erlcount_counter,
        erlcount_dispatch,
        erlcount_lib,
        erlcount_sup
    ]},
    {registered, [erlcount]},
    {applications, [kernel, stdlib, ppool]},
    {mod, {erlcount, []}},
    {env, [
        {directory, "."},
        {regex, [
            "if\\s.+->",
            "case\\s.+\\sof"
        ]},
        {max_files, 10}
    ]}
]}.
