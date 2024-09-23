{application, reference, [
    {vsn, "0.1.0"},
    {applications, [argv,
                    gleam_stdlib,
                    glint,
                    simplifile]},
    {description, "A development tool to reference other gleam files"},
    {modules, [example,
               reference]},
    {registered, []}
]}.
