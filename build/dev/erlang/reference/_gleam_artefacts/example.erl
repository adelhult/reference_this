-module(example).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export_type([reference_/0]).

-type reference_() :: {reference, list(binary()), binary(), binary()}.


