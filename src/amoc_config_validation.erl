-module(amoc_config_validation).

-compile([export_all, nowarn_export_all]).

%% API

positive_integer(I) -> is_integer(I) andalso I > 0.

nonnegative_integer(I) -> is_integer(I) andalso I >= 0.

binary(Binary) -> is_binary(Binary).

boolean(Boolean) -> is_boolean(Boolean).

binary_or_undefined(BinaryOrUndefined) ->
    binary(BinaryOrUndefined) orelse BinaryOrUndefined =:= undefined.
