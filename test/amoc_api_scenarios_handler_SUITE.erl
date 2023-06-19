-module(amoc_api_scenarios_handler_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("scenario_template.hrl").

-define(SCENARIOS_URL_S, "/scenarios").
-define(SCENARIOS_URL_U, "/scenarios/upload").
-define(SCENARIOS_URL_I(Module), "/scenarios/info/" ++ atom_to_list(Module)).
-define(SCENARIOS_URL_D(Module), "/scenarios/defaults/" ++ atom_to_list(Module)).

-define(SAMPLE_SCENARIO, sample_test).
-define(SAMPLE_SCENARIO_DECLARATION,
        "-module(" ++ atom_to_list(?SAMPLE_SCENARIO) ++ ").").

-export([all/0, groups/0, init_per_testcase/2, end_per_testcase/2]).

-export([
         get_scenarios_returns_200_and_scenarios_list_when_requested/1,
         put_scenarios_returns_400_and_error_when_scenario_is_not_valid/1,
         put_scenarios_returns_200_and_compile_error_when_scenario_source_not_valid/1,
         put_scenarios_returns_200_when_scenario_valid/1,
         get_scenario_info_returns_404_when_scenario_does_not_exist/1,
         get_scenario_info_returns_200_when_scenario_exists/1,
         get_scenario_defaults_returns_404_when_scenario_does_not_exist/1,
         get_scenario_defaults_returns_200_when_scenario_exists/1
        ]).


all() ->
    [{group, all}].

groups() ->
    [{all, [sequence], tests()}].

tests() ->
    [
     get_scenarios_returns_200_and_scenarios_list_when_requested,
     put_scenarios_returns_400_and_error_when_scenario_is_not_valid,
     put_scenarios_returns_200_and_compile_error_when_scenario_source_not_valid,
     put_scenarios_returns_200_when_scenario_valid,
     get_scenario_info_returns_200_when_scenario_exists,
     get_scenario_defaults_returns_200_when_scenario_exists,
     get_scenario_info_returns_404_when_scenario_does_not_exist,
     get_scenario_defaults_returns_404_when_scenario_does_not_exist
    ].

init_per_testcase(get_scenario_info_returns_404_when_scenario_does_not_exist, Config) ->
    amoc_scenario:remove_module(?SAMPLE_SCENARIO),
    amoc_api_helper:start_amoc(),
    Config;
init_per_testcase(_, Config) ->
    amoc_api_helper:start_amoc(),
    Config.

end_per_testcase(TestCase, _Config)
    when TestCase =:= get_scenario_info_returns_200_when_scenario_exists;
         TestCase =:= put_scenarios_returns_200_when_scenario_valid ->
    amoc_api_helper:remove_module(?SAMPLE_SCENARIO),
    amoc_api_helper:stop_amoc();
end_per_testcase(_, _Config) ->
    amoc_api_helper:stop_amoc().

get_scenarios_returns_200_and_scenarios_list_when_requested(_Config) ->
    %% when
    {CodeHttp, Body} = amoc_api_helper:get(?SCENARIOS_URL_S),
    %% then
    ?assertEqual(200, CodeHttp),
    ?assertMatch(#{<<"scenarios">> := List} when is_list(List), Body),
    maps:get(<<"scenarios">>, Body).

put_scenarios_returns_400_and_error_when_scenario_is_not_valid(_Config) ->
    %% given
    ScenarioContent = "invalid_source",
    %% when
    {CodeHttp, Body} = amoc_api_helper:put(?SCENARIOS_URL_U, ScenarioContent),
    ScenarioFileSource = amoc_api_helper:module_src(?SAMPLE_SCENARIO),
    %% then
    ?assertNot(filelib:is_regular(ScenarioFileSource)),
    ?assertEqual(400, CodeHttp),
    ?assertEqual(#{<<"error">> => <<"invalid module">>}, Body).

-if(?OTP_RELEASE >= 24).
-define(PARSE_ERROR,
        <<"\n                      [{{2,1},erl_parse,[\"syntax error before: \",[]]}]}]\n">>).
-else.
-define(PARSE_ERROR,
        <<"\n                      [{2,erl_parse,[\"syntax error before: \",[]]}]}]\n">>).
-endif.

put_scenarios_returns_200_and_compile_error_when_scenario_source_not_valid(_Config) ->
    %% given
    ScenarioContent = ?SAMPLE_SCENARIO_DECLARATION ++ "\ninvalid_source",
    %% when
    {CodeHttp, Body} = amoc_api_helper:put(?SCENARIOS_URL_U, ScenarioContent),
    ScenarioFileSource = amoc_api_helper:module_src(?SAMPLE_SCENARIO),
    %% then
    ?assertNot(filelib:is_regular(ScenarioFileSource)),
    ?assertEqual(200, CodeHttp),
    Error = <<"compilation errors: [{\"", (list_to_binary(ScenarioFileSource))/binary, "\",",
              (?PARSE_ERROR)/binary>>,
    ?assertEqual(#{<<"compile">> => Error}, Body).

put_scenarios_returns_200_when_scenario_valid(Config) ->
    %% given
    ScenarioContent = ?DUMMY_SCENARIO_MODULE(?SAMPLE_SCENARIO),
    %% when
    {CodeHttp, Body} = amoc_api_helper:put(?SCENARIOS_URL_U, ScenarioContent),
    ScenarioFileSource = amoc_api_helper:module_src(?SAMPLE_SCENARIO),
    ScenarioFileBeam = amoc_api_helper:module_beam(?SAMPLE_SCENARIO),
    %% then
    ?assertEqual(200, CodeHttp),
    ?assertEqual(#{<<"compile">> => <<"ok">>}, Body),
    ?assert(filelib:is_regular(ScenarioFileSource)),
    ?assert(filelib:is_regular(ScenarioFileBeam)),
    Scenarios = get_scenarios_returns_200_and_scenarios_list_when_requested(Config),
    ?assertEqual(true, lists:member(atom_to_binary(?SAMPLE_SCENARIO, utf8), Scenarios)).


get_scenario_info_returns_404_when_scenario_does_not_exist(_Config) ->
    amoc_scenario:remove_module(?SAMPLE_SCENARIO),
    %% when
    {CodeHttp, _Body} = amoc_api_helper:get(?SCENARIOS_URL_I(?SAMPLE_SCENARIO)),
    %% then
    ?assertEqual(404, CodeHttp).

get_scenario_info_returns_200_when_scenario_exists(Config) ->
    %% given scenario exists
    put_scenarios_returns_200_when_scenario_valid(Config),
    mock_amoc_amoc_scenario(),
    %% when
    {CodeHttp, Body} = amoc_api_helper:get(?SCENARIOS_URL_I(?SAMPLE_SCENARIO)),
    ?assertEqual(200, CodeHttp),
    ExpectedInfo = #{<<"doc">> => <<"\nsome edoc\n\n">>,
                     <<"parameters">> =>
                         #{<<"interarrival">> =>
                               #{<<"default_value">> => <<"50">>,
                                 <<"description">> => <<"\"a delay between creating the"
                                                        " processes for two consecutive"
                                                        " users (ms, def: 50ms)\"">>,
                                 <<"module">> => <<"amoc_controller">>,
                                 <<"update_fn">> =>
                                 <<"fun amoc_controller:maybe_update_interarrival_timer/2">>,
                                 <<"verification_fn">> =>
                                 <<"fun amoc_controller:positive_integer/1">>},
                           <<"some_parameter">> =>
                               #{<<"default_value">> => <<"undefined">>,
                                 <<"description">> => <<"\"some parameter\"">>,
                                 <<"module">> => <<"sample_test">>,
                                 <<"update_fn">> => <<"read_only">>,
                                 <<"verification_fn">> =>
                                 <<"fun amoc_config_attributes:none/1">>}}},
    ?assertMatch(ExpectedInfo, Body),
    meck:unload(amoc_scenario).

get_scenario_defaults_returns_404_when_scenario_does_not_exist(_Config) ->
    %% when
    {CodeHttp, _Body} = amoc_api_helper:get(?SCENARIOS_URL_D(?SAMPLE_SCENARIO)),
    %% then
    ?assertEqual(404, CodeHttp).

get_scenario_defaults_returns_200_when_scenario_exists(Config) ->
    %% given scenario exists
    put_scenarios_returns_200_when_scenario_valid(Config),
    mock_amoc_amoc_scenario(),
    %% when
    {CodeHttp, Body} = amoc_api_helper:get(?SCENARIOS_URL_D(?SAMPLE_SCENARIO)),
    ?assertEqual(200, CodeHttp),
    ExpectedInfo = #{<<"settings">> => #{<<"interarrival">> => <<"50">>,
                                         <<"some_parameter">> => <<"undefined">>}},
    ?assertMatch(ExpectedInfo, Body),
    meck:unload(amoc_scenario).

mock_amoc_amoc_scenario() ->
    ok = meck:new(amoc_scenario, [passthrough]),
    Fun = fun() ->
            Modules = meck:passthrough([]),
            Modules -- arsenal_app_modules()
          end,
    ok = meck:expect(amoc_scenario, list_configurable_modules, Fun).

-spec arsenal_app_modules() -> [atom()].
arsenal_app_modules() ->
    Path = code:lib_dir(amoc_arsenal_xmpp, ebin),
    {ok, FileNames} = file:list_dir(Path),
    BeamFileNames = lists:filter(fun(Name) -> filename:extension(Name) =:= ".beam" end, FileNames),
    [list_to_atom(filename:rootname(Name)) || Name <- BeamFileNames].
