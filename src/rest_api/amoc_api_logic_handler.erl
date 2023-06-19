%%==============================================================================
%% Copyright 2020 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_api_logic_handler).

-behaviour(amoc_rest_logic_handler).

-include_lib("kernel/include/logger.hrl").

-export([handle_request/3]).

-spec handle_request(OperationID :: amoc_rest_api:operation_id(),
                     Req :: cowboy_req:req(), Context :: #{}) ->
    {Status :: cowboy:http_status(), Headers :: cowboy:http_headers(), Body :: jsx:json_term()}.
handle_request('StatusGet', _Req, _Context) ->
    Status = amoc_api_helpers_status:get_status(),
    {200, #{}, Status};
handle_request('StatusNodeGet', _Req, #{node := BinNode}) ->
    Node = binary_to_atom(BinNode, utf8),
    case rpc:call(Node, amoc_api_helpers_status, get_status, []) of
        {badrpc, _} -> {404, #{}, #{}};
        Status -> {200, #{}, Status}
    end;
handle_request('NodesGet', _Req, _Context) ->
    Status = amoc_cluster:get_status(),
    Connected = maps:get(connected, Status, []),
    FailedToConnect = maps:get(failed_to_connect, Status, []),
    ConnectionLost = maps:get(connection_lost, Status, []),
    Up = [{Node, <<"up">>} || Node <- [node() | Connected]],
    DownNodes = lists:usort(FailedToConnect ++ ConnectionLost),
    Down = [{Node, <<"down">>} || Node <- DownNodes],
    ResponseList = Up ++ Down,
    {200, #{}, #{nodes => ResponseList}};
handle_request('ScenariosGet', _Req, _Context) ->
    Scenarios = amoc_scenario:list_scenario_modules(),
    BinaryScenarios = [atom_to_binary(S, utf8) || S <- Scenarios],
    {200, #{}, #{scenarios => BinaryScenarios}};
handle_request('ScenariosDefaultsIdGet', _Req, #{id := ScenarioName}) ->
    case amoc_api_helpers_scenario_info:is_loaded(ScenarioName) of
        false ->
            {404, #{}, #{}};
        {true, Scenario} ->
            Settings = amoc_api_helpers_scenario_info:scenario_settings(Scenario),
            {200, #{}, #{settings =>Settings}}
    end;
handle_request('ScenariosInfoIdGet', _Req, #{id := ScenarioName}) ->
    case amoc_api_helpers_scenario_info:is_loaded(ScenarioName) of
        false ->
            {404, #{}, #{}};
        {true, Scenario} ->
            EDoc = amoc_api_helpers_scenario_info:get_edoc(Scenario),
            Params = amoc_api_helpers_scenario_info:scenario_params(Scenario),
            {200, #{}, #{doc => EDoc, parameters => Params}}
    end;
handle_request('ScenariosUploadPut', Req, _Context) ->
    {ok, ModuleSource, _} = cowboy_req:read_body(Req),
    case amoc_api_helpers_scenario_upload:upload(ModuleSource) of
        ok ->
            {200, #{}, #{compile => <<"ok">>}};
        {error, invalid_module} ->
            {400, #{}, #{error => <<"invalid module">>}};
        {error, Error} ->
            {200, #{}, #{compile => Error}}
    end;
handle_request('ExecutionStartPatch', _Req, #{'ExecutionStart' := Body}) ->
    case amoc_dist:get_state() of
        idle ->
            Ret = amoc_api_helpers_execution:start(Body),
            process_ret_value(Ret);
        _ -> {409, #{}, #{}}
    end;
handle_request('ExecutionStopPatch', _Req, #{}) ->
    case amoc_dist:get_state() of
        running ->
            Ret = amoc_api_helpers_execution:stop(),
            process_ret_value(Ret);
        _ -> {409, #{}, #{}}
    end;
handle_request('ExecutionAddUsersPatch', _Req, #{'ExecutionChangeUsers' := Body}) ->
    case amoc_dist:get_state() of
        running ->
            Ret = amoc_api_helpers_execution:add_users(Body),
            process_ret_value(Ret);
        _ -> {409, #{}, #{}}
    end;
handle_request('ExecutionRemoveUsersPatch', _Req, #{'ExecutionChangeUsers' := Body}) ->
    case amoc_dist:get_state() of
        running ->
            Ret = amoc_api_helpers_execution:remove_users(Body),
            process_ret_value(Ret);
        _ -> {409, #{}, #{}}
    end;
handle_request('ExecutionUpdateSettingsPatch', _Req, #{'ExecutionUpdateSettings' := Body}) ->
    case amoc_dist:get_state() of
        running ->
            Ret = amoc_api_helpers_execution:update_settings(Body),
            process_ret_value(Ret);
        _ -> {409, #{}, #{}}
    end;
handle_request(OperationID, Req, Context) ->
    ?LOG_ERROR("Got not implemented request to process: ~p~n",
               [{OperationID, Req, Context}]),
    {501, #{}, #{}}.

process_ret_value({ok, _}) -> {200, #{}, #{}};
process_ret_value({error, Error}) ->
    {500, #{}, #{error => amoc_config_env:format(Error, binary)}}.
