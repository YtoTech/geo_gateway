%% @doc A simple example callback module for `gen_smtp_server_session' that also serves as
%% documentation for the required callback API.

-module(geo_gateway_smtp_server).
-author('yoan@ytotech.com').
-behaviour(gen_smtp_server_session).


-export([init/4, handle_HELO/2, handle_EHLO/3, handle_MAIL/2, handle_MAIL_extension/2,
	handle_RCPT/2, handle_RCPT_extension/2, handle_DATA/4, handle_RSET/1, handle_VRFY/2,
	handle_other/3, handle_AUTH/4, handle_info/2, handle_STARTTLS/1,
	code_change/3, terminate/2]).

-record(state,
	{
		options = [] :: list(),
		user :: map() | 'undefined'
	}).

-type(error_message() :: {'error', string(), #state{}}).

%% @doc Initialize the callback module's state for a new session.
%% The arguments to the function are the SMTP server's hostname (for use in the SMTP anner),
%% The number of current sessions (eg. so you can do session limiting), the IP address of the
%% connecting client, and a freeform list of options for the module. The Options are extracted
%% from the `callbackoptions' parameter passed into the `gen_smtp_server_session' when it was
%% started.
%%
%% If you want to continue the session, return `{ok, Banner, State}' where Banner is the SMTP
%% banner to send to the client and State is the callback module's state. The State will be passed
%% to ALL subsequent calls to the callback module, so it can be used to keep track of the SMTP
%% session. You can also return `{stop, Reason, Message}' where the session will exit with Reason
%% and send Message to the client.
-spec init(Hostname :: inet:hostname(), SessionCount :: non_neg_integer(), Address :: inet:ip_address(), Options :: list()) -> {'ok', iodata(), #state{}} | {'stop', any(), iodata()}.
init(Hostname, SessionCount, Address, Options) ->
	lager:debug("peer: ~p", [Address]),
	case SessionCount > 20 of
		false ->
			Banner = [Hostname, " ESMTP YtoTech_GeoSensors_Gateway_SMTP"],
			State = #state{options = Options},
			{ok, Banner, State};
		true ->
			lager:warning("Connection limit exceeded"),
			{stop, normal, ["421 ", Hostname, " is too busy to accept mail right now"]}
	end.

%% @doc Handle the HELO verb from the client. Arguments are the Hostname sent by the client as
%% part of the HELO and the callback State.
%%
%% Return values are `{ok, State}' to simply continue with a new state, `{ok, MessageSize, State}'
%% to continue with the SMTP session but to impose a maximum message size (which you can determine
%% , for example, by looking at the IP address passed in to the init function) and the new callback
%% state. You can reject the HELO by returning `{error, Message, State}' and the Message will be
%% sent back to the client. The reject message MUST contain the SMTP status code, eg. 554.
-spec handle_HELO(Hostname :: binary(), State :: #state{}) -> {'ok', pos_integer(), #state{}} | {'ok', #state{}} | error_message().
handle_HELO(<<"invalid">>, State) ->
	% contrived example
	{error, "554 invalid hostname", State};
handle_HELO(<<"trusted_host">>, State) ->
	{ok, State}; %% no size limit because we trust them.
handle_HELO(Hostname, State) ->
	lager:debug("HELO from ~s", [Hostname]),
	{ok, 655360, State}. % 640kb of HELO should be enough for anyone.
	%If {ok, State} was returned here, we'd use the default 10mb limit

%% @doc Handle the EHLO verb from the client. As with EHLO the hostname is provided as an argument,
%% but in addition to that the list of ESMTP Extensions enabled in the session is passed. This list
%% of extensions can be modified by the callback module to add/remove extensions.
%%
%% The return values are `{ok, Extensions, State}' where Extensions is the new list of extensions
%% to use for this session or `{error, Message, State}' where Message is the reject message as
%% with handle_HELO.
-spec handle_EHLO(Hostname :: binary(), Extensions :: list(), State :: #state{}) -> {'ok', list(), #state{}} | error_message().
handle_EHLO(<<"invalid">>, _Extensions, State) ->
	% contrived example
	{error, "554 invalid hostname", State};
handle_EHLO(Hostname, Extensions, State) ->
	lager:info("EHLO from ~s", [Hostname]),
	% You can advertise additional extensions, or remove some defaults
    lager:debug("auth is ~w", [proplists:get_value(auth, State#state.options, false)]),
	MyExtensions = case proplists:get_value(auth, State#state.options, false) of
		true ->
			% auth is enabled, so advertise it.
			Extensions ++ [{"AUTH", "PLAIN LOGIN CRAM-MD5"}];
		false ->
			Extensions
	end,
	{ok, MyExtensions, State}.

%% @doc Handle the MAIL FROM verb. The From argument is the email address specified by the
%% MAIL FROM command. Extensions to the MAIL verb are handled by the `handle_MAIL_extension'
%% function.
%%
%% Return values are either `{ok, State}' or `{error, Message, State}' as before.
-spec handle_MAIL(From :: binary(), State :: #state{}) -> {'ok', #state{}} | error_message().
handle_MAIL(From, State = #state{user=User}) when User =/= undefined ->
	% Filter the accepted mails.
	lager:info("Mail from ~s", [From]),
	case maps:find(email, State#state.user) of
		{ok, From} ->
			{ok, State};
		_ ->
			{error, "552 Not Managed", State}
	end;
handle_MAIL(From, State) ->
	% TODO Why gen_smtp is accepting the mail?
	lager:info("Refused a mail from ~s", [From]),
	{error, "535 Authentication failed", State}.

%% @doc Handle an extension to the MAIL verb. Return either `{ok, State}' or `error' to reject
%% the option.
handle_MAIL_extension(Extension, _State) ->
	lager:warning("Unknown MAIL FROM extension ~s", [Extension]),
	error.

handle_RCPT(_To, State) ->
	{ok, State}.

handle_RCPT_extension(Extension, _State) ->
	lager:warning("Unknown RCPT TO extension ~s", [Extension]),
	error.

-spec handle_DATA(From :: binary(), To :: [binary(),...], Data :: binary(), State :: #state{}) -> {'ok', string(), #state{}} | {'error', string(), #state{}}.
handle_DATA(_From, _To, <<>>, State) ->
	{error, "552 Message too small", State};
handle_DATA(From, To, Data, State) ->
	% Some kind of unique id.
	Reference = erlang:iolist_to_binary(lists:flatten([
		io_lib:format("~2.16.0b", [X]) || <<X>> <= erlang:md5(term_to_binary(unique_id()))
	])),
	% We do not relay emails but process them.
	lager:debug("message from ~s to ~p queued as ~s, body length ~p", [From, To, Reference, byte_size(Data)]),
	% We always try to parse emails.
	% TODO Refactorize and simplify: this is messy and we get lost here.
	% --> Create functions for decoding, parsing and/or forwarding?
	DumpsRawMessage = try mimemail:decode(Data) of
		{_Type, _SubType, Headers, _Properties, Body} ->
			lager:debug("From: ~s To: ~s", [From, To]),
			lager:debug("Headers: ~p", [Headers]),
			lager:debug("Body: ~s", [Body]),
			User = State#state.user,
			lager:debug("User: ~p", [User]),
			% Now parse the actual sensor payload.
			% TODO Make this customizable using a callback
			% parser_server.
			case geo_gateway_payload_parser:parse(
				Reference,
				Body,
				User,
				proplists:get_value(devices, State#state.options, #{})
			) of
				{ok, Payload, Device} ->
					lager:debug("Parsed payload: ~p", [Payload]),
					% When payload extracted from the mail,
					% give it to the forwarder module that will handle its
					% transmission.
					ok = geo_gateway_forwarding_router:forward(
						Reference,
						Payload,
						User,
						Device,
						proplists:get_value(forwarders, State#state.options, #{})
					);
				{error, Reason} ->
					% TODO Always dumps on error? (Have another parameter than dumps_incoming)
					% For e.g. dumps_on_parsing_error
					lager:error("Error while parsing device payload: ~s", [Reason])
			end,
			% If dumping is enabled on the user, dump all messages, whatever the outcome.
			% ---> Will be usefull for debugging. (And make the server iso with the Python one)
			maps:get(dumps_incoming, User, false)
	catch
		What:Why ->
			lager:error("Message decode FAILED with ~p:~p", [What, Why]),
			proplists:get_value(dumps_incoming, State#state.options, false)
	end,
	% Function to dump messages somewhere for analysis.
	DumpToFile = fun(Path, Name) ->
		File = erlang:iolist_to_binary([Path, Name, <<".eml">>]),
		lager:debug("Dump incoming message to ~s", [File]),
		case filelib:ensure_dir(File) of
			ok ->
				file:write_file(File, Data);
			_ ->
				ok
		end
	end,
	case DumpsRawMessage of
		false ->
			ok;
		true ->
			DumpToFile(proplists:get_value(dumps_directory, State#state.options, "dumps/"), Reference)
	end,
	% At this point, if we return ok, we've accepted responsibility for the email
	{ok, Reference, State}.

-spec handle_RSET(State :: #state{}) -> #state{}.
handle_RSET(State) ->
	% reset any relevant internal state
	State.

handle_VRFY(_Address, State) ->
	{error, "252 VRFY disabled by policy, just send some mail", State}.

-spec handle_other(Verb :: binary(), Args :: binary(), #state{}) -> {string(), #state{}}.
handle_other(Verb, _Args, State) ->
	% You can implement other SMTP verbs here, if you need to
	{["500 Error: command not recognized : '", Verb, "'"], State}.

%% this callback is OPTIONAL
%% it only gets called if you add AUTH to your ESMTP extensions
-spec handle_AUTH(Type :: 'login' | 'plain' | 'cram-md5', Username :: binary(), Password :: binary() | {binary(), binary()}, #state{}) -> {'ok', #state{}} | 'error'.
handle_AUTH(Type, Username, Password, State) ->
	% Retrieve user in user database and get the user password.
	case maps:find(Username, proplists:get_value(users, State#state.options, #{})) of
		{ok, User} ->
			handle_AUTH_user(Type, Password, maps:get(password, User), State#state{user=User});
		_ ->
			error
	end.

-spec handle_AUTH_user(Type :: 'login' | 'plain' | 'cram-md5', ProvidedPassword :: binary() | {binary(), binary()}, UserPassword :: binary(), #state{}) -> {'ok', #state{}} | 'error'.
handle_AUTH_user(Type, ProvidedPassword, UserPassword, State) when Type =:= login; Type =:= plain, ProvidedPassword =:= UserPassword ->
	{ok, State};
handle_AUTH_user('cram-md5', {Digest, Seed}, UserPassword, State = #state{user=User}) ->
	case smtp_util:compute_cram_digest(UserPassword, Seed) of
		Digest ->
			{ok, State};
		_ ->
			lager:info("Bad password for ~s", [maps:get(email, User)]),
			error
	end;
handle_AUTH_user(Type, Password, _UserPassword, _State) ->
	lager:warning("handle_AUTH error ~w ~w", [Type, Password]),
	error.

handle_info(_Info, State) ->
	{noreply, State}.

%% this callback is OPTIONAL
%% it only gets called if you add STARTTLS to your ESMTP extensions
-spec handle_STARTTLS(#state{}) -> #state{}.
handle_STARTTLS(State) ->
    lager:info("TLS Started"),
    State.

-spec code_change(OldVsn :: any(), State :: #state{}, Extra :: any()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

-spec terminate(Reason :: any(), State :: #state{}) -> {'ok', any(), #state{}}.
terminate(Reason, State) ->
	{ok, Reason, State}.

%%% Internal Functions %%%

-ifdef(deprecated_now).
unique_id() ->
    erlang:unique_integer().
-else.
unique_id() ->
    erlang:now().
-endif.
