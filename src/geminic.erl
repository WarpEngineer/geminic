%%%-------------------------------------------------------------------
%%% @author agmadi
%%% @copyright ( C ) 2021, agmadi
%%% @doc geminic - A Gemini protocol client library.
%%%
%%% @end
%%% Created : 2021-05-24 20:35:35.333672
%%% @todo Look into proxy ([https://github.com/LukeEmmet/duckling-proxy])
%%%-------------------------------------------------------------------
-module( geminic ).
-author( "agmadi" ).
-vsn( "0.1.0" ).

-behaviour( gen_server ).

-type clientcert()::{ { certfile, string() }, { keyfile, string() } } | undefined.

-type option()::{ autoredirect, true | false } 
		| { clientcert, clientcert() }.

-type optionlist()::[option()].

-type returnheader()::{ Code::string(), Meta::string() }.

-type returnresult():: { returnheader(), Result::string() }.

%% API
-export( [start_link/0] ).
-export( [request/1, request/2, setopt/2, getopts/0, getopt/1] ).
-export( [generate_outline/1, gather_links/1, make_cert/2] ).

%% gen_server callbacks
-export( [init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3] ).

-define( SERVER, ?MODULE ).
-define( DEFAULT_PORT, 1965 ).
-define( TIMEOUT, 30000 ).

-record( state, {from=[], socket=[], host=[], port=0, response=[], options=[]} ).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
	gen_server:start_link( {local, ?SERVER}, ?MODULE, [], [] ).

%%%-------------------------------------------------------------------
%%% @doc Send a request with default timeout of 30 seconds. 
%%% 
%%% @param URI The full URI requested, including the protocol.
%%% @returns A tuple of { Header, Body } or { error, Reason }
%%% @end
%%%-------------------------------------------------------------------
-spec request( URI::string() ) -> returnresult() | { error, Reason::term() }.
request( URI ) ->
	gen_server:call( ?SERVER, {request, URI}, ?TIMEOUT ).

%%%-------------------------------------------------------------------
%%% @doc Send a request with specific timeout. 
%%% 
%%% @param URI The full URI requested, including the protocol.
%%% @param Timeout The timeout in milliseconds or 'infinity'
%%% @returns A tuple of { Header, Body } or { error, Reason }
%%% @end
%%%-------------------------------------------------------------------
-spec request( URI::string(), Timeout::integer() | infinity ) -> returnresult() | { error, Reason::term() }.
request( URI, Timeout ) ->
	gen_server:call( ?SERVER, {request, URI}, Timeout ).

%%%-------------------------------------------------------------------
%%% @doc Set an option
%%% 
%%% @param Opt The key for the option to set
%%% @param Val The value for the option to set
%%% @returns The complete options list with all settings
%%% @end
%%%-------------------------------------------------------------------
-spec setopt( Opt::term(), Val::term() ) -> optionlist().
setopt( Opt, Val ) ->
	gen_server:call( ?SERVER, {setopt, Opt, Val} ).

%%%-------------------------------------------------------------------
%%% @doc Get all options
%%% 
%%% @returns The complete options list with all settings
%%% @end
%%%-------------------------------------------------------------------
-spec getopts() -> optionlist().
getopts() ->
	gen_server:call( ?SERVER, getopts ).

%%%-------------------------------------------------------------------
%%% @doc Get an option
%%% 
%%% @param Opt The key for the option to get
%%% @returns The value of the option or the atom 'error' if not found
%%% @end
%%%-------------------------------------------------------------------
-spec getopt( Opt::term() ) -> option() | error.
getopt( Opt ) ->
	gen_server:call( ?SERVER, {getopt, Opt} ).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init( Args ) -> {ok, State} |
%%			 {ok, State, Timeout} |
%%			 ignore |
%%			 {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init( [] ) ->
	{ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call( Request, From, State ) ->
%% 			{reply, Reply, State} |
%%			{reply, Reply, State, Timeout} |
%%			{noreply, State} |
%%			{noreply, State, Timeout} |
%%			{stop, Reason, Reply, State} |
%%			{stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call( {request, URI}, From, State ) ->
	Cert = case lists:keyfind( clientcert, 1, State#state.options ) of
		       { clientcert, C } -> C;
		       false -> undefined
	       end,
	{ Host, Port, Socket } = request_uri( URI, Cert ),
	{noreply, State#state{from=From, host=Host, port=Port, socket=Socket}};

handle_call( {setopt, Opt, Val}, _From, State ) ->
	NewOpts = lists:keystore( Opt, 1, State#state.options, {Opt, Val} ),
	NewState = State#state{options=NewOpts},
	{reply, NewOpts, NewState };

handle_call( getopts, _From, State ) ->
	{reply, State#state.options, State};

handle_call( {getopt, Opt}, _From, State ) ->
	case lists:keyfind( Opt, 1, State#state.options ) of 
		{ Opt, Val } ->
			{reply, Val, State};
		false ->
			{reply, error, State}
	end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast( Msg, State ) -> {noreply, State} |
%%				  {noreply, State, Timeout} |
%%				  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast( _Msg, State ) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% 
%% @spec handle_info( Info, State ) -> {noreply, State} |
%%				{noreply, State, Timeout} |
%%				{stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info( Info, State ) ->
	Socket = State#state.socket,
	PerformRedirect = case lists:keyfind( autoredirect, 1, State#state.options ) of
				  { autoredirect, AR } -> AR;
				  false -> false
			  end,
	Cert = case lists:keyfind( clientcert, 1, State#state.options ) of
		       { clientcert, C } -> C;
		       false -> undefined
	       end,
	case Info of
		{ssl, Socket, T} -> 
			{ noreply, State#state{response = [ T | State#state.response ]} };
		{ssl_closed, Socket } ->
			case parse_response( lists:reverse( State#state.response ) ) of
				{ { [$3 | _] , Redirect}, _ } when PerformRedirect =/= false ->
					NewURI = make_redirect_uri( State#state.host, State#state.port, Redirect ),
					{ Host, Port, NewSocket } = request_uri( NewURI, Cert ),
					{noreply, State#state{from=State#state.from, host=Host, port=Port, socket=NewSocket, response=[]}};
				Response ->
					gen_server:reply( State#state.from, Response ),
					{ noreply, #state{options=State#state.options} }
			end;
		Other ->
			io:format( "WHAT: ~tp~n", [ Other ] ),
			{ noreply, State }
	end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate( Reason, State ) -> void()
%% @end
%%--------------------------------------------------------------------
terminate( _Reason, _State ) ->
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change( OldVsn, State, Extra ) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change( _OldVsn, State, _Extra ) ->
		{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec send_request( Host::string(), Port::integer(), URI::string(), Cert::clientcert() ) -> Socket::any().
send_request( Host, Port, URI, Cert ) ->
	case Cert of
		undefined ->
			{ok, Socket} = ssl:connect( Host, Port, [], infinity ),
			ssl:send( Socket, sprintf( "~s\r\n", [ URI ] ) ),
			Socket;
		{ CertFile, CertKey } ->
			{ok, Socket} = ssl:connect( Host, Port, [ CertFile, CertKey ], infinity ),
			ssl:send( Socket, sprintf( "~s\r\n", [ URI ] ) ),
			Socket
	end.

-spec request_uri( URI::string(), Cert::clientcert() ) -> { Host::string(), Port::integer(), Socket::any() } | { error, Reason::term() }.
request_uri( URI, Cert ) ->
	case parse_host_port( URI ) of
		{ Host, Port } ->
			{ Host, Port, send_request( Host, Port, URI, Cert ) };
		error ->
			{ error, uri_parse_failed }
	end.

%%%===================================================================
%%% Utilities
%%%===================================================================

-spec sprintf( string(), string() ) -> string().
sprintf( Input, Format ) ->
	lists:flatten( io_lib:format( Input, Format ) ).

-spec parse_host_port( string() ) -> { string(), integer() } | error.
parse_host_port( URI ) ->
	case uri_string:parse( URI ) of
		#{ host := Host, port := Port } ->
			{ Host, Port };
		#{ host := Host } ->
			{ Host, ?DEFAULT_PORT };
		_ ->
			error
	end.

-spec make_redirect_uri( string(), integer(), string() ) -> string().
make_redirect_uri( OldHost, OldPort, RedirectURI ) ->
	case uri_string:parse( RedirectURI ) of
		#{ scheme := "gemini", host := _Host, port := _Port, path := _Path } ->
			% if redirect URI is complete, the return as is
			RedirectURI;
		#{ scheme := "gemini", host := Host, path := Path } ->
			uri_string:recompose( #{ scheme => "gemini", host => Host, path => Path, port => OldPort} );
		#{ path := Path } ->
			% probably a relative path
			uri_string:recompose( #{ scheme => "gemini", host => OldHost, path => Path, port => OldPort} );
		_ ->
			% weird so just return it
			RedirectURI
	end.

-spec parse_response( string() ) -> returnresult().
parse_response( Response ) ->
	case string:split( Response, "\r\n" ) of
		[ H | B ] ->
			{ parse_response_header( H ), parse_response_body( B ) };
		Other ->
			{ error, Other }
	end.

-spec parse_response_header( string() ) -> returnheader() | error.
parse_response_header( ResponseHeader ) ->
	case string:split( ResponseHeader, " " ) of
		[ Status, Meta ] ->
			{ Status, Meta };
		_Other ->
			error
	end.

-spec parse_response_body( string() ) -> string().
parse_response_body( Body ) ->
	lists:flatten( Body ).

%%%-------------------------------------------------------------------
%%% @doc Generate an outline using heading markers in the result
%%% 
%%% @param ReturnResult The result as returned by a request.
%%% @returns A list of strings containing any headers from the result.
%%% @end
%%%-------------------------------------------------------------------
-spec generate_outline( Result::returnresult() ) -> [ string() ].
generate_outline( { _Header, Body } ) ->
	Lines = string:split( Body, "\n", all ),
	outline( Lines, [] ).

-spec outline( [ string() ], [ string() ] ) -> [ string() ].
outline( [], Outline ) ->
	lists:reverse( Outline );
outline( [ Line | Rest ], Outline ) ->
	case Line of
		[ $# | _ ] ->
			outline( Rest, [ Line | Outline ] );
		_ ->
			outline( Rest, Outline )
	end.

%%%-------------------------------------------------------------------
%%% @doc Gather all links present in the result
%%% 
%%% @param ReturnResult The result as returned by a request.
%%% @returns A list of strings containing any links from the result.
%%% @end
%%%-------------------------------------------------------------------
-spec gather_links( Result::returnresult() ) -> [ string() ].
gather_links( { _Header, Body } ) ->
	Lines = string:split( Body, "\n", all ),
	links( Lines, [] ).

-spec links( [ string() ], [ string() ] ) -> [ string() ].
links( [], Links ) ->
	lists:reverse( Links );
links( [ Line | Rest ], Links ) ->
	case Line of
		[ $=, $> | Link ] ->
			links( Rest, [ strip_link( Link ) | Links ] );
		_ ->
			links( Rest, Links )
	end.

-spec strip_link( Link::string() ) -> string().
strip_link( Link ) ->
	{ L, _ } = string:take( string:trim( Link ), [ 32, 9 ], true ),
	L.

%%%-------------------------------------------------------------------
%%% @doc Create a certificate tuple to be used by setopt.
%%% 
%%% @param CertFile The file containing the certificate.
%%% @param KeyFile The file containin the private key.
%%% @returns A tuple that can be used by setopt to set the clientcert option.
%%% @end
%%%-------------------------------------------------------------------
-spec make_cert( CertFile::string(), KeyFile::string() ) -> clientcert().
make_cert( CertFile, KeyFile ) ->
	{ { certfile, CertFile }, { keyfile, KeyFile } }.

