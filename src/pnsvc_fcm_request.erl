-module(pnsvc_fcm_request).

-export([encode/1]).

-type notification() :: #{
    title => binary(),
    body => binary()
}.

-type android_notification() :: #{
    title => binary(),
    body => binary(),
    icon => binary(),
    color => binary(),
    sound => binary(),
    tag => binary(),
    click_action => binary(),
    body_loc_key => binary(),
    body_loc_args => [binary()],
    title_loc_key => binary(),
    title_loc_args => [binary()],
    channel_id => binary()
}.

-type android() :: #{
    collapse_key => binary(),
    priority => binary(),
    ttl => binary(), 
    restricted_package_name => binary(),
    data => map(),
    notification => android_notification()
}.

-type apns_alert() :: #{
    title => binary(),
    body => binary(),
    'body-loc-key' => binary(),
    'body-loc-args' => [binary()],
    'title-loc-key' => binary(),
    'title-loc-args' => [binary()],
    'loc-key' => binary(),
    'loc-args' => [binary()],
    'launch-image' => binary()
}.

-type apns_payload() :: #{
    alert => apns_alert() | binary(),
    badge => non_neg_integer(),
    sound => binary() | default,
    'content-available' => non_neg_integer(),
    category => binary(),
    'thread-id' => binary()
}.

-type apns() :: #{
    headers => map(),
    payload => apns_payload()
}.

-type webpush_fcm_option() :: #{
    link => binary()
}.

-type webpush() :: #{
    headers => map(),
    data => map(),
    notification => map(),
    fcm_options => webpush_fcm_option()
}.

-type message() :: #{
    token  => binary(),
    notification => notification(),
    android => android(),
    apns => apns(),
    webpush => webpush()
}.

-opaque request() :: #{
    message  => message()
}.

-export_type([request/0]).

-spec encode(Req :: request()) -> {ok, term()} | {error, term()}.
encode(Req) when is_binary(Req) -> {ok, Req};
encode(Req) -> jsone:try_encode(Req).
