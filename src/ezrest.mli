(** {1 "Easy" REST requests with cohttp} *)

(** {1 Request errors} *)

type error =
  | Too_many_redirects
  | Unexpected_redirect_body_content of Uri.t
  | Missing_redirect_header of Uri.t
  | Unhandled_response_code of unhandled_response_code
  | Connection_error of Unix.error * string
  | Trapped_exception of exn * Printexc.raw_backtrace
  | Timed_out

and unhandled_response_code = {
  uri : Uri.t;
  status : Cohttp.Code.status_code;
  body : string;
}

type nonrec 'a result = ('a, [ `Ezrest of error ]) result

(** {1 Timeouts}

    All of the functions defined below take an optional [timeout] parameter
    which attempts to have a reasonable default. Timeouts are specified in units
    of seconds. *)

val default_timeout : float ref
(** The current default timeout in seconds. Changing this affects all requests
    made after a new value is assigned. *)

(** {1 Responses} *)

type 'body t = {
  response : Cohttp.Response.t;
      (** Response metadata (status code, headers, etc) *)
  body : 'body;  (** Response body *)
}
(** The contents of a response *)

module type S = sig
  (** {1 Streaming vs eagerly-consumed bodies} *)

  type response
  (** The type of a response *)

  (** {1 The Verbs} *)

  val head :
    ?ctx:Cohttp_lwt_unix.Client.ctx ->
    ?headers:Cohttp.Header.t ->
    ?timeout:float ->
    Uri.t ->
    Cohttp.Response.t result Lwt.t
  (** [head ?ctx ?headers uri] returns the result of a [HEAD] request to [uri]. *)

  val get :
    ?ctx:Cohttp_lwt_unix.Client.ctx ->
    ?headers:Cohttp.Header.t ->
    ?timeout:float ->
    follow:int ->
    Uri.t ->
    response result Lwt.t
  (** [get ?ctx ?headers ~follow uri] returns the result of a [GET] request to
      [uri].

      @param follow specifies the maximum number of redirects to follow. *)

  val delete :
    ?ctx:Cohttp_lwt_unix.Client.ctx ->
    ?headers:Cohttp.Header.t ->
    ?timeout:float ->
    Uri.t ->
    response result Lwt.t
  (** [delete ?ctx ?headers ~follow uri] returns the result of a [DELETE]
      request to [uri]. *)

  val patch :
    ?ctx:Cohttp_lwt_unix.Client.ctx ->
    ?headers:Cohttp.Header.t ->
    ?timeout:float ->
    ?body:Cohttp_lwt.Body.t ->
    Uri.t ->
    response result Lwt.t
  (** [post ?ctx ?headers ?body uri] returns the result of a [PATCH] request to
      [uri].

      @param body is the [PATCH] body to use. There is no body by default. *)

  val post :
    ?ctx:Cohttp_lwt_unix.Client.ctx ->
    ?headers:Cohttp.Header.t ->
    ?timeout:float ->
    ?body:Cohttp_lwt.Body.t ->
    Uri.t ->
    response result Lwt.t
  (** [post ?ctx ?headers ?body uri] returns the result of a [POST] request to
      [uri].

      @param body is the [POST] body to use. There is no body by default. *)

  val put :
    ?ctx:Cohttp_lwt_unix.Client.ctx ->
    ?headers:Cohttp.Header.t ->
    ?timeout:float ->
    ?body:Cohttp_lwt.Body.t ->
    Uri.t ->
    response result Lwt.t
  (** [put ?ctx ?headers ?body uri] returns the result of a [PUT] request to
      [uri].

      @param body is the [PUT] body to use. There is no body by default. *)

  val post_form :
    ?ctx:Cohttp_lwt_unix.Client.ctx ->
    ?headers:Cohttp.Header.t ->
    ?timeout:float ->
    params:(string * string list) list ->
    Uri.t ->
    response result Lwt.t
  (** [post_form ?ctx ?headers ~params uri] returns the result of a form [POST]
      request to [uri].

      @param params specifies a list of [(key, value)] pairs which represent the
      form elements to send. *)

  val call :
    ?ctx:Cohttp_lwt_unix.Client.ctx ->
    ?headers:Cohttp.Header.t ->
    ?timeout:float ->
    ?body:Cohttp_lwt.Body.t ->
    Cohttp.Code.meth ->
    Uri.t ->
    response result Lwt.t
  (** [call ?ctx ?headers ?timeout ?body meth uri] makes a [meth] call to [uri].
      It is a more generic version of the other functions in this module. The
      more specific functions like {!get} and {!post} should be used when
      possible.

      @param body is the request body to send. There is no body by default. *)
end

module String_response_body : S with type response = string t
(** {1 Response bodies pre-consumed into a string} *)

module Ignore_response_body : S with type response = unit t
(** {1 Response bodies consumed and ignored} *)

module Streaming_response_body : S with type response = Cohttp_lwt.Body.t t
(** {1 Response bodies in their raw form} *)

include S with type response = string

(** {2 Printing and converting errors} *)

val pp_error : Format.formatter -> [ `Ezrest of error ] -> unit
val open_error : 'a result -> ('a, [> `Ezrest of error ]) Stdlib.result
val error_to_msg : 'a result -> ('a, [ `Msg of string ]) Stdlib.result
