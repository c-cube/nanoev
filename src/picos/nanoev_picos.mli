(** Basic interface with picos *)

module Background_thread = Background_thread
module Global_ev = Global_ev

(** {2 Non blocking IO primitives} *)

module Base = Base

include module type of struct
  include Base
end

(** {2 Building blocks on top of {!Base}} *)

module IO_in = IO_in
module IO_out = IO_out
module Net_client = Net_client
module Net_server = Net_server
