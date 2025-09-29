# nanoev

**NOTE (2025-09-29)**: I ended up not using this. Instead I reuse Lwt and Lwt_engine. I recommend not using this as there are some timer-related bugs.


A minimalistic but modular abstraction for IO event loops.

The goal of this library is to provide a uniform abstraction over multiple
system event loops, in a way that plays well with Picos.

## Usage

Very basic usage would look like this:

```ocaml
module EV = Nanoev_picos


let () =
  (* use a backend, eg. select *)
  let ev = Nanoev_unix.create () in

  (* install the backend *)
  Nanoev_picos.setup_bg_thread ev;

  (* setup a picos scheduler and use EV.read, EV.write, etc. *)
  …
```


## Backends

- [x] select
- [x] poll/ppoll
- [ ] epoll
- [ ] kqueue
- [ ] uring
