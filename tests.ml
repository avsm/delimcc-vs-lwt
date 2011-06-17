(*
 * Copyright (c) 2011 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Lwt
open Printf

module Fiber = struct
  let basic fn iters yields =
    for i = 1 to iters do
      for x = 1 to yields do
        Lwt_fiber.await (fn ())
      done
    done

  let recurse fn iters depth =
    let rec sum n =
      Lwt_fiber.await (fn ());
      match n with
      |0 -> 0
      |n -> n+(sum (n-1)) in
    for i = 1 to iters do
      ignore(sum depth)
    done

  let run test blockfn iters yields =
    Lwt_fiber.start (fun () -> test blockfn iters yields)
end

module LWT = struct
  let basic fn iters yields =
    for_lwt i = 1 to iters do
      for_lwt x = 1 to yields do
        fn ()
      done
    done

  let recurse fn iters depth =
    let rec sum n =
      lwt () = fn () in
      match n with
      |0 -> return 0
      |n ->
        lwt n' = sum (n-1) in
        return (n+n') in
    for_lwt i = 1 to iters do
      lwt res = sum depth in
      return ()
    done

  let run test blockfn iters yields =
    test blockfn iters yields
end

let results = Hashtbl.create 1

let time name test desc fn blockfn iters yields =
  let t1 = Unix.gettimeofday () in
  Lwt_unix.run (fn blockfn iters yields);
  let t2 = Unix.gettimeofday () in
  Hashtbl.add results (name,test,desc) (yields,t2-.t1);
  Printf.printf "%10s %s %s %d %d %f\n%!" name test desc iters yields (t2 -. t1)

let _ =
  let iters = 5000 in
  let yields = [ 50; 100; 200; 300; 400; 600; 800; 1000 ] in
  let blockfns = [  
      "quick", Lwt.return;
      "medium", (fun () -> Lwt.pause ());
      "slow", (fun () -> Lwt_unix.sleep 0.0);
    ] in
  let fns = [
    ("delimcc","basic"), Fiber.(run basic);
    ("delimcc","recurse"), Fiber.(run recurse);
    ("lwt","basic"), LWT.(run basic);
    ("lwt", "recurse"), LWT.(run recurse);
  ] in
  List.iter (fun ((name, test), fn) ->
    List.iter (fun (desc, blockfn) ->
      List.iter (fun yields ->
        time name test desc fn blockfn iters yields;
      ) yields
    ) blockfns
  ) fns;
  let fout = open_out "results.marshal" in
  Marshal.to_channel fout results []
