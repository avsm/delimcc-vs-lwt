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

open Printf

let google ~xlabel ~ylabel title results keys =
  (* output google charts API URL *)
  printf "http://chart.apis.google.com/chart?cht=lxy&chs=600x250&chtt=%s" title;
  printf "&chco=FF0000,00FF00,0000FF,FFAA00,AA00FF,00FFFF&";
  printf "chxt=x,x,y,y&chxl=1:|%s|3:|%s" xlabel ylabel;
  printf "&chds=a&chg=10,10,1,5&chd=t:";
  printf "%s" (String.concat "|" (List.map (fun key ->
    let x,y = List.split (List.rev (Hashtbl.find_all results key)) in
    let x = List.map string_of_int x in
    let y = List.map (sprintf "%.3f") y in
    sprintf "%s|%s" (String.concat "," x) (String.concat "," y);
  ) keys));
  printf "&chdl=%s" (String.concat "|" (List.map (fun (n,t,y) -> n^"-"^t^"-"^y) keys));
  printf "&chdlp=t&chls=%s\n" (String.concat "|" (List.map (fun _ -> "2") keys))

let _ = 
  let fin = open_in "results.marshal" in
  let h : (string * string * string, int * float) Hashtbl.t = Marshal.from_channel fin in
  close_in fin;
  Hashtbl.iter (fun (name,test,mode) (yields, time) ->
    printf "%s %s %s %d %f\n" name test mode yields time
  ) h;
  let keys = Hashtbl.fold (fun k v a -> if List.mem k a then a else k :: a) h [] in
  (* Basic no-recursion tests first *)
  (* 1) Lwt/delimcc quick *)
  let quick_keys = List.filter (function
     |_,"basic","quick" -> true
     |_ -> false) keys in
  let xlabel = "number-of-yields" in
  let ylabel = "seconds" in
  google ~xlabel ~ylabel "Direct%20non-blocking%20overhead" h quick_keys;
  (* 2) Lwt/delimcc medium/slow *) 
  let medium_keys = List.filter (function
     |_,"basic","medium" -> true
     |_,"basic","slow" -> true
     |_ -> false) keys in
  google ~xlabel ~ylabel "Direct%20blocking%20overhead" h medium_keys;
  let xlabel = "stack-depth" in
  (* 3) Lwt/delimcc slow recurse/basic *)
  let recurse_keys = List.filter (function
    |_,_,"slow" -> true
    |_ -> false) keys in
  google ~xlabel ~ylabel "Recurse%20vs%20basic" h recurse_keys;
  ()
