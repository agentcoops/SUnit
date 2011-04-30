(*  
 SUnit: A testing framework for SML. 
 A very literal port of OUnit (which, in turn, depends on HUnit). 
 Nothing new and fancy. Relies on exceptions far too much. 
   
 Author: coopf 
*)

structure OUnitPort =
struct


(* Various exceptions. Would be nice to instead make this code monadic... *)
exception Skip of string
fun skip_if b msg = if b then raise (Skip msg) else ()

exception Todo of string
fun todo msg = raise (Todo msg)

exception Failure of string
fun fail_with msg = raise (Failure msg)


(* The below code handles structural aspects of testing, describing a test's
   position in a suite*)
datatype node = 
         ListItem of int 
       | Label of string

type path = node list

val string_of_node =
    fn ListItem n => Int.toString n
     | Label s => s

fun string_of_path path =
    String.concatWith ":" (map string_of_node (List.rev path))


(* Core datatypes and helper functions for encoding and manipulating tests. *)
type test_fun = unit -> unit

datatype test =
         TestCase of test_fun       
       | TestLabel of string * test
       | TestList of test list 
             
datatype test_result = 
         RSuccess of path
       | RFailure of path * string
       | RError of path * string
       | RSkip of path * string
       | RTodo of path * string

datatype test_event =
         EStart of path
       | EEnd of path
       | EResult of test_result

fun test_case_count (TestCase _) = 1
  | test_case_count (TestLabel (_, t)) = test_case_count t
  (*| TestList l => 
   List.foldl (fn c t => c + test_case_count t) 0 l *)

val is_failure = 
 fn RFailure _ => true
  | _ => false

val is_success =
 fn RSuccess _  => true 
  | _ => false

val is_error = 
  fn RError _ => true
    | _ => false

val is_skip = 
  fn RSkip _ => true
   | _ => false

val is_todo = 
  fn RTodo _ => true
   | _ => false

fun was_successful [] = true
  | was_successful (RSuccess h::t) = was_successful t
  | was_successful (RSkip h::t)  = was_successful t    
  | was_successful (RFailure _) = false  
  | was_successful (RError _)  = false
  | was_successful (RTodo _) = false

val result_flavour =
 fn RError _ => "Error"
  | RFailure _ => "Failure"
  | RSuccess _ => "Success"
  | RSkip _ => "Skip"
  | RTodo _ => "Todo"

val result_path =
 fn RError (path, _) => path
  | RFailure (path, _) => path
  | RSuccess path => path
  | RSkip (path, _) => path
  | RTodo (path, _) => path

val result_msg =
 fn RError (_, msg) => msg
  | RFailure (_, msg) => msg
  | RSuccess _ => "Success"
  | RSkip (_, msg) => msg
  | RTodo (_, msg) => msg

val string_of_result =
 fn RSuccess _ => "ok\n"
  | RFailure (_, _) => "FAIL\n"
  | RError (_, _) => "ERROR\n"
  | RSkip (_, _) => "SKIP\n"
  | RTodo (_, _) => "TODO\n"


(* Defining the default test reporter. Currently doesn't support variable verbosity. *)
fun time_fun f =
    let val begin_time = Time.now () 
        val result = f ()
    in
        (Time.now () - begin_time, result)
    end

(* Wtf. PolyML has no standard library. *)
fun make_string count string =
    let fun make_string_accum accum =
         fn 0 => accum
          | count => make_string_accum (string ^ accum) (count - 1)
    in 
        make_string_accum "" count
    end

(* This was really just to test time_fun, but I need my own standard library now, apparently. *)
fun make_list count value =
    let fun make_list_accum accum =
         fn 0 => accum
          | count => make_list_accum (value::accum) (count - 1)
    in
        make_list_accum [] count
    end


fun perform_test report test = 
    let fun run_test_case f path =
            (f ();
             RSuccess path) 
            handle Failure s => RFailure (path, s ^ "\n")
                 | Skip s => RSkip(path, s)
                 | Todo s => RTodo (path, s)
                 | s => RError (path, "Unknown error!\n")
        and run_test path results test =
            case test of 
                TestCase f => let val result = 
                                      (report (EStart path);
                                       run_test_case f path)
                              in 
                                  (report (EResult result);
                                   report (EEnd path);
                                   result::results)
                              end
              (*| TestList (tests) => *)
              | TestLabel (label, t) => run_test ((Label label)::path) results t          
    in 
        run_test [] [] test
    end

fun run_test test =
    let val report_event = 
         fn EStart p => print ((string_of_path p) ^ "...\n")
          | EEnd _ => ()
          | EResult result => print (string_of_result result)

        fun print_result_list results =
            let val margin = 10 (* figure out equivalent of get_margin *)
                val separator1 = make_string margin "="
                val separator2 = make_string margin "-"
            in
                List.app
                    (fn result => (* omfg. need printf so bad! *)
                        let val path_string =  string_of_path (result_path result)
                            val flavour_string = result_flavour result
                            val intro_string = flavour_string ^ ": " ^ path_string ^ "\n"
                            val values = 
                                [separator1, intro_string,
                                 result_msg result, separator2]
                            val result_string = (String.concatWith "\n" values) ^ "\n"
                        in
                            print result_string
                        end)
                    results
            end
            
        val (running_time, results) = time_fun (fn () => perform_test report_event test)
        val errors = List.filter is_error results
        val failures = List.filter is_failure results
        val skips = List.filter is_skip results
        val todos = List.filter is_todo results

        val cases = test_case_count test
        val skip_count = List.length skips
        val result_count = List.length results
    in (
        print_result_list errors;
        print_result_list failures;
        print ("Ran: " ^ Int.toString result_count ^ "tests in " 
               ^ Time.toString running_time ^ " seconds.\n");
        if was_successful results then
            if skips = [] then
                print "OK!"
            else 
                print ("OK! Cases: " ^ cases ^ ", Skip: " ^ skip_count ^ "\n")
        else 
            print ("FAILED! Cases: " ^ cases ^ " Tried: " ^ result_count ^ 
                   " Errors: " ^ List.length errors ^ " Failures: " ^ 
                   List.length failures ^ " Skip: " ^ skip_count ^ 
                   " Todo: " ^ List.length todos ^ "\n")
        )
    end

            


(* Lets us perform set up/tear down actions around a test. *)
fun bracket set_up f tear_down () = 
    let val fixture = set_up ()
    in
        (f fixture;
         tear_down fixture) 
        handle e => 
               (tear_down fixture;
                raise e)
    end


fun assert_bool msg b = if not b then fail_with msg else ()


   
end





(* Guess we don't really need this... 
signature OUNIT =
sig
    type test
    type test_result
    type test_event
    type test_fun

    (* Core Functions *)
    val perform_test: (test_event -> 'a) -> test -> test_result list

    (* Assertions. Currently missing optional arguments. *)
    val assert_bool: string -> bool -> unit                         
    (*val assert_failure: string -> 'a
    val assert_string: string -> unit                          
    val assert_equal: 'a -> 'a -> unit (* This really needs to be generalized...*)        
    val assert_raises: exn -> (unit -> 'a) -> unit*)

    (* Skipping tests. TODO 
    fun skip_if: bool -> string -> unit 
    fun todo: string -> unit *)
                        
    (* Brackets, functional implementation of setUp/tearDown. *)
    (*val bracket: (unit -> 'a) -> ('a -> 'c) -> ('a -> 'b) -> unit -> 'b*)
end *)
