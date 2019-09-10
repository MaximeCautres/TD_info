let chan = open_out "test_int1" in
  output_binary_int64 chan min_int;
  output_binary_int64 chan max_int;  
  close_out chan;;

let chan = open_in_bin "test_int1" in
  let min = input_binary_int64 chan in
    let max = input_binary_int64 chan in
      close_in chan;
      print_int min;
      print_newline();
      print_int max
;;

let chan = open_out_bin "test_int2" in
  output_value chan min_int;
  output_value chan max_int;
  close_out chan;;

let chan = open_in_bin "test_int2" in
  let min = input_value chan in
    let max = input_value chan in
      close_in chan;
      print_int min;
      print_newline();
      print_int max
;;