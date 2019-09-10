(* Fichier calc.ml *)
try
  let tampon = lexing__create_lexer_channel std_in in
  while true do
    let résultat = parser__ligne lexer__lexeme tampon in print_int résultat;
    print_newline (); flush std_out
  done
with lexer__Fin_de_fichier -> ()
;;
