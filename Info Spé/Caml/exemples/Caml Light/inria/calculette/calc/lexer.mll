(* Fichier lexer.mll *)
{
#open "parser";; (* Le type token est défini dans *)
                 (* l’analyseur syntaxique parser.mli *)
        exception Fin_de_fichier;;
}
rule lexeme = parse
  [` ` `\t`]	{ lexeme lexbuf } (* supprime les blancs *)
  | [`\n` ]	{ FINDELIGNE }
  | [`0`-`9`]+	{ INT(int_of_string (get_lexeme lexbuf)) }
  | `+`		{ PLUS }
  | `-`		{ MOINS }
  | `*`		{ FOIS }
  | `/`		{ DIV }
  | `(`		{ PARENG }
  | `)`		{ PAREND }
  | eof		{ raise Fin_de_fichier }
;;









