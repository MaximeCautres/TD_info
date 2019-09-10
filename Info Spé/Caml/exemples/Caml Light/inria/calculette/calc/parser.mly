/* Fichier de l’analyseur syntaxique: parser.mly */
%token <int> INT
%token PLUS MOINS FOIS DIV
%token PARENG PAREND
%token FINDELIGNE
%right PLUS MOINS			/* priorité la plus faible */
%right FOIS DIV				/* priorité intermédiaire */
%nonassoc MOINSUNAIRE			/* priorité la plus forte */
%start ligne				/* le point d’entrée */
%type <int> ligne
%%
ligne:
    expr FINDELIGNE	{$1}
;
expr:
  INT				{$1}
  | PARENG expr PAREND		{$2}
  | expr PLUS expr		{$1 + $3}
  | expr MOINS expr		{$1 - $3}
  | expr FOIS expr		{$1 * $3}
  | expr DIV expr		{$1 / $3}
  | MOINS expr %prec MOINSUNAIRE { - $2 }
