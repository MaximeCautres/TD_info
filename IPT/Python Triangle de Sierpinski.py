longueur_coté=int( input() )

from math import *
n=int( log(longueur_coté) / log(2) )




def sierpinski(n):
    """Fonction récursive qui renvoit un tableau représentant un triangle de Sierpinski de niveau n"""

    longueur= 2**n #un triangle de niveau n à toujours une longueur de 2^n lignes

    tableau= [ "" for loop in range(longueur) ]#on a longueur ligne dans tableau

    if n==1:
        tableau[0]= tableau[0]+"##"
        tableau[1]= tableau[1]+"#"
        return(tableau)
    else:
        tab= sierpinski(n-1)
        #on ajoute des espaces dans le carré en bas à droite
        blanc=(2**(n-2))*" "
        for ligne in range(2**(n-2),2**(n-1) ):
            tab[ligne]=tab[ligne]+blanc

        #on ajoute le tableau en haut à gauche et en même temps le tableau en haut à droite
        for ligne in range(2**(n-1)):
            tableau[ligne]=tableau[ligne]+2*tab[ligne]


        #on ajoute ensuite le tableau en bas à gauche
        for ligne in range( 2**(n-1),longueur):
            tableau[ligne]=tableau[ligne]+tab[ligne-2**(n-1)]
        return(tableau)

if n==0:
    print("#")
else:
    tableau=sierpinski(n)
    for ligne in tableau:
        print(ligne)










