def sci(n):
   if n == 1:
      return ['#']
   else:
      tr = sci(n//2)*2
      for t in range(0,n//2):
          tr[t] *= 2
      for t in range(n//2,n):
         tr[t] += ' '*(n//2)
      return tr

# n=int(input())
# tr = sci(n)
# for lign in tr:
#    print(lign)

def hanoi(n,d,a):
    if n == 1:
        print('{} -> {}'.format(d,a))
    else:
        hanoi(n-1,d,6-d-a)
        print('{} -> {}'.format(d,a))
        hanoi(n-1,6-d-a,a)

# n = int(input())
# # hanoi(n,1,3)
'''such an important line under '''
#debut, fin = map(int, input().split())

def inverse(word):
    if word == '':
        return ''
    else:
        return inverse(word[1:])+word[0]

w = input()

caractere = "U"
code = ord(caractere)
print(code)

texte = "".join(texte.split())