

L = [12, 5, 3, 7, 6, 9, 10, 2]

M3 = [] # lista multiplilor de 3 din L
n = 0
for i in range(len(L)): 
    if L[i] % 3 == 0:
        n += 1
        M3.append(L[i])
print(n, "multiples of 3:", M3)


# iterez explicit
# modific niște obiecte pe parcursul iterării
# exprim cum construiesc (pas cu pas) rezultatul

M3 = [] # lista multiplilor de 3 din L
n = 0
for e in L:
    if e % 3 == 0:
        n += 1
        M3.append(e)
print(n, "multiples of 3:", M3)



# exprim ce este rezultatul meu în funcție de intrări

M3 = list(filter(lambda e: e % 3 == 0, L))
print(len(M3), "multiples of 3:", M3)

M3 = [e for e in L if e % 3 == 0]		# list comprehension
print(len(M3), "multiples of 3:", M3)




LM = [12, 3, 6, 9, 18, -18]

flag = True
for e in LM:
	if e % 3 != 0: flag = False

print("" if flag else "Not", "all elements are multiples of 3")


flag = len([e for e in LM if e % 3 == 0]) == len(LM)
print("" if flag else "Not", "all elements are multiples of 3")

flag = all([e % 3 == 0 for e in LM])
print("" if flag else "Not", "all elements are multiples of 3")













