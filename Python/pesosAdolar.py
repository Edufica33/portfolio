pesos = input("¿cuántos pesos mexicanos tienes?")
pesos = float(pesos)
pesos = round (pesos,3)
precio_dolar= 21
dolares = pesos/precio_dolar
dolares = round (dolares,3)
dolares = str(dolares)
print("tienes $"+dolares+" dolares")