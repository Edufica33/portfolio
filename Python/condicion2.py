coin = input("¿qué pesos usas: arg, colom, mex? ")
dolar_en_arg= 200
dolar_en_colom=3800
dolar_en_mex=21
def conversion(moneda_local):
    cantidad=float(input("¿Cuanto tienes en pesos "+coin+" ?  "))
    dolares=cantidad/moneda_local 
    dolares= round(dolares,3)
    dolares=str(dolares)
    print("tienes $"+dolares+" dolares")

if coin== "arg":
    conversion(dolar_en_arg)
elif coin == "colom":
    conversion(dolar_en_colom)
elif coin == "mex":
    conversion(dolar_en_mex)
else:
    print ("escribe arg, colom o mex")

