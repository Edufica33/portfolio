import random  

def juego(numero,ganador):
     
    if numero == ganador:
        return True
    elif numero < ganador:
        return  2
    else: 
        return  3


def run():
    escogido= int(input("Elige un número del 1 al 100: "))
    premiado = random.randint(1,100)
    while juego(escogido,premiado):
        if juego(escogido,premiado) ==2:
            escogido= int(input("Elige un número mayor "))
            if juego(escogido,premiado) == True:
                print (' ¡Ganaste! ')
                break
        elif juego(escogido,premiado)==3:
            escogido= int(input("Elige un número menor "))
            if juego(escogido,premiado) == True:
                print (' ¡Ganaste! ')
                break
   

if __name__=="__main__":
    run()