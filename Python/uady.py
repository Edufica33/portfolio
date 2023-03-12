
def run():


    r=1

    for  i in range(1,100000):
        r = r + 2*(4*i)
        i=i+1

    print (r)
if __name__=="__main__":
    run()