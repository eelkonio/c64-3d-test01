#!python3

import math

debug=False
#debug=True


logtable_x=[]
logtable_cos=[]
invlogtable_x=[]

log_upper_limit=(256/math.log(256))


print ("Calculating X logarithm table")
for x in range(-127, 128):
    if x<0:
        key=-x
    else:
        key=x
    if key!=0:
        value=int(math.log(abs(key))*log_upper_limit)
    else:
        value=0
    logtable_x.append(value)
    if debug:
        print(x,value)
    #print(x,value)

print ("Calculating X INVERSE logarithm table")
for invlogx in range(0, 256):
    if invlogx!=0:
        value=int(math.exp(invlogx/log_upper_limit))
    else:
        value=0
    invlogtable_x.append(value)
    if debug:
        print(invlogx,value)
    #print(invlogx,value)

        
print ("\nCalculating cosinus logarithm table")
for angle in range(0, 256):
    cosvalue=math.cos(angle/256*2*3.1415926535)
    if cosvalue<0:
        key=-cosvalue
    else:
        key=cosvalue
    if key!=0:
        cosvalue=int(math.log(abs(key))*log_upper_limit + 0.5)
        if cosvalue<-127:
            cosvalue=-127
    else:
        cosvalue=0
    if debug:
        print ("cosvalue=",cosvalue)
    logtable_cos.append(-cosvalue)
    if debug:
        print(angle,cosvalue)





#for x in range(0,127):
#    logx=logtable_x[x+128]
#    invlogx=invlogtable_x[logx]
#    print ("Compare: x=%d, logx=%d, newx=%d" %(x, logx, invlogx))

        

x=100
y=50
z=20
print ("x/y/z\t->\tx'/y'/z'")
for angle in range(0,1000):
    angleb=angle*math.sin(angle/(1000/8/3.1415926535))
    logx=logtable_x[x+128]
    logy=logtable_x[y+128]
    logz=logtable_x[z+128]

    # Angle ALPHA: rotation in X/Y plane
    logcosangle=logtable_cos[angle % 256]
    logsinangle=logtable_cos[(angle+192)%256]
    xnewlog1=int(logx-logcosangle)
    if xnewlog1<0:
        xnewlog1=0
    xnewlog2=int(logy-logsinangle)
    if xnewlog2<0:
        xnewlog2=0
    xnew=invlogtable_x[xnewlog1] - invlogtable_x[xnewlog2]

    ynewlog1=int(logx-logsinangle)
    ynewlog2=int(logy-logcosangle)
    if ynewlog1<0:
        ynewlog1=0
    if ynewlog2<0:
        ynewlog2=0
    ynew=invlogtable_x[ynewlog1] + invlogtable_x[ynewlog2]


    # Angle ALPHA: rotation in X/Y plane
    logynew=logtable_x[ynew+128]
    logcosangleb=logtable_cos[int(angleb % 256)]
    logsinangleb=logtable_cos[int((angleb+192)%256)]
    znewlog1=int(logz-logcosangleb)
    if znewlog1<0:
        znewlog1=0
    znewlog2=int(logz-logsinangleb)

    if znewlog2<0:
        znewlog2=0
    znew=invlogtable_x[znewlog1] - invlogtable_x[znewlog2]

    ynewlog1=int(logz-logsinangleb)
    ynewlog2=int(logynew-logcosangleb)
    if ynewlog1<0:
        ynewlog1=0
    if ynewlog2<0:
        ynewlog2=0
    ynewnew=invlogtable_x[ynewlog1] + invlogtable_x[ynewlog2]    

    print ("%d/%d/%d \t %d&%d \t %d/%d/%d" %(x,y,z, angle,angleb, xnew,ynewnew,znew ))
