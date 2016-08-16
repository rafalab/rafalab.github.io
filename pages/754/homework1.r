junk <-matrix(scan("Data/intensity.dat"),byrow=T,ncol=3)
junk1<-junk[,1]
junk2<-junk[,2]
junk3<-junk[,3]
junk1<-rev(junk1);junk2<-rev(junk2);junk3<-rev(junk3)
junkn<-table(junk1)
y<-rep(1,junkn[1])
u<-junk3[junk1==0];v<-junk2[junk1==0]
B<-rep(0,junkn[1])
A<-1+0*u
for (i in 2:7){
   y<-c(y,rep(0,(i-1)*junkn[i]),rep(1,junkn[i]))
   B<-c(B,rep(i+1,i*junkn[i]))
   junku<-junk3[junk1==i]
   junkv<-junk2[junk1==i]
   u<-c(u,rep(junku,i))
   v<-c(v,rep(junkv,i))
   junkA<-rep(1:i,rep(junkn[i],i))
   A<-c(A,junkA)
   }
y<-c(y,rep(0,7*junkn[8]))
B<-c(B,rep(8,7*junkn[8]))
BB<-as.factor(B)
junku<-junk3[junk1==8]
junkv<-junk2[junk1==8]
u<-c(u,rep(junku,7))
v<-c(v,rep(junkv,7))
junkA<-rep(1:7,rep(junkn[8],7))
A<-c(A,junkA)
AA<-as.factor(A)  

res <- glm(y~AA-1,family="binomial")
